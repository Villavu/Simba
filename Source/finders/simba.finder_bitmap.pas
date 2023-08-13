{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  The simple bitmap finder.
}
unit simba.finder_bitmap;

{$DEFINE SIMBA_MAX_OPTIMIZATION}
{$i simba.inc}

{.$DEFINE SIMBA_BENCHMARKS}

interface

uses
  Classes, SysUtils, Graphics,
  simba.mufasatypes, simba.colormath, simba.colormath_distance, simba.target, simba.bitmap,
  simba.colormath_distance_unrolled, simba.simplelock;

function FindBitmapOnTarget(Target: TSimbaTarget; Bitmap: TSimbaImage; Bounds: TBox;
                            Formula: EColorSpace; Tolerance: Single; Multipliers: TChannelMultipliers; MaxToFind: Integer = -1): TPointArray;

function FindBitmapOnBuffer(var Limit: TSimpleThreadsafeLimit;
                            Bitmap: TSimbaImage;
                            ColorSpace: EColorSpace; Tolerance: Single; Multipliers: TChannelMultipliers;
                            Buffer: PColorBGRA; BufferWidth: Integer;
                            SearchWidth, SearchHeight: Integer): TPointArray;

var
  BitmapFinderMultithreadOpts: record
    Enabled: Boolean;
    SliceWidth, SliceHeight: Integer;
  end;

implementation

uses
  simba.datetime,
  simba.overallocatearray,
  simba.threadpool, simba.tpa, simba.atpa;

// How much to "Slice" (vertically) the image up for multithreading.
function CalculateSlices(SearchWidth, SearchHeight: Integer): Integer;
var
  I: Integer;
begin
  Result := 1;

  if BitmapFinderMultithreadOpts.Enabled and (SearchWidth >= BitmapFinderMultithreadOpts.SliceWidth) and (SearchHeight >= (BitmapFinderMultithreadOpts.SliceHeight * 2)) then // not worth
  begin
    for I := SimbaThreadPool.ThreadCount - 1 downto 2 do
      if (SearchHeight div I) > BitmapFinderMultithreadOpts.SliceHeight then // Each slice is at least `MatchTemplateMT_SliceHeight` pixels
        Exit(I);
  end;

  // not possible to slice into at least `MatchTemplateMT_SliceHeight` pixels
end;

const
  BitmapColorSize = SizeOf(TColorHSL) + SizeOf(Boolean);

// Pre calculate color space
// and "transparent" (aka ignore) colors.
function ConvertBitmapColors(Bitmap: TSimbaImage; ColorSpace: EColorSpace): PByte;
var
  I: Integer;
  Source: PColorBGRA;
  Dest, DestFix: PByte;
begin
  // packed record Transparent: Boolean; Color: TColorXXX; end;
  Result := GetMem((Bitmap.Width * Bitmap.Height) * BitmapColorSize);

  Source := Bitmap.Data;
  Dest := Result;

  for I := 0 to (Bitmap.Width * Bitmap.Height) - 1 do
  begin
    PBoolean(Dest)^ := (Bitmap.TransparentColorActive and Source^.EqualsIgnoreAlpha(Bitmap.TransparentRGB));
    if not PBoolean(Dest)^ then
    begin
      DestFix := Dest + 1; // temp fix, https://gitlab.com/freepascal.org/fpc/source/-/commit/851af5033fb80d4e19c4a7b5c44d50a36f456374
      case ColorSpace of
        EColorSpace.RGB:    PColorRGB(DestFix)^ := Source^.ToRGB();
        EColorSpace.HSV:    PColorHSV(DestFix)^ := Source^.ToHSV();
        EColorSpace.HSL:    PColorHSL(DestFix)^ := Source^.ToHSL();
        EColorSpace.XYZ:    PColorXYZ(DestFix)^ := Source^.ToXYZ();
        EColorSpace.LCH:    PColorLCH(DestFix)^ := Source^.ToLCH();
        EColorSpace.LAB:    PColorLAB(DestFix)^ := Source^.ToLAB();
        EColorSpace.DeltaE: PColorLAB(DestFix)^ := Source^.ToLAB();
      end;
    end;

    Inc(Source);
    Inc(Dest, BitmapColorSize);
  end;
end;

function FindBitmapOnTarget(Target: TSimbaTarget; Bitmap: TSimbaImage; Bounds: TBox; Formula: EColorSpace; Tolerance: Single; Multipliers: TChannelMultipliers; MaxToFind: Integer): TPointArray;
var
  Buffer: PColorBGRA;
  BufferWidth: Integer;

  SliceResults: T2DPointArray;

  Limit: TSimpleThreadsafeLimit;

  procedure Execute(const Index, Lo, Hi: Integer);
  var
    TPA: TPointArray;
  begin
    TPA := FindBitmapOnBuffer(
      Limit,
      Bitmap, Formula, Tolerance, Multipliers,
      @Buffer[Lo * BufferWidth], BufferWidth, Bounds.Width, (Hi - Lo) + Bitmap.Height
    );

    SliceResults[Index] := TPA.Offset(Bounds.X1, Bounds.Y1 + Lo);
  end;

var
  T: Double;
  ThreadsUsed: Integer;
begin
  Result := [];

  Limit := TSimpleThreadsafeLimit.Create(MaxToFind);

  if Target.GetImageData(Bounds, Buffer, BufferWidth) then
  try
    {$IFDEF SIMBA_BENCHMARKS}
    T := HighResolutionTime();
    {$ENDIF}

    SetLength(SliceResults, CalculateSlices(Bounds.Width, Bounds.Height)); // Cannot exceed this
    ThreadsUsed := SimbaThreadPool.RunParallel(Length(SliceResults), 0, Bounds.Height - Bitmap.Height, @Execute);
    Result := SliceResults.Merge();
    if (MaxToFind > -1) and (Length(Result) > MaxToFind) then
      SetLength(Result, MaxToFind);

    {$IFDEF SIMBA_BENCHMARKS}
    DebugLn('FindBitmap: ColorSpace=%s Width=%d Height=%d ThreadsUsed=%d Time=%f', [Formula.AsString(), Bounds.Width, Bounds.Height, ThreadsUsed, HighResolutionTime() - T]);
    {$ENDIF}
  finally
    Target.FreeImageData(Buffer);
  end;
end;

function FindBitmapOnBuffer(var Limit: TSimpleThreadsafeLimit; Bitmap: TSimbaImage; ColorSpace: EColorSpace; Tolerance: Single; Multipliers: TChannelMultipliers; Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer): TPointArray;
var
  BitmapColors: PByte;

  CompareFunc: TColorDistanceFunc;
  MaxDistance: Single;

  function IsTransparent(const BitmapPtr: Pointer): Boolean; inline;
  begin
    Result := PBoolean(BitmapPtr)^;
  end;

  function Match(const BufferPtr: TColorBGRA; const BitmapPtr: PByte): Boolean; inline;
  begin
    Result := (CompareFunc(BitmapPtr + 1, BufferPtr, Multipliers) / MaxDistance * 100 <= Tolerance);
  end;

  function Hit(BufferPtr: PColorBGRA): Boolean;
  var
    X, Y: Integer;
    BitmapPtr: PByte;
  begin
    BitmapPtr := BitmapColors;

    for Y := 0 to Bitmap.Height - 1 do
    begin
      for X := 0 to Bitmap.Width - 1 do
      begin
        if (not IsTransparent(BitmapPtr)) and (not Match(BufferPtr^, BitmapPtr)) then
          Exit(False);

        Inc(BitmapPtr, BitmapColorSize);
        Inc(BufferPtr);
      end;

      Inc(BufferPtr, BufferWidth - Bitmap.Width);
    end;

    Result := True;
  end;

var
  X, Y: Integer;
  RowPtr: PColorBGRA;
  PointBuffer: TSimbaPointBuffer;
begin
  Result := [];

  case ColorSpace of
    EColorSpace.RGB:
      begin
        CompareFunc := TColorDistanceFunc(@DistanceRGB_UnRolled);
        MaxDistance := DistanceRGB_Max(Multipliers);
      end;

    EColorSpace.HSV:
      begin
        CompareFunc := TColorDistanceFunc(@DistanceHSV_UnRolled);
        MaxDistance := DistanceHSV_Max(Multipliers);
      end;

    EColorSpace.HSL:
      begin
        CompareFunc := TColorDistanceFunc(@DistanceHSL_Unrolled);
        MaxDistance := DistanceHSL_Max(Multipliers);
      end;

    EColorSpace.XYZ:
      begin
        CompareFunc := TColorDistanceFunc(@DistanceXYZ_UnRolled);
        MaxDistance := DistanceXYZ_Max(Multipliers);
      end;

    EColorSpace.LAB:
      begin
        CompareFunc := TColorDistanceFunc(@DistanceLAB_UnRolled);
        MaxDistance := DistanceLAB_Max(Multipliers);
      end;

    EColorSpace.LCH:
      begin
        CompareFunc := TColorDistanceFunc(@DistanceLCH_UnRolled);
        MaxDistance := DistanceLCH_Max(Multipliers);
      end;

    EColorSpace.DeltaE:
      begin
        CompareFunc := TColorDistanceFunc(@DistanceDeltaE_UnRolled);
        MaxDistance := DistanceDeltaE_Max(Multipliers);
      end;

    else
      SimbaException('FindBitmapOnBuffer: Formula invalid!');
  end;

  BitmapColors := ConvertBitmapColors(Bitmap, ColorSpace);

  try
    Dec(SearchWidth, Bitmap.Width);
    Dec(SearchHeight, Bitmap.Height);

    for Y := 0 to SearchHeight do
    begin
      RowPtr := @Buffer[Y * BufferWidth];

      for X := 0 to SearchWidth do
      begin
        if Hit(RowPtr) then
        begin
          PointBuffer.Add(X, Y);

          Limit.Inc();
        end;

        Inc(RowPtr);
      end;

      // Check if we reached the limit every row.
      if Limit.Reached() then
        Break;
    end;

    Result := PointBuffer.Trim();
  finally
    FreeMem(BitmapColors);
  end;
end;

initialization
  BitmapFinderMultithreadOpts.Enabled     := True;
  BitmapFinderMultithreadOpts.SliceHeight := 125;
  BitmapFinderMultithreadOpts.SliceWidth  := 250;

end.

