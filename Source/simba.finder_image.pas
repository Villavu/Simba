{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  The simple Image finder.
}
unit simba.finder_image;

{$i simba.inc}

{.$DEFINE SIMBA_BENCHMARKS}

interface

uses
  Classes, SysUtils,
  simba.base,
  simba.colormath,
  simba.target,
  simba.image;

function FindImageOnTarget(Target: TSimbaTarget; Image: TSimbaImage; Bounds: TBox;
                           Formula: EColorSpace; Tolerance: Single; Multipliers: TChannelMultipliers; MaxToFind: Integer = -1): TPointArray;

function FindTemplateOnTarget(Target: TSimbaTarget; Templ: TSimbaImage; out Match: Single; Bounds: TBox): TPoint;

implementation

uses
  simba.container_point,
  simba.matchtemplate,
  simba.threading,
  simba.multiprocessing,
  simba.colormath_distance,
  simba.colormath_distance_unrolled,
  simba.vartype_pointarray,
  simba.vartype_box,
  simba.vartype_matrix;

const
  BitmapColorSize = SizeOf(TColorHSL) + SizeOf(Boolean);

// Pre calculate color space
// and "transparent" (aka ignore) colors.
function ConvertBitmapColors(Image: TSimbaImage; ColorSpace: EColorSpace): PByte;
var
  I: Integer;
  Source: PColorBGRA;
  Dest, DestFix: PByte;
begin
  // packed record Transparent: Boolean; Color: TColorXXX; end;
  Result := GetMem((Image.Width * Image.Height) * BitmapColorSize);

  Source := Image.Data;
  Dest := Result;

  for I := 0 to (Image.Width * Image.Height) - 1 do
  begin
    PBoolean(Dest)^ := Source^.A = 0;

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

function FindImageOnBuffer(var Limit: TLimit; Image: TSimbaImage; ColorSpace: EColorSpace; Tolerance: Single; Multipliers: TChannelMultipliers; Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer): TPointArray;
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
    Result := (CompareFunc(BitmapPtr + 1, @BufferPtr, Multipliers) / MaxDistance * 100 <= Tolerance);
  end;

  function Hit(BufferPtr: PColorBGRA): Boolean;
  var
    X, Y: Integer;
    BitmapPtr: PByte;
  begin
    BitmapPtr := BitmapColors;

    for Y := 0 to Image.Height - 1 do
    begin
      for X := 0 to Image.Width - 1 do
      begin
        if (not IsTransparent(BitmapPtr)) and (not Match(BufferPtr^, BitmapPtr)) then
          Exit(False);

        Inc(BitmapPtr, BitmapColorSize);
        Inc(BufferPtr);
      end;

      Inc(BufferPtr, BufferWidth - Image.Width);
    end;

    Result := True;
  end;

var
  X, Y: Integer;
  RowPtr: PColorBGRA;
  PointBuffer: TPointBuffer;
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
  end;

  BitmapColors := ConvertBitmapColors(Image, ColorSpace);

  try
    Dec(SearchWidth, Image.Width);
    Dec(SearchHeight, Image.Height);

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

    Result := PointBuffer.ToArray(False);
  finally
    FreeMem(BitmapColors);
  end;
end;

function FindImageOnTarget(Target: TSimbaTarget; Image: TSimbaImage;
  Bounds: TBox; Formula: EColorSpace; Tolerance: Single;
  Multipliers: TChannelMultipliers; MaxToFind: Integer): TPointArray;
var
  Buffer: PColorBGRA;
  BufferWidth: Integer;

  SliceResults: T2DPointArray;

  Limit: TLimit;

  procedure Execute(const Index, Lo, Hi: Integer);
  var
    TPA: TPointArray;
  begin
    TPA := FindImageOnBuffer(
      Limit,
      Image, Formula, Tolerance, Multipliers,
      @Buffer[Lo * BufferWidth], BufferWidth, Bounds.Width, (Hi - Lo) + Image.Height
    );

    SliceResults[Index] := TPA.Offset(Bounds.X1, Bounds.Y1 + Lo);
  end;

{$IFDEF SIMBA_BENCHMARKS}
var
  T: Double;
  ThreadsUsed: Integer;
{$ENDIF}
begin
  Result := [];

  Limit := TLimit.Create(MaxToFind);

  if Target.GetImageData(Bounds, Buffer, BufferWidth) then
  try
    SetLength(SliceResults, SimbaMultiprocessing.ThreadsForArea(Bounds.Width, Bounds.Height)); // Cannot exceed this
    {$IFDEF SIMBA_BENCHMARKS}
    T := HighResolutionTime();
    ThreadsUsed :=
    {$ENDIF}
    SimbaMultiprocessing.Run(Length(SliceResults), 0, Bounds.Height - Image.Height, @Execute);

    Result := SliceResults.Merge();
    if (MaxToFind > -1) and (Length(Result) > MaxToFind) then
      SetLength(Result, MaxToFind);

    {$IFDEF SIMBA_BENCHMARKS}
    DebugLn('FindImage: ColorSpace=%s Width=%d Height=%d ThreadsUsed=%d Time=%f', [Formula.AsString(), Bounds.Width, Bounds.Height, ThreadsUsed, HighResolutionTime() - T]);
    {$ENDIF}
  finally
    Target.FreeImageData(Buffer);
  end;
end;

function FindTemplateOnTarget(Target: TSimbaTarget; Templ: TSimbaImage; out Match: Single; Bounds: TBox): TPoint;
var
  Image: TSimbaImage;
  Mat: TSingleMatrix;
  Best: TPoint;
begin
  Match := 0;

  if Target.GetImageDataAsImage(Bounds, Image) then
  try
    Mat := MatchTemplate(Image.ToMatrix(), Templ.ToMatrix(), TM_CCOEFF_NORMED);

    Best := Mat.ArgMax;
    Match := Mat[Best.Y, Best.X];
    Result := Best + Bounds.TopLeft;
  finally
    Image.Free();
  end;
end;

end.

