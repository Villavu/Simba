{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  The colorfinder.

  Lots of code from: https://github.com/slackydev/colorlib
}
unit simba.finder_color;

{$DEFINE SIMBA_MAX_OPTIMIZATION}
{$i simba.inc}

{.$DEFINE SIMBA_BUFFERCHECKS}
{.$DEFINE SIMBA_BENCHMARKS}

interface

uses
  Classes, SysUtils, Math, Graphics,
  simba.base, simba.colormath, simba.colormath_distance, simba.target;

function FindColorsOnTarget(Target: TSimbaTarget; Bounds: TBox;
                            Formula: EColorSpace; Color: TColor; Tolerance: Single; Multipliers: TChannelMultipliers): TPointArray;

function FindColorsOnBuffer(Formula: EColorSpace; Color: TColor; Tolerance: Single; Multipliers: TChannelMultipliers;
                            Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer; OffsetX, OffsetY: Integer {$IFDEF SIMBA_BUFFERCHECKS}; BufferLo, BufferHi: PColorBGRA{$ENDIF}): TPointArray;

function CountColorsOnTarget(Target: TSimbaTarget; Bounds: TBox;
                             Formula: EColorSpace; Color: TColor; Tolerance: Single; Multipliers: TChannelMultipliers): Integer;

function CountColorsOnBuffer(Formula: EColorSpace; Color: TColor; Tolerance: Single; Multipliers: TChannelMultipliers;
                             Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer {$IFDEF SIMBA_BUFFERCHECKS}; BufferLo, BufferHi: PColorBGRA{$ENDIF}): Integer;

function MatchColorsOnBuffer(Formula: EColorSpace; Color: TColor; Multipliers: TChannelMultipliers;
                             Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer {$IFDEF SIMBA_BUFFERCHECKS}; BufferLo, BufferHi: PColorBGRA{$ENDIF}): TSingleMatrix;

function MatchColorsOnTarget(Target: TSimbaTarget; Bounds: TBox;
                             Formula: EColorSpace; Color: TColor; Multipliers: TChannelMultipliers): TSingleMatrix;

var
  ColorFinderMultithreadOpts: record
    Enabled: Boolean;
    SliceWidth, SliceHeight: Integer;
  end;

implementation

uses
  simba.arraybuffer, simba.colormath_distance_unrolled, simba.threadpool,
  simba.array_pointarray, simba.matrix_float;

// How much to "Slice" (vertically) the image up for multithreading.
function CalculateSlices(SearchWidth, SearchHeight: Integer): Integer;
var
  I: Integer;
begin
  Result := 1;

  if ColorFinderMultithreadOpts.Enabled and (SearchWidth >= ColorFinderMultithreadOpts.SliceWidth) and (SearchHeight >= (ColorFinderMultithreadOpts.SliceHeight * 2)) then // not worth
  begin
    for I := SimbaThreadPool.ThreadCount - 1 downto 2 do
      if (SearchHeight div I) > ColorFinderMultithreadOpts.SliceHeight then // Each slice is at leastColorFinderMT_SliceHeight` pixels
        Exit(I);
  end;

  // not possible to slice into at least `ColorFinderMT_SliceHeight` pixels
end;

{$DEFINE MACRO_FINDCOLORS :=
var
  TargetColorContainer: array[0..2] of Single;
  TargetColor: Pointer;

  CompareFunc: TColorDistanceFunc;
  MaxDistance: Single;
  X, Y: Integer;
  RowPtr, Ptr: PColorBGRA;

  Cache: record
    Color: TColorBGRA;
    Dist: Single;
  end;

begin
  MACRO_FINDCOLORS_BEGIN

  TargetColor := @TargetColorContainer;

  case Formula of
    EColorSpace.RGB:
      begin
        CompareFunc := TColorDistanceFunc(@DistanceRGB_UnRolled);
        MaxDistance := DistanceRGB_Max(Multipliers);
        PColorRGB(TargetColor)^ := Color.ToRGB();
      end;

    EColorSpace.HSV:
      begin
        CompareFunc := TColorDistanceFunc(@DistanceHSV_UnRolled);
        MaxDistance := DistanceHSV_Max(Multipliers);
        PColorHSV(TargetColor)^ := Color.ToHSV();
      end;

    EColorSpace.HSL:
      begin
        CompareFunc := TColorDistanceFunc(@DistanceHSL_Unrolled);
        MaxDistance := DistanceHSL_Max(Multipliers);
        PColorHSL(TargetColor)^ := Color.ToHSL();
      end;

    EColorSpace.XYZ:
      begin
        CompareFunc := TColorDistanceFunc(@DistanceXYZ_UnRolled);
        MaxDistance := DistanceXYZ_Max(Multipliers);
        PColorXYZ(TargetColor)^ := Color.ToXYZ();
      end;

    EColorSpace.LAB:
      begin
        CompareFunc := TColorDistanceFunc(@DistanceLAB_UnRolled);
        MaxDistance := DistanceLAB_Max(Multipliers);
        PColorLAB(TargetColor)^ := Color.ToLAB();
      end;

    EColorSpace.LCH:
      begin
        CompareFunc := TColorDistanceFunc(@DistanceLCH_UnRolled);
        MaxDistance := DistanceLCH_Max(Multipliers);
        PColorLCH(TargetColor)^ := Color.ToLCH();
      end;

    EColorSpace.DeltaE:
      begin
        CompareFunc := TColorDistanceFunc(@DistanceDeltaE_UnRolled);
        MaxDistance := DistanceDeltaE_Max(Multipliers);
        PColorLAB(TargetColor)^ := Color.ToLAB();
      end;

    else
      SimbaException('MACRO_FINDCOLORS: Formula invalid!');
  end;

  if IsZero(MaxDistance{%H-}) or (SearchWidth <= 0) or (SearchHeight <= 0) or (Buffer = nil) or (BufferWidth <= 0) then
    Exit;

  RowPtr := Buffer;

  Dec(SearchHeight);
  Dec(SearchWidth);

  Cache.Color := RowPtr^;
  Cache.Dist := {%H-}CompareFunc(TargetColor, Cache.Color, Multipliers) / MaxDistance * 100;

  for Y := 0 to SearchHeight do
  begin
    Ptr := RowPtr;
    for X := 0 to SearchWidth do
    begin
      {$IFDEF SIMBA_BUFFERCHECKS}
      if (Ptr < BufferLo) or (Ptr > BufferHi) then
      begin
        DebugLn('Outside of buffer: %d, (Lo: %d, Hi: %d)', [PtrUInt(Ptr), PtrUInt(BufferLo), PtrUInt(BufferHi)]);
        Halt;
      end;
      {$ENDIF}

      if not Cache.Color.EqualsIgnoreAlpha(Ptr^) then
      begin
        Cache.Color := Ptr^;
        Cache.Dist  := CompareFunc(TargetColor, Cache.Color, Multipliers) / MaxDistance * 100;
      end;

      MACRO_FINDCOLORS_COMPARE

      Inc(Ptr);
    end;
    Inc(RowPtr, BufferWidth);
  end;

  MACRO_FINDCOLORS_END
end;
}

function FindColorsOnBuffer(Formula: EColorSpace; Color: TColor; Tolerance: Single; Multipliers: TChannelMultipliers;
                            Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer; OffsetX, OffsetY: Integer {$IFDEF SIMBA_BUFFERCHECKS}; BufferLo, BufferHi: PColorBGRA{$ENDIF}): TPointArray;
var
  PointBuffer: TSimbaPointBuffer;

  {$DEFINE MACRO_FINDCOLORS_BEGIN :=
    Result := [];
    PointBuffer.Init(16*1024);
  }
  {$DEFINE MACRO_FINDCOLORS_COMPARE :=
    if (Cache.Dist <= Tolerance) then
      PointBuffer.Add(X + OffsetX, Y + OffsetY);
  }
  {$DEFINE MACRO_FINDCOLORS_END :=
    Result := PointBuffer.ToArray(False);
  }
  MACRO_FINDCOLORS

function CountColorsOnBuffer(Formula: EColorSpace; Color: TColor; Tolerance: Single; Multipliers: TChannelMultipliers;
                             Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer {$IFDEF SIMBA_BUFFERCHECKS}; BufferLo, BufferHi: PColorBGRA{$ENDIF}): Integer;

  {$DEFINE MACRO_FINDCOLORS_BEGIN :=
    Result := 0;
  }
  {$DEFINE MACRO_FINDCOLORS_COMPARE :=
    if (Cache.Dist <= Tolerance) then
      Inc(Result);
  }
  {$DEFINE MACRO_FINDCOLORS_END := }
  MACRO_FINDCOLORS

function FindColorsOnTarget(Target: TSimbaTarget; Bounds: TBox;
                            Formula: EColorSpace; Color: TColor; Tolerance: Single; Multipliers: TChannelMultipliers): TPointArray;
var
  Buffer: PColorBGRA;
  BufferWidth: Integer;

  SliceResults: T2DPointArray;

  procedure Execute(const Index, Lo, Hi: Integer);
  begin
    SliceResults[Index] := FindColorsOnBuffer(
      Formula, Color, Tolerance, Multipliers,
      @Buffer[Lo * BufferWidth], BufferWidth, Bounds.Width, (Hi - Lo) + 1, Bounds.X1, Bounds.Y1 + Lo
      {$IFDEF SIMBA_BUFFERCHECKS}, Buffer, Buffer + (MemSize(Buffer) div SizeOf(TColorBGRA)) {$ENDIF}
    );
  end;

var
  ThreadsUsed: Integer;
  T: Double;
begin
  Result := [];

  if Target.GetImageData(Bounds, Buffer, BufferWidth) then
  try
    {$IFDEF SIMBA_BENCHMARKS}
    T := HighResolutionTime();
    {$ENDIF}

    SetLength(SliceResults, CalculateSlices(Bounds.Width, Bounds.Height)); // Cannot exceed this
    ThreadsUsed := SimbaThreadPool.RunParallel(Length(SliceResults), 0, Bounds.Height - 1, @Execute);
    Result := SliceResults.Merge();

    {$IFDEF SIMBA_BENCHMARKS}
    DebugLn('FindColors: ColorSpace=%s Width=%d Height=%d ThreadsUsed=%d Time=%f', [Formula.AsString(), Bounds.Width, Bounds.Height, ThreadsUsed, HighResolutionTime() - T]);
    {$ENDIF}
  finally
    Target.FreeImageData(Buffer);
  end;
end;

function CountColorsOnTarget(Target: TSimbaTarget; Bounds: TBox; Formula: EColorSpace; Color: TColor; Tolerance: Single; Multipliers: TChannelMultipliers): Integer;
var
  Buffer: PColorBGRA;
  BufferWidth: Integer;

  SliceResults: TIntegerArray;

  procedure Execute(const Index, Lo, Hi: Integer);
  begin
    SliceResults[Index] := CountColorsOnBuffer(
      Formula, Color, Tolerance, Multipliers,
      @Buffer[Lo * BufferWidth], BufferWidth, Bounds.Width, (Hi - Lo) + 1
      {$IFDEF SIMBA_BUFFERCHECKS}, Buffer, Buffer + (MemSize(Buffer) div SizeOf(TColorBGRA)) {$ENDIF}
    );
  end;

var
  ThreadsUsed: Integer;
  T: Double;
  I: Integer;
begin
  Result := 0;

  if Target.GetImageData(Bounds, Buffer, BufferWidth) then
  try
    {$IFDEF SIMBA_BENCHMARKS}
    T := HighResolutionTime();
    {$ENDIF}

    SetLength(SliceResults, CalculateSlices(Bounds.Width, Bounds.Height)); // Cannot exceed this
    ThreadsUsed := SimbaThreadPool.RunParallel(Length(SliceResults), 0, Bounds.Height - 1, @Execute);
    for I := 0 to High(SliceResults) do
      Result += SliceResults[I];

    {$IFDEF SIMBA_BENCHMARKS}
    DebugLn('CountColors: ColorSpace=%s Width=%d Height=%d ThreadsUsed=%d Time=%f', [Formula.AsString(), Bounds.Width, Bounds.Height, ThreadsUsed, HighResolutionTime() - T]);
    {$ENDIF}
  finally
    Target.FreeImageData(Buffer);
  end;
end;

function MatchColorsOnBuffer(Formula: EColorSpace; Color: TColor; Multipliers: TChannelMultipliers;
                             Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer {$IFDEF SIMBA_BUFFERCHECKS}; BufferLo, BufferHi: PColorBGRA{$ENDIF}): TSingleMatrix;

  {$DEFINE MACRO_FINDCOLORS_BEGIN :=
    Result.SetSize(SearchWidth, SearchHeight);
  }
  {$DEFINE MACRO_FINDCOLORS_COMPARE :=
    Result[Y, X] := 1 - Cache.Dist;
  }
  {$DEFINE MACRO_FINDCOLORS_END :=
    // Nothing
  }
  MACRO_FINDCOLORS

function MatchColorsOnTarget(Target: TSimbaTarget; Bounds: TBox;
                            Formula: EColorSpace; Color: TColor; Multipliers: TChannelMultipliers): TSingleMatrix;
var
  Buffer: PColorBGRA;
  BufferWidth: Integer;

  SliceResults: array of TSingleMatrix;

  procedure Execute(const Index, Lo, Hi: Integer);
  begin
    SliceResults[Index] := MatchColorsOnBuffer(
      Formula, Color, Multipliers,
      @Buffer[Lo * BufferWidth], BufferWidth, Bounds.Width, (Hi - Lo) + 1
      {$IFDEF SIMBA_BUFFERCHECKS}, Buffer, Buffer + (MemSize(Buffer) div SizeOf(TColorBGRA)) {$ENDIF}
    );
  end;

var
  ThreadsUsed: Integer;
  T: Double;
  I: Integer;
begin
  Result := [];

  if Target.GetImageData(Bounds, Buffer, BufferWidth) then
  try
    {$IFDEF SIMBA_BENCHMARKS}
    T := HighResolutionTime();
    {$ENDIF}

    SetLength(SliceResults, CalculateSlices(Bounds.Width, Bounds.Height)); // Cannot exceed this
    ThreadsUsed := SimbaThreadPool.RunParallel(Length(SliceResults), 0, Bounds.Height - 1, @Execute);
    for I := 0 to High(SliceResults) do
      Result += SliceResults[I];

    {$IFDEF SIMBA_BENCHMARKS}
    DebugLn('MatchColors: ColorSpace=%s Width=%d Height=%d ThreadsUsed=%d Time=%f', [Formula.AsString(), Bounds.Width, Bounds.Height, ThreadsUsed, HighResolutionTime() - T]);
    {$ENDIF}
  finally
    Target.FreeImageData(Buffer);
  end;
end;

initialization
  ColorFinderMultithreadOpts.Enabled     := True;
  ColorFinderMultithreadOpts.SliceWidth  := 125;
  ColorFinderMultithreadOpts.SliceHeight := 250;

end.
