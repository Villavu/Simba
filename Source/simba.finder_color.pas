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

{.$DEFINE SIMBA_BENCHMARKS}

interface

uses
  Classes, SysUtils, Math, Graphics,
  simba.base, simba.colormath, simba.colormath_distance, simba.target, simba.threading;

function FindColorsOnTarget(constref Target: TSimbaTarget; Bounds: TBox;
                            Formula: EColorSpace; Color: TColor; Tolerance: Single; Multipliers: TChannelMultipliers): TPointArray;

function FindColorsOnBuffer(Formula: EColorSpace; Color: TColor; Tolerance: Single; Multipliers: TChannelMultipliers;
                            Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer; OffsetX, OffsetY: Integer): TPointArray;

function CountColorsOnTarget(constref Target: TSimbaTarget; Bounds: TBox;
                             Formula: EColorSpace; Color: TColor; Tolerance: Single; Multipliers: TChannelMultipliers;
                             MaxToFind: Integer = -1): Integer;

function CountColorsOnBuffer(var Limit: TLimit;
                             Formula: EColorSpace; Color: TColor; Tolerance: Single; Multipliers: TChannelMultipliers;
                             Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer): Integer;

function MatchColorsOnTarget(constref Target: TSimbaTarget; Bounds: TBox;
                             Formula: EColorSpace; Color: TColor; Multipliers: TChannelMultipliers): TSingleMatrix;

function MatchColorsOnBuffer(Formula: EColorSpace; Color: TColor; Multipliers: TChannelMultipliers;
                             Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer): TSingleMatrix;

function GetColorOnTarget(constref Target: TSimbaTarget; P: TPoint): TColor;

function GetColorsOnBuffer(Points: TPointArray; Offset: TPoint; Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer): TColorArray;

function GetColorsOnTarget(constref Target: TSimbaTarget; Points: TPointArray): TColorArray;

function GetColorsMatrixOnBuffer(Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer): TIntegerMatrix;

function GetColorsMatrixOnTarget(constref Target: TSimbaTarget; Bounds: TBox): TIntegerMatrix;

var
  ColorFinderMultithreadOpts: record
    Enabled: Boolean;
    SliceWidth, SliceHeight: Integer;
  end;

implementation

uses
  simba.containers, simba.colormath_conversion, simba.colormath_distance_unrolled,
  simba.vartype_pointarray, simba.vartype_floatmatrix, simba.vartype_box;

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
      if not Cache.Color.EqualsIgnoreAlpha(Ptr^) then
      begin
        Cache.Color := Ptr^;
        Cache.Dist  := CompareFunc(TargetColor, Cache.Color, Multipliers) / MaxDistance * 100;
      end;

      MACRO_FINDCOLORS_COMPARE

      Inc(Ptr);
    end;

    MACRO_FINDCOLORS_ROW

    Inc(RowPtr, BufferWidth);
  end;

  MACRO_FINDCOLORS_END
end;
}

function FindColorsOnBuffer(Formula: EColorSpace; Color: TColor; Tolerance: Single; Multipliers: TChannelMultipliers;
                            Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer; OffsetX, OffsetY: Integer): TPointArray;
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
  {$DEFINE MACRO_FINDCOLORS_ROW :=
    // Nothing
  }
  {$DEFINE MACRO_FINDCOLORS_END :=
    Result := PointBuffer.ToArray(False);
  }
  MACRO_FINDCOLORS

function CountColorsOnBuffer(var Limit: TLimit;
                             Formula: EColorSpace; Color: TColor; Tolerance: Single; Multipliers: TChannelMultipliers;
                             Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer): Integer;

  {$DEFINE MACRO_FINDCOLORS_BEGIN :=
    Result := 0;
  }
  {$DEFINE MACRO_FINDCOLORS_COMPARE :=
    if (Cache.Dist <= Tolerance) then
      Limit.Inc();
  }
  {$DEFINE MACRO_FINDCOLORS_ROW :=
    if Limit.Reached() then Exit;
  }
  {$DEFINE MACRO_FINDCOLORS_END :=
    Result := Limit.Count;
  }
  MACRO_FINDCOLORS

function FindColorsOnTarget(constref Target: TSimbaTarget; Bounds: TBox;
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

function CountColorsOnTarget(constref Target: TSimbaTarget; Bounds: TBox; Formula: EColorSpace; Color: TColor; Tolerance: Single; Multipliers: TChannelMultipliers; MaxToFind: Integer): Integer;
var
  Limit: TLimit;

  Buffer: PColorBGRA;
  BufferWidth: Integer;

  procedure Execute(const Index, Lo, Hi: Integer);
  begin
    CountColorsOnBuffer(
      Limit,
      Formula, Color, Tolerance, Multipliers,
      @Buffer[Lo * BufferWidth], BufferWidth, Bounds.Width, (Hi - Lo) + 1
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

    Limit := TLimit.Create(MaxToFind);
    ThreadsUsed := SimbaThreadPool.RunParallel(CalculateSlices(Bounds.Width, Bounds.Height), 0, Bounds.Height - 1, @Execute);
    Result := Limit.Count;

    {$IFDEF SIMBA_BENCHMARKS}
    DebugLn('CountColors: ColorSpace=%s Width=%d Height=%d ThreadsUsed=%d Time=%f', [Formula.AsString(), Bounds.Width, Bounds.Height, ThreadsUsed, HighResolutionTime() - T]);
    {$ENDIF}
  finally
    Target.FreeImageData(Buffer);
  end;
end;

function MatchColorsOnBuffer(Formula: EColorSpace; Color: TColor; Multipliers: TChannelMultipliers;
                             Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer): TSingleMatrix;

  {$DEFINE MACRO_FINDCOLORS_BEGIN :=
    Result.SetSize(SearchWidth, SearchHeight);
  }
  {$DEFINE MACRO_FINDCOLORS_COMPARE :=
    Result[Y, X] := 1 - Cache.Dist;
  }
  {$DEFINE MACRO_FINDCOLORS_ROW :=
    // Nothing
  }
  {$DEFINE MACRO_FINDCOLORS_END :=
    // Nothing
  }
  MACRO_FINDCOLORS

function MatchColorsOnTarget(constref Target: TSimbaTarget; Bounds: TBox;
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

procedure HasColorOnBuffer(Formula: EColorSpace; Color: TColor; Tolerance: Single; Multipliers: TChannelMultipliers;
                           Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer;
                           var Limit: TLimit);

  {$DEFINE MACRO_FINDCOLORS_BEGIN :=
    // Nothing
  }
  {$DEFINE MACRO_FINDCOLORS_COMPARE :=
    if (Cache.Dist <= Tolerance) then
      Limit.Inc();
  }
  {$DEFINE MACRO_FINDCOLORS_ROW :=
    if Limit.Reached() then Exit;
  }
  {$DEFINE MACRO_FINDCOLORS_END :=
    // Nothing
  }
  MACRO_FINDCOLORS

function HasColorOnTarget(Target: TSimbaTarget; Bounds: TBox; Formula: EColorSpace; Color: TColor; Tolerance: Single; Multipliers: TChannelMultipliers; MinCount: Integer): Boolean;
var
  Buffer: PColorBGRA;
  BufferWidth: Integer;

  Limit: TLimit;

  procedure Execute(const Index, Lo, Hi: Integer);
  begin
    HasColorOnBuffer(
      Formula, Color, Tolerance, Multipliers,
      @Buffer[Lo * BufferWidth], BufferWidth, Bounds.Width, (Hi - Lo) + 1,
      Limit
    );
  end;

var
  ThreadsUsed: Integer;
  T: Double;
  I: Integer;
begin
  Limit := TLimit.Create(Max(MinCount, 1));

  if Target.GetImageData(Bounds, Buffer, BufferWidth) then
  try
    {$IFDEF SIMBA_BENCHMARKS}
    T := HighResolutionTime();
    {$ENDIF}

    ThreadsUsed := SimbaThreadPool.RunParallel(CalculateSlices(Bounds.Width, Bounds.Height), 0, Bounds.Height - 1, @Execute);

    {$IFDEF SIMBA_BENCHMARKS}
    DebugLn('HasColors: ColorSpace=%s Width=%d Height=%d ThreadsUsed=%d Time=%f', [Formula.AsString(), Bounds.Width, Bounds.Height, ThreadsUsed, HighResolutionTime() - T]);
    {$ENDIF}
  finally
    Target.FreeImageData(Buffer);
  end;

  Result := Limit.Reached;
end;

function GetColorOnTarget(constref Target: TSimbaTarget; P: TPoint): TColor;
var
  B: TBox;
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  Result := -1;

  B.X1 := P.X; B.Y1 := P.Y;
  B.X2 := P.X; B.Y2 := P.Y;

  if Target.GetImageData(B, Data, DataWidth) then
  try
    Result := TSimbaColorConversion.BGRAToColor(Data^);
  finally
    Target.FreeImageData(Data);
  end;
end;

function GetColorsOnBuffer(Points: TPointArray; Offset: TPoint; Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer): TColorArray;
var
  Count, I, X, Y: Integer;
begin
  Count := 0;

  SetLength(Result, Length(Points));
  for I := 0 to High(Points) do
  begin
    X := Points[I].X + Offset.X;
    Y := Points[I].Y + Offset.Y;
    if (X >= 0) and (Y >= 0) and (X < SearchWidth) and (Y < SearchHeight) then
    begin
      Result[Count] := TSimbaColorConversion.BGRAToColor(Buffer[Y * BufferWidth + X]);
      Inc(Count);
    end;
  end;
  SetLength(Result, Count);
end;

function GetColorsOnTarget(constref Target: TSimbaTarget; Points: TPointArray): TColorArray;
var
  Bounds, SearchBounds: TBox;
  Buffer: PColorBGRA;
  BufferWidth: Integer;
begin
  Bounds := Points.Bounds;
  SearchBounds := Bounds;

  if Target.GetImageData(SearchBounds, Buffer, BufferWidth) then
  try
    Result := GetColorsOnBuffer(Points, TPoint.Create(-Bounds.X1, -Bounds.Y1), Buffer, BufferWidth, SearchBounds.Width, SearchBounds.Height);
  finally
    Target.FreeImageData(Buffer);
  end;
end;

function GetColorsMatrixOnBuffer(Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer): TIntegerMatrix;
var
  RowPtr, Ptr: PColorBGRA;
  X, Y: Integer;
begin
  SetLength(Result, SearchHeight, SearchWidth);
  Dec(SearchWidth);
  Dec(SearchHeight);

  RowPtr := Buffer;
  for Y := 0 to SearchHeight do
  begin
    Ptr := RowPtr;

    for X := 0 to SearchWidth do
    begin
      Result[Y, X] := TSimbaColorConversion.BGRAToColor(Ptr^);
      Inc(Ptr);
    end;

    Inc(RowPtr, BufferWidth);
  end;
end;

function GetColorsMatrixOnTarget(constref Target: TSimbaTarget; Bounds: TBox): TIntegerMatrix;
var
  Buffer: PColorBGRA;
  BufferWidth: Integer;
begin
  if Target.GetImageData(Bounds, Buffer, BufferWidth) then
  try
    Result := GetColorsMatrixOnBuffer(Buffer, BufferWidth, Bounds.Width, Bounds.Height);
  finally
    Target.FreeImageData(Buffer);
  end;
end;

initialization
  ColorFinderMultithreadOpts.Enabled     := True;
  ColorFinderMultithreadOpts.SliceWidth  := 250;
  ColorFinderMultithreadOpts.SliceHeight := 250;

end.
