{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  The colorfinder.
  Lots of code from: https://github.com/slackydev/colorlib
}
unit simba.finder_color;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Math,
  simba.base,
  simba.colormath;

function SimbaFinder_FindColors(Data: PColorBGRA; PixelsPerRow, AWidth, AHeight: Integer; Offset: TPoint;
                                ColorSpace: EColorSpace; Color: TColor; Tolerance: Single; Multipliers: TChannelMultipliers): TPointArray;

function SimbaFinder_CountColors(Data: PColorBGRA; PixelsPerRow, AWidth, AHeight: Integer;
                                 ColorSpace: EColorSpace; Color: TColor; Tolerance: Single; Multipliers: TChannelMultipliers;
                                 MaxToFind: Integer = -1): Integer;

// each pixel's distance from Color, the number the finders compare with Tolerance:
// 0 is an exact match, 100 as far away as the colour space goes
function SimbaFinder_MatchColors(Data: PColorBGRA; PixelsPerRow, AWidth, AHeight: Integer;
                                 ColorSpace: EColorSpace; Color: TColor; Multipliers: TChannelMultipliers): TSingleMatrix;

implementation

uses
  simba.colormath_distance,
  simba.colormath_distance_unrolled,
  simba.vartype_pointarray,
  simba.vartype_matrix,
  simba.container_point,
  simba.threading,
  simba.multiprocessing;

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
  Cache.Dist := {%H-}CompareFunc(TargetColor, @Cache.Color, Multipliers) / MaxDistance * 100;

  for Y := 0 to SearchHeight do
  begin
    Ptr := RowPtr;
    for X := 0 to SearchWidth do
    begin
      if not Cache.Color.EqualsIgnoreAlpha(Ptr^) then
      begin
        Cache.Color := Ptr^;
        Cache.Dist  := CompareFunc(TargetColor, @Cache.Color, Multipliers) / MaxDistance * 100;
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

function FindColorsSlice(Formula: EColorSpace; Color: TColor; Tolerance: Single; Multipliers: TChannelMultipliers;
                         Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer; OffsetX, OffsetY: Integer): TPointArray;
var
  PointBuffer: TPointBuffer;

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

function CountColorsSlice(var Limit: TLimit;
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

function SimbaFinder_FindColors(Data: PColorBGRA; PixelsPerRow, AWidth, AHeight: Integer; Offset: TPoint;
                                ColorSpace: EColorSpace; Color: TColor; Tolerance: Single; Multipliers: TChannelMultipliers): TPointArray;
var
  SliceResults: T2DPointArray;

  procedure Execute(const Index, Lo, Hi: Integer);
  begin
    SliceResults[Index] := FindColorsSlice(
      ColorSpace, Color, Tolerance, Multipliers,
      @Data[Lo * PixelsPerRow], PixelsPerRow, AWidth, (Hi - Lo) + 1, Offset.X, Offset.Y + Lo
    );
  end;

begin
  Result := [];
  if (Data = nil) or (AWidth <= 0) or (AHeight <= 0) then
    Exit;

  SetLength(SliceResults, SimbaMultiprocessing.ThreadsForArea(AWidth, AHeight)); // Cannot exceed this
  SimbaMultiprocessing.Run(Length(SliceResults), 0, AHeight - 1, @Execute);

  Result := SliceResults.Merge();
end;

function SimbaFinder_CountColors(Data: PColorBGRA; PixelsPerRow, AWidth, AHeight: Integer;
                                 ColorSpace: EColorSpace; Color: TColor; Tolerance: Single; Multipliers: TChannelMultipliers;
                                 MaxToFind: Integer): Integer;
var
  Limit: TLimit;

  procedure Execute(const Index, Lo, Hi: Integer);
  begin
    CountColorsSlice(
      Limit,
      ColorSpace, Color, Tolerance, Multipliers,
      @Data[Lo * PixelsPerRow], PixelsPerRow, AWidth, (Hi - Lo) + 1
    );
  end;

begin
  Result := 0;
  if (Data = nil) or (AWidth <= 0) or (AHeight <= 0) then
    Exit;

  Limit := TLimit.Create(MaxToFind);
  SimbaMultiprocessing.Run(SimbaMultiprocessing.ThreadsForArea(AWidth, AHeight), 0, AHeight - 1, @Execute);

  Result := Limit.Count;
end;

function MatchColorsSlice(Formula: EColorSpace; Color: TColor; Multipliers: TChannelMultipliers;
                          Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer): TSingleMatrix;

  {$DEFINE MACRO_FINDCOLORS_BEGIN :=
    Result.SetSize(SearchWidth, SearchHeight);
  }
  {$DEFINE MACRO_FINDCOLORS_COMPARE :=
    Result[Y, X] := Cache.Dist;
  }
  {$DEFINE MACRO_FINDCOLORS_ROW :=
    // Nothing
  }
  {$DEFINE MACRO_FINDCOLORS_END :=
    // Nothing
  }
  MACRO_FINDCOLORS

function SimbaFinder_MatchColors(Data: PColorBGRA; PixelsPerRow, AWidth, AHeight: Integer;
                                 ColorSpace: EColorSpace; Color: TColor; Multipliers: TChannelMultipliers): TSingleMatrix;
var
  SliceResults: array of TSingleMatrix;

  procedure Execute(const Index, Lo, Hi: Integer);
  begin
    SliceResults[Index] := MatchColorsSlice(
      ColorSpace, Color, Multipliers,
      @Data[Lo * PixelsPerRow], PixelsPerRow, AWidth, (Hi - Lo) + 1
    );
  end;

var
  I: Integer;
begin
  Result := [];
  if (Data = nil) or (AWidth <= 0) or (AHeight <= 0) then
    Exit;

  SetLength(SliceResults, SimbaMultiprocessing.ThreadsForArea(AWidth, AHeight)); // Cannot exceed this
  SimbaMultiprocessing.Run(Length(SliceResults), 0, AHeight - 1, @Execute);

  for I := 0 to High(SliceResults) do
    Result += SliceResults[I];
end;

end.
