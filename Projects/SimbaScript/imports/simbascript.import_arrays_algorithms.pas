unit simbascript.import_arrays_algorithms;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

implementation

uses
  simba.tpa, simba.math;

procedure Lape_RAaSTPAEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  RAaSTPAEx(PPointArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_RAaSTPA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  RAaSTPA(PPointArray(Params^[0])^, PInt32(Params^[1])^);
end;

procedure Lape_NearbyPointInArrayEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := NearbyPointInArrayEx(PPoint(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PPointArray(Params^[3])^);
end;

procedure Lape_NearbyPointInArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := NearbyPointInArray(PPoint(Params^[0])^, PInt32(Params^[1])^, PPointArray(Params^[2])^);
end;

procedure Lape_QuickTPASort(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  QuickTPASort(PIntegerArray(Params^[0])^, PPointArray(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PBoolean(Params^[4])^);
end;

procedure Lape_QuickATPASort(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  QuickATPASort(PIntegerArray(Params^[0])^, P2DPointArray(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PBoolean(Params^[4])^);
end;

procedure Lape_SortTPAByX(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortTPAByX(PPointArray(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure Lape_SortTPAByY(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortTPAByY(PPointArray(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure Lape_FindTPARows(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := FindTPARows(PPointArray(Params^[0])^);
end;

procedure Lape_FindTPAColumns(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := FindTPAColumns(PPointArray(Params^[0])^);
end;

procedure Lape_SortTPAFrom(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortTPAFrom(PPointArray(Params^[0])^, PPoint(Params^[1])^);
end;

procedure Lape_SortATPAFrom(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortATPAFrom(P2DPointArray(Params^[0])^, PPoint(Params^[1])^);
end;

procedure Lape_SortATPAFromFirstPoint(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortATPAFromFirstPoint(P2DPointArray(Params^[0])^, PPoint(Params^[1])^);
end;

procedure Lape_SortATPAFromMidPoint(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortATPAFromMidPoint(P2DPointArray(Params^[0])^, PPoint(Params^[1])^);
end;

procedure Lape_InvertTPA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  InvertTPA(PPointArray(Params^[0])^);
end;

procedure Lape_InvertATPA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  InvertATPA(P2DPointArray(Params^[0])^);
end;

procedure Lape_MiddleTPAEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := MiddleTPAEx(PPointArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_MiddleTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := MiddleTPA(PPointArray(Params^[0])^);
end;

procedure Lape_SortATPASize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortATPASize(P2DPointArray(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure Lape_SortATPAFromSize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortATPAFromSize(P2DPointArray(Params^[0])^, PInt32(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure Lape_SplitTPAEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := SplitTPAEx(PPointArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_SplitTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := SplitTPA(PPointArray(Params^[0])^, PInt32(Params^[1])^);
end;

procedure Lape_FloodFillTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := FloodFillTPA(PPointArray(Params^[0])^);
end;

procedure Lape_FilterPointsPie(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  FilterPointsPie(PPointArray(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, False);
end;

procedure Lape_FilterPointsDist(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  FilterPointsDist(PPointArray(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
end;

procedure Lape_FilterPointsLine(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  FilterPointsLine(PPointArray(Params^[0])^, PExtended(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
end;

procedure Lape_FilterTPADist(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  FilterTPADist(PPointArray(Params^[0])^, PInt32(Params^[1])^);
end;

procedure Lape_GetATPABounds(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := GetATPABounds(P2DPointArray(Params^[0])^);
end;

procedure Lape_GetTPABounds(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := GetTPABounds(PPointArray(Params^[0])^);
end;

procedure Lape_FindTPAinTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := FindTPAinTPA(PPointArray(Params^[0])^, PPointArray(Params^[1])^, PPointArray(Params^[2])^);
end;

procedure Lape_GetSamePointsATPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := GetSamePointsATPA(P2DPointArray(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure Lape_FindTextTPAinTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := FindTextTPAinTPA(PInt32(Params^[0])^, PPointArray(Params^[1])^, PPointArray(Params^[2])^, PPointArray(Params^[3])^);
end;

procedure Lape_SortCircleWise(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortCircleWise(PPointArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PBoolean(Params^[4])^, PBoolean(Params^[5])^);
end;

procedure Lape_LinearSort(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  LinearSort(PPointArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PBoolean(Params^[4])^);
end;

procedure Lape_RotatePoint(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := RotatePoint(PPoint(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^);
end;

procedure Lape_ChangeDistPT(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := ChangeDistPT(PPoint(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, Pextended(Params^[3])^);
end;

procedure Lape_ChangeDistTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := ChangeDistTPA(PPointArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, Pextended(Params^[3])^);
end;

procedure Lape_FindGapsTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := FindGapsTPA(PPointArray(Params^[0])^, PInt32(Params^[1])^);
end;

procedure Lape_RemoveDistTPointArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := RemoveDistTPointArray(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PPointArray(Params^[3])^, PBoolean(Params^[4])^);
end;

procedure Lape_CombineTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := CombineTPA(PPointArray(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure Lape_ReArrangeandShortenArrayEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := ReArrangeandShortenArrayEx(PPointArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_ReArrangeandShortenArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := ReArrangeandShortenArray(PPointArray(Params^[0])^, PInt32(Params^[1])^);
end;

procedure Lape_TPAtoATPAEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := TPAtoATPAEx(PPointArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_TPAtoATPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := TPAtoATPA(PPointArray(Params^[0])^, PInt32(Params^[1])^);
end;

procedure Lape_MergeATPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := MergeATPA(P2DPointArray(Params^[0])^);
end;

procedure Lape_AppendTPA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  AppendTPA(PPointArray(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure Lape_TPAFromLine(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := TPAFromLine(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_EdgeFromBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := EdgeFromBox(PBox(Params^[0])^);
end;

procedure Lape_TPAFromBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := TPAFromBox(PBox(Params^[0])^);
end;

procedure Lape_TPAFromEllipse(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := TPAFromEllipse(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_TPAFromCircle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := TPAFromCircle(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_FillEllipse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  FillEllipse(PPointArray(Params^[0])^);
end;

procedure Lape_RotatePoints(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := RotatePoints(PPointArray(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^);
end;

procedure Lape_FindTPAEdges(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := FindTPAEdges(PPointArray(Params^[0])^);
end;

procedure Lape_ClearTPAFromTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := ClearTPAFromTPA(PPointArray(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure Lape_ReturnPointsNotInTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := ReturnPointsNotInTPA(PPointArray(Params^[0])^, PBox(Params^[1])^);
end;

procedure Lape_PointInTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PointInTPA(PPoint(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure Lape_ClearDoubleTPA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  ClearDoubleTPA(PPointArray(Params^[0])^);
end;

procedure Lape_TPACountSort(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TPACountSort(PPointArray(Params^[0])^, PPoint(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure Lape_TPACountSortBase(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TPACountSortBase(PPointArray(Params^[0])^, PPoint(Params^[1])^, PPoint(Params^[2])^, PBoolean(Params^[3])^);
end;

procedure Lape_SameTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SameTPA(PPointArray(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure Lape_TPAInATPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := TPAInATPA(PPointArray(Params^[0])^, P2DPointArray(Params^[1])^, PLongInt(Params^[2])^);
end;

procedure Lape_OffsetTPA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  OffsetTPA(PPointArray(Params^[0])^, PPoint(Params^[1])^);
end;

procedure Lape_OffsetATPA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  OffsetATPA(P2DPointArray(Params^[0])^, PPoint(Params^[1])^);
end;

procedure Lape_CopyTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := CopyTPA(PPointArray(Params^[0])^);
end;

procedure Lape_CopyATPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := CopyATPA(P2DPointArray(Params^[0])^);
end;

procedure Lape_TPAPosNext(const Params: PParamArray;const Result: pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
 PInt32(Result)^:= TPAPosNext(PPoint(Params^[0])^, PPointArray(Params^[1])^, PInt32(Params^[2])^, PBoolean(Params^[3])^);
end;

procedure Lape_GlueTPAs(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
 PPointArray(Result)^:= GlueTPAs(PPointArray(Params^[0])^, PPointArray(Params^[1])^, PBoolean(Params^[2])^, PBoolean(Params^[3])^);
end;

procedure Lape_SortATPAFromFirstPointX(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortATPAFromFirstPointX(P2DPointArray(Params^[0])^, PPoint(Params^[1])^);
end;

procedure Lape_SortATPAFromFirstPointY(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortATPAFromFirstPointY(P2DPointArray(Params^[0])^, PPoint(Params^[1])^);
end;

procedure Lape_FilterTPAsBetween(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  filterTPAsBetween(P2DPointArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_MedianTPAEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  MedianTPAEx(PPointArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_MedianTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := MedianTPA(PPointArray(Params^[0])^);
end;

procedure Lape_FilterPointsBox(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  FilterPointsBox(PPointArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
end;

procedure Lape_TPAFromPolygon(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := TPAFromPolygon(PPointArray(Params^[0])^);
end;

procedure Lape_ClusterTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := ClusterTPA(PPointArray(Params^[0])^, PInt32(Params^[1])^);
end;

procedure Lape_ClusterTPAEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := ClusterTPAEx(PPointArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_PartitionTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := PartitionTPA(PPointArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_InvertTIA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  InvertTIA(PIntegerArray(Params^[0])^);
end;

procedure Lape_SumIntegerArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := SumIntegerArray(PIntegerArray(Params^[0])^);
end;

procedure Lape_AverageTIA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := AverageTIA(PIntegerArray(Params^[0])^);
end;

procedure Lape_AverageExtended(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := AverageExtended(PExtendedArray(Params^[0])^);
end;

procedure Lape_Quicksort(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Quicksort(PIntegerArray(Params^[0])^, Low(PIntegerArray(Params^[0])^), High(PIntegerArray(Params^[0])^));
end;

procedure Lape_InIntArrayEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := InIntArrayEx(PIntegerArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_InIntArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := InIntArray(PIntegerArray(Params^[0])^, PInt32(Params^[1])^);
end;

procedure Lape_ClearSameIntegers(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  ClearSameIntegers(PIntegerArray(Params^[0])^);
end;

procedure Lape_CombineIntArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := CombineIntArray(PIntegerArray(Params^[0])^, PIntegerArray(Params^[1])^);
end;

procedure Lape_Import_Arrays_Algorithms(Compiler: TScriptCompiler);
begin
  with Compiler do
  begin
    Section := 'Arrays & Algorithms';

    addGlobalFunc('procedure RAaSTPAEx(var a: TPointArray; const w, h: Int32);', @Lape_RAaSTPAEx);
    addGlobalFunc('procedure RAaSTPA(var a: TPointArray; const Dist: Int32);', @Lape_RAaSTPA);
    addGlobalFunc('function NearbyPointInArrayEx(const P: TPoint; w, h: Int32; const a: TPointArray): Boolean', @Lape_NearbyPointInArrayEx);
    addGlobalFunc('function NearbyPointInArray(const P: TPoint; Dist: Int32; const a: TPointArray): Boolean', @Lape_NearbyPointInArray);
    addGlobalFunc('procedure QuickTPASort(var A: TIntegerArray; var B: TPointArray; iLo, iHi: Int32; SortUp: Boolean);', @Lape_QuickTPASort);
    addGlobalFunc('procedure QuickATPASort(var A: TIntegerArray; var B: T2DPointArray; iLo, iHi: Int32; SortUp: Boolean);', @Lape_QuickATPASort);
    addGlobalFunc('procedure SortTPAByX(var a: TPointArray; const LowToHi: Boolean);', @Lape_SortTPAByX);
    addGlobalFunc('procedure SortTPAByY(var a: TPointArray; const LowToHi: Boolean);', @Lape_SortTPAByY);
    addGlobalFunc('function FindTPARows(a: TPointArray): T2DPointArray;', @Lape_FindTPARows);
    addGlobalFunc('function FindTPAColumns(a: TPointArray): T2DPointArray;', @Lape_FindTPAColumns);
    addGlobalFunc('procedure SortTPAFrom(var a: TPointArray; const From: TPoint);', @Lape_SortTPAFrom);
    addGlobalFunc('procedure SortATPAFrom(var a: T2DPointArray; const From: TPoint);', @Lape_SortATPAFrom);
    addGlobalFunc('procedure SortATPAFromFirstPoint(var a: T2DPointArray; const From: TPoint);', @Lape_SortATPAFromFirstPoint);
    addGlobalFunc('procedure SortATPAFromMidPoint(var a: T2DPointArray; const From: TPoint);', @Lape_SortATPAFromMidPoint);
    addGlobalFunc('procedure SortATPAFromFirstPointX(var a: T2DPointArray; const From: TPoint);', @Lape_SortATPAFromFirstPointX);
    addGlobalFunc('procedure SortATPAFromFirstPointY(var a: T2DPointArray; const From: TPoint);', @Lape_SortATPAFromFirstPointY);
    addGlobalFunc('procedure InvertTPA(var a: TPointArray);', @Lape_InvertTPA);
    addGlobalFunc('procedure InvertATPA(var a: T2DPointArray);', @Lape_InvertATPA);
    addGlobalFunc('function MiddleTPAEx(const TPA: TPointArray; var x, y: Int32): Boolean', @Lape_MiddleTPAEx);
    addGlobalFunc('function MiddleTPA(const tpa: TPointArray): TPoint', @Lape_MiddleTPA);
    addGlobalFunc('procedure MedianTPAEx(var tpa: TPointArray; out x, y: Int32);', @Lape_MedianTPAEx);
    addGlobalFunc('function MedianTPA(var tpa: TPointArray): TPoint;', @Lape_MedianTPA);
    addGlobalFunc('procedure SortATPASize(var a: T2DPointArray; const BigFirst: Boolean);', @Lape_SortATPASize);
    addGlobalFunc('procedure SortATPAFromSize(var a: T2DPointArray; const Size: Int32; CloseFirst: Boolean);', @Lape_SortATPAFromSize);
    addGlobalFunc('procedure FilterTPAsBetween(var atpa: T2DPointArray; const minLength, maxLength: Int32);', @Lape_FilterTPAsBetween);
    addGlobalFunc('function SplitTPAEx(const arr: TPointArray; w, h: Int32): T2DPointArray', @Lape_SplitTPAEx);
    addGlobalFunc('function SplitTPA(const arr: TPointArray; Dist: Int32): T2DPointArray', @Lape_SplitTPA);
    addGlobalFunc('function ClusterTPAEx(const TPA: TPointArray; width, height: Int32): T2DPointArray;', @Lape_ClusterTPAEx);
    addGlobalFunc('function ClusterTPA(const TPA: TPointArray; dist: Int32): T2DPointArray;', @Lape_ClusterTPA);
    addGlobalFunc('function FloodFillTPA(const TPA: TPointArray): T2DPointArray', @Lape_FloodFillTPA);
    addGlobalFunc('procedure FilterPointsPie(var Points: TPointArray; const SD, ED, MinR, MaxR: Extended; Mx, My: Int32);', @Lape_FilterPointsPie);
    addGlobalFunc('procedure FilterPointsDist(var Points: TPointArray; const MinDist, MaxDist: Extended; Mx, My: Int32);', @Lape_FilterPointsDist);
    addGlobalFunc('procedure FilterPointsLine(var Points: TPointArray; Radial: Extended; Radius, MX, MY: Int32);', @Lape_FilterPointsLine);
    addGlobalFunc('procedure FilterPointsBox(var points: TPointArray; x1, y1, x2, y2: Int32);', @Lape_FilterPointsBox);
    addGlobalFunc('procedure FilterTPADist(var TPA: TPointArray; maxDist: Int32);', @Lape_FilterTPADist);
    addGlobalFunc('function GetATPABounds(const ATPA: T2DPointArray): TBox', @Lape_GetATPABounds);
    addGlobalFunc('function GetTPABounds(const TPA: TPointArray): TBox', @Lape_GetTPABounds);
    addGlobalFunc('function FindTPAinTPA(const SearchTPA, TotalTPA: TPointArray; var Matches: TPointArray): Boolean', @Lape_FindTPAinTPA);
    addGlobalFunc('function GetSamePointsATPA(const ATPA: T2DPointArray; var Matches: TPointArray): boolean', @Lape_GetSamePointsATPA);
    addGlobalFunc('function FindTextTPAinTPA(Height: Int32; const SearchTPA, TotalTPA: TPointArray; var Matches: TPointArray): Boolean', @Lape_FindTextTPAinTPA);
    addGlobalFunc('procedure SortCircleWise(var tpa: TPointArray; const cx, cy, StartDegree: Int32; SortUp, ClockWise: Boolean);', @Lape_SortCircleWise);
    addGlobalFunc('procedure LinearSort(var tpa: TPointArray; cx, cy, sd: Int32; SortUp: Boolean);', @Lape_LinearSort);
    addGlobalFunc('function RotatePoint(Const p: TPoint; angle, mx, my: Extended): TPoint', @Lape_RotatePoint);
    addGlobalFunc('function ChangeDistPT(const PT: TPoint; mx,my: Int32; newdist: extended): TPoint', @Lape_ChangeDistPT);
    addGlobalFunc('function ChangeDistTPA(var TPA: TPointArray; mx,my: Int32; newdist: extended): boolean', @Lape_ChangeDistTPA);
    addGlobalFunc('function FindGapsTPA(const TPA: TPointArray; MinPixels: Int32): T2DPointArray', @Lape_FindGapsTPA);
    addGlobalFunc('function RemoveDistTPointArray(x, y, dist: Int32; const ThePoints: TPointArray; RemoveHigher: Boolean): TPointArray', @Lape_RemoveDistTPointArray);
    addGlobalFunc('function CombineTPA(const Ar1, Ar2: TPointArray): TPointArray', @Lape_CombineTPA);
    addGlobalFunc('function ReArrangeandShortenArrayEx(const a: TPointArray; w, h: Int32): TPointArray', @Lape_ReArrangeandShortenArrayEx);
    addGlobalFunc('function ReArrangeandShortenArray(const a: TPointArray; Dist: Int32): TPointArray', @Lape_ReArrangeandShortenArray);
    addGlobalFunc('function TPAtoATPAEx(const TPA: TPointArray; w, h: Int32): T2DPointArray', @Lape_TPAtoATPAEx);
    addGlobalFunc('function TPAtoATPA(const TPA: TPointArray; Dist: Int32): T2DPointArray', @Lape_TPAtoATPA);
    addGlobalFunc('function MergeATPA(const ATPA: T2DPointArray): TPointArray', @Lape_MergeATPA);
    addGlobalFunc('procedure AppendTPA(var TPA: TPointArray; const ToAppend: TPointArray);', @Lape_AppendTPA);
    addGlobalFunc('function TPAFromLine(const x1, y1, x2, y2: Int32): TPointArray', @Lape_TPAFromLine);
    addGlobalFunc('function EdgeFromBox(const Box: TBox): TPointArray', @Lape_EdgeFromBox);
    addGlobalFunc('function TPAFromBox(const Box: TBox): TPointArray', @Lape_TPAFromBox);
    addGlobalFunc('function TPAFromEllipse(const CX, CY, XRadius, YRadius : Int32) : TPointArray', @Lape_TPAFromEllipse);
    addGlobalFunc('function TPAFromCircle(const CX, CY, Radius : Int32) : TPointArray', @Lape_TPAFromCircle);
    addGlobalFunc('function TPAFromPolygon(const Shape: TPointArray) : TPointArray', @Lape_TPAFromPolygon);
    addGlobalFunc('procedure FillEllipse(var a: TPointArray);', @Lape_FillEllipse);
    addGlobalFunc('function RotatePoints(const P: TPointArray; A, cx, cy: Extended): TPointArray', @Lape_RotatePoints);
    addGlobalFunc('function FindTPAEdges(const p: TPointArray): TPointArray', @Lape_FindTPAEdges);
    addGlobalFunc('function ClearTPAFromTPA(const arP, ClearPoints: TPointArray): TPointArray', @Lape_ClearTPAFromTPA);
    addGlobalFunc('function ReturnPointsNotInTPA(Const TotalTPA: TPointArray; const Box: TBox): TPointArray', @Lape_ReturnPointsNotInTPA);
    addGlobalFunc('function PointInTPA(p: TPoint; const arP: TPointArray): Boolean', @Lape_PointInTPA);
    addGlobalFunc('procedure ClearDoubleTPA(var TPA: TPointArray);', @Lape_ClearDoubleTPA);
    addGlobalFunc('procedure TPACountSort(var TPA: TPointArray; const max: TPoint; Const SortOnX: Boolean);', @Lape_TPACountSort);
    addGlobalFunc('procedure TPACountSortBase(var TPA: TPointArray; const maxx, base: TPoint; const SortOnX: Boolean);', @Lape_TPACountSortBase);
    addGlobalFunc('function SameTPA(const aTPA, bTPA: TPointArray): Boolean', @Lape_SameTPA);
    addGlobalFunc('function TPAInATPA(const TPA: TPointArray; const InATPA: T2DPointArray; var Index: LongInt): Boolean', @Lape_TPAInATPA);
    addGlobalFunc('procedure OffsetTPA(var TPA: TPointArray; const Offset: TPoint);', @Lape_OffsetTPA);
    addGlobalFunc('procedure OffsetATPA(var ATPA: T2DPointArray; const Offset: TPoint);', @Lape_OffsetATPA);
    addGlobalFunc('function CopyTPA(const TPA: TPointArray): TPointArray', @Lape_CopyTPA);
    addGlobalFunc('function CopyATPA(const ATPA: T2DPointArray): T2DPointArray', @Lape_CopyATPA);
    addGlobalFunc('function TPAPosNext(const Find: TPoint; const V: TPointArray; const PrevPos: Int32;const IsSortedAscending: Boolean): Int32;',@Lape_TPAPosNext);
    addGlobalFunc('function GlueTPAs(const V1, V2: TPointArray; const IsSortedAscending, byDifference: Boolean): TPointArray;',@Lape_GlueTPAs);
    addGlobalFunc('function PartitionTPA(const TPA: TPointArray; BoxWidth, BoxHeight: Int32): T2DPointArray;', @Lape_PartitionTPA);
    addGlobalFunc('procedure Quicksort(var Arr: TIntegerArray);', @Lape_Quicksort);
    addGlobalFunc('function InIntArrayEx(const Arr: TIntegerArray; var Index: Int32; const Number: Int32): Boolean', @Lape_InIntArrayEx);
    addGlobalFunc('function InIntArray(const Arr: TIntegerArray; Number: Int32): Boolean', @Lape_InIntArray);
    addGlobalFunc('procedure ClearSameIntegers(var Arr: TIntegerArray);', @Lape_ClearSameIntegers);
    addGlobalFunc('function CombineIntArray(const Arr1, Arr2: TIntegerArray): TIntegerArray', @Lape_CombineIntArray);
    addGlobalFunc('procedure InvertTIA(var Arr: TIntegerArray);', @Lape_InvertTIA);
    addGlobalFunc('function SumIntegerArray(const Arr: TIntegerArray): Int32', @Lape_SumIntegerArray);
    addGlobalFunc('function AverageTIA(const Arr: TIntegerArray): Int32', @Lape_AverageTIA);
    addGlobalFunc('function AverageExtended(const Arr: TExtendedArray): Extended', @Lape_AverageExtended);
  end;
end;

initialization
  RegisterScriptImport(@Lape_Import_Arrays_Algorithms);

end.

