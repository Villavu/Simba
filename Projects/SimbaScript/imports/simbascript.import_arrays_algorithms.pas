unit simbascript.import_arrays_algorithms;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_Arrays_Algorithms(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  simba.tpa, simba.math;

procedure Lape_RAaSTPAEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  RAaSTPAEx(PPointArray(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_RAaSTPA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  RAaSTPA(PPointArray(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_NearbyPointInArrayEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := NearbyPointInArrayEx(PPoint(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PPointArray(Params^[4])^);
end;

procedure Lape_NearbyPointInArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := NearbyPointInArray(PPoint(Params^[1])^, PInt32(Params^[2])^, PPointArray(Params^[3])^);
end;

procedure Lape_QuickTPASort(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  QuickTPASort(PIntegerArray(Params^[1])^, PPointArray(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PBoolean(Params^[5])^);
end;

procedure Lape_QuickATPASort(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  QuickATPASort(PIntegerArray(Params^[1])^, P2DPointArray(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PBoolean(Params^[5])^);
end;

procedure Lape_SortTPAByX(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortTPAByX(PPointArray(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure Lape_SortTPAByY(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortTPAByY(PPointArray(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure Lape_FindTPARows(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := FindTPARows(PPointArray(Params^[1])^);
end;

procedure Lape_FindTPAColumns(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := FindTPAColumns(PPointArray(Params^[1])^);
end;

procedure Lape_SortTPAFrom(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortTPAFrom(PPointArray(Params^[1])^, PPoint(Params^[2])^);
end;

procedure Lape_SortATPAFrom(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortATPAFrom(P2DPointArray(Params^[1])^, PPoint(Params^[2])^);
end;

procedure Lape_SortATPAFromFirstPoint(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortATPAFromFirstPoint(P2DPointArray(Params^[1])^, PPoint(Params^[2])^);
end;

procedure Lape_SortATPAFromMidPoint(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortATPAFromMidPoint(P2DPointArray(Params^[1])^, PPoint(Params^[2])^);
end;

procedure Lape_InvertTPA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  InvertTPA(PPointArray(Params^[1])^);
end;

procedure Lape_InvertATPA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  InvertATPA(P2DPointArray(Params^[1])^);
end;

procedure Lape_MiddleTPAEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := MiddleTPAEx(PPointArray(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_MiddleTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := MiddleTPA(PPointArray(Params^[1])^);
end;

procedure Lape_SortATPASize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortATPASize(P2DPointArray(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure Lape_SortATPAFromSize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortATPAFromSize(P2DPointArray(Params^[1])^, PInt32(Params^[2])^, PBoolean(Params^[3])^);
end;

procedure Lape_SplitTPAEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := SplitTPAEx(PPointArray(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_SplitTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := SplitTPA(PPointArray(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_FloodFillTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := FloodFillTPA(PPointArray(Params^[1])^);
end;

procedure Lape_FilterPointsPie(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  FilterPointsPie(PPointArray(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, False);
end;

procedure Lape_FilterPointsDist(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  FilterPointsDist(PPointArray(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^);
end;

procedure Lape_FilterPointsLine(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  FilterPointsLine(PPointArray(Params^[1])^, PExtended(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^);
end;

procedure Lape_FilterTPADist(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  FilterTPADist(PPointArray(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_GetATPABounds(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := GetATPABounds(P2DPointArray(Params^[1])^);
end;

procedure Lape_GetTPABounds(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := GetTPABounds(PPointArray(Params^[1])^);
end;

procedure Lape_FindTPAinTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := FindTPAinTPA(PPointArray(Params^[1])^, PPointArray(Params^[2])^, PPointArray(Params^[3])^);
end;

procedure Lape_GetSamePointsATPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := GetSamePointsATPA(P2DPointArray(Params^[1])^, PPointArray(Params^[2])^);
end;

procedure Lape_FindTextTPAinTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := FindTextTPAinTPA(PInt32(Params^[1])^, PPointArray(Params^[2])^, PPointArray(Params^[3])^, PPointArray(Params^[4])^);
end;

procedure Lape_SortCircleWise(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortCircleWise(PPointArray(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PBoolean(Params^[5])^, PBoolean(Params^[6])^);
end;

procedure Lape_LinearSort(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  LinearSort(PPointArray(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PBoolean(Params^[5])^);
end;

procedure Lape_RotatePoint(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := RotatePoint(PPoint(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^);
end;

procedure Lape_ChangeDistPT(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := ChangeDistPT(PPoint(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, Pextended(Params^[4])^);
end;

procedure Lape_ChangeDistTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := ChangeDistTPA(PPointArray(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, Pextended(Params^[4])^);
end;

procedure Lape_FindGapsTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := FindGapsTPA(PPointArray(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_RemoveDistTPointArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := RemoveDistTPointArray(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PPointArray(Params^[4])^, PBoolean(Params^[5])^);
end;

procedure Lape_CombineTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := CombineTPA(PPointArray(Params^[1])^, PPointArray(Params^[2])^);
end;

procedure Lape_ReArrangeandShortenArrayEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := ReArrangeandShortenArrayEx(PPointArray(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_ReArrangeandShortenArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := ReArrangeandShortenArray(PPointArray(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_TPAtoATPAEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := TPAtoATPAEx(PPointArray(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_TPAtoATPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := TPAtoATPA(PPointArray(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_MergeATPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := MergeATPA(P2DPointArray(Params^[1])^);
end;

procedure Lape_AppendTPA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  AppendTPA(PPointArray(Params^[1])^, PPointArray(Params^[2])^);
end;

procedure Lape_TPAFromLine(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := TPAFromLine(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
end;

procedure Lape_EdgeFromBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := EdgeFromBox(PBox(Params^[1])^);
end;

procedure Lape_TPAFromBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := TPAFromBox(PBox(Params^[1])^);
end;

procedure Lape_TPAFromEllipse(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := TPAFromEllipse(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
end;

procedure Lape_TPAFromCircle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := TPAFromCircle(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_FillEllipse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  FillEllipse(PPointArray(Params^[1])^);
end;

procedure Lape_RotatePoints(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := RotatePoints(PPointArray(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^);
end;

procedure Lape_FindTPAEdges(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := FindTPAEdges(PPointArray(Params^[1])^);
end;

procedure Lape_ClearTPAFromTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := ClearTPAFromTPA(PPointArray(Params^[1])^, PPointArray(Params^[2])^);
end;

procedure Lape_ReturnPointsNotInTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := ReturnPointsNotInTPA(PPointArray(Params^[1])^, PBox(Params^[2])^);
end;

procedure Lape_PointInTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PointInTPA(PPoint(Params^[1])^, PPointArray(Params^[2])^);
end;

procedure Lape_ClearDoubleTPA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  ClearDoubleTPA(PPointArray(Params^[1])^);
end;

procedure Lape_TPACountSort(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TPACountSort(PPointArray(Params^[1])^, PPoint(Params^[2])^, PBoolean(Params^[3])^);
end;

procedure Lape_TPACountSortBase(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TPACountSortBase(PPointArray(Params^[1])^, PPoint(Params^[2])^, PPoint(Params^[3])^, PBoolean(Params^[4])^);
end;

procedure Lape_SameTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SameTPA(PPointArray(Params^[1])^, PPointArray(Params^[2])^);
end;

procedure Lape_TPAInATPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := TPAInATPA(PPointArray(Params^[1])^, P2DPointArray(Params^[2])^, PLongInt(Params^[3])^);
end;

procedure Lape_OffsetTPA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  OffsetTPA(PPointArray(Params^[1])^, PPoint(Params^[2])^);
end;

procedure Lape_OffsetATPA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  OffsetATPA(P2DPointArray(Params^[1])^, PPoint(Params^[2])^);
end;

procedure Lape_CopyTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := CopyTPA(PPointArray(Params^[1])^);
end;

procedure Lape_CopyATPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := CopyATPA(P2DPointArray(Params^[1])^);
end;

procedure Lape_TPAPosNext(const Params: PParamArray;const Result: pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
 PInt32(Result)^:= TPAPosNext(PPoint(Params^[1])^, PPointArray(Params^[2])^, PInt32(Params^[3])^, PBoolean(Params^[4])^);
end;

procedure Lape_GlueTPAs(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
 PPointArray(Result)^:= GlueTPAs(PPointArray(Params^[1])^, PPointArray(Params^[2])^, PBoolean(Params^[3])^, PBoolean(Params^[4])^);
end;

procedure Lape_SortATPAFromFirstPointX(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortATPAFromFirstPointX(P2DPointArray(Params^[1])^, PPoint(Params^[2])^);
end;

procedure Lape_SortATPAFromFirstPointY(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortATPAFromFirstPointY(P2DPointArray(Params^[1])^, PPoint(Params^[2])^);
end;

procedure Lape_FilterTPAsBetween(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  filterTPAsBetween(P2DPointArray(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_MedianTPAEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  MedianTPAEx(PPointArray(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_MedianTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := MedianTPA(PPointArray(Params^[1])^);
end;

procedure Lape_FilterPointsBox(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  FilterPointsBox(PPointArray(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^);
end;

procedure Lape_TPAFromPolygon(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := TPAFromPolygon(PPointArray(Params^[1])^);
end;

procedure Lape_ClusterTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := ClusterTPA(PPointArray(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_ClusterTPAEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := ClusterTPAEx(PPointArray(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_PartitionTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := PartitionTPA(PPointArray(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_InvertTIA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  InvertTIA(PIntegerArray(Params^[1])^);
end;

procedure Lape_SumIntegerArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := SumIntegerArray(PIntegerArray(Params^[1])^);
end;

procedure Lape_AverageTIA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := AverageTIA(PIntegerArray(Params^[1])^);
end;

procedure Lape_AverageExtended(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := AverageExtended(PExtendedArray(Params^[1])^);
end;

procedure Lape_Quicksort(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Quicksort(PIntegerArray(Params^[1])^, Low(PIntegerArray(Params^[1])^), High(PIntegerArray(Params^[1])^));
end;

procedure Lape_InIntArrayEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := InIntArrayEx(PIntegerArray(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_InIntArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := InIntArray(PIntegerArray(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_ClearSameIntegers(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  ClearSameIntegers(PIntegerArray(Params^[1])^);
end;

procedure Lape_CombineIntArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := CombineIntArray(PIntegerArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

procedure Lape_Import_Arrays_Algorithms(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    Section := 'Arrays & Algorithms';

    addGlobalMethod('procedure RAaSTPAEx(var a: TPointArray; const w, h: Int32);', @Lape_RAaSTPAEx, Data);
    addGlobalMethod('procedure RAaSTPA(var a: TPointArray; const Dist: Int32);', @Lape_RAaSTPA, Data);
    addGlobalMethod('function NearbyPointInArrayEx(const P: TPoint; w, h: Int32; const a: TPointArray): Boolean', @Lape_NearbyPointInArrayEx, Data);
    addGlobalMethod('function NearbyPointInArray(const P: TPoint; Dist: Int32; const a: TPointArray): Boolean', @Lape_NearbyPointInArray, Data);
    addGlobalMethod('procedure QuickTPASort(var A: TIntegerArray; var B: TPointArray; iLo, iHi: Int32; SortUp: Boolean);', @Lape_QuickTPASort, Data);
    addGlobalMethod('procedure QuickATPASort(var A: TIntegerArray; var B: T2DPointArray; iLo, iHi: Int32; SortUp: Boolean);', @Lape_QuickATPASort, Data);
    addGlobalMethod('procedure SortTPAByX(var a: TPointArray; const LowToHi: Boolean);', @Lape_SortTPAByX, Data);
    addGlobalMethod('procedure SortTPAByY(var a: TPointArray; const LowToHi: Boolean);', @Lape_SortTPAByY, Data);
    addGlobalMethod('function FindTPARows(a: TPointArray): T2DPointArray;', @Lape_FindTPARows, Data);
    addGlobalMethod('function FindTPAColumns(a: TPointArray): T2DPointArray;', @Lape_FindTPAColumns, Data);
    addGlobalMethod('procedure SortTPAFrom(var a: TPointArray; const From: TPoint);', @Lape_SortTPAFrom, Data);
    addGlobalMethod('procedure SortATPAFrom(var a: T2DPointArray; const From: TPoint);', @Lape_SortATPAFrom, Data);
    addGlobalMethod('procedure SortATPAFromFirstPoint(var a: T2DPointArray; const From: TPoint);', @Lape_SortATPAFromFirstPoint, Data);
    addGlobalMethod('procedure SortATPAFromMidPoint(var a: T2DPointArray; const From: TPoint);', @Lape_SortATPAFromMidPoint, Data);
    addGlobalMethod('procedure SortATPAFromFirstPointX(var a: T2DPointArray; const From: TPoint);', @Lape_SortATPAFromFirstPointX, Data);
    addGlobalMethod('procedure SortATPAFromFirstPointY(var a: T2DPointArray; const From: TPoint);', @Lape_SortATPAFromFirstPointY, Data);
    addGlobalMethod('procedure InvertTPA(var a: TPointArray);', @Lape_InvertTPA, Data);
    addGlobalMethod('procedure InvertATPA(var a: T2DPointArray);', @Lape_InvertATPA, Data);
    addGlobalMethod('function MiddleTPAEx(const TPA: TPointArray; var x, y: Int32): Boolean', @Lape_MiddleTPAEx, Data);
    addGlobalMethod('function MiddleTPA(const tpa: TPointArray): TPoint', @Lape_MiddleTPA, Data);
    addGlobalMethod('procedure MedianTPAEx(var tpa: TPointArray; out x, y: Int32);', @Lape_MedianTPAEx, Data);
    addGlobalMethod('function MedianTPA(var tpa: TPointArray): TPoint;', @Lape_MedianTPA, Data);
    addGlobalMethod('procedure SortATPASize(var a: T2DPointArray; const BigFirst: Boolean);', @Lape_SortATPASize, Data);
    addGlobalMethod('procedure SortATPAFromSize(var a: T2DPointArray; const Size: Int32; CloseFirst: Boolean);', @Lape_SortATPAFromSize, Data);
    addGlobalMethod('procedure FilterTPAsBetween(var atpa: T2DPointArray; const minLength, maxLength: Int32);', @Lape_FilterTPAsBetween, Data);
    addGlobalMethod('function SplitTPAEx(const arr: TPointArray; w, h: Int32): T2DPointArray', @Lape_SplitTPAEx, Data);
    addGlobalMethod('function SplitTPA(const arr: TPointArray; Dist: Int32): T2DPointArray', @Lape_SplitTPA, Data);
    addGlobalMethod('function ClusterTPAEx(const TPA: TPointArray; width, height: Int32): T2DPointArray;', @Lape_ClusterTPAEx, Data);
    addGlobalMethod('function ClusterTPA(const TPA: TPointArray; dist: Int32): T2DPointArray;', @Lape_ClusterTPA, Data);
    addGlobalMethod('function FloodFillTPA(const TPA: TPointArray): T2DPointArray', @Lape_FloodFillTPA, Data);
    addGlobalMethod('procedure FilterPointsPie(var Points: TPointArray; const SD, ED, MinR, MaxR: Extended; Mx, My: Int32);', @Lape_FilterPointsPie, Data);
    addGlobalMethod('procedure FilterPointsDist(var Points: TPointArray; const MinDist, MaxDist: Extended; Mx, My: Int32);', @Lape_FilterPointsDist, Data);
    addGlobalMethod('procedure FilterPointsLine(var Points: TPointArray; Radial: Extended; Radius, MX, MY: Int32);', @Lape_FilterPointsLine, Data);
    addGlobalMethod('procedure FilterPointsBox(var points: TPointArray; x1, y1, x2, y2: Int32);', @Lape_FilterPointsBox, Data);
    addGlobalMethod('procedure FilterTPADist(var TPA: TPointArray; maxDist: Int32);', @Lape_FilterTPADist, Data);
    addGlobalMethod('function GetATPABounds(const ATPA: T2DPointArray): TBox', @Lape_GetATPABounds, Data);
    addGlobalMethod('function GetTPABounds(const TPA: TPointArray): TBox', @Lape_GetTPABounds, Data);
    addGlobalMethod('function FindTPAinTPA(const SearchTPA, TotalTPA: TPointArray; var Matches: TPointArray): Boolean', @Lape_FindTPAinTPA, Data);
    addGlobalMethod('function GetSamePointsATPA(const ATPA: T2DPointArray; var Matches: TPointArray): boolean', @Lape_GetSamePointsATPA, Data);
    addGlobalMethod('function FindTextTPAinTPA(Height: Int32; const SearchTPA, TotalTPA: TPointArray; var Matches: TPointArray): Boolean', @Lape_FindTextTPAinTPA, Data);
    addGlobalMethod('procedure SortCircleWise(var tpa: TPointArray; const cx, cy, StartDegree: Int32; SortUp, ClockWise: Boolean);', @Lape_SortCircleWise, Data);
    addGlobalMethod('procedure LinearSort(var tpa: TPointArray; cx, cy, sd: Int32; SortUp: Boolean);', @Lape_LinearSort, Data);
    addGlobalMethod('function RotatePoint(Const p: TPoint; angle, mx, my: Extended): TPoint', @Lape_RotatePoint, Data);
    addGlobalMethod('function ChangeDistPT(const PT: TPoint; mx,my: Int32; newdist: extended): TPoint', @Lape_ChangeDistPT, Data);
    addGlobalMethod('function ChangeDistTPA(var TPA: TPointArray; mx,my: Int32; newdist: extended): boolean', @Lape_ChangeDistTPA, Data);
    addGlobalMethod('function FindGapsTPA(const TPA: TPointArray; MinPixels: Int32): T2DPointArray', @Lape_FindGapsTPA, Data);
    addGlobalMethod('function RemoveDistTPointArray(x, y, dist: Int32; const ThePoints: TPointArray; RemoveHigher: Boolean): TPointArray', @Lape_RemoveDistTPointArray, Data);
    addGlobalMethod('function CombineTPA(const Ar1, Ar2: TPointArray): TPointArray', @Lape_CombineTPA, Data);
    addGlobalMethod('function ReArrangeandShortenArrayEx(const a: TPointArray; w, h: Int32): TPointArray', @Lape_ReArrangeandShortenArrayEx, Data);
    addGlobalMethod('function ReArrangeandShortenArray(const a: TPointArray; Dist: Int32): TPointArray', @Lape_ReArrangeandShortenArray, Data);
    addGlobalMethod('function TPAtoATPAEx(const TPA: TPointArray; w, h: Int32): T2DPointArray', @Lape_TPAtoATPAEx, Data);
    addGlobalMethod('function TPAtoATPA(const TPA: TPointArray; Dist: Int32): T2DPointArray', @Lape_TPAtoATPA, Data);
    addGlobalMethod('function MergeATPA(const ATPA: T2DPointArray): TPointArray', @Lape_MergeATPA, Data);
    addGlobalMethod('procedure AppendTPA(var TPA: TPointArray; const ToAppend: TPointArray);', @Lape_AppendTPA, Data);
    addGlobalMethod('function TPAFromLine(const x1, y1, x2, y2: Int32): TPointArray', @Lape_TPAFromLine, Data);
    addGlobalMethod('function EdgeFromBox(const Box: TBox): TPointArray', @Lape_EdgeFromBox, Data);
    addGlobalMethod('function TPAFromBox(const Box: TBox): TPointArray', @Lape_TPAFromBox, Data);
    addGlobalMethod('function TPAFromEllipse(const CX, CY, XRadius, YRadius : Int32) : TPointArray', @Lape_TPAFromEllipse, Data);
    addGlobalMethod('function TPAFromCircle(const CX, CY, Radius : Int32) : TPointArray', @Lape_TPAFromCircle, Data);
    addGlobalMethod('function TPAFromPolygon(const Shape: TPointArray) : TPointArray', @Lape_TPAFromPolygon, Data);
    addGlobalMethod('procedure FillEllipse(var a: TPointArray);', @Lape_FillEllipse, Data);
    addGlobalMethod('function RotatePoints(const P: TPointArray; A, cx, cy: Extended): TPointArray', @Lape_RotatePoints, Data);
    addGlobalMethod('function FindTPAEdges(const p: TPointArray): TPointArray', @Lape_FindTPAEdges, Data);
    addGlobalMethod('function ClearTPAFromTPA(const arP, ClearPoints: TPointArray): TPointArray', @Lape_ClearTPAFromTPA, Data);
    addGlobalMethod('function ReturnPointsNotInTPA(Const TotalTPA: TPointArray; const Box: TBox): TPointArray', @Lape_ReturnPointsNotInTPA, Data);
    addGlobalMethod('function PointInTPA(p: TPoint; const arP: TPointArray): Boolean', @Lape_PointInTPA, Data);
    addGlobalMethod('procedure ClearDoubleTPA(var TPA: TPointArray);', @Lape_ClearDoubleTPA, Data);
    addGlobalMethod('procedure TPACountSort(var TPA: TPointArray; const max: TPoint; Const SortOnX: Boolean);', @Lape_TPACountSort, Data);
    addGlobalMethod('procedure TPACountSortBase(var TPA: TPointArray; const maxx, base: TPoint; const SortOnX: Boolean);', @Lape_TPACountSortBase, Data);
    addGlobalMethod('function SameTPA(const aTPA, bTPA: TPointArray): Boolean', @Lape_SameTPA, Data);
    addGlobalMethod('function TPAInATPA(const TPA: TPointArray; const InATPA: T2DPointArray; var Index: LongInt): Boolean', @Lape_TPAInATPA, Data);
    addGlobalMethod('procedure OffsetTPA(var TPA: TPointArray; const Offset: TPoint);', @Lape_OffsetTPA, Data);
    addGlobalMethod('procedure OffsetATPA(var ATPA: T2DPointArray; const Offset: TPoint);', @Lape_OffsetATPA, Data);
    addGlobalMethod('function CopyTPA(const TPA: TPointArray): TPointArray', @Lape_CopyTPA, Data);
    addGlobalMethod('function CopyATPA(const ATPA: T2DPointArray): T2DPointArray', @Lape_CopyATPA, Data);
    addGlobalMethod('function TPAPosNext(const Find: TPoint; const V: TPointArray; const PrevPos: Int32;const IsSortedAscending: Boolean): Int32;',@Lape_TPAPosNext, Data);
    addGlobalMethod('function GlueTPAs(const V1, V2: TPointArray; const IsSortedAscending, byDifference: Boolean): TPointArray;',@Lape_GlueTPAs, Data);
    addGlobalMethod('function PartitionTPA(const TPA: TPointArray; BoxWidth, BoxHeight: Int32): T2DPointArray;', @Lape_PartitionTPA, Data);
    addGlobalMethod('procedure Quicksort(var Arr: TIntegerArray);', @Lape_Quicksort, Data);
    addGlobalMethod('function InIntArrayEx(const Arr: TIntegerArray; var Index: Int32; const Number: Int32): Boolean', @Lape_InIntArrayEx, Data);
    addGlobalMethod('function InIntArray(const Arr: TIntegerArray; Number: Int32): Boolean', @Lape_InIntArray, Data);
    addGlobalMethod('procedure ClearSameIntegers(var Arr: TIntegerArray);', @Lape_ClearSameIntegers, Data);
    addGlobalMethod('function CombineIntArray(const Arr1, Arr2: TIntegerArray): TIntegerArray', @Lape_CombineIntArray, Data);
    addGlobalMethod('procedure InvertTIA(var Arr: TIntegerArray);', @Lape_InvertTIA, Data);
    addGlobalMethod('function SumIntegerArray(const Arr: TIntegerArray): Int32', @Lape_SumIntegerArray, Data);
    addGlobalMethod('function AverageTIA(const Arr: TIntegerArray): Int32', @Lape_AverageTIA, Data);
    addGlobalMethod('function AverageExtended(const Arr: TExtendedArray): Extended', @Lape_AverageExtended, Data);
  end;
end;

end.

