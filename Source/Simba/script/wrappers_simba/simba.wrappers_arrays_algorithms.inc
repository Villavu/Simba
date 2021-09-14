procedure _LapeRAaSTPAEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  RAaSTPAEx(PPointArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure _LapeRAaSTPA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  RAaSTPA(PPointArray(Params^[0])^, PInt32(Params^[1])^);
end;

procedure _LapeNearbyPointInArrayEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := NearbyPointInArrayEx(PPoint(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PPointArray(Params^[3])^);
end;

procedure _LapeNearbyPointInArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := NearbyPointInArray(PPoint(Params^[0])^, PInt32(Params^[1])^, PPointArray(Params^[2])^);
end;

procedure _LapeQuickTPASort(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  QuickTPASort(PIntegerArray(Params^[0])^, PPointArray(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PBoolean(Params^[4])^);
end;

procedure _LapeQuickATPASort(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  QuickATPASort(PIntegerArray(Params^[0])^, P2DPointArray(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PBoolean(Params^[4])^);
end;

procedure _LapeSortTPAByX(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortTPAByX(PPointArray(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure _LapeSortTPAByY(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortTPAByY(PPointArray(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure _LapeFindTPARows(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := FindTPARows(PPointArray(Params^[0])^);
end;

procedure _LapeFindTPAColumns(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := FindTPAColumns(PPointArray(Params^[0])^);
end;

procedure _LapeSortTPAFrom(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortTPAFrom(PPointArray(Params^[0])^, PPoint(Params^[1])^);
end;

procedure _LapeSortATPAFrom(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortATPAFrom(P2DPointArray(Params^[0])^, PPoint(Params^[1])^);
end;

procedure _LapeSortATPAFromFirstPoint(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortATPAFromFirstPoint(P2DPointArray(Params^[0])^, PPoint(Params^[1])^);
end;

procedure _LapeSortATPAFromMidPoint(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortATPAFromMidPoint(P2DPointArray(Params^[0])^, PPoint(Params^[1])^);
end;

procedure _LapeInvertTPA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  InvertTPA(PPointArray(Params^[0])^);
end;

procedure _LapeInvertATPA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  InvertATPA(P2DPointArray(Params^[0])^);
end;

procedure _LapeMiddleTPAEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := MiddleTPAEx(PPointArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure _LapeMiddleTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := MiddleTPA(PPointArray(Params^[0])^);
end;

procedure _LapeSortATPASize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortATPASize(P2DPointArray(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure _LapeSortATPAFromSize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortATPAFromSize(P2DPointArray(Params^[0])^, PInt32(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeSplitTPAEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := SplitTPAEx(PPointArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure _LapeSplitTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := SplitTPA(PPointArray(Params^[0])^, PInt32(Params^[1])^);
end;

procedure _LapeFilterPointsPie(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  FilterPointsPie(PPointArray(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, False);
end;

procedure _LapeFilterPointsDist(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  FilterPointsDist(PPointArray(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
end;

procedure _LapeFilterPointsLine(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  FilterPointsLine(PPointArray(Params^[0])^, PExtended(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
end;

procedure _LapeFilterTPADist(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  FilterTPADist(PPointArray(Params^[0])^, PInt32(Params^[1])^);
end;

procedure _LapeGetATPABounds(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := GetATPABounds(P2DPointArray(Params^[0])^);
end;

procedure _LapeGetTPABounds(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := GetTPABounds(PPointArray(Params^[0])^);
end;

procedure _LapeFindTPAinTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := FindTPAinTPA(PPointArray(Params^[0])^, PPointArray(Params^[1])^, PPointArray(Params^[2])^);
end;

procedure _LapeGetSamePointsATPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := GetSamePointsATPA(P2DPointArray(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure _LapeSortCircleWise(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortCircleWise(PPointArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PBoolean(Params^[4])^, PBoolean(Params^[5])^);
end;

procedure _LapeLinearSort(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  LinearSort(PPointArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PBoolean(Params^[4])^);
end;

procedure _LapeRotatePoint(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := RotatePoint(PPoint(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^);
end;

procedure _LapeChangeDistPT(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := ChangeDistPT(PPoint(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, Pextended(Params^[3])^);
end;

procedure _LapeChangeDistTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := ChangeDistTPA(PPointArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, Pextended(Params^[3])^);
end;

procedure _LapeFindGapsTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := FindGapsTPA(PPointArray(Params^[0])^, PInt32(Params^[1])^);
end;

procedure _LapeRemoveDistTPointArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := RemoveDistTPointArray(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PPointArray(Params^[3])^, PBoolean(Params^[4])^);
end;

procedure _LapeCombineTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := CombineTPA(PPointArray(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure _LapeReArrangeandShortenArrayEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := ReArrangeandShortenArrayEx(PPointArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure _LapeReArrangeandShortenArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := ReArrangeandShortenArray(PPointArray(Params^[0])^, PInt32(Params^[1])^);
end;

procedure _LapeTPAtoATPAEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := TPAtoATPAEx(PPointArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure _LapeTPAtoATPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := TPAtoATPA(PPointArray(Params^[0])^, PInt32(Params^[1])^);
end;

procedure _LapeMergeATPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := MergeATPA(P2DPointArray(Params^[0])^);
end;

procedure _LapeTPAFromLine(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := TPAFromLine(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure _LapeEdgeFromBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := EdgeFromBox(PBox(Params^[0])^);
end;

procedure _LapeTPAFromBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := TPAFromBox(PBox(Params^[0])^);
end;

procedure _LapeTPAFromEllipse(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := TPAFromEllipse(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure _LapeTPAFromCircle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := TPAFromCircle(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure _LapeFillEllipse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  FillEllipse(PPointArray(Params^[0])^);
end;

procedure _LapeRotatePoints(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := RotatePoints(PPointArray(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^);
end;

procedure _LapeFindTPAEdges(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := FindTPAEdges(PPointArray(Params^[0])^);
end;

procedure _LapeClearTPAFromTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := ClearTPAFromTPA(PPointArray(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure _LapeReturnPointsNotInTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := ReturnPointsNotInTPA(PPointArray(Params^[0])^, PBox(Params^[1])^);
end;

procedure _LapePointInTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PointInTPA(PPoint(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure _LapeClearDoubleTPA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  ClearDoubleTPA(PPointArray(Params^[0])^);
end;

procedure _LapeTPACountSort(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TPACountSort(PPointArray(Params^[0])^, PPoint(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeTPACountSortBase(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TPACountSortBase(PPointArray(Params^[0])^, PPoint(Params^[1])^, PPoint(Params^[2])^, PBoolean(Params^[3])^);
end;

procedure _LapeSameTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SameTPA(PPointArray(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure _LapeTPAInATPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := TPAInATPA(PPointArray(Params^[0])^, P2DPointArray(Params^[1])^, PLongInt(Params^[2])^);
end;

procedure _LapeOffsetTPA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  OffsetTPA(PPointArray(Params^[0])^, PPoint(Params^[1])^);
end;

procedure _LapeOffsetATPA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  OffsetATPA(P2DPointArray(Params^[0])^, PPoint(Params^[1])^);
end;

procedure _LapeSortATPAFromFirstPointX(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortATPAFromFirstPointX(P2DPointArray(Params^[0])^, PPoint(Params^[1])^);
end;

procedure _LapeSortATPAFromFirstPointY(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortATPAFromFirstPointY(P2DPointArray(Params^[0])^, PPoint(Params^[1])^);
end;

procedure _LapeFilterTPAsBetween(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  filterTPAsBetween(P2DPointArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure _LapeMedianTPAEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  MedianTPAEx(PPointArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure _LapeMedianTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := MedianTPA(PPointArray(Params^[0])^);
end;

procedure _LapeFilterPointsBox(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  FilterPointsBox(PPointArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
end;

procedure _LapeTPAFromPolygon(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := TPAFromPolygon(PPointArray(Params^[0])^);
end;

procedure _LapeClusterTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := ClusterTPA(PPointArray(Params^[0])^, PInt32(Params^[1])^);
end;

procedure _LapeClusterTPAEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := ClusterTPAEx(PPointArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure _LapePartitionTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := PartitionTPA(PPointArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure _LapeInvertTIA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  InvertTIA(PIntegerArray(Params^[0])^);
end;

procedure _LapeSumIntegerArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := SumIntegerArray(PIntegerArray(Params^[0])^);
end;

procedure _LapeAverageTIA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := AverageTIA(PIntegerArray(Params^[0])^);
end;

procedure _LapeAverageExtended(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := AverageExtended(PExtendedArray(Params^[0])^);
end;

procedure _LapeQuicksort(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Quicksort(PIntegerArray(Params^[0])^, Low(PIntegerArray(Params^[0])^), High(PIntegerArray(Params^[0])^));
end;

procedure _LapeInIntArrayEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := InIntArrayEx(PIntegerArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure _LapeInIntArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := InIntArray(PIntegerArray(Params^[0])^, PInt32(Params^[1])^);
end;

procedure _LapeTPAErode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := TPAErode(PPointArray(Params^[0])^, PInt32(Params^[1])^);
end;

procedure _LapeTPAGrow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := TPAGrow(PPointArray(Params^[0])^, PInt32(Params^[1])^);
end;

procedure _LapePointsInRangeOf(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := PointsInRangeOf(PPointArray(Params^[0])^, PPointArray(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^);
end;

procedure _LapePointsInRangeOfEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := PointsInRangeOf(PPointArray(Params^[0])^, PPointArray(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^, PDouble(Params^[4])^, PDouble(Params^[5])^);
end;

procedure _LapeClearSameIntegers(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  ClearSameIntegers(PIntegerArray(Params^[0])^);
end;

