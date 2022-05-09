procedure _LapeNearbyPointInArrayEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := NearbyPointInArrayEx(PPoint(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PPointArray(Params^[3])^);
end;

procedure _LapeNearbyPointInArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := NearbyPointInArray(PPoint(Params^[0])^, PInteger(Params^[1])^, PPointArray(Params^[2])^);
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

procedure _LapeSortATPAFromFirstPoint(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortATPAFromFirstPoint(P2DPointArray(Params^[0])^, PPoint(Params^[1])^);
end;

procedure _LapeSortATPAFromMidPoint(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortATPAFromMidPoint(P2DPointArray(Params^[0])^, PPoint(Params^[1])^);
end;

procedure _LapeMiddleTPAEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := MiddleTPAEx(PPointArray(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
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
  SortATPAFromSize(P2DPointArray(Params^[0])^, PInteger(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeSplitTPAEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := SplitTPAEx(PPointArray(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeSplitTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := SplitTPA(PPointArray(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeFilterPointsPie(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  FilterPointsPie(PPointArray(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, False);
end;

procedure _LapeFilterPointsDist(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  FilterPointsDist(PPointArray(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeFilterPointsLine(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  FilterPointsLine(PPointArray(Params^[0])^, PExtended(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeGetATPABounds(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := GetATPABounds(P2DPointArray(Params^[0])^);
end;

procedure _LapeGetTPABounds(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := GetTPABounds(PPointArray(Params^[0])^);
end;

procedure _LapeGetSamePointsATPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := GetSamePointsATPA(P2DPointArray(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure _LapeRotatePoint(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := RotatePoint(PPoint(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^);
end;

procedure _LapeTPAtoATPAEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := TPAtoATPAEx(PPointArray(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeTPAtoATPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := TPAtoATPA(PPointArray(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeMergeATPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := MergeATPA(P2DPointArray(Params^[0])^);
end;

procedure _LapeTPAFromLine(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := TPAFromLine(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeTPAFromLineEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := TPAFromLine(PPoint(Params^[0])^, PPoint(Params^[1])^, PInteger(Params^[2])^);
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
  PPointArray(Result)^ := TPAFromEllipse(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeTPAFromCircle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := TPAFromCircle(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
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

procedure _LapeClearSamePoints(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  ClearSamePoints(PPointArray(Params^[0])^);
end;

procedure _LapeSameTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SameTPA(PPointArray(Params^[0])^, PPointArray(Params^[1])^);
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
  filterTPAsBetween(P2DPointArray(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeMedianTPAEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  MedianTPAEx(PPointArray(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeMedianTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := MedianTPA(PPointArray(Params^[0])^);
end;

procedure _LapeFilterPointsBox(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  FilterPointsBox(PPointArray(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeTPAFromPolygon(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := TPAFromPolygon(PPointArray(Params^[0])^);
end;

procedure _LapeClusterTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := ClusterTPA(PPointArray(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeClusterTPAEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := ClusterTPAEx(PPointArray(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapePartitionTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := PartitionTPA(PPointArray(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeTPAErode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := TPAErode(PPointArray(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeTPAGrow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := TPAGrow(PPointArray(Params^[0])^, PInteger(Params^[1])^);
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
  PIntegerArray(Params^[0])^ := Unique(PIntegerArray(Params^[0])^);
end;

procedure _LapeTPADensity(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDouble(Result)^ := TPADensity(PPointArray(Params^[0])^);
end;

procedure _LapeTPABorder(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := TPABorder(PPointArray(Params^[0])^);
end;

procedure _LapeTPAFloodFill(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := TPAFloodFill(PPointArray(Params^[0])^, PPoint(Params^[1])^);
end;

procedure _LapeTPAConnect(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := TPAConnect(PPointArray(Params^[0])^);
end;

procedure _LapeTPASkeleton(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := TPASkeleton(PPointArray(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeConvexHull(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := ConvexHull(PPointArray(Params^[0])^);
end;

procedure _LapeExcludePointsDist(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := ExcludePointsDist(PPointArray(Params^[0])^, PPoint(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^);
end;

procedure _LapeExcludePointsPolygon(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := ExcludePointsPolygon(PPointArray(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure _LapeExcludePointsBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := ExcludePointsBox(PPointArray(Params^[0])^, PBox(Params^[1])^);
end;

procedure _LapeSortTPACircular(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SortTPACircular(PPointArray(Params^[0])^, PPoint(Params^[1])^, PInteger(Params^[2])^, PBoolean(Params^[3])^);
end;

procedure _LapeTPAReduceByDistance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := TPAReduceByDistance(PPointArray(Params^[0])^, PInteger(Params^[1])^);
end;

