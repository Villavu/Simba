unit simba.import_algorithms;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.script_compiler, simba.mufasatypes, simba.tpa, simba.math,
  simba.algo_difference, simba.algo_intersection, simba.algo_symmetricDifference,
  simba.geometry;

//------- array.Difference(array)
procedure _Lape_UInt8_Difference(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PByteArray(Result)^ := Algo_UInt8_Difference(PByteArray(Params^[0])^, PByteArray(Params^[1])^)
end;

procedure _Lape_Int32_Difference(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := Algo_Int32_Difference(PIntegerArray(Params^[0])^, PIntegerArray(Params^[1])^)
end;

procedure _Lape_Int64_Difference(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64Array(Result)^ := Algo_Int64_Difference(PInt64Array(Params^[0])^, PInt64Array(Params^[1])^)
end;

procedure _Lape_Point_Difference(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := Algo_Point_Difference(PPointArray(Params^[0])^, PPointArray(Params^[1])^)
end;

procedure _Lape_Box_Difference(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Result)^ := Algo_Box_Difference(PBoxArray(Params^[0])^, PBoxArray(Params^[1])^)
end;

//------- array.SymmetricDifference(array)
procedure _Lape_UInt8_SymmetricDifference(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PByteArray(Result)^ := Algo_UInt8_SymmetricDifference(PByteArray(Params^[0])^, PByteArray(Params^[1])^)
end;

procedure _Lape_Int32_SymmetricDifference(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := Algo_Int32_SymmetricDifference(PIntegerArray(Params^[0])^, PIntegerArray(Params^[1])^)
end;

procedure _Lape_Int64_SymmetricDifference(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64Array(Result)^ := Algo_Int64_SymmetricDifference(PInt64Array(Params^[0])^, PInt64Array(Params^[1])^)
end;

procedure _Lape_Point_SymmetricDifference(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := Algo_Point_SymmetricDifference(PPointArray(Params^[0])^, PPointArray(Params^[1])^)
end;

procedure _Lape_Box_SymmetricDifference(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Result)^ := Algo_Box_SymmetricDifference(PBoxArray(Params^[0])^, PBoxArray(Params^[1])^)
end;


//------- array.Intersection(array)
procedure _Lape_UInt8_Intersection(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PByteArray(Result)^ := Algo_UInt8_Intersection(PByteArray(Params^[0])^, PByteArray(Params^[1])^)
end;

procedure _Lape_Int32_Intersection(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := Algo_Int32_Intersection(PIntegerArray(Params^[0])^, PIntegerArray(Params^[1])^)
end;

procedure _Lape_Int64_Intersection(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64Array(Result)^ := Algo_Int64_Intersection(PInt64Array(Params^[0])^, PInt64Array(Params^[1])^)
end;

procedure _Lape_Point_Intersection(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := Algo_Point_Intersection(PPointArray(Params^[0])^, PPointArray(Params^[1])^)
end;

procedure _Lape_Box_Intersection(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Result)^ := Algo_Box_Intersection(PBoxArray(Params^[0])^, PBoxArray(Params^[1])^)
end;



// -------------
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
  PPoint(Result)^ := TSimbaGeometry.RotatePoint(PPoint(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^);
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
  PPointArray(Result)^ := TSimbaGeometry.RotatePoints(PPointArray(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^);
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

procedure ImportAlgorithms(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('Algorithms');

    //------- array.Difference(array)
    addGlobalFunc('function TByteArray.Difference(Other: TByteArray): TByteArray', @_Lape_UInt8_Difference);
    addGlobalFunc('function TIntegerArray.Difference(Other: TIntegerArray): TIntegerArray', @_Lape_Int32_Difference);
    addGlobalFunc('function TInt64Array.Difference(Other: TInt64Array): TInt64Array', @_Lape_Int64_Difference);
    addGlobalFunc('function TPointArray.Difference(Other: TPointArray): TPointArray', @_Lape_Point_Difference);
    addGlobalFunc('function TBoxArray.Difference(Other: TBoxArray): TBoxArray', @_Lape_Box_Difference);

    //------- array.SymmetricDifference(array)
    addGlobalFunc('function TByteArray.SymmetricDifference(Other: TByteArray): TByteArray', @_Lape_UInt8_SymmetricDifference);
    addGlobalFunc('function TIntegerArray.SymmetricDifference(Other: TIntegerArray): TIntegerArray', @_Lape_Int32_SymmetricDifference);
    addGlobalFunc('function TInt64Array.SymmetricDifference(Other: TInt64Array): TInt64Array', @_Lape_Int64_SymmetricDifference);
    addGlobalFunc('function TPointArray.SymmetricDifference(Other: TPointArray): TPointArray', @_Lape_Point_SymmetricDifference);
    addGlobalFunc('function TBoxArray.SymmetricDifference(Other: TBoxArray): TBoxArray', @_Lape_Box_SymmetricDifference);

    //------- array.Intersection(array)
    addGlobalFunc('function TByteArray.Intersection(Other: TByteArray): TByteArray', @_Lape_UInt8_Intersection);
    addGlobalFunc('function TIntegerArray.Intersection(Other: TIntegerArray): TIntegerArray', @_Lape_Int32_Intersection);
    addGlobalFunc('function TInt64Array.Intersection(Other: TInt64Array): TInt64Array', @_Lape_Int64_Intersection);
    addGlobalFunc('function TPointArray.Intersection(Other: TPointArray): TPointArray', @_Lape_Point_Intersection);
    addGlobalFunc('function TBoxArray.Intersection(Other: TBoxArray): TBoxArray', @_Lape_Box_Intersection);

    addGlobalFunc('function NearbyPointInArrayEx(P: TPoint; W, H: Integer; const TPA: TPointArray): Boolean', @_LapeNearbyPointInArrayEx);
    addGlobalFunc('function NearbyPointInArray(P: TPoint; Dist: Integer; const TPA: TPointArray): Boolean', @_LapeNearbyPointInArray);
    addGlobalFunc('procedure SortTPAByX(var TPA: TPointArray; LowToHi: Boolean)', @_LapeSortTPAByX);
    addGlobalFunc('procedure SortTPAByY(var TPA: TPointArray; LowToHi: Boolean)', @_LapeSortTPAByY);
    addGlobalFunc('function FindTPARows(const TPA: TPointArray): T2DPointArray', @_LapeFindTPARows);
    addGlobalFunc('function FindTPAColumns(const TPA: TPointArray): T2DPointArray', @_LapeFindTPAColumns);
    addGlobalFunc('procedure SortTPAFrom(var TPA: TPointArray; From: TPoint)', @_LapeSortTPAFrom);
    addGlobalFunc('procedure SortATPAFromFirstPoint(var ATPA: T2DPointArray; From: TPoint)', @_LapeSortATPAFromFirstPoint);
    addGlobalFunc('procedure SortATPAFromMidPoint(var ATPA: T2DPointArray; From: TPoint)', @_LapeSortATPAFromMidPoint);
    addGlobalFunc('procedure SortATPAFromFirstPointX(var ATPA: T2DPointArray; From: TPoint)', @_LapeSortATPAFromFirstPointX);
    addGlobalFunc('procedure SortATPAFromFirstPointY(var ATPA: T2DPointArray; From: TPoint)', @_LapeSortATPAFromFirstPointY);
    addGlobalFunc('function MiddleTPAEx(const TPA: TPointArray; var X, Y: Integer): Boolean', @_LapeMiddleTPAEx);
    addGlobalFunc('function MiddleTPA(const TPA: TPointArray): TPoint', @_LapeMiddleTPA);
    addGlobalFunc('procedure MedianTPAEx(const TPA: TPointArray; out X, Y: Integer)', @_LapeMedianTPAEx);
    addGlobalFunc('function MedianTPA(const TPA: TPointArray): TPoint', @_LapeMedianTPA);
    addGlobalFunc('procedure SortATPASize(var ATPA: T2DPointArray; BigFirst: Boolean)', @_LapeSortATPASize);
    addGlobalFunc('procedure SortATPAFromSize(var ATPA: T2DPointArray; Size: Integer; CloseFirst: Boolean)', @_LapeSortATPAFromSize);
    addGlobalFunc('procedure FilterTPAsBetween(var ATPA: T2DPointArray; MinLength, MaxLength: Integer)', @_LapeFilterTPAsBetween);
    addGlobalFunc('function SplitTPAEx(const Arr: TPointArray; W, H: Integer): T2DPointArray', @_LapeSplitTPAEx);
    addGlobalFunc('function SplitTPA(const Arr: TPointArray; Dist: Integer): T2DPointArray', @_LapeSplitTPA);
    addGlobalFunc('function ClusterTPAEx(const TPA: TPointArray; Width, Height: Integer): T2DPointArray', @_LapeClusterTPAEx);
    addGlobalFunc('function ClusterTPA(const TPA: TPointArray; Dist: Integer): T2DPointArray', @_LapeClusterTPA);
    addGlobalFunc('procedure FilterPointsPie(var Points: TPointArray; SD, ED, MinR, MaxR: Extended; Mx, My: Integer)', @_LapeFilterPointsPie);
    addGlobalFunc('procedure FilterPointsDist(var Points: TPointArray; MinDist, MaxDist: Extended; Mx, My: Integer)', @_LapeFilterPointsDist);
    addGlobalFunc('procedure FilterPointsLine(var Points: TPointArray; Radial: Extended; Radius, MX, MY: Integer)', @_LapeFilterPointsLine);
    addGlobalFunc('procedure FilterPointsBox(var points: TPointArray; X1, Y1, X2, Y2: Integer)', @_LapeFilterPointsBox);
    addGlobalFunc('function GetATPABounds(const ATPA: T2DPointArray): TBox', @_LapeGetATPABounds);
    addGlobalFunc('function GetTPABounds(const TPA: TPointArray): TBox', @_LapeGetTPABounds);
    addGlobalFunc('function GetSamePointsATPA(const ATPA: T2DPointArray; var Matches: TPointArray): Boolean', @_LapeGetSamePointsATPA);
    addGlobalFunc('function TPAtoATPAEx(const TPA: TPointArray; W, H: Integer): T2DPointArray', @_LapeTPAtoATPAEx);
    addGlobalFunc('function TPAtoATPA(const TPA: TPointArray; Dist: Integer): T2DPointArray', @_LapeTPAtoATPA);
    addGlobalFunc('function MergeATPA(const ATPA: T2DPointArray): TPointArray', @_LapeMergeATPA);
    addGlobalFunc('function TPAFromLine(X1, Y1, X2, Y2: Integer; Thickness: Integer = 1): TPointArray; overload', @_LapeTPAFromLine);
    addGlobalFunc('function TPAFromLine(P1, P2: TPoint; Thickness: Integer = 1): TPointArray; overload', @_LapeTPAFromLineEx);
    addGlobalFunc('function EdgeFromBox(const Box: TBox): TPointArray', @_LapeEdgeFromBox);
    addGlobalFunc('function TPAFromBox(const Box: TBox): TPointArray', @_LapeTPAFromBox);
    addGlobalFunc('function TPAFromEllipse(CX, CY, XRadius, YRadius: Integer) : TPointArray', @_LapeTPAFromEllipse);
    addGlobalFunc('function TPAFromCircle(CX, CY, Radius: Integer): TPointArray', @_LapeTPAFromCircle);
    addGlobalFunc('function TPAFromPolygon(const Shape: TPointArray) : TPointArray', @_LapeTPAFromPolygon);
    addGlobalFunc('procedure FillEllipse(var TPA: TPointArray)', @_LapeFillEllipse);
    addGlobalFunc('function RotatePoint(const p: TPoint; angle, mx, my: Extended): TPoint', @_LapeRotatePoint);
    addGlobalFunc('function RotatePoints(const TPA: TPointArray; A, cx, cy: Extended): TPointArray', @_LapeRotatePoints);
    addGlobalFunc('function FindTPAEdges(const TPA: TPointArray): TPointArray', @_LapeFindTPAEdges);
    addGlobalFunc('function ClearTPAFromTPA(const Points, PointsToRemove: TPointArray): TPointArray', @_LapeClearTPAFromTPA);
    addGlobalFunc('function ReturnPointsNotInTPA(const TotalTPA: TPointArray; Box: TBox): TPointArray', @_LapeReturnPointsNotInTPA);
    addGlobalFunc('function SameTPA(const aTPA, bTPA: TPointArray): Boolean', @_LapeSameTPA);
    addGlobalFunc('procedure OffsetTPA(var TPA: TPointArray; Offset: TPoint)', @_LapeOffsetTPA);
    addGlobalFunc('procedure OffsetATPA(var ATPA: T2DPointArray; Offset: TPoint)', @_LapeOffsetATPA);
    addGlobalFunc('function PartitionTPA(const TPA: TPointArray; BoxWidth, BoxHeight: Integer): T2DPointArray', @_LapePartitionTPA);
    addGlobalFunc('procedure ClearSameIntegers(var Arr: TIntegerArray)', @_LapeClearSameIntegers);
    addGlobalFunc('procedure ClearSamePoints(var Arr: TPointArray)', @_LapeClearSamePoints);
    addGlobalFunc('function TPAErode(const TPA: TPointArray; Amount: Integer): TPointArray', @_LapeTPAErode);
    addGlobalFunc('function TPAGrow(const TPA: TPointArray; Amount: Integer): TPointArray', @_LapeTPAGrow);
    addGlobalFunc('function PointsInRangeOf(const Points, Other: TPointArray; MinDist, MaxDist: Double): TPointArray; overload', @_LapePointsInRangeOf);
    addGlobalFunc('function PointsInRangeOf(const Points, Other: TPointArray; MinDistX, MinDistY, MaxDistX, MaxDistY: Double): TPointArray; overload', @_LapePointsInRangeOfEx);
    addGlobalFunc('function ConvexHull(const TPA: TPointArray): TPointArray', @_LapeConvexHull);

    addGlobalFunc('function TPAReduceByDistance(const TPA: TPointArray; Dist: Integer): TPointArray', @_LapeTPAReduceByDistance);
    addGlobalFunc('function TPAConnect(const TPA: TPointArray): TPointArray', @_LapeTPAConnect);
    addGlobalFunc('function TPADensity(const TPA: TPointArray): Double;', @_LapeTPADensity);
    addGlobalFunc('function TPABorder(const TPA: TPointArray): TPointArray', @_LapeTPABorder);
    addGlobalFunc('function TPASkeleton(const TPA: TPointArray; FMin: Integer = 2; FMax: Integer = 6): TPointArray;', @_LapeTPASkeleton);

    addGlobalFunc('function ExcludePointsDist(const TPA: TPointArray; Center: TPoint; MinDist, MaxDist: Extended): TPointArray', @_LapeExcludePointsDist);
    addGlobalFunc('function ExcludePointsPolygon(const TPA: TPointArray; const Polygon: TPointArray): TPointArray', @_LapeExcludePointsPolygon);
    addGlobalFunc('function ExcludePointsBox(const TPA: TPointArray; Box: TBox): TPointArray', @_LapeExcludePointsBox);

    addGlobalFunc('procedure SortTPACircular(var TPA: TPointArray; Center: TPoint; StartDegrees: Integer; Clockwise: Boolean)', @_LapeSortTPACircular);

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportAlgorithms);

end.

