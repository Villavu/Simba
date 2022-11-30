unit simba.import_tpa;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.script_compiler, simba.mufasatypes, simba.tpa,
  simba.algo_difference, simba.algo_intersection, simba.algo_symmetricDifference,
  simba.geometry;

(*
TPointArray
===========
Methods relating to point arrays.
*)

(*
TPointArray.Difference
~~~~~~~~~~~~~~~~~~~~~~
function TPointArray.Difference(Other: TPointArray): TPointArray;
*)
procedure _Lape_Point_Difference(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := Algo_Point_Difference(PPointArray(Params^[0])^, PPointArray(Params^[1])^)
end;

(*
TPointArray.SymmetricDifference
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TPointArray.SymmetricDifference(Other: TPointArray): TPointArray;
*)
procedure _Lape_Point_SymmetricDifference(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := Algo_Point_SymmetricDifference(PPointArray(Params^[0])^, PPointArray(Params^[1])^)
end;

(*
TPointArray.Intersection
~~~~~~~~~~~~~~~~~~~~~~~~
function TPointArray.Intersection(Other: TPointArray): TPointArray;
*)
procedure _Lape_Point_Intersection(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := Algo_Point_Intersection(PPointArray(Params^[0])^, PPointArray(Params^[1])^)
end;

(*
TPointArray.CreateFromBox
~~~~~~~~~~~~~~~~~~~~~~~~~
function TPointArray.CreateFromBox(Box: TBox; Filled: Boolean): TPointArray; static;
*)
procedure _LapeTPACreateFromBox(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := TPointArray.CreateFromBox(PBox(Params^[0])^, PBoolean(Params^[1])^);
end;

(*
TPointArray.CreateFromEllipse
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TPointArray.CreateFromEllipse(Center: TPoint; RadiusX, RadiusY: Integer; Filled: Boolean): TPointArray; static;
*)
procedure _LapeTPACreateFromEllipse(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := TPointArray.CreateFromEllipse(PPoint(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PBoolean(Params^[3])^);
end;

(*
TPointArray.CreateFromCircle
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TPointArray.CreateFromCircle(Center: TPoint; Radius: Integer; Filled: Boolean): TPointArray; static;
*)
procedure _LapeTPACreateFromCircle(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := TPointArray.CreateFromCircle(PPoint(Params^[0])^, PInteger(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TPointArray.CreateFromLine
~~~~~~~~~~~~~~~~~~~~~~~~~~
function TPointArray.CreateFromLine(Start, Stop: TPoint): TPointArray; static;
*)
procedure _LapeTPACreateFromLine(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := TPointArray.CreateFromLine(PPoint(Params^[0])^, PPoint(Params^[1])^);
end;

(*
TPointArray.CreateFromPolygon
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TPointArray.CreateFromPolygon(Poly: TPointArray; Filled: Boolean): TPointArray; static;
*)
procedure _LapeTPACreateFromPolygon(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := TPointArray.CreateFromPolygon(PPointArray(Params^[0])^, PBoolean(Params^[1])^);
end;

(*
TPointArray.CreateFromSimplePolygon
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TPointArray.CreateFromSimplePolygon(Center: TPoint; Sides: Integer; Size: Integer; Filled: Boolean): TPointArray; static;
*)
procedure _LapeTPACreateFromSimplePolygon(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := TPointArray.CreateFromSimplePolygon(PPoint(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PBoolean(Params^[3])^);
end;

(*
TPointArray.Rows
~~~~~~~~~~~~~~~~
function TPointArray.Rows: T2DPointArray;
*)
procedure _LapeTPARows(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := PPointArray(Params^[0])^.Rows();
end;

(*
TPointArray.Columns
~~~~~~~~~~~~~~~~~~~
function TPointArray.Columns: T2DPointArray;
*)
procedure _LapeTPAColumns(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := PPointArray(Params^[0])^.Columns();
end;

(*
TPointArray.Offset
~~~~~~~~~~~~~~~~~~
function TPointArray.Offset(P: TPoint): TPointArray;
*)
procedure _LapeTPAOffset1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.Offset(PPoint(Params^[1])^);
end;

(*
TPointArray.Offset
~~~~~~~~~~~~~~~~~~
function TPointArray.Offset(X, Y: Integer): TPointArray;
*)
procedure _LapeTPAOffset2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.Offset(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TPointArray.FloodFill
~~~~~~~~~~~~~~~~~~~~~
function TPointArray.FloodFill(StartPoint: TPoint): TPointArray;
*)
procedure _LapeTPAFloodFill(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.FloodFill(PPoint(Params^[1])^);
end;

(*
TPointArray.Invert
~~~~~~~~~~~~~~~~~~
function TPointArray.Invert(Bounds: TBox): TPointArray;
*)
procedure _LapeTPAInvert1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.Invert(PBox(Params^[1])^);
end;

(*
TPointArray.Invert
~~~~~~~~~~~~~~~~~~
function TPointArray.Invert: TPointArray;
*)
procedure _LapeTPAInvert2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.Invert();
end;

(*
TPointArray.Bounds
~~~~~~~~~~~~~~~~~~
function TPointArray.Bounds: TBox;
*)
procedure _LapeTPABounds(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBox(Result)^ := PPointArray(Params^[0])^.Bounds();
end;

(*
TPointArray.Mean
~~~~~~~~~~~~~~~~
function TPointArray.Mean: TPoint;
*)
procedure _LapeTPAMean(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PPointArray(Params^[0])^.Mean();
end;

(*
TPointArray.ReduceByDistance
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TPointArray.ReduceByDistance(Dist: Integer): TPointArray;
*)
procedure _LapeTPAReduceByDistance(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.ReduceByDistance(PInteger(Params^[1])^);
end;

(*
TPointArray.PointsNearby
~~~~~~~~~~~~~~~~~~~~~~~~
function TPointArray.PointsNearby(Other: TPointArray; MinDist, MaxDist: Double): TPointArray;
*)
procedure _LapeTPAPointsNearby1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.PointsNearby(PPointArray(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^);
end;

(*
TPointArray.PointsNearby
~~~~~~~~~~~~~~~~~~~~~~~~
function TPointArray.PointsNearby(Other: TPointArray; MinDistX, MinDistY, MaxDistX, MaxDistY: Double): TPointArray;
*)
procedure _LapeTPAPointsNearby2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.PointsNearby(PPointArray(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^, PDouble(Params^[4])^, PDouble(Params^[5])^);
end;

(*
TPointArray.IsPointNearby
~~~~~~~~~~~~~~~~~~~~~~~~~
function TPointArray.IsPointNearby(Other: TPoint; MinDist, MaxDist: Double): Boolean;
*)
procedure _LapeTPAIsPointNearby1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PPointArray(Params^[0])^.IsPointNearby(PPoint(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^);
end;

(*
TPointArray.IsPointNearby
~~~~~~~~~~~~~~~~~~~~~~~~~
function TPointArray.IsPointNearby(Other: TPoint; MinDistX, MinDistY, MaxDistX, MaxDistY: Double): Boolean;
*)
procedure _LapeTPAIsPointNearby2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PPointArray(Params^[0])^.IsPointNearby(PPoint(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^, PDouble(Params^[4])^, PDouble(Params^[5])^);
end;

(*
TPointArray.NearestPoint
~~~~~~~~~~~~~~~~~~~~~~~~
function TPointArray.NearestPoint(Other: TPoint): TPoint;
*)
procedure _LapeTPANearestPoint(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PPointArray(Params^[0])^.NearestPoint(PPoint(Params^[1])^);
end;

(*
TPointArray.Density
~~~~~~~~~~~~~~~~~~~
function TPointArray.Density: Double;
*)
procedure _LapeTPADensity(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := PPointArray(Params^[0])^.Density();
end;

(*
TPointArray.Connect
~~~~~~~~~~~~~~~~~~~
function TPointArray.Connect: TPointArray;
*)
procedure _LapeTPAConnect(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.Connect();
end;

(*
TPointArray.Extremes
~~~~~~~~~~~~~~~~~~~~
function TPointArray.Extremes: TPointArray;
*)
procedure _LapeTPAExtremes(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.Extremes();
end;

(*
TPointArray.Rotate
~~~~~~~~~~~~~~~~~~
function TPointArray.Rotate(Radians: Double; Center: TPoint): TPointArray;
*)
procedure _LapeTPARotate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.Rotate(PDouble(Params^[1])^, PPoint(Params^[2])^);
end;

(*
TPointArray.RotateEx
~~~~~~~~~~~~~~~~~~~~
function TPointArray.RotateEx(Radians: Double): TPointArray;
*)
procedure _LapeTPARotateEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.RotateEx(PDouble(Params^[1])^);
end;

(*
TPointArray.Sort
~~~~~~~~~~~~~~~~
function TPointArray.Sort(Weights: TIntegerArray; LowToHigh: Boolean = True): TPointArray;
*)
procedure _LapeTPASort1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.Sort(PIntegerArray(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TPointArray.Sort
~~~~~~~~~~~~~~~~
function TPointArray.Sort(Weights: TDoubleArray; LowToHigh: Boolean = True): TPointArray;
*)
procedure _LapeTPASort2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.Sort(PDoubleArray(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TPointArray.SortByX
~~~~~~~~~~~~~~~~~~~
function TPointArray.SortByX(LowToHigh: Boolean = True): TPointArray;
*)
procedure _LapeTPASortByX(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.SortByX(PBoolean(Params^[1])^);
end;

(*
TPointArray.SortByY
~~~~~~~~~~~~~~~~~~~
function TPointArray.SortByY(LowToHigh: Boolean = True): TPointArray;
*)
procedure _LapeTPASortByY(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.SortByY(PBoolean(Params^[1])^);
end;

(*
TPointArray.SortFrom
~~~~~~~~~~~~~~~~~~~~
function TPointArray.SortFrom(From: TPoint): TPointArray;
*)
procedure _LapeTPASortFrom(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.SortFrom(PPoint(Params^[1])^);
end;

(*
TPointArray.SortByRow
~~~~~~~~~~~~~~~~~~~~~
function TPointArray.SortByRow(Reverse: Boolean = False): TPointArray;
*)
procedure _LapeTPASortByRow(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.SortByRow(PBoolean(Params^[1])^);
end;

(*
TPointArray.SortByColumn
~~~~~~~~~~~~~~~~~~~~~~~~
function TPointArray.SortByColumn(Reverse: Boolean = False): TPointArray;
*)
procedure _LapeTPASortByColumn(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.SortByColumn(PBoolean(Params^[1])^);
end;

(*
TPointArray.ExcludePie
~~~~~~~~~~~~~~~~~~~~~~
function TPointArray.ExcludePie(SD, ED, MinR, MaxR: Double; Center: TPoint): TPointArray;
*)
procedure _LapeTPAExcludePie(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.ExcludePie(PDouble(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^, PDouble(Params^[4])^, PPoint(Params^[5])^);
end;

(*
TPointArray.ExcludeDist
~~~~~~~~~~~~~~~~~~~~~~~
function TPointArray.ExcludeDist(Center: TPoint; MinDist, MaxDist: Double): TPointArray;
*)
procedure _LapeTPAExcludeDist(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.ExcludeDist(PPoint(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^);
end;

(*
TPointArray.ExcludePoints
~~~~~~~~~~~~~~~~~~~~~~~~~
function TPointArray.ExcludePoints(Points: TPointArray): TPointArray;
*)
procedure _LapeTPAExcludePoints(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.ExcludePoints(PPointArray(Params^[1])^);
end;

(*
TPointArray.ExcludePolygon
~~~~~~~~~~~~~~~~~~~~~~~~~~
function TPointArray.ExcludePolygon(Polygon: TPointArray): TPointArray;
*)
procedure _LapeTPAExcludePolygon(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.ExcludePolygon(PPointArray(Params^[1])^);
end;

(*
TPointArray.ExcludeBox
~~~~~~~~~~~~~~~~~~~~~~
function TPointArray.ExcludeBox(Box: TBox): TPointArray;
*)
procedure _LapeTPAExcludeBox(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.ExcludeBox(PBox(Params^[1])^);
end;

(*
TPointArray.Skeleton
~~~~~~~~~~~~~~~~~~~~
function TPointArray.Skeleton(FMin: Integer = 2; FMax: Integer = 6): TPointArray;
*)
procedure _LapeTPASkeleton(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.Skeleton(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TPointArray.Border
~~~~~~~~~~~~~~~~~~
function TPointArray.Border: TPointArray;
*)
procedure _LapeTPABorder(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.Border();
end;

(*
TPointArray.Edges
~~~~~~~~~~~~~~~~~
function TPointArray.Edges: TPointArray;
*)
procedure _LapeTPAEdges(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.Edges();
end;

(*
TPointArray.ConvexHull
~~~~~~~~~~~~~~~~~~~~~~
function TPointArray.ConvexHull: TPointArray;
*)
procedure _LapeTPAConvexHull(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.ConvexHull();
end;

(*
TPointArray.ShapeFill
~~~~~~~~~~~~~~~~~~~~~
function TPointArray.ShapeFill: TPointArray;
*)
procedure _LapeTPAShapeFill(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.ShapeFill();
end;

(*
TPointArray.Unique
~~~~~~~~~~~~~~~~~~
function TPointArray.Unique: TPointArray; override;
*)
procedure _LapeTPAUnique(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.Unique();
end;

(*
TPointArray.Split
~~~~~~~~~~~~~~~~~
function TPointArray.Split(Dist: Integer): T2DPointArray;
*)
procedure _LapeTPASplit1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := PPointArray(Params^[0])^.Split(PInteger(Params^[1])^);
end;

(*
TPointArray.Split
~~~~~~~~~~~~~~~~~
function TPointArray.Split(DistX, DistY: Integer): T2DPointArray;
*)
procedure _LapeTPASplit2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := PPointArray(Params^[0])^.Split(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TPointArray.Cluster
~~~~~~~~~~~~~~~~~~~
function TPointArray.Cluster(Dist: Integer): T2DPointArray;
*)
procedure _LapeTPACluster1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := PPointArray(Params^[0])^.Cluster(PInteger(Params^[1])^);
end;

(*
TPointArray.Cluster
~~~~~~~~~~~~~~~~~~~
function TPointArray.Cluster(DistX, DistY: Integer): T2DPointArray;
*)
procedure _LapeTPACluster2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := PPointArray(Params^[0])^.Cluster(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TPointArray.MinAreaRect
~~~~~~~~~~~~~~~~~~~~~~~
function TPointArray.MinAreaRect: TQuad;
*)
procedure _LapeTPAMinAreaRect(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PQuad(Result)^ := PPointArray(Params^[0])^.MinAreaRect();
end;

(*
TPointArray.Erode
~~~~~~~~~~~~~~~~~
function TPointArray.Erode(Iterations: Integer): TPointArray;
*)
procedure _LapeTPAErode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.Erode(PInteger(Params^[1])^);
end;

(*
TPointArray.Grow
~~~~~~~~~~~~~~~~
function TPointArray.Grow(Iterations: Integer): TPointArray;
*)
procedure _LapeTPAGrow(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.Grow(PInteger(Params^[1])^);
end;

(*
TPointArray.Partition
~~~~~~~~~~~~~~~~~~~~~
function TPointArray.Partition(Dist: Integer): T2DPointArray;
*)
procedure _LapeTPAPartition1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := PPointArray(Params^[0])^.Partition(PInteger(Params^[1])^);
end;

(*
TPointArray.Partition
~~~~~~~~~~~~~~~~~~~~~
function TPointArray.Partition(Width, Height: Integer): T2DPointArray;
*)
procedure _LapeTPAPartition2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := PPointArray(Params^[0])^.Partition(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TPointArray.PartitionEx
~~~~~~~~~~~~~~~~~~~~~~~
function TPointArray.PartitionEx(BoxWidth, BoxHeight: Integer): T2DPointArray;
*)
procedure _LapeTPAPartitionEx1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := PPointArray(Params^[0])^.PartitionEx(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TPointArray.PartitionEx
~~~~~~~~~~~~~~~~~~~~~~~
function TPointArray.PartitionEx(BoxWidth, BoxHeight: Integer): T2DPointArray;

PartitionEx with StartPoint.
*)
procedure _LapeTPAPartitionEx2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := PPointArray(Params^[0])^.PartitionEx(PPoint(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TPointArray.SortCircular
~~~~~~~~~~~~~~~~~~~~~~~~
function TPointArray.SortCircular(Center: TPoint; StartDegrees: Integer; Clockwise: Boolean): TPointArray;
*)
procedure _LapeTPASortCircular(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.SortCircular(PPoint(Params^[1])^, PInteger(Params^[2])^, PBoolean(Params^[3])^);
end;

procedure ImportTPA(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'TPointArray';

    addGlobalFunc('function TPointArray.CreateFromBox(Box: TBox; Filled: Boolean): TPointArray; static;', @_LapeTPACreateFromBox);
    addGlobalFunc('function TPointArray.CreateFromEllipse(Center: TPoint; RadiusX, RadiusY: Integer; Filled: Boolean): TPointArray; static;', @_LapeTPACreateFromEllipse);
    addGlobalFunc('function TPointArray.CreateFromCircle(Center: TPoint; Radius: Integer; Filled: Boolean): TPointArray; static;', @_LapeTPACreateFromCircle);
    addGlobalFunc('function TPointArray.CreateFromLine(Start, Stop: TPoint): TPointArray; static', @_LapeTPACreateFromLine);
    addGlobalFunc('function TPointArray.CreateFromPolygon(Poly: TPointArray; Filled: Boolean): TPointArray; static', @_LapeTPACreateFromPolygon);
    addGlobalFunc('function TPointArray.CreateFromSimplePolygon(Center: TPoint; Sides: Integer; Size: Integer; Filled: Boolean): TPointArray; static', @_LapeTPACreateFromSimplePolygon);

    addGlobalFunc('function TPointArray.ExcludePie(SD, ED, MinR, MaxR: Double; Center: TPoint): TPointArray;', @_LapeTPAExcludePie);
    addGlobalFunc('function TPointArray.ExcludeDist(Center: TPoint; MinDist, MaxDist: Double): TPointArray', @_LapeTPAExcludeDist);
    addGlobalFunc('function TPointArray.ExcludePoints(Points: TPointArray): TPointArray', @_LapeTPAExcludePoints);
    addGlobalFunc('function TPointArray.ExcludePolygon(Polygon: TPointArray): TPointArray', @_LapeTPAExcludePolygon);
    addGlobalFunc('function TPointArray.ExcludeBox(Box: TBox): TPointArray', @_LapeTPAExcludeBox);

    addGlobalFunc('function TPointArray.Skeleton(FMin: Integer = 2; FMax: Integer = 6): TPointArray;', @_LapeTPASkeleton);
    addGlobalFunc('function TPointArray.Border: TPointArray;', @_LapeTPABorder);
    addGlobalFunc('function TPointArray.Edges: TPointArray;', @_LapeTPAEdges);
    addGlobalFunc('function TPointArray.ConvexHull: TPointArray;', @_LapeTPAConvexHull);

    addGlobalFunc('function TPointArray.Erode(Iterations: Integer): TPointArray;', @_LapeTPAErode);
    addGlobalFunc('function TPointArray.Grow(Iterations: Integer): TPointArray;', @_LapeTPAGrow);

    addGlobalFunc('function TPointArray.Rows: T2DPointArray', @_LapeTPARows);
    addGlobalFunc('function TPointArray.Columns: T2DPointArray', @_LapeTPAColumns);
    addGlobalFunc('function TPointArray.Offset(P: TPoint): TPointArray; overload', @_LapeTPAOffset1);
    addGlobalFunc('function TPointArray.Offset(X, Y: Integer): TPointArray; overload', @_LapeTPAOffset2);
    addGlobalFunc('function TPointArray.FloodFill(StartPoint: TPoint): TPointArray;', @_LapeTPAFloodFill);
    addGlobalFunc('function TPointArray.ShapeFill: TPointArray', @_LapeTPAShapeFill);

    addGlobalFunc('function TPointArray.Extremes: TPointArray', @_LapeTPAExtremes);
    addGlobalFunc('function TPointArray.Bounds: TBox; overload', @_LapeTPABounds);
    addGlobalFunc('function TPointArray.MinAreaRect: TQuad', @_LapeTPAMinAreaRect);
    addGlobalFunc('function TPointArray.Mean: TPoint; overload', @_LapeTPAMean);

    addGlobalFunc('function TPointArray.Connect: TPointArray', @_LapeTPAConnect);
    addGlobalFunc('function TPointArray.Density: Double', @_LapeTPADensity);

    addGlobalFunc('function TPointArray.Invert(Bounds: TBox): TPointArray; overload', @_LapeTPAInvert1);
    addGlobalFunc('function TPointArray.Invert: TPointArray; overload', @_LapeTPAInvert2);

    addGlobalFunc('function TPointArray.Rotate(Radians: Double; Center: TPoint): TPointArray', @_LapeTPARotate);
    addGlobalFunc('function TPointArray.RotateEx(Radians: Double): TPointArray', @_LapeTPARotateEx);

    addGlobalFunc('function TPointArray.ReduceByDistance(Dist: Integer): TPointArray', @_LapeTPAReduceByDistance);

    addGlobalFunc('function TPointArray.PointsNearby(Other: TPointArray; MinDist, MaxDist: Double): TPointArray; overload', @_LapeTPAPointsNearby1);
    addGlobalFunc('function TPointArray.PointsNearby(Other: TPointArray; MinDistX, MinDistY, MaxDistX, MaxDistY: Double): TPointArray; overload', @_LapeTPAPointsNearby2);
    addGlobalFunc('function TPointArray.IsPointNearby(Other: TPoint; MinDist, MaxDist: Double): Boolean; overload', @_LapeTPAIsPointNearby1);
    addGlobalFunc('function TPointArray.IsPointNearby(Other: TPoint; MinDistX, MinDistY, MaxDistX, MaxDistY: Double): Boolean; overload', @_LapeTPAIsPointNearby2);

    addGlobalFunc('function TPointArray.NearestPoint(Other: TPoint): TPoint', @_LapeTPANearestPoint);

    addGlobalFunc('function TPointArray.Unique: TPointArray; override', @_LapeTPAUnique);

    addGlobalFunc('function TPointArray.Sort(Weights: TIntegerArray; LowToHigh: Boolean = True): TPointArray; overload;', @_LapeTPASort1);
    addGlobalFunc('function TPointArray.Sort(Weights: TDoubleArray; LowToHigh: Boolean = True): TPointArray; overload;', @_LapeTPASort2);
    addGlobalFunc('function TPointArray.SortByX(LowToHigh: Boolean = True): TPointArray;', @_LapeTPASortByX);
    addGlobalFunc('function TPointArray.SortByY(LowToHigh: Boolean = True): TPointArray;', @_LapeTPASortByY);
    addGlobalFunc('function TPointArray.SortFrom(From: TPoint): TPointArray;', @_LapeTPASortFrom);
    addGlobalFunc('function TPointArray.SortCircular(Center: TPoint; StartDegrees: Integer; Clockwise: Boolean): TPointArray', @_LapeTPASortCircular);

    addGlobalFunc('function TPointArray.SortByRow(Reverse: Boolean = False): TPointArray', @_LapeTPASortByRow);
    addGlobalFunc('function TPointArray.SortByColumn(Reverse: Boolean = False): TPointArray', @_LapeTPASortByColumn);

    addGlobalFunc('function TPointArray.Split(Dist: Integer): T2DPointArray; overload', @_LapeTPASplit1);
    addGlobalFunc('function TPointArray.Split(DistX, DistY: Integer): T2DPointArray; overload', @_LapeTPASplit2);
    addGlobalFunc('function TPointArray.Cluster(Dist: Integer): T2DPointArray; overload', @_LapeTPACluster1);
    addGlobalFunc('function TPointArray.Cluster(DistX, DistY: Integer): T2DPointArray; overload', @_LapeTPACluster2);

    addGlobalFunc('function TPointArray.Partition(Dist: Integer): T2DPointArray; overload', @_LapeTPAPartition1);
    addGlobalFunc('function TPointArray.Partition(Width, Height: Integer): T2DPointArray; overload', @_LapeTPAPartition2);
    addGlobalFunc('function TPointArray.PartitionEx(BoxWidth, BoxHeight: Integer): T2DPointArray; overload', @_LapeTPAPartitionEx1);
    addGlobalFunc('function TPointArray.PartitionEx(StartPoint: TPoint; BoxWidth, BoxHeight: Integer): T2DPointArray; overload', @_LapeTPAPartitionEx2);
    addGlobalFunc('function TPointArray.Intersection(Other: TPointArray): TPointArray', @_Lape_Point_Intersection);
    addGlobalFunc('function TPointArray.Difference(Other: TPointArray): TPointArray', @_Lape_Point_Difference);
    addGlobalFunc('function TPointArray.SymmetricDifference(Other: TPointArray): TPointArray', @_Lape_Point_SymmetricDifference);

    ImportingSection := '';
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportTPA);

end.
