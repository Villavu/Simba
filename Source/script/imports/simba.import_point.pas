unit simba.import_point;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script;

procedure ImportPoint(Script: TSimbaScript);

implementation

uses
  simba.vartype_point,
  simba.vartype_pointarray,
  simba.vartype_polygon,
  simba.vartype_quad, simba.vartype_circle,
  lptypes;


(*
TPoint
======
The TPoint type is a record which defines a X,Y integer coordinate.
*)

(*
Point
-----
```
function Point(X, Y: Integer): TPoint;
```
*)
procedure _LapePoint(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := Point(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

(*
TPoint.Create
-------------
```
function TPoint.Create(X, Y: Integer): TPoint; static;
```
*)
procedure _LapePoint_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := TPoint.Create(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

(*
TPoint.DistanceTo
-----------------
```
function TPoint.DistanceTo(Other: TPoint): Double;
```
*)
procedure _LapePoint_DistanceTo(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := PPoint(Params^[0])^.DistanceTo(PPoint(Params^[1])^);
end;

(*
TPoint.Rotate
-------------
```
function TPoint.Rotate(Radians: Double; Center: TPoint): TPoint;
```
*)
procedure _LapePoint_Rotate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PPoint(Params^[0])^.Rotate(PDouble(Params^[1])^, PPoint(Params^[2])^);
end;

(*
TPoint.Magnitude
----------------
```
function TPoint.Magnitude: Double;
```
*)
procedure _LapePoint_Magnitude(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := PPoint(Params^[0])^.Magnitude();
end;

(*
TPoint.AngleBetween
-------------------
```
function TPoint.AngleBetween(Other: TPoint): Double;
```
*)
procedure _LapePoint_AngleBetween(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := PPoint(Params^[0])^.AngleBetween(PPoint(Params^[1])^);
end;

(*
TPoint.Offset
-------------
```
function TPoint.Offset(X, Y: Integer): TPoint;
```
*)
procedure _LapePoint_Offset1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PPoint(Params^[0])^.Offset(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TPoint.Offset
-------------
```
function TPoint.Offset(P: TPoint): TPoint;
```
*)
procedure _LapePoint_Offset2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PPoint(Params^[0])^.Offset(PPoint(Params^[1])^);
end;

(*
TPoint.Random
-------------
```
function TPoint.Random(Min, Max: Integer): TPoint;
```
*)
procedure _LapePoint_Random1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PPoint(Params^[0])^.Random(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TPoint.Random
-------------
```
function TPoint.Random(Value: Integer): TPoint;
```
*)
procedure _LapePoint_Random2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PPoint(Params^[0])^.Random(PInteger(Params^[1])^);
end;

procedure _LapePoint_LT_Point(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PPoint(Params^[0])^ < PPoint(Params^[1])^;
end;

procedure _LapePoint_GT_Point(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PPoint(Params^[0])^ > PPoint(Params^[1])^;
end;

(*
TPoint +
--------
```
operator + (L, R: TPoint): TPoint;
```
*)
procedure _LapePoint_Plus_Point(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PPoint(Params^[0])^ + PPoint(Params^[1])^;
end;

(*
TPoint +=
---------
```
operator += (var L: TPoint; R: TPoint): TPoint;
```
*)
procedure _LapePoint_PlusAssign_Point(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Params^[0])^ += PPoint(Params^[1])^;
  PPoint(Result)^ := PPoint(Params^[0])^;
end;

(*
TPoint *
--------
```
operator * (L: TPoint; R: Double): TPoint;
```
*)
procedure _LapePoint_Multiply_Double(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PPoint(Params^[0])^ * PDouble(Params^[1])^;
end;


(*
TPoint \*=
----------
```
operator *= (var L: TPoint; R: Double): TPoint;
```
*)
procedure _LapePoint_MultiplyAssign_Double(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Params^[0])^ *= PDouble(Params^[1])^;
  PPoint(Result)^ := PPoint(Params^[0])^;
end;

(*
TPoint -
--------
```
operator - (L, R: TPoint): TPoint;
```
*)
procedure _LapePoint_Minus_Point(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PPoint(Params^[0])^ - PPoint(Params^[1])^;
end;

(*
TPoint -=
---------
```
operator -= (var L: TPoint; R: TPoint): TPoint;
```
*)
procedure _LapePoint_MinusAssign_Point(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Params^[0])^ -= PPoint(Params^[1])^;
  PPoint(Result)^ := PPoint(Params^[0])^;
end;

(*
TPointArray
===========
Methods relating to point arrays.
*)

(*
TPointArray.CreateFromBox
-------------------------
```
function TPointArray.CreateFromBox(Box: TBox; Filled: Boolean): TPointArray; static;
```
*)
procedure _LapeTPACreateFromBox(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := TPointArray.CreateFromBox(PBox(Params^[0])^, PBoolean(Params^[1])^);
end;

(*
TPointArray.CreateFromEllipse
-----------------------------
```
function TPointArray.CreateFromEllipse(Center: TPoint; RadiusX, RadiusY: Integer; Filled: Boolean): TPointArray; static;
```
*)
procedure _LapeTPACreateFromEllipse(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := TPointArray.CreateFromEllipse(PPoint(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PBoolean(Params^[3])^);
end;

(*
TPointArray.CreateFromCircle
----------------------------
```
function TPointArray.CreateFromCircle(Center: TPoint; Radius: Integer; Filled: Boolean): TPointArray; static;
```
*)
procedure _LapeTPACreateFromCircle(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := TPointArray.CreateFromCircle(PPoint(Params^[0])^, PInteger(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TPointArray.CreateFromLine
--------------------------
```
function TPointArray.CreateFromLine(Start, Stop: TPoint): TPointArray; static;
```
*)
procedure _LapeTPACreateFromLine(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := TPointArray.CreateFromLine(PPoint(Params^[0])^, PPoint(Params^[1])^);
end;

(*
TPointArray.CreateFromPolygon
-----------------------------
```
function TPointArray.CreateFromPolygon(Poly: TPointArray; Filled: Boolean): TPointArray; static;
```
*)
procedure _LapeTPACreateFromPolygon(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := TPointArray.CreateFromPolygon(PPointArray(Params^[0])^, PBoolean(Params^[1])^);
end;

(*
TPointArray.CreateFromSimplePolygon
-----------------------------------
```
function TPointArray.CreateFromSimplePolygon(Center: TPoint; Sides: Integer; Size: Integer; Filled: Boolean): TPointArray; static;
```
*)
procedure _LapeTPACreateFromSimplePolygon(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := TPointArray.CreateFromSimplePolygon(PPoint(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PBoolean(Params^[3])^);
end;

(*
TPointArray.CreateFromAxes
--------------------------
```
function TPointArray.CreateFromAxes(X, Y: TIntegerArray): TPointArray; static;
```
*)
procedure _LapeTPACreateFromAxes(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := TPointArray.CreateFromAxes(PIntegerArray(Params^[0])^, PIntegerArray(Params^[1])^);
end;

(*
TPointArray.Rows
----------------
```
function TPointArray.Rows: T2DPointArray;
```
*)
procedure _LapeTPARows(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := PPointArray(Params^[0])^.Rows();
end;

(*
TPointArray.Columns
-------------------
```
function TPointArray.Columns: T2DPointArray;
```
*)
procedure _LapeTPAColumns(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := PPointArray(Params^[0])^.Columns();
end;

(*
TPointArray.Offset
------------------
```
function TPointArray.Offset(P: TPoint): TPointArray;
```
*)
procedure _LapeTPAOffset(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.Offset(PPoint(Params^[1])^);
end;

(*
TPointArray.FloodFill
---------------------
```
function TPointArray.FloodFill(const StartPoint: TPoint; const EightWay: Boolean): TPointArray;
```
*)
procedure _LapeTPAFloodFill(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.FloodFill(PPoint(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TPointArray.Invert
------------------
```
function TPointArray.Invert(Bounds: TBox): TPointArray;
```
*)
procedure _LapeTPAInvert1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.Invert(PBox(Params^[1])^);
end;

(*
TPointArray.Invert
------------------
```
function TPointArray.Invert: TPointArray;
```
*)
procedure _LapeTPAInvert2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.Invert();
end;

(*
TPointArray.Bounds
------------------
```
function TPointArray.Bounds: TBox;
```
*)
procedure _LapeTPABounds(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBox(Result)^ := PPointArray(Params^[0])^.Bounds();
end;

(*
TPointArray.ReduceByDistance
----------------------------
```
function TPointArray.ReduceByDistance(Dist: Integer): TPointArray;
```
*)
procedure _LapeTPAReduceByDistance(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.ReduceByDistance(PInteger(Params^[1])^);
end;

(*
TPointArray.PointsNearby
------------------------
```
function TPointArray.PointsNearby(Other: TPointArray; MinDist, MaxDist: Double): TPointArray;
```
*)
procedure _LapeTPAPointsNearby1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.PointsNearby(PPointArray(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^);
end;

(*
TPointArray.PointsNearby
------------------------
```
function TPointArray.PointsNearby(Other: TPointArray; MinDistX, MinDistY, MaxDistX, MaxDistY: Double): TPointArray;
```
*)
procedure _LapeTPAPointsNearby2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.PointsNearby(PPointArray(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^, PDouble(Params^[4])^, PDouble(Params^[5])^);
end;

(*
TPointArray.IsPointNearby
-------------------------
```
function TPointArray.IsPointNearby(Other: TPoint; MinDist, MaxDist: Double): Boolean;
```
*)
procedure _LapeTPAIsPointNearby1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PPointArray(Params^[0])^.IsPointNearby(PPoint(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^);
end;

(*
TPointArray.IsPointNearby
-------------------------
```
function TPointArray.IsPointNearby(Other: TPoint; MinDistX, MinDistY, MaxDistX, MaxDistY: Double): Boolean;
```
*)
procedure _LapeTPAIsPointNearby2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PPointArray(Params^[0])^.IsPointNearby(PPoint(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^, PDouble(Params^[4])^, PDouble(Params^[5])^);
end;

(*
TPointArray.FurthestPoints
--------------------------
```
procedure TPointArray.FurthestPoints(out A, B: TPoint);
```
*)
procedure _LapeTPAFurthestPoints(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Params^[0])^.FurthestPoints(PPoint(Params^[1])^, PPoint(Params^[2])^);
end;

(*
TPointArray.NearestPoint
------------------------
```
function TPointArray.NearestPoint(Other: TPoint): TPoint;
```
*)
procedure _LapeTPANearestPoint(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PPointArray(Params^[0])^.NearestPoint(PPoint(Params^[1])^);
end;

(*
TPointArray.FurthestPoint
------------------------
```
function TPointArray.FurthestPoint(Other: TPoint): TPoint;
```
*)
procedure _LapeTPAFurthestPoint(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PPointArray(Params^[0])^.FurthestPoint(PPoint(Params^[1])^);
end;

(*
TPointArray.Density
-------------------
```
function TPointArray.Density: Double;
```
*)
procedure _LapeTPADensity(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := PPointArray(Params^[0])^.Density();
end;

(*
TPointArray.Connect
-------------------
```
function TPointArray.Connect: TPointArray;
```
*)
procedure _LapeTPAConnect(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.Connect();
end;

procedure _LapeTPAMean(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PPointArray(Params^[0])^.Mean;
end;

procedure _LapeTPAMedian(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PPointArray(Params^[0])^.Median;
end;

(*
TPointArray.Extremes
--------------------
```
function TPointArray.Extremes: TPointArray;
```
*)
procedure _LapeTPAExtremes(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.Extremes();
end;

(*
TPointArray.Rotate
------------------
```
function TPointArray.Rotate(Radians: Double; Center: TPoint): TPointArray;
```
*)
procedure _LapeTPARotate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.Rotate(PDouble(Params^[1])^, PPoint(Params^[2])^);
end;

(*
TPointArray.RotateEx
--------------------
```
function TPointArray.RotateEx(Radians: Double): TPointArray;
```
*)
procedure _LapeTPARotateEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.RotateEx(PDouble(Params^[1])^);
end;

(*
TPointArray.Sort
----------------
```
function TPointArray.Sort(Weights: TIntegerArray; LowToHigh: Boolean = True): TPointArray;
```
*)
procedure _LapeTPASort1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.Sort(PIntegerArray(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TPointArray.Sort
----------------
```
function TPointArray.Sort(Weights: TDoubleArray; LowToHigh: Boolean = True): TPointArray;
```
*)
procedure _LapeTPASort2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.Sort(PDoubleArray(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TPointArray.SortByX
-------------------
```
function TPointArray.SortByX(LowToHigh: Boolean = True): TPointArray;
```
*)
procedure _LapeTPASortByX(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.SortByX(PBoolean(Params^[1])^);
end;

(*
TPointArray.SortByY
-------------------
```
function TPointArray.SortByY(LowToHigh: Boolean = True): TPointArray;
```
*)
procedure _LapeTPASortByY(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.SortByY(PBoolean(Params^[1])^);
end;

(*
TPointArray.SortFrom
--------------------
```
function TPointArray.SortFrom(From: TPoint): TPointArray;
```
*)
procedure _LapeTPASortFrom(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.SortFrom(PPoint(Params^[1])^);
end;

(*
TPointArray.SortByRow
---------------------
```
function TPointArray.SortByRow(Reverse: Boolean = False): TPointArray;
```
*)
procedure _LapeTPASortByRow(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.SortByRow(PBoolean(Params^[1])^);
end;

(*
TPointArray.SortByColumn
------------------------
```
function TPointArray.SortByColumn(Reverse: Boolean = False): TPointArray;
```
*)
procedure _LapeTPASortByColumn(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.SortByColumn(PBoolean(Params^[1])^);
end;

(*
TPointArray.ExcludePie
----------------------
```
function TPointArray.ExcludePie(StartDegree, EndDegree, MinRadius, MaxRadius: Single; Center: TPoint): TPointArray;
```

Returns all points from `Self` are **not** inside the "pie slice" defined by StartDegree, EndDegree and MinRadius, MaxRadius and Center.

Note: Inverse of `ExcludePie`
*)
procedure _LapeTPAExcludePie(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.ExcludePie(PSingle(Params^[1])^, PSingle(Params^[2])^, PSingle(Params^[3])^, PSingle(Params^[4])^, PPoint(Params^[5])^);
end;

(*
TPointArray.ExcludeDist
-----------------------
```
function TPointArray.ExcludeDist(Center: TPoint; MinDist, MaxDist: Double): TPointArray;
```

Returns all points from `Self` that are **not** within `MinDist` and `MaxDist` from `Center`

Note: Inverse of `ExtractDist`
*)
procedure _LapeTPAExcludeDist(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.ExcludeDist(PPoint(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^);
end;

(*
TPointArray.ExcludePoints
-------------------------
```
function TPointArray.ExcludePoints(Points: TPointArray): TPointArray;
```

Returns all points from `Self` that are **not** in the `Points` array.
*)
procedure _LapeTPAExcludePoints(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.ExcludePoints(PPointArray(Params^[1])^);
end;

(*
TPointArray.ExcludePolygon
--------------------------
```
function TPointArray.ExcludePolygon(Polygon: TPolygon): TPointArray;
```

Returns all points from `Self` that are **not** inside the polygon.

Note: Inverse of `ExtractPolygon`
*)
procedure _LapeTPAExcludePolygon(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.ExcludePolygon(PPointArray(Params^[1])^);
end;

(*
TPointArray.ExcludeBox
----------------------
```
function TPointArray.ExcludeBox(Box: TBox): TPointArray;
```

Returns all points from `Self` that **not** inside the box.

Note: Inverse of `ExtractBox`
*)
procedure _LapeTPAExcludeBox(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.ExcludeBox(PBox(Params^[1])^);
end;

(*
TPointArray.ExcludeQuad
-----------------------
```
function TPointArray.ExcludeQuad(Quad: TQuad): TPointArray;
```

Returns all points from `Self` that **not** inside the quad.

Note: Inverse of `ExtractQuad`
*)
procedure _LapeTPAExcludeQuad(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.ExcludeQuad(PQuad(Params^[1])^);
end;

(*
TPointArray.ExtractDist
-----------------------
```
function TPointArray.ExtractDist(Center: TPoint; MinDist, MaxDist: Double): TPointArray;
```

Returns all points from `Self` that **are** within `MinDist` and `MaxDist` from `Center`

Note: Inverse of `ExtractDist`
*)
procedure _LapeTPAExtractDist(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.ExtractDist(PPoint(Params^[1])^, PSingle(Params^[2])^, PSingle(Params^[3])^);
end;

(*
TPointArray.ExtractPolygon
--------------------------
```
function TPointArray.ExtractPolygon(Polygon: TPolygon): TPointArray;
```

Returns all points from `Self` that are **inside** the polygon.
*)
procedure _LapeTPAExtractPolygon(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.ExtractPolygon(PPointArray(Params^[1])^);
end;

(*
TPointArray.ExtractBox
----------------------
```
function TPointArray.ExtractBox(Box: TBox): TPointArray;
```

Returns all points from `Self` that are **inside** the box.
*)
procedure _LapeTPAExtractBox(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.ExtractBox(PBox(Params^[1])^);
end;

(*
TPointArray.ExtractQuad
-----------------------
```
function TPointArray.ExtractQuad(Quad: TQuad): TPointArray;
```

Returns all points from `Self` that are **inside** the quad.
*)
procedure _LapeTPAExtractQuad(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.ExtractQuad(PQuad(Params^[1])^);
end;

(*
TPointArray.ExtractPie
----------------------
```
function TPointArray.ExtractPie(StartDegree, EndDegree, MinRadius, MaxRadius: Single; Center: TPoint): TPointArray;
```

Returns all points from `Self` are **inside** the "pie slice" defined by StartDegree, EndDegree and MinRadius, MaxRadius and Center.
*)
procedure _LapeTPAExtractPie(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.ExtractPie(PSingle(Params^[1])^, PSingle(Params^[2])^, PSingle(Params^[3])^, PSingle(Params^[4])^, PPoint(Params^[5])^);
end;

(*
TPointArray.Skeleton
--------------------
```
function TPointArray.Skeleton(FMin: Integer = 2; FMax: Integer = 6): TPointArray;
```
*)
procedure _LapeTPASkeleton(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.Skeleton(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TPointArray.Border
------------------
```
function TPointArray.Border: TPointArray;
```
*)
procedure _LapeTPABorder(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.Border();
end;

(*
TPointArray.Edges
-----------------
```
function TPointArray.Edges: TPointArray;
```
*)
procedure _LapeTPAEdges(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.Edges();
end;

(*
TPointArray.ConvexHull
----------------------
```
function TPointArray.ConvexHull: TPolygon;
```
*)
procedure _LapeTPAConvexHull(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPolygon(Result)^ := PPointArray(Params^[0])^.ConvexHull();
end;

(*
TPointArray.ShapeFill
---------------------
```
function TPointArray.ShapeFill: TPointArray;
```
*)
procedure _LapeTPAShapeFill(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.ShapeFill();
end;

(*
TPointArray.Split
-----------------
```
function TPointArray.Split(Dist: Integer): T2DPointArray;
```
*)
procedure _LapeTPASplit1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := PPointArray(Params^[0])^.Split(PInteger(Params^[1])^);
end;

(*
TPointArray.Split
-----------------
```
function TPointArray.Split(DistX, DistY: Integer): T2DPointArray;
```
*)
procedure _LapeTPASplit2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := PPointArray(Params^[0])^.Split(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TPointArray.Cluster
-------------------
```
function TPointArray.Cluster(Dist: Integer): T2DPointArray;
```
*)
procedure _LapeTPACluster1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := PPointArray(Params^[0])^.Cluster(PInteger(Params^[1])^);
end;

(*
TPointArray.Cluster
-------------------
```
function TPointArray.Cluster(DistX, DistY: Integer): T2DPointArray;
```
*)
procedure _LapeTPACluster2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := PPointArray(Params^[0])^.Cluster(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TPointArray.MinAreaRect
-----------------------
```
function TPointArray.MinAreaRect: TQuad;
```
*)
procedure _LapeTPAMinAreaRect(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PQuad(Result)^ := PPointArray(Params^[0])^.MinAreaRect();
end;

(*
TPointArray.MinAreaCircle
-------------------------
```
function TPointArray.MinAreaCircle: TCircle;
```
*)
procedure _LapeTPAMinAreaCircle(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCircle(Result)^ := PPointArray(Params^[0])^.MinAreaCircle();
end;

(*
TPointArray.Erode
-----------------
```
function TPointArray.Erode(Iterations: Integer): TPointArray;
```
*)
procedure _LapeTPAErode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.Erode(PInteger(Params^[1])^);
end;

(*
TPointArray.Grow
----------------
```
function TPointArray.Grow(Iterations: Integer): TPointArray;
```
*)
procedure _LapeTPAGrow(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.Grow(PInteger(Params^[1])^);
end;

(*
TPointArray.Partition
---------------------
```
function TPointArray.Partition(Dist: Integer): T2DPointArray;
```
*)
procedure _LapeTPAPartition1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := PPointArray(Params^[0])^.Partition(PInteger(Params^[1])^);
end;

(*
TPointArray.Partition
---------------------
```
function TPointArray.Partition(Width, Height: Integer): T2DPointArray;
```
*)
procedure _LapeTPAPartition2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := PPointArray(Params^[0])^.Partition(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TPointArray.PartitionEx
-----------------------
```
function TPointArray.PartitionEx(BoxWidth, BoxHeight: Integer): T2DPointArray;
```
*)
procedure _LapeTPAPartitionEx1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := PPointArray(Params^[0])^.PartitionEx(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TPointArray.PartitionEx
-----------------------
```
function TPointArray.PartitionEx(BoxWidth, BoxHeight: Integer): T2DPointArray;
```

PartitionEx with StartPoint.
*)
procedure _LapeTPAPartitionEx2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := PPointArray(Params^[0])^.PartitionEx(PPoint(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TPointArray.SortCircular
------------------------
```
function TPointArray.SortCircular(Center: TPoint; StartDegrees: Integer; Clockwise: Boolean): TPointArray;
```
*)
procedure _LapeTPASortCircular(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.SortCircular(PPoint(Params^[1])^, PInteger(Params^[2])^, PBoolean(Params^[3])^);
end;

(*
TPointArray.DistanceTransform
-----------------------------
```
function TPointArray.DistanceTransform: TSingleMatrix;
```
*)
procedure _LapeTPADistanceTransform(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Result)^ := PPointArray(Params^[0])^.DistanceTransform();
end;

(*
TPointArray.DistanceTransform
-----------------------------
```
function TPointArray.DistanceTransform: TSingleMatrix;
```
*)
procedure _LapeTPAQuickSkeleton(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.QuickSkeleton();
end;

(*
TPointArray.Circularity
-----------------------
```
function TPointArray.Circularity: Double;
```
*)
procedure _LapeTPACircularity(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := PPointArray(Params^[0])^.Circularity();
end;

(*
TPointArray.ConcaveHull
-----------------------
```
function TPointArray.ConcaveHull(Epsilon:Double=2.5; kCount:Int32=5): TPolygon;
```
*)
procedure _LapeTPAConcaveHull(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPolygon(Result)^ := PPointArray(Params^[0])^.ConcaveHull(PDouble(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TPointArray.ConcaveHullEx
-------------------------
```
function TPointArray.ConcaveHullEx(MaxLeap: Double=-1; Epsilon:Double=2): TPolygonArray;
```
*)
procedure _LapeTPAConcaveHullEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPolygonArray(Result)^ := PPointArray(Params^[0])^.ConcaveHullEx(PDouble(Params^[1])^, PDouble(Params^[2])^);
end;

(*
TPointArray.ConvexityDefects
----------------------------
```
function TPointArray.ConvexityDefects(Epsilon: Single; Mode: EConvexityDefects = EConvexityDefects.NONE): TPointArray;
```

Finds the defects in relation to a convex hull of the given concave hull.

 - EConvexityDefects.All     -> Keeps all convex points as well.
 - EConvexityDefects.Minimal -> Keeps the convex points that was linked to a defect
 - EConvexityDefects.None    -> Only defects
*)
procedure _LapeTPAConvexityDefects(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.ConvexityDefects(PSingle(Params^[1])^, EConvexityDefects(Params^[2]^));
end;

procedure _LapeTPAToAxes(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Params^[0])^.ToAxes(PIntegerArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

(*
T2DPointArray
=============
Arrays of TPointArrays.
*)

(*
T2DPointArray.Offset
--------------------
```
function T2DPointArray.Offset(P: TPoint): T2DPointArray;
```
*)
procedure _LapeATPA_Offset1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.Offset(PPoint(Params^[1])^);
end;

(*
T2DPointArray.Sort
------------------
```
function T2DPointArray.Sort(Weights: TIntegerArray; LowToHigh: Boolean = True): T2DPointArray;
```
*)
procedure _LapeATPA_Sort1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.Sort(PIntegerArray(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
T2DPointArray.Sort
------------------
```
function T2DPointArray.Sort(Weights: TDoubleArray; LowToHigh: Boolean = True): T2DPointArray;
```
*)
procedure _LapeATPA_Sort2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.Sort(PDoubleArray(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
T2DPointArray.SortFromSize
--------------------------
```
function T2DPointArray.SortFromSize(Size: Integer): T2DPointArray;
```
*)
procedure _LapeATPA_SortFromSize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.SortFromSize(PInteger(Params^[1])^);
end;

(*
T2DPointArray.SortFromIndex
---------------------------
```
function T2DPointArray.SortFromIndex(From: TPoint; Index: Integer = 0): T2DPointArray;
```
*)
procedure _LapeATPA_SortFromIndex(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.SortFromIndex(PPoint(Params^[1])^, PInteger(Params^[2])^);
end;

(*
T2DPointArray.SortFromFirstPoint
--------------------------------
```
function T2DPointArray.SortFromFirstPoint(From: TPoint): T2DPointArray;
```
*)
procedure _LapeATPA_SortFromFirstPoint(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.SortFromFirstPoint(PPoint(Params^[1])^);
end;

(*
T2DPointArray.SortFromFirstPointX
---------------------------------
```
function T2DPointArray.SortFromFirstPointX(From: TPoint): T2DPointArray;
```
*)
procedure _LapeATPA_SortFromFirstPointX(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.SortFromFirstPointX(PPoint(Params^[1])^);
end;

(*
T2DPointArray.SortFromFirstPointY
---------------------------------
```
function T2DPointArray.SortFromFirstPointY(From: TPoint): T2DPointArray;
```
*)
procedure _LapeATPA_SortFromFirstPointY(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.SortFromFirstPointY(PPoint(Params^[1])^);
end;

(*
T2DPointArray.SortFrom
----------------------
```
function T2DPointArray.SortFrom(From: TPoint): T2DPointArray;
```
*)
procedure _LapeATPA_SortFrom(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.SortFrom(PPoint(Params^[1])^);
end;

(*
T2DPointArray.SortByArea
------------------------
```
function T2DPointArray.SortByArea(LowToHigh: Boolean): T2DPointArray;
```
*)
procedure _LapeATPA_SortByArea(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.SortByArea(PBoolean(Params^[1])^);
end;

(*
T2DPointArray.SortBySize
------------------------
```
function T2DPointArray.SortBySize(LowToHigh: Boolean): T2DPointArray;
```
*)
procedure _LapeATPA_SortBySize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.SortBySize(PBoolean(Params^[1])^);
end;

(*
T2DPointArray.SortByDensity
---------------------------
```
function T2DPointArray.SortByDensity(LowToHigh: Boolean): T2DPointArray;
```
*)
procedure _LapeATPA_SortByDensity(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.SortByDensity(PBoolean(Params^[1])^);
end;

(*
T2DPointArray.SortByX
---------------------
```
function T2DPointArray.SortByX(LowToHigh: Boolean): T2DPointArray;
```
*)
procedure _LapeATPA_SortByX(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.SortByX(PBoolean(Params^[1])^);
end;

(*
T2DPointArray.SortByY
---------------------
```
function T2DPointArray.SortByY(LowToHigh: Boolean): T2DPointArray;
```
*)
procedure _LapeATPA_SortByY(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.SortByY(PBoolean(Params^[1])^);
end;

(*
T2DPointArray.SortByShortSide
-----------------------------
```
function T2DPointArray.SortByShortSide(LowToHigh: Boolean): T2DPointArray;
```
*)
procedure _LapeATPA_SortByShortSide(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.SortByShortSide(PBoolean(Params^[1])^);
end;

(*
T2DPointArray.SortByLongSide
----------------------------
```
function T2DPointArray.SortByLongSide(LowToHigh: Boolean): T2DPointArray;
```
*)
procedure _LapeATPA_SortByLongSide(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.SortByLongSide(PBoolean(Params^[1])^);
end;

(*
T2DPointArray.ExtractSize
-------------------------
```
function T2DPointArray.ExtractSize(Len: Integer; KeepIf: EComparator): T2DPointArray;
```
*)
procedure _LapeATPA_ExtractSize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.ExtractSize(PInteger(Params^[1])^, PComparator(Params^[2])^);
end;

(*
T2DPointArray.ExtractSizeEx
---------------------------
```
function T2DPointArray.ExtractSize(Len: Integer; KeepIf: EComparator): T2DPointArray;
```
*)
procedure _LapeATPA_ExtractSizeEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.ExtractSizeEx(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
T2DPointArray.ExtractDimensions
-------------------------------
```
function T2DPointArray.ExtractDimensions(MinShortSide, MinLongSide, MaxShortSide, MaxLongSide: Integer): T2DPointArray;
```
*)
procedure _LapeATPA_ExtractDimensions(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.ExtractDimensions(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

(*
T2DPointArray.ExtractDimensionsEx
---------------------------------
```
function T2DPointArray.ExtractDimensionsEx(MinShortSide, MinLongSide: Integer): T2DPointArray;
```
*)
procedure _LapeATPA_ExtractDimensionsEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.ExtractDimensionsEx(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
T2DPointArray.ExcludeSize
-------------------------
```
function T2DPointArray.ExcludeSize(Len: Integer; KeepIf: EComparator): T2DPointArray;
```
*)
procedure _LapeATPA_ExcludeSize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.ExcludeSize(PInteger(Params^[1])^, PComparator(Params^[2])^);
end;

(*
T2DPointArray.ExcludeSizeEx
---------------------------
```
function T2DPointArray.ExcludeSize(Len: Integer; KeepIf: EComparator): T2DPointArray;
```
*)
procedure _LapeATPA_ExcludeSizeEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.ExcludeSizeEx(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
T2DPointArray.ExcludeDimensions
-------------------------------
```
function T2DPointArray.ExcludeDimensions(MinShortSide, MinLongSide, MaxShortSide, MaxLongSide: Integer): T2DPointArray;
```
*)
procedure _LapeATPA_ExcludeDimensions(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.ExcludeDimensions(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

(*
T2DPointArray.ExcludeDimensionsEx
---------------------------------
```
function T2DPointArray.ExcludeDimensionsEx(MinShortSide, MinLongSide: Integer): T2DPointArray;
```
*)
procedure _LapeATPA_ExcludeDimensionsEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.ExcludeDimensionsEx(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
T2DPointArray.Smallest
----------------------
```
function T2DPointArray.Smallest: TPointArray;
```
*)
procedure _LapeATPA_Smallest(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := P2DPointArray(Params^[0])^.Smallest();
end;

(*
T2DPointArray.Largest
---------------------
```
function T2DPointArray.Largest: TPointArray;
```
*)
procedure _LapeATPA_Largest(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := P2DPointArray(Params^[0])^.Largest();
end;

(*
T2DPointArray.Bounds
--------------------
```
function T2DPointArray.Bounds: TBox;
```
*)
procedure _LapeATPA_Bounds(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBox(Result)^ := P2DPointArray(Params^[0])^.Bounds();
end;

(*
T2DPointArray.BoundsArray
-------------------------
```
function T2DPointArray.BoundsArray: TBoxArray;
```
*)
procedure _LapeATPA_BoundsArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := P2DPointArray(Params^[0])^.BoundsArray();
end;

procedure _LapeATPA_Mean(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := P2DPointArray(Params^[0])^.Mean();
end;

(*
T2DPointArray.Means
-------------------
```
function T2DPointArray.Means: TPointArray;
```
*)
procedure _LapeATPA_Means(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := P2DPointArray(Params^[0])^.Means();
end;

(*
T2DPointArray.Merge
-------------------
```
function T2DPointArray.Merge: TPointArray;
```
*)
procedure _LapeATPA_Merge(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := P2DPointArray(Params^[0])^.Merge();
end;

(*
T2DPointArray.Intersection
--------------------------
```
function T2DPointArray.Intersection: TPointArray;
```

Returns the points which exist in all arrays.
*)
procedure _LapeATPA_Intersection(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := P2DPointArray(Params^[0])^.Intersection();
end;

procedure ImportPoint(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    DumpSection := 'TPoint';

    addGlobalFunc('function Point(X, Y: Integer): TPoint', @_LapePoint);

    addGlobalFunc('function TPoint.Create(X, Y: Integer): TPoint; static;', @_LapePoint_Create);
    addGlobalFunc('function TPoint.DistanceTo(Other: TPoint): Double;', @_LapePoint_DistanceTo);
    addGlobalFunc('function TPoint.Rotate(Radians: Double; Center: TPoint): TPoint;', @_LapePoint_Rotate);
    addGlobalFunc('function TPoint.Magnitude: Double;', @_LapePoint_Magnitude);
    addGlobalFunc('function TPoint.AngleBetween(Other: TPoint): Double;', @_LapePoint_AngleBetween);
    addGlobalFunc('function TPoint.Offset(X, Y: Integer): TPoint; overload;', @_LapePoint_Offset1);
    addGlobalFunc('function TPoint.Offset(P: TPoint): TPoint; overload;', @_LapePoint_Offset2);
    addGlobalFunc('function TPoint.Random(Min, Max: Integer): TPoint; overload;', @_LapePoint_Random1);
    addGlobalFunc('function TPoint.Random(Value: Integer): TPoint; overload;', @_LapePoint_Random2);

    addGlobalFunc('operator < (L, R: TPoint): Boolean;', @_LapePoint_LT_Point);
    addGlobalFunc('operator > (L, R: TPoint): Boolean;', @_LapePoint_GT_Point);
    addGlobalFunc('operator + (L, R: TPoint): TPoint;', @_LapePoint_Plus_Point);
    addGlobalFunc('operator += (var L: TPoint; R: TPoint): TPoint;', @_LapePoint_PlusAssign_Point);
    addGlobalFunc('operator - (L, R: TPoint): TPoint;', @_LapePoint_Minus_Point);
    addGlobalFunc('operator -= (var L: TPoint; R: TPoint): TPoint;', @_LapePoint_MinusAssign_Point);
    addGlobalFunc('operator * (L: TPoint; R: Double): TPoint;', @_LapePoint_Multiply_Double);
    addGlobalFunc('operator *= (var L: TPoint; R: Double): TPoint;', @_LapePoint_MultiplyAssign_Double);

    DumpSection := 'TPointArray';

    addGlobalFunc('function TPointArray.CreateFromBox(Box: TBox; Filled: Boolean): TPointArray; static;', @_LapeTPACreateFromBox);
    addGlobalFunc('function TPointArray.CreateFromEllipse(Center: TPoint; RadiusX, RadiusY: Integer; Filled: Boolean): TPointArray; static;', @_LapeTPACreateFromEllipse);
    addGlobalFunc('function TPointArray.CreateFromCircle(Center: TPoint; Radius: Integer; Filled: Boolean): TPointArray; static;', @_LapeTPACreateFromCircle);
    addGlobalFunc('function TPointArray.CreateFromLine(Start, Stop: TPoint): TPointArray; static', @_LapeTPACreateFromLine);
    addGlobalFunc('function TPointArray.CreateFromPolygon(Poly: TPointArray; Filled: Boolean): TPointArray; static', @_LapeTPACreateFromPolygon);
    addGlobalFunc('function TPointArray.CreateFromSimplePolygon(Center: TPoint; Sides: Integer; Size: Integer; Filled: Boolean): TPointArray; static', @_LapeTPACreateFromSimplePolygon);
    addGlobalFunc('function TPointArray.CreateFromAxes(X, Y: TIntegerArray): TPointArray; static', @_LapeTPACreateFromAxes);

    addGlobalFunc('function TPointArray.ExcludePie(StartDegree, EndDegree, MinRadius, MaxRadius: Single; Center: TPoint): TPointArray;', @_LapeTPAExcludePie);
    addGlobalFunc('function TPointArray.ExcludeDist(Center: TPoint; MinDist, MaxDist: Double): TPointArray', @_LapeTPAExcludeDist);
    addGlobalFunc('function TPointArray.ExcludePoints(Points: TPointArray): TPointArray', @_LapeTPAExcludePoints);
    addGlobalFunc('function TPointArray.ExcludePolygon(Polygon: TPolygon): TPointArray', @_LapeTPAExcludePolygon);
    addGlobalFunc('function TPointArray.ExcludeBox(Box: TBox): TPointArray', @_LapeTPAExcludeBox);
    addGlobalFunc('function TPointArray.ExcludeQuad(Quad: TQuad): TPointArray', @_LapeTPAExcludeQuad);

    addGlobalFunc('function TPointArray.ExtractDist(Center: TPoint; MinDist, MaxDist: Single): TPointArray', @_LapeTPAExtractDist);
    addGlobalFunc('function TPointArray.ExtractPolygon(Polygon: TPolygon): TPointArray', @_LapeTPAExtractPolygon);
    addGlobalFunc('function TPointArray.ExtractBox(Box: TBox): TPointArray', @_LapeTPAExtractBox);
    addGlobalFunc('function TPointArray.ExtractQuad(Quad: TQuad): TPointArray', @_LapeTPAExtractQuad);
    addGlobalFunc('function TPointArray.ExtractPie(StartDegree, EndDegree, MinRadius, MaxRadius: Single; Center: TPoint): TPointArray', @_LapeTPAExtractPie);

    addGlobalFunc('function TPointArray.Skeleton(FMin: Integer = 2; FMax: Integer = 6): TPointArray;', @_LapeTPASkeleton);
    addGlobalFunc('function TPointArray.Border: TPointArray;', @_LapeTPABorder);
    addGlobalFunc('function TPointArray.Edges: TPointArray;', @_LapeTPAEdges);
    addGlobalFunc('function TPointArray.ConvexHull: TPolygon;', @_LapeTPAConvexHull);

    addGlobalFunc('function TPointArray.Erode(Iterations: Integer): TPointArray;', @_LapeTPAErode);
    addGlobalFunc('function TPointArray.Grow(Iterations: Integer): TPointArray;', @_LapeTPAGrow);

    addGlobalFunc('function TPointArray.Rows: T2DPointArray', @_LapeTPARows);
    addGlobalFunc('function TPointArray.Columns: T2DPointArray', @_LapeTPAColumns);
    addGlobalFunc('function TPointArray.Offset(P: TPoint): TPointArray', @_LapeTPAOffset);
    addGlobalFunc('function TPointArray.FloodFill(const StartPoint: TPoint; const EightWay: Boolean): TPointArray;', @_LapeTPAFloodFill);
    addGlobalFunc('function TPointArray.ShapeFill: TPointArray', @_LapeTPAShapeFill);

    addGlobalFunc('function TPointArray.Mean: TPoint', @_LapeTPAMean);
    addGlobalFunc('function TPointArray.Median: TPoint', @_LapeTPAMedian);
    addGlobalFunc('function TPointArray.Extremes: TPointArray', @_LapeTPAExtremes);
    addGlobalFunc('function TPointArray.Bounds: TBox', @_LapeTPABounds);
    addGlobalFunc('function TPointArray.MinAreaRect: TQuad', @_LapeTPAMinAreaRect);
    addGlobalFunc('function TPointArray.MinAreaCircle: TCircle', @_LapeTPAMinAreaCircle);

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

    addGlobalFunc('procedure TPointArray.FurthestPoints(out A, B: TPoint)', @_LapeTPAFurthestPoints);
    addGlobalFunc('function TPointArray.NearestPoint(Other: TPoint): TPoint', @_LapeTPANearestPoint);
    addGlobalFunc('function TPointArray.FurthestPoint(Other: TPoint): TPoint', @_LapeTPAFurthestPoint);

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

    addGlobalFunc('function TPointArray.DistanceTransform: TSingleMatrix;', @_LapeTPADistanceTransform);
    addGlobalFunc('function TPointArray.QuickSkeleton(): TPointArray;', @_LapeTPAQuickSkeleton);

    addGlobalFunc('function TPointArray.Circularity: Double;', @_LapeTPACircularity);

    addGlobalFunc('function TPointArray.ConcaveHull(Epsilon: Double = 2.5; kCount: Integer = 5): TPolygon;', @_LapeTPAConcaveHull);
    addGlobalFunc('function TPointArray.ConcaveHullEx(MaxLeap: Double = -1; Epsilon: Double = 2): TPolygonArray;', @_LapeTPAConcaveHullEx);

    addGlobalType('enum(NONE, ALL, MINIMAL)', 'EConvexityDefects');
    addGlobalFunc('function TPointArray.ConvexityDefects(Epsilon: Single = 0; Mode: EConvexityDefects = EConvexityDefects.NONE): TPointArray;', @_LapeTPAConvexityDefects);

    addGlobalFunc('procedure TPointArray.ToAxes(out X, Y: TIntegerArray);', @_LapeTPAToAxes);

    DumpSection := 'T2DPointArray';

    addGlobalFunc('function T2DPointArray.Offset(P: TPoint): T2DPointArray', @_LapeATPA_Offset1);

    addGlobalFunc('function T2DPointArray.Sort(Weights: TIntegerArray; LowToHigh: Boolean = True): T2DPointArray; overload', @_LapeATPA_Sort1);
    addGlobalFunc('function T2DPointArray.Sort(Weights: TDoubleArray; LowToHigh: Boolean = True): T2DPointArray; overload', @_LapeATPA_Sort2);
    addGlobalFunc('function T2DPointArray.SortFromSize(Size: Integer): T2DPointArray', @_LapeATPA_SortFromSize);
    addGlobalFunc('function T2DPointArray.SortFromIndex(From: TPoint; Index: Integer = 0): T2DPointArray', @_LapeATPA_SortFromIndex);
    addGlobalFunc('function T2DPointArray.SortFromFirstPoint(From: TPoint): T2DPointArray', @_LapeATPA_SortFromFirstPoint);
    addGlobalFunc('function T2DPointArray.SortFromFirstPointX(From: TPoint): T2DPointArray', @_LapeATPA_SortFromFirstPointX);
    addGlobalFunc('function T2DPointArray.SortFromFirstPointY(From: TPoint): T2DPointArray', @_LapeATPA_SortFromFirstPointY);

    addGlobalFunc('function T2DPointArray.SortFrom(From: TPoint): T2DPointArray', @_LapeATPA_SortFrom);

    addGlobalFunc('function T2DPointArray.SortByArea(LowToHigh: Boolean): T2DPointArray', @_LapeATPA_SortByArea);
    addGlobalFunc('function T2DPointArray.SortBySize(LowToHigh: Boolean): T2DPointArray', @_LapeATPA_SortBySize);
    addGlobalFunc('function T2DPointArray.SortByDensity(LowToHigh: Boolean): T2DPointArray', @_LapeATPA_SortByDensity);

    addGlobalFunc('function T2DPointArray.SortByX(LowToHigh: Boolean): T2DPointArray', @_LapeATPA_SortByX);
    addGlobalFunc('function T2DPointArray.SortByY(LowToHigh: Boolean): T2DPointArray', @_LapeATPA_SortByY);

    addGlobalFunc('function T2DPointArray.SortByShortSide(LowToHigh: Boolean): T2DPointArray', @_LapeATPA_SortByShortSide);
    addGlobalFunc('function T2DPointArray.SortByLongSide(LowToHigh: Boolean): T2DPointArray', @_LapeATPA_SortByLongSide);

    addGlobalFunc('function T2DPointArray.ExtractSize(Len: Integer; KeepIf: EComparator): T2DPointArray', @_LapeATPA_ExtractSize);
    addGlobalFunc('function T2DPointArray.ExtractSizeEx(MinLen, MaxLen: Integer): T2DPointArray', @_LapeATPA_ExtractSizeEx);
    addGlobalFunc('function T2DPointArray.ExtractDimensions(MinShortSide, MinLongSide, MaxShortSide, MaxLongSide: Integer): T2DPointArray', @_LapeATPA_ExtractDimensions);
    addGlobalFunc('function T2DPointArray.ExtractDimensionsEx(MinShortSide, MinLongSide: Integer): T2DPointArray', @_LapeATPA_ExtractDimensionsEx);

    addGlobalFunc('function T2DPointArray.ExcludeSize(Len: Integer; RemoveIf: EComparator): T2DPointArray', @_LapeATPA_ExcludeSize);
    addGlobalFunc('function T2DPointArray.ExcludeSizeEx(MinLen, MaxLen: Integer): T2DPointArray', @_LapeATPA_ExcludeSizeEx);
    addGlobalFunc('function T2DPointArray.ExcludeDimensions(MinShortSide, MinLongSide, MaxShortSide, MaxLongSide: Integer): T2DPointArray', @_LapeATPA_ExcludeDimensions);
    addGlobalFunc('function T2DPointArray.ExcludeDimensionsEx(MinShortSide, MinLongSide: Integer): T2DPointArray', @_LapeATPA_ExcludeDimensionsEx);

    addGlobalFunc('function T2DPointArray.Smallest: TPointArray', @_LapeATPA_Smallest);
    addGlobalFunc('function T2DPointArray.Largest: TPointArray', @_LapeATPA_Largest);

    addGlobalFunc('function T2DPointArray.Bounds: TBox', @_LapeATPA_Bounds);
    addGlobalFunc('function T2DPointArray.BoundsArray: TBoxArray', @_LapeATPA_BoundsArray);

    addGlobalFunc('function T2DPointArray.Mean: TPoint', @_LapeATPA_Mean);
    addGlobalFunc('function T2DPointArray.Means: TPointArray', @_LapeATPA_Means);
    addGlobalFunc('function T2DPointArray.Merge: TPointArray;', @_LapeATPA_Merge);
    addGlobalFunc('function T2DPointArray.Intersection: TPointArray; overload;', @_LapeATPA_Intersection);

    DumpSection := '';
  end;
end;

end.
