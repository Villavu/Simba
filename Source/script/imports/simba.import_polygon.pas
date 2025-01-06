{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.import_polygon;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script;

procedure ImportPolygon(Script: TSimbaScript);

implementation

uses
  lptypes,
  simba.vartype_polygon,
  simba.vartype_triangle;

(*
Polygon
=======
Polygon type
*)

(*
TPolygon.Bounds
---------------
```
function TPolygon.Bounds: TBox;
```
*)
procedure _LapePolygon_Bounds(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBox(Result)^ := PPolygon(Params^[0])^.Bounds;
end;

(*
TPolygon.Mean
-------------
```
function TPolygon.Mean: TPoint;
```
*)
procedure _LapePolygon_Mean(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PPolygon(Params^[0])^.Mean;
end;

(*
TPolygon.Area
-------------
```
function TPolygon.Area: Double;
```
*)
procedure _LapePolygon_Area(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := PPolygon(Params^[0])^.Area;
end;

(*
TPolygon.IsConvex
-----------------
```
function TPolygon.IsConvex(Polygon: TPointArray): Boolean;
```
Returns if the polygon is convex, order does not matter. A concave polygon will return False.
*)
procedure _LapePolygon_IsConvex(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PPolygon(Params^[0])^.IsConvex;
end;

(*
TPolygon.Contains
-----------------
```
function TPolygon.Contains(p: TPoint): Boolean;
```
Is the point in the polygon?
*)
procedure _LapePolygon_Contains(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PPolygon(Params^[0])^.Contains(PPoint(Params^[1])^);
end;

(*
TPolygon.ContainsLine
---------------------
```
function TPolygon.ContainsLine(p,q: TPoint): Boolean;
```
Returns True if the line fits within the bounds of the polygon.
*)
procedure _LapePolygon_ContainsLine(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PPolygon(Params^[0])^.ContainsLine(PPoint(Params^[1])^, PPoint(Params^[2])^);
end;

(*
TPolygon.NearestEdge
--------------------
```
function TPolygon.NearestEdge(P: TPoint): TPoint
```
*)
procedure _LapePolygon_NearestEdge(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PPolygon(Params^[0])^.NearestEdge(PPoint(Params^[1])^);
end;

(*
TPolygon.Expand
---------------
```
function TPolygon.Expand(Amount: Integer): TPolygon
```
*)
procedure _LapePolygon_Expand(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPolygon(Result)^ := PPolygon(Params^[0])^.Expand(PInteger(Params^[1])^);
end;

(*
TPolygon.Triangulate
--------------------
```
function TPolygon.Triangulate(MinArea: Double = 0; MaxDepth: Int32 = 0): TTriangleArray;
```

Break the polygon into triangles, the smallest possible polygon. The order of the
input does matter, if it fails, try to reverse the Poly with Poly.Reversed()

This is a custom algorithm by slacky, based around the concept of trimming "ears",
if you dont like the output, you may have more luck with rolling the Polygon before calling.

Two default params exists as well, `MinArea` and `MaxDepth`, they work in tandom,
`MinArea` parameter is for setting a minimum size of triangles added to result, and as this method
works iteratively, removing triangles in a circle around the shape over and over, `MaxDepth` refers
to the max number of rounds it has moved around the shape before it ignores `MinArea` paramater.
*)
procedure _LapePolygon_Triangulate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTriangleArray(Result)^ := PPolygon(Params^[0])^.Triangulate(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TPolygon.DouglasPeucker
-----------------------
> function TPolygon.DouglasPeucker(Epsilon: Double): TPolygon;

Returns the two points that are furthest away from eachother in a polygon.
*)
procedure _LapePolygon_DouglasPeucker(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPolygon(Result)^ := PPolygon(Params^[0])^.DouglasPeucker(PDouble(Params^[1])^);
end;

(*
TPolygon.FurthestPoints
-----------------------
> procedure TPolygon.FurthestPoints(out A,B: TPoint);

Returns the two points that are furthest away from eachother in a polygon.
*)
procedure _LapePolygon_FurthestPoints(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPolygon(Params^[0])^.FurthestPoints(PPoint(Params^[1])^, PPoint(Params^[2])^);
end;

procedure _LapePoint_IN_Polygon(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PPoint(Params^[0])^ in PPolygon(Params^[1])^;
end;

procedure ImportPolygon(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    DumpSection := 'TPolygon';

    addGlobalType('array of TPoint', 'TPolygon');
    addGlobalType('array of TPolygon', 'TPolygonArray');

    addGlobalFunc('function TPolygon.Bounds: TBox', @_LapePolygon_Bounds);
    addGlobalFunc('function TPolygon.Mean: TPoint', @_LapePolygon_Mean);
    addGlobalFunc('function TPolygon.Area: Double', @_LapePolygon_Area);
    addGlobalFunc('function TPolygon.IsConvex: Boolean', @_LapePolygon_IsConvex);
    addGlobalFunc('function TPolygon.Contains(p: TPoint): Boolean', @_LapePolygon_Contains);
    addGlobalFunc('function TPolygon.ContainsLine(a1, a2: TPoint): Boolean', @_LapePolygon_ContainsLine);
    addGlobalFunc('function TPolygon.NearestEdge(P: TPoint): TPoint', @_LapePolygon_NearestEdge);
    addGlobalFunc('function TPolygon.Expand(Amount: Integer): TPolygon', @_LapePolygon_Expand);
    addGlobalFunc('function TPolygon.Triangulate(MinArea: Single; MaxDepth: Int32): TTriangleArray;', @_LapePolygon_Triangulate);
    addGlobalFunc('function TPolygon.DouglasPeucker(Epsilon: Double): TPolygon;', @_LapePolygon_DouglasPeucker);
    addGlobalFunc('procedure TPolygon.FurthestPoints(out A, B: TPoint);', @_LapePolygon_FurthestPoints);

    addGlobalFunc('operator in(Left: TPoint; Right: TPolygon): Boolean;', @_LapePoint_IN_Polygon);

    DumpSection := '';
  end;
end;

end.

