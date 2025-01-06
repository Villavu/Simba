{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.import_triangle;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script;

procedure ImportTriangle(Script: TSimbaScript);

implementation

uses
  lptypes,
  simba.vartype_triangle,
  simba.vartype_circle;

(*
TTriangle
=========
The TTriangle type is a record which stores three TPoints (A, B, C)

See: <https://en.wikipedia.org/wiki/Triangle>

The order of A,B,C should not matter in any functions for it.
*)

(*
TTriangle.Create
----------------
```
function TTriangle.Create(A, B, C: TPoint): TTriangle; static;
```
*)
procedure _LapeTriangle_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TTriangle(Result^) := TTriangle.Create(TPoint(Params^[0]^), TPoint(Params^[1]^), TPoint(Params^[2]^));
end;


(*
TTriangle.Centroid
------------------
```
function TTriangle.Centroid(): TPoint;
```

Same as Mean, as mean of a triangle is also the Centroid.
*)
procedure _LapeTriangle_Centroid(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TPoint(Result^) := TTriangle(Params^[0]^).Centroid();
end;

(*
TTriangle.SymmedianPoint
------------------------
```
function TTriangle.SymmedianPoint(): TPoint;
```

The symmedian point is the intersection of the symmedians (reflections of the medians across the angle bisectors).
*)
procedure _LapeTriangle_SymmedianPoint(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TPoint(Result^) := TTriangle(Params^[0]^).Centroid();
end;

(*
TTriangle.Incenter
------------------
```
function TTriangle.Incenter(): TPoint;
```

The incenter is the center of the incircle, which is the largest circle that can fit entirely
inside the triangle, tangent to all three sides.
*)
procedure _LapeTriangle_Incenter(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TPoint(Result^) := TTriangle(Params^[0]^).Centroid();
end;

(*
TTriangle.Rotate
----------------
```
function TTriangle.Rotate(Angle: Double): TTriangle;
```
*)
procedure _LapeTriangle_Rotate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TTriangle(Result^) := TTriangle(Params^[0]^).Rotate(Double(Params^[1]^));
end;

(*
TTriangle.Contains
------------------
```
function TTriangle.Contains(P: TPoint): Boolean;
```
*)
procedure _LapeTriangle_Contains(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Boolean(Result^) := TTriangle(Params^[0]^).Contains(TPoint(Params^[1]^));
end;

(*
TTriangle.Offset
----------------
```
function TTriangle.Offset(P: TPoint): TTriangle;
```
*)
procedure _LapeTriangle_Offset(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TTriangle(Result^) := TTriangle(Params^[0]^).Offset(TPoint(Params^[1]^));
end;

(*
TTriangle.Expand
----------------
```
function TTriangle.Expand(Amount: Integer): TTriangle;
```
*)
procedure _LapeTriangle_Expand(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TTriangle(Result^) := TTriangle(Params^[0]^).Expand(Integer(Params^[1]^));
end;

(*
TTriangle.NearestEdge
---------------------
```
function TTriangle.NearestEdge(P: TPoint): TPoint;
```

Returns the point on the outline of the triangle which is nearest to **P**
*)
procedure _LapeTriangle_NearestEdge(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TPoint(Result^) := TTriangle(Params^[0]^).NearestEdge(TPoint(Params^[1]^));
end;


(*
TTriangle.IsObtuse
------------------
```
function TTriangle.IsObtuse(): Boolean; overload;
function TTriangle.IsObtuse(out Obstuse: TPoint): Boolean; overload;
```

Returns True if the triangle is obtuse, can also return the point

Obtuse triangle is a triangle in which one of it angles is greater than 90 degrees. 
*)
procedure _LapeTriangle_IsObtuse(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Boolean(Result^) := TTriangle(Params^[0]^).IsObtuse();
end;

procedure _LapeTriangle_IsObtuseEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Boolean(Result^) := TTriangle(Params^[0]^).IsObtuse(TPoint(Params^[1]^));
end;

(*
TTriangle.Circumcircle
----------------------
```
function TTriangle.Circumcircle(): TCircle; 
```

Returns the circumcircle (the circle passing through points A, B, and C) of the triangle.
*)
procedure _LapeTriangle_Circumcircle(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TCircle(Result^) := TTriangle(Params^[0]^).Circumcircle();
end;

(*
TTriangle.Incircle
------------------
```
function TTriangle.Incircle(): TCircle;
```

Returns the incircle (the largest circle inscribed within the triangle A, B, and C).
*)
procedure _LapeTriangle_Incircle(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TCircle(Result^) := TTriangle(Params^[0]^).Incircle();
end;

(*
TTriangle.RandomPoint
---------------------
```
function TTriangle.RandomPoint(): TPoint;
```
*)
procedure _LapeTriangle_RandomPoint(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := TTriangle(Params^[0]^).RandomPoint();
end;

(*
TTriangle.RandomPointCenter
---------------------------
```
function TTriangle.RandomPointCenter(): TPoint;
```
*)
procedure _LapeTriangle_RandomPointCenter(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := TTriangle(Params^[0]^).RandomPointCenter();
end;

(*
TTriangle.Area
--------------
```
property TTriangle.Area: Integer;
```
*)
procedure _LapeTriangle_Area_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Integer(Result^) := TTriangle(Params^[0]^).Area;
end;

(*
TTriangle.Corners
-----------------
```
property TTriangle.Corners: TPointArray;
```
*)
procedure _LapeTriangle_Corners_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TPointArray(Result^) := TTriangle(Params^[0]^).Corners;
end;

(*
TTriangle.Bounds
----------------
```
property TTriangle.Bounds: TBox;
```
*)
procedure _LapeTriangle_Bounds_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TBox(Result^) := TTriangle(Params^[0]^).Bounds;
end;

(*
TTriangle.Mean
--------------
```
property TTriangle.Mean: TPoint;
```
*)
procedure _LapeTriangle_Mean_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TPoint(Result^) := TTriangle(Params^[0]^).Mean;
end;

(*
in
--
```
operator in(Left: TPoint; Right: TTriangle): Boolean;
```
*)
procedure _LapePoint_IN_Triangle(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Boolean(Result^) := TPoint(Params^[0]^) in TTriangle(Params^[1]^);
end;

procedure ImportTriangle(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    DumpSection := 'TTriangle';

    addGlobalFunc('function TTriangle.Create(A, B, C: TPoint): TTriangle; static', @_LapeTriangle_Create);

    addGlobalFunc('function TTriangle.Centroid(): TPoint', @_LapeTriangle_Centroid);
    addGlobalFunc('function TTriangle.SymmedianPoint(): TPoint', @_LapeTriangle_SymmedianPoint);
    addGlobalFunc('function TTriangle.Incenter(): TPoint', @_LapeTriangle_Incenter);

    addGlobalFunc('function TTriangle.Rotate(Angle: Double): TTriangle', @_LapeTriangle_Rotate);
    addGlobalFunc('function TTriangle.Contains(P: TPoint): Boolean', @_LapeTriangle_Contains);
    addGlobalFunc('function TTriangle.Offset(P: TPoint): TTriangle', @_LapeTriangle_Offset);
    addGlobalFunc('function TTriangle.Expand(Amount: Int32): TTriangle', @_LapeTriangle_Expand);
    addGlobalFunc('function TTriangle.NearestEdge(P: TPoint): TPoint', @_LapeTriangle_NearestEdge);
    addGlobalFunc('function TTriangle.IsObtuse(): Boolean; overload', @_LapeTriangle_IsObtuse);
    addGlobalFunc('function TTriangle.IsObtuse(out P: TPoint): Boolean; overload', @_LapeTriangle_IsObtuseEx);
    addGlobalFunc('function TTriangle.Circumcircle(): TCircle', @_LapeTriangle_Circumcircle);
    addGlobalFunc('function TTriangle.Incircle(): TCircle', @_LapeTriangle_Incircle);

    addGlobalFunc('function TTriangle.RandomPoint: TPoint', @_LapeTriangle_RandomPoint);
    addGlobalFunc('function TTriangle.RandomPointCenter: TPoint', @_LapeTriangle_RandomPointCenter);

    addProperty('TTriangle', 'Area',    'Integer',     @_LapeTriangle_Area_Read);
    addProperty('TTriangle', 'Corners', 'TPointArray', @_LapeTriangle_Corners_Read);
    addProperty('TTriangle', 'Bounds',  'TBox',        @_LapeTriangle_Bounds_Read);
    addProperty('TTriangle', 'Mean',    'TPoint',      @_LapeTriangle_Mean_Read);

    addGlobalFunc('operator in(Left: TPoint; Right: TTriangle): Boolean;', @_LapePoint_IN_Triangle);

    DumpSection := '';
  end;
end;

end.
