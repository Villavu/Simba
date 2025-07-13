{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.import_quad;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script;

procedure ImportQuad(Script: TSimbaScript);

implementation

uses
  lptypes,
  simba.vartype_quad;

type
  PQuad = ^TQuad;
  PQuadArray = ^TQuadArray;

(*
Quad
====
The TQuad type is a record which stores four TPoints (Top, Right, Bottom, Left)

See: <https://en.wikipedia.org/wiki/Quadrilateral>

 - `Top` is the "most" top point (lowest Y)
 - `Right` is the "most" right point (highest X)
 - `Bottom` is the "most" bottom point (highest Y)
 - `Left` is the "most" left point (lowest X)
*)

(*
TQuad.Create
------------
```
function TQuad.Create(ATop, ARight, ABottom, ALeft: TPoint): TQuad; static;
```
*)
procedure _LapeQuad_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PQuad(Result)^ := TQuad.Create(PPoint(Params^[0])^, PPoint(Params^[1])^, PPoint(Params^[2])^, PPoint(Params^[3])^);
end;

(*
TQuad.Create
------------
```
function TQuad.Create(Box: TBox): TQuad; static;
```
*)
procedure _LapeQuad_CreateFromBox(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PQuad(Result)^ := TQuad.CreateFromBox(PBox(Params^[0])^);
end;

(*
TQuad.Create
------------
```
function TQuad.Create(Points: TPointArray): TQuad; static;
```
*)
procedure _LapeQuad_CreateFromPoints(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PQuad(Result)^ := TQuad.CreateFromPoints(PPointArray(Params^[0])^);
end;

(*
TQuad.Rotate
------------
```
function TQuad.Rotate(Angle: Double): TQuad;
```
*)
procedure _LapeQuad_Rotate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PQuad(Result)^ := PQuad(Params^[0])^.Rotate(PDouble(Params^[1])^);
end;

(*
TQuad.Contains
--------------
```
function TQuad.Contains(P: TPoint): Boolean;
```
*)
procedure _LapeQuad_Contains(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PQuad(Params^[0])^.Contains(PPoint(Params^[1])^);
end;

(*
TQuad.Offset
------------
```
function TQuad.Offset(P: TPoint): TQuad;
```
*)
procedure _LapeQuad_Offset(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PQuad(Result)^ := PQuad(Params^[0])^.Offset(PPoint(Params^[1])^);
end;

(*
TQuad.Expand
------------
```
function TQuad.Expand(Amount: Integer): TQuad;
```
*)
procedure _LapeQuad_Expand(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PQuad(Result)^ := PQuad(Params^[0])^.Expand(PInteger(Params^[1])^);
end;

(*
TQuad.NearestEdge
-----------------
```
function TQuad.NearestEdge(P: TPoint): TPoint;
```
*)
procedure _LapeQuad_NearestEdge(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PQuad(Params^[0])^.NearestEdge(PPoint(Params^[1])^);
end;

(*
TQuad.Normalize
---------------
```
function TQuad.Normalize: TQuad;
```
*)
procedure _LapeQuad_Normalize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PQuad(Result)^ := PQuad(Params^[0])^.Normalize();
end;

(*
TQuad.RandomPoint
-----------------
```
function TQuad.RandomPoint: TPoint;
```

Returns a completely random point in the quad.
*)
procedure _LapeQuad_RandomPoint(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PQuad(Params^[0])^.RandomPoint();
end;

(*
TQuad.RandomPointCenter
-----------------------
```
function TQuad.RandomPointCenter: TPoint;
```

Returns a random point in the quad which is weighted torwards the quad's center.
*)
procedure _LapeQuad_RandomPointCenter(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PQuad(Params^[0])^.RandomPointCenter();
end;

(*
TQuad.Area
----------
```
property TQuad.Area: Integer;
```
*)
procedure _LapeQuad_Area_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PQuad(Params^[0])^.Area;
end;

(*
TQuad.Corners
-------------
```
property TQuad.Corners: TPointArray;
```
*)
procedure _LapeQuad_Corners_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PQuad(Params^[0])^.Corners;
end;

(*
TQuad.Bounds
------------
```
property TQuad.Bounds: TBox;
```
*)
procedure _LapeQuad_Bounds_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBox(Result)^ := PQuad(Params^[0])^.Bounds;
end;

(*
TQuad.ShortSideLen
------------------
```
property TQuad.ShortSideLen: Integer;
```
*)
procedure _LapeQuad_ShortSideLen_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PQuad(Params^[0])^.ShortSideLen;
end;

(*
TQuad.LongSideLen
-----------------
```
property TQuad.LongSideLen: Integer;
```
*)
procedure _LapeQuad_LongSideLen_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PQuad(Params^[0])^.LongSideLen;
end;

(*
TQuad.Mean
----------
```
property TQuad.Mean: TPoint;
```
*)
procedure _LapeQuad_Mean_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PQuad(Params^[0])^.Mean;
end;

(*
TQuadArray.Merge
----------------
```
function TQuadArray.Merge: TQuadArray;
```
*)
procedure _LapeQuadArray_Merge(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PQuad(Result)^ := PQuadArray(Params^[0])^.Merge;
end;

(*
TQuadArray.Means
----------------
```
function TQuadArray.Means: TPointArray;
```
*)
procedure _LapeQuadArray_Means(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PQuadArray(Params^[0])^.Means;
end;

(*
TQuadArray.Offset
-----------------
```
function TQuadArray.Offset(P: TPoint): TQuadArray;
```
*)
procedure _LapeQuadArray_Offset(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PQuadArray(Result)^ := PQuadArray(Params^[0])^.Offset(PPoint(Params^[1])^);
end;

(*
TQuadArray.Expand
-----------------
```
function TQuadArray.Expand(Amount: Integer): TQuadArray;
```
*)
procedure _LapeQuadArray_Expand(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PQuadArray(Result)^ := PQuadArray(Params^[0])^.Expand(PInteger(Params^[1])^);
end;

(*
TQuadArray.ContainsPoint
------------------------
```
function TQuadArray.ContainsPoint(P: TPoint): Integer;
```
*)
procedure _LapeQuadArray_ContainsPoint(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PQuadArray(Params^[0])^.ContainsPoint(PPoint(Params^[1])^);
end;

(*
TQuadArray.SortFrom
-------------------
```
function TQuadArray.SortFrom(P: TPoint): TQuadArray;
```
*)
procedure _LapeQuadArray_SortFrom(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PQuadArray(Result)^ := PQuadArray(Params^[0])^.SortFrom(PPoint(Params^[1])^);
end;

(*
TQuadArray.SortByShortSide
--------------------------
```
function TQuadArray.SortByShortSide(LowToHigh: Boolean = True): TQuadArray;
```
*)
procedure _LapeQuadArray_SortByShortSide(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PQuadArray(Result)^ := PQuadArray(Params^[0])^.SortByShortSide(PBoolean(Params^[1])^);
end;

(*
TQuadArray.SortByLongSide
-------------------------
```
function TQuadArray.SortByLongSide(LowToHigh: Boolean = True): TQuadArray;
```
*)
procedure _LapeQuadArray_SortByLongSide(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PQuadArray(Result)^ := PQuadArray(Params^[0])^.SortByLongSide(PBoolean(Params^[1])^);
end;

(*
TQuadArray.SortByArea
---------------------
```
function TQuadArray.SortByArea(LowToHigh: Boolean = True): TQuadArray;
```
*)
procedure _LapeQuadArray_SortByArea(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PQuadArray(Result)^ := PQuadArray(Params^[0])^.SortByArea(PBoolean(Params^[1])^);
end;

(*
in
--
```
operator in(Left: TPoint; Right: TQuad): Boolean;
```
*)
procedure _LapePoint_IN_Quad(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PPoint(Params^[0])^ in PQuad(Params^[1])^;
end;

procedure ImportQuad(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    DumpSection := 'Quad';

    addGlobalFunc('function TQuad.Create(ATop, ARight, ABottom, ALeft: TPoint): TQuad; static; overload', @_LapeQuad_Create);
    addGlobalFunc('function TQuad.CreateFromBox(Box: TBox): TQuad; static; overload', @_LapeQuad_CreateFromBox);
    addGlobalFunc('function TQuad.CreateFromPoints(Points: TPointArray): TQuad; static; overload', @_LapeQuad_CreateFromPoints);

    addGlobalFunc('function TQuad.Rotate(Angle: Double): TQuad', @_LapeQuad_Rotate);
    addGlobalFunc('function TQuad.Contains(P: TPoint): Boolean', @_LapeQuad_Contains);
    addGlobalFunc('function TQuad.Offset(P: TPoint): TQuad', @_LapeQuad_Offset);
    addGlobalFunc('function TQuad.Expand(Amount: Integer): TQuad', @_LapeQuad_Expand);
    addGlobalFunc('function TQuad.NearestEdge(P: TPoint): TPoint', @_LapeQuad_NearestEdge);
    addGlobalFunc('function TQuad.Normalize: TQuad', @_LapeQuad_Normalize);
    addGlobalFunc('function TQuad.RandomPoint: TPoint', @_LapeQuad_RandomPoint);
    addGlobalFunc('function TQuad.RandomPointCenter: TPoint', @_LapeQuad_RandomPointCenter);

    addProperty('TQuad', 'Area', 'Integer', @_LapeQuad_Area_Read);
    addProperty('TQuad', 'Corners', 'TPointArray', @_LapeQuad_Corners_Read);
    addProperty('TQuad', 'Bounds', 'TBox', @_LapeQuad_Bounds_Read);
    addProperty('TQuad', 'ShortSideLen', 'Integer', @_LapeQuad_ShortSideLen_Read);
    addProperty('TQuad', 'LongSideLen', 'Integer', @_LapeQuad_LongSideLen_Read);
    addProperty('TQuad', 'Mean', 'TPoint', @_LapeQuad_Mean_Read);

    addGlobalFunc('function TQuadArray.Merge: TQuad;', @_LapeQuadArray_Merge);
    addGlobalFunc('function TQuadArray.Means: TPointArray;', @_LapeQuadArray_Means);
    addGlobalFunc('function TQuadArray.Offset(P: TPoint): TQuadArray;', @_LapeQuadArray_Offset);
    addGlobalFunc('function TQuadArray.Expand(SizeMod: Integer): TQuadArray;', @_LapeQuadArray_Expand);
    addGlobalFunc('function TQuadArray.ContainsPoint(P: TPoint): Integer;', @_LapeQuadArray_ContainsPoint);
    addGlobalFunc('function TQuadArray.SortFrom(From: TPoint): TQuadArray', @_LapeQuadArray_SortFrom);
    addGlobalFunc('function TQuadArray.SortByShortSide(LowToHigh: Boolean = True): TQuadArray', @_LapeQuadArray_SortByShortSide);
    addGlobalFunc('function TQuadArray.SortByLongSide(LowToHigh: Boolean = True): TQuadArray', @_LapeQuadArray_SortByLongSide);
    addGlobalFunc('function TQuadArray.SortByArea(LowToHigh: Boolean = True): TQuadArray;', @_LapeQuadArray_SortByArea);

    addGlobalFunc('operator in(Left: TPoint; Right: TQuad): Boolean;', @_LapePoint_IN_Quad);

    DumpSection := '';
  end;
end;

end.
