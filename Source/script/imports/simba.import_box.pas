unit simba.import_box;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script;

procedure ImportBox(Script: TSimbaScript);

implementation

uses
  lptypes,
  simba.vartype_box;

(*
TBox
====
The `TBox` type is a record which defines a box from top left and bottom right coords.

- `X1` is the **top left** `X` coord
- `Y1` is the **top left** `Y` coord
- `X2` is the **bottom right** `X` coord
- `Y2` is the **bottom right** `Y` coord
*)

(*
Box
---
```
function Box(X1, Y1, X2, Y2: Integer): TBox;
```
*)
procedure _LapeBox1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBox(Result)^ := TBox.Create(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

(*
Box
---
```
function Box(Mid: TPoint; XRad, YRad: Integer): TBox;
```
*)
procedure _LapeBox2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBox(Result)^ := TBox.Create(PPoint(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TBox.Create
-----------
```
function TBox.Create(X1, Y1, X2, Y2: Integer): TBox; static;
```
*)
procedure _LapeBox_Create1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBox(Result)^ := TBox.Create(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TBox.Create
-----------
```
function TBox.Create(Center: TPoint; XRad, YRad: Integer): TBox; static;
```
*)
procedure _LapeBox_Create2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBox(Result)^ := TBox.Create(PPoint(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TBox.Area
---------
```
property TBox.Area: Int64;
```
*)
procedure _LapeBox_Area(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := PBox(Params^[0])^.Area;
end;

(*
TBox.Size
---------
```
property TBox.Size: TSize;
```
*)
procedure _LapeBox_Size_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSize(Result)^ := PBox(Params^[0])^.Size;
end;

(*
TBox.Expand
-----------
```
function TBox.Expand(SizeMod: Integer): TBox;
```
*)
procedure _LapeBox_Expand1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBox(Result)^ := PBox(Params^[0])^.Expand(PInteger(Params^[1])^);
end;

(*
TBox.Expand
-----------
```
function TBox.Expand(WidMod, HeiMod: Integer): TBox;
```
*)
procedure _LapeBox_Expand2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBox(Result)^ := PBox(Params^[0])^.Expand(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TBox.Expand
-----------
```
function TBox.Expand(SizeMod: Integer; MaxBounds: TBox): TBox;
```
*)
procedure _LapeBox_Expand3(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBox(Result)^ := PBox(Params^[0])^.Expand(PInteger(Params^[1])^, PBox(Params^[2])^);
end;

(*
TBox.Expand
-----------
```
function TBox.Expand(WidMod, HeiMod: Integer; MaxBounds: TBox): TBox;
```
*)
procedure _LapeBox_Expand4(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBox(Result)^ := PBox(Params^[0])^.Expand(PInteger(Params^[1])^, PInteger(Params^[2])^, PBox(Params^[3])^);
end;

(*
TBox.Contains
-------------
```
function TBox.Contains(Point: TPoint): Boolean;
```
*)
procedure _LapeBox_Contains(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PBox(Params^[0])^.Contains(PPoint(Params^[1])^);
end;

(*
TBox.Partition
--------------
```
function TBox.Partition(Rows, Cols: Integer): TBoxArray;
```
*)
procedure _LapeBox_Partition(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := PBox(Params^[0])^.Partition(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TBox.Offset
-----------
```
function TBox.Offset(P: TPoint): TBox;
```
*)
procedure _LapeBox_Offset1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBox(Result)^ := PBox(Params^[0])^.Offset(PPoint(Params^[1])^);
end;

(*
TBox.Combine
------------
```
function TBox.Combine(Other: TBox): TBox;
```
*)
procedure _LapeBox_Combine(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBox(Result)^ := PBox(Params^[0])^.Combine(PBox(Params^[1])^);
end;

(*
TBox.Invert
-----------
```
function TBox.Invert(Space: TBox): TBoxArray;
```
*)
procedure _LapeBox_Invert(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := PBox(Params^[0])^.Invert(PBox(Params^[1])^);
end;

(*
TBox.NearestEdge
----------------
```
function TBox.NearestEdge(P: TPoint): TPoint;
```
*)
procedure _LapeBox_NearestEdge(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PBox(Params^[0])^.NearestEdge(PPoint(Params^[1])^);
end;

(*
TBox.Intersect
--------------
```
function TBox.Intersect(P: TPoint): TPoint;
```
*)
procedure _LapeBox_Intersect(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PBox(Params^[0])^.Intersect(PPoint(Params^[1])^);
end;

(*
TBox.Clip
---------
```
function TBox.Clip(Other: TBox): TBox;
```
*)
procedure _LapeBox_Clip(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBox(Result)^ := PBox(Params^[0])^.Clip(PBox(Params^[1])^);
end;

(*
TBox.Normalize
--------------
```
function TBox.Normalize: TBox;
```
*)
procedure _LapeBox_Normalize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBox(Result)^ := PBox(Params^[0])^.Normalize();
end;

(*
TBox.Width
----------
```
property TBox.Width: Integer;
```
*)
procedure _LapeBox_Width(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PBox(Params^[0])^.Width;
end;

(*
TBox.Height
-----------
```
property TBox.Height: Integer;
```
*)
procedure _LapeBox_Height(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PBox(Params^[0])^.Height;
end;

(*
TBox.Center
-----------
```
property TBox.Center: TPoint;
```
*)
procedure _LapeBox_Center(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PBox(Params^[0])^.Center;
end;

(*
TBox.Corners
------------
```
function TBox.Corners: TPointArray;
```
*)
procedure _LapeBox_Corners(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PBox(Params^[0])^.Corners();
end;

(*
TBox.RandomPoint
----------------
```
function TBox.RandomPoint: TPoint;
```

Returns a completely random point in the box.
*)
procedure _LapeBox_RandomPoint(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PBox(Params^[0])^.RandomPoint();
end;

(*
TBox.RandomPointCenter
----------------------
```
function TBox.RandomPointCenter: TPoint;
```

Returns a random point in the box which is weighted towards the box center.
*)
procedure _LapeBox_RandomPointCenter(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PBox(Params^[0])^.RandomPointCenter();
end;

(*
TBox.TopLeft
------------
```
property TBox.TopLeft: TPoint;
```
*)
procedure _LapeBox_TopLeft(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PBox(Params^[0])^.TopLeft;
end;

(*
TBox.TopRight
-------------
```
property TBox.TopRight: TPoint;
```
*)
procedure _LapeBox_TopRight(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PBox(Params^[0])^.TopRight;
end;

(*
TBox.BottomLeft
---------------
```
property TBox.BottomLeft: TPoint;
```
*)
procedure _LapeBox_BottomLeft(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PBox(Params^[0])^.BottomLeft;
end;

(*
TBox.BottomRight
----------------
```
property TBox.BottomRight: TPoint;
```
*)
procedure _LapeBox_BottomRight(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PBox(Params^[0])^.BottomRight;
end;

(*
TBoxArray.Create
----------------
```
function TBoxArray.Create(Start: TPoint; Columns, Rows, Width, Height: Integer; Spacing: TPoint): TBoxArray; static;
```
*)
procedure _LapeBoxArray_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := TBoxArray.Create(PPoint(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PPoint(Params^[5])^);
end;

(*
TBoxArray.Pack
--------------
```
function TBoxArray.Pack: TBoxArray;
```
*)
procedure _LapeBoxArray_Pack(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.Pack();
end;

(*
TBoxArray.SortFrom
------------------
```
function TBoxArray.SortFrom(From: TPoint): TBoxArray;
```
*)
procedure _LapeBoxArray_SortFrom(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.SortFrom(PPoint(Params^[1])^);
end;

(*
TBoxArray.SortByX
-----------------
```
function TBoxArray.SortByX(LowToHigh: Boolean): TBoxArray;
```
*)
procedure _LapeBoxArray_SortByX(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.SortByX(PBoolean(Params^[1])^);
end;

(*
TBoxArray.SortByY
-----------------
```
function TBoxArray.SortByY(LowToHigh: Boolean; const Result: Pointer): TBoxArray;
```
*)
procedure _LapeBoxArray_SortByY(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.SortByY(PBoolean(Params^[1])^);
end;

(*
TBoxArray.SortByWidth
---------------------
```
function TBoxArray.SortByWidth(LowToHigh: Boolean; const Result: Pointer): TBoxArray;
```
*)
procedure _LapeBoxArray_SortByWidth(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.SortByWidth(PBoolean(Params^[1])^);
end;

(*
TBoxArray.SortByHeight
----------------------
```
function TBoxArray.SortByHeight(LowToHigh: Boolean; const Result: Pointer): TBoxArray;
```
*)
procedure _LapeBoxArray_SortByHeight(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.SortByHeight(PBoolean(Params^[1])^);
end;

(*
TBoxArray.SortByArea
--------------------
```
function TBoxArray.SortByArea(LowToHigh: Boolean; const Result: Pointer): TBoxArray;
```
*)
procedure _LapeBoxArray_SortByArea(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.SortByArea(PBoolean(Params^[1])^);
end;

(*
TBoxArray.Merge
---------------
```
function TBoxArray.Merge: TBox;
```
*)
procedure _LapeBoxArray_Merge(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBox(Result)^ := PBoxArray(Params^[0])^.Merge();
end;

(*
TBoxArray.Centers
-----------------
```
function TBoxArray.Centers: TPointArray;
```
*)
procedure _LapeBoxArray_Centers(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PBoxArray(Params^[0])^.Centers();
end;

(*
TBoxArray.Offset
----------------
```
function TBoxArray.Offset(P: TPoint): TBoxArray;
```
*)
procedure _LapeBoxArray_Offset(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.Offset(PPoint(Params^[1])^);
end;

(*
TBoxArray.Expand
----------------
```
function TBoxArray.Expand(SizeMod: Integer): TBoxArray;
```
*)
procedure _LapeBoxArray_Expand1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.Expand(PInteger(Params^[1])^);
end;

(*
TBoxArray.Expand
----------------
```
function TBoxArray.Expand(WidMod, HeiMod: Integer): TBoxArray;
```
*)
procedure _LapeBoxArray_Expand2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.Expand(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TBoxArray.ContainsPoint
-----------------------
```
function TBoxArray.ContainsPoint(P: TPoint; out Index: Integer): Boolean;
```
*)
procedure _LapeBoxArray_ContainsPoint1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PBoxArray(Params^[0])^.ContainsPoint(PPoint(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TBoxArray.ContainsPoint
-----------------------
```
function TBoxArray.ContainsPoint(P: TPoint): Boolean; overload;
```
*)
procedure _LapeBoxArray_ContainsPoint2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PBoxArray(Params^[0])^.ContainsPoint(PPoint(Params^[1])^);
end;

(*
TBoxArray.Sort
--------------
```
function TBoxArray.Sort(Weights: TIntegerArray; LowToHigh: Boolean = True): TBoxArray
```
*)
procedure _LapeBoxArray_Sort1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.Sort(PDoubleArray(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TBoxArray.Sort
--------------
```
function TBoxArray.Sort(Weights: TDoubleArray; LowToHigh: Boolean = True): TBoxArray
```
*)
procedure _LapeBoxArray_Sort2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.Sort(PDoubleArray(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure ImportBox(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    DumpSection := 'Box';

    addGlobalFunc('function Box(X1, Y1, X2, Y2: Integer): TBox; overload', @_LapeBox1);
    addGlobalFunc('function Box(Mid: TPoint; XRad, YRad: Integer): TBox; overload', @_LapeBox2);

    addGlobalFunc('function TBox.Create(X1, Y1, X2, Y2: Integer): TBox; static; overload;', @_LapeBox_Create1);
    addGlobalFunc('function TBox.Create(Center: TPoint; XRad, YRad: Integer): TBox; static; overload;', @_LapeBox_Create2);

    addGlobalFunc('function TBox.Expand(SizeMod: Integer): TBox; overload;', @_LapeBox_Expand1);
    addGlobalFunc('function TBox.Expand(SizeMod: Integer; MaxBounds: TBox): TBox; overload;', @_LapeBox_Expand3);
    addGlobalFunc('function TBox.Expand(WidMod, HeiMod: Integer): TBox; overload;', @_LapeBox_Expand2);
    addGlobalFunc('function TBox.Expand(WidMod, HeiMod: Integer; MaxBounds: TBox): TBox; overload;', @_LapeBox_Expand4);
    addGlobalFunc('function TBox.Contains(Point: TPoint): Boolean;', @_LapeBox_Contains);
    addGlobalFunc('function TBox.Partition(Rows, Cols: Integer): TBoxArray;', @_LapeBox_Partition);
    addGlobalFunc('function TBox.Offset(P: TPoint): TBox; overload;', @_LapeBox_Offset1);
    addGlobalFunc('function TBox.Combine(Other: TBox): TBox;', @_LapeBox_Combine);
    addGlobalFunc('function TBox.Invert(Space: TBox): TBoxArray;', @_LapeBox_Invert);

    addGlobalFunc('function TBox.NearestEdge(P: TPoint): TPoint;', @_LapeBox_NearestEdge);
    addGlobalFunc('function TBox.Intersect(P: TPoint): TPoint;', @_LapeBox_Intersect);

    addGlobalFunc('function TBox.Clip(Other: TBox): TBox', @_LapeBox_Clip);
    addGlobalFunc('function TBox.Normalize: TBox', @_LapeBox_Normalize);
    addGlobalFunc('function TBox.Corners: TPointArray;', @_LapeBox_Corners);

    addProperty('TBox', 'Width', 'Integer', @_LapeBox_Width);
    addProperty('TBox', 'Height', 'Integer', @_LapeBox_Height);
    addProperty('TBox', 'Center', 'TPoint', @_LapeBox_Center);
    addProperty('TBox', 'Area', 'Int64', @_LapeBox_Area);
    addProperty('TBox', 'Size', 'TSize', @_LapeBox_Size_Read);

    addGlobalFunc('function TBox.RandomPoint: TPoint', @_LapeBox_RandomPoint);
    addGlobalFunc('function TBox.RandomPointCenter: TPoint', @_LapeBox_RandomPointCenter);

    addGlobalFunc('property TBox.TopLeft: TPoint;', @_LapeBox_TopLeft);
    addGlobalFunc('property TBox.TopRight: TPoint;', @_LapeBox_TopRight);
    addGlobalFunc('property TBox.BottomLeft: TPoint;', @_LapeBox_BottomLeft);
    addGlobalFunc('property TBox.BottomRight: TPoint;', @_LapeBox_BottomRight);

    addGlobalFunc('function TBoxArray.Create(Start: TPoint; Columns, Rows, Width, Height: Integer; Spacing: TPoint): TBoxArray; static;', @_LapeBoxArray_Create);
    addGlobalFunc('function TBoxArray.Pack: TBoxArray;', @_LapeBoxArray_Pack);

    addGlobalFunc('function TBoxArray.Sort(Weights: TIntegerArray; LowToHigh: Boolean = True): TBoxArray; overload', @_LapeBoxArray_Sort1);
    addGlobalFunc('function TBoxArray.Sort(Weights: TDoubleArray; LowToHigh: Boolean = True): TBoxArray; overload', @_LapeBoxArray_Sort2);

    addGlobalFunc('function TBoxArray.SortFrom(From: TPoint): TBoxArray', @_LapeBoxArray_SortFrom);
    addGlobalFunc('function TBoxArray.SortByX(LowToHigh: Boolean = True): TBoxArray', @_LapeBoxArray_SortByX);
    addGlobalFunc('function TBoxArray.SortByY(LowToHigh: Boolean = True): TBoxArray', @_LapeBoxArray_SortByY);
    addGlobalFunc('function TBoxArray.SortByWidth(LowToHigh: Boolean = True): TBoxArray', @_LapeBoxArray_SortByWidth);
    addGlobalFunc('function TBoxArray.SortByHeight(LowToHigh: Boolean = True): TBoxArray', @_LapeBoxArray_SortByHeight);
    addGlobalFunc('function TBoxArray.SortByArea(LowToHigh: Boolean = True): TBoxArray;', @_LapeBoxArray_SortByArea);

    addGlobalFunc('function TBoxArray.Merge: TBox;', @_LapeBoxArray_Merge);
    addGlobalFunc('function TBoxArray.Centers: TPointArray;', @_LapeBoxArray_Centers);
    addGlobalFunc('function TBoxArray.Offset(P: TPoint): TBoxArray;', @_LapeBoxArray_Offset);

    addGlobalFunc('function TBoxArray.Expand(SizeMod: Integer): TBoxArray; overload;', @_LapeBoxArray_Expand1);
    addGlobalFunc('function TBoxArray.Expand(WidMod, HeiMod: Integer): TBoxArray; overload;', @_LapeBoxArray_Expand2);

    addGlobalFunc('function TBoxArray.ContainsPoint(P: TPoint; out Index: Integer): Boolean; overload;', @_LapeBoxArray_ContainsPoint1);
    addGlobalFunc('function TBoxArray.ContainsPoint(P: TPoint): Boolean; overload;', @_LapeBoxArray_ContainsPoint2);

    DumpSection := '';
  end;
end;

end.
