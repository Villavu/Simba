unit simba.import_boxarray;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.script_compiler;

procedure ImportBoxArray(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes,
  simba.algo_difference, simba.algo_intersection, simba.algo_symmetricDifference;

(*
TBoxArray
=========
TBoxArray methods
*)

(*
TBoxArray.Create
~~~~~~~~~~~~~~~~
> function TBoxArray.Create(Start: TPoint; Columns, Rows, Width, Height: Integer; Spacing: TPoint): TBoxArray; static;
*)
procedure _LapeBoxArray_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := TBoxArray.Create(PPoint(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PPoint(Params^[5])^);
end;

(*
TBoxArray.Pack
~~~~~~~~~~~~~~
> function TBoxArray.Pack: TBoxArray;
*)
procedure _LapeBoxArray_Pack(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.Pack();
end;

(*
TBoxArray.SortFrom
~~~~~~~~~~~~~~~~~~
> function TBoxArray.SortFrom(From: TPoint): TBoxArray;
*)
procedure _LapeBoxArray_SortFrom(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.SortFrom(PPoint(Params^[1])^);
end;

(*
TBoxArray.SortByX
~~~~~~~~~~~~~~~~~
> function TBoxArray.SortByX(LowToHigh: Boolean): TBoxArray;
*)
procedure _LapeBoxArray_SortByX(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.SortByX(PBoolean(Params^[1])^);
end;

(*
TBoxArray.SortByY
~~~~~~~~~~~~~~~~~
> function TBoxArray.SortByY(LowToHigh: Boolean; const Result: Pointer): TBoxArray;
*)
procedure _LapeBoxArray_SortByY(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.SortByY(PBoolean(Params^[1])^);
end;

(*
TBoxArray.SortByWidth
~~~~~~~~~~~~~~~~~~~~~
> function TBoxArray.SortByWidth(LowToHigh: Boolean; const Result: Pointer): TBoxArray;
*)
procedure _LapeBoxArray_SortByWidth(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.SortByWidth(PBoolean(Params^[1])^);
end;

(*
TBoxArray.SortByHeight
~~~~~~~~~~~~~~~~~~~~~~
> function TBoxArray.SortByHeight(LowToHigh: Boolean; const Result: Pointer): TBoxArray;
*)
procedure _LapeBoxArray_SortByHeight(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.SortByHeight(PBoolean(Params^[1])^);
end;

(*
TBoxArray.SortByArea
~~~~~~~~~~~~~~~~~~~~
> function TBoxArray.SortByArea(LowToHigh: Boolean; const Result: Pointer): TBoxArray;
*)
procedure _LapeBoxArray_SortByArea(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.SortByArea(PBoolean(Params^[1])^);
end;

(*
TBoxArray.Merge
~~~~~~~~~~~~~~~
> function TBoxArray.Merge: TBox;
*)
procedure _LapeBoxArray_Merge(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBox(Result)^ := PBoxArray(Params^[0])^.Merge();
end;

(*
TBoxArray.Centers
~~~~~~~~~~~~~~~~~
> function TBoxArray.Centers: TPointArray;
*)
procedure _LapeBoxArray_Centers(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PBoxArray(Params^[0])^.Centers();
end;

(*
TBoxArray.Offset
~~~~~~~~~~~~~~~~
> function TBoxArray.Offset(P: TPoint): TBoxArray;
*)
procedure _LapeBoxArray_Offset1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.Offset(PPoint(Params^[1])^);
end;

(*
TBoxArray.Offset
~~~~~~~~~~~~~~~~
> function TBoxArray.Offset(X, Y: Integer): TBoxArray;
*)
procedure _LapeBoxArray_Offset2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.Offset(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TBoxArray.Expand
~~~~~~~~~~~~~~~~
> function TBoxArray.Expand(SizeMod: Integer): TBoxArray;
*)
procedure _LapeBoxArray_Expand1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.Expand(PInteger(Params^[1])^);
end;

(*
TBoxArray.Expand
~~~~~~~~~~~~~~~~
> function TBoxArray.Expand(WidMod, HeiMod: Integer): TBoxArray;
*)
procedure _LapeBoxArray_Expand2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.Expand(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TBoxArray.ContainsPoint
~~~~~~~~~~~~~~~~~~~~~~~
> function TBoxArray.ContainsPoint(P: TPoint; out Index: Integer): Boolean;
*)
procedure _LapeBoxArray_ContainsPoint1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PBoxArray(Params^[0])^.ContainsPoint(PPoint(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TBoxArray.ContainsPoint
~~~~~~~~~~~~~~~~~~~~~~~
> function TBoxArray.ContainsPoint(P: TPoint): Boolean; overload;
*)
procedure _LapeBoxArray_ContainsPoint2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PBoxArray(Params^[0])^.ContainsPoint(PPoint(Params^[1])^);
end;

(*
TBoxArray.Sort
~~~~~~~~~~~~~~
> function TBoxArray.Sort(Weights: TIntegerArray; LowToHigh: Boolean = True): TBoxArray
*)
procedure _LapeBoxArray_Sort1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.Sort(PDoubleArray(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TBoxArray.Sort
~~~~~~~~~~~~~~
> function TBoxArray.Sort(Weights: TDoubleArray; LowToHigh: Boolean = True): TBoxArray
*)
procedure _LapeBoxArray_Sort2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.Sort(PDoubleArray(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TBoxArray.SymmetricDifference
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> function TBoxArray.SymmetricDifference(Other: TBoxArray): TBoxArray;
*)
procedure _Lape_Box_SymmetricDifference(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := Algo_Box_SymmetricDifference(PBoxArray(Params^[0])^, PBoxArray(Params^[1])^)
end;

(*
TBoxArray.Difference
~~~~~~~~~~~~~~~~~~~~
> function TBoxArray.Difference(Other: TBoxArray): TBoxArray;
*)
procedure _Lape_Box_Difference(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := Algo_Box_Difference(PBoxArray(Params^[0])^, PBoxArray(Params^[1])^)
end;

(*
TBoxArray.Intersection
~~~~~~~~~~~~~~~~~~~~~~
> function TBoxArray.Intersection(Other: TBoxArray): TBoxArray;
*)
procedure _Lape_Box_Intersection(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := Algo_Box_Intersection(PBoxArray(Params^[0])^, PBoxArray(Params^[1])^)
end;

procedure ImportBoxArray(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'TBoxArray';

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
    addGlobalFunc('function TBoxArray.Offset(P: TPoint): TBoxArray; overload;', @_LapeBoxArray_Offset1);
    addGlobalFunc('function TBoxArray.Offset(X, Y: Integer): TBoxArray; overload;', @_LapeBoxArray_Offset2);

    addGlobalFunc('function TBoxArray.Expand(SizeMod: Integer): TBoxArray; overload;', @_LapeBoxArray_Expand1);
    addGlobalFunc('function TBoxArray.Expand(WidMod, HeiMod: Integer): TBoxArray; overload;', @_LapeBoxArray_Expand2);

    addGlobalFunc('function TBoxArray.ContainsPoint(P: TPoint; out Index: Integer): Boolean; overload;', @_LapeBoxArray_ContainsPoint1);
    addGlobalFunc('function TBoxArray.ContainsPoint(P: TPoint): Boolean; overload;', @_LapeBoxArray_ContainsPoint2);

    addGlobalFunc('function TBoxArray.Difference(Other: TBoxArray): TBoxArray', @_Lape_Box_Difference);
    addGlobalFunc('function TBoxArray.SymmetricDifference(Other: TBoxArray): TBoxArray', @_Lape_Box_SymmetricDifference);
    addGlobalFunc('function TBoxArray.Intersection(Other: TBoxArray): TBoxArray', @_Lape_Box_Intersection);

    ImportingSection := '';
  end;
end;

end.