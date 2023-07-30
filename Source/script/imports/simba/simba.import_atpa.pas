unit simba.import_atpa;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.script_compiler;

procedure ImportATPA(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes,
  simba.atpa;

(*
T2DPointArray
=============
Methods relating to arrays of TPointArray.
*)

(*
T2DPointArray.Offset
~~~~~~~~~~~~~~~~~~~~
> function T2DPointArray.Offset(P: TPoint): T2DPointArray;
*)
procedure _LapeATPA_Offset1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.Offset(PPoint(Params^[1])^);
end;

(*
T2DPointArray.Offset
~~~~~~~~~~~~~~~~~~~~
> function T2DPointArray.Offset(X, Y: Integer): T2DPointArray;
*)
procedure _LapeATPA_Offset2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.Offset(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
T2DPointArray.Sort
~~~~~~~~~~~~~~~~~~
> function T2DPointArray.Sort(Weights: TIntegerArray; LowToHigh: Boolean = True): T2DPointArray;
*)
procedure _LapeATPA_Sort1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.Sort(PIntegerArray(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
T2DPointArray.Sort
~~~~~~~~~~~~~~~~~~
> function T2DPointArray.Sort(Weights: TDoubleArray; LowToHigh: Boolean = True): T2DPointArray;
*)
procedure _LapeATPA_Sort2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.Sort(PDoubleArray(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
T2DPointArray.SortFromSize
~~~~~~~~~~~~~~~~~~~~~~~~~~
> function T2DPointArray.SortFromSize(Size: Integer): T2DPointArray;
*)
procedure _LapeATPA_SortFromSize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.SortFromSize(PInteger(Params^[1])^);
end;

(*
T2DPointArray.SortFromIndex
~~~~~~~~~~~~~~~~~~~~~~~~~~~
> function T2DPointArray.SortFromIndex(From: TPoint; Index: Integer = 0): T2DPointArray;
*)
procedure _LapeATPA_SortFromIndex(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.SortFromIndex(PPoint(Params^[1])^, PInteger(Params^[2])^);
end;

(*
T2DPointArray.SortFromFirstPoint
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> function T2DPointArray.SortFromFirstPoint(From: TPoint): T2DPointArray;
*)
procedure _LapeATPA_SortFromFirstPoint(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.SortFromFirstPoint(PPoint(Params^[1])^);
end;

(*
T2DPointArray.SortFromFirstPointX
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> function T2DPointArray.SortFromFirstPointX(From: TPoint): T2DPointArray;
*)
procedure _LapeATPA_SortFromFirstPointX(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.SortFromFirstPointX(PPoint(Params^[1])^);
end;

(*
T2DPointArray.SortFromFirstPointY
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> function T2DPointArray.SortFromFirstPointY(From: TPoint): T2DPointArray;
*)
procedure _LapeATPA_SortFromFirstPointY(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.SortFromFirstPointY(PPoint(Params^[1])^);
end;

(*
T2DPointArray.SortFrom
~~~~~~~~~~~~~~~~~~~~~~
> function T2DPointArray.SortFrom(From: TPoint): T2DPointArray;
*)
procedure _LapeATPA_SortFrom(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.SortFrom(PPoint(Params^[1])^);
end;

(*
T2DPointArray.SortByArea
~~~~~~~~~~~~~~~~~~~~~~~~
> function T2DPointArray.SortByArea(LowToHigh: Boolean): T2DPointArray;
*)
procedure _LapeATPA_SortByArea(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.SortByArea(PBoolean(Params^[1])^);
end;

(*
T2DPointArray.SortBySize
~~~~~~~~~~~~~~~~~~~~~~~~
> function T2DPointArray.SortBySize(LowToHigh: Boolean): T2DPointArray;
*)
procedure _LapeATPA_SortBySize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.SortBySize(PBoolean(Params^[1])^);
end;

(*
T2DPointArray.SortByDensity
~~~~~~~~~~~~~~~~~~~~~~~~~~~
> function T2DPointArray.SortByDensity(LowToHigh: Boolean): T2DPointArray;
*)
procedure _LapeATPA_SortByDensity(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.SortByDensity(PBoolean(Params^[1])^);
end;

(*
T2DPointArray.SortByX
~~~~~~~~~~~~~~~~~~~~~
> function T2DPointArray.SortByX(LowToHigh: Boolean): T2DPointArray;
*)
procedure _LapeATPA_SortByX(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.SortByX(PBoolean(Params^[1])^);
end;

(*
T2DPointArray.SortByY
~~~~~~~~~~~~~~~~~~~~~
> function T2DPointArray.SortByY(LowToHigh: Boolean): T2DPointArray;
*)
procedure _LapeATPA_SortByY(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.SortByY(PBoolean(Params^[1])^);
end;

(*
T2DPointArray.SortByShortSide
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> function T2DPointArray.SortByShortSide(LowToHigh: Boolean): T2DPointArray;
*)
procedure _LapeATPA_SortByShortSide(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.SortByShortSide(PBoolean(Params^[1])^);
end;

(*
T2DPointArray.SortByLongSide
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> function T2DPointArray.SortByLongSide(LowToHigh: Boolean): T2DPointArray;
*)
procedure _LapeATPA_SortByLongSide(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.SortByLongSide(PBoolean(Params^[1])^);
end;

(*
T2DPointArray.ExcludeSize
~~~~~~~~~~~~~~~~~~~~~~~~~
> function T2DPointArray.ExcludeSize(Len: Integer; KeepIf: EComparator): T2DPointArray;
*)
procedure _LapeATPA_ExcludeSize1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.ExcludeSize(PInteger(Params^[1])^, PComparator(Params^[2])^);
end;

(*
T2DPointArray.ExcludeSize
~~~~~~~~~~~~~~~~~~~~~~~~~
> function T2DPointArray.ExcludeSize(MinLen, MaxLen: Integer): T2DPointArray;
*)
procedure _LapeATPA_ExcludeSize2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.ExcludeSize(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
T2DPointArray.ExcludeSizeEx
~~~~~~~~~~~~~~~~~~~~~~~~~~~
> function T2DPointArray.ExcludeSizeEx(MaxLen: Integer): T2DPointArray;
*)
procedure _LapeATPA_ExcludeSizeEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.ExcludeSizeEx(PInteger(Params^[1])^);
end;

(*
T2DPointArray.ExcludeDimensions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> function T2DPointArray.ExcludeDimensions(MinShortSide, MinLongSide, MaxShortSide, MaxLongSide: Integer): T2DPointArray;
*)
procedure _LapeATPA_ExcludeDimensions(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.ExcludeDimensions(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

(*
T2DPointArray.ExcludeDimensionsEx
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> function T2DPointArray.ExcludeDimensionsEx(MaxShortSide, MaxLongSide: Integer): T2DPointArray;
*)
procedure _LapeATPA_ExcludeDimensionsEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  P2DPointArray(Result)^ := P2DPointArray(Params^[0])^.ExcludeDimensionsEx(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
T2DPointArray.Smallest
~~~~~~~~~~~~~~~~~~~~~~
> function T2DPointArray.Smallest: TPointArray;
*)
procedure _LapeATPA_Smallest(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := P2DPointArray(Params^[0])^.Smallest();
end;

(*
T2DPointArray.Largest
~~~~~~~~~~~~~~~~~~~~~
> function T2DPointArray.Largest: TPointArray;
*)
procedure _LapeATPA_Largest(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := P2DPointArray(Params^[0])^.Largest();
end;

(*
T2DPointArray.Bounds
~~~~~~~~~~~~~~~~~~~~
> function T2DPointArray.Bounds: TBox;
*)
procedure _LapeATPA_Bounds(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBox(Result)^ := P2DPointArray(Params^[0])^.Bounds();
end;

(*
T2DPointArray.BoundsArray
~~~~~~~~~~~~~~~~~~~~~~~~~
> function T2DPointArray.BoundsArray: TBoxArray;
*)
procedure _LapeATPA_BoundsArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoxArray(Result)^ := P2DPointArray(Params^[0])^.BoundsArray();
end;

(*
T2DPointArray.Mean
~~~~~~~~~~~~~~~~~~
> function T2DPointArray.Mean: TPoint;
*)
procedure _LapeATPA_Mean(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := P2DPointArray(Params^[0])^.Mean();
end;

(*
T2DPointArray.Means
~~~~~~~~~~~~~~~~~~~
> function T2DPointArray.Means: TPointArray;
*)
procedure _LapeATPA_Means(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := P2DPointArray(Params^[0])^.Means();
end;

(*
T2DPointArray.Merge
~~~~~~~~~~~~~~~~~~~
> function T2DPointArray.Merge: TPointArray;
*)
procedure _LapeATPA_Merge(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := P2DPointArray(Params^[0])^.Merge();
end;

(*
T2DPointArray.Intersection
~~~~~~~~~~~~~~~~~~~~~~~~~~
> function T2DPointArray.Intersection: TPointArray;

Returns the points which exist in all arrays.
*)
procedure _LapeATPA_Intersection(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := P2DPointArray(Params^[0])^.Intersection();
end;

procedure ImportATPA(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'T2DPointArray';

    addGlobalFunc('function T2DPointArray.Offset(P: TPoint): T2DPointArray; overload', @_LapeATPA_Offset1);
    addGlobalFunc('function T2DPointArray.Offset(X, Y: Integer): T2DPointArray; overload', @_LapeATPA_Offset2);

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

    addGlobalFunc('function T2DPointArray.ExcludeSize(Len: Integer; KeepIf: EComparator): T2DPointArray; overload', @_LapeATPA_ExcludeSize1);
    addGlobalFunc('function T2DPointArray.ExcludeSize(MinLen, MaxLen: Integer): T2DPointArray; overload', @_LapeATPA_ExcludeSize2);
    addGlobalFunc('function T2DPointArray.ExcludeSizeEx(MaxLen: Integer): T2DPointArray', @_LapeATPA_ExcludeSizeEx);
    addGlobalFunc('function T2DPointArray.ExcludeDimensions(MinShortSide, MinLongSide, MaxShortSide, MaxLongSide: Integer): T2DPointArray', @_LapeATPA_ExcludeDimensions);
    addGlobalFunc('function T2DPointArray.ExcludeDimensionsEx(MinShortSide, MinLongSide: Integer): T2DPointArray', @_LapeATPA_ExcludeDimensionsEx);

    addGlobalFunc('function T2DPointArray.Smallest: TPointArray', @_LapeATPA_Smallest);
    addGlobalFunc('function T2DPointArray.Largest: TPointArray', @_LapeATPA_Largest);

    addGlobalFunc('function T2DPointArray.Bounds: TBox', @_LapeATPA_Bounds);
    addGlobalFunc('function T2DPointArray.BoundsArray: TBoxArray', @_LapeATPA_BoundsArray);

    addGlobalFunc('function T2DPointArray.Mean: TPoint; overload', @_LapeATPA_Mean);
    addGlobalFunc('function T2DPointArray.Means: TPointArray', @_LapeATPA_Means);
    addGlobalFunc('function T2DPointArray.Merge: TPointArray;', @_LapeATPA_Merge);
    addGlobalFunc('function T2DPointArray.Intersection: TPointArray', @_LapeATPA_Intersection);

    ImportingSection := '';
  end;
end;

end.