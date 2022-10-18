unit simba.import_internal;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lazloggerbase, lptypes,
  simba.script_compiler, simba.mufasatypes, simba.tpa, simba.algo_sort, simba.algo_unique,
    simba.algo_difference, simba.algo_intersection, simba.algo_symmetricDifference;

procedure _LapeWrite(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  DbgOut(PString(Params^[0])^);
end;

procedure _LapeWriteLn(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  DebugLn();
end;

// Sort
procedure _LapeSort_IntegerArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  specialize Sort<Integer>(PIntegerArray(Params^[0])^);
end;

procedure _LapeSort_SingleArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  specialize Sort<Single>(PSingleArray(Params^[0])^);
end;

procedure _LapeSort_DoubleArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  specialize Sort<Double>(PDoubleArray(Params^[0])^);
end;

// Unique
procedure _LapeUnique_PointArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPointArray(Params^[0])^.Unique();
end;

procedure _LapeUnique_IntegerArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerArray(Result)^ := Algo_Unique_Integer(PIntegerArray(Params^[0])^);
end;

procedure _LapeUnique_StringArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := Algo_Unique_String(PStringArray(Params^[0])^);
end;

procedure _LapeUnique_SingleArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleArray(Result)^ := Algo_Unique_Single(PSingleArray(Params^[0])^);
end;

procedure _LapeUnique_DoubleArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDoubleArray(Result)^ := Algo_Unique_Double(PDoubleArray(Params^[0])^);
end;

// IndicesOf
procedure _LapeIndicesOf_PointArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerArray(Result)^ := specialize IndicesOf<TPoint>(PPoint(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure _LapeIndicesOf_IntegerArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerArray(Result)^ := specialize IndicesOf<Integer>(PInteger(Params^[0])^, PIntegerArray(Params^[1])^);
end;

procedure _LapeIndicesOf_StringArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerArray(Result)^ := specialize IndicesOf<String>(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

procedure _LapeIndicesOf_SingleArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerArray(Result)^ := specialize IndicesOf_SameValue<Single>(PSingle(Params^[0])^, PSingleArray(Params^[1])^);
end;

procedure _LapeIndicesOf_DoubleArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerArray(Result)^ := specialize IndicesOf_SameValue<Double>(PDouble(Params^[0])^, PDoubleArray(Params^[1])^);
end;

// IndexOf
procedure _LapeIndexOf_PointArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := specialize IndexOf<TPoint>(PPoint(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure _LapeIndexOf_IntegerArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := specialize IndexOf<Integer>(PInteger(Params^[0])^, PIntegerArray(Params^[1])^);
end;

procedure _LapeIndexOf_StringArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := specialize IndexOf<String>(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

procedure _LapeIndexOf_SingleArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := specialize IndexOf_SameValue<Single>(PSingle(Params^[0])^, PSingleArray(Params^[1])^);
end;

procedure _LapeIndexOf_DoubleArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := specialize IndexOf_SameValue<Double>(PDouble(Params^[0])^, PDoubleArray(Params^[1])^);
end;

// Sum
procedure _LapeArraySum_PointArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := specialize Sum<TPoint, TPoint>(PPointArray(Params^[0])^);
end;

procedure _LapeArraySum_IntegerArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := specialize Sum<Integer, Int64>(PIntegerArray(Params^[0])^);
end;

procedure _LapeArraySum_SingleArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PExtended(Result)^ := specialize Sum<Single, Double>(PSingleArray(Params^[0])^);
end;

procedure _LapeArraySum_DoubleArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PExtended(Result)^ := specialize Sum<Double, Double>(PDoubleArray(Params^[0])^);
end;

// Min
procedure _LapeArrayMin_IntegerArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := specialize MinA<Integer>(PIntegerArray(Params^[0])^);
end;

procedure _LapeArrayMin_SingleArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := specialize MinA<Single>(PSingleArray(Params^[0])^);
end;

procedure _LapeArrayMin_DoubleArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := specialize MinA<Double>(PDoubleArray(Params^[0])^);
end;

// Max
procedure _LapeArrayMax_IntegerArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := specialize MaxA<Integer>(PIntegerArray(Params^[0])^);
end;

procedure _LapeArrayMax_SingleArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := specialize MaxA<Single>(PSingleArray(Params^[0])^);
end;

procedure _LapeArrayMax_DoubleArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := specialize MaxA<Double>(PDoubleArray(Params^[0])^);
end;

(*
TByteArray.Intersection
~~~~~~~~~~~~~~~~~~~~~~~
function TByteArray.Intersection(Other: TByteArray): TByteArray;
*)
procedure _Lape_UInt8_Intersection(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PByteArray(Result)^ := Algo_UInt8_Intersection(PByteArray(Params^[0])^, PByteArray(Params^[1])^)
end;

(*
TIntegerArray.Intersection
~~~~~~~~~~~~~~~~~~~~~~~~~~
function TIntegerArray.Intersection(Other: TIntegerArray): TIntegerArray;
*)
procedure _Lape_Int32_Intersection(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerArray(Result)^ := Algo_Int32_Intersection(PIntegerArray(Params^[0])^, PIntegerArray(Params^[1])^)
end;

(*
TInt64Array.Intersection
~~~~~~~~~~~~~~~~~~~~~~~~
function TInt64Array.Intersection(Other: TInt64Array): TInt64Array;
*)
procedure _Lape_Int64_Intersection(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64Array(Result)^ := Algo_Int64_Intersection(PInt64Array(Params^[0])^, PInt64Array(Params^[1])^)
end;

(*
TByteArray.SymmetricDifference
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TByteArray.SymmetricDifference(Other: TByteArray): TByteArray;
*)
procedure _Lape_UInt8_SymmetricDifference(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PByteArray(Result)^ := Algo_UInt8_SymmetricDifference(PByteArray(Params^[0])^, PByteArray(Params^[1])^)
end;

(*
TIntegerArray.SymmetricDifference
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TIntegerArray.SymmetricDifference(Other: TIntegerArray): TIntegerArray;
*)
procedure _Lape_Int32_SymmetricDifference(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerArray(Result)^ := Algo_Int32_SymmetricDifference(PIntegerArray(Params^[0])^, PIntegerArray(Params^[1])^)
end;

(*
TInt64Array.SymmetricDifference
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TInt64Array.SymmetricDifference(Other: TInt64Array): TInt64Array;
*)
procedure _Lape_Int64_SymmetricDifference(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64Array(Result)^ := Algo_Int64_SymmetricDifference(PInt64Array(Params^[0])^, PInt64Array(Params^[1])^)
end;

(*
TByteArray.Difference
~~~~~~~~~~~~~~~~~~~~~
function TByteArray.Difference(Other: TByteArray): TByteArray;
*)
procedure _Lape_UInt8_Difference(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PByteArray(Result)^ := Algo_UInt8_Difference(PByteArray(Params^[0])^, PByteArray(Params^[1])^)
end;

(*
TIntegerArray.Difference
~~~~~~~~~~~~~~~~~~~~~~~~
function TIntegerArray.Difference(Other: TIntegerArray): TIntegerArray;
*)
procedure _Lape_Int32_Difference(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerArray(Result)^ := Algo_Int32_Difference(PIntegerArray(Params^[0])^, PIntegerArray(Params^[1])^)
end;

(*
TInt64Array.Difference
~~~~~~~~~~~~~~~~~~~~~~
function TInt64Array.Difference(Other: TInt64Array): TInt64Array;
*)
procedure _Lape_Int64_Difference(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64Array(Result)^ := Algo_Int64_Difference(PInt64Array(Params^[0])^, PInt64Array(Params^[1])^)
end;

procedure ImportInternal(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addGlobalFunc('procedure _Write(S: String); override', @_LapeWrite);
    addGlobalFunc('procedure _WriteLn; override', @_LapeWriteLn);

    addGlobalFunc('procedure _Sort(var a: TIntegerArray); overload;', @_LapeSort_IntegerArray);
    addGlobalFunc('procedure _Sort(var a: TSingleArray); overload;', @_LapeSort_SingleArray);
    addGlobalFunc('procedure _Sort(var a: TDoubleArray); overload;', @_LapeSort_DoubleArray);

    addGlobalFunc('function _Unique(const a: TPointArray): TPointArray; overload', @_LapeUnique_PointArray);
    addGlobalFunc('function _Unique(const a: TIntegerArray): TIntegerArray; overload', @_LapeUnique_IntegerArray);
    addGlobalFunc('function _Unique(const a: TSingleArray): TSingleArray; overload', @_LapeUnique_SingleArray);
    addGlobalFunc('function _Unique(const a: TDoubleArray): TDoubleArray; overload', @_LapeUnique_DoubleArray);
    addGlobalFunc('function _Unique(const a: TStringArray): TStringArray; overload', @_LapeUnique_StringArray);

    addGlobalFunc('function _IndicesOf(const Item: String; const a: TStringArray): TIntegerArray; overload', @_LapeIndicesOf_StringArray);
    addGlobalFunc('function _IndicesOf(const Item: TPoint; const a: TPointArray): TIntegerArray; overload', @_LapeIndicesOf_PointArray);
    addGlobalFunc('function _IndicesOf(const Item: Integer; const a: TIntegerArray): TIntegerArray; overload', @_LapeIndicesOf_IntegerArray);
    addGlobalFunc('function _IndicesOf(const Item: Single; const a: TSingleArray): TIntegerArray; overload', @_LapeIndicesOf_SingleArray);
    addGlobalFunc('function _IndicesOf(const Item: Double; const a: TDoubleArray): TIntegerArray; overload', @_LapeIndicesOf_DoubleArray);

    addGlobalFunc('function _IndexOf(const Item: String; const a: TStringArray): Integer; overload', @_LapeIndexOf_StringArray);
    addGlobalFunc('function _IndexOf(const Item: TPoint; const a: TPointArray): Integer; overload', @_LapeIndexOf_PointArray);
    addGlobalFunc('function _IndexOf(const Item: Integer; const a: TIntegerArray): Integer; overload', @_LapeIndexOf_IntegerArray);
    addGlobalFunc('function _IndexOf(const Item: Single; const a: TSingleArray): Integer; overload', @_LapeIndexOf_SingleArray);
    addGlobalFunc('function _IndexOf(const Item: Double; const a: TDoubleArray): Integer; overload', @_LapeIndexOf_DoubleArray);

    addGlobalFunc('function _ArraySum(const a: TPointArray): TPoint; overload', @_LapeArraySum_PointArray);
    addGlobalFunc('function _ArraySum(const a: TIntegerArray): Int64; overload', @_LapeArraySum_IntegerArray);
    addGlobalFunc('function _ArraySum(const a: TSingleArray): Extended; overload', @_LapeArraySum_SingleArray);
    addGlobalFunc('function _ArraySum(const a: TDoubleArray): Extended; overload', @_LapeArraySum_DoubleArray);

    addGlobalFunc('function _ArrayMin(const a: TIntegerArray): Int64; overload', @_LapeArrayMin_IntegerArray);
    addGlobalFunc('function _ArrayMin(const a: TSingleArray): Single; overload', @_LapeArrayMin_SingleArray);
    addGlobalFunc('function _ArrayMin(const a: TDoubleArray): Double; overload', @_LapeArrayMin_DoubleArray);

    addGlobalFunc('function _ArrayMax(const a: TIntegerArray): Int64; overload', @_LapeArrayMax_IntegerArray);
    addGlobalFunc('function _ArrayMax(const a: TSingleArray): Single; overload', @_LapeArrayMax_SingleArray);
    addGlobalFunc('function _ArrayMax(const a: TDoubleArray): Double; overload', @_LapeArrayMax_DoubleArray);

    addGlobalFunc('function TByteArray.Difference(Other: TByteArray): TByteArray', @_Lape_UInt8_Difference);
    addGlobalFunc('function TIntegerArray.Difference(Other: TIntegerArray): TIntegerArray', @_Lape_Int32_Difference);
    addGlobalFunc('function TInt64Array.Difference(Other: TInt64Array): TInt64Array', @_Lape_Int64_Difference);

    addGlobalFunc('function TByteArray.SymmetricDifference(Other: TByteArray): TByteArray', @_Lape_UInt8_SymmetricDifference);
    addGlobalFunc('function TIntegerArray.SymmetricDifference(Other: TIntegerArray): TIntegerArray', @_Lape_Int32_SymmetricDifference);
    addGlobalFunc('function TInt64Array.SymmetricDifference(Other: TInt64Array): TInt64Array', @_Lape_Int64_SymmetricDifference);

    addGlobalFunc('function TByteArray.Intersection(Other: TByteArray): TByteArray', @_Lape_UInt8_Intersection);
    addGlobalFunc('function TIntegerArray.Intersection(Other: TIntegerArray): TIntegerArray', @_Lape_Int32_Intersection);
    addGlobalFunc('function TInt64Array.Intersection(Other: TInt64Array): TInt64Array', @_Lape_Int64_Intersection);
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportInternal);

end.

