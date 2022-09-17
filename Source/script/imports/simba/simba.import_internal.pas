unit simba.import_internal;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lazloggerbase, lptypes,
  simba.script_compiler, simba.mufasatypes, simba.tpa, simba.algo_sort, simba.algo_unique;

procedure _LapeWrite(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  DbgOut(PString(Params^[0])^);
end;

procedure _LapeWriteLn(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  DebugLn();
end;

// Sort
procedure _LapeSort_IntegerArray(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  specialize Sort<Integer>(PIntegerArray(Params^[0])^);
end;

procedure _LapeSort_SingleArray(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  specialize Sort<Single>(PSingleArray(Params^[0])^);
end;

procedure _LapeSort_DoubleArray(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  specialize Sort<Double>(PDoubleArray(Params^[0])^);
end;

// Unique
procedure _LapeUnique_PointArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := UniqueTPA(PPointArray(Params^[0])^);
end;

procedure _LapeUnique_IntegerArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := Unique(PIntegerArray(Params^[0])^);
end;

procedure _LapeUnique_StringArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringArray(Result)^ := Unique(PStringArray(Params^[0])^);
end;

procedure _LapeUnique_SingleArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleArray(Result)^ := specialize Unique_SameValue<Single>(PSingleArray(Params^[0])^);
end;

procedure _LapeUnique_DoubleArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDoubleArray(Result)^ := specialize Unique_SameValue<Double>(PDoubleArray(Params^[0])^);
end;

// IndicesOf
procedure _LapeIndicesOf_PointArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := specialize IndicesOf<TPoint>(PPoint(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure _LapeIndicesOf_IntegerArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := specialize IndicesOf<Integer>(PInteger(Params^[0])^, PIntegerArray(Params^[1])^);
end;

procedure _LapeIndicesOf_StringArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := specialize IndicesOf<String>(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

procedure _LapeIndicesOf_SingleArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := specialize IndicesOf_SameValue<Single>(PSingle(Params^[0])^, PSingleArray(Params^[1])^);
end;

procedure _LapeIndicesOf_DoubleArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := specialize IndicesOf_SameValue<Double>(PDouble(Params^[0])^, PDoubleArray(Params^[1])^);
end;

// IndexOf
procedure _LapeIndexOf_PointArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := specialize IndexOf<TPoint>(PPoint(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure _LapeIndexOf_IntegerArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := specialize IndexOf<Integer>(PInteger(Params^[0])^, PIntegerArray(Params^[1])^);
end;

procedure _LapeIndexOf_StringArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := specialize IndexOf<String>(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

procedure _LapeIndexOf_SingleArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := specialize IndexOf_SameValue<Single>(PSingle(Params^[0])^, PSingleArray(Params^[1])^);
end;

procedure _LapeIndexOf_DoubleArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := specialize IndexOf_SameValue<Double>(PDouble(Params^[0])^, PDoubleArray(Params^[1])^);
end;

// Sum
procedure _LapeArraySum_PointArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := specialize Sum<TPoint, TPoint>(PPointArray(Params^[0])^);
end;

procedure _LapeArraySum_IntegerArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := specialize Sum<Integer, Int64>(PIntegerArray(Params^[0])^);
end;

procedure _LapeArraySum_SingleArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := specialize Sum<Single, Double>(PSingleArray(Params^[0])^);
end;

procedure _LapeArraySum_DoubleArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := specialize Sum<Double, Double>(PDoubleArray(Params^[0])^);
end;

// Min
procedure _LapeArrayMin_IntegerArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := specialize MinA<Integer>(PIntegerArray(Params^[0])^);
end;

procedure _LapeArrayMin_SingleArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingle(Result)^ := specialize MinA<Single>(PSingleArray(Params^[0])^);
end;

procedure _LapeArrayMin_DoubleArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDouble(Result)^ := specialize MinA<Double>(PDoubleArray(Params^[0])^);
end;

// Max
procedure _LapeArrayMax_IntegerArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := specialize MaxA<Integer>(PIntegerArray(Params^[0])^);
end;

procedure _LapeArrayMax_SingleArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingle(Result)^ := specialize MaxA<Single>(PSingleArray(Params^[0])^);
end;

procedure _LapeArrayMax_DoubleArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDouble(Result)^ := specialize MaxA<Double>(PDoubleArray(Params^[0])^);
end;

procedure ImportInternal(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection(''); // Hidden

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

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportInternal);

end.

