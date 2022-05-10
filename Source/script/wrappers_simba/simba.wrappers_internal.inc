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
  Sort(PIntegerArray(Params^[0])^);
end;

procedure _LapeSort_SingleArray(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Sort(PSingleArray(Params^[0])^);
end;

procedure _LapeSort_DoubleArray(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Sort(PDoubleArray(Params^[0])^);
end;

procedure _LapeSort_ExtendedArray(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Sort(PExtendedArray(Params^[0])^);
end;

// Unique
procedure _LapeUnique_PointArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := Unique(PPointArray(Params^[0])^);
end;

procedure _LapeUnique_IntegerArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := Unique(PIntegerArray(Params^[0])^);
end;

procedure _LapeUnique_SingleArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleArray(Result)^ := Unique(PSingleArray(Params^[0])^);
end;

procedure _LapeUnique_DoubleArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDoubleArray(Result)^ := Unique(PDoubleArray(Params^[0])^);
end;

procedure _LapeUnique_ExtendedArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtendedArray(Result)^ := Unique(PExtendedArray(Params^[0])^);
end;

procedure _LapeUnique_StringArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringArray(Result)^ := Unique(PStringArray(Params^[0])^);
end;

// IndicesOf
procedure _LapeIndicesOf_PointArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := IndicesOf(PPoint(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure _LapeIndicesOf_IntegerArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := IndicesOf(PInteger(Params^[0])^, PIntegerArray(Params^[1])^);
end;

procedure _LapeIndicesOf_StringArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := IndicesOf(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

procedure _LapeIndicesOf_SingleArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := IndicesOf(PSingle(Params^[0])^, PSingleArray(Params^[1])^);
end;

procedure _LapeIndicesOf_DoubleArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := IndicesOf(PDouble(Params^[0])^, PDoubleArray(Params^[1])^);
end;

procedure _LapeIndicesOf_ExtendedArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := IndicesOf(PExtended(Params^[0])^, PExtendedArray(Params^[1])^);
end;

// IndexOf
procedure _LapeIndexOf_PointArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := IndexOf(PPoint(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure _LapeIndexOf_IntegerArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := IndexOf(PInteger(Params^[0])^, PIntegerArray(Params^[1])^);
end;

procedure _LapeIndexOf_StringArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := IndexOf(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

procedure _LapeIndexOf_SingleArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := IndexOf(PSingle(Params^[0])^, PSingleArray(Params^[1])^);
end;

procedure _LapeIndexOf_DoubleArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := IndexOf(PDouble(Params^[0])^, PDoubleArray(Params^[1])^);
end;

procedure _LapeIndexOf_ExtendedArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := IndexOf(PExtended(Params^[0])^, PExtendedArray(Params^[1])^);
end;

// Sum
procedure _LapeArraySum_PointArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := Sum(PPointArray(Params^[0])^);
end;

procedure _LapeArraySum_IntegerArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := Sum(PIntegerArray(Params^[0])^);
end;

procedure _LapeArraySum_SingleArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Sum(PSingleArray(Params^[0])^);
end;

procedure _LapeArraySum_DoubleArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Sum(PDoubleArray(Params^[0])^);
end;

procedure _LapeArraySum_ExtendedArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Sum(PExtendedArray(Params^[0])^);
end;

// Min
procedure _LapeArrayMin_IntegerArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := Min(PIntegerArray(Params^[0])^);
end;

procedure _LapeArrayMin_SingleArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingle(Result)^ := Min(PSingleArray(Params^[0])^);
end;

procedure _LapeArrayMin_DoubleArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDouble(Result)^ := Min(PDoubleArray(Params^[0])^);
end;

procedure _LapeArrayMin_ExtendedArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Min(PExtendedArray(Params^[0])^);
end;

// Max
procedure _LapeArrayMax_IntegerArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := Max(PIntegerArray(Params^[0])^);
end;

procedure _LapeArrayMax_SingleArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingle(Result)^ := Max(PSingleArray(Params^[0])^);
end;

procedure _LapeArrayMax_DoubleArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDouble(Result)^ := Max(PDoubleArray(Params^[0])^);
end;

procedure _LapeArrayMax_ExtendedArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Max(PExtendedArray(Params^[0])^);
end;



