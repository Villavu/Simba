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

// Unique
procedure _LapeUnique_PointArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := Unique(PPointArray(Params^[0])^);
end;

procedure _LapeUnique_IntegerArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := Unique(PIntegerArray(Params^[0])^);
end;

procedure _LapeUnique_DoubleArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDoubleArray(Result)^ := Unique(PDoubleArray(Params^[0])^);
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

procedure _LapeIndicesOf_DoubleArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := IndicesOf(PDouble(Params^[0])^, PDoubleArray(Params^[1])^);
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

procedure _LapeIndexOf_DoubleArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := IndexOf(PDouble(Params^[0])^, PDoubleArray(Params^[1])^);
end;
