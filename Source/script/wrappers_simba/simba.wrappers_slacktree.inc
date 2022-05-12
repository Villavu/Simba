procedure _LapeSlackTreeInit(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSlackTree(Params^[0])^.Init(PPointArray(Params^[1])^);
end;

procedure _LapeSlackTreeIndexOf(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PSlackTree(Params^[0])^.IndexOf(PPoint(Params^[1])^);
end;

procedure _LapeSlackTreeFind(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Result)^ := PSlackTree(Params^[0])^.Find(PPoint(Params^[1])^);
end;

procedure _LapeSlackTreeHideNode(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSlackTree(Params^[0])^.HideNode(PInteger(Params^[1])^);
end;

procedure _LapeSlackTreeHideNode2(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PSlackTree(Params^[0])^.HideNode(PPoint(Params^[1])^);
end;

procedure _LapeSlackTreeRawNearest(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Result)^ := PSlackTree(Params^[0])^.RawNearest(PPoint(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeSlackTreeNearest(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PSlackTree(Params^[0])^.Nearest(PPoint(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeSlackTreeRawKNearest(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TNodeRefArray(Result^) := PSlackTree(Params^[0])^.RawKNearest(PPoint(Params^[1])^, PInteger(Params^[2])^, PBoolean(Params^[3])^);
end;

procedure _LapeSlackTreeKNearest(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := PSlackTree(Params^[0])^.KNearest(PPoint(Params^[1])^, PInteger(Params^[2])^, PBoolean(Params^[3])^);
end;

procedure _LapeSlackTreeRawRangeQuery(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TNodeRefArray(Result^) := PSlackTree(Params^[0])^.RawRangeQuery(PBox(Params^[1])^);
end;

procedure _LapeSlackTreeRangeQuery(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := PSlackTree(Params^[0])^.RangeQuery(PBox(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeSlackTreeRangeQueryEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := PSlackTree(Params^[0])^.RangeQueryEx(PPoint(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^, PBoolean(Params^[4])^);
end;

procedure _LapeSlackTreeRangeQueryEx2(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := PSlackTree(Params^[0])^.RangeQueryEx(PPoint(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^, PDouble(Params^[4])^, PDouble(Params^[5])^, PBoolean(Params^[6])^);
end;

procedure _LapeSlackTreeRefArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TNodeRefArray(Result^) := PSlackTree(Params^[0])^.RefArray;
end;
