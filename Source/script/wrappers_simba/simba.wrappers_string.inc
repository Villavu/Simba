procedure _LapeCompressString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pstring(Result)^ := CompressString(Pstring(Params^[0])^);
end;

procedure _LapeDecompressString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pstring(Result)^ := DecompressString(Pstring(Params^[0])^);
end;

procedure _LapeBase64Encode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pstring(Result)^ := Base64Encode(Pstring(Params^[0])^);
end;

procedure _LapeBase64Decode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pstring(Result)^ := Base64Decode(Pstring(Params^[0])^);
end;

procedure _LapeExtractFromStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pstring(Result)^ := ExtractFromStr(Pstring(Params^[0])^, PStrExtr(Params^[1])^);
end;

procedure _LapeBetween(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pstring(Result)^ := Between(Pstring(Params^[0])^, Pstring(Params^[1])^, Pstring(Params^[2])^);
end;

procedure _LapeImplode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pstring(Result)^ := Implode(Pstring(Params^[0])^, PStringArray(Params^[1])^);
end;

procedure _LapeExplode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringArray(Result)^ := Explode(Pstring(Params^[0])^, Pstring(Params^[1])^);
end;

procedure _LapeExecRegExpr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := ExecRegExpr(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeSplitRegExpr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  List: TStringList;
begin
  List := TStringList.Create();
  try
    SplitRegExpr(PString(Params^[0])^, PString(Params^[1])^, List);

    PStringArray(Result)^ := List.ToStringArray();
  finally
    List.Free();
  end;
end;

procedure _LapeReplaceRegExpr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := ReplaceRegExpr(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^, Pboolean(Params^[3])^);
end;

procedure _LapePosEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PosEx(PString(Params^[0])^, PString(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeLevDist(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := LevDistance(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeNormLevDist(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := NormLevDistance(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeStringMatch(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := StringMatch(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeMultiBetween(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringArray(Result)^ := MultiBetween(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeIsArrInStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := IsArrInStr(PStringArray(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapePosMulti(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := PosMulti(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeIsStrInArr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := IsStrInArr(PString(Params^[0])^, PBoolean(Params^[1])^, PStringArray(Params^[2])^);
end;

procedure _LapeUTF8Decode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWideString(Result)^ := UTF8Decode(PString(Params^[0])^);
end;

procedure _LapeUTF8Encode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PAnsiString(Result)^ := UTF8Encode(PWideString(Params^[0])^);
end;

