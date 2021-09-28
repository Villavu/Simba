procedure _LapeMDTMS_AddDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMDTMS(Params^[0])^.AddDTM(PSDTM(Params^[1])^);
end;

procedure _LapeMDTMS_AddDTMEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMDTMS(Params^[0])^.AddDTM(PMDTM(Params^[1])^);
end;

procedure _LapeMDTMS_ExistsDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMDTMS(Params^[0])^.ExistsDTM(Pinteger(Params^[1])^);
end;

procedure _LapeMDTMS_GetDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTM(Result)^ := PMDTMS(Params^[0])^.GetDTM(PInteger(Params^[1])^);
end;

procedure _LapeMDTMS_FreeDTM(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTMS(Params^[0])^.FreeDTM(PInteger(Params^[1])^);
end;

procedure _LapeMDTMS_StringToDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMDTMS(Params^[0])^.StringToDTM(PlpString(Params^[1])^);
end;

procedure _LapeMDTMS_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTMS(Params^[0])^ := TMDTMS.Create(PObject(Params^[1])^);
end;

procedure _LapeMDTMS_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTMS(Params^[0])^.Free();
end;

