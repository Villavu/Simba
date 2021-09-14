procedure _LapeSetDTMName(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.MDTMs[PInt32(Params^[0])^].Name := PString(Params^[0])^;
end;

procedure _LapeDTMFromString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := SimbaScript.Client.MDTMs.StringToDTM(PString(Params^[0])^);
end;

procedure _LapeFreeDTM(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.MDTMs.FreeDTM(PInt32(Params^[0])^);
end;

procedure _LapeGetDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTM(Result)^ := SimbaScript.Client.MDTMs.GetDTM(PInt32(Params^[0])^);
end;

procedure _LapeAddTSDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := SimbaScript.Client.MDTMs.AddDTM(PSDTM(Params^[0])^);
end;

procedure _LapeAddDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := SimbaScript.Client.MDTMs.AddDTM(PMDTM(Params^[0])^);
end;

procedure _LapeMDTMToSDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSDTM(Result)^ := MDTMToSDTM(PMDTM(Params^[0])^);
end;

procedure _LapeSDTMToMDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTM(Result)^ := SDTMToMDTM(PSDTM(Params^[0])^);
end;

procedure _LapeCreateDTMPoint(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTMPoint(Result)^ := CreateDTMPoint(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, Pboolean(Params^[5])^);
end;

procedure _LapeDTMExists(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PBoolean(Result)^ := MDTMs.ExistsDTM(PInt32(Params^[0])^);
end;
