procedure _LapeClient_IOManager_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Result)^ := PClient(Params^[0])^.IOManager;
end;

procedure _LapeClient_IOManager_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PClient(Params^[0])^.IOManager := PIOManager(Params^[1])^;
end;

procedure _LapeClient_MFiles_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFiles(Result)^ := PClient(Params^[0])^.MFiles;
end;

procedure _LapeClient_MFiles_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PClient(Params^[0])^.MFiles := PMFiles(Params^[1])^;
end;

procedure _LapeClient_MFinder_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFinder(Result)^ := PClient(Params^[0])^.MFinder;
end;

procedure _LapeClient_MFinder_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PClient(Params^[0])^.MFinder := PMFinder(Params^[1])^;
end;

procedure _LapeClient_MDTMs_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTMS(Result)^ := PClient(Params^[0])^.MDTMs;
end;

procedure _LapeClient_MDTMs_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PClient(Params^[0])^.MDTMs := PMDTMS(Params^[1])^;
end;

procedure _LapeClient_MOCR_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMOCR(Result)^ := PClient(Params^[0])^.MOCR;
end;

procedure _LapeClient_MOCR_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PClient(Params^[0])^.MOCR := PMOCR(Params^[1])^;
end;

procedure _LapeClient_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PClient(Params^[0])^ := TClient.Create(PString(Params^[1])^, PIOManager(Params^[2])^);
end;

procedure _LapeClient_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PClient(Params^[0])^.Free();
end;

