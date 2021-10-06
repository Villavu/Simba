procedure _LapeWrite(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Write(PString(Params^[0])^);
end;

procedure _LapeWriteLn(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  WriteLn('');

  Flush(Output);
end;

procedure _LapeGetEnvironmentVariable(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := GetEnvironmentVariable(PString(Params^[0])^);
end;

procedure _LapeGetCurrentThreadID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPtrUInt(Result)^ := PtrUInt(GetCurrentThreadID());
end;

procedure _LapeGetMainThreadID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPtrUInt(Result)^ := PtrUInt(MainThreadID);
end;

procedure _LapeGetProcessorCount(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPtrUInt(Result)^ := TThread.ProcessorCount;
end;

procedure _LapeWait(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Sleep(PUInt32(Params^[0])^);
end;

type
  TSync = object
    Params: PParamArray;

    procedure Execute;
  end;

procedure TSync.Execute;
type
  TSyncProcedure = procedure of object; {$IF DEFINED(CPU32) and DEFINED(LAPE_CDECL)}cdecl;{$ENDIF}
begin
  TSyncProcedure(Params^[0]^)();
end;

procedure _LapeSync(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Sync: TSync;
begin
  Sync := Default(TSync);
  Sync.Params := Params;

  TThread.Synchronize(nil, @Sync.Execute);
end;

procedure _LapePauseScript(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.State := bUnknown;
end;

procedure _LapeTerminateScript(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.State := bFalse;
end;

procedure _LapeIsTerminated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaScript.Terminated;
end;

procedure _LapeIsUserTerminated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaScript.Terminated and SimbaScript.UserTerminated;
end;

