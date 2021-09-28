procedure _LapeRunCommandInDir(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcessID(Result)^ := SimbaProcess.RunCommandInDir(PString(Params^[0])^, PString(Params^[1])^, PStringArray(Params^[2])^);
end;

procedure _LapeRunCommandInDirOutput(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcessExitStatus(Result)^ := SimbaProcess.RunCommandInDir(PString(Params^[0])^, PString(Params^[1])^, PStringArray(Params^[2])^, PString(Params^[3])^);
end;

procedure _LapeRunCommand(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcessID(Result)^ := SimbaProcess.RunCommand(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

procedure _LapeRunCommandOutput(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcessExitStatus(Result)^ := SimbaProcess.RunCommand(PString(Params^[0])^, PStringArray(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeRunCommandTimeout(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaProcess.RunCommandTimeout(PString(Params^[0])^, PStringArray(Params^[1])^, PString(Params^[2])^, PInt32(Params^[3])^);
end;

procedure _LapeGetScriptParameters(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringArray(Result)^ := SimbaProcess.GetScriptParameters();
end;

procedure _LapeGetScriptParameter(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaProcess.GetScriptParameter(PString(Params^[0])^);
end;

procedure _LapeIsProcessRunning(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaProcess.IsProcessRunning(PProcessID(Params^[0])^);
end;

procedure _LapeIsProcess64Bit(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaProcess.IsProcess64Bit(PProcessID(Params^[0])^);
end;

procedure _LapeGetProcessPath(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaProcess.GetProcessPath(PProcessID(Params^[0])^);
end;

procedure _LapeTerminateProcess(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaProcess.TerminateProcess(PProcessID(Params^[0])^);
end;

procedure _LapeRunScript(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcessExitStatus(Result)^ := SimbaProcess.RunScript(PString(Params^[0])^, PStringArray(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeRunScriptEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcessID(Result)^ := SimbaProcess.RunScript(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

procedure _LapeRunScriptOutputToFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcessID(Result)^ := SimbaProcess.RunScriptOutputToFile(PString(Params^[0])^, PStringArray(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeGetScriptPID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcessID(Result)^ := GetProcessID();
end;

