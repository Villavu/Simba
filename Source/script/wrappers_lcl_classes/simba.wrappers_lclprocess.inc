type
  PInputPipeStream = ^TInputPipeStream;
  POutputPipeStream = ^TOutputPipeStream;
  PProcess = ^TProcess;
  PProcessOptions = ^TProcessOptions;
  PProcessPriority = ^TProcessPriority;
  PShowWindowOptions = ^TShowWindowOptions;
  PStartupOptions = ^TStartupOptions;

procedure _LapeOutputPipeStream_Seek(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pint64(Result)^ := POutputPipeStream(Params^[0])^.Seek(Pint64(Params^[1])^, PSeekOrigin(Params^[2])^);
end;

procedure _LapeOutputPipeStream_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Plongint(Result)^ := POutputPipeStream(Params^[0])^.Read(PLongint(Params^[1])^, PLongint(Params^[2])^);
end;

procedure _LapeOutputPipeStream_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  POutputPipeStream(Params^[0])^ := TOutputPipeStream.Create(PHandle(Params^[1])^);
end;

procedure _LapeOutputPipeStream_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  POutputPipeStream(Params^[0])^.Free();
end;

procedure _LapeInputPipeStream_Write(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PLongint(Result)^ := PInputPipeStream(Params^[0])^.Write(PLongint(Params^[1])^, PLongint(Params^[2])^);
end;

procedure _LapeInputPipeStream_Seek(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pint64(Result)^ := PInputPipeStream(Params^[0])^.Seek(Pint64(Params^[1])^, PSeekOrigin(Params^[2])^);
end;

procedure _LapeInputPipeStream_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Plongint(Result)^ := PInputPipeStream(Params^[0])^.Read(PLongint(Params^[1])^, PLongint(Params^[2])^);
end;

procedure _LapeInputPipeStream_NumBytesAvailable_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PUInt32(Result)^ := PInputPipeStream(Params^[0])^.NumBytesAvailable;
end;

procedure _LapeInputPipeStream_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInputPipeStream(Params^[0])^ := TInputPipeStream.Create(PHandle(Params^[1])^);
end;

procedure _LapeInputPipeStream_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInputPipeStream(Params^[0])^.Free();
end;

procedure _LapeProcess_Execute(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.Execute();
end;

procedure _LapeProcess_CloseInput(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.CloseInput();
end;

procedure _LapeProcess_CloseOutput(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.CloseOutput();
end;

procedure _LapeProcess_CloseStderr(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.CloseStderr();
end;

procedure _LapeProcess_Resume(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PProcess(Params^[0])^.Resume();
end;

procedure _LapeProcess_Suspend(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PProcess(Params^[0])^.Suspend();
end;

procedure _LapeProcess_Terminate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PProcess(Params^[0])^.Terminate(PInteger(Params^[1])^);
end;

procedure _LapeProcess_WaitOnExit(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PProcess(Params^[0])^.WaitOnExit();
end;

procedure _LapeProcess_WindowRect_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Prect(Result)^ := PProcess(Params^[0])^.WindowRect;
end;

procedure _LapeProcess_WindowRect_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.WindowRect := Prect(Params^[1])^;
end;

procedure _LapeProcess_Handle_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PHandle(Result)^ := PProcess(Params^[0])^.Handle;
end;

procedure _LapeProcess_ProcessHandle_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PHandle(Result)^ := PProcess(Params^[0])^.ProcessHandle;
end;

procedure _LapeProcess_ThreadHandle_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PHandle(Result)^ := PProcess(Params^[0])^.ThreadHandle;
end;

procedure _LapeProcess_ProcessID_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PProcess(Params^[0])^.ProcessID;
end;

procedure _LapeProcess_ThreadID_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PProcess(Params^[0])^.ThreadID;
end;

procedure _LapeProcess_ExitStatus_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PProcess(Params^[0])^.ExitStatus;
end;

procedure _LapeProcess_InheritHandles_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PProcess(Params^[0])^.InheritHandles;
end;

procedure _LapeProcess_InheritHandles_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.InheritHandles := PBoolean(Params^[1])^;
end;

procedure _LapeProcess_PipeBufferSize_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pcardinal(Result)^ := PProcess(Params^[0])^.PipeBufferSize;
end;

procedure _LapeProcess_PipeBufferSize_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.PipeBufferSize := Pcardinal(Params^[1])^;
end;

procedure _LapeProcess_Active_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PProcess(Params^[0])^.Active;
end;

procedure _LapeProcess_Active_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.Active := PBoolean(Params^[1])^;
end;

procedure _LapeProcess_CommandLine_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PProcess(Params^[0])^.CommandLine;
end;

procedure _LapeProcess_CommandLine_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.CommandLine := PString(Params^[1])^;
end;

procedure _LapeProcess_Executable_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PProcess(Params^[0])^.Executable;
end;

procedure _LapeProcess_Executable_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.Executable := PString(Params^[1])^;
end;

procedure _LapeProcess_Parameters_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Result)^ := PProcess(Params^[0])^.Parameters;
end;

procedure _LapeProcess_Parameters_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.Parameters := PStrings(Params^[1])^;
end;

procedure _LapeProcess_ConsoleTitle_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PProcess(Params^[0])^.ConsoleTitle;
end;

procedure _LapeProcess_ConsoleTitle_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.ConsoleTitle := PString(Params^[1])^;
end;

procedure _LapeProcess_CurrentDirectory_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PProcess(Params^[0])^.CurrentDirectory;
end;

procedure _LapeProcess_CurrentDirectory_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.CurrentDirectory := PString(Params^[1])^;
end;

procedure _LapeProcess_Desktop_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PProcess(Params^[0])^.Desktop;
end;

procedure _LapeProcess_Desktop_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.Desktop := PString(Params^[1])^;
end;

procedure _LapeProcess_Environment_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Result)^ := PProcess(Params^[0])^.Environment;
end;

procedure _LapeProcess_Environment_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.Environment := PStrings(Params^[1])^;
end;

procedure _LapeProcess_Options_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcessOptions(Result)^ := PProcess(Params^[0])^.Options;
end;

procedure _LapeProcess_Options_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.Options := PProcessOptions(Params^[1])^;
end;

procedure _LapeProcess_Priority_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcessPriority(Result)^ := PProcess(Params^[0])^.Priority;
end;

procedure _LapeProcess_Priority_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.Priority := PProcessPriority(Params^[1])^;
end;

procedure _LapeProcess_StartupOptions_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStartupOptions(Result)^ := PProcess(Params^[0])^.StartupOptions;
end;

procedure _LapeProcess_StartupOptions_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.StartupOptions := PStartupOptions(Params^[1])^;
end;

procedure _LapeProcess_Running_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PProcess(Params^[0])^.Running;
end;

procedure _LapeProcess_ShowWindow_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PShowWindowOptions(Result)^ := PProcess(Params^[0])^.ShowWindow;
end;

procedure _LapeProcess_ShowWindow_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.ShowWindow := PShowWindowOptions(Params^[1])^;
end;

procedure _LapeProcess_WindowColumns_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCardinal(Result)^ := PProcess(Params^[0])^.WindowColumns;
end;

procedure _LapeProcess_WindowColumns_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.WindowColumns := PCardinal(Params^[1])^;
end;

procedure _LapeProcess_WindowHeight_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCardinal(Result)^ := PProcess(Params^[0])^.WindowHeight;
end;

procedure _LapeProcess_WindowHeight_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.WindowHeight := PCardinal(Params^[1])^;
end;

procedure _LapeProcess_WindowLeft_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCardinal(Result)^ := PProcess(Params^[0])^.WindowLeft;
end;

procedure _LapeProcess_WindowLeft_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.WindowLeft := PCardinal(Params^[1])^;
end;

procedure _LapeProcess_WindowRows_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCardinal(Result)^ := PProcess(Params^[0])^.WindowRows;
end;

procedure _LapeProcess_WindowRows_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.WindowRows := PCardinal(Params^[1])^;
end;

procedure _LapeProcess_WindowTop_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCardinal(Result)^ := PProcess(Params^[0])^.WindowTop;
end;

procedure _LapeProcess_WindowTop_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.WindowTop := PCardinal(Params^[1])^;
end;

procedure _LapeProcess_WindowWidth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCardinal(Result)^ := PProcess(Params^[0])^.WindowWidth;
end;

procedure _LapeProcess_WindowWidth_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.WindowWidth := PCardinal(Params^[1])^;
end;

procedure _LapeProcess_FillAttribute_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCardinal(Result)^ := PProcess(Params^[0])^.FillAttribute;
end;

procedure _LapeProcess_FillAttribute_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.FillAttribute := PCardinal(Params^[1])^;
end;

procedure _LapeProcess_XTermProgram_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PProcess(Params^[0])^.XTermProgram;
end;

procedure _LapeProcess_XTermProgram_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.XTermProgram := PString(Params^[1])^;
end;

procedure _LapeProcess_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^ := TProcess.Create(PComponent(Params^[1])^);
end;

procedure _LapeProcess_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.Free();
end;

procedure _LapeProcess_Input_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  POutputPipeStream(Result)^ := PProcess(Params^[0])^.Input;
end;

procedure _LapeProcess_Output_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInputPipeStream(Result)^ := PProcess(Params^[0])^.Output;
end;

procedure _LapeProcess_Stderr_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInputPipeStream(Result)^ := PProcess(Params^[0])^.Stderr;
end;

procedure _LapeProcess_ExitCode_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PProcess(Params^[0])^.ExitCode;
end;

