unit simba.import_lcl_misc;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, process, spin, pipes, menus, graphics, lptypes, ffi,
  simba.script_compiler;

type
  PBitmap = ^TBitmap;
  PNotifyEvent = ^TNotifyEvent;
  PRect = ^TRect;
  PHandle = ^THandle;
  PComponent = ^TComponent;
  PSeekOrigin = ^TSeekOrigin;
  PStrings = ^TStrings;
  PInputPipeStream = ^TInputPipeStream;
  POutputPipeStream = ^TOutputPipeStream;
  PProcess = ^TProcess;
  PProcessOptions = ^TProcessOptions;
  PProcessPriority = ^TProcessPriority;
  PShowWindowOptions = ^TShowWindowOptions;
  PStartupOptions = ^TStartupOptions;

  PCustomFloatSpinEdit = ^TCustomFloatSpinEdit;
  PCustomSpinEdit = ^TCustomSpinEdit;
  PFloatSpinEdit = ^TFloatSpinEdit;
  PSpinEdit = ^TSpinEdit;

  PMenu = ^TMenu;
  PMainMenu = ^TMainMenu;
  PMenuItem = ^TMenuItem;

procedure _LapeOutputPipeStream_Seek(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pint64(Result)^ := POutputPipeStream(Params^[0])^.Seek(Pint64(Params^[1])^, PSeekOrigin(Params^[2])^);
end;

procedure _LapeOutputPipeStream_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := POutputPipeStream(Params^[0])^.Read(PPointer(Params^[1])^, PInteger(Params^[2])^);
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
  PInteger(Result)^ := PInputPipeStream(Params^[0])^.Write(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeInputPipeStream_Seek(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pint64(Result)^ := PInputPipeStream(Params^[0])^.Seek(Pint64(Params^[1])^, PSeekOrigin(Params^[2])^);
end;

procedure _LapeInputPipeStream_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PInputPipeStream(Params^[0])^.Read(PInteger(Params^[1])^, PInteger(Params^[2])^);
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
  PRect(Result)^ := PProcess(Params^[0])^.WindowRect;
end;

procedure _LapeProcess_WindowRect_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.WindowRect := PRect(Params^[1])^;
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

procedure _LapeCustomFloatSpinEdit_GetLimitedValue(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PDouble(Result)^ := PCustomFloatSpinEdit(Params^[0])^.GetLimitedValue(PDouble(Params^[1])^);
end;

procedure _LapeCustomFloatSpinEdit_ValueToStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PCustomFloatSpinEdit(Params^[0])^.ValueToStr(PDouble(Params^[1])^);
end;

procedure _LapeCustomFloatSpinEdit_StrToValue(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PDouble(Result)^ := PCustomFloatSpinEdit(Params^[0])^.StrToValue(PString(Params^[1])^);
end;

procedure _LapeCustomFloatSpinEdit_DecimalPlaces_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomFloatSpinEdit(Params^[0])^.DecimalPlaces;
end;

procedure _LapeCustomFloatSpinEdit_DecimalPlaces_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomFloatSpinEdit(Params^[0])^.DecimalPlaces := PInteger(Params^[1])^;
end;

procedure _LapeCustomFloatSpinEdit_Increment_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PDouble(Result)^ := PCustomFloatSpinEdit(Params^[0])^.Increment;
end;

procedure _LapeCustomFloatSpinEdit_Increment_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomFloatSpinEdit(Params^[0])^.Increment := PDouble(Params^[1])^;
end;

procedure _LapeCustomFloatSpinEdit_MinValue_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PDouble(Result)^ := PCustomFloatSpinEdit(Params^[0])^.MinValue;
end;

procedure _LapeCustomFloatSpinEdit_MinValue_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomFloatSpinEdit(Params^[0])^.MinValue := PDouble(Params^[1])^;
end;

procedure _LapeCustomFloatSpinEdit_MaxValue_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PDouble(Result)^ := PCustomFloatSpinEdit(Params^[0])^.MaxValue;
end;

procedure _LapeCustomFloatSpinEdit_MaxValue_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomFloatSpinEdit(Params^[0])^.MaxValue := PDouble(Params^[1])^;
end;

procedure _LapeCustomFloatSpinEdit_Value_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PDouble(Result)^ := PCustomFloatSpinEdit(Params^[0])^.Value;
end;

procedure _LapeCustomFloatSpinEdit_Value_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomFloatSpinEdit(Params^[0])^.Value := PDouble(Params^[1])^;
end;

procedure _LapeCustomFloatSpinEdit_ValueEmpty_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomFloatSpinEdit(Params^[0])^.ValueEmpty;
end;

procedure _LapeCustomFloatSpinEdit_ValueEmpty_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomFloatSpinEdit(Params^[0])^.ValueEmpty := PBoolean(Params^[1])^;
end;

procedure _LapeCustomFloatSpinEdit_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomFloatSpinEdit(Params^[0])^ := TCustomFloatSpinEdit.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomFloatSpinEdit_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomFloatSpinEdit(Params^[0])^.Free();
end;

procedure _LapeFloatSpinEdit_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFloatSpinEdit(Params^[0])^ := TFloatSpinEdit.Create(PComponent(Params^[1])^);
end;

procedure _LapeFloatSpinEdit_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFloatSpinEdit(Params^[0])^.Free();
end;

procedure _LapeCustomSpinEdit_Value_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomSpinEdit(Params^[0])^.Value;
end;

procedure _LapeCustomSpinEdit_Value_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomSpinEdit(Params^[0])^.Value := Pinteger(Params^[1])^;
end;

procedure _LapeCustomSpinEdit_MinValue_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomSpinEdit(Params^[0])^.MinValue;
end;

procedure _LapeCustomSpinEdit_MinValue_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomSpinEdit(Params^[0])^.MinValue := Pinteger(Params^[1])^;
end;

procedure _LapeCustomSpinEdit_MaxValue_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomSpinEdit(Params^[0])^.MaxValue;
end;

procedure _LapeCustomSpinEdit_MaxValue_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomSpinEdit(Params^[0])^.MaxValue := Pinteger(Params^[1])^;
end;

procedure _LapeCustomSpinEdit_Increment_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomSpinEdit(Params^[0])^.Increment;
end;

procedure _LapeCustomSpinEdit_Increment_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomSpinEdit(Params^[0])^.Increment := Pinteger(Params^[1])^;
end;

procedure _LapeCustomSpinEdit_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomSpinEdit(Params^[0])^ := TCustomSpinEdit.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomSpinEdit_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomSpinEdit(Params^[0])^.Free();
end;

procedure _LapeSpinEdit_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PSpinEdit(Params^[0])^ := TSpinEdit.Create(PComponent(Params^[1])^);
end;

procedure _LapeSpinEdit_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PSpinEdit(Params^[0])^.Free();
end;

procedure _LapeMenuItem_Find(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Result)^ := PMenuItem(Params^[0])^.Find(PString(Params^[1])^);
end;

procedure _LapeMenuItem_GetParentMenu(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenu(Result)^ := PMenuItem(Params^[0])^.GetParentMenu();
end;

procedure _LapeMenuItem_GetIsRightToLeft(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenuItem(Params^[0])^.GetIsRightToLeft();
end;

procedure _LapeMenuItem_HandleAllocated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenuItem(Params^[0])^.HandleAllocated();
end;

procedure _LapeMenuItem_HasIcon(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMenuItem(Params^[0])^.HasIcon();
end;

procedure _LapeMenuItem_IndexOf(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMenuItem(Params^[0])^.IndexOf(PMenuItem(Params^[1])^);
end;

procedure _LapeMenuItem_IndexOfCaption(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMenuItem(Params^[0])^.IndexOfCaption(PString(Params^[1])^);
end;

procedure _LapeMenuItem_VisibleIndexOf(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMenuItem(Params^[0])^.VisibleIndexOf(PMenuItem(Params^[1])^);
end;

procedure _LapeMenuItem_Add(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Add(PMenuItem(Params^[1])^);
end;

procedure _LapeMenuItem_AddSeparator(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.AddSeparator();
end;

procedure _LapeMenuItem_Click(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Click();
end;

procedure _LapeMenuItem_Delete(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Delete(PInteger(Params^[1])^);
end;

procedure _LapeMenuItem_HandleNeeded(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.HandleNeeded();
end;

procedure _LapeMenuItem_Insert(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Insert(PInteger(Params^[1])^, PMenuItem(Params^[2])^);
end;

procedure _LapeMenuItem_RecreateHandle(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.RecreateHandle();
end;

procedure _LapeMenuItem_Remove(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Remove(PMenuItem(Params^[1])^);
end;

procedure _LapeMenuItem_IsCheckItem(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMenuItem(Params^[0])^.IsCheckItem();
end;

procedure _LapeMenuItem_IsLine(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenuItem(Params^[0])^.IsLine();
end;

procedure _LapeMenuItem_IsInMenuBar(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMenuItem(Params^[0])^.IsInMenuBar();
end;

procedure _LapeMenuItem_Clear(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Clear();
end;

procedure _LapeMenuItem_HasBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMenuItem(Params^[0])^.HasBitmap();
end;

procedure _LapeMenuItem_Count_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMenuItem(Params^[0])^.Count;
end;

procedure _LapeMenuItem_Items_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Result)^ := PMenuItem(Params^[0])^.Items[PInteger(Params^[1])^];
end;

procedure _LapeMenuItem_MenuIndex_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMenuItem(Params^[0])^.MenuIndex;
end;

procedure _LapeMenuItem_MenuIndex_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.MenuIndex := PInteger(Params^[1])^;
end;

procedure _LapeMenuItem_Menu_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenu(Result)^ := PMenuItem(Params^[0])^.Menu;
end;

procedure _LapeMenuItem_Parent_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Result)^ := PMenuItem(Params^[0])^.Parent;
end;

procedure _LapeMenuItem_Command_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWord(Result)^ := PMenuItem(Params^[0])^.Command;
end;

procedure _LapeMenuItem_MenuVisibleIndex(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PMenuItem(Params^[0])^.MenuVisibleIndex();
end;

procedure _LapeMenuItem_AutoCheck_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMenuItem(Params^[0])^.AutoCheck;
end;

procedure _LapeMenuItem_AutoCheck_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.AutoCheck := Pboolean(Params^[1])^;
end;

procedure _LapeMenuItem_Default_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenuItem(Params^[0])^.Default;
end;

procedure _LapeMenuItem_Default_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Default := PBoolean(Params^[1])^;
end;

procedure _LapeMenuItem_Bitmap_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBitmap(Result)^ := PMenuItem(Params^[0])^.Bitmap;
end;

procedure _LapeMenuItem_Bitmap_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Bitmap := PBitmap(Params^[1])^;
end;

procedure _LapeMenuItem_GroupIndex_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  pbyte(Result)^ := PMenuItem(Params^[0])^.GroupIndex;
end;

procedure _LapeMenuItem_GroupIndex_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.GroupIndex := pbyte(Params^[1])^;
end;

procedure _LapeMenuItem_Hint_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PMenuItem(Params^[0])^.Hint;
end;

procedure _LapeMenuItem_Hint_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Hint := PString(Params^[1])^;
end;

procedure _LapeMenuItem_RadioItem_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenuItem(Params^[0])^.RadioItem;
end;

procedure _LapeMenuItem_RadioItem_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.RadioItem := PBoolean(Params^[1])^;
end;

procedure _LapeMenuItem_RightJustify_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMenuItem(Params^[0])^.RightJustify;
end;

procedure _LapeMenuItem_RightJustify_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.RightJustify := Pboolean(Params^[1])^;
end;

procedure _LapeMenuItem_OnClick_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PMenuItem(Params^[0])^.OnClick;
end;

procedure _LapeMenuItem_OnClick_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.OnClick := PNotifyEvent(Params^[1])^;
end;

procedure _LapeMenuItem_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^ := TMenuItem.Create(PComponent(Params^[1])^);
end;

procedure _LapeMenuItem_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Free();
end;

procedure _LapeMenuItem_Caption_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PMenuItem(Params^[0])^.Caption;
end;

procedure _LapeMenuItem_Caption_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Caption := PString(Params^[1])^;
end;

procedure _LapeMenuItem_Checked_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenuItem(Params^[0])^.Checked;
end;

procedure _LapeMenuItem_Checked_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Checked := PBoolean(Params^[1])^;
end;

procedure _LapeMenuItem_AddEx(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
type
  TMenuItemArray = array of TMenuItem;
begin
  PMenuItem(Params^[0])^.Add(TMenuItemArray(Params^[1]^));
end;

procedure _LapeMenuItem_AddMenu(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Result)^ := TMenuItem.Create(PMenu(Params^[0])^);
  PMenuItem(Result)^.Caption := PString(Params^[1])^;
  PMenuItem(Params^[0])^.Add(PMenuItem(Result)^);
end;

procedure _LapeMenu_HandleAllocated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenu(Params^[0])^.HandleAllocated();
end;

procedure _LapeMenu_IsRightToLeft(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenu(Params^[0])^.IsRightToLeft();
end;

procedure _LapeMenu_UseRightToLeftAlignment(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenu(Params^[0])^.UseRightToLeftAlignment();
end;

procedure _LapeMenu_UseRightToLeftReading(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenu(Params^[0])^.UseRightToLeftReading();
end;

procedure _LapeMenu_HandleNeeded(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenu(Params^[0])^.HandleNeeded();
end;

procedure _LapeMenu_DispatchCommand(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenu(Params^[0])^.DispatchCommand(PWord(Params^[1])^);
end;

procedure _LapeMenu_Parent_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PComponent(Result)^ := PMenu(Params^[0])^.Parent;
end;

procedure _LapeMenu_Parent_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenu(Params^[0])^.Parent := PComponent(Params^[1])^;
end;

procedure _LapeMenu_ShortcutHandled_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMenu(Params^[0])^.ShortcutHandled;
end;

procedure _LapeMenu_ShortcutHandled_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenu(Params^[0])^.ShortcutHandled := Pboolean(Params^[1])^;
end;

procedure _LapeMenu_ParentBidiMode_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenu(Params^[0])^.ParentBidiMode;
end;

procedure _LapeMenu_ParentBidiMode_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenu(Params^[0])^.ParentBidiMode := PBoolean(Params^[1])^;
end;

procedure _LapeMenu_Items_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Result)^ := PMenu(Params^[0])^.Items;
end;

procedure _LapeMenu_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenu(Params^[0])^ := TMenu.Create(PComponent(Params^[1])^);
end;

procedure _LapeMenu_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenu(Params^[0])^.Free();
end;

procedure _LapeMenu_AddMenu(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Result)^ := TMenuItem.Create(PMenu(Params^[0])^);
  PMenuItem(Result)^.Caption := PString(Params^[1])^;
  PMenu(Params^[0])^.Items.Add(PMenuItem(Result)^);
end;

procedure _LapeMainMenu_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMainMenu(Params^[0])^ := TMainMenu.Create(PComponent(Params^[1])^);
end;

procedure _LapeMainMenu_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMainMenu(Params^[0])^.Free();
end;

procedure ImportLCLMisc(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClass('TCustomFloatSpinEdit', 'TCustomEdit');
    addGlobalFunc('function TCustomFloatSpinEdit.GetLimitedValue(const AValue: Double): Double;', @_LapeCustomFloatSpinEdit_GetLimitedValue);
    addGlobalFunc('function TCustomFloatSpinEdit.ValueToStr(const AValue: Double): String;', @_LapeCustomFloatSpinEdit_ValueToStr);
    addGlobalFunc('function TCustomFloatSpinEdit.StrToValue(const S: String): Double;', @_LapeCustomFloatSpinEdit_StrToValue);
    addClassVar('TCustomFloatSpinEdit', 'DecimalPlaces', 'Integer', @_LapeCustomFloatSpinEdit_DecimalPlaces_Read, @_LapeCustomFloatSpinEdit_DecimalPlaces_Write);
    addClassVar('TCustomFloatSpinEdit', 'Increment', 'Double', @_LapeCustomFloatSpinEdit_Increment_Read, @_LapeCustomFloatSpinEdit_Increment_Write);
    addClassVar('TCustomFloatSpinEdit', 'MinValue', 'Double', @_LapeCustomFloatSpinEdit_MinValue_Read, @_LapeCustomFloatSpinEdit_MinValue_Write);
    addClassVar('TCustomFloatSpinEdit', 'MaxValue', 'Double', @_LapeCustomFloatSpinEdit_MaxValue_Read, @_LapeCustomFloatSpinEdit_MaxValue_Write);
    addClassVar('TCustomFloatSpinEdit', 'Value', 'Double', @_LapeCustomFloatSpinEdit_Value_Read, @_LapeCustomFloatSpinEdit_Value_Write);
    addClassVar('TCustomFloatSpinEdit', 'ValueEmpty', 'Boolean', @_LapeCustomFloatSpinEdit_ValueEmpty_Read, @_LapeCustomFloatSpinEdit_ValueEmpty_Write);
    addGlobalFunc('procedure TCustomFloatSpinEdit.Init(TheOwner: TComponent); override', @_LapeCustomFloatSpinEdit_Init);

    addClass('TFloatSpinEdit', 'TCustomFloatSpinEdit');
    addGlobalFunc('procedure TFloatSpinEdit.Init(TheOwner: TComponent); override', @_LapeFloatSpinEdit_Init);

    addClass('TCustomSpinEdit', 'TCustomFloatSpinEdit');
    addClassVar('TCustomSpinEdit', 'Value', 'integer', @_LapeCustomSpinEdit_Value_Read, @_LapeCustomSpinEdit_Value_Write);
    addClassVar('TCustomSpinEdit', 'MinValue', 'integer', @_LapeCustomSpinEdit_MinValue_Read, @_LapeCustomSpinEdit_MinValue_Write);
    addClassVar('TCustomSpinEdit', 'MaxValue', 'integer', @_LapeCustomSpinEdit_MaxValue_Read, @_LapeCustomSpinEdit_MaxValue_Write);
    addClassVar('TCustomSpinEdit', 'Increment', 'integer', @_LapeCustomSpinEdit_Increment_Read, @_LapeCustomSpinEdit_Increment_Write);
    addGlobalFunc('procedure TCustomSpinEdit.Init(TheOwner: TComponent); override', @_LapeCustomSpinEdit_Init);

    addClass('TSpinEdit', 'TCustomSpinEdit');
    addGlobalFunc('procedure TSpinEdit.Init(TheOwner: TComponent); override', @_LapeSpinEdit_Init);

    addGlobalType('(oRunSuspended,poWaitOnExit,poUsePipes,poStderrToOutPut,poNoConsole,poNewConsole,poDefaultErrorMode,poNewProcessGroup,poDebugProcess,poDebugOnlyThisProcess,poDetached,poPassInput,poRunIdle)', 'TProcessOption');
    addGlobalType('(swoNone,swoHIDE,swoMaximize,swoMinimize,swoRestore,swoShow, swoShowDefault,swoShowMaximized,swoShowMinimized, swoshowMinNOActive,swoShowNA,swoShowNoActivate,swoShowNormal)', 'TShowWindowOptions');
    addGlobalType('(suoUseShowWindow,suoUseSize,suoUsePosition, suoUseCountChars,suoUseFillAttribute)', 'TStartupOption');
    addGlobalType('(ppHigh,ppIdle,ppNormal,ppRealTime)', 'TProcessPriority');
    addGlobalType('set of TProcessOption', 'TProcessOptions');
    addGlobalType('set of TStartupOption', 'TStartupOptions');

    addClass('TOutputPipeStream', 'THandleStream');
    addGlobalFunc('procedure TOutputPipeStream.Init(AHandle: THandle); override', @_LapeOutputPipeStream_Init);

    addClass('TInputPipeStream', 'THandleStream');
    addClassVar('TInputPipeStream', 'NumBytesAvailable', 'UInt32', @_LapeInputPipeStream_NumBytesAvailable_Read, nil);
    addGlobalFunc('procedure TInputPipeStream.Init(AHandle: THandle); override', @_LapeInputPipeStream_Init);

    addClass('TProcess', 'TComponent');
    addGlobalFunc('procedure TProcess.Execute;', @_LapeProcess_Execute);
    addGlobalFunc('procedure TProcess.CloseInput;', @_LapeProcess_CloseInput);
    addGlobalFunc('procedure TProcess.CloseOutput;', @_LapeProcess_CloseOutput);
    addGlobalFunc('procedure TProcess.CloseStderr;', @_LapeProcess_CloseStderr);
    addGlobalFunc('function TProcess.Resume: Integer;', @_LapeProcess_Resume);
    addGlobalFunc('function TProcess.Suspend: Integer;', @_LapeProcess_Suspend);
    addGlobalFunc('function TProcess.Terminate(AExitCode : Integer): Boolean;', @_LapeProcess_Terminate);
    addGlobalFunc('function TProcess.WaitOnExit: Boolean;', @_LapeProcess_WaitOnExit);
    addClassVar('TProcess', 'WindowRect', 'TRect', @_LapeProcess_WindowRect_Read, @_LapeProcess_WindowRect_Write);
    addClassVar('TProcess', 'Handle', 'THandle', @_LapeProcess_Handle_Read);
    addClassVar('TProcess', 'ProcessHandle', 'THandle', @_LapeProcess_ProcessHandle_Read);
    addClassVar('TProcess', 'ThreadHandle', 'THandle', @_LapeProcess_ThreadHandle_Read);
    addClassVar('TProcess', 'ProcessID', 'Integer', @_LapeProcess_ProcessID_Read);
    addClassVar('TProcess', 'ThreadID', 'Integer', @_LapeProcess_ThreadID_Read);
    addClassVar('TProcess', 'Input', 'TOutputPipeStream', @_LapeProcess_Input_Read);
    addClassVar('TProcess', 'Output', 'TInputPipeStream', @_LapeProcess_Output_Read);
    addClassVar('TProcess', 'Stderr', 'TinputPipeStream', @_LapeProcess_Stderr_Read);
    addClassVar('TProcess', 'ExitStatus', 'Integer', @_LapeProcess_ExitStatus_Read);
    addClassVar('TProcess', 'ExitCode', 'Integer', @_LapeProcess_ExitCode_Read);
    addClassVar('TProcess', 'InheritHandles', 'Boolean', @_LapeProcess_InheritHandles_Read, @_LapeProcess_InheritHandles_Write);
    addClassVar('TProcess', 'PipeBufferSize', 'UInt32', @_LapeProcess_PipeBufferSize_Read, @_LapeProcess_PipeBufferSize_Write);
    addClassVar('TProcess', 'Active', 'Boolean', @_LapeProcess_Active_Read, @_LapeProcess_Active_Write);
    addClassVar('TProcess', 'CommandLine', 'String', @_LapeProcess_CommandLine_Read, @_LapeProcess_CommandLine_Write);
    addClassVar('TProcess', 'Executable', 'String', @_LapeProcess_Executable_Read, @_LapeProcess_Executable_Write);
    addClassVar('TProcess', 'Parameters', 'TStrings', @_LapeProcess_Parameters_Read, @_LapeProcess_Parameters_Write);
    addClassVar('TProcess', 'ConsoleTitle', 'String', @_LapeProcess_ConsoleTitle_Read, @_LapeProcess_ConsoleTitle_Write);
    addClassVar('TProcess', 'CurrentDirectory', 'String', @_LapeProcess_CurrentDirectory_Read, @_LapeProcess_CurrentDirectory_Write);
    addClassVar('TProcess', 'Desktop', 'String', @_LapeProcess_Desktop_Read, @_LapeProcess_Desktop_Write);
    addClassVar('TProcess', 'Environment', 'TStrings', @_LapeProcess_Environment_Read, @_LapeProcess_Environment_Write);
    addClassVar('TProcess', 'Options', 'TProcessOptions', @_LapeProcess_Options_Read, @_LapeProcess_Options_Write);
    addClassVar('TProcess', 'Priority', 'TProcessPriority', @_LapeProcess_Priority_Read, @_LapeProcess_Priority_Write);
    addClassVar('TProcess', 'StartupOptions', 'TStartupOptions', @_LapeProcess_StartupOptions_Read, @_LapeProcess_StartupOptions_Write);
    addClassVar('TProcess', 'Running', 'Boolean', @_LapeProcess_Running_Read);
    addClassVar('TProcess', 'ShowWindow', 'TShowWindowOptions', @_LapeProcess_ShowWindow_Read, @_LapeProcess_ShowWindow_Write);
    addClassVar('TProcess', 'WindowColumns', 'UInt32', @_LapeProcess_WindowColumns_Read, @_LapeProcess_WindowColumns_Write);
    addClassVar('TProcess', 'WindowHeight', 'UInt32', @_LapeProcess_WindowHeight_Read, @_LapeProcess_WindowHeight_Write);
    addClassVar('TProcess', 'WindowLeft', 'UInt32', @_LapeProcess_WindowLeft_Read, @_LapeProcess_WindowLeft_Write);
    addClassVar('TProcess', 'WindowRows', 'UInt32', @_LapeProcess_WindowRows_Read, @_LapeProcess_WindowRows_Write);
    addClassVar('TProcess', 'WindowTop', 'UInt32', @_LapeProcess_WindowTop_Read, @_LapeProcess_WindowTop_Write);
    addClassVar('TProcess', 'WindowWidth', 'UInt32', @_LapeProcess_WindowWidth_Read, @_LapeProcess_WindowWidth_Write);
    addClassVar('TProcess', 'FillAttribute', 'UInt32', @_LapeProcess_FillAttribute_Read, @_LapeProcess_FillAttribute_Write);
    addClassVar('TProcess', 'XTermProgram', 'String', @_LapeProcess_XTermProgram_Read, @_LapeProcess_XTermProgram_Write);
    addGlobalFunc('procedure TProcess.Init(AOwner : TComponent); override', @_LapeProcess_Init);
    //addGlobalFunc('procedure TProcess.Free;', @_LapeProcess_Free);

    addClass('TMenu', 'TComponent');
    addClass('TMenuItem', 'TComponent');
    addGlobalFunc('function TMenuItem.Find(const ACaption: string): TMenuItem;', @_LapeMenuItem_Find);
    addGlobalFunc('function TMenuItem.GetParentMenu: TMenu;', @_LapeMenuItem_GetParentMenu);
    addGlobalFunc('function TMenuItem.GetIsRightToLeft: Boolean;', @_LapeMenuItem_GetIsRightToLeft);
    addGlobalFunc('function TMenuItem.HandleAllocated: Boolean;', @_LapeMenuItem_HandleAllocated);
    addGlobalFunc('function TMenuItem.HasIcon: boolean;', @_LapeMenuItem_HasIcon);
    addGlobalFunc('function TMenuItem.IndexOf(Item: TMenuItem): Integer;', @_LapeMenuItem_IndexOf);
    addGlobalFunc('function TMenuItem.IndexOfCaption(const ACaption: string): Integer;', @_LapeMenuItem_IndexOfCaption);
    addGlobalFunc('function TMenuItem.VisibleIndexOf(Item: TMenuItem): Integer;', @_LapeMenuItem_VisibleIndexOf);
    addGlobalFunc('procedure TMenuItem.Add(Item: TMenuItem);', @_LapeMenuItem_Add);
    addGlobalFunc('procedure TMenuItem.AddEx(Items: array of TMenuItem);', @_LapeMenuItem_AddEx);
    addGlobalFunc('procedure TMenuItem.AddSeparator;', @_LapeMenuItem_AddSeparator);
    addGlobalFunc('procedure TMenuItem.Click;', @_LapeMenuItem_Click);
    addGlobalFunc('procedure TMenuItem.Delete(Index: Integer);', @_LapeMenuItem_Delete);
    addGlobalFunc('procedure TMenuItem.HandleNeeded;', @_LapeMenuItem_HandleNeeded);
    addGlobalFunc('procedure TMenuItem.Insert(Index: Integer; Item: TMenuItem);', @_LapeMenuItem_Insert);
    addGlobalFunc('procedure TMenuItem.RecreateHandle;', @_LapeMenuItem_RecreateHandle);
    addGlobalFunc('procedure TMenuItem.Remove(Item: TMenuItem);', @_LapeMenuItem_Remove);
    addGlobalFunc('function TMenuItem.IsCheckItem: boolean;', @_LapeMenuItem_IsCheckItem);
    addGlobalFunc('function TMenuItem.IsLine: Boolean;', @_LapeMenuItem_IsLine);
    addGlobalFunc('function TMenuItem.IsInMenuBar: boolean;', @_LapeMenuItem_IsInMenuBar);
    addGlobalFunc('procedure TMenuItem.Clear;', @_LapeMenuItem_Clear);
    addGlobalFunc('function TMenuItem.HasBitmap: boolean;', @_LapeMenuItem_HasBitmap);
    addGlobalFunc('function TMenuItem.AddMenu(s: string): TMenuItem;', @_LapeMenuItem_AddMenu);
    addClassVar('TMenuItem', 'Count', 'Integer', @_LapeMenuItem_Count_Read);
    addClassVar('TMenuItem', 'Items', 'TMenuItem', @_LapeMenuItem_Items_Read);
    addClassVar('TMenuItem', 'Hint', 'String', @_LapeMenuItem_Hint_Read, @_LapeMenuItem_Hint_Write);
    addClassVar('TMenuItem', 'Checked', 'Boolean', @_LapeMenuItem_Checked_Read, @_LapeMenuItem_Checked_Write);
    addClassVar('TMenuItem', 'MenuIndex', 'Integer', @_LapeMenuItem_MenuIndex_Read, @_LapeMenuItem_MenuIndex_Write);
    addClassVar('TMenuItem', 'Menu', 'TMenu', @_LapeMenuItem_Menu_Read);
    addClassVar('TMenuItem', 'Parent', 'TMenuItem', @_LapeMenuItem_Parent_Read);
    addClassVar('TMenuItem', 'Command', 'Int16', @_LapeMenuItem_Command_Read);
    addClassVar('TMenuItem', 'AutoCheck', 'boolean', @_LapeMenuItem_AutoCheck_Read, @_LapeMenuItem_AutoCheck_Write);
    addClassVar('TMenuItem', 'Default', 'Boolean', @_LapeMenuItem_Default_Read, @_LapeMenuItem_Default_Write);
    addClassVar('TMenuItem', 'Bitmap', 'TBitmap', @_LapeMenuItem_Bitmap_Read, @_LapeMenuItem_Bitmap_Write);
    addClassVar('TMenuItem', 'GroupIndex', 'Byte', @_LapeMenuItem_GroupIndex_Read, @_LapeMenuItem_GroupIndex_Write);
    addClassVar('TMenuItem', 'RadioItem', 'Boolean', @_LapeMenuItem_RadioItem_Read, @_LapeMenuItem_RadioItem_Write);
    addClassVar('TMenuItem', 'RightJustify', 'boolean', @_LapeMenuItem_RightJustify_Read, @_LapeMenuItem_RightJustify_Write);
    addClassVar('TMenuItem', 'OnClick', 'TNotifyEvent', @_LapeMenuItem_OnClick_Read, @_LapeMenuItem_OnClick_Write);
    addClassVar('TMenuItem', 'Caption', 'String', @_LapeMenuItem_Caption_Read, @_LapeMenuItem_Caption_Write);
    addGlobalFunc('procedure TMenuItem.Init(AOwner: TComponent); override', @_LapeMenuItem_Init);
    // addGlobalFunc('procedure TMenuItem.Free;', @_LapeMenuItem_Free);

    addGlobalFunc('function TMenu.HandleAllocated: Boolean;', @_LapeMenu_HandleAllocated);
    addGlobalFunc('function TMenu.IsRightToLeft: Boolean;', @_LapeMenu_IsRightToLeft);
    addGlobalFunc('function TMenu.UseRightToLeftAlignment: Boolean;', @_LapeMenu_UseRightToLeftAlignment);
    addGlobalFunc('function TMenu.UseRightToLeftReading: Boolean;', @_LapeMenu_UseRightToLeftReading);
    addGlobalFunc('procedure TMenu.HandleNeeded;', @_LapeMenu_HandleNeeded);
    addGlobalFunc('function TMenu.DispatchCommand(ACommand: Int16): Boolean;', @_LapeMenu_DispatchCommand);
    addGlobalFunc('function TMenu.AddMenu(name: string): TMenuItem;', @_LapeMenu_AddMenu);
    addClassVar('TMenu', 'Parent', 'TComponent', @_LapeMenu_Parent_Read, @_LapeMenu_Parent_Write);
    addClassVar('TMenu', 'ShortcutHandled', 'boolean', @_LapeMenu_ShortcutHandled_Read, @_LapeMenu_ShortcutHandled_Write);
    addClassVar('TMenu', 'Items', 'TMenuItem', @_LapeMenu_Items_Read, nil);
    addGlobalFunc('procedure TMenu.Init(AOwner: TComponent); override', @_LapeMenu_Init);
    //addGlobalFunc('procedure TMenu.Free;', @_LapeMenu_Free);

    addClass('TMainMenu', 'TMenu');
    addGlobalFunc('procedure TMainMenu.Init(AOwner: TComponent); override', @_LapeMainMenu_Init);
    //addGlobalFunc('procedure TMainMenu.Free;', @_LapeMainMenu_Free);
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportLCLMisc);

end.
