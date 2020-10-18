unit simba.script_import_lclprocess;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_LCLProcess(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  process, pipes;

type
  PSeekOrigin = ^TSeekOrigin;
  PComponent = ^TComponent;
  PProcess = ^TProcess;
  PRect = ^TRect;
  PHandle = ^THandle;
  PProcessOption = ^TProcessOption;
  PShowWindowOptions = ^TShowWindowOptions;
  PStartupOptions = ^TStartupOptions;
  PProcessOptions = ^TProcessOptions;
  PProcessPriority = ^TProcessPriority;
  PStartupOption = ^TStartupOption;
  POutputPipeStream = ^TOutputPipeStream;
  PInputPipeStream = ^TInputPipeStream;
  PInt64 = ^Int64;
  PLongInt = ^LongInt;


//function Seek(const Offset: int64; Origin: TSeekOrigin): int64; override;
procedure Lape_TOutputPipeStream_Seek(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pint64(Result)^ := POutputPipeStream(Params^[0])^.Seek(Pint64(Params^[1])^, PSeekOrigin(Params^[2])^);
end;

//Function Read (Var Buffer; Count : Longint) : longint; Override;
procedure Lape_TOutputPipeStream_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Plongint(Result)^ := POutputPipeStream(Params^[0])^.Read(PLongint(Params^[1])^, PLongint(Params^[2])^);
end;

//constructor Create();
procedure Lape_TOutputPipeStream_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POutputPipeStream(Params^[0])^ := TOutputPipeStream.Create(PHandle(Params^[1])^);
end;

//procedure Free();
procedure Lape_TOutputPipeStream_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POutputPipeStream(Params^[0])^.Free();
end;

procedure Lape_Import_TOutputPipeStream(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TOutputPipeStream', 'THandleStream');

   // addGlobalFunc('function TOutputPipeStream.Seek(const Offset: int64; Origin: TSeekOrigin): int64; constref;', @Lape_TOutputPipeStream_Seek);
   // addGlobalFunc('function TOutputPipeStream.Read(Var Buffer; Count : Longint): longint; constref;', @Lape_TOutputPipeStream_Read);
    addGlobalFunc('procedure TOutputPipeStream.Init(AHandle: THandle); override;', @Lape_TOutputPipeStream_Init);
   // addGlobalFunc('procedure TOutputPipeStream.Free(); constref;', @Lape_TOutputPipeStream_Free);
  end;
end;

//Function Write (Const Buffer; Count : Longint) :Longint; Override;
procedure Lape_TInputPipeStream_Write(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLongint(Result)^ := PInputPipeStream(Params^[0])^.Write(PLongint(Params^[1])^, PLongint(Params^[2])^);
end;

//function Seek(const Offset: int64; Origin: TSeekOrigin): int64; override;
procedure Lape_TInputPipeStream_Seek(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pint64(Result)^ := PInputPipeStream(Params^[0])^.Seek(Pint64(Params^[1])^, PSeekOrigin(Params^[2])^);
end;

//Function Read (Var Buffer; Count : Longint) : longint; Override;
procedure Lape_TInputPipeStream_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Plongint(Result)^ := PInputPipeStream(Params^[0])^.Read(PLongint(Params^[1])^, PLongint(Params^[2])^);
end;

//Read: property NumBytesAvailable: UInt32 read GetNumBytesAvailable;
procedure Lape_TInputPipeStream_NumBytesAvailable_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PUInt32(Result)^ := PInputPipeStream(Params^[0])^.NumBytesAvailable;
end;

//constructor Create();
procedure Lape_TInputPipeStream_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInputPipeStream(Params^[0])^ := TInputPipeStream.Create(PHandle(Params^[1])^);
end;

//procedure Free();
procedure Lape_TInputPipeStream_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInputPipeStream(Params^[0])^.Free();
end;

procedure Lape_Import_TInputPipeStream(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TInputPipeStream', 'THandleStream');

    //addGlobalFunc('function TInputPipeStream.Write(constref Buffer; Count : Longint): Longint; constref;', @Lape_TInputPipeStream_Write);
    //addGlobalFunc('function TInputPipeStream.Seek(const Offset: int64; Origin: TSeekOrigin): int64; constref;', @Lape_TInputPipeStream_Seek);
    //addGlobalFunc('function TInputPipeStream.Read(var Buffer; Count : Longint): longint; constref;', @Lape_TInputPipeStream_Read);
    addClassVar('TInputPipeStream', 'NumBytesAvailable', 'UInt32', @Lape_TInputPipeStream_NumBytesAvailable_Read, nil);
    addGlobalFunc('procedure TInputPipeStream.Init(AHandle: THandle); override;', @Lape_TInputPipeStream_Init);
    //addGlobalFunc('procedure TInputPipeStream.Free(); constref;', @Lape_TInputPipeStream_Free);
  end;
end;

//Procedure Execute; virtual;
procedure Lape_TProcess_Execute(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.Execute();
end;

//procedure CloseInput; virtual;
procedure Lape_TProcess_CloseInput(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.CloseInput();
end;

//procedure CloseOutput; virtual;
procedure Lape_TProcess_CloseOutput(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.CloseOutput();
end;

//procedure CloseStderr; virtual;
procedure Lape_TProcess_CloseStderr(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.CloseStderr();
end;

//Function Resume : Integer; virtual;
procedure Lape_TProcess_Resume(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PProcess(Params^[0])^.Resume();
end;

//Function Suspend : Integer; virtual;
procedure Lape_TProcess_Suspend(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PProcess(Params^[0])^.Suspend();
end;

//Function Terminate (AExitCode : Integer): Boolean; virtual;
procedure Lape_TProcess_Terminate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PProcess(Params^[0])^.Terminate(PInteger(Params^[1])^);
end;

//Function WaitOnExit : Boolean;
procedure Lape_TProcess_WaitOnExit(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PProcess(Params^[0])^.WaitOnExit();
end;

//Read: Property WindowRect : Trect Read GetWindowRect Write SetWindowRect;
procedure Lape_TProcess_WindowRect_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Prect(Result)^ := PProcess(Params^[0])^.WindowRect;
end;

//Write: Property WindowRect : Trect Read GetWindowRect Write SetWindowRect;
procedure Lape_TProcess_WindowRect_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.WindowRect := Prect(Params^[1])^;
end;

//Read: Property Handle : THandle Read FProcessHandle;
procedure Lape_TProcess_Handle_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PHandle(Result)^ := PProcess(Params^[0])^.Handle;
end;

//Read: Property ProcessHandle : THandle Read FProcessHandle;
procedure Lape_TProcess_ProcessHandle_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PHandle(Result)^ := PProcess(Params^[0])^.ProcessHandle;
end;

//Read: Property ThreadHandle : THandle Read FThreadHandle;
procedure Lape_TProcess_ThreadHandle_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PHandle(Result)^ := PProcess(Params^[0])^.ThreadHandle;
end;

//Read: Property ProcessID : Integer Read FProcessID;
procedure Lape_TProcess_ProcessID_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PProcess(Params^[0])^.ProcessID;
end;

//Read: Property ThreadID : Integer Read FThreadID;
procedure Lape_TProcess_ThreadID_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PProcess(Params^[0])^.ThreadID;
end;

//Read: Property ExitStatus : Integer Read GetExitStatus;
procedure Lape_TProcess_ExitStatus_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PProcess(Params^[0])^.ExitStatus;
end;

//Read: Property InheritHandles : Boolean Read FInheritHandles Write FInheritHandles;
procedure Lape_TProcess_InheritHandles_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PProcess(Params^[0])^.InheritHandles;
end;

//Write: Property InheritHandles : Boolean Read FInheritHandles Write FInheritHandles;
procedure Lape_TProcess_InheritHandles_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.InheritHandles := PBoolean(Params^[1])^;
end;

//Read: property PipeBufferSize : cardinal read FPipeBufferSize write FPipeBufferSize default 1024;
procedure Lape_TProcess_PipeBufferSize_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pcardinal(Result)^ := PProcess(Params^[0])^.PipeBufferSize;
end;

//Write: property PipeBufferSize : cardinal read FPipeBufferSize write FPipeBufferSize default 1024;
procedure Lape_TProcess_PipeBufferSize_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.PipeBufferSize := Pcardinal(Params^[1])^;
end;

//Read: Property Active : Boolean Read GetRunning Write SetActive;
procedure Lape_TProcess_Active_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PProcess(Params^[0])^.Active;
end;

//Write: Property Active : Boolean Read GetRunning Write SetActive;
procedure Lape_TProcess_Active_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.Active := PBoolean(Params^[1])^;
end;

//Read: Property ApplicationName : String Read FApplicationName Write SetApplicationName; deprecated;
procedure Lape_TProcess_ApplicationName_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PProcess(Params^[0])^.ApplicationName;
end;

//Write: Property ApplicationName : String Read FApplicationName Write SetApplicationName; deprecated;
procedure Lape_TProcess_ApplicationName_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.ApplicationName := PlpString(Params^[1])^;
end;

//Read: Property CommandLine : String Read FCommandLine Write SetCommandLine ; deprecated;
procedure Lape_TProcess_CommandLine_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PProcess(Params^[0])^.CommandLine;
end;

//Write: Property CommandLine : String Read FCommandLine Write SetCommandLine ; deprecated;
procedure Lape_TProcess_CommandLine_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.CommandLine := PlpString(Params^[1])^;
end;

//Read: Property Executable : String Read FExecutable Write FExecutable;
procedure Lape_TProcess_Executable_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PProcess(Params^[0])^.Executable;
end;

//Write: Property Executable : String Read FExecutable Write FExecutable;
procedure Lape_TProcess_Executable_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.Executable := PlpString(Params^[1])^;
end;

//Read: Property Parameters : TStrings Read FParameters Write SetParameters;
procedure Lape_TProcess_Parameters_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Result)^ := PProcess(Params^[0])^.Parameters;
end;

//Write: Property Parameters : TStrings Read FParameters Write SetParameters;
procedure Lape_TProcess_Parameters_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.Parameters := PStrings(Params^[1])^;
end;

//Read: Property ConsoleTitle : String Read FConsoleTitle Write FConsoleTitle;
procedure Lape_TProcess_ConsoleTitle_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PProcess(Params^[0])^.ConsoleTitle;
end;

//Write: Property ConsoleTitle : String Read FConsoleTitle Write FConsoleTitle;
procedure Lape_TProcess_ConsoleTitle_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.ConsoleTitle := PlpString(Params^[1])^;
end;

//Read: Property CurrentDirectory : String Read FCurrentDirectory Write FCurrentDirectory;
procedure Lape_TProcess_CurrentDirectory_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PProcess(Params^[0])^.CurrentDirectory;
end;

//Write: Property CurrentDirectory : String Read FCurrentDirectory Write FCurrentDirectory;
procedure Lape_TProcess_CurrentDirectory_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.CurrentDirectory := PlpString(Params^[1])^;
end;

//Read: Property Desktop : String Read FDesktop Write FDesktop;
procedure Lape_TProcess_Desktop_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PProcess(Params^[0])^.Desktop;
end;

//Write: Property Desktop : String Read FDesktop Write FDesktop;
procedure Lape_TProcess_Desktop_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.Desktop := PlpString(Params^[1])^;
end;

//Read: Property Environment : TStrings Read FEnvironment Write SetEnvironment;
procedure Lape_TProcess_Environment_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Result)^ := PProcess(Params^[0])^.Environment;
end;

//Write: Property Environment : TStrings Read FEnvironment Write SetEnvironment;
procedure Lape_TProcess_Environment_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.Environment := PStrings(Params^[1])^;
end;

//Read: Property Options : TProcessOptions Read FProcessOptions Write SetProcessOptions;
procedure Lape_TProcess_Options_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcessOptions(Result)^ := PProcess(Params^[0])^.Options;
end;

//Write: Property Options : TProcessOptions Read FProcessOptions Write SetProcessOptions;
procedure Lape_TProcess_Options_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.Options := PProcessOptions(Params^[1])^;
end;

//Read: Property Priority : TProcessPriority Read FProcessPriority Write FProcessPriority;
procedure Lape_TProcess_Priority_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcessPriority(Result)^ := PProcess(Params^[0])^.Priority;
end;

//Write: Property Priority : TProcessPriority Read FProcessPriority Write FProcessPriority;
procedure Lape_TProcess_Priority_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.Priority := PProcessPriority(Params^[1])^;
end;

//Read: Property StartupOptions : TStartupOptions Read FStartupOptions Write FStartupOptions;
procedure Lape_TProcess_StartupOptions_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStartupOptions(Result)^ := PProcess(Params^[0])^.StartupOptions;
end;

//Write: Property StartupOptions : TStartupOptions Read FStartupOptions Write FStartupOptions;
procedure Lape_TProcess_StartupOptions_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.StartupOptions := PStartupOptions(Params^[1])^;
end;

//Read: Property Running : Boolean Read GetRunning;
procedure Lape_TProcess_Running_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PProcess(Params^[0])^.Running;
end;

//Read: Property ShowWindow : TShowWindowOptions Read FShowWindow Write SetShowWindow;
procedure Lape_TProcess_ShowWindow_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PShowWindowOptions(Result)^ := PProcess(Params^[0])^.ShowWindow;
end;

//Write: Property ShowWindow : TShowWindowOptions Read FShowWindow Write SetShowWindow;
procedure Lape_TProcess_ShowWindow_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.ShowWindow := PShowWindowOptions(Params^[1])^;
end;

//Read: Property WindowColumns : Cardinal Read dwXCountChars Write SetWindowColumns;
procedure Lape_TProcess_WindowColumns_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCardinal(Result)^ := PProcess(Params^[0])^.WindowColumns;
end;

//Write: Property WindowColumns : Cardinal Read dwXCountChars Write SetWindowColumns;
procedure Lape_TProcess_WindowColumns_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.WindowColumns := PCardinal(Params^[1])^;
end;

//Read: Property WindowHeight : Cardinal Read dwYSize Write SetWindowHeight;
procedure Lape_TProcess_WindowHeight_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCardinal(Result)^ := PProcess(Params^[0])^.WindowHeight;
end;

//Write: Property WindowHeight : Cardinal Read dwYSize Write SetWindowHeight;
procedure Lape_TProcess_WindowHeight_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.WindowHeight := PCardinal(Params^[1])^;
end;

//Read: Property WindowLeft : Cardinal Read dwX Write SetWindowLeft;
procedure Lape_TProcess_WindowLeft_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCardinal(Result)^ := PProcess(Params^[0])^.WindowLeft;
end;

//Write: Property WindowLeft : Cardinal Read dwX Write SetWindowLeft;
procedure Lape_TProcess_WindowLeft_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.WindowLeft := PCardinal(Params^[1])^;
end;

//Read: Property WindowRows : Cardinal Read dwYCountChars Write SetWindowRows;
procedure Lape_TProcess_WindowRows_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCardinal(Result)^ := PProcess(Params^[0])^.WindowRows;
end;

//Write: Property WindowRows : Cardinal Read dwYCountChars Write SetWindowRows;
procedure Lape_TProcess_WindowRows_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.WindowRows := PCardinal(Params^[1])^;
end;

//Read: Property WindowTop : Cardinal Read dwY Write SetWindowTop ;
procedure Lape_TProcess_WindowTop_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCardinal(Result)^ := PProcess(Params^[0])^.WindowTop;
end;

//Write: Property WindowTop : Cardinal Read dwY Write SetWindowTop ;
procedure Lape_TProcess_WindowTop_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.WindowTop := PCardinal(Params^[1])^;
end;

//Read: Property WindowWidth : Cardinal Read dwXSize Write SetWindowWidth;
procedure Lape_TProcess_WindowWidth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCardinal(Result)^ := PProcess(Params^[0])^.WindowWidth;
end;

//Write: Property WindowWidth : Cardinal Read dwXSize Write SetWindowWidth;
procedure Lape_TProcess_WindowWidth_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.WindowWidth := PCardinal(Params^[1])^;
end;

//Read: Property FillAttribute : Cardinal read FFillAttribute Write FFillAttribute;
procedure Lape_TProcess_FillAttribute_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCardinal(Result)^ := PProcess(Params^[0])^.FillAttribute;
end;

//Write: Property FillAttribute : Cardinal read FFillAttribute Write FFillAttribute;
procedure Lape_TProcess_FillAttribute_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.FillAttribute := PCardinal(Params^[1])^;
end;

//Read: Property XTermProgram : String Read FXTermProgram Write FXTermProgram;
procedure Lape_TProcess_XTermProgram_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PProcess(Params^[0])^.XTermProgram;
end;

//Write: Property XTermProgram : String Read FXTermProgram Write FXTermProgram;
procedure Lape_TProcess_XTermProgram_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.XTermProgram := PlpString(Params^[1])^;
end;

//constructor Create();
procedure Lape_TProcess_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^ := TProcess.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TProcess_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcess(Params^[0])^.Free();
end;

//Read: Property Input  : TOutputPipeStream Read FInputStream;
procedure Lape_TProcess_Input_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POutputPipeStream(Result)^ := PProcess(Params^[0])^.Input;
end;

//Read: Property Output : TInputPipeStream  Read FOutputStream;
procedure Lape_TProcess_Output_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInputPipeStream(Result)^ := PProcess(Params^[0])^.Output;
end;

//Read: Property Stderr : TinputPipeStream  Read FStderrStream;
procedure Lape_TProcess_Stderr_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PinputPipeStream(Result)^ := PProcess(Params^[0])^.Stderr;
end;

procedure Lape_Import_TProcess(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TProcess', 'TComponent');

    addGlobalFunc('procedure TProcess.Execute(); constref;', @Lape_TProcess_Execute);
    addGlobalFunc('procedure TProcess.CloseInput(); constref;', @Lape_TProcess_CloseInput);
    addGlobalFunc('procedure TProcess.CloseOutput(); constref;', @Lape_TProcess_CloseOutput);
    addGlobalFunc('procedure TProcess.CloseStderr(); constref;', @Lape_TProcess_CloseStderr);
    addGlobalFunc('function TProcess.Resume(): Integer; constref;', @Lape_TProcess_Resume);
    addGlobalFunc('function TProcess.Suspend(): Integer; constref;', @Lape_TProcess_Suspend);
    addGlobalFunc('function TProcess.Terminate(AExitCode : Integer): Boolean; constref;', @Lape_TProcess_Terminate);
    addGlobalFunc('function TProcess.WaitOnExit(): Boolean; constref;', @Lape_TProcess_WaitOnExit);
    addClassVar('TProcess', 'WindowRect', 'TRect', @Lape_TProcess_WindowRect_Read, @Lape_TProcess_WindowRect_Write);
    addClassVar('TProcess', 'Handle', 'THandle', @Lape_TProcess_Handle_Read, nil);
    addClassVar('TProcess', 'ProcessHandle', 'THandle', @Lape_TProcess_ProcessHandle_Read, nil);
    addClassVar('TProcess', 'ThreadHandle', 'THandle', @Lape_TProcess_ThreadHandle_Read, nil);
    addClassVar('TProcess', 'ProcessID', 'Integer', @Lape_TProcess_ProcessID_Read, nil);
    addClassVar('TProcess', 'ThreadID', 'Integer', @Lape_TProcess_ThreadID_Read, nil);
    addClassVar('TProcess', 'Input', 'TOutputPipeStream', @Lape_TProcess_Input_Read, nil);
    addClassVar('TProcess', 'Output', 'TInputPipeStream', @Lape_TProcess_Output_Read, nil);
    addClassVar('TProcess', 'Stderr', 'TinputPipeStream', @Lape_TProcess_Stderr_Read, nil);
    addClassVar('TProcess', 'ExitStatus', 'Integer', @Lape_TProcess_ExitStatus_Read, nil);
    addClassVar('TProcess', 'InheritHandles', 'Boolean', @Lape_TProcess_InheritHandles_Read, @Lape_TProcess_InheritHandles_Write);
    addClassVar('TProcess', 'PipeBufferSize', 'cardinal', @Lape_TProcess_PipeBufferSize_Read, @Lape_TProcess_PipeBufferSize_Write);
    addClassVar('TProcess', 'Active', 'Boolean', @Lape_TProcess_Active_Read, @Lape_TProcess_Active_Write);
    addClassVar('TProcess', 'ApplicationName', 'String', @Lape_TProcess_ApplicationName_Read, @Lape_TProcess_ApplicationName_Write);
    addClassVar('TProcess', 'CommandLine', 'String', @Lape_TProcess_CommandLine_Read, @Lape_TProcess_CommandLine_Write);
    addClassVar('TProcess', 'Executable', 'String', @Lape_TProcess_Executable_Read, @Lape_TProcess_Executable_Write);
    addClassVar('TProcess', 'Parameters', 'TStrings', @Lape_TProcess_Parameters_Read, @Lape_TProcess_Parameters_Write);
    addClassVar('TProcess', 'ConsoleTitle', 'String', @Lape_TProcess_ConsoleTitle_Read, @Lape_TProcess_ConsoleTitle_Write);
    addClassVar('TProcess', 'CurrentDirectory', 'String', @Lape_TProcess_CurrentDirectory_Read, @Lape_TProcess_CurrentDirectory_Write);
    addClassVar('TProcess', 'Desktop', 'String', @Lape_TProcess_Desktop_Read, @Lape_TProcess_Desktop_Write);
    addClassVar('TProcess', 'Environment', 'TStrings', @Lape_TProcess_Environment_Read, @Lape_TProcess_Environment_Write);
    addClassVar('TProcess', 'Options', 'TProcessOptions', @Lape_TProcess_Options_Read, @Lape_TProcess_Options_Write);
    addClassVar('TProcess', 'Priority', 'TProcessPriority', @Lape_TProcess_Priority_Read, @Lape_TProcess_Priority_Write);
    addClassVar('TProcess', 'StartupOptions', 'TStartupOptions', @Lape_TProcess_StartupOptions_Read, @Lape_TProcess_StartupOptions_Write);
    addClassVar('TProcess', 'Running', 'Boolean', @Lape_TProcess_Running_Read, nil);
    addClassVar('TProcess', 'ShowWindow', 'TShowWindowOptions', @Lape_TProcess_ShowWindow_Read, @Lape_TProcess_ShowWindow_Write);
    addClassVar('TProcess', 'WindowColumns', 'Cardinal', @Lape_TProcess_WindowColumns_Read, @Lape_TProcess_WindowColumns_Write);
    addClassVar('TProcess', 'WindowHeight', 'Cardinal', @Lape_TProcess_WindowHeight_Read, @Lape_TProcess_WindowHeight_Write);
    addClassVar('TProcess', 'WindowLeft', 'Cardinal', @Lape_TProcess_WindowLeft_Read, @Lape_TProcess_WindowLeft_Write);
    addClassVar('TProcess', 'WindowRows', 'Cardinal', @Lape_TProcess_WindowRows_Read, @Lape_TProcess_WindowRows_Write);
    addClassVar('TProcess', 'WindowTop', 'Cardinal', @Lape_TProcess_WindowTop_Read, @Lape_TProcess_WindowTop_Write);
    addClassVar('TProcess', 'WindowWidth', 'Cardinal', @Lape_TProcess_WindowWidth_Read, @Lape_TProcess_WindowWidth_Write);
    addClassVar('TProcess', 'FillAttribute', 'Cardinal', @Lape_TProcess_FillAttribute_Read, @Lape_TProcess_FillAttribute_Write);
    addClassVar('TProcess', 'XTermProgram', 'String', @Lape_TProcess_XTermProgram_Read, @Lape_TProcess_XTermProgram_Write);
    addGlobalFunc('procedure TProcess.Init(AOwner : TComponent); overrride;', @Lape_TProcess_Init);
    //addGlobalFunc('procedure TProcess.Free(); constref;', @Lape_TProcess_Free);
  end;
end;

procedure Lape_Import_LCLProcess(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
   begin
     addGlobalType('(poRunSuspended,poWaitOnExit, poUsePipes,poStderrToOutPut, poNoConsole,poNewConsole, poDefaultErrorMode,poNewProcessGroup, poDebugProcess,poDebugOnlyThisProcess)', 'TProcessOption');
     addGlobalType('(swoNone,swoHIDE,swoMaximize,swoMinimize,swoRestore,swoShow, swoShowDefault,swoShowMaximized,swoShowMinimized, swoshowMinNOActive,swoShowNA,swoShowNoActivate,swoShowNormal)', 'TShowWindowOptions');
     addGlobalType('(suoUseShowWindow,suoUseSize,suoUsePosition, suoUseCountChars,suoUseFillAttribute)', 'TStartupOption');
     addGlobalType('(ppHigh,ppIdle,ppNormal,ppRealTime)', 'TProcessPriority');
     addGlobalType('set of TProcessOption', 'TProcessOptions');
     addGlobalType('set of TStartupOption', 'TStartupOptions');
   end;

  Lape_Import_TInputPipeStream(Compiler);
  Lape_Import_TOutputPipeStream(Compiler);
  Lape_Import_TProcess(Compiler);
end;

end.

