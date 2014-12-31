unit lplclprocess;

{$mode objfpc}{$H+}
{$I Simba.inc}

interface

uses
  Classes, SysUtils, lpCompiler, lpTypes, lpClassHelper;

procedure RegisterLCLProcess(Compiler: TLapeCompiler);

implementation

uses
  lplclsystem, Process, Pipes;

type
  PProcess = ^TProcess;
  PRect = ^TRect;
  PHandle = ^THandle;
  PShowWindowOptions = ^TShowWindowOptions;
  PStartupOptions = ^TStartupOptions;
  PProcessOptions = ^TProcessOptions;
  PProcessPriority = ^TProcessPriority;
  POutputPipeStream = ^TOutputPipeStream;
  PInputPipeStream = ^TInputPipeStream;
  PInt64 = ^Int64;
  PLongInt = ^LongInt;

//function Seek(const Offset: int64; Origin: TSeekOrigin): int64; override;
procedure TOutputPipeStream_Seek(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pint64(Result)^ := POutputPipeStream(Params^[0])^.Seek(Pint64(Params^[1])^, PSeekOrigin(Params^[2])^);
end;

//Function Read (Var Buffer; Count : Longint) : longint; Override;
procedure TOutputPipeStream_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Plongint(Result)^ := POutputPipeStream(Params^[0])^.Read(PLongint(Params^[1])^, PLongint(Params^[2])^);
end;

//constructor Create();
procedure TOutputPipeStream_Init(const Params: PParamArray); lape_extdecl
begin
  POutputPipeStream(Params^[0])^ := TOutputPipeStream.Create(PHandle(Params^[1])^);
end;

//procedure Free();
procedure TOutputPipeStream_Free(const Params: PParamArray); lape_extdecl
begin
  POutputPipeStream(Params^[0])^.Free();
end;

procedure Register_TOutputPipeStream(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TOutputPipeStream', 'THandleStream');

    addGlobalFunc('function TOutputPipeStream.Seek(const Offset: int64; Origin: TSeekOrigin): int64;', @TOutputPipeStream_Seek);
    addGlobalFunc('function TOutputPipeStream.Read(Var Buffer; Count : Longint): longint;', @TOutputPipeStream_Read);
    addGlobalFunc('procedure TOutputPipeStream.Init(AHandle: THandle);', @TOutputPipeStream_Init);
    addGlobalFunc('procedure TOutputPipeStream.Free();', @TOutputPipeStream_Free);
  end;
end;

//Function Write (Const Buffer; Count : Longint) :Longint; Override;
procedure TInputPipeStream_Write(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PLongint(Result)^ := PInputPipeStream(Params^[0])^.Write(PLongint(Params^[1])^, PLongint(Params^[2])^);
end;

//function Seek(const Offset: int64; Origin: TSeekOrigin): int64; override;
procedure TInputPipeStream_Seek(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pint64(Result)^ := PInputPipeStream(Params^[0])^.Seek(Pint64(Params^[1])^, PSeekOrigin(Params^[2])^);
end;

//Function Read (Var Buffer; Count : Longint) : longint; Override;
procedure TInputPipeStream_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Plongint(Result)^ := PInputPipeStream(Params^[0])^.Read(PLongint(Params^[1])^, PLongint(Params^[2])^);
end;

//Read: property NumBytesAvailable: DWord read GetNumBytesAvailable;
procedure TInputPipeStream_NumBytesAvailable_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PDWord(Result)^ := PInputPipeStream(Params^[0])^.NumBytesAvailable;
end;

//constructor Create();
procedure TInputPipeStream_Init(const Params: PParamArray); lape_extdecl
begin
  PInputPipeStream(Params^[0])^ := TInputPipeStream.Create(PHandle(Params^[1])^);
end;

//procedure Free();
procedure TInputPipeStream_Free(const Params: PParamArray); lape_extdecl
begin
  PInputPipeStream(Params^[0])^.Free();
end;

procedure Register_TInputPipeStream(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TInputPipeStream', 'THandleStream');

    addGlobalFunc('function TInputPipeStream.Write(constref Buffer; Count : Longint): Longint;', @TInputPipeStream_Write);
    addGlobalFunc('function TInputPipeStream.Seek(const Offset: int64; Origin: TSeekOrigin): int64;', @TInputPipeStream_Seek);
    addGlobalFunc('function TInputPipeStream.Read(var Buffer; Count : Longint): longint;', @TInputPipeStream_Read);
    addClassVar('TInputPipeStream', 'NumBytesAvailable', 'DWord', @TInputPipeStream_NumBytesAvailable_Read, nil);
    addGlobalFunc('procedure TInputPipeStream.Init(AHandle: THandle);', @TInputPipeStream_Init);
    addGlobalFunc('procedure TInputPipeStream.Free();', @TInputPipeStream_Free);
  end;
end;

//Procedure Execute; virtual;
procedure TProcess_Execute(const Params: PParamArray); lape_extdecl
begin
  PProcess(Params^[0])^.Execute();
end;

//procedure CloseInput; virtual;
procedure TProcess_CloseInput(const Params: PParamArray); lape_extdecl
begin
  PProcess(Params^[0])^.CloseInput();
end;

//procedure CloseOutput; virtual;
procedure TProcess_CloseOutput(const Params: PParamArray); lape_extdecl
begin
  PProcess(Params^[0])^.CloseOutput();
end;

//procedure CloseStderr; virtual;
procedure TProcess_CloseStderr(const Params: PParamArray); lape_extdecl
begin
  PProcess(Params^[0])^.CloseStderr();
end;

//Function Resume : Integer; virtual;
procedure TProcess_Resume(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PProcess(Params^[0])^.Resume();
end;

//Function Suspend : Integer; virtual;
procedure TProcess_Suspend(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PProcess(Params^[0])^.Suspend();
end;

//Function Terminate (AExitCode : Integer): Boolean; virtual;
procedure TProcess_Terminate(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PProcess(Params^[0])^.Terminate(PInteger(Params^[1])^);
end;

//Function WaitOnExit : Boolean;
procedure TProcess_WaitOnExit(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PProcess(Params^[0])^.WaitOnExit();
end;

//Read: Property WindowRect : Trect Read GetWindowRect Write SetWindowRect;
procedure TProcess_WindowRect_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Prect(Result)^ := PProcess(Params^[0])^.WindowRect;
end;

//Write: Property WindowRect : Trect Read GetWindowRect Write SetWindowRect;
procedure TProcess_WindowRect_Write(const Params: PParamArray); lape_extdecl
begin
  PProcess(Params^[0])^.WindowRect := Prect(Params^[1])^;
end;

//Read: Property Handle : THandle Read FProcessHandle;
procedure TProcess_Handle_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PHandle(Result)^ := PProcess(Params^[0])^.Handle;
end;

//Read: Property ProcessHandle : THandle Read FProcessHandle;
procedure TProcess_ProcessHandle_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PHandle(Result)^ := PProcess(Params^[0])^.ProcessHandle;
end;

//Read: Property ThreadHandle : THandle Read FThreadHandle;
procedure TProcess_ThreadHandle_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PHandle(Result)^ := PProcess(Params^[0])^.ThreadHandle;
end;

//Read: Property ProcessID : Integer Read FProcessID;
procedure TProcess_ProcessID_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PProcess(Params^[0])^.ProcessID;
end;

//Read: Property ThreadID : Integer Read FThreadID;
procedure TProcess_ThreadID_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PProcess(Params^[0])^.ThreadID;
end;

//Read: Property ExitStatus : Integer Read GetExitStatus;
procedure TProcess_ExitStatus_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PProcess(Params^[0])^.ExitStatus;
end;

//Read: Property InheritHandles : Boolean Read FInheritHandles Write FInheritHandles;
procedure TProcess_InheritHandles_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PProcess(Params^[0])^.InheritHandles;
end;

//Write: Property InheritHandles : Boolean Read FInheritHandles Write FInheritHandles;
procedure TProcess_InheritHandles_Write(const Params: PParamArray); lape_extdecl
begin
  PProcess(Params^[0])^.InheritHandles := PBoolean(Params^[1])^;
end;

//Read: property PipeBufferSize : cardinal read FPipeBufferSize write FPipeBufferSize default 1024;
procedure TProcess_PipeBufferSize_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pcardinal(Result)^ := PProcess(Params^[0])^.PipeBufferSize;
end;

//Write: property PipeBufferSize : cardinal read FPipeBufferSize write FPipeBufferSize default 1024;
procedure TProcess_PipeBufferSize_Write(const Params: PParamArray); lape_extdecl
begin
  PProcess(Params^[0])^.PipeBufferSize := Pcardinal(Params^[1])^;
end;

//Read: Property Active : Boolean Read GetRunning Write SetActive;
procedure TProcess_Active_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PProcess(Params^[0])^.Active;
end;

//Write: Property Active : Boolean Read GetRunning Write SetActive;
procedure TProcess_Active_Write(const Params: PParamArray); lape_extdecl
begin
  PProcess(Params^[0])^.Active := PBoolean(Params^[1])^;
end;

//Read: Property ApplicationName : String Read FApplicationName Write SetApplicationName; deprecated;
procedure TProcess_ApplicationName_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PProcess(Params^[0])^.ApplicationName;
end;

//Write: Property ApplicationName : String Read FApplicationName Write SetApplicationName; deprecated;
procedure TProcess_ApplicationName_Write(const Params: PParamArray); lape_extdecl
begin
  PProcess(Params^[0])^.ApplicationName := PlpString(Params^[1])^;
end;

//Read: Property CommandLine : String Read FCommandLine Write SetCommandLine ; deprecated;
procedure TProcess_CommandLine_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PProcess(Params^[0])^.CommandLine;
end;

//Write: Property CommandLine : String Read FCommandLine Write SetCommandLine ; deprecated;
procedure TProcess_CommandLine_Write(const Params: PParamArray); lape_extdecl
begin
  PProcess(Params^[0])^.CommandLine := PlpString(Params^[1])^;
end;

//Read: Property Executable : String Read FExecutable Write FExecutable;
procedure TProcess_Executable_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PProcess(Params^[0])^.Executable;
end;

//Write: Property Executable : String Read FExecutable Write FExecutable;
procedure TProcess_Executable_Write(const Params: PParamArray); lape_extdecl
begin
  PProcess(Params^[0])^.Executable := PlpString(Params^[1])^;
end;

//Read: Property Parameters : TStrings Read FParameters Write SetParameters;
procedure TProcess_Parameters_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PStrings(Result)^ := PProcess(Params^[0])^.Parameters;
end;

//Write: Property Parameters : TStrings Read FParameters Write SetParameters;
procedure TProcess_Parameters_Write(const Params: PParamArray); lape_extdecl
begin
  PProcess(Params^[0])^.Parameters := PStrings(Params^[1])^;
end;

//Read: Property ConsoleTitle : String Read FConsoleTitle Write FConsoleTitle;
procedure TProcess_ConsoleTitle_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PProcess(Params^[0])^.ConsoleTitle;
end;

//Write: Property ConsoleTitle : String Read FConsoleTitle Write FConsoleTitle;
procedure TProcess_ConsoleTitle_Write(const Params: PParamArray); lape_extdecl
begin
  PProcess(Params^[0])^.ConsoleTitle := PlpString(Params^[1])^;
end;

//Read: Property CurrentDirectory : String Read FCurrentDirectory Write FCurrentDirectory;
procedure TProcess_CurrentDirectory_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PProcess(Params^[0])^.CurrentDirectory;
end;

//Write: Property CurrentDirectory : String Read FCurrentDirectory Write FCurrentDirectory;
procedure TProcess_CurrentDirectory_Write(const Params: PParamArray); lape_extdecl
begin
  PProcess(Params^[0])^.CurrentDirectory := PlpString(Params^[1])^;
end;

//Read: Property Desktop : String Read FDesktop Write FDesktop;
procedure TProcess_Desktop_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PProcess(Params^[0])^.Desktop;
end;

//Write: Property Desktop : String Read FDesktop Write FDesktop;
procedure TProcess_Desktop_Write(const Params: PParamArray); lape_extdecl
begin
  PProcess(Params^[0])^.Desktop := PlpString(Params^[1])^;
end;

//Read: Property Environment : TStrings Read FEnvironment Write SetEnvironment;
procedure TProcess_Environment_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PStrings(Result)^ := PProcess(Params^[0])^.Environment;
end;

//Write: Property Environment : TStrings Read FEnvironment Write SetEnvironment;
procedure TProcess_Environment_Write(const Params: PParamArray); lape_extdecl
begin
  PProcess(Params^[0])^.Environment := PStrings(Params^[1])^;
end;

//Read: Property Options : TProcessOptions Read FProcessOptions Write SetProcessOptions;
procedure TProcess_Options_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PProcessOptions(Result)^ := PProcess(Params^[0])^.Options;
end;

//Write: Property Options : TProcessOptions Read FProcessOptions Write SetProcessOptions;
procedure TProcess_Options_Write(const Params: PParamArray); lape_extdecl
begin
  PProcess(Params^[0])^.Options := PProcessOptions(Params^[1])^;
end;

//Read: Property Priority : TProcessPriority Read FProcessPriority Write FProcessPriority;
procedure TProcess_Priority_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PProcessPriority(Result)^ := PProcess(Params^[0])^.Priority;
end;

//Write: Property Priority : TProcessPriority Read FProcessPriority Write FProcessPriority;
procedure TProcess_Priority_Write(const Params: PParamArray); lape_extdecl
begin
  PProcess(Params^[0])^.Priority := PProcessPriority(Params^[1])^;
end;

//Read: Property StartupOptions : TStartupOptions Read FStartupOptions Write FStartupOptions;
procedure TProcess_StartupOptions_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PStartupOptions(Result)^ := PProcess(Params^[0])^.StartupOptions;
end;

//Write: Property StartupOptions : TStartupOptions Read FStartupOptions Write FStartupOptions;
procedure TProcess_StartupOptions_Write(const Params: PParamArray); lape_extdecl
begin
  PProcess(Params^[0])^.StartupOptions := PStartupOptions(Params^[1])^;
end;

//Read: Property Running : Boolean Read GetRunning;
procedure TProcess_Running_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PProcess(Params^[0])^.Running;
end;

//Read: Property ShowWindow : TShowWindowOptions Read FShowWindow Write SetShowWindow;
procedure TProcess_ShowWindow_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PShowWindowOptions(Result)^ := PProcess(Params^[0])^.ShowWindow;
end;

//Write: Property ShowWindow : TShowWindowOptions Read FShowWindow Write SetShowWindow;
procedure TProcess_ShowWindow_Write(const Params: PParamArray); lape_extdecl
begin
  PProcess(Params^[0])^.ShowWindow := PShowWindowOptions(Params^[1])^;
end;

//Read: Property WindowColumns : Cardinal Read dwXCountChars Write SetWindowColumns;
procedure TProcess_WindowColumns_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCardinal(Result)^ := PProcess(Params^[0])^.WindowColumns;
end;

//Write: Property WindowColumns : Cardinal Read dwXCountChars Write SetWindowColumns;
procedure TProcess_WindowColumns_Write(const Params: PParamArray); lape_extdecl
begin
  PProcess(Params^[0])^.WindowColumns := PCardinal(Params^[1])^;
end;

//Read: Property WindowHeight : Cardinal Read dwYSize Write SetWindowHeight;
procedure TProcess_WindowHeight_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCardinal(Result)^ := PProcess(Params^[0])^.WindowHeight;
end;

//Write: Property WindowHeight : Cardinal Read dwYSize Write SetWindowHeight;
procedure TProcess_WindowHeight_Write(const Params: PParamArray); lape_extdecl
begin
  PProcess(Params^[0])^.WindowHeight := PCardinal(Params^[1])^;
end;

//Read: Property WindowLeft : Cardinal Read dwX Write SetWindowLeft;
procedure TProcess_WindowLeft_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCardinal(Result)^ := PProcess(Params^[0])^.WindowLeft;
end;

//Write: Property WindowLeft : Cardinal Read dwX Write SetWindowLeft;
procedure TProcess_WindowLeft_Write(const Params: PParamArray); lape_extdecl
begin
  PProcess(Params^[0])^.WindowLeft := PCardinal(Params^[1])^;
end;

//Read: Property WindowRows : Cardinal Read dwYCountChars Write SetWindowRows;
procedure TProcess_WindowRows_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCardinal(Result)^ := PProcess(Params^[0])^.WindowRows;
end;

//Write: Property WindowRows : Cardinal Read dwYCountChars Write SetWindowRows;
procedure TProcess_WindowRows_Write(const Params: PParamArray); lape_extdecl
begin
  PProcess(Params^[0])^.WindowRows := PCardinal(Params^[1])^;
end;

//Read: Property WindowTop : Cardinal Read dwY Write SetWindowTop ;
procedure TProcess_WindowTop_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCardinal(Result)^ := PProcess(Params^[0])^.WindowTop;
end;

//Write: Property WindowTop : Cardinal Read dwY Write SetWindowTop ;
procedure TProcess_WindowTop_Write(const Params: PParamArray); lape_extdecl
begin
  PProcess(Params^[0])^.WindowTop := PCardinal(Params^[1])^;
end;

//Read: Property WindowWidth : Cardinal Read dwXSize Write SetWindowWidth;
procedure TProcess_WindowWidth_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCardinal(Result)^ := PProcess(Params^[0])^.WindowWidth;
end;

//Write: Property WindowWidth : Cardinal Read dwXSize Write SetWindowWidth;
procedure TProcess_WindowWidth_Write(const Params: PParamArray); lape_extdecl
begin
  PProcess(Params^[0])^.WindowWidth := PCardinal(Params^[1])^;
end;

//Read: Property FillAttribute : Cardinal read FFillAttribute Write FFillAttribute;
procedure TProcess_FillAttribute_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCardinal(Result)^ := PProcess(Params^[0])^.FillAttribute;
end;

//Write: Property FillAttribute : Cardinal read FFillAttribute Write FFillAttribute;
procedure TProcess_FillAttribute_Write(const Params: PParamArray); lape_extdecl
begin
  PProcess(Params^[0])^.FillAttribute := PCardinal(Params^[1])^;
end;

//Read: Property XTermProgram : String Read FXTermProgram Write FXTermProgram;
procedure TProcess_XTermProgram_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PProcess(Params^[0])^.XTermProgram;
end;

//Write: Property XTermProgram : String Read FXTermProgram Write FXTermProgram;
procedure TProcess_XTermProgram_Write(const Params: PParamArray); lape_extdecl
begin
  PProcess(Params^[0])^.XTermProgram := PlpString(Params^[1])^;
end;

//constructor Create();
procedure TProcess_Init(const Params: PParamArray); lape_extdecl
begin
  PProcess(Params^[0])^ := TProcess.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TProcess_Free(const Params: PParamArray); lape_extdecl
begin
  PProcess(Params^[0])^.Free();
end;

//Read: Property Input  : TOutputPipeStream Read FInputStream;
procedure TProcess_Input_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  POutputPipeStream(Result)^ := PProcess(Params^[0])^.Input;
end;

//Read: Property Output : TInputPipeStream  Read FOutputStream;
procedure TProcess_Output_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInputPipeStream(Result)^ := PProcess(Params^[0])^.Output;
end;

//Read: Property Stderr : TinputPipeStream  Read FStderrStream;
procedure TProcess_Stderr_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PinputPipeStream(Result)^ := PProcess(Params^[0])^.Stderr;
end;

procedure Register_TProcess(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TProcess', 'TComponent');

    addGlobalFunc('procedure TProcess.Execute();', @TProcess_Execute);
    addGlobalFunc('procedure TProcess.CloseInput();', @TProcess_CloseInput);
    addGlobalFunc('procedure TProcess.CloseOutput();', @TProcess_CloseOutput);
    addGlobalFunc('procedure TProcess.CloseStderr();', @TProcess_CloseStderr);
    addGlobalFunc('function TProcess.Resume(): Integer;', @TProcess_Resume);
    addGlobalFunc('function TProcess.Suspend(): Integer;', @TProcess_Suspend);
    addGlobalFunc('function TProcess.Terminate(AExitCode : Integer): Boolean;', @TProcess_Terminate);
    addGlobalFunc('function TProcess.WaitOnExit(): Boolean;', @TProcess_WaitOnExit);
    addClassVar('TProcess', 'WindowRect', 'TRect', @TProcess_WindowRect_Read, @TProcess_WindowRect_Write);
    addClassVar('TProcess', 'Handle', 'THandle', @TProcess_Handle_Read, nil);
    addClassVar('TProcess', 'ProcessHandle', 'THandle', @TProcess_ProcessHandle_Read, nil);
    addClassVar('TProcess', 'ThreadHandle', 'THandle', @TProcess_ThreadHandle_Read, nil);
    addClassVar('TProcess', 'ProcessID', 'Integer', @TProcess_ProcessID_Read, nil);
    addClassVar('TProcess', 'ThreadID', 'Integer', @TProcess_ThreadID_Read, nil);
    addClassVar('TProcess', 'Input', 'TOutputPipeStream', @TProcess_Input_Read, nil);
    addClassVar('TProcess', 'Output', 'TInputPipeStream', @TProcess_Output_Read, nil);
    addClassVar('TProcess', 'Stderr', 'TinputPipeStream', @TProcess_Stderr_Read, nil);
    addClassVar('TProcess', 'ExitStatus', 'Integer', @TProcess_ExitStatus_Read, nil);
    addClassVar('TProcess', 'InheritHandles', 'Boolean', @TProcess_InheritHandles_Read, @TProcess_InheritHandles_Write);
    addClassVar('TProcess', 'PipeBufferSize', 'cardinal', @TProcess_PipeBufferSize_Read, @TProcess_PipeBufferSize_Write);
    addClassVar('TProcess', 'Active', 'Boolean', @TProcess_Active_Read, @TProcess_Active_Write);
    addClassVar('TProcess', 'ApplicationName', 'String', @TProcess_ApplicationName_Read, @TProcess_ApplicationName_Write);
    addClassVar('TProcess', 'CommandLine', 'String', @TProcess_CommandLine_Read, @TProcess_CommandLine_Write);
    addClassVar('TProcess', 'Executable', 'String', @TProcess_Executable_Read, @TProcess_Executable_Write);
    addClassVar('TProcess', 'Parameters', 'TStrings', @TProcess_Parameters_Read, @TProcess_Parameters_Write);
    addClassVar('TProcess', 'ConsoleTitle', 'String', @TProcess_ConsoleTitle_Read, @TProcess_ConsoleTitle_Write);
    addClassVar('TProcess', 'CurrentDirectory', 'String', @TProcess_CurrentDirectory_Read, @TProcess_CurrentDirectory_Write);
    addClassVar('TProcess', 'Desktop', 'String', @TProcess_Desktop_Read, @TProcess_Desktop_Write);
    addClassVar('TProcess', 'Environment', 'TStrings', @TProcess_Environment_Read, @TProcess_Environment_Write);
    addClassVar('TProcess', 'Options', 'TProcessOptions', @TProcess_Options_Read, @TProcess_Options_Write);
    addClassVar('TProcess', 'Priority', 'TProcessPriority', @TProcess_Priority_Read, @TProcess_Priority_Write);
    addClassVar('TProcess', 'StartupOptions', 'TStartupOptions', @TProcess_StartupOptions_Read, @TProcess_StartupOptions_Write);
    addClassVar('TProcess', 'Running', 'Boolean', @TProcess_Running_Read, nil);
    addClassVar('TProcess', 'ShowWindow', 'TShowWindowOptions', @TProcess_ShowWindow_Read, @TProcess_ShowWindow_Write);
    addClassVar('TProcess', 'WindowColumns', 'Cardinal', @TProcess_WindowColumns_Read, @TProcess_WindowColumns_Write);
    addClassVar('TProcess', 'WindowHeight', 'Cardinal', @TProcess_WindowHeight_Read, @TProcess_WindowHeight_Write);
    addClassVar('TProcess', 'WindowLeft', 'Cardinal', @TProcess_WindowLeft_Read, @TProcess_WindowLeft_Write);
    addClassVar('TProcess', 'WindowRows', 'Cardinal', @TProcess_WindowRows_Read, @TProcess_WindowRows_Write);
    addClassVar('TProcess', 'WindowTop', 'Cardinal', @TProcess_WindowTop_Read, @TProcess_WindowTop_Write);
    addClassVar('TProcess', 'WindowWidth', 'Cardinal', @TProcess_WindowWidth_Read, @TProcess_WindowWidth_Write);
    addClassVar('TProcess', 'FillAttribute', 'Cardinal', @TProcess_FillAttribute_Read, @TProcess_FillAttribute_Write);
    addClassVar('TProcess', 'XTermProgram', 'String', @TProcess_XTermProgram_Read, @TProcess_XTermProgram_Write);
    addGlobalFunc('procedure TProcess.Init(AOwner : TComponent);', @TProcess_Init);
    addGlobalFunc('procedure TProcess.Free();', @TProcess_Free);
  end;
end;

procedure RegisterLCLProcess(Compiler: TLapeCompiler);
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

  Register_TInputPipeStream(Compiler);
  Register_TOutputPipeStream(Compiler);
  Register_TProcess(Compiler);
end;

end.

