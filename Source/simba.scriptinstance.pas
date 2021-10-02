{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.scriptinstance;

{$i simba.inc}

interface

uses
  classes, sysutils, process,
  simba.script_communication, simba.debuggerform;

type
  TSimbaScriptInstance = class(TComponent)
  protected
    FProcess: TProcess;

    FSimbaCommunication: TSimbaCommunicationServer;

    FTarget: THandle;

    FStartTime: UInt64;
    FScriptFile: String;
    FScriptName: String;

    FListen: TThread;

    FState: ESimbaScriptState;

    FDebuggingForm: TSimbaDebuggerForm;

    procedure Start(Args: array of String);

    procedure ManageProcess;

    function GetTimeRunning: UInt64;
    function GetExitCode: Int32;
    function GetPID: UInt32;
  public
    property DebuggerForm: TSimbaDebuggerForm read FDebuggingForm;
    property Process: TProcess read FProcess;
    property State: ESimbaScriptState read FState write FState;

    // Parameters to pass to script
    property ScriptName: String write FScriptName;
    property ScriptFile: String write FScriptFile;
    property Target: THandle write FTarget;

    // Stats
    property TimeRunning: UInt64 read GetTimeRunning;
    property ExitCode: Int32 read GetExitCode;
    property PID: UInt32 read GetPID;

    // Start
    procedure Run(DebuggingForm: TSimbaDebuggerForm = nil);
    procedure Compile;

    // Change the state
    procedure Resume;
    procedure Pause;
    procedure Stop;
    procedure Kill;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  forms, lazloggerbase,
  simba.outputform;

procedure TSimbaScriptInstance.ManageProcess;
begin
  FProcess.WaitOnExit();
  DebugLn('TSimbaScriptProcess.ManageProcess: Process %d terminated (%d)', [FProcess.ProcessID, FProcess.ExitCode]);
  Free();
end;

function TSimbaScriptInstance.GetTimeRunning: UInt64;
begin
  Result := GetTickCount64() - FStartTime;
end;

function TSimbaScriptInstance.GetExitCode: Int32;
begin
  Result := FProcess.ExitCode;
end;

function TSimbaScriptInstance.GetPID: UInt32;
begin
  Result := FProcess.ProcessID;
end;

procedure TSimbaScriptInstance.Start(Args: array of String);
begin
   if (FScriptName <> '') then
    FProcess.Parameters.Add('--scriptname=' + FScriptName);

  FProcess.Parameters.Add('--simbacommunication=%s', [FSimbaCommunication.Client]);
  FProcess.Parameters.Add('--target=' + IntToStr(FTarget));
  FProcess.Parameters.AddStrings(Args);
  FProcess.Parameters.Add(FScriptFile);
  FProcess.Execute();

  FListen := SimbaOutputForm.Listen(FProcess.Output);
  TThread.ExecuteInThread(@ManageProcess);

  FStartTime := GetTickCount64();
end;

procedure TSimbaScriptInstance.Run(DebuggingForm: TSimbaDebuggerForm);
begin
  if (DebuggingForm <> nil) then
  begin
    FDebuggingForm := DebuggingForm;

    Start(['--debugging', '--run']);
  end else
    Start(['--run']);
end;

procedure TSimbaScriptInstance.Compile;
begin
  Start(['--compile']);
end;

procedure TSimbaScriptInstance.Resume;
begin
  FState := STATE_RUNNING;
  FProcess.Input.Write(FState, SizeOf(Int32));
end;

procedure TSimbaScriptInstance.Pause;
begin
  FState := STATE_PAUSED;
  FProcess.Input.Write(FState, SizeOf(Int32));
end;

procedure TSimbaScriptInstance.Stop;
begin
  if (FState = STATE_STOP) then
    FProcess.Terminate(0)
  else
  begin
    FState := STATE_STOP;
    FProcess.Input.Write(FState, SizeOf(Int32));
  end;
end;

constructor TSimbaScriptInstance.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FState := STATE_RUNNING;
  FSimbaCommunication := TSimbaCommunicationServer.Create(Self);

  FProcess := TProcess.Create(nil);
  FProcess.PipeBufferSize := 16 * 1024;
  FProcess.CurrentDirectory := Application.Location;
  FProcess.Options := FProcess.Options + [poUsePipes, poStderrToOutPut];
  FProcess.Executable := Application.ExeName;
end;

procedure TSimbaScriptInstance.Kill;
begin
  DebugLn('TSimbaScriptInstance.Kill');

  FProcess.Terminate(0);
end;

destructor TSimbaScriptInstance.Destroy;
begin
  DebugLn('TSimbaScriptInstance.Destroy');

  if (FListen <> nil) then
    FreeAndNil(FListen);
  if (FProcess <> nil) then
    FreeAndNil(FProcess);
  if (FSimbaCommunication <> nil) then
    FreeAndNil(FSimbaCommunication);

  inherited Destroy();
end;

end.

