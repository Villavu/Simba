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
  simba.mufasatypes, simba.scriptinstance_communication, simba.debuggerform, simba.windowhandle, simba.outputform;

type
  TSimbaScriptError = record
    Message: String;
    FileName: String;
    Line: Integer;
    Column: Integer;
  end;

  TSimbaScriptInstance = class(TComponent)
  protected
    FProcess: TProcess;

    FSimbaCommunication: TSimbaScriptInstanceCommunication;

    FTarget: TWindowHandle;

    FStartTime: UInt64;
    FScriptFile: String;
    //FScriptName: String;

    FOutputBox: TSimbaOutputBox;
    FOutputThread: TThread;

    FState: ESimbaScriptState;

    FDebuggingForm: TSimbaDebuggerForm;

    FErrorSet: Boolean;
    FError: TSimbaScriptError;

    procedure Start(Args: array of String);

    procedure DoOutputThread;
    procedure DoOutputThreadTerminated(Sender: TObject);

    function GetTimeRunning: UInt64;
    function GetExitCode: Int32;
    function GetPID: UInt32;

    procedure SetState(Value: ESimbaScriptState);
    procedure SetError(Value: TSimbaScriptError);
  public
    property DebuggerForm: TSimbaDebuggerForm read FDebuggingForm;
    property Process: TProcess read FProcess;
    property State: ESimbaScriptState read FState write SetState;
    property OutputBox: TSimbaOutputBox read FOutputBox;

    // Parameters to pass to script
    property ScriptFile: String write FScriptFile;
    property Target: TWindowHandle write FTarget;

    // Stats
    property TimeRunning: UInt64 read GetTimeRunning;
    property ExitCode: Int32 read GetExitCode;
    property PID: UInt32 read GetPID;
    property Error: TSimbaScriptError read FError write SetError;

    // Start
    procedure Run(DebuggingForm: TSimbaDebuggerForm = nil);
    procedure Compile;

    // Change the state
    procedure Resume;
    procedure Pause;
    procedure Stop;
    procedure Kill;

    function IsActiveTab: Boolean;

    constructor Create(AOwner: TComponent; AOutputBox: TSimbaOutputBox); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  forms, simba.scripttabsform, simba.scripttab, simba.ide_events;

procedure TSimbaScriptInstance.DoOutputThread;
var
  Buffer: array[1..4096] of Char;
  Remaining: String;
  Count: Integer;
begin
  try
    Remaining := '';

    repeat
      Count := FProcess.Output.Read(Buffer[1], Length(Buffer));

      if (Count > 0) then
      begin
        Remaining := FOutputBox.Add(Remaining + Copy(Buffer, 1, Count));
        if (Count < Length(Buffer)) then
          Sleep(500);
      end;
    until (Count = 0);
  except
    on E: Exception do
      DebugLn('Listener thread exception: ' + E.Message);
  end;
end;

procedure TSimbaScriptInstance.DoOutputThreadTerminated(Sender: TObject);
var
  ScriptTab: TSimbaScriptTab;
begin
  Assert(GetCurrentThreadID = MainThreadID);

  DebugLn('TSimbaScriptInstance.DoOutputThreadTerminated');

  if FProcess.Running then
    FProcess.Terminate(100);

  if FErrorSet then
  try
    SimbaOutputForm.LockTabChange();

    ScriptTab := SimbaScriptTabsForm.FindTab(Self);
    if (ScriptTab <> nil) then
    begin
      // Check error is not in a include
      if (SameFileName(ScriptTab.ScriptFileName, Error.FileName)) or ((ScriptTab.ScriptFileName = '') and (ScriptTab.ScriptTitle = Error.FileName)) then
      begin
        ScriptTab.Show();
        ScriptTab.Editor.FocusLine(Error.Line, Error.Column, $0000A5);
      end else
      if SimbaScriptTabsForm.Open(Error.FileName) then
        SimbaScriptTabsForm.CurrentEditor.FocusLine(Error.Line, Error.Column, $0000A5);
    end;
  finally
    SimbaOutputForm.UnlockTabChange();
  end;

  Self.Free();
end;

function TSimbaScriptInstance.GetTimeRunning: UInt64;
begin
  if (FStartTime = 0) then
    Result := 0
  else
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

procedure TSimbaScriptInstance.SetState(Value: ESimbaScriptState);
begin
  FState := Value;

  SimbaIDEEvents.CallOnScriptStateChange(Self);
end;

procedure TSimbaScriptInstance.SetError(Value: TSimbaScriptError);
begin
  FErrorSet := True;
  FError := Value;
end;

procedure TSimbaScriptInstance.Start(Args: array of String);
begin
  FStartTime := GetTickCount64();

  FProcess.Parameters.Add('--simbacommunication=%s', [FSimbaCommunication.ClientID]);
  FProcess.Parameters.Add('--target=' + FTarget.AsString());
  FProcess.Parameters.AddStrings(Args);
  FProcess.Parameters.Add(FScriptFile);
  FProcess.Execute();

  FOutputThread := TThread.ExecuteInThread(@DoOutputThread);
  FOutputThread.OnTerminate := @DoOutputThreadTerminated;

  State := ESimbaScriptState.STATE_RUNNING;
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
  FState := ESimbaScriptState.STATE_RUNNING;
  FProcess.Input.Write(FState, SizeOf(Int32));
end;

procedure TSimbaScriptInstance.Pause;
begin
  FState := ESimbaScriptState.STATE_PAUSED;
  FProcess.Input.Write(FState, SizeOf(Int32));
end;

procedure TSimbaScriptInstance.Stop;
begin
  if (FState = ESimbaScriptState.STATE_STOP) then
    FProcess.Terminate(0)
  else
  begin
    FState := ESimbaScriptState.STATE_STOP;
    FProcess.Input.Write(FState, SizeOf(Int32));
  end;
end;

constructor TSimbaScriptInstance.Create(AOwner: TComponent; AOutputBox: TSimbaOutputBox);
begin
  inherited Create(AOwner);

  FOutputBox := AOutputBox;
  FState := ESimbaScriptState.STATE_RUNNING;
  FSimbaCommunication := TSimbaScriptInstanceCommunication.Create(Self);

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

function TSimbaScriptInstance.IsActiveTab: Boolean;
begin
  Result := (Owner is TSimbaScriptTab) and TSimbaScriptTab(Owner).IsActiveTab();
end;

destructor TSimbaScriptInstance.Destroy;
begin
  DebugLn('TSimbaScriptInstance.Destroy');

  State := ESimbaScriptState.STATE_NONE;

  if (FProcess <> nil) then
    FreeAndNil(FProcess);
  if (FSimbaCommunication <> nil) then
    FreeAndNil(FSimbaCommunication);

  inherited Destroy();
end;

end.

