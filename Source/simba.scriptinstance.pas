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
  simba.mufasatypes, simba.scriptinstance_communication, simba.windowhandle, simba.outputform;

type
  TSimbaScriptError = record
    Message: String;
    FileName: String;
    Line: Integer;
    Column: Integer;
  end;

  TSimbaScriptInstance = class(TComponent)
  protected
  const
    EXIT_CODE_FORCE_STOP = 1000;
  protected
    FProcess: TProcess;

    FSimbaCommunication: TSimbaScriptInstanceCommunication;

    FTarget: TWindowHandle;

    FStartTime: UInt64;
    FScriptFile: String;

    FOutputBox: TSimbaOutputBox;
    FOutputThread: TThread;

    FState: ESimbaScriptState;
    FError: TSimbaScriptError;

    procedure Start(Args: array of String);

    procedure DoOutputThread;
    procedure DoOutputThreadTerminated(Sender: TObject);

    function GetTimeRunning: UInt64;

    procedure SetState(Value: ESimbaScriptState);
  public
    property Process: TProcess read FProcess;
    property State: ESimbaScriptState read FState write SetState;
    property OutputBox: TSimbaOutputBox read FOutputBox;
    property ScriptFile: String write FScriptFile;
    property Target: TWindowHandle write FTarget;
    property TimeRunning: UInt64 read GetTimeRunning;

    // Set an error to be processed when process ends
    property Error: TSimbaScriptError read FError write FError;

    // Start
    procedure Run;
    procedure Compile;

    // Change the running state
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
  forms, simba.scripttabsform, simba.scripttab, simba.ide_events, simba.threading, simba.datetime;

procedure TSimbaScriptInstance.DoOutputThread;
var
  ReadBuffer, RemainingBuffer: String;

  procedure EmptyProcessOutput;
  var
    Count: Integer;
  begin
    while (FProcess.Output.NumBytesAvailable > 0) do
    begin
      Count := FProcess.Output.Read(ReadBuffer[1], Length(ReadBuffer));
      if (Count > 0) then
        RemainingBuffer := FOutputBox.Add(RemainingBuffer + Copy(ReadBuffer, 1, Count));
    end;

    SimbaIDEEvents.CallOnScriptRunning(Self);
  end;

begin
  try
    SetLength(ReadBuffer, 8192);
    SetLength(RemainingBuffer, 0);

    while FProcess.Running do
    begin
      EmptyProcessOutput();

      Sleep(500);
    end;

    DebugLn('Script process[%d] terminated. Exit code: %d', [FProcess.ProcessID, FProcess.ExitCode]);

    EmptyProcessOutput();
  except
    on E: Exception do
      DebugLn('Script process[%d] output thread crashed: %s', [FProcess.ProcessID, E.Message]);
  end;
end;

procedure TSimbaScriptInstance.DoOutputThreadTerminated(Sender: TObject);
var
  ScriptTab: TSimbaScriptTab;
begin
  AssertMainThread('TSimbaScriptInstance.DoOutputThreadTerminated');
  if FProcess.Running then
    FProcess.Terminate(EXIT_CODE_FORCE_STOP);

  if (Owner is TSimbaScriptTab) then
  begin
    ScriptTab := TSimbaScriptTab(Owner);

    try
      SimbaOutputForm.LockTabChange();

      if (FProcess.ExitCode = EXIT_CODE_FORCE_STOP) then
      begin
        ScriptTab.Show();
        ScriptTab.OutputBox.AddLine([EDebugLn.FOCUS, EDebugLn.GREEN], 'Script was force terminated after ' + FormatMilliseconds(TimeRunning, '\[hh:mm:ss\].'));
      end;

      if (FError.Column <> 0) or (FError.Line <> 0) or (FError.Message <> '') or (FError.FileName <> '') then
      begin
        // Check error is not in a include
        if (SameFileName(ScriptTab.ScriptFileName, Error.FileName)) or ((ScriptTab.ScriptFileName = '') and (ScriptTab.ScriptTitle = Error.FileName)) then
        begin
          ScriptTab.Show();
          ScriptTab.Editor.FocusLine(Error.Line, Error.Column, $0000A5);
        end else
        // else, open the file and display.
        if SimbaScriptTabsForm.Open(Error.FileName) then
          SimbaScriptTabsForm.CurrentEditor.FocusLine(Error.Line, Error.Column, $0000A5);
      end;
    finally
      SimbaOutputForm.UnlockTabChange();
    end;
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

procedure TSimbaScriptInstance.SetState(Value: ESimbaScriptState);
begin
  FState := Value;

  SimbaIDEEvents.CallOnScriptStateChange(Self);
end;

procedure TSimbaScriptInstance.Start(Args: array of String);
begin
  FStartTime := GetTickCount64();

  FProcess.Parameters.Add('--simbacommunication=%s', [FSimbaCommunication.ClientID]);
  FProcess.Parameters.Add('--target=' + FTarget.AsString());
  FProcess.Parameters.AddStrings(Args);
  FProcess.Parameters.Add(FScriptFile);
  FProcess.Execute();

  FOutputThread := RunInThread(@DoOutputThread);
  FOutputThread.OnTerminate := @DoOutputThreadTerminated;

  State := ESimbaScriptState.STATE_RUNNING;
end;

procedure TSimbaScriptInstance.Run;
begin
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
    FProcess.Terminate(EXIT_CODE_FORCE_STOP)
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
  FProcess.Terminate(0);
end;

function TSimbaScriptInstance.IsActiveTab: Boolean;
begin
  Result := (Owner is TSimbaScriptTab) and TSimbaScriptTab(Owner).IsActiveTab();
end;

destructor TSimbaScriptInstance.Destroy;
begin
  State := ESimbaScriptState.STATE_NONE;

  if (FProcess <> nil) then
    FreeAndNil(FProcess);
  if (FSimbaCommunication <> nil) then
    FreeAndNil(FSimbaCommunication);

  inherited Destroy();
end;

end.

