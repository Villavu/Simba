unit simba.scriptinstance;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, process,
  simba.script_communication, simba.debuggerform;

const
  OUTPUT_BUFFER_SIZE = 16 * 1024;

type
  TSimbaScriptInstance = class;

  TSimbaScriptProcess = class(TProcess)
  public
    OnExecute: TNotifyEvent;
    OnDestroy: TNotifyEvent;

    procedure Execute; override;

    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

  TSimbaScriptOutputThread = class(TThread)
  protected
    FScript: TSimbaScriptInstance;

    procedure Execute; override;
  public
    constructor Create(Script: TSimbaScriptInstance); reintroduce;
  end;

  TSimbaScriptInstance = class
  protected
    FProcess: TSimbaScriptProcess;

    FSimbaCommunication: TSimbaCommunicationServer;
    FOutputThread: TSimbaScriptOutputThread;

    FTarget: THandle;

    FStartTime: UInt64;
    FScript: String;
    FScriptFile: String;
    FScriptName: String;

    FState: ESimbaScriptState;

    FDebuggingForm: TSimbaDebuggerForm;

    procedure OnExecuteProcess(Sender: TObject);
    procedure OnDestroyProcess(Sender: TObject);

    function GetTimeRunning: UInt64;
    function GetExitCode: Int32;
    function GetPID: UInt32;

    procedure SetScript(Value: String);
  public
    property DebuggerForm: TSimbaDebuggerForm read FDebuggingForm;
    property Process: TSimbaScriptProcess read FProcess;
    property State: ESimbaScriptState read FState write FState;

    // Parameters to pass to script
    property Script: String write FScript;
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

    function IsPaused: Boolean;
    function IsStopping: Boolean;
    function IsRunning: Boolean;
    function IsFinished: Boolean;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  forms,
  simba.stringutil, simba.debugform, simba.settings;

procedure TSimbaScriptOutputThread.Execute;
var
  Output: TStringList;

  function Process(constref Data: String): String;
  var
    I: Int32;
    Lines: TStringArray;
  begin
    Result := '';

    Lines := Explode(LineEnding, Data);
    for I := 0 to High(Lines) - 1 do
      Output.Add(Lines[I]);

    // Pipe buffer is full, this is not a complete line.
    if not Data.EndsWith(LineEnding) then
      Exit(Lines[High(Lines)]);

    Output.Add(Lines[High(Lines)]);
  end;

var
  Buffer: array[1..OUTPUT_BUFFER_SIZE] of Char;
  Remaining: String;
  Count: Int32;
begin
  Output := TStringList.Create();

  Remaining := '';

  try
    while (not Terminated) or (FScript.Process.Output.NumBytesAvailable > 0) do
    begin
      while (FScript.Process.Output.NumBytesAvailable > 0) do
      begin
        Output.Clear();

        Count := FScript.Process.Output.Read(Buffer[1], Length(Buffer));
        if (Count = 0) then
          Break;

        Remaining := Process(Remaining + Copy(Buffer, 1, Count));

        if (Output.Count > 0) then
          SimbaDebugForm.Add(Output);
      end;

      Sleep(250);
    end;
  except
    on E: Exception do
      WriteLn('Output Thread Exception: ' + E.Message);
  end;

  if (Length(Remaining) > 0) then
    SimbaDebugForm.Add(Remaining);

  Output.Free();
end;

constructor TSimbaScriptOutputThread.Create(Script: TSimbaScriptInstance);
begin
  inherited Create(False, 512*512);

  FScript := Script;
end;

procedure TSimbaScriptProcess.Execute;
begin
  inherited Execute();

  if (OnExecute <> nil) then
    OnExecute(Self);
end;

constructor TSimbaScriptProcess.Create;
begin
  inherited Create(Application);
end;

destructor TSimbaScriptProcess.Destroy;
begin
  if Running then
    Terminate(0);

  if (OnDestroy <> nil) then
    OnDestroy(Self);

  inherited Destroy();
end;

procedure TSimbaScriptInstance.SetScript(Value: String);
var
  FileName: String;
begin
  FileName := GetTempFileName(SimbaSettings.Environment.DataPath.Value, '.script');

  with TStringList.Create() do
  try
    Text := Value;

    SaveToFile(FileName);
  finally
    Free();
  end;

  FProcess.Parameters.Add('--scriptfile=' + FileName);
end;

procedure TSimbaScriptInstance.OnExecuteProcess(Sender: TObject);
begin
  FOutputThread := TSimbaScriptOutputThread.Create(Self);
end;

procedure TSimbaScriptInstance.OnDestroyProcess(Sender: TObject);
begin
  if (FOutputThread <> nil) then
  begin
    FOutputThread.Terminate();
    FOutputThread.WaitFor();
    FOutputThread.Free();
  end;
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

procedure TSimbaScriptInstance.Run(DebuggingForm: TSimbaDebuggerForm);
begin
  if (FScript <> '') then
  begin
    FScriptFile := GetTempFileName(SimbaSettings.Environment.DataPath.Value, '.script');

    with TStringList.Create() do
    try
      Text := FScript;

      SaveToFile(FScriptFile);
    finally
      Free();
    end;

    FProcess.Parameters.Add('--scriptname=' + FScriptName);
  end;

  FDebuggingForm := DebuggingForm;

  if (FDebuggingForm <> nil) then
  begin
    FDebuggingForm.Clear();

    FProcess.Parameters.Add('--debugging');
  end;

  FProcess.Parameters.Add('--target=' + IntToStr(FTarget));
  FProcess.Parameters.Add('--run');
  FProcess.Parameters.Add(FScriptFile);
  FProcess.Execute();

  FStartTime := GetTickCount64();
end;

procedure TSimbaScriptInstance.Compile;
begin
  if (FScript <> '') then
  begin
    FScriptFile := GetTempFileName(SimbaSettings.Environment.DataPath.Value, '.script');

    with TStringList.Create() do
    try
      Text := FScript;

      SaveToFile(FScriptFile);
    finally
      Free();
    end;

    FProcess.Parameters.Add('--scriptname=' + FScriptName);
  end;

  FProcess.Parameters.Add('--target=' + IntToStr(FTarget));
  FProcess.Parameters.Add('--compile');
  FProcess.Parameters.Add(FScriptFile);
  FProcess.Execute();

  FStartTime := GetTickCount64();
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
  FState := STATE_STOP;
  FProcess.Input.Write(FState, SizeOf(Int32));
end;

constructor TSimbaScriptInstance.Create;
begin
  FState := STATE_RUNNING;
  FSimbaCommunication := TSimbaCommunicationServer.Create(Self);

  FProcess := TSimbaScriptProcess.Create();
  FProcess.PipeBufferSize := OUTPUT_BUFFER_SIZE;
  FProcess.OnExecute := @Self.OnExecuteProcess;
  FProcess.OnDestroy := @Self.OnDestroyProcess;
  FProcess.CurrentDirectory := Application.Location;
  FProcess.Options := FProcess.Options + [poUsePipes, poStderrToOutPut];

  FProcess.Parameters.Add('--simbacommunication=%s', [FSimbaCommunication.Client]);
  FProcess.Parameters.Add('--apppath=%s', [Application.Location]);
  FProcess.Parameters.Add('--datapath=%s', [SimbaSettings.Environment.DataPath.Value]);
  FProcess.Parameters.Add('--includepath=%s', [SimbaSettings.Environment.IncludePath.Value]);
  FProcess.Parameters.Add('--pluginpath=%s', [SimbaSettings.Environment.PluginPath.Value]);
  FProcess.Parameters.Add('--fontpath=%s', [SimbaSettings.Environment.FontPath.Value]);

  FProcess.Executable := Application.ExeName;
end;

procedure TSimbaScriptInstance.Kill;
begin
  if (FProcess <> nil) then
    FProcess.Terminate(0);
end;

function TSimbaScriptInstance.IsPaused: Boolean;
begin
  Result := FState = STATE_PAUSED;
end;

function TSimbaScriptInstance.IsStopping: Boolean;
begin
  Result := FState = STATE_STOP;
end;

function TSimbaScriptInstance.IsRunning: Boolean;
begin
  Result := FState = STATE_RUNNING;
end;

function TSimbaScriptInstance.IsFinished: Boolean;
begin
  Result := not FProcess.Running;
end;

destructor TSimbaScriptInstance.Destroy;
begin
  if (FSimbaCommunication <> nil) then
    FSimbaCommunication.Free();

  if (FProcess <> nil) then
  begin
    if FProcess.Running then
      FProcess.Terminate(0);

    FProcess.Free();
  end;

  inherited Destroy();
end;

end.

