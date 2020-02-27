unit simba.scriptinstance;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, process,
  simba.ipc, simba.script_common, simba.oswindow;

type
  TSimbaScriptProcess = class(TProcess)
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

  TSimbaScriptInstance = class
  protected
    FProcess: TSimbaScriptProcess;

    FStateServer: TSimbaIPC_Server;
    FOutputServer: TSimbaIPC_Server;
    FMethodServer: TSimbaIPC_Server;

    FStateThread: TThread;
    FOutputThread: TThread;
    FMethodThread: TThread;

    FManageOutput: Boolean;
    FOutput: TStringList;

    FState: ESimbaScriptState;
    FStartTime: UInt64;

    procedure RunStateServer;
    procedure RunOutputServer;
    procedure RunMethodServer;

    function GetTimeRunning: UInt64;
    function GetIsPaused: Boolean;
    function GetIsStopping: Boolean;
    function GetIsRunning: Boolean;
    function GetExitCode: Int32;

    procedure SetScript(Value: String);
    procedure SetScriptFile(Value: String);
    procedure SetScriptName(Value: String);
    procedure SetTargetWindow(Value: TOSWindow);
  public
    // Output
    property ManageOutput: Boolean read FManageOutput write FManageOutput; // Automatially output to debug box
    property Output: TStringList read FOutput write FOutput;

    // Parameters to pass to script
    property Script: String write SetScript;
    property ScriptFile: String write SetScriptFile;
    property ScriptName: String Write SetScriptName;
    property TargetWindow: TOSWindow write SetTargetWindow;

    // Stats
    property TimeRunning: UInt64 read GetTimeRunning;
    property ExitCode: Int32 read GetExitCode;

    // Get state
    property IsPaused: Boolean read GetIsPaused;
    property IsStopping: Boolean read GetIsStopping;
    property IsRunning: Boolean read GetIsRunning;

    // Start
    procedure Run;
    procedure Compile;
    procedure Dump;

    // Change the state
    procedure Resume;
    procedure Pause;
    procedure Stop;
    procedure Kill;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  {$IFDEF UNIX}
  baseunix,
  {$ENDIF}
  forms, dialogs,
  simba.script_simbamethod, simba.debugform, simba.settings;

constructor TSimbaScriptProcess.Create;
begin
  inherited Create(Application);
end;

destructor TSimbaScriptProcess.Destroy;
begin
  if Running then
    Terminate(0);

  inherited Destroy();
end;

function TSimbaScriptInstance.GetIsRunning: Boolean;
begin
  Result := FProcess.Running;
end;

function TSimbaScriptInstance.GetIsPaused: Boolean;
begin
  Result := FProcess.Running and (FState = SCRIPT_PAUSED);
end;

function TSimbaScriptInstance.GetIsStopping: Boolean;
begin
  Result := FProcess.Running and (FState = SCRIPT_STOPPING);
end;

procedure TSimbaScriptInstance.SetScriptFile(Value: String);
begin
  FProcess.Parameters.Add('--scriptfile=' + Value);
end;

procedure TSimbaScriptInstance.SetScriptName(Value: String);
begin
  FProcess.Parameters.Add('--scriptname=' + Value);
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

procedure TSimbaScriptInstance.SetTargetWindow(Value: TOSWindow);
begin
  FProcess.Parameters.Add('--target-window=' + IntToStr(Value));
end;

procedure TSimbaScriptInstance.RunMethodServer;
var
  Method: TSimbaMethod;
  Message: Int32;
begin
  Method.Script := Self;
  Method.Params := TMemoryStream.Create();
  Method.Result := TMemoryStream.Create();

  try
    while True do
    begin
      if not FMethodServer.ReadMessage(Message, Method.Params) then
        Break;

      case ESimbaMethod(Message) of
        SIMBA_METHOD_DEBUG_IMAGE:         TThread.Synchronize(nil, @Method._DebugImage);
        SIMBA_METHOD_DEBUG_IMAGE_DRAW:    TThread.Synchronize(nil, @Method._DebugImageDraw);
        SIMBA_METHOD_SCRIPT_ERROR:        TThread.Synchronize(nil, @Method._ScriptError);
        SIMBA_METHOD_CLEAR_DEBUG:         TThread.Synchronize(nil, @Method._ClearDebug);
        SIMBA_METHOD_DEBUG_IMAGE_CLEAR:   TThread.Synchronize(nil, @Method._DebugImageClear);
        SIMBA_METHOD_DEBUG_IMAGE_DISPLAY: TThread.Synchronize(nil, @Method._DebugImageDisplay);
        SIMBA_METHOD_GET_PID:             TThread.Synchronize(nil, @Method._GetPID);
        SIMBA_METHOD_DEBUG_IMAGE_GET:     TThread.Synchronize(nil, @Method._DebugImageGetImage);
        SIMBA_METHOD_BALLOON_HINT:        TThread.Synchronize(nil, @Method._ShowBalloonHint);
        SIMBA_METHOD_DISGUISE:            TThread.Synchronize(nil, @Method._Disguise);
        SIMBA_METHOD_STATUS:              TThread.Synchronize(nil, @Method._Status);
        else
          raise Exception.CreateFmt('Invalid method %d', [Method.Method]);
      end;

      FMethodServer.WriteMessage(Message, Method.Result);
    end;
  except
    on e: Exception do
      WriteLn('Method Server: ' + e.Message);
  end;

  Method.Params.Free();
  Method.Result.Free();

  FMethodServer.Free();
  FMethodServer := nil;
end;

function TSimbaScriptInstance.GetTimeRunning: UInt64;
begin
  Result := GetTickCount64() - FStartTime;
end;

procedure TSimbaScriptInstance.RunOutputServer;

  function Process(constref Output: String): String;
  var
    Start, Index: Int32;
  begin
    Index := 1;
    Start := 1;

    for Index := 1 to Length(Output) do
      if (Output[Index] = #0) then
      begin
        if (Index - Start > 0) then
          FOutput.Add(Copy(Output, Start, (Index - Start)));

        Start := Index + 1;
      end;

    Result := Copy(Output, Start, (Index - Start) + 1);
  end;

var
  Buffer: array[1..TSimbaIPC.BUFFER_SIZE] of Char;
  Remaining: String;
  Count: Int32;
begin
  Remaining := '';

  try
    while True do
    begin
      if FManageOutput then
        FOutput.Clear();

      Count := FOutputServer.Read(Buffer[1], Length(Buffer));
      if Count = PIPE_TERMINATED then
        Break;

      Remaining := Process(Remaining + Copy(Buffer, 1, Count));

      if FManageOutput then
        SimbaDebugForm.Add(FOutput);
    end;
  except
    on e: Exception do
      WriteLn('Output Server: ' + e.Message);
  end;

  FOutput.Free();

  FOutputServer.Free();
  FOutputServer := nil;
end;

procedure TSimbaScriptInstance.RunStateServer;
var
  Count: Int32;
begin
  try
    while True do
    begin
      Count := FStateServer.Read(FState, SizeOf(Int32));
      if Count = PIPE_TERMINATED then
        Break;
    end;
  except
    on E: Exception do
      WriteLn('State Server: ' + E.Message);
  end;

  FStateServer.Free();
  FStateServer := nil;
end;

function TSimbaScriptInstance.GetExitCode: Int32;
begin
  Result := FProcess.ExitCode;
end;

procedure TSimbaScriptInstance.Run;
begin
  FProcess.Parameters.Add('--run');
  FProcess.Execute();

  FState := SCRIPT_RUNNING;
  FStartTime := GetTickCount64();
end;

procedure TSimbaScriptInstance.Compile;
begin
  FProcess.Parameters.Add('--compile');
  FProcess.Execute();

  FState := SCRIPT_RUNNING;
  FStartTime := GetTickCount64();
end;

procedure TSimbaScriptInstance.Dump;
begin
  FProcess.Parameters.Add('--dump');
  FProcess.Options := FProcess.Options + [poWaitOnExit];
  FProcess.Execute();

  FState := SCRIPT_RUNNING;
  FStartTime := GetTickCount64();
end;

procedure TSimbaScriptInstance.Resume;
var
  Message: Int32 = Ord(SCRIPT_RUNNING);
begin
  FStateServer.Write(Message, SizeOf(Int32));
end;

procedure TSimbaScriptInstance.Pause;
var
  Message: Int32 = Ord(SCRIPT_PAUSED);
begin
  FStateServer.Write(Message, SizeOf(Int32));
end;

procedure TSimbaScriptInstance.Stop;
var
  Message: Int32 = Ord(SCRIPT_STOPPING);
begin
  FStateServer.Write(Message, SizeOf(Int32));
end;

constructor TSimbaScriptInstance.Create;
begin
  FMethodServer := TSimbaIPC_Server.Create();
  FOutputServer := TSimbaIPC_Server.Create();
  FStateServer := TSimbaIPC_Server.Create();

  FOutput := TStringList.Create();

  FProcess := TSimbaScriptProcess.Create();
  FProcess.InheritHandles := True;
  FProcess.CurrentDirectory := Application.Location;
  FProcess.Options := FProcess.Options + [poStderrToOutPut];

  FProcess.Parameters.Add('--method-client=%s', [FMethodServer.Client]);
  FProcess.Parameters.Add('--output-client=%s', [FOutputServer.Client]);
  FProcess.Parameters.Add('--state-client=%s', [FStateServer.Client]);

  FProcess.Parameters.Add('--apppath=%s', [Application.Location]);
  FProcess.Parameters.Add('--datapath=%s', [SimbaSettings.Environment.DataPath.Value]);
  FProcess.Parameters.Add('--includepath=%s', [SimbaSettings.Environment.IncludePath.Value]);
  FProcess.Parameters.Add('--pluginpath=%s', [SimbaSettings.Environment.PluginPath.Value]);
  FProcess.Parameters.Add('--fontpath=%s', [SimbaSettings.Environment.FontPath.Value]);
  FProcess.Parameters.Add('--scriptpath=%s', [SimbaSettings.Environment.ScriptPath.Value]);

  FProcess.Executable := SimbaSettings.Environment.ScriptExectuablePath.Value;
  if (not FileExists(FProcess.Executable)) then
    raise Exception.Create('SimbaScript exectuable not found: ' + FProcess.Executable);

  {$IFDEF UNIX}
  if fpchmod(FProcess.Executable, &755) <> 0 then //rwxr-xr-x
    raise Exception.Create('Unable to make SimbaScript exectuable');
  {$ENDIF}

  FMethodThread := TThread.ExecuteInThread(@RunMethodServer);
  FStateThread := TThread.ExecuteInThread(@RunStateServer);
  FOutputThread := TThread.ExecuteInThread(@RunOutputServer);
end;

procedure TSimbaScriptInstance.Kill;
begin
  if (FProcess <> nil) then
    FProcess.Terminate(0);
end;

destructor TSimbaScriptInstance.Destroy;
begin
  if (FProcess <> nil) then
  begin
    if FProcess.Running then
      FProcess.Terminate(0);

    FProcess.Free();
  end;

  if (FMethodServer <> nil) then FMethodServer.Terminate();
  if (FStateServer <> nil)  then FStateServer.Terminate();
  if (FOutputServer <> nil) then FOutputServer.Terminate();

  if (FOutputThread = nil) then // exception raised in create
    FOutput.Free();

  inherited Destroy();
end;

end.

