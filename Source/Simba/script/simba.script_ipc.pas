unit simba.script_ipc;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, pipes, extctrls,
  simba.bitmap;

type
  TSimbaScriptState = (STATE_PAUSED, STATE_STOP, STATE_RUNNING);

  TSimbaMethod = class
  public
    Params: TMemoryStream;
    Result: TMemoryStream;

    procedure Invoke; virtual; abstract;

    constructor Create;
    destructor Destroy; override;
  end;

  TSimbaMethod_Status = class(TSimbaMethod)
  public
    procedure Invoke; override;
    constructor Create(Status: ShortString); overload;
  end;

  TSimbaMethod_Disguse = class(TSimbaMethod)
  public
    procedure Invoke; override;
    constructor Create(Title: ShortString); overload;
  end;

  TSimbaMethod_ShowBalloonHint = class(TSimbaMethod)
  public
    procedure Invoke; override;
    constructor Create(Title, Hint: ShortString; Timeout: Int32; Flags: TBalloonFlags); overload;
  end;

  TSimbaMethod_GetSimbaPID = class(TSimbaMethod)
  public
    procedure Invoke; override;
    constructor Create; overload;
  end;

  TSimbaMethod_ClearDebug = class(TSimbaMethod)
  public
    procedure Invoke; override;
    constructor Create; overload;
  end;

  TSimbaMethod_GetSimbaTargetPID = class(TSimbaMethod)
  public
    procedure Invoke; override;
    constructor Create; overload;
  end;

  TSimbaMethod_GetSimbaTargetWindow = class(TSimbaMethod)
  public
    procedure Invoke; override;
    constructor Create; overload;
  end;

  TSimbaMethod_ScriptError = class(TSimbaMethod)
  public
    procedure Invoke; override;
    constructor Create(PID: SizeUInt; Line, Col: Int32; FileName: ShortString); overload;
  end;

  TSimbaMethod_ShowBitmap = class(TSimbaMethod)
  public
    procedure Invoke; override;
    constructor Create(Bitmap: TMufasaBitmap); overload;
  end;

  TSimbaMethod_ClearDebugImage = class(TSimbaMethod)
  public
    procedure Invoke; override;
    constructor Create; overload;
  end;

  TSimbaMethod_DisplayDebugImage = class(TSimbaMethod)
  public
    procedure Invoke; override;
    constructor Create(Width, Height: Int32); overload;
  end;

  TSimbaMethod_UpdateDebugImage = class(TSimbaMethod)
  public
    procedure Invoke; override;
    constructor Create(Bitmap: TMufasaBitmap); overload;
  end;

  TSimbaMethod_ScriptStateChanged = class(TSimbaMethod)
  public
    procedure Invoke; override;
    constructor Create(PID: SizeUInt; State: Int32); overload;
  end;

  TSimbaMethodClass = class of TSimbaMethod;
  TSimbaMethodClasses = array of TSimbaMethodClass;

const
  SimbaMethodClasses: TSimbaMethodClasses = (
    TSimbaMethod_Status,
    TSimbaMethod_Disguse,
    TSimbaMethod_ShowBalloonHint,
    TSimbaMethod_GetSimbaPID,
    TSimbaMethod_ClearDebug,
    TSimbaMethod_GetSimbaTargetPID,
    TSimbaMethod_GetSimbaTargetWindow,
    TSimbaMethod_ScriptError,
    TSimbaMethod_ShowBitmap,
    TSimbaMethod_ClearDebugImage,
    TSimbaMethod_DisplayDebugImage,
    TSimbaMethod_UpdateDebugImage,
    TSimbaMethod_ScriptStateChanged
  );

type
  TSimbaMethodServer = class
  protected
  type
    TMessageHeader = packed record
      Size: Int32;
      Method: String[60];
    end;
  protected
    FInputClient: TOutputPipeStream;
    FInputStream: TInputPipeStream;
    FOutputClient: TInputPipeStream;
    FOutputStream: TOutputPipeStream;

    FClient: String;
    FThread: TThread;

    procedure Execute;
  public
    property Client: String read FClient;

    constructor Create;
    destructor Destroy; override;
  end;

  TSimbaMethodClient = class
  protected
  type
    TMessageHeader = packed record
      Size: Int32;
      Method: String[60];
    end;
  protected
    FInputStream: TInputPipeStream;
    FOutputStream: TOutputPipeStream;
  public
    procedure Invoke(Method: TSimbaMethod);

    constructor Create(Server: String);
  end;

implementation

uses
  {$IFDEF WINDOWS}
  windows,
  {$ENDIF}
  forms, math,
  simba.main, simba.debugform, simba.debugimage,
  simba.scripttabsform, simba.mufasatypes;

function DuplicateHandle(Handle: THandle): THandle;
begin
  {$IFDEF WINDOWS}
  if not Windows.DuplicateHandle(GetCurrentProcess(), Handle, GetCurrentProcess(), @Result, 0, True, DUPLICATE_SAME_ACCESS) then
    raise Exception.Create('Unable to duplicate handle');

  CloseHandle(Handle);
  {$ELSE}
  Result := Handle; // Unix subprocesses inherit handles already
  {$ENDIF}
end;

procedure TSimbaMethod_ScriptStateChanged.Invoke;
var
  PID: SizeUInt;
  State: TSimbaScriptState;
  I: Int32;
begin
  Params.Read(PID, SizeOf(Int32));
  Params.Read(State, SizeOf(TSimbaScriptState));

  for I := 0 to SimbaScriptTabsForm.TabCount - 1 do
    if SimbaScriptTabsForm.Tabs[I].ScriptInstance.Process.ProcessID = PID then
      SimbaScriptTabsForm.Tabs[I].ScriptInstance.State := State;
end;

constructor TSimbaMethod_ScriptStateChanged.Create(PID: SizeUInt; State: Int32);
begin
  inherited Create();

  Params.Write(GetProcessID(), SizeOf(SizeUInt));
  Params.Write(State, SizeOf(Int32));
end;

procedure TSimbaMethod_UpdateDebugImage.Invoke;
var
  Width, Height: Int32;
begin
  Params.Read(Width, SizeOf(Int32));
  Params.Read(Height, SizeOf(Int32));

  SimbaDebugImageForm.ImageBox.Background.LoadFromPointer(PRGB32(Params.Memory + Params.Position), Width, Height);
  SimbaDebugImageForm.ImageBox.BackgroundChanged(False);
  SimbaDebugImageForm.ImageBox.Update();
end;

constructor TSimbaMethod_UpdateDebugImage.Create(Bitmap: TMufasaBitmap);
begin
  inherited Create();

  Params.Write(Bitmap.Width, SizeOf(Int32));
  Params.Write(Bitmap.Height, SizeOf(Int32));
  Params.Write(Bitmap.FData^, Bitmap.Width * Bitmap.Height * SizeOf(TRGB32));
end;

procedure TSimbaMethod_DisplayDebugImage.Invoke;
var
  Width, Height: Int32;
begin
  Params.Read(Width, SizeOf(Int32));
  Params.Read(Height, SizeOf(Int32));

  SimbaDebugImageForm.Resize(Width, Height, True);
end;

constructor TSimbaMethod_DisplayDebugImage.Create(Width, Height: Int32);
begin
  inherited Create();

  Params.Write(Width, SizeOf(Int32));
  Params.Write(Height, SizeOf(Int32));
end;

procedure TSimbaMethod_ClearDebugImage.Invoke;
begin
  SimbaDebugImageForm.ImageBox.Background.Canvas.Brush.Color := 0;
  SimbaDebugImageForm.ImageBox.Background.Canvas.Clear();
  SimbaDebugImageForm.ImageBox.Update();
end;

constructor TSimbaMethod_ClearDebugImage.Create;
begin
  inherited Create();
end;

procedure TSimbaMethod_ShowBitmap.Invoke;
var
  Width, Height: Int32;
begin;
  Params.Read(Width, SizeOf(Int32));
  Params.Read(Height, SizeOf(Int32));

  SimbaDebugImageForm.Resize(Max(Width, SimbaDebugImageForm.ImageBox.Background.Width), Max(Height, SimbaDebugImageForm.ImageBox.Background.Height), True);

  SimbaDebugImageForm.ImageBox.Background.LoadFromPointer(PRGB32(Params.Memory + Params.Position), Width, Height);
  SimbaDebugImageForm.ImageBox.BackgroundChanged(False);
  SimbaDebugImageForm.ImageBox.Update();
end;

constructor TSimbaMethod_ShowBitmap.Create(Bitmap: TMufasaBitmap);
begin
  inherited Create();

  Params.Write(Bitmap.Width, SizeOf(Int32));
  Params.Write(Bitmap.Height, SizeOf(Int32));
  Params.Write(Bitmap.FData^, Bitmap.Width * Bitmap.Height * SizeOf(TRGB32));
end;

procedure TSimbaMethod_ScriptError.Invoke;
var
  Line, Col: Int32;
  FileName: ShortString;
  PID: SizeUInt;
  I: Int32;
begin
  Params.Read(PID, SizeOf(SizeUInt));
  Params.Read(Line, SizeOf(Int32));
  Params.Read(Col, SizeOf(Int32));
  Params.Read(FileName, SizeOf(ShortString));

  for I := 0 to SimbaScriptTabsForm.TabCount - 1 do
  begin
    if SimbaScriptTabsForm.Tabs[I].ScriptInstance.Process.ProcessID = PID then
    begin
      if (SimbaScriptTabsForm.Tabs[I].ScriptName = FileName) then
        SimbaScriptTabsForm.Tabs[I].MakeVisible()
      else
      begin
        if not FileExists(FileName) then
          Exit;

        SimbaScriptTabsForm.Open(FileName);
      end;

      SimbaScriptTabsForm.CurrentTab.ScriptErrorLine := Line;
      SimbaScriptTabsForm.CurrentEditor.CaretX := Col;
      SimbaScriptTabsForm.CurrentEditor.CaretY := Line;

      SimbaDebugForm.Editor.TopLine := SimbaDebugForm.Editor.Lines.Count; // ensure error message is visible
    end;
  end;
end;

constructor TSimbaMethod_ScriptError.Create(PID: SizeUInt; Line, Col: Int32; FileName: ShortString);
begin
  inherited Create();

  Params.Write(PID, SizeOf(SizeUInt));
  Params.Write(Line, SizeOf(Int32));
  Params.Write(Col, SizeOf(Int32));
  Params.Write(FileName, SizeOf(ShortString));
end;

procedure TSimbaMethod_GetSimbaTargetWindow.Invoke;
begin
  Result.Write(SimbaForm.WindowSelection, SizeOf(PtrUInt));
end;

constructor TSimbaMethod_GetSimbaTargetWindow.Create;
begin
  inherited Create();
end;

procedure TSimbaMethod_GetSimbaTargetPID.Invoke;
begin
  Result.Write(SimbaForm.ProcessSelection, SizeOf(PtrUInt));
end;

constructor TSimbaMethod_GetSimbaTargetPID.Create;
begin
  inherited Create();
end;

procedure TSimbaMethod_ClearDebug.Invoke;
begin
  SimbaDebugForm.Clear();
end;

constructor TSimbaMethod_ClearDebug.Create;
begin
  inherited Create();
end;

procedure TSimbaMethod_GetSimbaPID.Invoke;
begin
  Result.Write(GetProcessID(), SizeOf(PtrUInt));
end;

constructor TSimbaMethod_GetSimbaPID.Create;
begin
  inherited Create();
end;

procedure TSimbaMethod_ShowBalloonHint.Invoke;
var
  Title, Hint: ShortString;
  Timeout: Int32;
  Flags: TBalloonFlags;
begin
  Params.Read(Title, SizeOf(ShortString));
  Params.Read(Hint, SizeOf(ShortString));
  Params.Read(Timeout, SizeOf(Int32));
  Params.Read(Flags, SizeOf(TBalloonFlags));

  SimbaForm.TrayIcon.BalloonTitle := Title;
  SimbaForm.TrayIcon.BalloonHint := Hint;
  SimbaForm.TrayIcon.BalloonTimeout := Timeout;
  SimbaForm.TrayIcon.BalloonFlags := Flags;
  SimbaForm.TrayIcon.ShowBalloonHint();
end;

constructor TSimbaMethod_ShowBalloonHint.Create(Title, Hint: ShortString; Timeout: Int32; Flags: TBalloonFlags);
begin
  inherited Create();

  Params.Write(Title, SizeOf(ShortString));
  Params.Write(Hint, SizeOf(ShortString));
  Params.Write(Timeout, SizeOf(Int32));
  Params.Write(Flags, SizeOf(TBalloonFlags));
end;

procedure TSimbaMethod_Status.Invoke;
var
  Status: ShortString;
begin
  Params.Read(Status, SizeOf(ShortString));

  SimbaScriptTabsForm.StatusPanelFileName.Caption := Status;
end;

constructor TSimbaMethod_Status.Create(Status: ShortString);
begin
  inherited Create();

  Params.Write(Status, SizeOf(ShortString));
end;

procedure TSimbaMethod_Disguse.Invoke;
var
  Title: ShortString;
begin
  Params.Read(Title, SizeOf(ShortString));

  SimbaForm.Title := Title;
end;

constructor TSimbaMethod_Disguse.Create(Title: ShortString);
begin
  inherited Create();

  Params.Write(Title, SizeOf(ShortString));
end;

constructor TSimbaMethod.Create;
begin
  Params := TMemoryStream.Create();
  Result := TMemoryStream.Create();
end;

destructor TSimbaMethod.Destroy;
begin
  Params.Free();
  Result.Free();

  inherited Destroy();
end;

procedure TSimbaMethodClient.Invoke(Method: TSimbaMethod);
var
  Header: TMessageHeader;
begin
  // Send request
  Header.Size := Method.Params.Size;
  Header.Method := Method.ClassName;

  FOutputStream.Write(Header, SizeOf(TMessageHeader));
  FOutputStream.Write(Method.Params.Memory^, Method.Params.Size);

  // Read result
  FInputStream.Read(Header, SizeOf(TMessageHeader));

  if Header.Size > 0 then
  begin
    Method.Result.CopyFrom(FInputStream, Header.Size);
    Method.Result.Position := 0;
  end;
end;

constructor TSimbaMethodClient.Create(Server: String);
begin
  FOutputStream := TOutputPipeStream.Create(StrToInt('$' + Server.SubString(0, 16)));
  FInputStream := TInputPipeStream.Create(StrToInt('$' + Server.SubString(16, 16)));
end;

constructor TSimbaMethodServer.Create;
var
  InputHandle: THandle = 0;
  OutputHandle: THandle = 0;
begin
  // Input
  if (not CreatePipeHandles(InputHandle, OutputHandle, 1024 * 64)) then
    raise Exception.Create('Unable to create input pipe');

  FInputStream := TInputPipeStream.Create(InputHandle);
  FInputClient := TOutputPipeStream.Create(DuplicateHandle(OutputHandle));

  // Output
  if (not CreatePipeHandles(InputHandle, OutputHandle, 1024 * 64)) then
    raise Exception.Create('Unable to create output pipe');

  FOutputStream := TOutputPipeStream.Create(OutputHandle);
  FOutputClient := TInputPipeStream.Create(DuplicateHandle(InputHandle));

  FClient := IntToHex(FInputClient.Handle, 16) + IntToHex(FOutputClient.Handle, 16);
  FThread := TThread.ExecuteInThread(@Execute);
end;

procedure TSimbaMethodServer.Execute;

  function GetMethod(Name: ShortString): TSimbaMethod;
  var
    I: Int32;
  begin
    for I := 0 to High(SimbaMethodClasses) do
      if SimbaMethodClasses[I].ClassName = Name then
        Exit(SimbaMethodClasses[I].Create());
  end;

var
  Header: TMessageHeader;
  Method: TSimbaMethod;
begin
  try
    while (FInputStream.Read(Header, SizeOf(TMessageHeader)) = SizeOf(TMessageHeader)) and (not FThread.CheckTerminated) do
    begin
      Method := GetMethod(Header.Method);

      // Copy parameters
      if (Header.Size > 0) then
      begin
        Method.Params.CopyFrom(FInputStream, Header.Size);
        Method.Params.Position := 0;
      end;

      TThread.Synchronize(TThread.CurrentThread, @Method.Invoke);

      // Write result header
      Header.Size := Method.Result.Size;
      Header.Method := Method.ClassName;

      FOutputStream.Write(Header, SizeOf(TMessageHeader));

      // Write result data
      if (Method.Result.Size > 0) then
        FOutputStream.Write(Method.Result.Memory^, Method.Result.Size);

      Method.Free();
    end;
  except
    on E: Exception do
      WriteLn('TSimbaMethodServer.Execute ' + E.Message);
  end;
end;

destructor TSimbaMethodServer.Destroy;
begin
  FThread.FreeOnTerminate := False;
  FThread.Terminate();

  while not FThread.Finished do
  begin
    try
      FInputClient.WriteByte(0); // Wake from read.
    except
    end;

    Sleep(50);
  end;

  FThread.Free();

  FInputStream.Free();
  FInputClient.Free();
  FOutputStream.Free();
  FOutputClient.Free();

  inherited Destroy();
end;

end.

