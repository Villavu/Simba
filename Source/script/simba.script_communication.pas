{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.script_communication;

{$i simba.inc}

interface

uses
  classes, sysutils, pipes, extctrls, syncobjs,
  simba.bitmap;

type
  ESimbaScriptState = (STATE_PAUSED, STATE_STOP, STATE_RUNNING, STATE_NONE);

  PSimbaScriptDebuggerEvent = ^TSimbaScriptDebuggerEvent;
  TSimbaScriptDebuggerEvent = packed record
    Method: Int16;
    Depth: Int16;
    Exception: Boolean;
  end;
  TSimbaScriptDebuggerEvents = array of TSimbaScriptDebuggerEvent;

  TSimbaMethod = class
  protected
    procedure DoInvoke; virtual; abstract;
  public
    Params: TMemoryStream;
    Result: TMemoryStream;

    procedure Invoke; virtual;

    constructor Create;
    destructor Destroy; override;
  end;

  TSimbaMethod_Status = class(TSimbaMethod)
  protected
    procedure DoInvoke; override;
  public
    constructor Create(Status: ShortString); overload;
  end;

  TSimbaMethod_Disguse = class(TSimbaMethod)
  protected
    procedure DoInvoke; override;
  public
    constructor Create(Title: ShortString); overload;
  end;

  TSimbaMethod_ShowBalloonHint = class(TSimbaMethod)
  protected
    procedure DoInvoke; override;
  public
    constructor Create(Title, Hint: ShortString; Timeout: Int32; Flags: TBalloonFlags); overload;
  end;

  TSimbaMethod_GetSimbaPID = class(TSimbaMethod)
  protected
    procedure DoInvoke; override;
  public
    constructor Create; overload;
  end;

  TSimbaMethod_ClearDebug = class(TSimbaMethod)
  protected
    procedure DoInvoke; override;
  public
    constructor Create; overload;
  end;

  TSimbaMethod_GetSimbaTargetPID = class(TSimbaMethod)
  protected
    procedure DoInvoke; override;
  public
    constructor Create; overload;
  end;

  TSimbaMethod_GetSimbaTargetWindow = class(TSimbaMethod)
  protected
    procedure DoInvoke; override;
  public
    constructor Create; overload;
  end;

  TSimbaMethod_ScriptError = class(TSimbaMethod)
  protected
    procedure DoInvoke; override;
  public
    constructor Create(Line, Col: Int32; FileName: ShortString); overload;
  end;

  TSimbaMethod_ShowBitmap = class(TSimbaMethod)
  protected
    procedure DoInvoke; override;
  public
    constructor Create(Bitmap: TMufasaBitmap); overload;
  end;

  TSimbaMethod_ClearDebugImage = class(TSimbaMethod)
  protected
    procedure DoInvoke; override;
  public
    constructor Create; overload;
  end;

  TSimbaMethod_DisplayDebugImage = class(TSimbaMethod)
  protected
    procedure DoInvoke; override;
  public
    constructor Create(Width, Height: Int32); overload;
  end;

  TSimbaMethod_UpdateDebugImage = class(TSimbaMethod)
  protected
    procedure DoInvoke; override;
  public
    constructor Create(Bitmap: TMufasaBitmap); overload;
  end;

  TSimbaMethod_ScriptStateChanged = class(TSimbaMethod)
  protected
    procedure DoInvoke; override;
  public
    constructor Create(State: Int32); overload;
  end;

  TSimbaMethod_DebuggingMethod = class(TSimbaMethod)
  protected
    procedure DoInvoke; override;
  public
    procedure Invoke; override; // No need to synchronize

    constructor Create(Method: ShortString); overload;
  end;

  TSimbaMethod_DebuggerEvents = class(TSimbaMethod)
  protected
    procedure DoInvoke; override;
  public
    procedure Invoke; override; // No need to synchronize

    constructor Create(Events: TMemoryStream);
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
    TSimbaMethod_ScriptStateChanged,
    TSimbaMethod_DebuggingMethod,
    TSimbaMethod_DebuggerEvents
  );

type
  TSimbaCommunicationHeader = packed record
    Size: Int32;
    Method: String[60];
  end;

  TSimbaCommunicationServer = class
  protected
    FInputClient: TOutputPipeStream;
    FInputStream: TInputPipeStream;
    FOutputClient: TInputPipeStream;
    FOutputStream: TOutputPipeStream;

    FClient: String;
    FThread: TThread;
    FScript: TObject;

    procedure Execute;
  public
    property Client: String read FClient;

    constructor Create(Script: TObject);
    destructor Destroy; override;
  end;

  TSimbaCommunicationClient = class
  protected
    FInputStream: TInputPipeStream;
    FOutputStream: TOutputPipeStream;
    FLock: TCriticalSection;
  public
    procedure Invoke(Method: TSimbaMethod);

    constructor Create(Server: String);
    destructor Destroy; override;
  end;

implementation

uses
  math, forms,
  {$IFDEF WINDOWS}
  windows,
  {$ENDIF}
  simba.main, simba.outputform, simba.debugimageform,
  simba.scripttabsform, simba.scripttab, simba.mufasatypes, simba.scriptinstance;

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

procedure TSimbaMethod.Invoke;
begin
  TThread.Synchronize(TThread.CurrentThread, @DoInvoke);
end;

procedure TSimbaMethod_ScriptStateChanged.DoInvoke;
var
  State: ESimbaScriptState;
  Script: TSimbaScriptInstance;
begin
  Params.Read(State, SizeOf(ESimbaScriptState));
  Params.Read(Script, SizeOf(TSimbaScriptInstance));

  Script.State := State;
end;

constructor TSimbaMethod_ScriptStateChanged.Create(State: Int32);
begin
  inherited Create();

  Params.Write(State, SizeOf(Int32));
end;

procedure TSimbaMethod_UpdateDebugImage.DoInvoke;
var
  Width, Height: Int32;
begin
  Params.Read(Width, SizeOf(Int32));
  Params.Read(Height, SizeOf(Int32));

  SimbaDebugImageForm.ImageBox.Background.LoadFromPointer(PRGB32(Params.Memory + Params.Position), Width, Height);
  SimbaDebugImageForm.ImageBox.BackgroundChanged(False, False);
  SimbaDebugImageForm.ImageBox.Update();
end;

constructor TSimbaMethod_UpdateDebugImage.Create(Bitmap: TMufasaBitmap);
begin
  inherited Create();

  Params.Write(Bitmap.Width, SizeOf(Int32));
  Params.Write(Bitmap.Height, SizeOf(Int32));
  Params.Write(Bitmap.Data^, Bitmap.Width * Bitmap.Height * SizeOf(TRGB32));
end;

procedure TSimbaMethod_DisplayDebugImage.DoInvoke;
var
  Width, Height: Int32;
begin
  Params.Read(Width, SizeOf(Int32));
  Params.Read(Height, SizeOf(Int32));

  SimbaDebugImageForm.Display(Width, Height, True);
end;

constructor TSimbaMethod_DisplayDebugImage.Create(Width, Height: Int32);
begin
  inherited Create();

  Params.Write(Width, SizeOf(Int32));
  Params.Write(Height, SizeOf(Int32));
end;

procedure TSimbaMethod_ClearDebugImage.DoInvoke;
begin
  SimbaDebugImageForm.ImageBox.Background.Canvas.Brush.Color := 0;
  SimbaDebugImageForm.ImageBox.Background.Canvas.Clear();
  SimbaDebugImageForm.ImageBox.Update();
end;

constructor TSimbaMethod_ClearDebugImage.Create;
begin
  inherited Create();
end;

procedure TSimbaMethod_ShowBitmap.DoInvoke;
var
  Width, Height: Int32;
  DisplayWidth, DisplayHeight: Int32;
begin
  Params.Read(Width, SizeOf(Int32));
  Params.Read(Height, SizeOf(Int32));

  // Only resize debug image if imagebox is too small.
  DisplayWidth := SimbaDebugImageForm.ImageBox.Width;
  if (Width > SimbaDebugImageForm.ImageBox.Width) then
    DisplayWidth := Width;
  DisplayHeight:= SimbaDebugImageForm.ImageBox.Height;
  if (Height > SimbaDebugImageForm.ImageBox.Height + SimbaDebugImageForm.ImageBox.StatusBar.Height) then
    DisplayHeight := Height + SimbaDebugImageForm.ImageBox.StatusBar.Height;

  SimbaDebugImageForm.Display(DisplayWidth, DisplayHeight, True);
  SimbaDebugImageForm.ImageBox.Background.LoadFromPointer(PRGB32(Params.Memory + Params.Position), Width, Height);
  SimbaDebugImageForm.ImageBox.BackgroundChanged(False, False);
  SimbaDebugImageForm.ImageBox.Update();
end;

constructor TSimbaMethod_ShowBitmap.Create(Bitmap: TMufasaBitmap);
begin
  inherited Create();

  Params.Write(Bitmap.Width, SizeOf(Int32));
  Params.Write(Bitmap.Height, SizeOf(Int32));
  Params.Write(Bitmap.Data^, Bitmap.Width * Bitmap.Height * SizeOf(TRGB32));
end;

procedure TSimbaMethod_ScriptError.DoInvoke;
var
  Line, Col: Int32;
  FileName: ShortString;
  ScriptInstance: TSimbaScriptInstance;
  ScriptTab: TSimbaScriptTab;
begin
  Params.Read(Line, SizeOf(Int32));
  Params.Read(Col, SizeOf(Int32));
  Params.Read(FileName, SizeOf(ShortString));
  Params.Read(ScriptInstance, SizeOf(TSimbaScriptInstance));

  ScriptTab := SimbaScriptTabsForm.FindTab(ScriptInstance);
  if (ScriptTab <> nil) then
  begin
    if (ScriptTab.ScriptFileName = FileName) or ((ScriptTab.ScriptFileName = '') and (ScriptTab.ScriptTitle = FileName)) then
    begin
      ScriptTab.Show();
      ScriptTab.SetError(Line, Col);
    end else
    if SimbaScriptTabsForm.Open(FileName) then
      SimbaScriptTabsForm.CurrentTab.SetError(Line, Col);
  end;

  // Ensure error message is visible
  SimbaOutputForm.Editor.CaretX := 0;
  SimbaOutputForm.Editor.CaretY := SimbaOutputForm.Editor.Lines.Count;
end;

constructor TSimbaMethod_ScriptError.Create(Line, Col: Int32; FileName: ShortString);
begin
  inherited Create();

  Params.Write(Line, SizeOf(Int32));
  Params.Write(Col, SizeOf(Int32));
  Params.Write(FileName, SizeOf(ShortString));
end;

procedure TSimbaMethod_GetSimbaTargetWindow.DoInvoke;
begin
  Result.Write(SimbaForm.WindowSelection, SizeOf(PtrUInt));
end;

constructor TSimbaMethod_GetSimbaTargetWindow.Create;
begin
  inherited Create();
end;

procedure TSimbaMethod_GetSimbaTargetPID.DoInvoke;
begin
  Result.Write(SimbaForm.ProcessSelection, SizeOf(PtrUInt));
end;

constructor TSimbaMethod_GetSimbaTargetPID.Create;
begin
  inherited Create();
end;

procedure TSimbaMethod_ClearDebug.DoInvoke;
begin
  SimbaOutputForm.Clear();
end;

constructor TSimbaMethod_ClearDebug.Create;
begin
  inherited Create();
end;

procedure TSimbaMethod_GetSimbaPID.DoInvoke;
begin
  Result.Write(GetProcessID(), SizeOf(PtrUInt));
end;

constructor TSimbaMethod_GetSimbaPID.Create;
begin
  inherited Create();
end;

procedure TSimbaMethod_ShowBalloonHint.DoInvoke;
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

procedure TSimbaMethod_Status.DoInvoke;
var
  Status: ShortString;
begin
  Params.Read(Status, SizeOf(ShortString));

  SimbaForm.StatusPanelFileName.Caption := Status;
end;

constructor TSimbaMethod_Status.Create(Status: ShortString);
begin
  inherited Create();

  Params.Write(Status, SizeOf(ShortString));
end;

procedure TSimbaMethod_Disguse.DoInvoke;
var
  Title: ShortString;
begin
  Params.Read(Title, SizeOf(ShortString));

  Application.Title := Title;
end;

constructor TSimbaMethod_Disguse.Create(Title: ShortString);
begin
  inherited Create();

  Params.Write(Title, SizeOf(ShortString));
end;

procedure TSimbaMethod_DebuggingMethod.DoInvoke;
var
  Method: ShortString;
  Script: TSimbaScriptInstance;
begin
  Params.Read(Method, SizeOf(ShortString));
  Params.Read(Script, SizeOf(TSimbaScriptInstance));

  Script.DebuggerForm.AddMethod(Method);
end;

procedure TSimbaMethod_DebuggingMethod.Invoke;
begin
  DoInvoke();
end;

constructor TSimbaMethod_DebuggingMethod.Create(Method: ShortString);
begin
  inherited Create();

  Params.Write(Method, SizeOf(ShortString));
end;

procedure TSimbaMethod_DebuggerEvents.DoInvoke;
var
  Script: TSimbaScriptInstance;
begin
  Params.Position := Params.Size - SizeOf(TSimbaScriptInstance);
  Params.Read(Script, SizeOf(TSimbaScriptInstance));

  Script.DebuggerForm.AddEvents(Params.Memory, (Params.Size - SizeOf(TSimbaScriptInstance)) div SizeOf(TSimbaScriptDebuggerEvent));
end;

procedure TSimbaMethod_DebuggerEvents.Invoke;
begin
  DoInvoke();
end;

constructor TSimbaMethod_DebuggerEvents.Create(Events: TMemoryStream);
begin
  inherited Create();

  Params.Write(Events.Memory^, Events.Position);
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

procedure TSimbaCommunicationClient.Invoke(Method: TSimbaMethod);
var
  Header: TSimbaCommunicationHeader;
begin
  FLock.Enter();

  try
    // Send request
    Header.Size := Method.Params.Size;
    Header.Method := Method.ClassName;

    FOutputStream.Write(Header, SizeOf(TSimbaCommunicationHeader));
    FOutputStream.Write(Method.Params.Memory^, Method.Params.Size);

    // Read result
    FInputStream.Read(Header, SizeOf(TSimbaCommunicationHeader));

    if Header.Size > 0 then
    begin
      Method.Result.CopyFrom(FInputStream, Header.Size);
      Method.Result.Position := 0;
    end;
  finally
    FLock.Leave();
  end;
end;

constructor TSimbaCommunicationClient.Create(Server: String);
begin
  FLock := syncobjs.TCriticalSection.Create();

  FOutputStream := TOutputPipeStream.Create(StrToInt('$' + Server.SubString(0, 16)));
  FInputStream := TInputPipeStream.Create(StrToInt('$' + Server.SubString(16, 16)));
end;

destructor TSimbaCommunicationClient.Destroy;
begin
  FLock.Free();

  FOutputStream.Free();
  FInputStream.Free();

  inherited Destroy();
end;

constructor TSimbaCommunicationServer.Create(Script: TObject);
var
  InputHandle: THandle = 0;
  OutputHandle: THandle = 0;
begin
  FScript := Script;

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

procedure TSimbaCommunicationServer.Execute;

  function GetMethod(Name: ShortString): TSimbaMethod; inline;
  var
    I: Int32;
  begin
    for I := 0 to High(SimbaMethodClasses) do
      if SimbaMethodClasses[I].ClassName = Name then
        Exit(SimbaMethodClasses[I].Create());
  end;

var
  Header: TSimbaCommunicationHeader;
  Method: TSimbaMethod;
begin
  try
    while (FInputStream.Read(Header, SizeOf(TSimbaCommunicationHeader)) = SizeOf(TSimbaCommunicationHeader)) and (not TThread.CheckTerminated) do
    begin
      Method := GetMethod(Header.Method);

      // Copy parameters
      if (Header.Size > 0) then
        Method.Params.CopyFrom(FInputStream, Header.Size);

      // Append script instance
      Method.Params.Write(FScript, SizeOf(TObject));
      Method.Params.Position := 0;

      // Invoke
      Method.Invoke();

      // Write result header
      Header.Size := Method.Result.Size;
      Header.Method := Method.ClassName;

      FOutputStream.Write(Header, SizeOf(TSimbaCommunicationHeader));

      // Write result data
      if (Method.Result.Size > 0) then
        FOutputStream.Write(Method.Result.Memory^, Method.Result.Size);

      Method.Free();
    end;
  except
    on E: Exception do
      WriteLn('TSimbaCommunicationServer.Execute ' + E.Message);
  end;
end;

destructor TSimbaCommunicationServer.Destroy;
begin
  FThread.FreeOnTerminate := False;
  FThread.Terminate();

  while not FThread.Finished do
  begin
    try
      FInputClient.WriteByte(0); // Wake from Read()
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

