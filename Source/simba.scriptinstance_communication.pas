{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.scriptinstance_communication;

{$i simba.inc}

interface

uses
  classes, sysutils, forms, extctrls, typinfo, lazloggerbase,
  simba.ipc, simba.mufasatypes;

type
  TSimbaScriptInstanceCommunication = class(TSimbaIPCServer)
  protected
    FScriptInstance: TObject;
    FMethods: array[ESimbaCommunicationMessage] of record
      Method: TProc;
      ThreadSafe: Boolean;
    end;
    FParams: TMemoryStream;
    FResult: TMemoryStream;

    procedure OnMessage(MessageID: Integer; Params, Result: TMemoryStream); override;

    procedure Disguise;
    procedure Status;

    procedure ShowBalloonHint;

    procedure GetSimbaPID;
    procedure GetSimbaTargetWindow;
    procedure GetSimbaTargetPID;

    procedure ScriptStateChanged;
    procedure ScriptError;

    procedure DebugImage_SetZoom;
    procedure DebugImage_SetMaxSize;
    procedure DebugImage_MoveTo;
    procedure DebugImage_Draw;
    procedure DebugImage_Show;
    procedure DebugImage_Hide;
    procedure DebugImage_Display;
    procedure DebugImage_Clear;

    procedure DebugMethodName;
    procedure DebugEvents;
  public
    constructor Create(ScriptInstance: TObject); reintroduce;
  end;

implementation

uses
  simba.scriptinstance,
  simba.main, simba.debugimageform;

procedure TSimbaScriptInstanceCommunication.OnMessage(MessageID: Integer; Params, Result: TMemoryStream);
var
  Message: ESimbaCommunicationMessage absolute MessageID;

  procedure DoMethod;
  begin
    try
      FMethods[Message].Method();
    except
      on E: Exception do
        DebugLn('%s :: %s', [GetEnumName(TypeInfo(ESimbaCommunicationMessage), MessageID), E.ToString()]);
    end;
  end;

begin
  FParams := Params;
  FResult := Result;

  if FMethods[Message].ThreadSafe then
    DoMethod()
  else
    Sync(@DoMethod);
end;

procedure TSimbaScriptInstanceCommunication.Disguise;
begin
  SimbaForm.Caption := FParams.ReadAnsiString();
end;

procedure TSimbaScriptInstanceCommunication.Status;
begin
  SimbaForm.StatusPanelFileName.Caption := FParams.ReadAnsiString();
end;

procedure TSimbaScriptInstanceCommunication.ShowBalloonHint;
var
  Title, Hint: String;
  Timeout: Integer;
  Flags: TBalloonFlags;
begin
  Title := FParams.ReadAnsiString;
  Hint := FParams.ReadAnsiString;

  FParams.Read(Timeout, SizeOf(Integer));
  FParams.Read(Flags, SizeOf(TBalloonFlags));

  SimbaForm.TrayIcon.BalloonTitle := Title;
  SimbaForm.TrayIcon.BalloonHint := Hint;
  SimbaForm.TrayIcon.BalloonTimeout := Timeout;
  SimbaForm.TrayIcon.BalloonFlags := Flags;
  SimbaForm.TrayIcon.ShowBalloonHint();
end;

procedure TSimbaScriptInstanceCommunication.GetSimbaPID;
begin
  FResult.Write(GetProcessID(), SizeOf(PtrUInt));
end;

procedure TSimbaScriptInstanceCommunication.GetSimbaTargetWindow;
begin
  FResult.Write(SimbaForm.WindowSelection, SizeOf(PtrUInt));
end;

procedure TSimbaScriptInstanceCommunication.GetSimbaTargetPID;
begin
  FResult.Write(SimbaForm.ProcessSelection, SizeOf(PtrUInt));
end;

procedure TSimbaScriptInstanceCommunication.ScriptStateChanged;
var
  State: ESimbaScriptState;
begin
  FParams.Read(State, SizeOf(ESimbaScriptState));

  TSimbaScriptInstance(FScriptInstance).State := State;
end;

procedure TSimbaScriptInstanceCommunication.ScriptError;
var
  Error: TSimbaScriptError;
begin
  Error.Message  := FParams.ReadAnsiString();
  Error.FileName := FParams.ReadAnsiString();

  FParams.Read(Error.Line, SizeOf(Integer));
  FParams.Read(Error.Column, SizeOf(Integer));

  TSimbaScriptInstance(FScriptInstance).Error := Error;
end;

procedure TSimbaScriptInstanceCommunication.DebugImage_SetZoom;
var
  Level: Single;
begin
  FParams.Read(Level, SizeOf(Single));

  SimbaDebugImageForm.ImageBox.Zoom := Level;
end;

procedure TSimbaScriptInstanceCommunication.DebugImage_SetMaxSize;
var
  Width, Height: Integer;
begin
  FParams.Read(Width, SizeOf(Integer));
  FParams.Read(Height, SizeOf(Integer));

  SimbaDebugImageForm.SetMaxSize(Width, Height);
end;

procedure TSimbaScriptInstanceCommunication.DebugImage_MoveTo;
var
  X, Y: Integer;
begin
  FParams.Read(X, SizeOf(Integer));
  FParams.Read(Y, SizeOf(Integer));

  SimbaDebugImageForm.ImageBox.MoveTo(X, Y);
end;

procedure TSimbaScriptInstanceCommunication.DebugImage_Draw;
var
  Width, Height: Integer;
begin
  FParams.Read(Width, SizeOf(Integer));
  FParams.Read(Height, SizeOf(Integer));

  SimbaDebugImageForm.ImageBox.SetBackground(FParams.Memory + FParams.Position, Width, Height);
end;

procedure TSimbaScriptInstanceCommunication.DebugImage_Show;
var
  EnsureVisible: Boolean;
  Width, Height: Integer;
begin
  FParams.Read(EnsureVisible, SizeOf(Boolean));
  FParams.Read(Width, SizeOf(Integer));
  FParams.Read(Height, SizeOf(Integer));

  SimbaDebugImageForm.ImageBox.SetBackground(FParams.Memory + FParams.Position, Width, Height);
  SimbaDebugImageForm.SetSize(Width, Height, EnsureVisible);
end;

procedure TSimbaScriptInstanceCommunication.DebugImage_Hide;
begin
  SimbaDebugImageForm.Close();
end;

procedure TSimbaScriptInstanceCommunication.DebugImage_Display;
var
  Width, Height: Integer;
begin
  FParams.Read(Width, SizeOf(Integer));
  FParams.Read(Height, SizeOf(Integer));

  SimbaDebugImageForm.SetSize(Width, Height);
end;

procedure TSimbaScriptInstanceCommunication.DebugImage_Clear;
begin
  SimbaDebugImageForm.ImageBox.Background.Brush.Color := 0;
  SimbaDebugImageForm.ImageBox.Background.Clear();
end;

procedure TSimbaScriptInstanceCommunication.DebugMethodName;
begin
  TSimbaScriptInstance(FScriptInstance).DebuggerForm.AddMethod(FParams.ReadAnsiString());
end;

procedure TSimbaScriptInstanceCommunication.DebugEvents;
var
  Count: Integer;
begin
  FParams.Read(Count, SizeOf(Integer));

  TSimbaScriptInstance(FScriptInstance).DebuggerForm.AddEvents(FParams.Memory + FParams.Position, Count);
end;

constructor TSimbaScriptInstanceCommunication.Create(ScriptInstance: TObject);

  procedure SetMethod(Message: ESimbaCommunicationMessage; Proc: TProc; ThreadSafe: Boolean = False);
  begin
    FMethods[Message].Method     := Proc;
    FMethods[Message].ThreadSafe := Threadsafe;
  end;

begin
  inherited Create();

  FScriptInstance := ScriptInstance;

  SetMethod(ESimbaCommunicationMessage.STATUS,              @Status);
  SetMethod(ESimbaCommunicationMessage.DISGUSE,             @Disguise);
  SetMethod(ESimbaCommunicationMessage.SIMBA_PID,           @GetSimbaPID);
  SetMethod(ESimbaCommunicationMessage.SIMBA_TARGET_PID,    @GetSimbaTargetPID);
  SetMethod(ESimbaCommunicationMessage.SIMBA_TARGET_WINDOW, @GetSimbaTargetWindow);
  SetMethod(ESimbaCommunicationMessage.SCRIPT_ERROR,        @ScriptError);
  SetMethod(ESimbaCommunicationMessage.SCRIPT_STATE_CHANGE, @ScriptStateChanged);
  SetMethod(ESimbaCommunicationMessage.BALLOON_HINT,        @ShowBalloonHint);
  SetMethod(ESimbaCommunicationMessage.DEBUGIMAGE_MAXSIZE,  @DebugImage_SetMaxSize);
  SetMethod(ESimbaCommunicationMessage.DEBUGIMAGE_DRAW,     @DebugImage_Draw);
  SetMethod(ESimbaCommunicationMessage.DEBUGIMAGE_SHOW,     @DebugImage_Show);
  SetMethod(ESimbaCommunicationMessage.DEBUGIMAGE_HIDE,     @DebugImage_Hide);
  SetMethod(ESimbaCommunicationMessage.DEBUGIMAGE_ZOOM,     @DebugImage_SetZoom);
  SetMethod(ESimbaCommunicationMessage.DEBUGIMAGE_MOVETO,   @DebugImage_MoveTo);
  SetMethod(ESimbaCommunicationMessage.DEBUGIMAGE_DISPLAY,  @DebugImage_Display);
  SetMethod(ESimbaCommunicationMessage.DEBUGIMAGE_CLEAR,    @DebugImage_Clear);
  SetMethod(ESimbaCommunicationMessage.DEBUG_METHOD_NAME,   @DebugMethodName, True);
  SetMethod(ESimbaCommunicationMessage.DEBUG_EVENTS,        @DebugEvents,     True);
end;

end.

