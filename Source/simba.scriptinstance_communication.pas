{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.scriptinstance_communication;

{$i simba.inc}

interface

uses
  classes, sysutils, forms, extctrls,
  simba.ipc, simba.mufasatypes;

type
  TSimbaScriptInstanceCommunication = class(TSimbaIPCServer)
  protected
    FScriptInstance: TObject;
    FMethods: array[ESimbaCommunicationMessage] of TThreadMethod;
    FThreadSafe: array[ESimbaCommunicationMessage] of Boolean;
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
  Message: ESimbaCommunicationMessage;
begin
  FParams := Params;
  FResult := Result;

  Message := ESimbaCommunicationMessage(MessageID);
  if FThreadSafe[Message] then
    FMethods[Message]()
  else
    TThread.Synchronize(TThread.CurrentThread, FMethods[Message]);
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

  SimbaDebugImageForm.ImageBox.Background.LoadFromPointer(FParams.Memory + FParams.Position, Width, Height);
  SimbaDebugImageForm.ImageBox.BackgroundChanged(False, False);
  SimbaDebugImageForm.ImageBox.Update();
end;

procedure TSimbaScriptInstanceCommunication.DebugImage_Show;
var
  Width, Height: Integer;
begin
  FParams.Read(Width, SizeOf(Integer));
  FParams.Read(Height, SizeOf(Integer));

  SimbaDebugImageForm.ImageBox.Background.LoadFromPointer(FParams.Memory + FParams.Position, Width, Height);
  SimbaDebugImageForm.ImageBox.BackgroundChanged(False, False);
  SimbaDebugImageForm.ImageBox.Update();

  SimbaDebugImageForm.SetSize(Width, Height);
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
  SimbaDebugImageForm.ImageBox.Background.Clear();
  SimbaDebugImageForm.ImageBox.BackgroundChanged(False, False);
  SimbaDebugImageForm.ImageBox.Update();
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
begin
  inherited Create();

  FScriptInstance := ScriptInstance;

  FMethods[ESimbaCommunicationMessage.STATUS]              := @Status;
  FMethods[ESimbaCommunicationMessage.DISGUSE]             := @Disguise;
  FMethods[ESimbaCommunicationMessage.SIMBA_PID]           := @GetSimbaPID;
  FMethods[ESimbaCommunicationMessage.SIMBA_TARGET_PID]    := @GetSimbaTargetPID;
  FMethods[ESimbaCommunicationMessage.SIMBA_TARGET_WINDOW] := @GetSimbaTargetWindow;
  FMethods[ESimbaCommunicationMessage.SCRIPT_ERROR]        := @ScriptError;
  FMethods[ESimbaCommunicationMessage.SCRIPT_STATE_CHANGE] := @ScriptStateChanged;
  FMethods[ESimbaCommunicationMessage.BALLOON_HINT]        := @ShowBalloonHint;
  FMethods[ESimbaCommunicationMessage.DEBUGIMAGE_MAXSIZE]  := @DebugImage_SetMaxSize;
  FMethods[ESimbaCommunicationMessage.DEBUGIMAGE_DRAW]     := @DebugImage_Draw;
  FMethods[ESimbaCommunicationMessage.DEBUGIMAGE_SHOW]     := @DebugImage_Show;
  FMethods[ESimbaCommunicationMessage.DEBUGIMAGE_HIDE]     := @DebugImage_Hide;
  FMethods[ESimbaCommunicationMessage.DEBUGIMAGE_ZOOM]     := @DebugImage_SetZoom;
  FMethods[ESimbaCommunicationMessage.DEBUGIMAGE_MOVETO]   := @DebugImage_MoveTo;
  FMethods[ESimbaCommunicationMessage.DEBUGIMAGE_DISPLAY]  := @DebugImage_Display;
  FMethods[ESimbaCommunicationMessage.DEBUGIMAGE_CLEAR]    := @DebugImage_Clear;

  FMethods[ESimbaCommunicationMessage.DEBUG_METHOD_NAME]   := @DebugMethodName;
  FMethods[ESimbaCommunicationMessage.DEBUG_EVENTS]        := @DebugEvents;

  FThreadSafe[ESimbaCommunicationMessage.DEBUG_METHOD_NAME] := True;
  FThreadSafe[ESimbaCommunicationMessage.DEBUG_EVENTS]      := True;
end;

end.

