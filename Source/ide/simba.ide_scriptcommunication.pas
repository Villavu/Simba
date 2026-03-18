{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_scriptcommunication;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, ExtCtrls, Graphics,
  simba.base, simba.ide_ipc, simba.ide_tab;

type
  TSimbaScriptInstanceCommunication = class(TSimbaIPCServer)
  protected
    FRunner: TSimbaScriptTabRunner;
    FMethods: array[ESimbaCommunicationMessage] of TProcedureOfObject;
    FParams: TMemoryStream;
    FResult: TMemoryStream;

    procedure OnMessage(MessageID: Integer; Params, Result: TMemoryStream); override;

    procedure GetScript;
    procedure SetSimbaTitle;

    procedure ShowTrayNotification;

    procedure GetSimbaPID;
    procedure GetSimbaTargetWindow;
    procedure GetSimbaTargetPID;

    procedure ScriptStateChanged;
    procedure ScriptError;

    procedure DebugImage_SetMaxSize;
    procedure DebugImage_Update;
    procedure DebugImage_Hide;
    procedure DebugImage_Display;
    procedure DebugImage_DisplayXY;

    procedure DebugMatrix_Update;
  public
    constructor Create(Runner: TSimbaScriptTabRunner); reintroduce;
  end;

implementation

uses
  simba.form_main,
  simba.ide_debugimage,
  simba.ide_vars,
  simba.threading,
  simba.process;

procedure TSimbaScriptInstanceCommunication.OnMessage(MessageID: Integer; Params, Result: TMemoryStream);
var
  Message: ESimbaCommunicationMessage absolute MessageID;
begin
  FParams := Params;
  FResult := Result;

  FMethods[Message]();
end;

procedure TSimbaScriptInstanceCommunication.GetScript;
begin
  FResult.WriteAnsiString(FRunner.ScriptTitle);
  FResult.WriteAnsiString(FRunner.Script);
end;

procedure TSimbaScriptInstanceCommunication.SetSimbaTitle;

  procedure Execute;
  begin
    SimbaMainForm.Caption := FParams.ReadAnsiString();
  end;

begin
  RunInMainThread(@Execute);
end;

procedure TSimbaScriptInstanceCommunication.ShowTrayNotification;
var
  Title, Hint: String;
  Timeout: Integer;

  procedure Execute;
  begin
    SimbaMainForm.TrayIcon.BalloonTitle   := Title;
    SimbaMainForm.TrayIcon.BalloonHint    := Hint;
    SimbaMainForm.TrayIcon.BalloonTimeout := Timeout;
    SimbaMainForm.TrayIcon.ShowBalloonHint();
  end;

begin
  Title := FParams.ReadAnsiString;
  Hint  := FParams.ReadAnsiString;

  FParams.Read(Timeout, SizeOf(Integer));

  RunInMainThread(@Execute);
end;

// Threadsafe
procedure TSimbaScriptInstanceCommunication.GetSimbaPID;
begin
  FResult.Write(GetProcessID(), SizeOf(TProcessID));
end;

// Threadsafe
procedure TSimbaScriptInstanceCommunication.GetSimbaTargetWindow;
begin
  FResult.Write(SimbaIDEVars.WindowSelection, SizeOf(TWindowHandle));
end;

// Threadsafe
procedure TSimbaScriptInstanceCommunication.GetSimbaTargetPID;
begin
  FResult.Write(SimbaIDEVars.ProcessSelection, SizeOf(TProcessID));
end;

procedure TSimbaScriptInstanceCommunication.ScriptStateChanged;
var
  State: ESimbaScriptState;

  procedure Execute;
  begin
    FRunner.State := State;
  end;

begin
  FParams.Read(State, SizeOf(ESimbaScriptState));

  RunInMainThread(@Execute);
end;

procedure TSimbaScriptInstanceCommunication.ScriptError;
var
  Message, FileName: String;
  Line, Column: Integer;

  procedure Execute;
  begin
    FRunner.SetError(Message, FileName, Line, Column);
  end;

begin
  Message  := FParams.ReadAnsiString();
  FileName := FParams.ReadAnsiString();

  FParams.Read(Line, SizeOf(Integer));
  FParams.Read(Column, SizeOf(Integer));

  RunInMainThread(@Execute);
end;

procedure TSimbaScriptInstanceCommunication.DebugImage_SetMaxSize;
var
  Width, Height: Integer;

  procedure Execute;
  begin
    SimbaDebugImageForm.SetMaxSize(Width, Height);
  end;

begin
  FParams.Read(Width, SizeOf(Integer));
  FParams.Read(Height, SizeOf(Integer));

  RunInMainThread(@Execute);
end;

procedure TSimbaScriptInstanceCommunication.DebugImage_Update;
begin
  SimbaDebugImageForm.UpdateFromStream(FInputStream);
end;

procedure TSimbaScriptInstanceCommunication.DebugImage_Hide;

  procedure Execute;
  begin
    SimbaDebugImageForm.Close();
  end;

begin
  RunInMainThread(@Execute);
end;

procedure TSimbaScriptInstanceCommunication.DebugImage_Display;
var
  Width, Height: Integer;

  procedure Execute;
  begin
    SimbaDebugImageForm.SetSize(Width, Height, True);
  end;

begin
  FParams.Read(Width, SizeOf(Integer));
  FParams.Read(Height, SizeOf(Integer));

  RunInMainThread(@Execute);
end;

procedure TSimbaScriptInstanceCommunication.DebugImage_DisplayXY;
var
  X, Y, Width, Height: Integer;

  procedure Execute;
  begin
    SimbaDebugImageForm.Left := X;
    SimbaDebugImageForm.Top  := Y;
    SimbaDebugImageForm.SetSize(Width, Height, True);
  end;

begin
  FParams.Read(X, SizeOf(Integer));
  FParams.Read(Y, SizeOf(Integer));
  FParams.Read(Width, SizeOf(Integer));
  FParams.Read(Height, SizeOf(Integer));

  RunInMainThread(@Execute);
end;

procedure TSimbaScriptInstanceCommunication.DebugMatrix_Update;
begin
  SimbaDebugMatrixForm.UpdateFromStream(FInputStream);
end;

constructor TSimbaScriptInstanceCommunication.Create(Runner: TSimbaScriptTabRunner);
begin
  inherited Create(Runner);

  FRunner := Runner;

  FMethods[ESimbaCommunicationMessage.SCRIPT]                := @GetScript;
  FMethods[ESimbaCommunicationMessage.SIMBA_TITLE]           := @SetSimbaTitle;
  FMethods[ESimbaCommunicationMessage.SIMBA_PID]             := @GetSimbaPID;
  FMethods[ESimbaCommunicationMessage.SIMBA_TARGET_PID]      := @GetSimbaTargetPID;
  FMethods[ESimbaCommunicationMessage.SIMBA_TARGET_WINDOW]   := @GetSimbaTargetWindow;
  FMethods[ESimbaCommunicationMessage.SCRIPT_ERROR]          := @ScriptError;
  FMethods[ESimbaCommunicationMessage.SCRIPT_STATE_CHANGE]   := @ScriptStateChanged;
  FMethods[ESimbaCommunicationMessage.TRAY_NOTIFICATION]     := @ShowTrayNotification;
  FMethods[ESimbaCommunicationMessage.DEBUGIMAGE_MAXSIZE]    := @DebugImage_SetMaxSize;
  FMethods[ESimbaCommunicationMessage.DEBUGIMAGE_UPDATE]     := @DebugImage_Update;
  FMethods[ESimbaCommunicationMessage.DEBUGIMAGE_HIDE]       := @DebugImage_Hide;
  FMethods[ESimbaCommunicationMessage.DEBUGIMAGE_DISPLAY]    := @DebugImage_Display;
  FMethods[ESimbaCommunicationMessage.DEBUGIMAGE_DISPLAY_XY] := @DebugImage_DisplayXY;

  FMethods[ESimbaCommunicationMessage.DEBUGMATRIX_UPDATE]    := @DebugMatrix_Update;
end;

end.

