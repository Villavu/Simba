{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Communication with a script process via pipes.
  IDE is the "server" and script is the "client"
}
unit simba.ide_scriptcommunication;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.ipc, simba.ide_tab;

type
  TSimbaScriptInstanceCommunication = class(TSimbaIPCConnection)
  protected
    FRunner: TSimbaScriptTabRunner;

    procedure HandleIncoming; override;

    procedure GetScript;
    procedure SetSimbaTitle;

    procedure ShowTrayNotification;

    procedure GetSimbaPID;
    procedure GetSimbaTargetWindow;
    procedure GetSimbaTargetPID;

    procedure ScriptStateChanged;
    procedure ScriptError;

    procedure DebugImage_Update;
    procedure DebugImage_Display;
    procedure DebugImage_DisplayXY;
    procedure DebugImage_SetMaxSize;
    procedure DebugImage_Close;

    procedure DebugMatrix_Update;
  public
    // AOwner is its TSimbaIPCProcess, which its TSimbaScriptTabRunner owns
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  simba.ide_controller,
  simba.ide_debugimage,
  simba.process,
  simba.threading,
  simba.vartype_matrix;

procedure TSimbaScriptInstanceCommunication.HandleIncoming;
begin
  case Incoming.Key of
    'GetScript':             GetScript();
    'SetSimbaTitle':         SetSimbaTitle();
    'ShowTrayNotification':  ShowTrayNotification();
    'GetSimbaPID':           GetSimbaPID();
    'GetSimbaTargetWindow':  GetSimbaTargetWindow();
    'GetSimbaTargetPID':     GetSimbaTargetPID();
    'ScriptStateChanged':    ScriptStateChanged();
    'ScriptError':           ScriptError();
    'DebugImage_Update':     DebugImage_Update();
    'DebugImage_Display':    DebugImage_Display();
    'DebugImage_DisplayXY':  DebugImage_DisplayXY();
    'DebugImage_SetMaxSize': DebugImage_SetMaxSize();
    'DebugImage_Close':      DebugImage_Close();
    'DebugMatrix_Update':    DebugMatrix_Update();
    else
      raise Exception.Create('Unknown message');
  end;
end;

procedure TSimbaScriptInstanceCommunication.GetScript;
var
  Title, Script: String;
begin
  Title := FRunner.ScriptTitle;
  Script := FRunner.Script;

  Incoming.BeginResponse(SizeOf(Int32) + Length(Title) + SizeOf(Int32) + Length(Script));
  Incoming.WriteString(Title);
  Incoming.WriteString(Script);
end;

procedure TSimbaScriptInstanceCommunication.SetSimbaTitle;
var
  Title: String;

  procedure Execute;
  begin
    SimbaController.SetWindowTitle(Title);
  end;

begin
  Incoming.ReadString(Title);

  RunInMainThread(@Execute);
end;

procedure TSimbaScriptInstanceCommunication.ShowTrayNotification;
var
  Title, Message: String;
  Timeout: Int32;

  procedure Execute;
  begin
    SimbaController.ShowTrayNotifaction(Title,  Message, Timeout);
  end;

begin
  Incoming.ReadString(Title);
  Incoming.ReadString(Message);
  Incoming.ReadInteger(Timeout);

  RunInMainThread(@Execute);
end;

// Threadsafe
procedure TSimbaScriptInstanceCommunication.GetSimbaPID;
var
  PID: TProcessID;
begin
  PID := GetProcessID();

  Incoming.BeginResponse(SizeOf(TProcessID));
  Incoming.WriteData(PID, SizeOf(TProcessID));
end;

// Threadsafe
procedure TSimbaScriptInstanceCommunication.GetSimbaTargetWindow;
var
  Window: TWindowHandle;
begin
  Window := SimbaController.WindowSelection;

  Incoming.BeginResponse(SizeOf(TWindowHandle));
  Incoming.WriteData(Window, SizeOf(TWindowHandle));
end;

// Threadsafe
procedure TSimbaScriptInstanceCommunication.GetSimbaTargetPID;
var
  PID: TProcessID;
begin
  PID := SimbaController.ProcessSelection;

  Incoming.BeginResponse(SizeOf(TProcessID));
  Incoming.WriteData(PID, SizeOf(TProcessID));
end;

procedure TSimbaScriptInstanceCommunication.ScriptStateChanged;
var
  State: ESimbaScriptState;

  procedure Execute;
  begin
    FRunner.State := State;
  end;

begin
  Incoming.ReadData(State, SizeOf(ESimbaScriptState));

  RunInMainThread(@Execute);
end;

procedure TSimbaScriptInstanceCommunication.ScriptError;
var
  Message, FileName: String;
  Line, Column: Int32;

  procedure Execute;
  begin
    FRunner.SetError(Message, FileName, Line, Column);
  end;

begin
  Incoming.ReadString(Message);
  Incoming.ReadString(FileName);
  Incoming.ReadInteger(Line);
  Incoming.ReadInteger(Column);

  RunInMainThread(@Execute);
end;

procedure TSimbaScriptInstanceCommunication.DebugImage_Update;
var
  Width, Height: Int32;
  Resize, EnsureVisible: Boolean;
begin
  Incoming.ReadInteger(Width);
  Incoming.ReadInteger(Height);
  Incoming.ReadBoolean(Resize);
  Incoming.ReadBoolean(EnsureVisible);

  SimbaDebugImageForm.BeginUpdate(Width, Height);
  try
    Incoming.ReadData(SimbaDebugImageForm.BackBuffer.Data^, Int64(Width * Height * SizeOf(TColorBGRA)));
  finally
    SimbaDebugImageForm.EndUpdate(Resize, EnsureVisible);
  end;
end;

procedure TSimbaScriptInstanceCommunication.DebugImage_Display;
var
  Width, Height: Int32;
begin
  Incoming.ReadInteger(Width);
  Incoming.ReadInteger(Height);

  SimbaDebugImageForm.Display(Width, Height);
end;

procedure TSimbaScriptInstanceCommunication.DebugImage_DisplayXY;
var
  X, Y, Width, Height: Int32;
begin
  Incoming.ReadInteger(X);
  Incoming.ReadInteger(Y);
  Incoming.ReadInteger(Width);
  Incoming.ReadInteger(Height);

  SimbaDebugImageForm.Display(X, Y, Width, Height);
end;

procedure TSimbaScriptInstanceCommunication.DebugImage_SetMaxSize;
var
  Width, Height: Int32;
begin
  Incoming.ReadInteger(Width);
  Incoming.ReadInteger(Height);

  SimbaDebugImageForm.SetMaxSize(Width, Height);
end;

procedure TSimbaScriptInstanceCommunication.DebugImage_Close;
begin
  SimbaDebugImageForm.Close();
end;

procedure TSimbaScriptInstanceCommunication.DebugMatrix_Update;
var
  Width, Height, Y: Int32;
  Resize, EnsureVisible: Boolean;
  Matrix: TSingleMatrix;
begin
  Incoming.ReadInteger(Width);
  Incoming.ReadInteger(Height);
  Incoming.ReadBoolean(Resize);
  Incoming.ReadBoolean(EnsureVisible);

  Matrix.SetSize(Width, Height);
  for Y := 0 to Height - 1 do
    Incoming.ReadData(Matrix[Y, 0], Width * SizeOf(Single));

  SimbaDebugMatrixForm.BeginUpdate(Width, Height);
  try
    SimbaDebugMatrixForm.Matrix := Matrix;

    Incoming.ReadData(SimbaDebugMatrixForm.BackBuffer.Data^, Int64(Width * Height * SizeOf(TColorBGRA)));
  finally
    SimbaDebugMatrixForm.EndUpdate(Resize, EnsureVisible);
  end;
end;

constructor TSimbaScriptInstanceCommunication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FRunner := AOwner.Owner as TSimbaScriptTabRunner;
end;

end.

