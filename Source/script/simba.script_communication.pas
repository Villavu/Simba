{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.script_communication;

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.base, simba.image, simba.process, simba.ipc;

type
  TSimbaScriptCommunication = class(TSimbaIPCConnection)
  protected
    procedure HandleIncoming; override;
  public
    function GetScript(out Title: String): String;
    procedure SetSimbaTitle(S: String);

    procedure ShowTrayNotification(Title, Message: String; Timeout: Integer);

    function GetSimbaPID: TProcessID;
    function GetSimbaTargetWindow: TWindowHandle;
    function GetSimbaTargetPID: TProcessID;

    procedure ScriptStateChanged(State: ESimbaScriptState);
    procedure ScriptError(Message: String; Line, Column: Integer; FileName: String);

    procedure DebugImage_Update(Image: TSimbaImage; Resize, EnsureVisible: Boolean);
    procedure DebugImage_Display(Width, Height: Integer); overload;
    procedure DebugImage_Display(X, Y, Width, Height: Integer); overload;
    procedure DebugImage_SetMaxSize(Width, Height: Integer);
    procedure DebugImage_Close;

    procedure DebugMatrix_Update(Mat: TSingleMatrix; ColorMapType: Integer; Resize, EnsureVisible: Boolean);
  end;

implementation

uses
  simba.vartype_matrix;

// Simba sends the script nothing
procedure TSimbaScriptCommunication.HandleIncoming;
begin
  raise Exception.Create('Unknown message');
end;

function TSimbaScriptCommunication.GetScript(out Title: String): String;
begin
  Outgoing.BeginMessage('GetScript', 0);
  try
    Outgoing.Send();

    Outgoing.ReadString(Title);
    Outgoing.ReadString(Result);
  finally
    Outgoing.EndMessage();
  end;
end;

procedure TSimbaScriptCommunication.SetSimbaTitle(S: String);
begin
  Outgoing.BeginMessage('SetSimbaTitle', SizeOf(Int32) + Length(S));
  try
    Outgoing.WriteString(S);
    Outgoing.Send();
  finally
    Outgoing.EndMessage();
  end;
end;

procedure TSimbaScriptCommunication.ShowTrayNotification(Title, Message: String; Timeout: Integer);
begin
  Outgoing.BeginMessage('ShowTrayNotification', SizeOf(Int32) + Length(Title) + SizeOf(Int32) + Length(Message) + SizeOf(Int32));
  try
    Outgoing.WriteString(Title);
    Outgoing.WriteString(Message);
    Outgoing.WriteInteger(Timeout);
    Outgoing.Send();
  finally
    Outgoing.EndMessage();
  end;
end;

function TSimbaScriptCommunication.GetSimbaPID: TProcessID;
begin
  Outgoing.BeginMessage('GetSimbaPID', 0);
  try
    Outgoing.Send();

    Outgoing.ReadData(Result, SizeOf(TProcessID));
  finally
    Outgoing.EndMessage();
  end;
end;

function TSimbaScriptCommunication.GetSimbaTargetWindow: TWindowHandle;
begin
  Outgoing.BeginMessage('GetSimbaTargetWindow', 0);
  try
    Outgoing.Send();

    Outgoing.ReadData(Result, SizeOf(TWindowHandle));
  finally
    Outgoing.EndMessage();
  end;
end;

function TSimbaScriptCommunication.GetSimbaTargetPID: TProcessID;
begin
  Outgoing.BeginMessage('GetSimbaTargetPID', 0);
  try
    Outgoing.Send();

    Outgoing.ReadData(Result, SizeOf(TProcessID));
  finally
    Outgoing.EndMessage();
  end;
end;

procedure TSimbaScriptCommunication.ScriptStateChanged(State: ESimbaScriptState);
begin
  Outgoing.BeginMessage('ScriptStateChanged', SizeOf(ESimbaScriptState));
  try
    Outgoing.WriteData(State, SizeOf(ESimbaScriptState));
    Outgoing.Send();
  finally
    Outgoing.EndMessage();
  end;
end;

procedure TSimbaScriptCommunication.ScriptError(Message: String; Line, Column: Integer; FileName: String);
begin
  Outgoing.BeginMessage('ScriptError', SizeOf(Int32) + Length(Message) + SizeOf(Int32) + Length(FileName) + 2 * SizeOf(Int32));
  try
    Outgoing.WriteString(Message);
    Outgoing.WriteString(FileName);
    Outgoing.WriteInteger(Line);
    Outgoing.WriteInteger(Column);
    Outgoing.Send();
  finally
    Outgoing.EndMessage();
  end;
end;

procedure TSimbaScriptCommunication.DebugImage_Update(Image: TSimbaImage; Resize, EnsureVisible: Boolean);
begin
  if (Image = nil) or (Image.Width = 0) or (Image.Height = 0) then
    Exit;

  Outgoing.BeginMessage('DebugImage_Update', 2 * SizeOf(Int32) + 2 * SizeOf(Boolean) + Int64(Image.PixelCount * SizeOf(TColorBGRA)));
  try
    Outgoing.WriteInteger(Image.Width);
    Outgoing.WriteInteger(Image.Height);
    Outgoing.WriteBoolean(Resize);
    Outgoing.WriteBoolean(EnsureVisible);
    Outgoing.WriteData(Image.Data^, Int64(Image.PixelCount * SizeOf(TColorBGRA)));
    Outgoing.Send();
  finally
    Outgoing.EndMessage();
  end;
end;

procedure TSimbaScriptCommunication.DebugImage_Display(Width, Height: Integer);
begin
  Outgoing.BeginMessage('DebugImage_Display', 2 * SizeOf(Int32));
  try
    Outgoing.WriteInteger(Width);
    Outgoing.WriteInteger(Height);
    Outgoing.Send();
  finally
    Outgoing.EndMessage();
  end;
end;

procedure TSimbaScriptCommunication.DebugImage_Display(X, Y, Width, Height: Integer);
begin
  Outgoing.BeginMessage('DebugImage_DisplayXY', 4 * SizeOf(Int32));
  try
    Outgoing.WriteInteger(X);
    Outgoing.WriteInteger(Y);
    Outgoing.WriteInteger(Width);
    Outgoing.WriteInteger(Height);
    Outgoing.Send();
  finally
    Outgoing.EndMessage();
  end;
end;

procedure TSimbaScriptCommunication.DebugImage_SetMaxSize(Width, Height: Integer);
begin
  Outgoing.BeginMessage('DebugImage_SetMaxSize', 2 * SizeOf(Int32));
  try
    Outgoing.WriteInteger(Width);
    Outgoing.WriteInteger(Height);
    Outgoing.Send();
  finally
    Outgoing.EndMessage();
  end;
end;

procedure TSimbaScriptCommunication.DebugImage_Close;
begin
  Outgoing.SendMessage('DebugImage_Close');
end;

procedure TSimbaScriptCommunication.DebugMatrix_Update(Mat: TSingleMatrix; ColorMapType: Integer; Resize, EnsureVisible: Boolean);
var
  Width, Height, Y: Integer;
  Image: TSimbaImage;
begin
  if not Mat.GetSize(Width, Height) then
    Exit;
  for Y := 0 to Height - 1 do
    if (Length(Mat[Y]) <> Width) then
      SimbaException('DebugMatrix: every row must be the same length');

  // drawn here: Simba shows the image, and the values under the mouse
  Image := TSimbaImage.Create();
  try
    Image.FromMatrix(Mat, ColorMapType);

    Outgoing.BeginMessage('DebugMatrix_Update', 2 * SizeOf(Int32) + 2 * SizeOf(Boolean) + Int64(Width * Height * (SizeOf(Single) + SizeOf(TColorBGRA))));
    try
      Outgoing.WriteInteger(Width);
      Outgoing.WriteInteger(Height);
      Outgoing.WriteBoolean(Resize);
      Outgoing.WriteBoolean(EnsureVisible);
      for Y := 0 to Height - 1 do
        Outgoing.WriteData(Mat[Y, 0], Width * SizeOf(Single));
      Outgoing.WriteData(Image.Data^, Int64(Width * Height * SizeOf(TColorBGRA)));
      Outgoing.Send();
    finally
      Outgoing.EndMessage();
    end;
  finally
    Image.Free();
  end;
end;

end.

