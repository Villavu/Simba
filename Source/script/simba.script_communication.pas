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
  simba.base, simba.image, simba.process, simba.ide_ipc;

type
  TSimbaScriptCommunication = class(TSimbaIPCClient)
  public
    function GetScript(out Name: String): String;

    function GetSimbaTargetWindow: TWindowHandle;
    function GetSimbaTargetPID: TProcessID;
    function GetSimbaPID: TProcessID;

    procedure ScriptError(Message: String; Line, Column: Integer; FileName: String);
    procedure ScriptStateChanged(State: ESimbaScriptState);

    procedure ShowTrayNotification(Title, Message: String; Timeout: Integer);

    procedure SetSimbaTitle(S: String);

    procedure DebugImage_SetMaxSize(Width, Height: Integer);
    procedure DebugImage_Show(Bitmap: TSimbaImage; EnsureVisible: Boolean);
    procedure DebugImage_Update(Bitmap: TSimbaImage);
    procedure DebugImage_Hide;
    procedure DebugImage_Display(Width, Height: Integer); overload;
    procedure DebugImage_Display(X, Y, Width, Height: Integer); overload;
  end;

implementation

function TSimbaScriptCommunication.GetScript(out Name: String): String;
begin
  BeginInvoke(Integer(ESimbaCommunicationMessage.SCRIPT));

  try
    Invoke();

    Name := FResult.ReadAnsiString();
    Result := FResult.ReadAnsiString();
  finally
    EndInvoke();
  end;
end;

function TSimbaScriptCommunication.GetSimbaTargetWindow: TWindowHandle;
begin
  BeginInvoke(Integer(ESimbaCommunicationMessage.SIMBA_TARGET_WINDOW));

  try
    Invoke();

    FResult.Read(Result, SizeOf(TWindowHandle));
  finally
    EndInvoke();
  end;
end;

function TSimbaScriptCommunication.GetSimbaTargetPID: TProcessID;
begin
  BeginInvoke(Integer(ESimbaCommunicationMessage.SIMBA_TARGET_PID));

  try
    Invoke();

    FResult.Read(Result, SizeOf(TProcessID));
  finally
    EndInvoke();
  end;
end;

function TSimbaScriptCommunication.GetSimbaPID: TProcessID;
begin
  BeginInvoke(Integer(ESimbaCommunicationMessage.SIMBA_PID));

  try
    Invoke();

    FResult.Read(Result, SizeOf(TProcessID));
  finally
    EndInvoke();
  end;
end;

procedure TSimbaScriptCommunication.ScriptError(Message: String; Line, Column: Integer; FileName: String);
begin
  BeginInvoke(Integer(ESimbaCommunicationMessage.SCRIPT_ERROR));

  try
    FParams.WriteAnsiString(Message);
    FParams.WriteAnsiString(FileName);
    FParams.Write(Line, SizeOf(Integer));
    FParams.Write(Column, SizeOf(Integer));

    Invoke();
  finally
    EndInvoke();
  end;
end;

procedure TSimbaScriptCommunication.ScriptStateChanged(State: ESimbaScriptState);
begin
  BeginInvoke(Integer(ESimbaCommunicationMessage.SCRIPT_STATE_CHANGE));

  try
    FParams.Write(State, SizeOf(ESimbaScriptState));

    Invoke();
  finally
    EndInvoke();
  end;
end;

procedure TSimbaScriptCommunication.ShowTrayNotification(Title, Message: String; Timeout: Integer);
begin
  BeginInvoke(Integer(ESimbaCommunicationMessage.TRAY_NOTIFICATION));

  try
    FParams.WriteAnsiString(Title);
    FParams.WriteAnsiString(Message);
    FParams.Write(Timeout, SizeOf(Integer));

    Invoke();
  finally
    EndInvoke();
  end;
end;

procedure TSimbaScriptCommunication.SetSimbaTitle(S: String);
begin
  BeginInvoke(Integer(ESimbaCommunicationMessage.SIMBA_TITLE));

  try
    FParams.WriteAnsiString(S);

    Invoke();
  finally
    EndInvoke();
  end;
end;

procedure TSimbaScriptCommunication.DebugImage_SetMaxSize(Width, Height: Integer);
begin
  BeginInvoke(Integer(ESimbaCommunicationMessage.DEBUGIMAGE_MAXSIZE));

  try
    FParams.Write(Width, SizeOf(Integer));
    FParams.Write(Height, SizeOf(Integer));

    Invoke();
  finally
    EndInvoke();
  end;
end;

// Send chunked ... faster & less memory needed.
procedure TSimbaScriptCommunication.DebugImage_Show(Bitmap: TSimbaImage; EnsureVisible: Boolean);
var
  Header: TSimbaIPCHeader;
  Y: Integer;
begin
  if (Bitmap = nil) or (Bitmap.Width = 0) or (Bitmap.Height = 0) then
    Exit;

  BeginInvoke(Integer(ESimbaCommunicationMessage.DEBUGIMAGE_SHOW));

  try
    Header.Size      := 0;
    Header.MessageID := FMessageID;

    FOutputStream.Write(Header, SizeOf(TSimbaIPCHeader));
    FOutputStream.Write(EnsureVisible, SizeOf(Boolean));
    FOutputStream.Write(Bitmap.Width, SizeOf(Integer));
    FOutputStream.Write(Bitmap.Height, SizeOf(Integer));

    for Y := 0 to Bitmap.Height - 1 do
      FOutputStream.Write(Bitmap.Data[Y * Bitmap.Width], Bitmap.Width * SizeOf(TColorBGRA));

    // Read result
    FInputStream.Read(Header, SizeOf(TSimbaIPCHeader));
    if (Header.Size > 0) then
    begin
      FResult.CopyFrom(FInputStream, Header.Size);
      FResult.Position := 0;
    end;
  finally
    EndInvoke();
  end;
end;

// Send chunked ... faster & less memory needed.
// Data is sent row by row
procedure TSimbaScriptCommunication.DebugImage_Update(Bitmap: TSimbaImage);
var
  Header: TSimbaIPCHeader;
  Y: Integer;
begin
  if (Bitmap = nil) or (Bitmap.Width = 0) or (Bitmap.Height = 0) then
    Exit;

  BeginInvoke(Integer(ESimbaCommunicationMessage.DEBUGIMAGE_UPDATE));

  try
    Header.Size      := 0;
    Header.MessageID := FMessageID;

    FOutputStream.Write(Header, SizeOf(TSimbaIPCHeader));
    FOutputStream.Write(Bitmap.Width, SizeOf(Integer));
    FOutputStream.Write(Bitmap.Height, SizeOf(Integer));

    for Y := 0 to Bitmap.Height - 1 do
      FOutputStream.Write(Bitmap.Data[Y * Bitmap.Width], Bitmap.Width * SizeOf(TColorBGRA));

    // Read result
    FInputStream.Read(Header, SizeOf(TSimbaIPCHeader));
    if (Header.Size > 0) then
    begin
      FResult.CopyFrom(FInputStream, Header.Size);
      FResult.Position := 0;
    end;
  finally
    EndInvoke();
  end;
end;

procedure TSimbaScriptCommunication.DebugImage_Hide;
begin
  BeginInvoke(Integer(ESimbaCommunicationMessage.DEBUGIMAGE_HIDE));

  try
    Invoke();
  finally
    EndInvoke();
  end;
end;

procedure TSimbaScriptCommunication.DebugImage_Display(Width, Height: Integer);
begin
  BeginInvoke(Integer(ESimbaCommunicationMessage.DEBUGIMAGE_DISPLAY));

  try
    FParams.Write(Width, SizeOf(Integer));
    FParams.Write(Height, SizeOf(Integer));

    Invoke();
  finally
    EndInvoke();
  end;
end;

procedure TSimbaScriptCommunication.DebugImage_Display(X, Y, Width, Height: Integer);
begin
  BeginInvoke(Integer(ESimbaCommunicationMessage.DEBUGIMAGE_DISPLAY_XY));

  try
    FParams.Write(X, SizeOf(Integer));
    FParams.Write(Y, SizeOf(Integer));
    FParams.Write(Width, SizeOf(Integer));
    FParams.Write(Height, SizeOf(Integer));

    Invoke();
  finally
    EndInvoke();
  end;
end;

end.

