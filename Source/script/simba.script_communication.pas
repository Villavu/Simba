{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.script_communication;

{$i simba.inc}

interface

uses
  classes, sysutils, extctrls,
  simba.ipc, simba.mufasatypes, simba.bitmap;

type
  TSimbaScriptCommunication = class(TSimbaIPCClient)
  protected
    procedure BeginInvoke(Message: ESimbaCommunicationMessage); overload;
  public
    function GetSimbaTargetWindow: PtrUInt;
    function GetSimbaTargetPID: PtrUInt;
    function GetSimbaPID: PtrUInt;

    procedure ScriptError(Message: String; Line, Column: Integer; FileName: String);
    procedure ScriptStateChanged(State: ESimbaScriptState);

    procedure ShowBalloonHint(Title, Hint: String; Timeout: Integer; Flags: TBalloonFlags);

    procedure Status(S: String);
    procedure Disguse(S: String);

    procedure DebugImage_SetZoom(Level: Single);
    procedure DebugImage_SetMaxSize(Width, Height: Integer);
    procedure DebugImage_MoveTo(X, Y: Integer);
    procedure DebugImage_Draw(Bitmap: TMufasaBitmap);
    procedure DebugImage_Show(Bitmap: TMufasaBitmap);
    procedure DebugImage_Hide;
    procedure DebugImage_Display(Width, Height: Integer);
    procedure DebugImage_Clear;

    procedure DebugMethodName(Name: String);
    procedure DebugEvents(Events: TMemoryStream);
  end;

implementation

procedure TSimbaScriptCommunication.BeginInvoke(Message: ESimbaCommunicationMessage);
begin
  BeginInvoke(Ord(Message));
end;

function TSimbaScriptCommunication.GetSimbaTargetWindow: PtrUInt;
begin
  BeginInvoke(ESimbaCommunicationMessage.SIMBA_TARGET_WINDOW);

  try
    Invoke();

    FResult.Read(Result, SizeOf(PtrUInt));
  finally
    EndInvoke();
  end;
end;

function TSimbaScriptCommunication.GetSimbaTargetPID: PtrUInt;
begin
  BeginInvoke(ESimbaCommunicationMessage.SIMBA_TARGET_PID);

  try
    Invoke();

    FResult.Read(Result, SizeOf(PtrUInt));
  finally
    EndInvoke();
  end;
end;

function TSimbaScriptCommunication.GetSimbaPID: PtrUInt;
begin
  BeginInvoke(ESimbaCommunicationMessage.SIMBA_PID);

  try
    Invoke();

    FResult.Read(Result, SizeOf(PtrUInt));
  finally
    EndInvoke();
  end;
end;

procedure TSimbaScriptCommunication.ScriptError(Message: String; Line, Column: Integer; FileName: String);
begin
  BeginInvoke(ESimbaCommunicationMessage.SCRIPT_ERROR);

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
  BeginInvoke(ESimbaCommunicationMessage.SCRIPT_STATE_CHANGE);

  try
    FParams.Write(State, SizeOf(ESimbaScriptState));

    Invoke();
  finally
    EndInvoke();
  end;
end;

procedure TSimbaScriptCommunication.ShowBalloonHint(Title, Hint: String; Timeout: Integer; Flags: TBalloonFlags);
begin
  BeginInvoke(ESimbaCommunicationMessage.BALLOON_HINT);

  try
    FParams.WriteAnsiString(Title);
    FParams.WriteAnsiString(Hint);
    FParams.Write(Timeout, SizeOf(Integer));
    FParams.Write(Flags, SizeOf(Flags));

    Invoke();
  finally
    EndInvoke();
  end;
end;

procedure TSimbaScriptCommunication.Status(S: String);
begin
  BeginInvoke(ESimbaCommunicationMessage.STATUS);

  try
    FParams.WriteAnsiString(S);

    Invoke();
  finally
    EndInvoke();
  end;
end;

procedure TSimbaScriptCommunication.Disguse(S: String);
begin
  BeginInvoke(ESimbaCommunicationMessage.DISGUSE);

  try
    FParams.WriteAnsiString(S);

    Invoke();
  finally
    EndInvoke();
  end;
end;

procedure TSimbaScriptCommunication.DebugImage_SetZoom(Level: Single);
begin
  BeginInvoke(ESimbaCommunicationMessage.DEBUGIMAGE_ZOOM);

  try
    FParams.Write(Level, SizeOf(Single));

    Invoke();
  finally
    EndInvoke();
  end;
end;

procedure TSimbaScriptCommunication.DebugImage_SetMaxSize(Width, Height: Integer);
begin
  BeginInvoke(ESimbaCommunicationMessage.DEBUGIMAGE_MAXSIZE);

  try
    FParams.Write(Width, SizeOf(Integer));
    FParams.Write(Height, SizeOf(Integer));

    Invoke();
  finally
    EndInvoke();
  end;
end;

procedure TSimbaScriptCommunication.DebugImage_MoveTo(X, Y: Integer);
begin
  BeginInvoke(ESimbaCommunicationMessage.DEBUGIMAGE_MOVETO);

  try
    FParams.Write(X, SizeOf(Integer));
    FParams.Write(Y, SizeOf(Integer));

    Invoke();
  finally
    EndInvoke();
  end;
end;

procedure TSimbaScriptCommunication.DebugImage_Draw(Bitmap: TMufasaBitmap);
begin
  BeginInvoke(ESimbaCommunicationMessage.DEBUGIMAGE_DRAW);

  try
    FParams.Write(Bitmap.Width, SizeOf(Integer));
    FParams.Write(Bitmap.Height, SizeOf(Integer));
    FParams.Write(Bitmap.Data^, Bitmap.Width * Bitmap.Height * SizeOf(TRGB32));

    Invoke();
  finally
    EndInvoke();
  end;
end;

procedure TSimbaScriptCommunication.DebugImage_Show(Bitmap: TMufasaBitmap);
begin
  BeginInvoke(ESimbaCommunicationMessage.DEBUGIMAGE_SHOW);

  try
    FParams.Write(Bitmap.Width, SizeOf(Integer));
    FParams.Write(Bitmap.Height, SizeOf(Integer));
    FParams.Write(Bitmap.Data^, Bitmap.Width * Bitmap.Height * SizeOf(TRGB32));

    Invoke();
  finally
    EndInvoke();
  end;
end;

procedure TSimbaScriptCommunication.DebugImage_Hide;
begin
  BeginInvoke(ESimbaCommunicationMessage.DEBUGIMAGE_HIDE);

  try
    Invoke();
  finally
    EndInvoke();
  end;
end;

procedure TSimbaScriptCommunication.DebugImage_Display(Width, Height: Integer);
begin
  BeginInvoke(ESimbaCommunicationMessage.DEBUGIMAGE_DISPLAY);

  try
    FParams.Write(Width, SizeOf(Integer));
    FParams.Write(Height, SizeOf(Integer));

    Invoke();
  finally
    EndInvoke();
  end;
end;

procedure TSimbaScriptCommunication.DebugImage_Clear;
begin
   BeginInvoke(ESimbaCommunicationMessage.DEBUGIMAGE_CLEAR);

  try
    Invoke();
  finally
    EndInvoke();
  end;
end;

procedure TSimbaScriptCommunication.DebugMethodName(Name: String);
begin
  BeginInvoke(ESimbaCommunicationMessage.DEBUG_METHOD_NAME);

  try
    FParams.WriteAnsiString(Name);

    Invoke();
  finally
    EndInvoke();
  end;
end;

procedure TSimbaScriptCommunication.DebugEvents(Events: TMemoryStream);
var
  Count: Integer;
begin
  BeginInvoke(ESimbaCommunicationMessage.DEBUG_EVENTS);

  try
    Count := Events.Position div SizeOf(TSimbaScriptDebuggerEvent);

    FParams.Write(Count, SizeOf(Integer));
    FParams.Write(Events.Memory^, Events.Position);

    Invoke();
  finally
    EndInvoke();
  end;
end;

end.

