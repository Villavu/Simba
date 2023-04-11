{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.target_eios;

{
  EIOS loads a plugin and calls a specific export when required.

  When `SetEIOSTarget` is called from the script the `EIOS_RequestTarget` export is
  called with possible data being passed from the script. A pointer must be returned
  which will be the `Target` parameter when future exports are called.
}

{
  When ReturnData is called the following methods are called:

  // This is a procedure that receives the image buffer pointer and hopefully updates the contents of such pointer.
  UpdateImageBuffer: procedure(FTarget: pointer)

  //Works similar to the UpdateImageBuffer procedure, but it is a function instead and uses the result as the new image buffer pointer of the FTarget.
  UpdateImageBufferEx: function(FTarget: pointer): PColorBGRA:

  // Does the same as UpdateImageBuffer but it specifies which region of the image buffer requires to be updated.
  UpdateImageBufferBounds: procedure(FTarget: pointer; sx,sy,ex,ey: integer):
}

{
  Because they all have the same "responsibility", of keeping the image buffer
  updated, they are consulted exclusively and have the following priority (from highest to lowest):

  UpdateImageBufferBounds is checked first
  UpdateImageBufferEx is checked second
  UpdateImageBuffer is checked last
}

{
  GetTargetDimensions should reference the buffers width and height.
}

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.mufasatypes, simba.colormath, dynlibs;

type
  PEIOSTarget = ^TEIOSTarget;
  TEIOSTarget = record
    Lib: TLibHandle;
    ImageBuffer: PColorBGRA;
    Target: Pointer;

    RequestTarget: function(Data: PChar): Pointer; stdcall;
    ReleaseTarget: procedure(Target: Pointer); stdcall;

    GetTargetDimensions: procedure(Target: Pointer; var Width, Height: Integer); stdcall;
    GetTargetPosition: procedure(Target: Pointer; var Left, Top: Integer); stdcall;
    GetImageBuffer: function(Target: Pointer): PColorBGRA; stdcall;
    UpdateImageBuffer: procedure(Target: Pointer); stdcall;
    UpdateImageBufferEx: function(Target: Pointer): PColorBGRA; stdcall;
    UpdateImageBufferBounds: procedure(Target: Pointer; X1, Y1, X2, Y2: Integer); stdcall;

    GetMousePosition: procedure(Target: Pointer; var X, Y: Integer); stdcall;
    MoveMouse: procedure(Target: Pointer; X, Y: Integer); stdcall;
    ScrollMouse: procedure(Target: pointer; X, Y: Integer; Lines: Integer); stdcall;
    HoldMouse: procedure(Target: Pointer; X, Y: Integer; Button: Integer); stdcall;
    ReleaseMouse: procedure(Target: Pointer; X, Y: Integer; Button: Integer); stdcall;
    IsMouseButtonHeld: function(Target: Pointer; Button: Integer): Boolean; stdcall;

    SendString: procedure(Target: Pointer; str: PChar; KeyWait, KeyModWait: Integer); stdcall;
    HoldKey: procedure(Target: Pointer; Key: Integer); stdcall;
    ReleaseKey: procedure(Target: Pointer; Key: Integer); stdcall;
    IsKeyHeld: function(Target: Pointer; Key: Integer): Boolean; stdcall;
    GetKeyCode: function(Target: Pointer; Character: Char): Integer; stdcall;
 end;

function LoadEIOS(FileName, Args: String): TEIOSTarget;

function EIOSTarget_GetImageData(Target: Pointer; X, Y, Width, Height: Integer; var Data: PColorBGRA; var DataWidth: Integer): Boolean;
procedure EIOSTarget_GetDimensions(Target: Pointer; out Width, Height: Integer);

procedure EIOSTarget_KeyDown(Target: Pointer; Key: KeyCode);
procedure EIOSTarget_KeyUp(Target: Pointer; Key: KeyCode);
procedure EIOSTarget_KeySend(Target: Pointer; Key: Char; KeyDownTime, KeyUpTime, ModifierDownTime, ModifierUpTime: Integer);
function EIOSTarget_KeyPressed(Target: Pointer; Key: KeyCode): Boolean;

function EIOSTarget_MousePressed(Target: Pointer; Button: MouseButton): Boolean;
function EIOSTarget_MousePosition(Target: Pointer): TPoint;
procedure EIOSTarget_MouseTeleport(Target: Pointer; P: TPoint);
procedure EIOSTarget_MouseDown(Target: Pointer; Button: MouseButton);
procedure EIOSTarget_MouseUp(Target: Pointer; Button: MouseButton);
procedure EIOSTarget_MouseScroll(Target: Pointer; Scrolls: Integer);

implementation

uses
  simba.script_pluginloader;

function LoadEIOS(FileName, Args: String): TEIOSTarget;
begin
  with Result do
  begin
    try
      Lib := LoadPlugin(FileName);
    except
      on E: Exception do
        raise Exception.Create('EIOS: ' + E.Message);
    end;

    Pointer(RequestTarget) := GetProcedureAddress(Lib, 'EIOS_RequestTarget');
    Pointer(ReleaseTarget) := GetProcedureAddress(Lib, 'EIOS_ReleaseTarget');

    Pointer(GetTargetPosition) := GetProcedureAddress(Lib, 'EIOS_GetTargetPosition');
    Pointer(GetTargetDimensions) := GetProcedureAddress(Lib, 'EIOS_GetTargetDimensions');
    Pointer(GetImageBuffer) := GetProcedureAddress(Lib, 'EIOS_GetImageBuffer');
    Pointer(UpdateImageBuffer) := GetProcedureAddress(Lib, 'EIOS_UpdateImageBuffer');
    Pointer(UpdateImageBufferEx) := GetProcedureAddress(Lib, 'EIOS_UpdateImageBufferEx');
    Pointer(UpdateImageBufferBounds) := GetProcedureAddress(Lib, 'EIOS_UpdateImageBufferBounds');

    Pointer(GetMousePosition) := GetProcedureAddress(Lib, 'EIOS_GetMousePosition');
    Pointer(MoveMouse) := GetProcedureAddress(Lib, 'EIOS_MoveMouse');
    Pointer(ScrollMouse) := GetProcedureAddress(Lib, 'EIOS_ScrollMouse');
    Pointer(HoldMouse) := GetProcedureAddress(Lib, 'EIOS_HoldMouse');
    Pointer(ReleaseMouse) := GetProcedureAddress(Lib, 'EIOS_ReleaseMouse');
    Pointer(IsMouseButtonHeld) := GetProcedureAddress(Lib, 'EIOS_IsMouseButtonHeld');

    Pointer(SendString) := GetProcedureAddress(Lib, 'EIOS_SendString');
    Pointer(HoldKey) := GetProcedureAddress(Lib, 'EIOS_HoldKey');
    Pointer(ReleaseKey) := GetProcedureAddress(Lib, 'EIOS_ReleaseKey');
    Pointer(IsKeyHeld) := GetProcedureAddress(Lib, 'EIOS_IsKeyHeld');
    Pointer(GetKeyCode) := GetProcedureAddress(Lib, 'EIOS_GetKeyCode');

    if Assigned(RequestTarget) then
    begin
      Target := RequestTarget(PChar(Args));
      if (Target = nil) then
        raise Exception.Create('EIOS: RequestTarget returned nil');
    end else
      raise Exception.Create('EIOS: RequestTarget not exported');

    if Assigned(GetImageBuffer) then
      ImageBuffer := GetImageBuffer(Target);
  end;
end;

function EIOSTarget_GetImageData(Target: Pointer; X, Y, Width, Height: Integer; var Data: PColorBGRA; var DataWidth: Integer): Boolean;
var
  BufferWidth, BufferHeight: Integer;
begin
  with PEIOSTarget(Target)^ do
  begin
    if Assigned(UpdateImageBufferBounds) then
      UpdateImageBufferBounds(Target, X, Y, X + Width, Y + Height)
    else
    if Assigned(UpdateImageBufferEx) then
      ImageBuffer := UpdateImageBufferEx(Target)
    else
    if Assigned(UpdateImageBuffer) then
      UpdateImageBuffer(Target);

    GetTargetDimensions(Target, BufferWidth, BufferHeight);

    Result := (ImageBuffer <> nil);
    if Result then
    begin
      Data := @ImageBuffer[Y * BufferWidth + X];
      DataWidth := BufferWidth;
    end;
  end;
end;

procedure EIOSTarget_GetDimensions(Target: Pointer; out Width, Height: Integer);
begin
  with PEIOSTarget(Target)^ do
  begin
    Width := 0;
    Height := 0;
    if Assigned(GetTargetDimensions) then
      GetTargetDimensions(Target, Width, Height);
  end;
end;

function EIOSTarget_MousePressed(Target: Pointer; Button: MouseButton): Boolean;
begin
  with PEIOSTarget(Target)^ do
  begin
    if Assigned(IsMouseButtonHeld) then
      Result := IsMouseButtonHeld(Target, Int32(Button))
    else
      Result := False;
  end;
end;

function EIOSTarget_KeyPressed(Target: Pointer; Key: KeyCode): Boolean;
begin
  with PEIOSTarget(Target)^ do
  begin
    if Assigned(IsKeyHeld) then
      Result := IsKeyHeld(Target, Int32(Key))
    else
      Result := False;
  end;
end;

function EIOSTarget_MousePosition(Target: Pointer): TPoint;
begin
  with PEIOSTarget(Target)^ do
  begin
    Result.X := 0;
    Result.Y := 0;
    if Assigned(GetMousePosition) then
      GetMousePosition(Target, Result.X, Result.Y);
  end;
end;

procedure EIOSTarget_MouseTeleport(Target: Pointer; P: TPoint);
begin
  with PEIOSTarget(Target)^ do
  begin
    if Assigned(MoveMouse) then
      MoveMouse(Target, P.X, P.Y);
  end;
end;

procedure EIOSTarget_MouseDown(Target: Pointer; Button: MouseButton);
begin
  with PEIOSTarget(Target)^, EIOSTarget_MousePosition(Target) do
  begin
    if Assigned(HoldMouse) then
      HoldMouse(Target, X, Y, Int32(Button));
  end;
end;

procedure EIOSTarget_MouseUp(Target: Pointer; Button: MouseButton);
begin
  with PEIOSTarget(Target)^, EIOSTarget_MousePosition(Target) do
  begin
    if Assigned(ReleaseMouse) then
      ReleaseMouse(Target, X, Y, Int32(Button));
  end;
end;

procedure EIOSTarget_MouseScroll(Target: Pointer; Scrolls: Integer);
begin
  with PEIOSTarget(Target)^, EIOSTarget_MousePosition(Target) do
  begin
    if Assigned(ScrollMouse) then
      ScrollMouse(Target, X, Y, Scrolls);
  end;
end;

procedure EIOSTarget_KeyDown(Target: Pointer; Key: KeyCode);
begin
  with PEIOSTarget(Target)^ do
  begin
    if Assigned(HoldKey) then
      HoldKey(Target, Int32(Key));
  end;
end;

procedure EIOSTarget_KeyUp(Target: Pointer; Key: KeyCode);
begin
  with PEIOSTarget(Target)^ do
  begin
    if Assigned(ReleaseKey) then
      ReleaseKey(Target, Int32(Key));
  end;
end;

procedure EIOSTarget_KeySend(Target: Pointer; Key: Char; KeyDownTime, KeyUpTime, ModifierDownTime, ModifierUpTime: Integer);
begin
  with PEIOSTarget(Target)^ do
  begin
    if Assigned(SendString) then
      SendString(Target, PChar(String(Key)), KeyDownTime, ModifierDownTime);
  end;
end;

end.

