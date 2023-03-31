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
  TEIOSExports = record
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

  TEIOSTarget = record
  private
    FLib: TLibHandle;
    FExports: TEIOSExports;
    FImageBuffer: PColorBGRA;
    FTarget: Pointer;
  public
    procedure Load(FileName: String; Args: String);

    function GetImageData(X, Y, Width, Height: Integer; var Data: PColorBGRA; var DataWidth: Integer): Boolean;
    procedure GetDimensions(out W, H: Integer);

    procedure KeyDown(Key: KeyCode);
    procedure KeyUp(Key: KeyCode);
    procedure KeySend(Key: Char; KeyDownTime, KeyUpTime, ModifierDownTime, ModifierUpTime: Integer);
    function KeyPressed(Key: KeyCode): Boolean;

    function MousePressed(Button: MouseButton): Boolean;
    function MousePosition: TPoint;
    procedure MouseTeleport(P: TPoint);
    procedure MouseDown(Button: MouseButton);
    procedure MouseUp(Button: MouseButton);
    procedure MouseScroll(Scrolls: Integer);
  end;

implementation

uses
  simba.script_pluginloader;

procedure TEIOSTarget.Load(FileName: String; Args: String);
begin
  try
    FLib := LoadPlugin(FileName);
  except
    on E: Exception do
      raise Exception.Create('EIOS: ' + E.Message);
  end;

  with FExports do
  begin
    Pointer(RequestTarget) := GetProcedureAddress(FLib, 'EIOS_RequestTarget');
    Pointer(ReleaseTarget) := GetProcedureAddress(FLib, 'EIOS_ReleaseTarget');

    Pointer(GetTargetPosition) := GetProcedureAddress(FLib, 'EIOS_GetTargetPosition');
    Pointer(GetTargetDimensions) := GetProcedureAddress(FLib, 'EIOS_GetTargetDimensions');
    Pointer(GetImageBuffer) := GetProcedureAddress(FLib, 'EIOS_GetImageBuffer');
    Pointer(UpdateImageBuffer) := GetProcedureAddress(FLib, 'EIOS_UpdateImageBuffer');
    Pointer(UpdateImageBufferEx) := GetProcedureAddress(FLib, 'EIOS_UpdateImageBufferEx');
    Pointer(UpdateImageBufferBounds) := GetProcedureAddress(FLib, 'EIOS_UpdateImageBufferBounds');

    Pointer(GetMousePosition) := GetProcedureAddress(FLib, 'EIOS_GetMousePosition');
    Pointer(MoveMouse) := GetProcedureAddress(FLib, 'EIOS_MoveMouse');
    Pointer(ScrollMouse) := GetProcedureAddress(FLib, 'EIOS_ScrollMouse');
    Pointer(HoldMouse) := GetProcedureAddress(FLib, 'EIOS_HoldMouse');
    Pointer(ReleaseMouse) := GetProcedureAddress(FLib, 'EIOS_ReleaseMouse');
    Pointer(IsMouseButtonHeld) := GetProcedureAddress(FLib, 'EIOS_IsMouseButtonHeld');

    Pointer(SendString) := GetProcedureAddress(FLib, 'EIOS_SendString');
    Pointer(HoldKey) := GetProcedureAddress(FLib, 'EIOS_HoldKey');
    Pointer(ReleaseKey) := GetProcedureAddress(FLib, 'EIOS_ReleaseKey');
    Pointer(IsKeyHeld) := GetProcedureAddress(FLib, 'EIOS_IsKeyHeld');
    Pointer(GetKeyCode) := GetProcedureAddress(FLib, 'EIOS_GetKeyCode');
  end;

  if Assigned(FExports.RequestTarget) then
  begin
    FTarget := FExports.RequestTarget(PChar(Args));
    if (FTarget = nil) then
      raise Exception.Create('EIOS: RequestTarget returned nil');
  end else
    raise Exception.Create('EIOS: RequestTarget not exported');

  if Assigned(FExports.GetImageBuffer) then
    FImageBuffer := FExports.GetImageBuffer(FTarget);
end;

function TEIOSTarget.GetImageData(X, Y, Width, Height: Integer; var Data: PColorBGRA; var DataWidth: Integer): Boolean;
var
  TargetWidth, TargetHeight: Integer;
begin
  if Pointer(FExports.UpdateImageBufferBounds) <> nil then
    FExports.UpdateImageBufferBounds(FTarget, X, Y, X + Width, Y + Height)
  else
  if Pointer(FExports.UpdateImageBufferEx) <> nil then
    FImageBuffer := FExports.UpdateImageBufferEx(FTarget)
  else
  if Pointer(FExports.UpdateImageBuffer) <> nil then
    FExports.UpdateImageBuffer(FTarget);

  FExports.GetTargetDimensions(FTarget, TargetWidth, TargetHeight);

  Result := (FImageBuffer <> nil);
  if Result then
    Data := @FImageBuffer[Y * TargetWidth + X];
  DataWidth := TargetWidth;
end;

procedure TEIOSTarget.GetDimensions(out W, H: Integer);
begin
  W := 0;
  H := 0;

  if Assigned(FExports.GetTargetDimensions) then
    FExports.GetTargetDimensions(FTarget, W, H);
end;

function TEIOSTarget.MousePressed(Button: MouseButton): Boolean;
begin
  if Assigned(FExports.IsMouseButtonHeld) then
    Result := FExports.IsMouseButtonHeld(FTarget, Int32(Button))
  else
    Result := False;
end;

function TEIOSTarget.KeyPressed(Key: KeyCode): Boolean;
begin
  if Assigned(FExports.IsKeyHeld) then
    Result := FExports.IsKeyHeld(FTarget, Int32(Key))
  else
    Result := False;
end;

function TEIOSTarget.MousePosition: TPoint;
begin
  Result.X := 0;
  Result.Y := 0;

  if Assigned(FExports.GetMousePosition) then
    FExports.GetMousePosition(FTarget, Result.X, Result.Y);
end;

procedure TEIOSTarget.MouseTeleport(P: TPoint);
begin
  if Assigned(FExports.MoveMouse) then
    FExports.MoveMouse(FTarget, P.X, P.Y);
end;

procedure TEIOSTarget.MouseDown(Button: MouseButton);
begin
  if Assigned(FExports.HoldMouse) then
    with MousePosition() do
      FExports.HoldMouse(FTarget, X, Y, Int32(Button));
end;

procedure TEIOSTarget.MouseUp(Button: MouseButton);
begin
  if Assigned(FExports.ReleaseMouse) then
    with MousePosition() do
      FExports.ReleaseMouse(FTarget, X, Y, Int32(Button));
end;

procedure TEIOSTarget.MouseScroll(Scrolls: Integer);
begin
  if Assigned(FExports.ScrollMouse) then
    with MousePosition() do
      FExports.ScrollMouse(FTarget, X, Y, Scrolls);
end;

procedure TEIOSTarget.KeyDown(Key: KeyCode);
begin
  if Assigned(FExports.HoldKey) then
    FExports.HoldKey(FTarget, Int32(Key));
end;

procedure TEIOSTarget.KeyUp(Key: KeyCode);
begin
  if Assigned(FExports.ReleaseKey) then
    FExports.ReleaseKey(FTarget, Int32(Key));
end;

procedure TEIOSTarget.KeySend(Key: Char; KeyDownTime, KeyUpTime, ModifierDownTime, ModifierUpTime: Integer);
begin
  if Assigned(FExports.SendString) then
    FExports.SendString(FTarget, PChar(String(Key)), KeyDownTime, ModifierDownTime);
end;

end.

