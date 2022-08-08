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
  UpdateImageBufferEx: function(FTarget: pointer): prgb32:

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
  simba.target, simba.mufasatypes, dynlibs;

type
  PEIOS_Client = ^TEIOS_Client;
  TEIOS_Client = record
    RequestTarget: function(Data: PChar): Pointer; stdcall;
    ReleaseTarget: procedure(Target: Pointer); stdcall;

    GetTargetDimensions: procedure(Target: Pointer; var Width, Height: Integer); stdcall;
    GetTargetPosition: procedure(Target: Pointer; var Left, Top: Integer); stdcall;
    GetImageBuffer: function(Target: Pointer): PRGB32; stdcall;
    UpdateImageBuffer: procedure(Target: Pointer); stdcall;
    UpdateImageBufferEx: function(Target: Pointer): PRGB32; stdcall;
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

  PEIOS_Target = ^TEIOS_Target;
  TEIOS_Target = class(TTarget)
  protected
    FLib: TLibHandle;
    FClient: TEIOS_Client;
    FTarget: Pointer;
    FBuffer: PRGB32;

    procedure GetTargetBounds(out Bounds: TBox); override;
  public
    constructor Create(Plugin: String; Data: String);
    destructor Destroy; override;

    function ReturnData(X, Y, Width, Height: Integer): TRetData; override;
    function CopyData(X, Y, Width, Height: Integer): PRGB32; override;

    procedure GetMousePosition(out X, Y: Integer); override;
    procedure MoveMouse(X, Y: Integer); override;
    procedure ScrollMouse(X, Y: Integer; Lines: Integer); override;
    procedure HoldMouse(X, Y: Integer; Button: TClickType); override;
    procedure ReleaseMouse(X, Y: Integer; Button: TClickType); override;
    function IsMouseButtonHeld(Button: TClickType) : Boolean;override;

    procedure SendString(Text: String; KeyWait, KeyModWait: Integer); override;
    procedure SendStringEx(Text: String; MinKeyWait, MaxKeyWait: Integer); override;
    procedure HoldKey(Key: Integer); override;
    procedure ReleaseKey(Key: Integer); override;
    function IsKeyHeld(Key: Integer): Boolean; override;
    function GetKeyCode(Character: Char) : Integer; override;
  end;

implementation

uses
  simba.math;

procedure TEIOS_Target.GetTargetBounds(out Bounds: TBox);
begin
  if Pointer(FClient.GetTargetPosition) <> nil then
    FClient.GetTargetPosition(FTarget, Bounds.X1, Bounds.Y1)
  else
  begin
    // SMART doesn't export this
    // raise Exception.Create('EIOS_GetTargetPosition not available');

    Bounds.X1 := 0;
    Bounds.Y1 := 0;
  end;

  if Pointer(FClient.GetTargetDimensions) <> nil then
    FClient.GetTargetDimensions(FTarget, Bounds.X2, Bounds.Y2)
  else
    raise Exception.Create('EIOS_GetTargetDimensions not available');
end;

constructor TEIOS_Target.Create(Plugin: String; Data: String);
begin
  inherited Create();

  FLib := LoadLibrary(Plugin);
  if (FLib = NilHandle) then
    raise Exception.Create('TEIOS_Target.Create: Unable to load plugin: ' + Plugin);

  with FClient do
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
    Pointer(ScrollMouse) := GetProcedureAddress(FLib,'EIOS_ScrollMouse');
    Pointer(HoldMouse) := GetProcedureAddress(FLib, 'EIOS_HoldMouse');
    Pointer(ReleaseMouse) := GetProcedureAddress(FLib, 'EIOS_ReleaseMouse');
    Pointer(IsMouseButtonHeld) := GetProcedureAddress(FLib, 'EIOS_IsMouseButtonHeld');

    Pointer(SendString) := GetProcedureAddress(FLib, 'EIOS_SendString');
    Pointer(HoldKey) := GetProcedureAddress(FLib, 'EIOS_HoldKey');
    Pointer(ReleaseKey) := GetProcedureAddress(FLib, 'EIOS_ReleaseKey');
    Pointer(IsKeyHeld) := GetProcedureAddress(FLib, 'EIOS_IsKeyHeld');
    Pointer(GetKeyCode) := GetProcedureAddress(FLib, 'EIOS_GetKeyCode');
  end;

  FTarget := nil;
  FBuffer := nil;

  if Pointer(FClient.RequestTarget) <> nil then
  begin
    FTarget := FClient.RequestTarget(PChar(Data));
    if (FTarget = nil) then
      raise Exception.Create('EIOS_RequestTarget returned nil');
  end else
    raise Exception.Create('EIOS_RequestTarget not available');

  if Pointer(FClient.GetImageBuffer) <> nil then
    FBuffer := FClient.GetImageBuffer(FTarget);
end;

destructor TEIOS_Target.Destroy;
begin
  if (FLib <> NilHandle) then
    FreeLibrary(FLib);

  if (FClient.ReleaseTarget <> nil) and (FTarget <> nil) then
    FClient.ReleaseTarget(FTarget);

  inherited Destroy();
end;

function TEIOS_Target.ReturnData(X, Y, Width, Height: Integer): TRetData;
var
  Bounds: TBox;
begin
  if Pointer(FClient.UpdateImageBufferBounds) <> nil then
    FClient.UpdateImageBufferBounds(FTarget, X, Y, X + Width, Y + Height)
  else
  if Pointer(FClient.UpdateImageBufferEx) <> nil then
    FBuffer := FClient.UpdateImageBufferEx(FTarget)
  else
  if Pointer(FClient.UpdateImageBuffer) <> nil then
    FClient.UpdateImageBuffer(FTarget);

  if (FBuffer <> nil) then
  begin
    if ValidateImageCapture(X, Y, Width, Height, Bounds) then
    begin
      Result.Ptr := @FBuffer[Y * Bounds.X2 + X];
      Result.RowLen := Bounds.X2;
      Result.IncPtrWith := Bounds.X2 - Width;
    end else
      Result := Default(TRetData);
  end else
    Result := Default(TRetData);
end;

function TEIOS_Target.CopyData(X, Y, Width, Height: Integer): PRGB32;
var
  Bounds: TBox;
  Loop: Integer;
begin
  if Pointer(FClient.UpdateImageBufferBounds) <> nil then
    FClient.UpdateImageBufferBounds(FTarget, X, Y, X + Width, Y + Height)
  else
  if Pointer(FClient.UpdateImageBufferEx) <> nil then
    FBuffer := FClient.UpdateImageBufferEx(FTarget)
  else
  if Pointer(FClient.UpdateImageBuffer) <> nil then
    FClient.UpdateImageBuffer(FTarget);

  if (FBuffer <> nil) then
  begin
    if ValidateImageCapture(X, Y, Width, Height, Bounds) then
    begin
      Result := GetMem(Width * Height * SizeOf(TRGB32));
      for Loop := 0 to Height - 1 do
        Move(FBuffer[(Y + Loop) * Bounds.X2 + X], Result[Loop * Width], Width * SizeOf(TRGB32));
    end else
      Result := nil;
  end else
    Result := nil;
end;

procedure TEIOS_Target.GetMousePosition(out X, Y: Integer);
begin
  if Pointer(FClient.GetMousePosition) <> nil then
  begin
    FClient.GetMousePosition(FTarget, X, Y);

    X := X - FMouseClientArea.X1;
    Y := Y - FMouseClientArea.Y1;
  end else
    inherited GetMousePosition(X, Y);
end;

procedure TEIOS_Target.MoveMouse(X, Y: Integer);
begin
  MouseClientAreaOffset(X, Y);

  if Pointer(FClient.MoveMouse) <> nil then
    FClient.MoveMouse(FTarget, X, Y)
  else
    inherited MoveMouse(X, Y);
end;

procedure TEIOS_Target.ScrollMouse(X, Y: Integer; Lines: Integer);
begin
  MouseClientAreaOffset(X, Y);

  if Pointer(FClient.ScrollMouse) <> nil then
    FClient.ScrollMouse(FTarget, X, Y, Lines)
  else
    inherited ScrollMouse(X, Y, Lines);
end;

procedure TEIOS_Target.HoldMouse(X, Y: Integer; Button: TClickType);
begin
  MouseClientAreaOffset(X, Y);

  if Pointer(FClient.HoldMouse) <> nil then
  begin
    case Button of
      MOUSE_LEFT:   FClient.HoldMouse(FTarget, X, Y, 1);
      MOUSE_MIDDLE: FClient.HoldMouse(FTarget, X, Y, 2);
      MOUSE_RIGHT:  FClient.HoldMouse(FTarget, X, Y, 3);
    end;
  end else
    inherited HoldMouse(X, Y, Button);
end;

procedure TEIOS_Target.ReleaseMouse(X, Y: Integer; Button: TClickType);
begin
  MouseClientAreaOffset(X, Y);

  if Pointer(FClient.ReleaseMouse) <> nil then
  begin
    case Button of
      MOUSE_LEFT:   FClient.ReleaseMouse(FTarget, X, Y, 1);
      MOUSE_MIDDLE: FClient.ReleaseMouse(FTarget, X, Y, 2);
      MOUSE_RIGHT:  FClient.ReleaseMouse(FTarget, X, Y, 3);
    end;
  end else
    inherited ReleaseMouse(X, Y, Button);
end;

function TEIOS_Target.IsMouseButtonHeld(Button: TClickType): Boolean;
begin
  if Pointer(FClient.IsMouseButtonHeld) <> nil then
  begin
    case Button of
      MOUSE_LEFT:   Result := FClient.IsMouseButtonHeld(FTarget, 1);
      MOUSE_MIDDLE: Result := FClient.IsMouseButtonHeld(FTarget, 2);
      MOUSE_RIGHT:  Result := FClient.IsMouseButtonHeld(FTarget, 3);
    end;
  end else
    Result := inherited IsMouseButtonHeld(Button);
end;

procedure TEIOS_Target.SendString(Text: String; KeyWait, KeyModWait: Integer);
begin
  if Pointer(FClient.SendString) <> nil then
    FClient.SendString(FTarget, PChar(Text), KeyWait, KeyModWait)
  else
    inherited SendString(Text, KeyWait, KeyModWait);
end;

procedure TEIOS_Target.SendStringEx(Text: String; MinKeyWait, MaxKeyWait: Integer);

  function GetRandomWait: Integer;
  begin
    Result := Trunc(TruncatedGauss(MinKeyWait, MaxKeyWait));
  end;

var
  Character: Char;
begin
  for Character in Text do
    SendString(Character, GetRandomWait(), GetRandomWait());
end;

procedure TEIOS_Target.HoldKey(Key: Integer);
begin
  if Pointer(FClient.HoldKey) <> nil then
    FClient.HoldKey(FTarget, Key)
  else
    inherited HoldKey(Key);
end;

procedure TEIOS_Target.ReleaseKey(Key: Integer);
begin
  if Pointer(FClient.ReleaseKey) <> nil then
    FClient.ReleaseKey(FTarget, Key)
  else
    inherited ReleaseKey(Key);
end;

function TEIOS_Target.IsKeyHeld(Key: Integer): Boolean;
begin
  if Pointer(FClient.IsKeyHeld) <> nil then
    Result := FClient.IsKeyHeld(FTarget, Key)
  else
    Result := inherited IsKeyHeld(Key);
end;

function TEIOS_Target.GetKeyCode(Character: Char): Integer;
begin
  if Pointer(FClient.GetKeyCode) <> nil then
    Result := FClient.GetKeyCode(FTarget, Character)
  else
    Result := inherited GetKeyCode(Character);
end;

end.

