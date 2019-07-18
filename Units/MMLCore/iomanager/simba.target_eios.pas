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

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils,
  simba.target, mufasatypes, libloader, dynlibs;

type
  PEIOS_Client = ^TEIOS_Client;
  TEIOS_Client = record
    RequestTarget: function(Data: PChar): Pointer; stdcall;
    ReleaseTarget: procedure(Target: Pointer); stdcall;

    GetTargetDimensions: procedure(Target: Pointer; var Width, Height: Int32); stdcall;
    GetTargetPosition: procedure(var Left, Top: Int32); stdcall;
    GetImageBuffer: function(Target: Pointer): PRGB32; stdcall;
    UpdateImageBuffer: procedure(Target: Pointer); stdcall;
    UpdateImageBufferEx: function(Target: Pointer): PRGB32; stdcall;
    UpdateImageBufferBounds: procedure(Target: Pointer; X1, Y1, X2, Y2: Int32); stdcall;

    GetMousePosition: procedure(Target: Pointer; var X, Y: Int32); stdcall;
    MoveMouse: procedure(Target: Pointer; X, Y: Int32); stdcall;
    ScrollMouse: procedure(target : pointer; X, Y: Int32; Lines: Int32); stdcall;
    HoldMouse: procedure(Target: Pointer; X, Y: Int32; Button: Int32); stdcall;
    ReleaseMouse: procedure(Target: Pointer; X, Y: Int32; Button: Int32); stdcall;
    IsMouseButtonHeld: function(Target: Pointer; Button: Int32): Boolean; stdcall;

    SendString: procedure(Target: Pointer; str: PChar; KeyWait, KeyModWait: Int32); stdcall;
    HoldKey: procedure(Target: Pointer; Key: Int32); stdcall;
    ReleaseKey: procedure(Target: Pointer; Key: Int32); stdcall;
    IsKeyHeld: function(Target: Pointer; Key: Int32): Boolean; stdcall;
    GetKeyCode: function(Target: Pointer; Character: Char): Int32; stdcall;
  end;

  PEIOS_Controller = ^TEIOS_Client;
  TEIOS_Controller = class(TGenericLoader)
  protected
    Clients: array of TEIOS_Client;

    function InitPlugin(Plugin: TLibHandle): Boolean; override;
    function FindClient(Name: String): Int32;
  public
    function ClientExists(Name: String): Boolean;
    function GetClient(Name: String): TEIOS_Client;
  end;

  PEIOS_Target = ^TEIOS_Target;
  TEIOS_Target = class(TTarget)
  protected
    FClient: TEIOS_Client;
    FTarget: Pointer;
    FBuffer: PRGB32;
  public
    constructor Create(Client: TEIOS_Client; Data: String);
    destructor Destroy; override;

    procedure GetTargetDimensions(out Width, Height: Int32); override;
    procedure GetTargetPosition(out Left, Top: Int32); override;
    function ReturnData(X, Y, Width, Height: Int32): TRetData; override;
    function CopyData(X, Y, Width, Height: Int32): PRGB32; override;

    procedure GetMousePosition(out X, Y: Int32); override;
    procedure MoveMouse(X, Y: Int32); override;
    procedure ScrollMouse(X, Y: Int32; Lines: Int32); override;
    procedure HoldMouse(X, Y: Int32; Button: TClickType); override;
    procedure ReleaseMouse(X, Y: Int32; Button: TClickType); override;
    function  IsMouseButtonHeld(Button: TClickType) : Boolean;override;

    procedure SendString(Text: String; KeyWait, KeyModWait: Int32); override;
    procedure HoldKey(Key: Int32); override;
    procedure ReleaseKey(Key: Int32); override;
    function IsKeyHeld(Key: Int32): Boolean; override;
    function GetKeyCode(Character: Char) : Int32; override;
    function GetHandle: PtrUInt; override;
  end;

var
  EIOSController: TEIOS_Controller;

implementation

function TEIOS_Controller.InitPlugin(Plugin: TLibHandle): Boolean;
var
  Count: Int32;
begin
  Count := Length(Clients);
  SetLength(Clients, Count + 1);

  with Clients[Count] do
  begin
    Pointer(RequestTarget) := GetProcAddress(Plugin, PChar('EIOS_RequestTarget'));
    Pointer(ReleaseTarget) := GetProcAddress(Plugin, PChar('EIOS_ReleaseTarget'));

    Pointer(GetTargetDimensions) := GetProcAddress(Plugin, PChar('EIOS_GetTargetDimensions'));
    Pointer(GetImageBuffer) := GetProcAddress(Plugin, PChar('EIOS_GetImageBuffer'));
    Pointer(UpdateImageBuffer) := GetProcAddress(Plugin, PChar('EIOS_UpdateImageBuffer'));
    Pointer(UpdateImageBufferEx) := GetProcAddress(Plugin, PChar('EIOS_UpdateImageBufferEx'));
    Pointer(UpdateImageBufferBounds) := GetProcAddress(Plugin, PChar('EIOS_UpdateImageBufferBounds'));

    Pointer(GetMousePosition) := GetProcAddress(Plugin, PChar('EIOS_GetMousePosition'));
    Pointer(MoveMouse) := GetProcAddress(Plugin, PChar('EIOS_MoveMouse'));
    Pointer(ScrollMouse) := GetProcAddress(Plugin,PChar('EIOS_ScrollMouse'));
    Pointer(HoldMouse) := GetProcAddress(Plugin, PChar('EIOS_HoldMouse'));
    Pointer(ReleaseMouse) := GetProcAddress(Plugin, PChar('EIOS_ReleaseMouse'));

    Pointer(SendString) := GetProcAddress(Plugin, PChar('EIOS_SendString'));
    Pointer(HoldKey) := GetProcAddress(Plugin, PChar('EIOS_HoldKey'));
    Pointer(ReleaseKey) := GetProcAddress(Plugin, PChar('EIOS_ReleaseKey'));
    Pointer(IsKeyHeld) := GetProcAddress(Plugin, PChar('EIOS_IsKeyHeld'));
  end;

  Result := True;
end;

function TEIOS_Controller.FindClient(Name: String): Int32;
begin
  Result := LoadPlugin(Name);
end;

function TEIOS_Controller.ClientExists(Name: String): Boolean;
begin
  Result := FindClient(Name) >= 0;
end;

function TEIOS_Controller.GetClient(Name: String): TEIOS_Client;
var
  i: Int32;
begin
  i := FindClient(Name);
  if i >= 0 then
    Result := Clients[i]
end;

constructor TEIOS_Target.Create(Client: TEIOS_Client; Data: String);
begin
  inherited Create();

  FClient := Client;
  FTarget := nil;
  FBuffer := nil;

  if Pointer(FClient.RequestTarget) <> nil then
  begin
    FTarget := FClient.RequestTarget(PChar(Data));
    if (FTarget = nil) then
      raise Exception.Create('EIOS RequestTarget failed');
  end else
    raise Exception.Create('No EIOS RequestTarget available');

  if Pointer(FClient.GetImageBuffer) <> nil then
    FBuffer := FClient.GetImageBuffer(FTarget);
end;

destructor TEIOS_Target.Destroy;
begin
  if (FClient.ReleaseTarget <> nil) and (FTarget <> nil) then
    FClient.ReleaseTarget(FTarget);

  inherited Destroy();
end;

procedure TEIOS_Target.GetTargetDimensions(out Width, Height: Int32);
begin
  if Pointer(FClient.GetTargetDimensions) <> nil then
  begin
    if FImageClientAreaSet then
    begin
      Width := FImageClientArea.X2 - FImageClientArea.X1;
      Height := FImageClientArea.Y2 - FImageClientArea.Y1;
    end else
      FClient.GetTargetDimensions(FTarget, Width, Height);
  end else
    inherited GetTargetDimensions(Width, Height);
end;

procedure TEIOS_Target.GetTargetPosition(out Left, Top: Int32);
begin
  if Pointer(FClient.GetTargetDimensions) <> nil then
    FClient.GetTargetDimensions(FTarget, Left, Top)
  else
    inherited GetTargetDimensions(Left, Top);
end;

function TEIOS_Target.ReturnData(X, Y, Width, Height: Int32): TRetData;
var
  Bounds: TBox;
  Loop: Int32;
begin
  ImageClientAreaOffset(X, Y);

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
    Bounds := GetClientBounds();

    if Bounds.Contains(X, Y, Width, Height) then
    begin
      Result.Ptr := @FBuffer[Y * Bounds.X2 + X];
      Result.RowLen := Bounds.X2;
      Result.IncPtrWith := Bounds.X2 - Width;
    end else
      Result := NullReturnData;
  end else
    Result := NullReturnData;
end;

function TEIOS_Target.CopyData(X, Y, Width, Height: Int32): PRGB32;
var
  Bounds: TBox;
  Loop: Int32;
begin
  ImageClientAreaOffset(X, Y);

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
    Bounds := GetClientBounds();

    if Bounds.Contains(X, Y, Width, Height) then
    begin
      Result := GetMem(Width * Height * SizeOf(TRGB32));
      for Loop := 0 to Height - 1 do
        Move(FBuffer[(Y + Loop) * Bounds.X2 + X], Result[Loop * Width], Width * SizeOf(TRGB32));
    end else
      Result := nil;
  end else
    Result := nil;
end;

procedure TEIOS_Target.GetMousePosition(out X, Y: Int32);
begin
  if Pointer(FClient.GetMousePosition) <> nil then
  begin
    FClient.GetMousePosition(FTarget, X, Y);

    X := X - FMouseClientArea.X1;
    Y := Y - FMouseClientArea.Y1;
  end else
    inherited GetMousePosition(X, Y);
end;

procedure TEIOS_Target.MoveMouse(X, Y: Int32);
begin
  MouseClientAreaOffset(X, Y);

  if Pointer(FClient.MoveMouse) <> nil then
    FClient.MoveMouse(FTarget, X, Y)
  else
    inherited MoveMouse(X, Y);
end;

procedure TEIOS_Target.ScrollMouse(X, Y: Int32; Lines: Int32);
begin
  MouseClientAreaOffset(X, Y);

  if Pointer(FClient.ScrollMouse) <> nil then
    FClient.ScrollMouse(FTarget, X, Y, Lines)
  else
    inherited Scrollmouse(X, Y, Lines);
end;

procedure TEIOS_Target.HoldMouse(X, Y: Int32; Button: TClickType);
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

procedure TEIOS_Target.ReleaseMouse(X, Y: Int32; Button: TClickType);
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

procedure TEIOS_Target.SendString(Text: String; KeyWait, KeyModWait: Int32);
begin
  if Pointer(FClient.SendString) <> nil then
    FClient.SendString(FTarget, PChar(Text), KeyWait, KeyModWait)
  else
    inherited SendString(Text, KeyWait, KeyModWait);
end;

procedure TEIOS_Target.HoldKey(Key: Int32);
begin
  if Pointer(FClient.HoldKey) <> nil then
    FClient.HoldKey(FTarget, Key)
  else
    inherited HoldKey(Key);
end;

procedure TEIOS_Target.ReleaseKey(Key: Int32);
begin
  if Pointer(FClient.ReleaseKey) <> nil then
    FClient.ReleaseKey(FTarget, Key)
  else
    inherited ReleaseKey(Key);
end;

function TEIOS_Target.IsKeyHeld(Key: Int32): Boolean;
begin
  if Pointer(FClient.IsKeyHeld) <> nil then
    Result := FClient.IsKeyHeld(FTarget, Key)
  else
    Result := inherited IsKeyHeld(Key);
end;

function TEIOS_Target.GetKeyCode(Character: Char): Int32;
begin
  if Pointer(FClient.GetKeyCode) <> nil then
    Result := FClient.GetKeyCode(FTarget, Character)
  else
    Result := inherited GetKeyCode(Character);
end;

function TEIOS_Target.GetHandle: PtrUInt;
begin
  Result := PtrUInt(FTarget);
end;

initialization
  EIOSController := TEIOS_Controller.Create();

finalization
  EIOSController.Free();

end.

