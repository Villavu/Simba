{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
{$i simba.inc}

unit simba.target_window;

interface

uses
  classes, sysutils,
  simba.mufasatypes, simba.target;

type
  TWindowTarget = class(TTarget)
  protected
    FWindowHandle: TWindowHandle;
    FBuffer: PRGB32;

    procedure GetTargetBounds(out Bounds: TBox); override;
  public
    function CopyData(X, Y, Width, Height: Integer): PRGB32; override;
    function ReturnData(X, Y, Width, Height: Integer): TRetData; override;

    function TargetValid: Boolean; override;

    procedure ActivateClient; override;
    procedure GetMousePosition(out X, Y: Integer); override;
    procedure MoveMouse(X, Y: Integer); override;
    procedure ScrollMouse(X, Y, Lines: Integer); override;
    procedure HoldMouse(X, Y: Integer; Button: TClickType); override;
    procedure ReleaseMouse(X, Y: Integer; Button: TClickType); override;
    function IsMouseButtonHeld(Button: TClickType): Boolean; override;

    procedure SendStringEx(Text: String; MinKeyWait, MaxKeyWait: Integer); override;
    procedure SendString(Text: String; KeyWait, KeyModWait: Integer); override;
    procedure HoldKey(Key: Integer); override;
    procedure ReleaseKey(Key: Integer); override;
    function IsKeyHeld(Key: Integer): Boolean; override;
    function GetKeyCode(Key: Char): Integer; override;

    property WindowHandle: TWindowHandle read FWindowHandle;

    constructor Create(Handle: TWindowHandle); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  simba.nativeinterface;

function TWindowTarget.TargetValid: Boolean;
begin
  Result := SimbaNativeInterface.IsWindowValid(FWindowHandle);
end;

procedure TWindowTarget.ActivateClient;
begin
  SimbaNativeInterface.ActivateWindow(FWindowHandle);
end;

procedure TWindowTarget.GetTargetBounds(out Bounds: TBox);
begin
  SimbaNativeInterface.GetWindowBounds(FWindowHandle, Bounds);
end;

function TWindowTarget.ReturnData(X, Y, Width, Height: Integer): TRetData;
var
  Bounds: TBox;
begin
  Result := Default(TRetData);

  if ValidateImageCapture(X, Y, Width, Height, Bounds) and SimbaNativeInterface.GetWindowImage(FWindowHandle, X, Y, Width, Height, FBuffer) then
  begin
    Result.Ptr := FBuffer;
    Result.IncPtrWith := 0;
    Result.RowLen := Width;
  end;
end;

function TWindowTarget.CopyData(X, Y, Width, Height: Integer): PRGB32;
var
  Bounds: TBox;
begin
  Result := nil;

  if ValidateImageCapture(X, Y, Width, Height, Bounds) then
    SimbaNativeInterface.GetWindowImage(FWindowHandle, X, Y, Width, Height, Result);
end;

procedure TWindowTarget.GetMousePosition(out X, Y: Integer);
var
  Position: TPoint;
begin
  Position := SimbaNativeInterface.GetMousePosition(FWindowHandle);

  X := Position.X;
  Y := Position.Y;

  if FMouseClientAreaSet then
  begin
    X := X - FMouseClientArea.X1;
    Y := Y - FMouseClientArea.Y1;
  end;
end;

procedure TWindowTarget.MoveMouse(X, Y: Integer);
begin
  MouseClientAreaOffset(X, Y);

  SimbaNativeInterface.SetMousePosition(FWindowHandle, TPoint.Create(X, Y));
end;

procedure TWindowTarget.ScrollMouse(X, Y, Lines: Integer);
begin
  MoveMouse(X, Y);

  SimbaNativeInterface.ScrollMouse(Lines);
end;

procedure TWindowTarget.HoldMouse(X, Y: Integer; Button: TClickType);
begin
  MoveMouse(X, Y);

  SimbaNativeInterface.HoldMouse(Button);
end;

procedure TWindowTarget.ReleaseMouse(X, Y: Integer; Button: TClickType);
begin
  MoveMouse(X, Y);

  SimbaNativeInterface.ReleaseMouse(Button);
end;

function TWindowTarget.IsMouseButtonHeld(Button: TClickType): Boolean;
begin
  Result := SimbaNativeInterface.IsMouseButtonHeld(Button);
end;

procedure TWindowTarget.SendString(Text: String; KeyWait, KeyModWait: Integer);
begin
  SimbaNativeInterface.SendString(Text, KeyWait, KeyModWait);
end;

procedure TWindowTarget.SendStringEx(Text: String; MinKeyWait, MaxKeyWait: Integer);
begin
  SimbaNativeInterface.SendStringEx(Text, MinKeyWait, MaxKeyWait);
end;

procedure TWindowTarget.HoldKey(Key: Integer);
begin
  SimbaNativeInterface.HoldKey(Key);
end;

procedure TWindowTarget.ReleaseKey(Key: Integer);
begin
  SimbaNativeInterface.ReleaseKey(Key);
end;

function TWindowTarget.IsKeyHeld(Key: Integer): Boolean;
begin
  Result := SimbaNativeInterface.IsKeyHeld(Key);
end;

function TWindowTarget.GetKeyCode(Key: Char): Integer;
begin
  Result := SimbaNativeInterface.GetVirtualKeyCode(Key);
end;

constructor TWindowTarget.Create(Handle: TWindowHandle);
begin
  inherited Create();

  FWindowHandle := Handle;
end;

destructor TWindowTarget.Destroy;
begin
  if (FBuffer <> nil) then
    FreeMemAndNil(FBuffer);

  inherited Destroy();
end;

end.

