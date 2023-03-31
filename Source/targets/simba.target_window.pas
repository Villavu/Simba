unit simba.target_window;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes;

type
  TSimbaWindowTarget = record
    Handle: TWindowHandle;

    procedure GetDimensions(out W, H: Integer);
    function GetImageData(X, Y, Width, Height: Integer; var Data: PColorBGRA; var DataWidth: Integer): Boolean;

    function Focus: Boolean;
    function IsFocused: Boolean;
    function IsValid: Boolean;

    procedure KeyDown(Key: KeyCode);
    procedure KeyUp(Key: KeyCode);
    procedure KeySend(Key: Char; KeyDownTime, KeyUpTime, ModifierDownTime, ModifierUpTime: Integer);
    function KeyPressed(Key: KeyCode): Boolean;

    procedure MouseTeleport(P: TPoint);
    function MousePosition: TPoint;
    function MousePressed(Button: MouseButton): Boolean;
    procedure MouseUp(Button: MouseButton);
    procedure MouseDown(Button: MouseButton);
    procedure MouseScroll(Scrolls: Integer);
  end;

implementation

uses
  simba.nativeinterface;

procedure TSimbaWindowTarget.GetDimensions(out W, H: Integer);
var
  B: TBox;
begin
  if SimbaNativeInterface.GetWindowBounds(Handle, B) then
  begin
    W := B.Width - 1;
    H := B.Height - 1;
  end else
  begin
    W := 0;
    H := 0;
  end;
end;

function TSimbaWindowTarget.GetImageData(X, Y, Width, Height: Integer; var Data: PColorBGRA; var DataWidth: Integer): Boolean;
begin
  Result := SimbaNativeInterface.GetWindowImage(Handle, X, Y, Width, Height, Data);
  if Result then
    DataWidth := Width;
end;

function TSimbaWindowTarget.Focus: Boolean;
begin
  Result := SimbaNativeInterface.ActivateWindow(Handle);
end;

function TSimbaWindowTarget.IsFocused: Boolean;
begin
  Result := SimbaNativeInterface.IsWindowActive(Handle);
end;

function TSimbaWindowTarget.IsValid: Boolean;
begin
  Result := SimbaNativeInterface.IsWindowValid(Handle);
end;

procedure TSimbaWindowTarget.KeyDown(Key: KeyCode);
begin
  SimbaNativeInterface.KeyDown(Key);
end;

procedure TSimbaWindowTarget.KeyUp(Key: KeyCode);
begin
  SimbaNativeInterface.KeyUp(Key);
end;

procedure TSimbaWindowTarget.KeySend(Key: Char; KeyDownTime, KeyUpTime, ModifierDownTime, ModifierUpTime: Integer);
begin
  SimbaNativeInterface.KeySend(Key, KeyDownTime, KeyUpTime, ModifierDownTime, ModifierUpTime);
end;

function TSimbaWindowTarget.KeyPressed(Key: KeyCode): Boolean;
begin
  Result := SimbaNativeInterface.KeyPressed(Key);
end;

procedure TSimbaWindowTarget.MouseTeleport(P: TPoint);
begin
  SimbaNativeInterface.MouseTeleport(Handle, P);
end;

function TSimbaWindowTarget.MousePosition: TPoint;
begin
  Result := SimbaNativeInterface.GetMousePosition(Handle);
end;

function TSimbaWindowTarget.MousePressed(Button: MouseButton): Boolean;
begin
  Result := SimbaNativeInterface.MousePressed(Button);
end;

procedure TSimbaWindowTarget.MouseUp(Button: MouseButton);
begin
  SimbaNativeInterface.MouseUp(Button);
end;

procedure TSimbaWindowTarget.MouseDown(Button: MouseButton);
begin
  SimbaNativeInterface.MouseDown(Button);
end;

procedure TSimbaWindowTarget.MouseScroll(Scrolls: Integer);
begin
  SimbaNativeInterface.MouseScroll(Scrolls);
end;

end.

