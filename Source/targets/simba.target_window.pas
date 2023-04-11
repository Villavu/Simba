unit simba.target_window;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes;

procedure WindowTarget_GetDimensions(Target: Pointer; out W, H: Integer);
function WindowTarget_GetImageData(Target: Pointer; X, Y, Width, Height: Integer; var Data: PColorBGRA; var DataWidth: Integer): Boolean;

function WindowTarget_Focus(Target: Pointer): Boolean;
function WindowTarget_IsFocused(Target: Pointer): Boolean;
function WindowTarget_IsValid(Target: Pointer): Boolean;

procedure WindowTarget_KeyDown(Target: Pointer; Key: KeyCode);
procedure WindowTarget_KeyUp(Target: Pointer; Key: KeyCode);
procedure WindowTarget_KeySend(Target: Pointer; Key: Char; KeyDownTime, KeyUpTime, ModifierDownTime, ModifierUpTime: Integer);
function WindowTarget_KeyPressed(Target: Pointer; Key: KeyCode): Boolean;

procedure WindowTarget_MouseTeleport(Target: Pointer; P: TPoint);
function WindowTarget_MousePosition(Target: Pointer): TPoint;
function WindowTarget_MousePressed(Target: Pointer; Button: MouseButton): Boolean;
procedure WindowTarget_MouseUp(Target: Pointer; Button: MouseButton);
procedure WindowTarget_MouseDown(Target: Pointer; Button: MouseButton);
procedure WindowTarget_MouseScroll(Target: Pointer; Scrolls: Integer);

implementation

uses
  simba.nativeinterface;

procedure WindowTarget_GetDimensions(Target: Pointer; out W, H: Integer);
var
  B: TBox;
begin
  if SimbaNativeInterface.GetWindowBounds(PWindowHandle(Target)^, B) then
  begin
    W := B.Width - 1;
    H := B.Height - 1;
  end else
  begin
    W := 0;
    H := 0;
  end;
end;

function WindowTarget_GetImageData(Target: Pointer; X, Y, Width, Height: Integer; var Data: PColorBGRA; var DataWidth: Integer): Boolean;
begin
  Result := SimbaNativeInterface.GetWindowImage(PWindowHandle(Target)^, X, Y, Width, Height, Data);
  if Result then
    DataWidth := Width;
end;

function WindowTarget_Focus(Target: Pointer): Boolean;
begin
  Result := SimbaNativeInterface.ActivateWindow(PWindowHandle(Target)^);
end;

function WindowTarget_IsFocused(Target: Pointer): Boolean;
begin
  Result := SimbaNativeInterface.IsWindowActive(PWindowHandle(Target)^);
end;

function WindowTarget_IsValid(Target: Pointer): Boolean;
begin
  Result := SimbaNativeInterface.IsWindowValid(PWindowHandle(Target)^);
end;

procedure WindowTarget_KeyDown(Target: Pointer; Key: KeyCode);
begin
  SimbaNativeInterface.KeyDown(Key);
end;

procedure WindowTarget_KeyUp(Target: Pointer; Key: KeyCode);
begin
  SimbaNativeInterface.KeyUp(Key);
end;

procedure WindowTarget_KeySend(Target: Pointer; Key: Char; KeyDownTime, KeyUpTime, ModifierDownTime, ModifierUpTime: Integer);
begin
  SimbaNativeInterface.KeySend(Key, KeyDownTime, KeyUpTime, ModifierDownTime, ModifierUpTime);
end;

function WindowTarget_KeyPressed(Target: Pointer; Key: KeyCode): Boolean;
begin
  Result := SimbaNativeInterface.KeyPressed(Key);
end;

procedure WindowTarget_MouseTeleport(Target: Pointer; P: TPoint);
begin
  SimbaNativeInterface.MouseTeleport(PWindowHandle(Target)^, P);
end;

function WindowTarget_MousePosition(Target: Pointer): TPoint;
begin
  Result := SimbaNativeInterface.GetMousePosition(PWindowHandle(Target)^);
end;

function WindowTarget_MousePressed(Target: Pointer; Button: MouseButton): Boolean;
begin
  Result := SimbaNativeInterface.MousePressed(Button);
end;

procedure WindowTarget_MouseUp(Target: Pointer; Button: MouseButton);
begin
  SimbaNativeInterface.MouseUp(Button);
end;

procedure WindowTarget_MouseDown(Target: Pointer; Button: MouseButton);
begin
  SimbaNativeInterface.MouseDown(Button);
end;

procedure WindowTarget_MouseScroll(Target: Pointer; Scrolls: Integer);
begin
  SimbaNativeInterface.MouseScroll(Scrolls);
end;

end.

