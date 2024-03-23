unit simba.target_window;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base;

procedure WindowTarget_GetDimensions(Target: Pointer; out W, H: Integer);
function WindowTarget_GetImageData(Target: Pointer; X, Y, Width, Height: Integer; var Data: PColorBGRA; var DataWidth: Integer): Boolean;

function WindowTarget_Focus(Target: Pointer): Boolean;
function WindowTarget_IsFocused(Target: Pointer): Boolean;
function WindowTarget_IsValid(Target: Pointer): Boolean;

procedure WindowTarget_KeyDown(Target: Pointer; Key: EKeyCode);
procedure WindowTarget_KeyUp(Target: Pointer; Key: EKeyCode);
procedure WindowTarget_KeySend(Target: Pointer; Text: PChar; TextLen: Integer; SleepTimes: PInt32);
function WindowTarget_KeyPressed(Target: Pointer; Key: EKeyCode): Boolean;

procedure WindowTarget_MouseTeleport(Target: Pointer; P: TPoint);
function WindowTarget_MousePosition(Target: Pointer): TPoint;
function WindowTarget_MousePressed(Target: Pointer; Button: EMouseButton): Boolean;
procedure WindowTarget_MouseUp(Target: Pointer; Button: EMouseButton);
procedure WindowTarget_MouseDown(Target: Pointer; Button: EMouseButton);
procedure WindowTarget_MouseScroll(Target: Pointer; Scrolls: Integer);

implementation

uses
  simba.nativeinterface, simba.vartype_box;

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

procedure WindowTarget_KeyDown(Target: Pointer; Key: EKeyCode);
begin
  SimbaNativeInterface.KeyDown(Key);
end;

procedure WindowTarget_KeyUp(Target: Pointer; Key: EKeyCode);
begin
  SimbaNativeInterface.KeyUp(Key);
end;

procedure WindowTarget_KeySend(Target: Pointer; Text: PChar; TextLen: Integer; SleepTimes: PInt32);
begin
  SimbaNativeInterface.KeySend(Text, TextLen, SleepTimes);
end;

function WindowTarget_KeyPressed(Target: Pointer; Key: EKeyCode): Boolean;
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

function WindowTarget_MousePressed(Target: Pointer; Button: EMouseButton): Boolean;
begin
  Result := SimbaNativeInterface.MousePressed(Button);
end;

procedure WindowTarget_MouseUp(Target: Pointer; Button: EMouseButton);
begin
  SimbaNativeInterface.MouseUp(Button);
end;

procedure WindowTarget_MouseDown(Target: Pointer; Button: EMouseButton);
begin
  SimbaNativeInterface.MouseDown(Button);
end;

procedure WindowTarget_MouseScroll(Target: Pointer; Scrolls: Integer);
begin
  SimbaNativeInterface.MouseScroll(Scrolls);
end;

end.

