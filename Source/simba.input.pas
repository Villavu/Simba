{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.input;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.internaltarget, simba.bitmap;

type
  PSimbaInput = ^TSimbaInput;
  TSimbaInput = packed record
  const
    DEFAULT_KEY_PRESS_MIN = 20;
    DEFAULT_KEY_PRESS_MAX = 100;

    DEFAULT_CLICK_MIN = 20;
    DEFAULT_CLICK_MAX = 160;
  private
    FTarget: TSimbaInternalTarget;

    function GetKeyPressMin: Integer;
    function GetKeyPressMax: Integer;

    function GetClickPressMin: Integer;
    function GetClickPressMax: Integer;
  public
    KeyPressMin: Integer;
    KeyPressMax: Integer;

    MouseClickMin: Integer;
    MouseClickMax: Integer;

    procedure SetTargetDesktop;
    procedure SetTargetWindow(Window: TWindowHandle);
    procedure SetTargetBitmap(Bitmap: TMufasaBitmap);
    procedure SetTargetEIOS(Plugin, Args: String);

    procedure GetTargetDimensions(out Width, Height: Integer);

    function IsTargetValid: Boolean;
    function IsFocused: Boolean;
    function Focus: Boolean;

    function MousePosition: TPoint;
    function MousePressed(Button: MouseButton): Boolean;
    procedure MouseClick(Button: MouseButton);
    procedure MouseTeleport(P: TPoint);
    procedure MouseDown(Button: MouseButton);
    procedure MouseUp(Button: MouseButton);
    procedure MouseScroll(Scrolls: Integer);

    procedure KeySend(Text: String);
    procedure KeyPress(Key: KeyCode);
    procedure KeyDown(Key: KeyCode);
    procedure KeyUp(Key: KeyCode);
    function KeyPressed(Key: KeyCode): Boolean;

    function CharToKeyCode(C: Char): KeyCode;

    class operator Initialize(var Self: TSimbaInput);
  end;

implementation

uses
  simba.nativeinterface;

function TSimbaInput.GetKeyPressMin: Integer;
begin
  Result := KeyPressMin;
  if (Result = 0) then
    Result := DEFAULT_KEY_PRESS_MIN;
end;

function TSimbaInput.GetKeyPressMax: Integer;
begin
  Result := KeyPressMax;
  if (Result = 0) then
    Result := DEFAULT_KEY_PRESS_MAX;
end;

function TSimbaInput.GetClickPressMin: Integer;
begin
  Result := MouseClickMin;
  if (Result = 0) then
    Result := DEFAULT_CLICK_MIN;
end;

function TSimbaInput.GetClickPressMax: Integer;
begin
  Result := MouseClickMax;
  if (Result = 0) then
    Result := DEFAULT_CLICK_MAX;
end;

procedure TSimbaInput.SetTargetDesktop;
begin
  FTarget.SetDesktop();
end;

procedure TSimbaInput.SetTargetWindow(Window: TWindowHandle);
begin
  FTarget.SetWindow(Window);
end;

procedure TSimbaInput.SetTargetBitmap(Bitmap: TMufasaBitmap);
begin
  FTarget.SetBitmap(Bitmap);
end;

procedure TSimbaInput.SetTargetEIOS(Plugin, Args: String);
begin
  FTarget.SetEIOS(Plugin, Args);
end;

procedure TSimbaInput.GetTargetDimensions(out Width, Height: Integer);
begin
  FTarget.GetDimensions(Width, Height);
end;

function TSimbaInput.IsTargetValid: Boolean;
begin
  Result := FTarget.IsValid();
end;

function TSimbaInput.IsFocused: Boolean;
begin
  Result := FTarget.IsFocused();
end;

function TSimbaInput.Focus: Boolean;
begin
  Result := FTarget.Focus();
end;

function TSimbaInput.MousePosition: TPoint;
begin
  Result := FTarget.MousePosition();
end;

function TSimbaInput.MousePressed(Button: MouseButton): Boolean;
begin
  Result := FTarget.MousePressed(Button);
end;

procedure TSimbaInput.MouseClick(Button: MouseButton);
begin
  FTarget.MouseClick(Button, GetClickPressMin(), GetClickPressMax());
end;

procedure TSimbaInput.MouseTeleport(P: TPoint);
begin
  FTarget.MouseTeleport(P);
end;

procedure TSimbaInput.MouseDown(Button: MouseButton);
begin
  FTarget.MouseDown(Button);
end;

procedure TSimbaInput.MouseUp(Button: MouseButton);
begin
  FTarget.MouseUp(Button);
end;

procedure TSimbaInput.MouseScroll(Scrolls: Integer);
begin
  FTarget.MouseScroll(Scrolls);
end;

procedure TSimbaInput.KeySend(Text: String);
begin
  FTarget.KeySend(Text, GetKeyPressMin(), GetKeyPressMax());
end;

procedure TSimbaInput.KeyPress(Key: KeyCode);
begin
  FTarget.KeyPress(Key, GetKeyPressMin(), GetKeyPressMax());
end;

procedure TSimbaInput.KeyDown(Key: KeyCode);
begin
  FTarget.KeyDown(Key);
end;

procedure TSimbaInput.KeyUp(Key: KeyCode);
begin
  FTarget.KeyUp(Key);
end;

function TSimbaInput.KeyPressed(Key: KeyCode): Boolean;
begin
  Result := FTarget.KeyPressed(Key);
end;

function TSimbaInput.CharToKeyCode(C: Char): KeyCode;
begin
  case C of
    '0'..'9': Result := KeyCode(Ord(KeyCode.NUM_0) + Ord(C) - Ord('0'));
    'a'..'z': Result := KeyCode(Ord(KeyCode.A) + Ord(C) - Ord('a'));
    'A'..'Z': Result := KeyCode(Ord(KeyCode.A) + Ord(C) - Ord('A'));
    #34, #39: Result := KeyCode.OEM_7;
    #32: Result := KeyCode.SPACE;
    '!': Result := KeyCode.NUM_1;
    '#': Result := KeyCode.NUM_3;
    '$': Result := KeyCode.NUM_4;
    '%': Result := KeyCode.NUM_5;
    '&': Result := KeyCode.NUM_7;
    '(': Result := KeyCode.NUM_9;
    ')': Result := KeyCode.NUM_0;
    '*': Result := KeyCode.NUM_8;
    '+': Result := KeyCode.ADD;
    ',': Result := KeyCode.OEM_COMMA;
    '-': Result := KeyCode.OEM_MINUS;
    '.': Result := KeyCode.OEM_PERIOD;
    '/': Result := KeyCode.OEM_2;
    ':': Result := KeyCode.OEM_1;
    ';': Result := KeyCode.OEM_1;
    '<': Result := KeyCode.OEM_COMMA;
    '=': Result := KeyCode.ADD;
    '>': Result := KeyCode.OEM_PERIOD;
    '?': Result := KeyCode.OEM_2;
    '@': Result := KeyCode.NUM_2;
    '[': Result := KeyCode.OEM_4;
    '\': Result := KeyCode.OEM_5;
    ']': Result := KeyCode.OEM_6;
    '^': Result := KeyCode.NUM_6;
    '_': Result := KeyCode.OEM_MINUS;
    '`': Result := KeyCode.OEM_3;
    '{': Result := KeyCode.OEM_4;
    '|': Result := KeyCode.OEM_5;
    '}': Result := KeyCode.OEM_6;
    '~': Result := KeyCode.OEM_3;
    else
      Result := KeyCode.UNKNOWN;
  end;
end;

class operator TSimbaInput.Initialize(var Self: TSimbaInput);
begin
  Self := Default(TSimbaInput);
end;

end.

