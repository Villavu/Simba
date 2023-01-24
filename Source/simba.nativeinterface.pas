{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.nativeinterface;

{$i simba.inc}

interface

uses
  classes, sysutils, graphics,
  simba.mufasatypes;

type
  TSimbaNativeInterface = class
  public
    procedure HoldKeyNativeKeyCode(KeyCode: Integer; WaitTime: Integer = 0); virtual; abstract;
    procedure ReleaseKeyNativeKeyCode(KeyCode: Integer; WaitTime: Integer = 0); virtual; abstract;

    function VirtualKeyToNativeKeyCode(VirtualKey: Integer): Integer; virtual; abstract;
    function GetNativeKeyCodeAndModifiers(Character: Char; out Code: Integer; out Modifiers: TShiftState): Boolean; virtual; abstract;

    function GetWindowImage(Window: TWindowHandle; X, Y, Width, Height: Integer; var ImageData: PRGB32): Boolean; virtual; abstract;
    function GetWindowBounds(Window: TWindowHandle; out Bounds: TBox): Boolean; virtual; abstract;
    function GetWindowBounds(Window: TWindowHandle): TBox; virtual; abstract;

    function GetMousePosition: TPoint; virtual; abstract;
    function GetMousePosition(Window: TWindowHandle): TPoint; virtual; abstract;
    procedure SetMousePosition(Window: TWindowHandle; Position: TPoint); virtual; abstract;
    procedure ScrollMouse(Lines: Integer); virtual; abstract;
    procedure HoldMouse(Button: TClickType); virtual; abstract;
    procedure ReleaseMouse(Button: TClickType); virtual; abstract;
    function IsMouseButtonHeld(Button: TClickType): Boolean; virtual; abstract;

    function IsKeyHeld(Key: Integer): Boolean; virtual; abstract;

    procedure HoldKey(VirtualKey: Integer; WaitTime: Integer = 0); virtual; abstract;
    procedure ReleaseKey(VirtualKey: Integer; WaitTime: Integer = 0); virtual; abstract;

    function GetProcessMemUsage(PID: SizeUInt): Int64; virtual; abstract;
    function GetProcessPath(PID: SizeUInt): String; virtual; abstract;
    function IsProcess64Bit(PID: SizeUInt): Boolean; virtual; abstract;
    function IsProcessRunning(PID: SizeUInt): Boolean; virtual; abstract;
    procedure TerminateProcess(PID: SizeUInt); virtual; abstract;

    function IsWindowActive(Window: TWindowHandle): Boolean; virtual; abstract;
    function IsWindowValid(Window: TWindowHandle): Boolean; virtual; abstract;
    function GetWindowPID(Window: TWindowHandle): Integer; virtual; abstract;
    function IsWindowVisible(Window: TWindowHandle): Boolean; virtual; abstract;
    function GetRootWindow(Window: TWindowHandle): TWindowHandle; virtual; abstract;
    function GetWindowTitle(Window: TWindowHandle): WideString; virtual; abstract;
    function GetWindowClass(Window: TWindowHandle): WideString; virtual; abstract;
    function GetWindowChildren(Window: TWindowHandle; Recursive: Boolean): TWindowHandleArray; virtual; abstract;
    function GetVisibleWindows: TWindowHandleArray; virtual; abstract;
    function GetWindows: TWindowHandleArray; virtual; abstract;
    function GetTopWindows: TWindowHandleArray; virtual; abstract;

    function GetWindowAtCursor(Exclude: TWindowHandleArray): TWindowHandle; virtual; abstract;
    function GetDesktopWindow: TWindowHandle; virtual; abstract;
    function GetActiveWindow: TWindowHandle; virtual; abstract;

    procedure SetWindowBounds(Window: TWindowHandle; Bounds: TBox); virtual; abstract;
    function ActivateWindow(Window: TWindowHandle): Boolean; virtual; abstract;

    function HighResolutionTime: Double; virtual; abstract;

    procedure OpenDirectory(Path: String); virtual; abstract;

    // Not abstract

    function WindowHandleToStr(WindowHandle: TWindowHandle): String; virtual;
    function WindowHandleFromStr(Str: String): TWindowHandle; virtual;
    procedure PreciseSleep(Milliseconds: UInt32); virtual;

    procedure PlaySound(Path: String); virtual;
    procedure StopSound; virtual;

    procedure ShowTerminal; virtual;
    procedure HideTerminal; virtual;

    procedure OpenFile(Path: String); virtual;
    procedure OpenURL(URL: String); virtual;

    function GetVirtualKeyCode(Character: Char): Integer; virtual;

    procedure SendString(Text: String; KeyWait, KeyModWait: Integer); virtual;
    procedure SendStringEx(Text: String; MinKeyWait, MaxMaxWait: Integer); virtual;
  end;

var
  SimbaNativeInterface: TSimbaNativeInterface;

implementation

{$WARN 4046 ERROR} // stop compiling on creating a class with an abstract method

{$IF (NOT DEFINED(WINDOWS)) and (NOT DEFINED(LINUX)) and (NOT DEFINED(DARWIN)))}
  {$ERROR This platform has no native interface!}
{$ENDIF}

uses
  LCLType, LCLIntf,
  simba.random,
  {$IF DEFINED(WINDOWS)}
  simba.nativeinterface_windows;
  {$ELSEIF DEFINED(LINUX)}
  simba.nativeinterface_linux;
  {$ELSEIF DEFINED(DARWIN)}
  simba.nativeinterface_darwin;
  {$ENDIF}

procedure TSimbaNativeInterface.SendString(Text: String; KeyWait, KeyModWait: Integer);
var
  Character: Char;
  KeyCode: Integer;
  KeyModifiers: TShiftState;
begin
  for Character in Text do
  begin
    if not GetNativeKeyCodeAndModifiers(Character, KeyCode, KeyModifiers) then
      raise Exception.CreateFmt('SendString: Unknown key code for "%s"', [Character]);

    if (KeyModifiers <> []) then
    begin
      if (ssShift in KeyModifiers) then HoldKey(VK_SHIFT,   KeyModWait);
      if (ssCtrl  in KeyModifiers) then HoldKey(VK_CONTROL, KeyModWait);
      if (ssAlt   in KeyModifiers) then HoldKey(VK_MENU,    KeyModWait);
    end;

    HoldKeyNativeKeyCode(KeyCode,    KeyWait);
    ReleaseKeyNativeKeyCode(KeyCode, KeyWait);

    if (KeyModifiers <> []) then
    begin
      if (ssShift in KeyModifiers) then ReleaseKey(VK_SHIFT,   KeyModWait);
      if (ssCtrl  in KeyModifiers) then ReleaseKey(VK_CONTROL, KeyModWait);
      if (ssAlt   in KeyModifiers) then ReleaseKey(VK_MENU,    KeyModWait);
    end;
  end;
end;

procedure TSimbaNativeInterface.SendStringEx(Text: String; MinKeyWait, MaxMaxWait: Integer);
var
  Character: Char;
  KeyCode: Integer;
  KeyModifiers: TShiftState;
begin
  for Character in Text do
  begin
    if not GetNativeKeyCodeAndModifiers(Character, KeyCode, KeyModifiers) then
      raise Exception.CreateFmt('SendStringEx: Unknown key code for "%s"', [Character]);

    if (KeyModifiers <> []) then
    begin
      if (ssShift in KeyModifiers) then HoldKey(VK_SHIFT,   RandomLeft(MinKeyWait, MaxMaxWait));
      if (ssCtrl  in KeyModifiers) then HoldKey(VK_CONTROL, RandomLeft(MinKeyWait, MaxMaxWait));
      if (ssAlt   in KeyModifiers) then HoldKey(VK_MENU,    RandomLeft(MinKeyWait, MaxMaxWait));
    end;

    HoldKeyNativeKeyCode(KeyCode,    RandomLeft(MinKeyWait, MaxMaxWait));
    ReleaseKeyNativeKeyCode(KeyCode, RandomLeft(MinKeyWait, MaxMaxWait));

    if (KeyModifiers <> []) then
    begin
      if (ssShift in KeyModifiers) then ReleaseKey(VK_SHIFT,   RandomLeft(MinKeyWait, MaxMaxWait));
      if (ssCtrl  in KeyModifiers) then ReleaseKey(VK_CONTROL, RandomLeft(MinKeyWait, MaxMaxWait));
      if (ssAlt   in KeyModifiers) then ReleaseKey(VK_MENU,    RandomLeft(MinKeyWait, MaxMaxWait));
    end;
  end;
end;

function TSimbaNativeInterface.WindowHandleToStr(WindowHandle: TWindowHandle): String;
begin
  Result := IntToStr(WindowHandle);
end;

function TSimbaNativeInterface.WindowHandleFromStr(Str: String): TWindowHandle;
begin
  Result := Str.ToInt64(0);
end;

procedure TSimbaNativeInterface.PreciseSleep(Milliseconds: UInt32);
begin
  Sleep(Milliseconds);
end;

procedure TSimbaNativeInterface.PlaySound(Path: String);
begin
  { not critical }
end;

procedure TSimbaNativeInterface.StopSound;
begin
  { not critical }
end;

procedure TSimbaNativeInterface.ShowTerminal;
begin
  { not critical }
end;

procedure TSimbaNativeInterface.HideTerminal;
begin
  { not critical }
end;

procedure TSimbaNativeInterface.OpenFile(Path: String);
begin
  LCLIntf.OpenDocument(Path);
end;

procedure TSimbaNativeInterface.OpenURL(URL: String);
begin
  LCLIntf.OpenURL(URL);
end;

function TSimbaNativeInterface.GetVirtualKeyCode(Character: Char): Integer;
begin
  case Character of
    '0'..'9': Result := VK_0 + Ord(Character) - Ord('0');
    'a'..'z': Result := VK_A + Ord(Character) - Ord('a');
    'A'..'Z': Result := VK_A + Ord(Character) - Ord('A');
    ' ': Result := VK_SPACE;
    '!': Result := VK_1;
    '"': Result := VK_OEM_7;
    '#': Result := VK_3;
    '$': Result := VK_4;
    '%': Result := VK_5;
    '&': Result := VK_7;
    '''': Result := VK_OEM_7;
    '(': Result := VK_9;
    ')': Result := VK_0;
    '*': Result := VK_8;
    '+': Result := VK_ADD;
    ',': Result := VK_OEM_COMMA;
    '-': Result := VK_OEM_MINUS;
    '.': Result := VK_OEM_PERIOD;
    '/': Result := VK_OEM_2;
    ':': Result := VK_OEM_1;
    ';': Result := VK_OEM_1;
    '<': Result := VK_OEM_COMMA;
    '=': Result := VK_ADD;
    '>': Result := VK_OEM_PERIOD;
    '?': Result := VK_OEM_2;
    '@': Result := VK_2;
    '[': Result := VK_OEM_4;
    '\': Result := VK_OEM_5;
    ']': Result := VK_OEM_6;
    '^': Result := VK_6;
    '_': Result := VK_OEM_MINUS;
    '`': Result := VK_OEM_3;
    '{': Result := VK_OEM_4;
    '|': Result := VK_OEM_5;
    '}': Result := VK_OEM_6;
    '~': Result := VK_OEM_3;
    else
      Result := VK_UNKNOWN;
  end;
end;

initialization
  SimbaNativeInterface := {$IF DEFINED(WINDOWS)}
                          TSimbaNativeInterface_Windows.Create();
                          {$ELSEIF DEFINED(LINUX)}
                          TSimbaNativeInterface_Linux.Create();
                          {$ELSEIF DEFINED(DARWIN)}
                          TSimbaNativeInterface_Darwin.Create();
                          {$ENDIF}

finalization
  if (SimbaNativeInterface <> nil) then
    FreeAndNil(SimbaNativeInterface)

end.

