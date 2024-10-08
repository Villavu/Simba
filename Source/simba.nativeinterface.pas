{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.nativeinterface;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base;

type
  TSimbaNativeInterface = class
  public
    function GetWindowImage(Window: TWindowHandle; X, Y, Width, Height: Integer; var ImageData: PColorBGRA): Boolean; virtual; abstract;
    function GetWindowBounds(Window: TWindowHandle; out Bounds: TBox): Boolean; virtual; abstract;
    function GetWindowBounds(Window: TWindowHandle): TBox; virtual; abstract;

    procedure MouseUp(Button: EMouseButton); virtual; abstract;
    procedure MouseDown(Button: EMouseButton); virtual; abstract;
    procedure MouseScroll(Scrolls: Integer); virtual; abstract;
    procedure MouseTeleport(RelativeWindow: TWindowHandle; P: TPoint); virtual; abstract;
    function MousePressed(Button: EMouseButton): Boolean; virtual; abstract;

    function GetMousePosition: TPoint; virtual; abstract;
    function GetMousePosition(Window: TWindowHandle): TPoint; virtual; abstract;

    procedure KeySend(Text: PChar; TextLen: Integer; SleepTimes: PInt32); virtual; abstract;
    function KeyPressed(Key: EKeyCode): Boolean; virtual; abstract;
    procedure KeyDown(Key: EKeyCode); virtual; abstract;
    procedure KeyUp(Key: EKeyCode); virtual; abstract;

    function GetProcessStartTime(PID: SizeUInt): TDateTime; virtual; abstract;
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

    procedure OpenFile(Path: String); virtual;
    procedure OpenURL(URL: String); virtual;

    function GetVirtualKeyCode(Character: Char): Integer; virtual;
  end;

var
  SimbaNativeInterface: TSimbaNativeInterface;

implementation

{$IF (NOT DEFINED(WINDOWS)) and (NOT DEFINED(LINUX)) and (NOT DEFINED(DARWIN)))}
  {$ERROR This platform has no native interface!}
{$ENDIF}

uses
  LCLType, LCLIntf,
  simba.vartype_string,
  {$IF DEFINED(WINDOWS)}
  simba.nativeinterface_windows;
  {$ELSEIF DEFINED(LINUX)}
  simba.nativeinterface_linux;
  {$ELSEIF DEFINED(DARWIN)}
  simba.nativeinterface_darwin;
  {$ENDIF}

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

