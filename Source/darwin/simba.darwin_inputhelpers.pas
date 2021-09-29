{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.darwin_inputhelpers;

{$i simba.inc}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  macosall;

type
  TWindowInfo = record
    id: CGWindowID;
    title: String;
    x: Integer;
    y: Integer;
    width: Integer;
    height: Integer;
    alpha: Single;
    onScreen: Boolean;
    layer: Integer;
    memoryUsage: Integer;
    ownerName: String;
    ownerPid: Integer;
    sharingState: Boolean;
    storeType: Integer;
  end;

  TWindow = type PtrUInt;
  TWindowArray = array of TWindow;

  //Input functions
  function VirtualKeyCodeToMac(AKey: Word): Word;
  function VirtualKeyCodeToCharCode(AKey: Word): Word;

  //Window functions
  function isWindowActive(windowId: CGWindowID): Boolean;
  function SetWindowActive(windowId: CGWindowID): Boolean;
  procedure GetCursorPos(out X, Y: Int32);
  function IsMouseButtonDown(Button: CGMouseButton): Boolean;
  function GetWindowInfo(windowId: CGWindowID): TWindowInfo;
  function GetOnScreenWindows(): TWindowArray;
  function GetChildWindows(parent: CGWindowID): TWindowArray;

  //AX functions
  function GetChildWindowsAX(parent: CGWindowID): TWindowArray;
  function GetActiveWindowAX(): CGWindowID;
  procedure SetWindowBoundsAX(window: CGWindowID; X, Y, Width, Height: Integer);

  //Private API..
  function AXUIElementGetWindow(element: AXUIElementRef; var windowId: CGWindowID): AXError; external name '__AXUIElementGetWindow';

implementation

uses
  LCLType, cocoaall, cocoautils;

type
  NSWorkspaceFix = objccategory external (NSWorkspace)
    function frontmostApplication(): NSRunningApplication; message 'frontmostApplication';
  end;

function VirtualKeyCodeToMac(AKey: Word): Word;
begin
  case AKey of
    VK_LBUTTON:             Result := $FFFF;
    VK_RBUTTON:             Result := $FFFF;
    VK_CANCEL:              Result := $FFFF;
    VK_MBUTTON:             Result := $FFFF;
    VK_XBUTTON1:            Result := $FFFF;
    VK_XBUTTON2:            Result := $FFFF;
    VK_BACK:                Result := $33;
    VK_TAB:                 Result := $30;
    VK_CLEAR:               Result := $47;
    VK_RETURN:              Result := $24;  //OR: VK_KEYPAD_ENTER = $4C
    VK_SHIFT:               Result := $38;
    VK_CONTROL:             Result := $3B;
    VK_MENU:                Result := $3A; //Left Option
    VK_PAUSE:               Result := $FFFF; //No Idea
    VK_CAPITAL:             Result := $39;
    VK_KANA:                Result := $68;
    //VK_HANGUEL:             Result := $68;
    //VK_IME_ON:              Result := $FFFF;
    VK_JUNJA:               Result := $FFFF;
    VK_FINAL:               Result := $FFFF;
    VK_HANJA:               Result := $FFFF;
    //VK_KANJI:               Result := $FFFF;
    //VK_IME_OFF:             Result := $FFFF;
    VK_ESCAPE:              Result := $35;
    VK_CONVERT:             Result := $FFFF;
    VK_NONCONVERT:          Result := $FFFF;
    VK_ACCEPT:              Result := $FFFF;
    VK_MODECHANGE:          Result := $FFFF;
    VK_SPACE:               Result := $31;
    VK_PRIOR:               Result := $74;
    VK_NEXT:                Result := $79;
    VK_END:                 Result := $77;
    VK_HOME:                Result := $73;
    VK_LEFT:                Result := $7B;
    VK_UP:                  Result := $7E;
    VK_RIGHT:               Result := $7C;
    VK_DOWN:                Result := $7D;
    VK_SELECT:              Result := $FFFF;
    VK_PRINT:               Result := $FFFF;
    VK_EXECUTE:             Result := $FFFF;
    VK_SNAPSHOT:            Result := $FFFF;
    VK_INSERT:              Result := $FFFF;
    VK_DELETE:              Result := $75;
    VK_HELP:                Result := $72;
    VK_0:                   Result := $1D;
    VK_1:                   Result := $12;
    VK_2:                   Result := $13;
    VK_3:                   Result := $14;
    VK_4:                   Result := $15;
    VK_5:                   Result := $17;
    VK_6:                   Result := $16;
    VK_7:                   Result := $1A;
    VK_8:                   Result := $1C;
    VK_9:                   Result := $19;
    VK_A:                   Result := $00;
    VK_B:                   Result := $0B;
    VK_C:                   Result := $08;
    VK_D:                   Result := $02;
    VK_E:                   Result := $0E;
    VK_F:                   Result := $03;
    VK_G:                   Result := $05;
    VK_H:                   Result := $04;
    VK_I:                   Result := $22;
    VK_J:                   Result := $26;
    VK_K:                   Result := $28;
    VK_L:                   Result := $25;
    VK_M:                   Result := $2E;
    VK_N:                   Result := $2D;
    VK_O:                   Result := $1F;
    VK_P:                   Result := $23;
    VK_Q:                   Result := $0C;
    VK_R:                   Result := $0F;
    VK_S:                   Result := $01;
    VK_T:                   Result := $11;
    VK_U:                   Result := $20;
    VK_V:                   Result := $09;
    VK_W:                   Result := $0D;
    VK_X:                   Result := $07;
    VK_Y:                   Result := $10;
    VK_Z:                   Result := $06;
    VK_LWIN:                Result := $37; //Left Command
    VK_RWIN:                Result := $36; //Right Command
    VK_APPS:                Result := $3D; //Right Option???
    VK_SLEEP:               Result := $FFFF;
    VK_NUMPAD0:             Result := $52;
    VK_NUMPAD1:             Result := $53;
    VK_NUMPAD2:             Result := $54;
    VK_NUMPAD3:             Result := $55;
    VK_NUMPAD4:             Result := $56;
    VK_NUMPAD5:             Result := $57;
    VK_NUMPAD6:             Result := $58;
    VK_NUMPAD7:             Result := $59;
    VK_NUMPAD8:             Result := $5B;
    VK_NUMPAD9:             Result := $5C;
    VK_MULTIPLY:            Result := $43;
    VK_ADD:                 Result := $45;
    VK_SEPARATOR:           Result := $2B; //Separator used will be VK_COMMA instead of VK_PERIOD
    VK_SUBTRACT:            Result := $4E;
    VK_DECIMAL:             Result := $41;
    VK_DIVIDE:              Result := $4B;
    VK_F1:                  Result := $7A;
    VK_F2:                  Result := $78;
    VK_F3:                  Result := $63;
    VK_F4:                  Result := $76;
    VK_F5:                  Result := $60;
    VK_F6:                  Result := $61;
    VK_F7:                  Result := $62;
    VK_F8:                  Result := $64;
    VK_F9:                  Result := $65;
    VK_F10:                 Result := $6D;
    VK_F11:                 Result := $67;
    VK_F12:                 Result := $6F;
    VK_F13:                 Result := $69;
    VK_F14:                 Result := $6B;
    VK_F15:                 Result := $71;
    VK_F16:                 Result := $6A;
    VK_F17:                 Result := $40;
    VK_F18:                 Result := $4F;
    VK_F19:                 Result := $50;
    VK_F20:                 Result := $5A;
    VK_F21:                 Result := $FFFF;
    VK_F22:                 Result := $FFFF;
    VK_F23:                 Result := $FFFF;
    VK_F24:                 Result := $FFFF;
    VK_NUMLOCK:             Result := $FFFF; //OR VK_NUMPAD_CLEAR = $47
    VK_SCROLL:              Result := $FFFF; //No idea..
    VK_LSHIFT:              Result := $38;
    VK_RSHIFT:              Result := $3C;
    VK_LCONTROL:            Result := $3B;
    VK_RCONTROL:            Result := $3E;
    VK_LMENU:               Result := $3A;  //Left Option Key
    VK_RMENU:               Result := $3D;  //Right Option Key
    VK_BROWSER_BACK:        Result := $FFFF;
    VK_BROWSER_REFRESH:     Result := $FFFF;
    VK_BROWSER_STOP:        Result := $FFFF;
    VK_BROWSER_SEARCH:      Result := $FFFF;
    VK_BROWSER_FAVORITES:   Result := $FFFF;
    VK_BROWSER_HOME:        Result := $FFFF;
    VK_VOLUME_MUTE:         Result := $4A;
    VK_VOLUME_DOWN:         Result := $49;
    VK_VOLUME_UP:           Result := $48;
    VK_MEDIA_NEXT_TRACK:    Result := $FFFF;
    VK_MEDIA_PREV_TRACK:    Result := $FFFF;
    VK_MEDIA_STOP:          Result := $FFFF;
    VK_MEDIA_PLAY_PAUSE:    Result := $FFFF;
    VK_LAUNCH_MAIL:         Result := $FFFF;
    VK_LAUNCH_MEDIA_SELECT: Result := $FFFF;
    VK_LAUNCH_APP1:         Result := $FFFF;
    VK_LAUNCH_APP2:         Result := $FFFF;
    VK_OEM_1:               Result := $29;  // ;:
    VK_OEM_PLUS:            Result := $45;
    VK_OEM_COMMA:           Result := $2B;
    VK_OEM_MINUS:           Result := $1B;
    VK_OEM_PERIOD:          Result := $2F;
    VK_OEM_2:               Result := $2C;  // /?
    VK_OEM_3:               Result := $32;  // `~
    VK_OEM_4:               Result := $21;  // [{
    VK_OEM_5:               Result := $2A;  // \|
    VK_OEM_6:               Result := $1E;  // ]}
    VK_OEM_7:               Result := $27;  // '"
    VK_OEM_8:               Result := $FFFF; //Misc
    $E1:                    Result := $18;  //VK_EQUAL
    VK_OEM_102:             Result := $2A;  // backslash RT-102
    $E3:                    Result := $51;  //VK_KEYPAD_EQUALS
    $E4:                    Result := $3F;  //VK_FUNCTION
    VK_PROCESSKEY:          Result := $FFFF;
    //VK_PACKET:              Result := $FFFF;
    VK_ATTN:                Result := $FFFF;
    VK_CRSEL:               Result := $FFFF;
    VK_EXSEL:               Result := $FFFF;
    VK_EREOF:               Result := $FFFF;
    VK_PLAY:                Result := $FFFF; //No idea
    VK_ZOOM:                Result := $FFFF;
    VK_NONAME:              Result := $FFFF;
    VK_PA1:                 Result := $FFFF;
    VK_OEM_CLEAR:           Result := $47;
  else
    Result := $FFFF;
  end;
end;

function VirtualKeyCodeToCharCode(AKey: Word): Word;
begin
  case AKey of
    VK_CANCEL:              Result := $18;
    VK_BACK:                Result := $08;
    VK_TAB:                 Result := $09;
    VK_RETURN:              Result := $0A;
    VK_ESCAPE:              Result := $1B;
    VK_SPACE:               Result := $20;
    VK_0:                   Result := $30;
    VK_1:                   Result := $31;
    VK_2:                   Result := $32;
    VK_3:                   Result := $33;
    VK_4:                   Result := $34;
    VK_5:                   Result := $35;
    VK_6:                   Result := $36;
    VK_7:                   Result := $37;
    VK_8:                   Result := $38;
    VK_9:                   Result := $39;
    VK_A:                   Result := $41;
    VK_B:                   Result := $42;
    VK_C:                   Result := $43;
    VK_D:                   Result := $44;
    VK_E:                   Result := $45;
    VK_F:                   Result := $46;
    VK_G:                   Result := $47;
    VK_H:                   Result := $48;
    VK_I:                   Result := $49;
    VK_J:                   Result := $4A;
    VK_K:                   Result := $4B;
    VK_L:                   Result := $4C;
    VK_M:                   Result := $4D;
    VK_N:                   Result := $4E;
    VK_O:                   Result := $4F;
    VK_P:                   Result := $50;
    VK_Q:                   Result := $51;
    VK_R:                   Result := $52;
    VK_S:                   Result := $53;
    VK_T:                   Result := $54;
    VK_U:                   Result := $55;
    VK_V:                   Result := $56;
    VK_W:                   Result := $57;
    VK_X:                   Result := $58;
    VK_Y:                   Result := $59;
    VK_Z:                   Result := $5A;
    VK_NUMPAD0:             Result := $30;
    VK_NUMPAD1:             Result := $31;
    VK_NUMPAD2:             Result := $32;
    VK_NUMPAD3:             Result := $33;
    VK_NUMPAD4:             Result := $34;
    VK_NUMPAD5:             Result := $35;
    VK_NUMPAD6:             Result := $36;
    VK_NUMPAD7:             Result := $37;
    VK_NUMPAD8:             Result := $38;
    VK_NUMPAD9:             Result := $39;
    VK_MULTIPLY:            Result := $2A;
    VK_ADD:                 Result := $2B;
    VK_SEPARATOR:           Result := $2C; //Separator used will be VK_COMMA instead of VK_PERIOD
    VK_SUBTRACT:            Result := $2D;
    VK_DECIMAL:             Result := $2E;
    VK_DIVIDE:              Result := $2F;
    VK_OEM_1:               Result := $3B;  // ;
    VK_OEM_PLUS:            Result := $2B;
    VK_OEM_MINUS:           Result := $2D;
    VK_OEM_PERIOD:          Result := $2E;
    VK_OEM_2:               Result := $2F;  // /
    VK_OEM_3:               Result := $60;  // `
    VK_OEM_4:               Result := $5B;  // [
    VK_OEM_5:               Result := $5C;  // \
    VK_OEM_6:               Result := $5D;  // ]
    VK_OEM_7:               Result := $27;  // '
    $E1:                    Result := $3D;  //VK_EQUAL
    VK_OEM_102:             Result := $5C;  // backslash RT-102
    $E3:                    Result := $3D;  //VK_KEYPAD_EQUALS
  else
    Result := $0;
  end;
end;

function GetWindowInfo(windowId: CGWindowID): TWindowInfo;
var
  windowIds, windows: CFArrayRef;
  windowInfo: CFDictionaryRef;
  LocalPool: NSAutoReleasePool;
  bounds: CGRect;
  boundsRect: TRect;
begin
  LocalPool := NSAutoReleasePool.alloc.init;

  bounds := CGRectNull;
  windowIds := CFArrayCreateMutable(nil, 1, nil);
  CFArrayAppendValue(windowIds, UnivPtr(windowId));
  windows := CGWindowListCreateDescriptionFromArray(windowIds);
  CFRelease(windowIds);
  if CFArrayGetCount(windows) <> 0 then
  begin
    windowInfo := CFDictionaryRef(CFArrayGetValueAtIndex(windows, 0));
    CGRectMakeWithDictionaryRepresentation(CFDictionaryGetValue(windowInfo, kCGWindowBounds), bounds);
    boundsRect := CGRectToRect(bounds);

    result.ID := NSNumber(CFDictionaryGetValue(windowInfo, kCGWindowNumber)).unsignedIntValue;
    result.Title := CFStringToString(CFDictionaryGetValue(windowInfo, kCGWindowName));
    result.X := boundsRect.left;
    result.Y := boundsRect.top;
    result.Width := boundsRect.width;
    result.Height := boundsRect.height;
    result.Alpha := NSNumber(CFDictionaryGetValue(windowInfo, kCGWindowAlpha)).floatValue;
    result.onScreen := NSNumber(CFDictionaryGetValue(windowInfo, kCGWindowIsOnscreen)).boolValue;
    result.Layer := NSNumber(CFDictionaryGetValue(windowInfo, kCGWindowLayer)).intValue;
    result.MemoryUsage := NSNumber(CFDictionaryGetValue(windowInfo, kCGWindowMemoryUsage)).intValue;
    result.OwnerName := CFStringToString(CFDictionaryGetValue(windowInfo, kCGWindowOwnerName));
    result.OwnerPid := NSNumber(CFDictionaryGetValue(windowInfo, kCGWindowOwnerPID)).intValue;
    result.SharingState := NSNumber(CFDictionaryGetValue(windowInfo, kCGWindowSharingState)).boolValue;
    result.StoreType := NSNumber(CFDictionaryGetValue(windowInfo, kCGWindowStoreType)).intValue;
  end else
    begin
      result.id := High(CGWindowID);
    end;
  CFRelease(windows);
  LocalPool.release;
end;

function isWindowActive(windowId: CGWindowID): Boolean;
var
  LocalPool: NSAutoReleasePool;
begin
  LocalPool := NSAutoReleasePool.alloc.init;
  Result := NSWorkspace.sharedWorkspace.frontmostApplication.processIdentifier = GetWindowInfo(windowId).ownerPid;
  LocalPool.release;
end;

//Does not actually set the window active.. It sets ALL windows of the process active..
function SetWindowActive(windowId: CGWindowID): Boolean;
var
  app: NSRunningApplication;
  LocalPool: NSAutoReleasePool;
begin
  LocalPool := NSAutoReleasePool.alloc.init;
  app := NSRunningApplication.runningApplicationWithProcessIdentifier(GetWindowInfo(windowId).ownerPid);
  Result := app.activateWithOptions(NSApplicationActivateIgnoringOtherApps);
  LocalPool.release;
end;

procedure GetCursorPos(out X, Y: Int32);
var
  event: CGEventRef;
  point: CGPoint;
begin
  event := CGEventCreate(nil);
  point := CGEventGetLocation(event);
  X := round(point.x);
  Y := round(point.y);
  CFRelease(event);
end;

function IsMouseButtonDown(Button: CGMouseButton): Boolean;
begin
  Result := CGEventSourceButtonState(kCGEventSourceStateCombinedSessionState, Button) > 0;
end;

//Return every single window that is on screen..
function GetOnScreenWindows(): TWindowArray;
var
  Windows: CFArrayRef;
  WindowInfo: CFDictionaryRef;
  i: Int32;
  LocalPool: NSAutoReleasePool;
begin
  LocalPool := NSAutoReleasePool.alloc.init;
  SetLength(Result, 0);
  Windows := CGWindowListCopyWindowInfo(kCGWindowListOptionOnScreenOnly , kCGNullWindowID);

  for i := 0 to CFArrayGetCount(Windows) - 1 do
  begin
    WindowInfo := CFArrayGetValueAtIndex(Windows, i);
    SetLength(Result, Length(Result) + 1);
    Result[i] := NSNumber(CFDictionaryGetValue(windowInfo, kCGWindowNumber)).unsignedIntValue;
  end;

  CFRelease(Windows);
  LocalPool.release;
end;

//Retrieve child windows of a parent with the same PID..
function GetChildWindows(parent: CGWindowID): TWindowArray;
var
  ParentPid: Integer;
  Windows: CFArrayRef;
  WindowInfo: CFDictionaryRef;
  i: Int32;
  LocalPool: NSAutoReleasePool;
begin
  ParentPid := GetWindowInfo(parent).ownerPid;

  LocalPool := NSAutoReleasePool.alloc.init;
  SetLength(Result, 0);
  Windows := CGWindowListCopyWindowInfo(kCGWindowListOptionOnScreenOnly, parent);

  for i := 0 to CFArrayGetCount(Windows) - 1 do
  begin
    WindowInfo := CFArrayGetValueAtIndex(Windows, i);
    if (ParentPid = NSNumber(CFDictionaryGetValue(WindowInfo, kCGWindowOwnerPID)).intValue) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := NSNumber(CFDictionaryGetValue(WindowInfo, kCGWindowNumber)).unsignedIntValue;
    end;
  end;

  CFRelease(Windows);
  LocalPool.release;
end;

//Can actually retrieve child windows of a parent.. not the non-sense above..
function GetChildWindowsAX(parent: CGWindowID): TWindowArray;
var
  ParentPid: Integer;
  application: AXUIElementRef;
  childWindows: CFArrayRef;
  children: NSArray;
  windowId: CGWindowID;
  error: AXError;
  i: Int32;
  LocalPool: NSAutoReleasePool;
begin
  ParentPid := GetWindowInfo(parent).ownerPid;

  LocalPool := NSAutoReleasePool.alloc.init;
  SetLength(Result, 0);
  application := AXUIElementCreateApplication(ParentPid);

  childWindows := nil;
  AXUIElementCopyAttributeValues(application, CFSTR('AXWindows'), 0, 99999, childWindows);
  if childWindows <> nil then
  begin
    children := NSArray(childWindows);

    for i := 0 to children.count - 1 do
    begin
      windowId := 0;
      error := AXUIElementGetWindow(AXUIElementRef(children.objectAtIndex(i)), windowId);
      if (error = kAXErrorSuccess) then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := windowId;
      end;
   end;

    CFRelease(childWindows);
  end;

  CFRelease(application);
  LocalPool.release;
end;

function GetActiveWindowAX(): CGWindowID;
var
  error: AXError;
  systemWide: AXUIElementRef;
  FocusedApplication: AXUIElementRef;
  FocusedWindow: AXUIElementRef;
  LocalPool: NSAutoReleasePool;
begin
  LocalPool := NSAutoReleasePool.alloc.init;
  systemWide := AXUIElementCreateSystemWide();

  Result := kCGNullWindowID;
  AXUIElementCopyAttributeValue(systemWide, CFSTR('AXFocusedApplication'), FocusedApplication);
  AXUIElementCopyAttributeValue(FocusedApplication, CFStringRef(NSAccessibilityFocusedWindowAttribute), FocusedWindow);
  error := AXUIElementGetWindow(FocusedWindow, Result);
  if error <> kAXErrorSuccess then
     Result := kCGNullWindowID;

  CFRelease(FocusedApplication);
  CFRelease(FocusedWindow);
  CFRelease(systemWide);
  LocalPool.release;
end;

procedure SetWindowBoundsAX(window: CGWindowID; X, Y, Width, Height: Integer);
var
  application: AXUIElementRef;
  windows: CFArrayRef;
  windowId: CGWindowID;
  error: AXError;
  origin: NSPoint;
  size: NSSize;
  storage: CFTypeRef;
  i: Int32;
begin
  application := AXUIElementCreateApplication(GetWindowInfo(window).ownerPid);
  origin.x := CGFloat(X);
  origin.y := CGFloat(Y);
  size.width := CGFloat(Width);
  size.height := CGFloat(Height);

  windows := nil;
  AXUIElementCopyAttributeValues(application, CFSTR('AXWindows'), 0, 99999, windows);
  if windows <> nil then
  begin
    for i := 0 to CFArrayGetCount(windows) - 1 do
    begin
      windowId := kCGNullWindowID;
      error := AXUIElementGetWindow(AXUIElementRef(CFArrayGetValueAtIndex(windows, i)), windowId);
      if ((error = kAXErrorSuccess) and (windowId = window)) then
      begin
        storage := AXValueCreate(kAXValueCGPointType, UnivPtr(@origin));
        AXUIElementSetAttributeValue(application, CFStringRef(NSAccessibilityPositionAttribute), storage);
        CFRelease(storage);

        storage := AXValueCreate(kAXValueCGSizeType, UnivPtr(@size));
        AXUIElementSetAttributeValue(application, CFStringRef(NSAccessibilitySizeAttribute), storage);
        CFRelease(storage);
        break;
      end;
    end;

    CFRelease(windows);
  end;
end;

end.

