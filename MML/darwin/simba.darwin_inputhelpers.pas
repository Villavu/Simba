unit simba.darwin_inputhelpers;

{$mode objfpc}{$H+}
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
  LCLType, cocoaall, cocoa_extra, cocoautils;

type
  NSWorkspaceFix = objccategory external (NSWorkspace)
    function frontmostApplication(): NSRunningApplication; message 'frontmostApplication';
  end;

function VirtualKeyCodeToMac(AKey: Word): Word;
begin
  case AKey of
  VK_BACK      : Result := $33;
  VK_TAB       : Result := $30;
  VK_RETURN    : Result := $24;
  VK_PAUSE     : Result := $71;
  VK_CAPITAL   : Result := $39;
  VK_ESCAPE    : Result := $35;
  VK_SPACE     : Result := $31;
  VK_PRIOR     : Result := $74;
  VK_NEXT      : Result := $79;
  VK_END       : Result := $77;
  VK_HOME      : Result := $73;
  VK_LEFT      : Result := $7B;
  VK_UP        : Result := $7E;
  VK_RIGHT     : Result := $7C;
  VK_DOWN      : Result := $7D;
  VK_SNAPSHOT  : Result := $69;
  VK_INSERT    : Result := $72;
  VK_DELETE    : Result := $75;
  VK_HELP      : Result := $72;
  VK_SLEEP     : Result := $7F7F;
  VK_NUMPAD0   : Result := $52;
  VK_NUMPAD1   : Result := $53;
  VK_NUMPAD2   : Result := $54;
  VK_NUMPAD3   : Result := $55;
  VK_NUMPAD4   : Result := $56;
  VK_NUMPAD5   : Result := $57;
  VK_NUMPAD6   : Result := $58;
  VK_NUMPAD7   : Result := $59;
  VK_NUMPAD8   : Result := $5b;
  VK_NUMPAD9   : Result := $5c;
  //VK_MULTIPLY  : Result := $43;
  //VK_ADD       : Result := $45;
  VK_SEPARATOR : Result := $41;
  VK_SUBTRACT  : Result := $4E;
  VK_DECIMAL   : Result := $41;
  VK_DIVIDE    : Result := $4B;
  VK_F1        : Result := $7A;
  VK_F2        : Result := $78;
  VK_F3        : Result := $63;
  VK_F4        : Result := $76;
  VK_F5        : Result := $60;
  VK_F6        : Result := $61;
  VK_F7        : Result := $62;
  VK_F8        : Result := $64;
  VK_F9        : Result := $65;
  VK_F10       : Result := $6D;
  VK_F11       : Result := $67;
  VK_F12       : Result := $6F;
  VK_F13       : Result := $69;
  VK_F14       : Result := $6B;
  VK_F15       : Result := $71;
  VK_F16       : Result := $6A;
  VK_F17       : Result := $40;
  VK_F18       : Result := $4F;
  VK_F19       : Result := $50;
  VK_NUMLOCK   : Result := $47;
  VK_CLEAR     : Result := $47;
  VK_SCROLL    : Result := $6B;
  VK_SHIFT     : Result := $38;
  VK_CONTROL   : Result := $37;
  VK_MENU      : Result := $3A;
  VK_OEM_3     : Result := 50;
  //VK_OEM_MINUS : Result := 27;
  VK_OEM_PLUS  : Result := 24;
  VK_OEM_5     : Result := 42;
  VK_OEM_4     : Result := 33;
  VK_OEM_6     : Result := 30;
  VK_OEM_1     : Result := 41;
  VK_OEM_7     : Result := 39;
  VK_OEM_COMMA : Result := 43;
  //VK_OEM_PERIOD: Result := 47;
  //VK_OEM_2     : Result := 44;
  else
    Result := 0;
  end;
end;

function VirtualKeyCodeToCharCode(AKey: Word): Word;
begin
  case AKey of
  VK_MULTIPLY  : Result := Ord('*');
  VK_ADD       : Result := Ord('+');
  VK_OEM_MINUS : Result := Ord('-');
  VK_OEM_PERIOD: Result := Ord('.');
  VK_OEM_2     : Result := Ord('/');
  else
    Result := AKey;
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

