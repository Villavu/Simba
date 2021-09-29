{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
{$i simba.inc}
{$modeswitch objectivec2}

unit simba.target_darwin;

interface

uses
  classes, sysutils,
  macosall, lcltype, simba.target, simba.oswindow, simba.mufasatypes;

type
  TWindowTarget = class(TTarget)
  protected
    FWindow: TOSWindow;
    FAutoFocus: Boolean;
    FBuffer: PRGB32;
    FBufferContext: CGContextRef;
    FBufferWidth, FBufferHeight: Int32;
    FCursorWindow: TOSWindow;

    function GetHandle: PtrUInt; override;
    procedure SetHandle(Value: PtrUInt); override;

    function GetAutoFocus: Boolean; override;
    procedure SetAutoFocus(Value: Boolean); override;

    procedure GetTargetBounds(out Bounds: TBox); override;
  public
    function CopyData(X, Y, Width, Height: Int32): PRGB32; override;
    function ReturnData(X, Y, Width, Height: Int32): TRetData; override;
    procedure FreeReturnData; override;

    function TargetValid: Boolean; override;

    procedure ActivateClient; override;
    procedure GetMousePosition(out X, Y: Int32); override;
    procedure ScrollMouse(X, Y: Int32; Lines: Int32); override;
    procedure MoveMouse(X, Y: Int32); override;
    procedure HoldMouse(X, Y: Int32; Button: TClickType); override;
    procedure ReleaseMouse(X, Y: Int32; Button: TClickType); override;
    function IsMouseButtonHeld(Button: TClickType): Boolean;override;

    procedure SendString(Text: String; KeyWait, KeyModWait: Int32); override;
    procedure HoldKey(Key: Int32); override;
    procedure ReleaseKey(Key: Int32); override;
    function IsKeyHeld(Key: Int32): Boolean; override;
    function GetKeyCode(Character: Char): Int32; override;

    constructor Create(Target: TOSWindow);
    destructor Destroy; override;
  end;

implementation

uses
  simba.darwin_inputhelpers, dialogs;

procedure SendKeyInput(Key: Word; Down: Boolean);
var
  Char: Word;
begin
  Char := 0;
  if Key in [VK_A .. VK_Z] then
  begin
    Char := Ord('A') + Key - VK_A;
  end;

  if Key in [VK_0 .. VK_9] then
  begin
    Char := Ord('0') + Key - VK_0;
  end;

  CGPostKeyboardEvent(Char, VirtualKeyCodeToMac(Key), boolean_t(Down));
end;

function TWindowTarget.GetHandle: PtrUInt;
begin
  Result := FWindow;
end;

procedure TWindowTarget.SetHandle(Value: PtrUInt);
begin
  FWindow := Value;
  if (FWindow = 0) then
  begin
    FWindow := GetDesktopWindow();
    FCursorWindow := GetOnScreenWindows()[0]; // Cursor is always topmost
  end;
end;

function TWindowTarget.GetAutoFocus: Boolean;
begin
  Result := FAutoFocus;
end;

procedure TWindowTarget.SetAutoFocus(Value: Boolean);
begin
  FAutoFocus := Value;
end;

procedure TWindowTarget.GetTargetBounds(out Bounds: TBox);
var
  Attempts: Int32;
begin
  for Attempts := 1 to 5 do
  begin
    if FWindow.GetBounds(Bounds) then
      Exit;

    Self.InvalidTarget();
  end;

  raise Exception.CreateFmt('Invalid window handle: %d', [FWindow]);
end;

constructor TWindowTarget.Create(Target: TOSWindow);
begin
  inherited Create();

  Handle := Target;
end;

destructor TWindowTarget.Destroy;
begin
  if (FBuffer <> nil) then
    FreeMem(FBuffer);
  if (FBufferContext <> nil) then
    CGContextRelease(FBufferContext);

  inherited Destroy();
end;

function TWindowTarget.TargetValid: Boolean;
var
  windowIds, windows: CFArrayRef;
begin
  windowIds := CFArrayCreateMutable(nil, 1, nil);
  CFArrayAppendValue(windowIds, UnivPtr(FWindow));
  windows := CGWindowListCreateDescriptionFromArray(windowIds);
  CFRelease(windowIds);
  Result := CFArrayGetCount(windows) <> 0;
  CFRelease(windows);
end;

procedure TWindowTarget.ActivateClient;
begin
  FWindow.Activate();
end;

function TWindowTarget.CopyData(X, Y, Width, Height: Int32): PRGB32;
var
  Bounds: TBox;
  Image: CGImageRef;
  ColorSpace: CGColorSpaceRef;
  Context: CGContextRef;
  Buffer: PRGB32;
  W, H, Loop: Int32;
begin
  Result := nil;

  if FAutoFocus then
    ActivateClient();

  GetTargetBounds(Bounds);

  ImageClientAreaOffset(X, Y);

  if Bounds.Contains(Bounds.X1 + X, Bounds.Y1 + Y, Width, Height) then
  begin
    if FWindow = GetDesktopWindow() then
      Image := CGWindowListCreateImage(CGRectNull, kCGWindowListOptionOnScreenBelowWindow, FCursorWindow, kCGWindowImageBoundsIgnoreFraming)
    else
      Image := CGWindowListCreateImage(CGRectNull, kCGWindowListOptionIncludingWindow, FWindow, kCGWindowImageBoundsIgnoreFraming);

    W := Bounds.Width - 1;
    H := Bounds.Height - 1;

    ColorSpace := CGColorSpaceCreateDeviceRGB();

    Buffer := GetMem(W * H * SizeOf(TRGB32));
    Context := CGBitmapContextCreate(Buffer, W, H, 8, W * SizeOf(TRGB32), ColorSpace, kCGImageAlphaPremultipliedFirst or kCGBitmapByteOrder32Little);

    CGColorSpaceRelease(ColorSpace);

    CGContextDrawImage(Context, CGRectMake(0, 0, W, H), Image);
    CGImageRelease(Image);

    Result := GetMem(Width * Height * SizeOf(TRGB32));

    for Loop := 0 to Height - 1 do
      Move(Buffer[(Loop + Y) * W + X], Result[Loop * Width], Width * SizeOf(TRGB32));

    FreeMem(Buffer);
    CGContextRelease(Context);
  end;
end;

function TWindowTarget.ReturnData(X, Y, Width, Height: Int32): TRetData;
var
  Bounds: TBox;
  Image: CGImageRef;
  ColorSpace: CGColorSpaceRef;
  W, H: Int32;
begin
  Result := NullReturnData;

  if FAutoFocus then
    ActivateClient();

  GetTargetBounds(Bounds);

  ImageClientAreaOffset(X, Y);

  if Bounds.Contains(Bounds.X1 + X, Bounds.Y1 + Y, Width, Height) then
  begin
    if FWindow = GetDesktopWindow() then
      Image := CGWindowListCreateImage(CGRectNull, kCGWindowListOptionOnScreenBelowWindow, FCursorWindow, kCGWindowImageBoundsIgnoreFraming)
    else
      Image := CGWindowListCreateImage(CGRectNull, kCGWindowListOptionIncludingWindow, FWindow, kCGWindowImageBoundsIgnoreFraming);

    W := Bounds.Width - 1;
    H := Bounds.Height - 1;

    if (FBuffer = nil) or (FBufferWidth <> W) or (FBufferHeight <> H) then
    begin
      FBufferWidth := W;
      FBufferHeight := H;

      if (FBuffer <> nil) then
        FreeMem(FBuffer);
      if (FBufferContext <> nil) then
        CGContextRelease(FBufferContext);

      ColorSpace := CGColorSpaceCreateDeviceRGB();

      FBuffer := GetMem(W * H * SizeOf(TRGB32));
      FBufferContext := CGBitmapContextCreate(FBuffer, W, H, 8, W * SizeOf(TRGB32), ColorSpace, kCGImageAlphaPremultipliedFirst or kCGBitmapByteOrder32Little);

      CGColorSpaceRelease(ColorSpace);
    end;

    CGContextDrawImage(FBufferContext, CGRectMake(0, 0, W, H), Image);
    CGImageRelease(Image);

    Result.Ptr := PRGB32(FBuffer);
    Result.IncPtrWith := FBufferWidth - Width;
    Result.RowLen := FBufferWidth;

    // Move image to start of buffer
    Move(FBuffer[Y * FBufferWidth + X], FBuffer^, FBufferWidth * Height * SizeOf(TRGB32));

    Result.Ptr := PRGB32(FBuffer);
  end;
end;

procedure TWindowTarget.FreeReturnData;
begin
end;

procedure TWindowTarget.GetMousePosition(out X, Y: Int32);
var
  event: CGEventRef;
  point: CGPoint;
  Left, Top: Int32;
begin
  event := CGEventCreate(nil);
  point := CGEventGetLocation(event);
  CFRelease(event);

  X := Round(point.x);
  Y := Round(point.y);

  MouseClientAreaOffset(X, Y);
  GetTargetPosition(Left, Top);

  X -= Left;
  Y -= Top;
end;

procedure TWindowTarget.ScrollMouse(X, Y: Int32; Lines: Int32);
var
  ScrollEvent: CGEventRef;
begin
  if FAutoFocus then
    ActivateClient();

  ScrollEvent := CGEventCreateScrollWheelEvent(nil, kCGScrollEventUnitPixel, 1, lines * 10);
  CGEventPost(kCGHIDEventTap, scrollEvent);
  CFRelease(scrollEvent);
end;

procedure TWindowTarget.MoveMouse(X, Y: Int32);
var
  Left, Top: Int32;
begin
  if FAutoFocus then
    ActivateClient();

  MouseClientAreaOffset(X, Y);
  GetTargetPosition(Left, Top);

  CGWarpMouseCursorPosition(CGPointMake(Left + X, Top + Y));
end;

procedure TWindowTarget.HoldMouse(X, Y: Int32; Button: TClickType);
var
  event: CGEventRef;
  eventType, mouseButton: LongInt;
  Left, Top: Int32;
begin
  if FAutoFocus then
    ActivateClient();

  case Button of
    MOUSE_LEFT:
      begin
        eventType := 1 {kCGEventLeftMouseDown};
        mouseButton := kCGMouseButtonLeft;
      end;
    mouse_Middle:
      begin
        eventType := 25 {kCGEventOtherMouseDown};
        mouseButton := kCGMouseButtonCenter;
      end;
    mouse_Right:
      begin
        eventType := 3 {kCGEventRightMouseDown};
        mouseButton := kCGMouseButtonRight;
      end;
  end;

  GetTargetPosition(Left, Top);

  event := CGEventCreateMouseEvent(nil, eventType, CGPointMake(Left + X, Top + Y), mouseButton);
  CGEventPost(kCGSessionEventTap, event);
  CFRelease(event);
end;

procedure TWindowTarget.ReleaseMouse(X, Y: Int32; Button: TClickType);
var
  event: CGEventRef;
  eventType, mouseButton: LongInt;
  Left, Top: Int32;
begin
  if FAutoFocus then
    ActivateClient();

  case Button of
    MOUSE_LEFT:
      begin
        eventType := 2 {kCGEventLeftMouseUp};
        mouseButton := kCGMouseButtonLeft;
      end;
    mouse_Middle:
      begin
        eventType := 26 {kCGEventOtherMouseUp};
        mouseButton := kCGMouseButtonCenter;
      end;
    mouse_Right:
      begin
        eventType := 4 {kCGEventRightMouseUp};
        mouseButton := kCGMouseButtonRight;
      end;
  end;

  GetTargetPosition(Left, Top);

  event := CGEventCreateMouseEvent(nil, eventType, CGPointMake(Left + X, Top + Y), mouseButton);
  CGEventPost(kCGSessionEventTap, event);
  CFRelease(event);
end;

function TWindowTarget.IsMouseButtonHeld(Button: TClickType): Boolean;
var
  mouseButton: UInt32;
  buttonStateResult: CBool;
begin
  case Button of
    MOUSE_LEFT:   mouseButton := kCGMouseButtonLeft;
    MOUSE_MIDDLE: mouseButton := kCGMouseButtonCenter;
    MOUSE_RIGHT:  mouseButton := kCGMouseButtonRight;
  else
    Result := False;
  end;

  buttonStateResult := CGEventSourceButtonState(kCGEventSourceStateCombinedSessionState, mouseButton);
  Result := buttonStateResult > 0;
end;

procedure TWindowTarget.SendString(Text: String; KeyWait, KeyModWait: Int32);
var
   I, L: Integer;
   K: Byte;
   HoldShift: Boolean;
begin
  if FAutoFocus then
    ActivateClient();

  HoldShift := False;
  L := Length(Text);
  for I := 1 to L do
  begin
    if (((Text[I] >= 'A') and (Text[I] <= 'Z')) or
        ((Text[I] >= '!') and (Text[I] <= '&')) or
        ((Text[I] >= '(') and (Text[I] <= '+')) or
        (Text[I] = ':') or
        ((Text[I] >= '<') and (Text[I] <= '@')) or
        ((Text[I] >= '^') and (Text[I] <= '_')) or
        ((Text[I] >= '{') and (Text[I] <= '~'))) then
    begin
      HoldKey(VK_SHIFT);
      HoldShift := True;
      sleep(keymodwait shr 1);
    end;

    K := GetKeyCode(Text[I]);
    HoldKey(K);

    if keywait <> 0 then
      Sleep(keywait);

    ReleaseKey(K);

    if (HoldShift) then
    begin
      HoldShift := False;
      sleep(keymodwait shr 1);
      ReleaseKey(VK_SHIFT);
    end;
  end;
end;

procedure TWindowTarget.HoldKey(Key: Int32);
begin
  if FAutoFocus then
    ActivateClient();

  SendKeyInput(key, True);
end;

procedure TWindowTarget.ReleaseKey(Key: Int32);
begin
  if FAutoFocus then
    ActivateClient();

  SendKeyInput(key, False);
end;

function TWindowTarget.IsKeyHeld(Key: Int32): Boolean;
begin
  Result := Boolean(CGEventSourceKeyState(kCGEventSourceStateCombinedSessionState, VirtualKeyCodeToMac(key)));
end;

function TWindowTarget.GetKeyCode(Character: Char): Int32;
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
    Char($8): Result := VK_BACK;
    Char($9): Result := VK_TAB;
    Char($A): Result := VK_RETURN;
  else
    Raise Exception.CreateFMT('GetKeyCode - char (%s) is not in A..z',[Character]);
  end
end;

end.
