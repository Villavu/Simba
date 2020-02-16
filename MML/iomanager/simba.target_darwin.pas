{$mode objfpc}{$H+}
{$modeswitch objectivec2}

unit simba.target_darwin;

interface

uses
  classes, sysutils,
  macosall, lcltype, simba.target, simba.oswindow, simba.mufasatypes, CocoaAll;

type
  TWindowTarget = class(TTarget)
  protected
    FWindow: TOSWindow;
    FAutoFocus: Boolean;
    FBuffer: PRGB32;
    FBufferContext: CGContextRef;
    FBufferWidth, FBufferHeight: Int32;

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

// fixme
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
    Result.RowLen := Width;

    Inc(Result.Ptr, Y * FBufferWidth);
    Inc(Result.Ptr, X);
  end;
end;

procedure TWindowTarget.FreeReturnData;
begin
end;

procedure TWindowTarget.GetMousePosition(out X, Y: Int32);
var
  event: CGEventRef;
  point: CGPoint;
begin
  event := CGEventCreate(nil);
  point := CGEventGetLocation(event);
  CFRelease(event);

  X := round(point.x);
  Y := round(point.y);
  MouseClientAreaOffset(X, Y);
end;

procedure TWindowTarget.ScrollMouse(X, Y: Int32; Lines: Int32);
var
   ScrollEvent: CGEventRef;
begin
  if FAutoFocus then
    ActivateClient();

  MouseClientAreaOffset(X, Y);

  ScrollEvent := CGEventCreateScrollWheelEvent(nil, kCGScrollEventUnitPixel, 1, lines * 10);
  CGEventPost(kCGHIDEventTap, scrollEvent);
  CFRelease(scrollEvent);
end;

procedure TWindowTarget.MoveMouse(X, Y: Int32);
{var
  event: CGEventRef;}
begin
  if FAutoFocus then
    ActivateClient();

  MouseClientAreaOffset(X, Y);

  {CGWarpCursorPos(
  event := CGEventCreateMouseEvent(nil, {kCGEventMouseMoved} 5, CGPointMake(x, y), 0); //CGWarpCursorPos
  CGEventPost(kCGSessionEventTap, event);
  CFRelease(event);}

  CGWarpMouseCursorPosition(CGPointMake(x, y));
end;

procedure TWindowTarget.HoldMouse(X, Y: Int32; Button: TClickType);
var
  event: CGEventRef;
  eventType, mouseButton: LongInt;
begin
  if FAutoFocus then
    ActivateClient();

  case button of
    mouse_Left:
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

  event := CGEventCreateMouseEvent(nil, eventType, CGPointMake(x, y), mouseButton);
  CGEventPost(kCGSessionEventTap, event);
  CFRelease(event);
end;

procedure TWindowTarget.ReleaseMouse(X, Y: Int32; Button: TClickType);
var
  event: CGEventRef;
  eventType, mouseButton: LongInt;
begin
  if FAutoFocus then
    ActivateClient();

  case button of
    mouse_Left:
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

  event := CGEventCreateMouseEvent(nil, eventType, CGPointMake(x, y), mouseButton);
  CGEventPost(kCGSessionEventTap, event);
  CFRelease(event);
end;

function TWindowTarget.IsMouseButtonHeld(Button: TClickType): Boolean;
var
  mouseButton: UInt32;
  buttonStateResult: CBool;
begin
  case button of
    mouse_Left:   mouseButton := kCGMouseButtonLeft;
    mouse_Middle: mouseButton := kCGMouseButtonCenter;
    mouse_Right:  mouseButton := kCGMouseButtonRight;
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
  else
    Raise Exception.CreateFMT('GetKeyCode - char (%s) is not in A..z',[Character]);
  end
end;

end.
