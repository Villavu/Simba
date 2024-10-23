{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.target;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.image, simba.image_utils, simba.externalcanvas,
  simba.target_eios, simba.target_window, simba.target_image, simba.target_plugin,
  simba.colormath, simba.dtm;

const
  DEFAULT_KEY_PRESS_MIN = 20;
  DEFAULT_KEY_PRESS_MAX = 125;

  DEFAULT_CLICK_MIN = 40;
  DEFAULT_CLICK_MAX = 220;

  DEFAULT_MOUSE_TIMEOUT = 15000;
  DEFAULT_MOUSE_SPEED   = 10;
  DEFAULT_MOUSE_GRAVITY = 9;
  DEFAULT_MOUSE_WIND    = 4;

type
  {$PUSH}
  {$SCOPEDENUMS ON}
  ESimbaTargetKind = (NONE, IMAGE, WINDOW, EIOS, PLUGIN);
  {$POP}

const
  TargetName: array[ESimbaTargetKind] of String = ('NONE', 'IMAGE', 'WINDOW', 'EIOS', 'PLUGIN');

type
  TSimbaTargetInfo = record
    Kind: ESimbaTargetKind;
    Target: Pointer;
    Image: TSimbaImage;
    Window: TWindowHandle;
    EIOS: TEIOSTarget;
    Plugin: TSimbaPluginTarget;

    GetDimensions: procedure(Target: Pointer; out W, H: Integer);
    GetImageData: function(Target: Pointer; X, Y, Width, Height: Integer; var Data: PColorBGRA; var DataWidth: Integer): Boolean;

    IsValid: function(Target: Pointer): Boolean;
    IsFocused: function(Target: Pointer): Boolean;
    Focus: function(Target: Pointer): Boolean;

    KeyDown: procedure(Target: Pointer; Key: EKeyCode);
    KeyUp: procedure(Target: Pointer; Key: EKeyCode);
    KeySend: procedure(Target: Pointer; Text: PChar; TextLen: Int32; SleepTimes: PInt32);
    KeyPressed: function(Target: Pointer; Key: EKeyCode): Boolean;

    MouseTeleport: procedure(Target: Pointer; P: TPoint);
    MousePosition: function(Target: Pointer): TPoint;
    MousePressed: function(Target: Pointer; Button: EMouseButton): Boolean;
    MouseDown: procedure(Target: Pointer; Button: EMouseButton);
    MouseUp: procedure(Target: Pointer; Button: EMouseButton);
    MouseScroll: procedure(Target: Pointer; Scrolls: Integer);
  end;

  TMouseButtonEvent   = procedure(Target: Pointer; Button: EMouseButton; Down: Boolean) of object;
  TMouseTeleportEvent = procedure(Target: Pointer; P: TPoint) of object;
  TMouseMovingEvent   = procedure(Target: Pointer; var X, Y, DestX, DestY: Double; var Stop: Boolean) of object;
  TTargetEvent        = procedure(Target: Pointer) of object;

  PSimbaTarget = ^TSimbaTarget;
  TSimbaTarget = record
  private
    FTarget: TSimbaTargetInfo;
    FFrozen: record
      Bounds: TBox;
      DataWidth: Integer;
      Data: array of TColorBGRA;
    end;
    FInvalidEvents: array of TMethod;
    FChangeEvents: array of TMethod;
    FCustomClientArea: TBox;
    FAutoFocus: Boolean;

    function ValidateBounds(var ABounds: TBox): Boolean;

    procedure ChangeTarget(Kind: ESimbaTargetKind);
    procedure TargetChanged;

    procedure CheckMethod(Method: Pointer; Name: String);
    procedure CheckInvalidTarget;
    procedure CheckAutoFocus;

    function GetBounds: TBox;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetSize: TSize;

    function GetMouseX: Integer;
    function GetMouseY: Integer;
    function GetMouseXY: TPoint;
    procedure SetMouseX(Value: Integer);
    procedure SetMouseY(Value: Integer);
    procedure SetMouseXY(Value: TPoint);
  public
    MouseOptions: record
      ButtonEvents: array of TMethod;
      TeleportEvents: array of TMethod;
      MovingEvents: array of TMethod;

      MinClickTime: Integer;
      MaxClickTime: Integer;

      Speed: Double;
      Gravity: Double;
      Wind: Double;
      Accuracy: Double;
      Timeout: Integer;
    end;

    KeyOptions: record
      MinPressTime: Integer;
      MaxPressTime: Integer;
    end;

    // target
    procedure SetDesktop;
    procedure SetWindow(Window: TWindowHandle);
    procedure SetImage(Image: TSimbaImage);
    procedure SetEIOS(FileName, Args: String);
    procedure SetPlugin(FileName, Args: String); overload;
    procedure SetPlugin(FileName, Args: String; out DebugImage: TSimbaExternalCanvas); overload;

    function AddTargetChangeEvent(Event: TTargetEvent): TTargetEvent;
    function AddTargetInvalidEvent(Event: TTargetEvent): TTargetEvent;
    procedure RemoveTargetChangeEvent(Event: TTargetEvent);
    procedure RemoveTargetInvalidEvent(Event: TTargetEvent);

    function GetImageDataAsImage(var ABounds: TBox; out Image: TSimbaImage): Boolean;
    function GetImageData(var ABounds: TBox; var Data: PColorBGRA; var DataWidth: Integer): Boolean;
    procedure FreeImageData(var Data: PColorBGRA);

    function IsImageFrozen: Boolean;
    procedure FreezeImage(ABounds: TBox);
    procedure UnFreezeImage;

    function GetImage(ABounds: TBox): TSimbaImage;

    function IsValid: Boolean;
    function IsFocused: Boolean;
    function Focus: Boolean;

    function Copy: TSimbaTarget;
    function ToString: String;

    property TargetKind: ESimbaTargetKind read FTarget.Kind;
    property TargetWindow: TWindowHandle read FTarget.Window;
    property TargetImage: TSimbaImage read FTarget.Image;

    property Bounds: TBox read GetBounds;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Size: TSize read GetSize;

    property CustomClientArea: TBox read FCustomClientArea write FCustomClientArea;
    property AutoFocus: Boolean read FAutoFocus write FAutoFocus;

    // Mouse
    function AddMouseEvent(Event: TMouseButtonEvent): TMouseButtonEvent; overload;
    function AddMouseEvent(Event: TMouseTeleportEvent): TMouseTeleportEvent; overload;
    function AddMouseEvent(Event: TMouseMovingEvent): TMouseMovingEvent; overload;

    procedure RemoveMouseEvent(Event: TMouseButtonEvent); overload;
    procedure RemoveMouseEvent(Event: TMouseTeleportEvent); overload;
    procedure RemoveMouseEvent(Event: TMouseMovingEvent); overload;

    procedure MouseMove(Dest: TPoint); overload;
    procedure MouseMove(Box: TBox; ForcedMove: Boolean = False); overload;
    procedure MouseMove(Quad: TQuad; ForcedMove: Boolean = False); overload;
    procedure MouseClick(Button: EMouseButton);
    procedure MouseTeleport(P: TPoint);
    procedure MouseDown(Button: EMouseButton);
    procedure MouseUp(Button: EMouseButton);
    procedure MouseScroll(Scrolls: Integer);
    function MousePressed(Button: EMouseButton): Boolean;

    property MouseX: Integer read GetMouseX write SetMouseX;
    property MouseY: Integer read GetMouseY write SetMouseY;
    property MouseXY: TPoint read GetMouseXY write SetMouseXY;

    // Keyboard
    procedure KeySend(Text: String);
    procedure KeyPress(Key: EKeyCode);
    procedure KeyDown(Key: EKeyCode);
    procedure KeyUp(Key: EKeyCode);
    function KeyPressed(Key: EKeyCode): Boolean;
    function KeyCodeFromChar(C: Char): EKeyCode;

    // Finder - color
    function MatchColor(Color: TColor; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; ABounds: TBox): TSingleMatrix;

    function FindColor(Color: TColor; Tolerance: Single; ABounds: TBox): TPointArray; overload;
    function FindColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; ABounds: TBox): TPointArray; overload;
    function FindColor(Color: TColorTolerance; ABounds: TBox): TPointArray; overload;

    function CountColor(Color: TColor; Tolerance: Single; ABounds: TBox): Integer; overload;
    function CountColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; ABounds: TBox): Integer; overload;
    function CountColor(Color: TColorTolerance; ABounds: TBox): Integer; overload;

    function HasColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MinCount: Integer; ABounds: TBox): Boolean; overload;
    function HasColor(Color: TColor; Tolerance: Single; MinCount: Integer; ABounds: TBox): Boolean; overload;
    function HasColor(Color: TColorTolerance; MinCount: Integer; ABounds: TBox): Boolean; overload;

    function GetColor(P: TPoint): TColor;
    function GetColors(Points: TPointArray): TColorArray;
    function GetColorsMatrix(ABounds: TBox): TIntegerMatrix;

    // Finder - image
    function FindImage(Image: TSimbaImage; Tolerance: Single; ABounds: TBox): TPoint; overload;
    function FindImage(Image: TSimbaImage; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; ABounds: TBox): TPoint; overload;
    function FindImageEx(Image: TSimbaImage; Tolerance: Single; MaxToFind: Integer; ABounds: TBox): TPointArray; overload;
    function FindImageEx(Image: TSimbaImage; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MaxToFind: Integer; ABounds: TBox): TPointArray; overload;
    function HasImage(Image: TSimbaImage; Tolerance: Single; MinCount: Integer; ABounds: TBox): Boolean; overload;
    function HasImage(Image: TSimbaImage; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MinCount: Integer; ABounds: TBox): Boolean; overload;

    // Finder - template
    function FindTemplate(Templ: TSimbaImage; out Match: Single; ABounds: TBox): TPoint;
    function HasTemplate(Templ: TSimbaImage; MinMatch: Single; ABounds: TBox): Boolean;

    // Finder - dtm
    function FindDTM(DTM: TDTM; ABounds: TBox): TPoint;
    function FindDTMEx(DTM: TDTM; MaxToFind: Integer; ABounds: TBox): TPointArray;
    function FindDTMRotated(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; ABounds: TBox): TPoint;
    function FindDTMRotatedEx(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; MaxToFind: Integer; ABounds: TBox): TPointArray;

    // Finder - other
    function FindEdges(MinDiff: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; ABounds: TBox): TPointArray; overload;
    function FindEdges(MinDiff: Single; ABounds: TBox): TPointArray; overload;

    function GetPixelDifference(WaitTime: Integer; ABounds: TBox): Integer; overload;
    function GetPixelDifference(WaitTime: Integer; Tolerance: Single; ABounds: TBox): Integer; overload;
    function GetPixelDifferenceTPA(WaitTime: Integer; ABounds: TBox): TPointArray; overload;
    function GetPixelDifferenceTPA(WaitTime: Integer; Tolerance: Single; ABounds: TBox): TPointArray; overload;

    function AverageBrightness(ABounds: TBox): Integer;
    function PeakBrightness(ABounds: TBox): Integer;

    class operator Initialize(var Self: TSimbaTarget);
  end;

implementation

uses
  simba.nativeinterface, simba.vartype_box, simba.vartype_quad, simba.target_movemouse, simba.random,
  simba.finder_color, simba.finder_image, simba.finder_dtm;

type
  TEventArray = array of TMethod;

function AppendEvent(var Arr: TEventArray; Event: TMethod): TMethod;
var
  I: Integer;
begin
  Result := Event;

  // check duplicate
  for I := 0 to High(Arr) do
    if (Arr[I].Code = Event.Code) and (Arr[I].Data = Event.Data) then
      Exit;
  Arr += [Event];
end;

procedure DeleteEvent(var Arr: TEventArray; Event: TMethod);
var
  I: Integer;
begin
  for I := High(Arr) downto 0 do
    if (Arr[I].Code = Event.Code) and (Arr[I].Data = Event.Data) then
      Delete(Arr, I, 1);
end;

function TSimbaTarget.MousePressed(Button: EMouseButton): Boolean;
begin
  CheckMethod(FTarget.MousePressed, 'MousePressed');

  Result := FTarget.MousePressed(FTarget.Target, Button);
end;

procedure TSimbaTarget.MouseMove(Dest: TPoint);
begin
  MoveMouseOnTarget(Self, Dest);
end;

procedure TSimbaTarget.MouseMove(Box: TBox; ForcedMove: Boolean);
begin
  if ForcedMove or (not Box.Contains(MouseXY)) then
    MouseMove(Box.RandomPointCenter());
end;

procedure TSimbaTarget.MouseMove(Quad: TQuad; ForcedMove: Boolean);
begin
  if ForcedMove or (not Quad.Contains(MouseXY)) then
    MouseMove(Quad.RandomPointCenter());
end;

procedure TSimbaTarget.MouseClick(Button: EMouseButton);
var
  Time: Integer;
begin
  if (MouseOptions.MinClickTime = 0) and (MouseOptions.MaxClickTime = 0) then
    Time := RandomLeft(DEFAULT_CLICK_MIN, DEFAULT_CLICK_MAX)
  else
    Time := RandomLeft(MouseOptions.MinClickTime, MouseOptions.MaxClickTime);

  MouseDown(Button);
  SimbaNativeInterface.PreciseSleep(Time);
  MouseUp(Button);
end;

procedure TSimbaTarget.MouseTeleport(P: TPoint);
var
  I: Integer;
begin
  CheckMethod(FTarget.MouseTeleport, 'MouseTeleport');
  for I := 0 to High(MouseOptions.TeleportEvents) do
    TMouseTeleportEvent(MouseOptions.TeleportEvents[I])(@Self, P);

  FTarget.MouseTeleport(FTarget.Target, P);
end;

procedure TSimbaTarget.MouseDown(Button: EMouseButton);
var
  I: Integer;
begin
  CheckMethod(FTarget.MouseDown, 'MouseDown');
  CheckAutoFocus();
  for I := 0 to High(MouseOptions.ButtonEvents) do
    TMouseButtonEvent(MouseOptions.ButtonEvents[I])(@Self, Button, True);

  FTarget.MouseDown(FTarget.Target, Button);
end;

procedure TSimbaTarget.MouseUp(Button: EMouseButton);
var
  I: Integer;
begin
  CheckMethod(FTarget.MouseUp, 'MouseUp');
  CheckAutoFocus();
  for I := 0 to High(MouseOptions.ButtonEvents) do
    TMouseButtonEvent(MouseOptions.ButtonEvents[I])(@Self, Button, False);

  FTarget.MouseUp(FTarget.Target, Button);
end;

procedure TSimbaTarget.MouseScroll(Scrolls: Integer);
begin
  CheckMethod(FTarget.MouseScroll, 'MouseScroll');
  CheckAutoFocus();

  FTarget.MouseScroll(FTarget.Target, Scrolls);
end;

procedure TSimbaTarget.KeySend(Text: String);
var
  I: Integer;
  SleepTimes: TIntegerArray;
begin
  CheckMethod(FTarget.KeySend, 'KeySend');
  CheckAutoFocus();

  if (Length(Text) > 0) then
  begin
    SetLength(SleepTimes, Length(Text) * 4);

    if (KeyOptions.MinPressTime = 0) and (KeyOptions.MaxPressTime = 0) then
      for I := 0 to High(SleepTimes) do
        SleepTimes[I] := RandomLeft(DEFAULT_KEY_PRESS_MIN, DEFAULT_KEY_PRESS_MAX)
    else
      for I := 0 to High(SleepTimes) do
        SleepTimes[I] := RandomLeft(KeyOptions.MinPressTime, KeyOptions.MaxPressTime);

    FTarget.KeySend(FTarget.Target, PChar(Text), Length(Text), @SleepTimes[0]);
  end;
end;

procedure TSimbaTarget.KeyPress(Key: EKeyCode);
var
  Time: Integer;
begin
  if (KeyOptions.MinPressTime = 0) and (KeyOptions.MaxPressTime = 0) then
    Time := RandomLeft(DEFAULT_KEY_PRESS_MIN, DEFAULT_KEY_PRESS_MAX)
  else
    Time := RandomLeft(KeyOptions.MinPressTime, KeyOptions.MaxPressTime);

  KeyDown(Key);
  SimbaNativeInterface.PreciseSleep(Time);
  KeyUp(Key);
end;

procedure TSimbaTarget.KeyDown(Key: EKeyCode);
begin
  CheckMethod(FTarget.KeyDown, 'KeyDown');
  CheckAutoFocus();

  FTarget.KeyDown(FTarget.Target, Key);
end;

procedure TSimbaTarget.KeyUp(Key: EKeyCode);
begin
  CheckMethod(FTarget.KeyUp, 'KeyUp');
  CheckAutoFocus();

  FTarget.KeyUp(FTarget.Target, Key);
end;

function TSimbaTarget.KeyPressed(Key: EKeyCode): Boolean;
begin
  CheckMethod(FTarget.KeyPressed, 'KeyPressed');

  Result := FTarget.KeyPressed(FTarget.Target, Key);
end;

function TSimbaTarget.KeyCodeFromChar(C: Char): EKeyCode;
begin
  case C of
    '0'..'9': Result := EKeyCode(Ord(EKeyCode.NUM_0) + Ord(C) - Ord('0'));
    'a'..'z': Result := EKeyCode(Ord(EKeyCode.A)     + Ord(C) - Ord('a'));
    'A'..'Z': Result := EKeyCode(Ord(EKeyCode.A)     + Ord(C) - Ord('A'));
    #32:      Result := EKeyCode.SPACE;
    ')':      Result := EKeyCode.NUM_0;
    '!':      Result := EKeyCode.NUM_1;
    '@':      Result := EKeyCode.NUM_2;
    '#':      Result := EKeyCode.NUM_3;
    '$':      Result := EKeyCode.NUM_4;
    '%':      Result := EKeyCode.NUM_5;
    '^':      Result := EKeyCode.NUM_6;
    '&':      Result := EKeyCode.NUM_7;
    '*':      Result := EKeyCode.NUM_8;
    '(':      Result := EKeyCode.NUM_9;
    ':', ';': Result := EKeyCode.OEM_1;
    '/', '?': Result := EKeyCode.OEM_2;
    '`', '~': Result := EKeyCode.OEM_3;
    '{', '[': Result := EKeyCode.OEM_4;
    '\', '|': Result := EKeyCode.OEM_5;
    '}', ']': Result := EKeyCode.OEM_6;
    #34, #39: Result := EKeyCode.OEM_7;
    '+', '=': Result := EKeyCode.OEM_PLUS;
    '-', '_': Result := EKeyCode.OEM_MINUS;
    ',', '<': Result := EKeyCode.OEM_COMMA;
    '.', '>': Result := EKeyCode.OEM_PERIOD;
    else
      Result := EKeyCode.UNKNOWN;
  end;
end;

function TSimbaTarget.MatchColor(Color: TColor; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; ABounds: TBox): TSingleMatrix;
begin
  Result := MatchColorsOnTarget(Self, ABounds, ColorSpace, Color, Multipliers);
end;

function TSimbaTarget.FindColor(Color: TColor; Tolerance: Single; ABounds: TBox): TPointArray;
begin
  Result := FindColorsOnTarget(Self, ABounds, DefaultColorSpace, Color, Tolerance, DefaultMultipliers);
end;

function TSimbaTarget.FindColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; ABounds: TBox): TPointArray;
begin
  Result := FindColorsOnTarget(Self, ABounds, ColorSpace, Color, Tolerance, Multipliers);
end;

function TSimbaTarget.FindColor(Color: TColorTolerance; ABounds: TBox): TPointArray;
begin
  Result := FindColorsOnTarget(Self, ABounds, Color.ColorSpace, Color.Color, Color.Tolerance, Color.Multipliers);
end;

function TSimbaTarget.CountColor(Color: TColor; Tolerance: Single; ABounds: TBox): Integer;
begin
  Result := CountColorsOnTarget(Self, ABounds, DefaultColorSpace, Color, Tolerance, DefaultMultipliers);
end;

function TSimbaTarget.CountColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; ABounds: TBox): Integer;
begin
  Result := CountColorsOnTarget(Self, ABounds, ColorSpace, Color, Tolerance, Multipliers);
end;

function TSimbaTarget.CountColor(Color: TColorTolerance; ABounds: TBox): Integer;
begin
  Result := CountColorsOnTarget(Self, ABounds, Color.ColorSpace, Color.Color, Color.Tolerance, Color.Multipliers);
end;

function TSimbaTarget.HasColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MinCount: Integer; ABounds: TBox): Boolean;
begin
  Result := CountColorsOnTarget(Self, ABounds, ColorSpace, Color, Tolerance, Multipliers, MinCount) >= MinCount;
end;

function TSimbaTarget.HasColor(Color: TColor; Tolerance: Single; MinCount: Integer; ABounds: TBox): Boolean;
begin
  Result := CountColorsOnTarget(Self, ABounds, DefaultColorSpace, Color, Tolerance, DefaultMultipliers, MinCount) >= MinCount;
end;

function TSimbaTarget.HasColor(Color: TColorTolerance; MinCount: Integer; ABounds: TBox): Boolean;
begin
  Result := CountColorsOnTarget(Self, ABounds, Color.ColorSpace, Color.Color, Color.Tolerance, Color.Multipliers, MinCount) >= MinCount;
end;

function TSimbaTarget.GetColor(P: TPoint): TColor;
begin
  Result := GetColorOnTarget(Self, P);
end;

function TSimbaTarget.GetColors(Points: TPointArray): TColorArray;
begin
  Result := GetColorsOnTarget(Self, Points);
end;

function TSimbaTarget.GetColorsMatrix(ABounds: TBox): TIntegerMatrix;
begin
  Result := GetColorsMatrixOnTarget(Self, ABounds);
end;

function TSimbaTarget.FindImageEx(Image: TSimbaImage; Tolerance: Single; MaxToFind: Integer; ABounds: TBox): TPointArray;
begin
  Result := FindImageOnTarget(Self, Image, ABounds, DefaultColorSpace, Tolerance, DefaultMultipliers, MaxToFind);
end;

function TSimbaTarget.FindImageEx(Image: TSimbaImage; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MaxToFind: Integer; ABounds: TBox): TPointArray;
begin
  Result := FindImageOnTarget(Self, Image, ABounds, ColorSpace, Tolerance, Multipliers, MaxToFind);
end;

function TSimbaTarget.FindImage(Image: TSimbaImage; Tolerance: Single; ABounds: TBox): TPoint;
var
  TPA: TPointArray;
begin
  TPA := FindImageOnTarget(Self, Image, ABounds, DefaultColorSpace, Tolerance, DefaultMultipliers, 1);
  if (Length(TPA) > 0) then
    Result := TPA[0]
  else
    Result := TPoint.Create(-1, -1);
end;

function TSimbaTarget.FindImage(Image: TSimbaImage; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; ABounds: TBox): TPoint;
var
  TPA: TPointArray;
begin
  TPA := FindImageOnTarget(Self, Image, ABounds, ColorSpace, Tolerance, Multipliers, 1);
  if (Length(TPA) > 0) then
    Result := TPA[0]
  else
    Result := TPoint.Create(-1, -1);
end;

function TSimbaTarget.HasImage(Image: TSimbaImage; Tolerance: Single; MinCount: Integer; ABounds: TBox): Boolean;
begin
  Result := Length(FindImageOnTarget(Self, Image, ABounds, DefaultColorSpace, Tolerance, DefaultMultipliers, MinCount)) >= MinCount;
end;

function TSimbaTarget.HasImage(Image: TSimbaImage; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MinCount: Integer; ABounds: TBox): Boolean;
begin
  Result := Length(FindImageOnTarget(Self, Image, ABounds, ColorSpace, Tolerance, Multipliers, MinCount)) >= MinCount;
end;

function TSimbaTarget.FindTemplate(Templ: TSimbaImage; out Match: Single; ABounds: TBox): TPoint;
begin
  Result := FindTemplateOnTarget(Self, Templ, Match, ABounds);
end;

function TSimbaTarget.HasTemplate(Templ: TSimbaImage; MinMatch: Single; ABounds: TBox): Boolean;
begin
  Result := HasTemplateOnTarget(Self, Templ, MinMatch, ABounds);
end;

function TSimbaTarget.FindDTMEx(DTM: TDTM; MaxToFind: Integer; ABounds: TBox): TPointArray;
begin
  Result := FindDTMOnTarget(Self, DTM, ABounds, MaxToFind);
end;

function TSimbaTarget.FindDTMRotatedEx(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; MaxToFind: Integer; ABounds: TBox): TPointArray;
begin
  Result := FindDTMRotatedOnTarget(Self, DTM, StartDegrees, EndDegrees, Step, FoundDegrees, ABounds, MaxToFind);
end;

function TSimbaTarget.FindDTM(DTM: TDTM; ABounds: TBox): TPoint;
var
  TPA: TPointArray;
begin
  TPA := FindDTMOnTarget(Self, DTM, ABounds, 1);
  if (Length(TPA) > 0) then
    Result := TPA[0]
  else
    Result := TPoint.Create(-1, -1);
end;

function TSimbaTarget.FindDTMRotated(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; ABounds: TBox): TPoint;
var
  TPA: TPointArray;
begin
  TPA := FindDTMRotatedOnTarget(Self, DTM, StartDegrees, EndDegrees, Step, FoundDegrees, ABounds, 1);
  if (Length(TPA) > 0) then
    Result := TPA[0]
  else
    Result := TPoint.Create(-1, -1);
end;

function TSimbaTarget.GetPixelDifference(WaitTime: Integer; ABounds: TBox): Integer;
var
  ImgBefore, ImgAfter: TSimbaImage;
begin
  Result := 0;

  ImgBefore := nil;
  ImgAfter := nil;
  if GetImageDataAsImage(ABounds, ImgBefore) then
  try
    Sleep(WaitTime);
    if GetImageDataAsImage(ABounds, ImgAfter) and (ImgBefore.Width = ImgAfter.Width) and (ImgBefore.Height = ImgAfter.Height) then
      Result := ImgBefore.PixelDifference(ImgAfter);
  finally
    if (ImgBefore <> nil) then
      ImgBefore.Free();
    if (ImgAfter <> nil) then
      ImgAfter.Free();
  end;
end;

function TSimbaTarget.GetPixelDifference(WaitTime: Integer; Tolerance: Single; ABounds: TBox): Integer;
var
  ImgBefore, ImgAfter: TSimbaImage;
begin
  Result := 0;

  ImgBefore := nil;
  ImgAfter := nil;
  if GetImageDataAsImage(ABounds, ImgBefore) then
  try
    Sleep(WaitTime);
    if GetImageDataAsImage(ABounds, ImgAfter) and (ImgBefore.Width = ImgAfter.Width) and (ImgBefore.Height = ImgAfter.Height) then
      Result := ImgBefore.PixelDifference(ImgAfter, Tolerance);
  finally
    if (ImgBefore <> nil) then
      ImgBefore.Free();
    if (ImgAfter <> nil) then
      ImgAfter.Free();
  end;
end;

function TSimbaTarget.GetPixelDifferenceTPA(WaitTime: Integer; ABounds: TBox): TPointArray;
var
  ImgBefore, ImgAfter: TSimbaImage;
begin
  Result := nil;

  ImgBefore := nil;
  ImgAfter := nil;
  if GetImageDataAsImage(ABounds, ImgBefore) then
  try
    Sleep(WaitTime);
    if GetImageDataAsImage(ABounds, ImgAfter) and (ImgBefore.Width = ImgAfter.Width) and (ImgBefore.Height = ImgAfter.Height) then
      Result := ImgBefore.PixelDifferenceTPA(ImgAfter);
  finally
    if (ImgBefore <> nil) then
      ImgBefore.Free();
    if (ImgAfter <> nil) then
      ImgAfter.Free();
  end;
end;

function TSimbaTarget.GetPixelDifferenceTPA(WaitTime: Integer; Tolerance: Single; ABounds: TBox): TPointArray;
var
  ImgBefore, ImgAfter: TSimbaImage;
begin
  Result := nil;

  ImgBefore := nil;
  ImgAfter := nil;
  if GetImageDataAsImage(ABounds, ImgBefore) then
  try
    Sleep(WaitTime);
    if GetImageDataAsImage(ABounds, ImgAfter) and (ImgBefore.Width = ImgAfter.Width) and (ImgBefore.Height = ImgAfter.Height) then
      Result := ImgBefore.PixelDifferenceTPA(ImgAfter, Tolerance);
  finally
    if (ImgBefore <> nil) then
      ImgBefore.Free();
    if (ImgAfter <> nil) then
      ImgAfter.Free();
  end;
end;

function TSimbaTarget.AverageBrightness(ABounds: TBox): Integer;
var
  Image: TSimbaImage;
begin
  if GetImageDataAsImage(ABounds, Image) then
  try
    Result := Image.AverageBrightness();
  finally
    Image.Free();
  end;
end;

function TSimbaTarget.PeakBrightness(ABounds: TBox): Integer;
var
  Image: TSimbaImage;
begin
  if GetImageDataAsImage(ABounds, Image) then
  try
    Result := Image.PeakBrightness();
  finally
    Image.Free();
  end;
end;

function TSimbaTarget.FindEdges(MinDiff: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; ABounds: TBox): TPointArray;
var
  Image: TSimbaImage;
begin
  if GetImageDataAsImage(ABounds, Image) then
  try
    Result := Image.FindEdges(MinDiff, ColorSpace, Multipliers);
  finally
    Image.Free();
  end;
end;

function TSimbaTarget.FindEdges(MinDiff: Single; ABounds: TBox): TPointArray;
var
  Image: TSimbaImage;
begin
  if GetImageDataAsImage(ABounds, Image) then
  try
    Result := Image.FindEdges(MinDiff);
  finally
    Image.Free();
  end;
end;

function TSimbaTarget.AddTargetChangeEvent(Event: TTargetEvent): TTargetEvent;
begin
  Result := TTargetEvent(AppendEvent(FChangeEvents, TMethod(Event)));
end;

function TSimbaTarget.AddTargetInvalidEvent(Event: TTargetEvent): TTargetEvent;
begin
  Result := TTargetEvent(AppendEvent(FInvalidEvents, TMethod(Event)));
end;

procedure TSimbaTarget.RemoveTargetChangeEvent(Event: TTargetEvent);
begin
  DeleteEvent(FChangeEvents, TMethod(Event));
end;

procedure TSimbaTarget.RemoveTargetInvalidEvent(Event: TTargetEvent);
begin
  DeleteEvent(FInvalidEvents, TMethod(Event));
end;

procedure TSimbaTarget.CheckMethod(Method: Pointer; Name: String);
begin
  if (Method = nil) then
    SimbaException('Target "%s" cannot %s', [TargetName[FTarget.Kind], Name]);
end;

function TSimbaTarget.GetMouseX: Integer;
begin
  Result := MouseXY.X;
end;

function TSimbaTarget.GetMouseXY: TPoint;
begin
  CheckMethod(FTarget.MousePosition, 'MousePosition');

  Result := FTarget.MousePosition(FTarget.Target);
end;

function TSimbaTarget.GetMouseY: Integer;
begin
  Result := MouseXY.Y;
end;

procedure TSimbaTarget.SetMouseX(Value: Integer);
begin
  MouseTeleport(TPoint.Create(Value, MouseY));
end;

procedure TSimbaTarget.SetMouseY(Value: Integer);
begin
  MouseTeleport(TPoint.Create(MouseX, Value));
end;

procedure TSimbaTarget.SetMouseXY(Value: TPoint);
begin
  MouseTeleport(Value);
end;

function TSimbaTarget.AddMouseEvent(Event: TMouseButtonEvent): TMouseButtonEvent;
begin
  Result := TMouseButtonEvent(AppendEvent(MouseOptions.ButtonEvents, TMethod(Event)));
end;

function TSimbaTarget.AddMouseEvent(Event: TMouseTeleportEvent): TMouseTeleportEvent;
begin
  Result := TMouseTeleportEvent(AppendEvent(MouseOptions.TeleportEvents, TMethod(Event)));
end;

function TSimbaTarget.AddMouseEvent(Event: TMouseMovingEvent): TMouseMovingEvent;
begin
  Result := TMouseMovingEvent(AppendEvent(MouseOptions.MovingEvents, TMethod(Event)));
end;

procedure TSimbaTarget.RemoveMouseEvent(Event: TMouseButtonEvent);
begin
  DeleteEvent(MouseOptions.ButtonEvents, TMethod(Event));
end;

procedure TSimbaTarget.RemoveMouseEvent(Event: TMouseTeleportEvent);
begin
  DeleteEvent(MouseOptions.TeleportEvents, TMethod(Event));
end;

procedure TSimbaTarget.RemoveMouseEvent(Event: TMouseMovingEvent);
begin
  DeleteEvent(MouseOptions.MovingEvents, TMethod(Event));
end;

procedure TSimbaTarget.ChangeTarget(Kind: ESimbaTargetKind);
begin
  FTarget := Default(TSimbaTargetInfo);
  FTarget.Kind := Kind;
end;

procedure TSimbaTarget.TargetChanged;
var
  I: Integer;
begin
  for I := 0 to High(FChangeEvents) do
    TTargetEvent(FChangeEvents[I])(@Self);
end;

procedure TSimbaTarget.CheckInvalidTarget;
var
  Attempt, I: Integer;
begin
  if IsValid() then
    Exit;

  if (Length(FInvalidEvents) > 0) then
    for Attempt := 1 to 5 do
    begin
      Sleep(100);

      for I := 0 to High(FInvalidEvents) do
      begin
        TTargetEvent(FInvalidEvents[I])(@Self);
        if IsValid() then
          Exit;
      end;
    end;

  SimbaException('Target is invalid: %s', [ToString()]);
end;

procedure TSimbaTarget.CheckAutoFocus;
begin
  if FAutoFocus and (not IsFocused()) then
    Focus();
end;

function TSimbaTarget.IsValid: Boolean;
begin
  CheckMethod(FTarget.IsValid, 'IsValid');

  Result := FTarget.IsValid(FTarget.Target);
end;

function TSimbaTarget.IsFocused: Boolean;
begin
  CheckMethod(FTarget.IsFocused, 'IsFocused');

  Result := FTarget.IsFocused(FTarget.Target);
end;

function TSimbaTarget.Focus: Boolean;
begin
  CheckMethod(FTarget.Focus, 'Focus');

  Result := FTarget.Focus(FTarget.Target);
end;

function TSimbaTarget.GetBounds: TBox;
begin
  with Size do
  begin
    Result.X1 := 0;
    Result.Y1 := 0;
    Result.X2 := Width - 1;
    Result.Y2 := Height - 1;
  end;
end;

function TSimbaTarget.GetSize: TSize;
begin
  CheckMethod(FTarget.GetDimensions, 'GetDimensions');
  CheckInvalidTarget();

  FTarget.GetDimensions(FTarget.Target, Result.cx, Result.cy);
end;

function TSimbaTarget.GetWidth: Integer;
begin
  Result := Size.Width;
end;

function TSimbaTarget.GetHeight: Integer;
begin
  Result := Size.Height;
end;

function TSimbaTarget.GetImage(ABounds: TBox): TSimbaImage;
var
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  if GetImageData(ABounds, Data, DataWidth) then
  try
    Result := TSimbaImage.CreateFromData(ABounds.Width, ABounds.Height, Data, DataWidth);
    Result.FillWithAlpha(ALPHA_OPAQUE);
  finally
    FreeImageData(Data);
  end
  else
    Result := TSimbaImage.Create();
end;

procedure TSimbaTarget.SetDesktop;
begin
  SetWindow(SimbaNativeInterface.GetDesktopWindow());
end;

procedure TSimbaTarget.SetWindow(Window: TWindowHandle);
begin
  ChangeTarget(ESimbaTargetKind.WINDOW);

  FTarget.Window := Window;
  FTarget.Target := @FTarget.Window;

  FTarget.Focus := @WindowTarget_Focus;
  FTarget.IsFocused := @WindowTarget_IsFocused;
  FTarget.IsValid := @WindowTarget_IsValid;

  FTarget.KeyDown := @WindowTarget_KeyDown;
  FTarget.KeyUp := @WindowTarget_KeyUp;
  FTarget.KeySend := @WindowTarget_KeySend;
  FTarget.KeyPressed := @WindowTarget_KeyPressed;

  FTarget.MouseTeleport := @WindowTarget_MouseTeleport;
  FTarget.MousePosition := @WindowTarget_MousePosition;
  FTarget.MousePressed := @WindowTarget_MousePressed;
  FTarget.MouseDown := @WindowTarget_MouseDown;
  FTarget.MouseUp := @WindowTarget_MouseUp;
  FTarget.MouseScroll := @WindowTarget_MouseScroll;

  FTarget.GetDimensions := @WindowTarget_GetDimensions;
  FTarget.GetImageData := @WindowTarget_GetImageData;

  TargetChanged();
end;

procedure TSimbaTarget.SetImage(Image: TSimbaImage);
begin
  ChangeTarget(ESimbaTargetKind.IMAGE);

  FTarget.Image := Image;
  FTarget.Target := FTarget.Image;

  FTarget.GetDimensions := @ImageTarget_GetDimensions;
  FTarget.GetImageData := @ImageTarget_GetImageData;
  FTarget.IsValid := @ImageTarget_IsValid;

  TargetChanged();
end;

procedure TSimbaTarget.SetEIOS(FileName, Args: String);
begin
  ChangeTarget(ESimbaTargetKind.EIOS);

  FTarget.EIOS := LoadEIOS(FileName, Args);
  FTarget.Target := @FTarget.EIOS;

  FTarget.KeyDown := @EIOSTarget_KeyDown;
  FTarget.KeyUp := @EIOSTarget_KeyUp;
  FTarget.KeySend := @EIOSTarget_KeySend;
  FTarget.KeyPressed := @EIOSTarget_KeyPressed;

  FTarget.MouseTeleport := @EIOSTarget_MouseTeleport;
  FTarget.MousePosition := @EIOSTarget_MousePosition;
  FTarget.MousePressed := @EIOSTarget_MousePressed;
  FTarget.MouseDown := @EIOSTarget_MouseDown;
  FTarget.MouseUp := @EIOSTarget_MouseUp;
  FTarget.MouseScroll := @EIOSTarget_MouseScroll;

  FTarget.GetDimensions := @EIOSTarget_GetDimensions;
  FTarget.GetImageData := @EIOSTarget_GetImageData;

  FTarget.IsValid := @EIOSTarget_IsValid;

  TargetChanged();
end;

procedure TSimbaTarget.SetPlugin(FileName, Args: String);
begin
  ChangeTarget(ESimbaTargetKind.PLUGIN);

  FTarget.Plugin := LoadPluginTarget(FileName, Args);
  FTarget.Target := @FTarget.Plugin;

  FTarget.KeyDown := @PluginTarget_KeyDown;
  FTarget.KeyUp := @PluginTarget_KeyUp;
  FTarget.KeySend := @PluginTarget_KeySend;
  FTarget.KeyPressed := @PluginTarget_KeyPressed;

  FTarget.MouseTeleport := @PluginTarget_MouseTeleport;
  FTarget.MousePosition := @PluginTarget_MousePosition;
  FTarget.MousePressed := @PluginTarget_MousePressed;
  FTarget.MouseDown := @PluginTarget_MouseDown;
  FTarget.MouseUp := @PluginTarget_MouseUp;
  FTarget.MouseScroll := @PluginTarget_MouseScroll;

  FTarget.GetDimensions := @PluginTarget_GetDimensions;
  FTarget.GetImageData := @PluginTarget_GetImageData;

  FTarget.IsValid := @PluginTarget_IsValid;

  TargetChanged();
end;

procedure TSimbaTarget.SetPlugin(FileName, Args: String; out DebugImage: TSimbaExternalCanvas);
begin
  ChangeTarget(ESimbaTargetKind.PLUGIN);

  FTarget.Plugin := LoadPluginTarget(FileName, Args, DebugImage);
  FTarget.Target := @FTarget.Plugin;

  FTarget.KeyDown := @PluginTarget_KeyDown;
  FTarget.KeyUp := @PluginTarget_KeyUp;
  FTarget.KeySend := @PluginTarget_KeySend;
  FTarget.KeyPressed := @PluginTarget_KeyPressed;

  FTarget.MouseTeleport := @PluginTarget_MouseTeleport;
  FTarget.MousePosition := @PluginTarget_MousePosition;
  FTarget.MousePressed := @PluginTarget_MousePressed;
  FTarget.MouseDown := @PluginTarget_MouseDown;
  FTarget.MouseUp := @PluginTarget_MouseUp;
  FTarget.MouseScroll := @PluginTarget_MouseScroll;

  FTarget.GetDimensions := @PluginTarget_GetDimensions;
  FTarget.GetImageData := @PluginTarget_GetImageData;

  FTarget.IsValid := @PluginTarget_IsValid;

  TargetChanged();
end;

function TSimbaTarget.ValidateBounds(var ABounds: TBox): Boolean;

  procedure ValidateBoundsInCustomClientArea;
  begin
    if (ABounds.X1 = -1) and (ABounds.Y1 = -1) and (ABounds.X2 = -1) and (ABounds.Y2 = -1) then
      ABounds := FCustomClientArea
    else
    begin
      ABounds := ABounds.Offset(FCustomClientArea.TopLeft);

      if (ABounds.X1 < FCustomClientArea.X1)  then ABounds.X1 := FCustomClientArea.X1;
      if (ABounds.Y1 < FCustomClientArea.Y1)  then ABounds.Y1 := FCustomClientArea.Y1;
      if (ABounds.X2 >= FCustomClientArea.X2) then ABounds.X2 := FCustomClientArea.X2 - 1;
      if (ABounds.Y2 >= FCustomClientArea.Y2) then ABounds.Y2 := FCustomClientArea.Y2 - 1;
    end;
  end;

var
  W, H: Integer;
begin
  with Size do
  begin
    W := Width;
    H := Height;
  end;

  if (FCustomClientArea = TBox.ZERO) then
    if (ABounds.X1 = -1) and (ABounds.Y1 = -1) and (ABounds.X2 = -1) and (ABounds.Y2 = -1) then
    begin
      ABounds.X1 := 0;
      ABounds.Y1 := 0;
      ABounds.X2 := W - 1;
      ABounds.Y2 := H - 1;
    end else
    begin
      if (ABounds.X1 < 0)  then ABounds.X1 := 0;
      if (ABounds.Y1 < 0)  then ABounds.Y1 := 0;
      if (ABounds.X2 >= W) then ABounds.X2 := W - 1;
      if (ABounds.Y2 >= H) then ABounds.Y2 := H - 1;
    end
  else
    ValidateBoundsInCustomClientArea();

  Result := (ABounds.Width > 0) and (ABounds.Height > 0);
end;

function TSimbaTarget.GetImageDataAsImage(var ABounds: TBox; out Image: TSimbaImage): Boolean;
var
  Data: PColorBGRA = nil;
  DataWidth: Integer;
  Y: Integer;
begin
  Result := GetImageData(ABounds, Data, DataWidth);
  if Result then
  begin
    Image := TSimbaImage.Create(ABounds.Width, ABounds.Height);
    for Y := 0 to Image.Height - 1 do
      Move(Data[Y * DataWidth], Image.Data[Y * Image.Width], Image.Width * SizeOf(TColorBGRA));

    FreeImageData(Data);
  end;
end;

function TSimbaTarget.GetImageData(var ABounds: TBox; var Data: PColorBGRA; var DataWidth: Integer): Boolean;
begin
  CheckMethod(FTarget.GetImageData, 'GetImageData');
  if IsImageFrozen() then
  begin
    Data := @FFrozen.Data[0];
    DataWidth := FFrozen.DataWidth;
    ABounds := FFrozen.Bounds;

    Exit(True);
  end;

  Data := nil;
  Result := ValidateBounds(ABounds) and FTarget.GetImageData(FTarget.Target, ABounds.X1, ABounds.Y1, ABounds.Width, ABounds.Height, Data, DataWidth);
end;

procedure TSimbaTarget.FreeImageData(var Data: PColorBGRA);
begin
  if IsImageFrozen() and (Data = @FFrozen.Data[0]) then
    Exit;
  if (FTarget.Kind in [ESimbaTargetKind.WINDOW]) then
    FreeMem(Data);
end;

function TSimbaTarget.IsImageFrozen: Boolean;
begin
  Result := Length(FFrozen.Data) > 0;
end;

procedure TSimbaTarget.FreezeImage(ABounds: TBox);
var
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  if GetImageData(ABounds, Data, DataWidth) then
  try
    FFrozen.Bounds := ABounds;
    FFrozen.DataWidth := DataWidth;

    SetLength(FFrozen.Data, DataWidth * ABounds.Height);
    Move(Data^, FFrozen.Data[0], Length(FFrozen.Data) * SizeOf(TColorBGRA));
  finally
    FreeImageData(Data);
  end;
end;

procedure TSimbaTarget.UnFreezeImage;
begin
  FFrozen.Data := [];
end;

function TSimbaTarget.Copy: TSimbaTarget;

  procedure MakeUnique(var Arr: TEventArray);
  begin
    if (Pointer(Arr) <> nil) then
      Arr := System.Copy(Arr);
  end;

begin
  Result := Self;

  MakeUnique(Result.FInvalidEvents);
  MakeUnique(Result.FChangeEvents);
  MakeUnique(Result.MouseOptions.ButtonEvents);
  MakeUnique(Result.MouseOptions.TeleportEvents);
  MakeUnique(Result.MouseOptions.MovingEvents);
end;

function TSimbaTarget.ToString: String;
begin
  Result := 'ESimbaTargetKind.' + TargetName[FTarget.Kind];

  case FTarget.Kind of
    ESimbaTargetKind.IMAGE:
      if (FTarget.Image = nil) then
        Result := 'IMAGE: TImage(nil)'
      else
        Result := 'IMAGE: TImage(%P), Size=%dx%d'.Format([Pointer(FTarget.Image), FTarget.Image.Width, FTarget.Image.Height]);

    ESimbaTargetKind.WINDOW:
      Result := 'WINDOW: Handle=%d, Valid: %s'.Format([FTarget.Window, BoolToStr(IsValid(), True)]);

    ESimbaTargetKind.EIOS:
      Result := 'EIOS: File="%s" Target=%P'.Format([FTarget.EIOS.FileName, FTarget.EIOS.Target]);

    ESimbaTargetKind.PLUGIN:
      Result := 'PLUGIN: File="%s" Target=%P'.Format([FTarget.Plugin.FileName, FTarget.Plugin.Target]);
  end;
end;

class operator TSimbaTarget.Initialize(var Self: TSimbaTarget);
begin
  Self := Default(TSimbaTarget);
end;

end.

