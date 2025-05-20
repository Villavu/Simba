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
  simba.base, simba.baseclass, simba.image, simba.image_utils, simba.externalcanvas,
  simba.target_eios, simba.target_window, simba.target_image, simba.target_plugin,
  simba.colormath, simba.dtm,
  simba.vartype_quad, simba.containers;

type
  {$PUSH}
  {$SCOPEDENUMS ON}
  ESimbaTargetKind = (NONE, IMAGE, WINDOW, EIOS, PLUGIN);
  {$POP}

const
  TargetName: array[ESimbaTargetKind] of String = ('NONE', 'IMAGE', 'WINDOW', 'EIOS', 'PLUGIN');

type
  TSimbaTargetMethods = record
    GetDimensions: procedure(Target: Pointer; out W, H: Integer);
    GetImageData: function(Target: Pointer; X, Y, Width, Height: Integer; out Data: PColorBGRA; out DataWidth: Integer): Boolean;

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

  {$scopedenums on}
  ETargetEvent = (
    TARGET_CHANGE, TARGET_INVALID, TARGET_RESIZE,
    MOUSE_TELEPORT, MOUSE_BUTTON
  );
  {$scopedenums off}

  TTargetEventData = record
    Event: ETargetEvent;

    TargetResize: record
      Width, Height: Integer;
    end;
    TargetChange: record
      { nothing }
    end;
    TargetInvalid: record
      { nothing }
    end;

    MouseButton: record
      Button: EMouseButton;
      Down: Boolean;
    end;

    MouseTeleport: record
      X: Integer;
      Y: Integer;
    end;
  end;

  TSimbaTarget = class;
  TSimbaTargetEvent = procedure(Target: TSimbaTarget; Data: TTargetEventData; UserData: Pointer) of object;

  TSimbaTargetEventManager = class
  protected
    FEvents: array[ETargetEvent] of array of record
      Method: TSimbaTargetEvent;
      UserData: Pointer;
    end;
  public
    destructor Destroy; override;

    function Add(Event: ETargetEvent; Method: TSimbaTargetEvent; UserData: Pointer; UserDataSize: Integer): Integer;
    procedure Remove(Event: ETargetEvent; Index: Integer);
    function Has(Event: ETargetEvent): Boolean;

    procedure Call(Target: TSimbaTarget; var Data: TTargetEventData);
    procedure CallTeleportEvent(Target: TSimbaTarget; X, Y: Integer);
    procedure CallMouseButtonEvent(Target: TSimbaTarget; Button: EMouseButton; Down: Boolean);
    procedure CallResizeEvent(Target: TSimbaTarget; Width, Height: Integer);
    procedure CallTargetChange(Target: TSimbaTarget);
    procedure CallTargetInvalid(Target: TSimbaTarget);
  end;

  TSimbaTargetOptions = class
  public
    ForceFocus: Boolean;

    MousePressMin: Integer;
    MousePressMax: Integer;
    MouseSpeed: Double;
    MouseGravity: Double;
    MouseWind: Double;
    MouseTimeout: Integer;

    KeyPressMin: Integer;
    KeyPressMax: Integer;

    constructor Create;
  end;

  TSimbaTarget = class(TSimbaBaseClass)
  private
    FTargetKind: ESimbaTargetKind;
    FTargetImage: TSimbaImage;
    FTargetWindow: TWindowHandle;
    FTargetEIOS: TEIOSTarget;
    FTargetPlugin: TSimbaPluginTarget;
    FTarget: Pointer;
    FTargetMethods: TSimbaTargetMethods;
    FOptions: TSimbaTargetOptions;

    FFrozenImage: record
      Bounds: TBox;
      DataWidth: Integer;
      Data: array of TColorBGRA;
    end;

    FEventManager: TSimbaTargetEventManager;
    FCustomClientArea: TBox;
    FLastSize: TSize;

    function ValidateBounds(var ABounds: TBox): Boolean;

    procedure ChangeTarget(Kind: ESimbaTargetKind);
    procedure TargetChanged;

    procedure CheckMethod(Method: Pointer; AName: String);
    procedure CheckResize(NewSize: TSize);
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
    function AddEvent(Event: ETargetEvent; Method: TSimbaTargetEvent; UserData: Pointer; UserDataSize: Integer): Integer;
    procedure RemoveEvent(Event: ETargetEvent; Index: Integer);

    // target
    procedure SetDesktop;
    procedure SetWindow(Window: TWindowHandle);
    procedure SetImage(Image: TSimbaImage);
    procedure SetEIOS(FileName, Args: String);
    procedure SetPlugin(FileName, Args: String); overload;
    procedure SetPlugin(FileName, Args: String; out DebugImage: TSimbaExternalCanvas); overload;

    function GetImageDataAsImage(var ABounds: TBox; out Image: TSimbaImage): Boolean;
    function GetImageData(var ABounds: TBox; out Data: PColorBGRA; out DataWidth: Integer): Boolean;
    procedure FreeImageData(var Data: PColorBGRA);

    function IsImageFrozen: Boolean;
    procedure FreezeImage(ABounds: TBox);
    procedure UnFreezeImage;

    function GetImage(ABounds: TBox): TSimbaImage; overload;
    function GetImage: TSimbaImage; overload;

    function IsValid: Boolean;
    function IsFocused: Boolean;
    function Focus: Boolean;

    function ToString: String; override;

    property TargetKind: ESimbaTargetKind read FTargetKind;
    property Bounds: TBox read GetBounds;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Size: TSize read GetSize;
    property Options: TSimbaTargetOptions read FOptions write FOptions;

    property CustomClientArea: TBox read FCustomClientArea write FCustomClientArea;

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
    function FindColor(Color: TColorTolerance; ABounds: TBox): TPointArray; overload;

    function CountColor(Color: TColor; Tolerance: Single; ABounds: TBox): Integer; overload;
    function CountColor(Color: TColorTolerance; ABounds: TBox): Integer; overload;

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

    // Finder - template
    function FindTemplate(Templ: TSimbaImage; out Match: Single; ABounds: TBox): TPoint;

    // Finder - dtm
    function FindDTM(DTM: TDTM; ABounds: TBox): TPoint;
    function FindDTMEx(DTM: TDTM; MaxToFind: Integer; ABounds: TBox): TPointArray;
    function FindDTMRotated(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; ABounds: TBox): TPoint;
    function FindDTMRotatedEx(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; MaxToFind: Integer; ABounds: TBox): TPointArray;

    // Finder - other
    function FindEdges(MinDiff: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; ABounds: TBox): TPointArray; overload;
    function FindEdges(MinDiff: Single; ABounds: TBox): TPointArray; overload;

    function GetPixelDifference(WaitTime: Integer; ABounds: TBox): TPointArray; overload;
    function GetPixelDifference(WaitTime: Integer; Tolerance: Single; ABounds: TBox): TPointArray; overload;

    function AverageBrightness(ABounds: TBox): Integer;
    function PeakBrightness(ABounds: TBox): Integer;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  simba.nativeinterface, simba.vartype_box, simba.target_movemouse, simba.random,
  simba.finder_color, simba.finder_image, simba.finder_dtm;

function TSimbaTargetEventManager.Add(Event: ETargetEvent; Method: TSimbaTargetEvent; UserData: Pointer; UserDataSize: Integer): Integer;

  procedure DoAdd(Index: Integer);
  begin
    FEvents[Event][Index].Method := Method;
    FEvents[Event][Index].UserData := nil;

    if (UserDataSize > 0) then
    begin
      FEvents[Event][Index].UserData := GetMem(UserDataSize);
      Move(UserData^, FEvents[Event][Index].UserData^, UserDataSize);
    end;
  end;

begin
  // find a free space
  for Result := 0 to High(FEvents[Event]) do
    if (FEvents[Event][Result].Method = nil) then
    begin
      DoAdd(Result);
      Exit;
    end;

  // append
  Result := Length(FEvents[Event]);
  SetLength(FEvents[Event], Result + 1);
  DoAdd(Result);
end;

procedure TSimbaTargetEventManager.Remove(Event: ETargetEvent; Index: Integer);
begin
  if (Index < 0) or (Index > High(FEvents[Event])) then
    Exit;

  FEvents[Event][Index].Method := nil;
  if (FEvents[Event][Index].UserData <> nil) then
    FreeMemAndNil(FEvents[Event][Index].UserData);
end;

function TSimbaTargetEventManager.Has(Event: ETargetEvent): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(FEvents[Event]) do
    if Assigned(FEvents[Event][I].Method) then
      Exit(True);
  Result := False;
end;

procedure TSimbaTargetEventManager.Call(Target: TSimbaTarget; var Data: TTargetEventData);
var
  I: Integer;
begin
  for I := 0 to High(FEvents[Data.Event]) do
    with FEvents[Data.Event][I] do
      if Assigned(Method) then
        Method(Target, Data, UserData);
end;

procedure TSimbaTargetEventManager.CallTeleportEvent(Target: TSimbaTarget; X, Y: Integer);
var
  Data: TTargetEventData;
begin
  if Has(ETargetEvent.MOUSE_TELEPORT) then
  begin
    Data := Default(TTargetEventData);
    Data.Event := ETargetEvent.MOUSE_TELEPORT;
    Data.MouseTeleport.X := X;
    Data.MouseTeleport.Y := Y;

    Call(Target, Data);
  end;
end;

procedure TSimbaTargetEventManager.CallMouseButtonEvent(Target: TSimbaTarget; Button: EMouseButton; Down: Boolean);
var
  Data: TTargetEventData;
begin
  if Has(ETargetEvent.MOUSE_BUTTON) then
  begin
    Data := Default(TTargetEventData);
    Data.Event := ETargetEvent.MOUSE_BUTTON;
    Data.MouseButton.Button := Button;
    Data.MouseButton.Down   := Down;

    Call(Target, Data);
  end;
end;

procedure TSimbaTargetEventManager.CallResizeEvent(Target: TSimbaTarget; Width, Height: Integer);
var
  Data: TTargetEventData;
begin
  if Has(ETargetEvent.TARGET_RESIZE) then
  begin
    Data := Default(TTargetEventData);
    Data.Event := ETargetEvent.TARGET_RESIZE;
    Data.TargetResize.Width := Width;
    Data.TargetResize.Height := Height;

    Call(Target, Data);
  end;
end;

procedure TSimbaTargetEventManager.CallTargetChange(Target: TSimbaTarget);
var
  Data: TTargetEventData;
begin
  if Has(ETargetEvent.TARGET_CHANGE) then
  begin
    Data := Default(TTargetEventData);
    Data.Event := ETargetEvent.TARGET_CHANGE;

    Call(Target, Data);
  end;
end;

procedure TSimbaTargetEventManager.CallTargetInvalid(Target: TSimbaTarget);
var
  Data: TTargetEventData;
begin
  if Has(ETargetEvent.TARGET_INVALID) then
  begin
    Data := Default(TTargetEventData);
    Data.Event := ETargetEvent.TARGET_INVALID;

    Call(Target, Data);
  end;
end;

destructor TSimbaTargetEventManager.Destroy;
var
  Event: ETargetEvent;
  I: Integer;
begin
  for Event in ETargetEvent do
    for I := 0 to High(FEvents[Event]) do
      if (FEvents[Event][I].UserData <> nil) then
        FreeMemAndNil(FEvents[Event][I].UserData);

  inherited Destroy();
end;

constructor TSimbaTargetOptions.Create;
begin
  inherited Create();

  Self.ForceFocus := False;

  Self.MousePressMin := 40;
  Self.MousePressMax := 220;
  Self.MouseSpeed    := 10;
  Self.MouseGravity  := 9;
  Self.MouseWind     := 4;
  Self.MouseTimeout  := 15000;

  Self.KeyPressMin := 20;
  Self.KeyPressMax := 125;
end;

function TSimbaTarget.MousePressed(Button: EMouseButton): Boolean;
begin
  CheckMethod(FTargetMethods.MousePressed, 'MousePressed');

  Result := FTargetMethods.MousePressed(FTarget, Button);
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
  Time := RandomLeft(FOptions.MousePressMin, FOptions.MousePressMax);

  MouseDown(Button);
  SimbaNativeInterface.PreciseSleep(Time);
  MouseUp(Button);
end;

procedure TSimbaTarget.MouseTeleport(P: TPoint);
begin
  CheckMethod(FTargetMethods.MouseTeleport, 'MouseTeleport');

  FEventManager.CallTeleportEvent(Self, P.X, P.Y);
  FTargetMethods.MouseTeleport(FTarget, P);
end;

procedure TSimbaTarget.MouseDown(Button: EMouseButton);
begin
  CheckMethod(FTargetMethods.MouseDown, 'MouseDown');
  CheckAutoFocus();

  FEventManager.CallMouseButtonEvent(Self, Button, True);
  FTargetMethods.MouseDown(FTarget, Button);
end;

procedure TSimbaTarget.MouseUp(Button: EMouseButton);
begin
  CheckMethod(FTargetMethods.MouseUp, 'MouseUp');
  CheckAutoFocus();

  FEventManager.CallMouseButtonEvent(Self, Button, False);
  FTargetMethods.MouseUp(FTarget, Button);
end;

procedure TSimbaTarget.MouseScroll(Scrolls: Integer);
begin
  CheckMethod(FTargetMethods.MouseScroll, 'MouseScroll');
  CheckAutoFocus();

  FTargetMethods.MouseScroll(FTarget, Scrolls);
end;

procedure TSimbaTarget.KeySend(Text: String);
var
  I: Integer;
  SleepTimes: TIntegerArray;
begin
  CheckMethod(FTargetMethods.KeySend, 'KeySend');
  CheckAutoFocus();

  if (Length(Text) > 0) then
  begin
    SetLength(SleepTimes, Length(Text) * 4);
    for I := 0 to High(SleepTimes) do
      SleepTimes[I] := RandomLeft(FOptions.KeyPressMin, FOptions.KeyPressMax);

    FTargetMethods.KeySend(FTarget, PChar(Text), Length(Text), @SleepTimes[0]);
  end;
end;

procedure TSimbaTarget.KeyPress(Key: EKeyCode);
var
  Time: Integer;
begin
  Time := RandomLeft(FOptions.KeyPressMin, FOptions.KeyPressMax);

  KeyDown(Key);
  SimbaNativeInterface.PreciseSleep(Time);
  KeyUp(Key);
end;

procedure TSimbaTarget.KeyDown(Key: EKeyCode);
begin
  CheckMethod(FTargetMethods.KeyDown, 'KeyDown');
  CheckAutoFocus();

  FTargetMethods.KeyDown(FTarget, Key);
end;

procedure TSimbaTarget.KeyUp(Key: EKeyCode);
begin
  CheckMethod(FTargetMethods.KeyUp, 'KeyUp');
  CheckAutoFocus();

  FTargetMethods.KeyUp(FTarget, Key);
end;

function TSimbaTarget.KeyPressed(Key: EKeyCode): Boolean;
begin
  CheckMethod(FTargetMethods.KeyPressed, 'KeyPressed');

  Result := FTargetMethods.KeyPressed(FTarget, Key);
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

function TSimbaTarget.FindColor(Color: TColorTolerance; ABounds: TBox): TPointArray;
begin
  Result := FindColorsOnTarget(Self, ABounds, Color.ColorSpace, Color.Color, Color.Tolerance, Color.Multipliers);
end;

function TSimbaTarget.CountColor(Color: TColor; Tolerance: Single; ABounds: TBox): Integer;
begin
  Result := CountColorsOnTarget(Self, ABounds, DefaultColorSpace, Color, Tolerance, DefaultMultipliers);
end;

function TSimbaTarget.CountColor(Color: TColorTolerance; ABounds: TBox): Integer;
begin
  Result := CountColorsOnTarget(Self, ABounds, Color.ColorSpace, Color.Color, Color.Tolerance, Color.Multipliers);
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

function TSimbaTarget.FindTemplate(Templ: TSimbaImage; out Match: Single; ABounds: TBox): TPoint;
begin
  Result := FindTemplateOnTarget(Self, Templ, Match, ABounds);
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

function TSimbaTarget.GetPixelDifference(WaitTime: Integer; ABounds: TBox): TPointArray;
var
  ImgBefore, ImgAfter: TSimbaImage;
begin
  Result := [];

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

function TSimbaTarget.GetPixelDifference(WaitTime: Integer; Tolerance: Single; ABounds: TBox): TPointArray;
var
  ImgBefore, ImgAfter: TSimbaImage;
begin
  Result := [];

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

function TSimbaTarget.AverageBrightness(ABounds: TBox): Integer;
begin
  Result := AverageBrightnessOnTarget(Self, Bounds);
end;

function TSimbaTarget.PeakBrightness(ABounds: TBox): Integer;
begin
  Result := PeakBrightnessOnTarget(Self, Bounds);
end;

constructor TSimbaTarget.Create;
begin
  inherited Create();

  FEventManager := TSimbaTargetEventManager.Create();
  FOptions := TSimbaTargetOptions.Create();

  SetDesktop();
end;

destructor TSimbaTarget.Destroy;
begin
  ChangeTarget(ESimbaTargetKind.NONE);
  if (FEventManager <> nil) then
    FreeAndNil(FEventManager);
  if (FOptions <> nil) then
    FreeAndNil(FOptions);

  inherited Destroy();
end;

function TSimbaTarget.FindEdges(MinDiff: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; ABounds: TBox): TPointArray;
begin
  Result := FindEdgesOnTarget(Self, ABounds, MinDiff, ColorSpace, Multipliers);
end;

function TSimbaTarget.FindEdges(MinDiff: Single; ABounds: TBox): TPointArray;
begin
  Result := FindEdgesOnTarget(Self, ABounds, MinDiff, DefaultColorSpace, DefaultMultipliers);
end;

procedure TSimbaTarget.CheckMethod(Method: Pointer; AName: String);
begin
  if (Method = nil) then
    SimbaException('Target "%s" cannot %s', [TargetName[FTargetKind], AName]);
end;

procedure TSimbaTarget.CheckResize(NewSize: TSize);
begin
  if (FLastSize = NewSize) then
    Exit;
  FLastSize := NewSize;

  FEventManager.CallResizeEvent(Self, NewSize.Width, NewSize.Height);
end;

function TSimbaTarget.GetMouseX: Integer;
begin
  Result := MouseXY.X;
end;

function TSimbaTarget.GetMouseXY: TPoint;
begin
  CheckMethod(FTargetMethods.MousePosition, 'MousePosition');

  Result := FTargetMethods.MousePosition(FTarget);
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

function TSimbaTarget.AddEvent(Event: ETargetEvent; Method: TSimbaTargetEvent; UserData: Pointer; UserDataSize: Integer): Integer;
begin
  Result := FEventManager.Add(Event, Method, UserData, UserDataSize);
end;

procedure TSimbaTarget.RemoveEvent(Event: ETargetEvent; Index: Integer);
begin
  FEventManager.Remove(Event, Index);
end;

procedure TSimbaTarget.ChangeTarget(Kind: ESimbaTargetKind);
begin
  if (FTargetKind = ESimbaTargetKind.IMAGE) and (FTargetImage <> nil) then
    FreeAndNil(FTargetImage);

  FTargetMethods := Default(TSimbaTargetMethods);
  FTargetKind := Kind;
end;

procedure TSimbaTarget.TargetChanged;
begin
  FEventManager.CallTargetChange(Self);
end;

procedure TSimbaTarget.CheckInvalidTarget;
var
  Attempt: Integer;
begin
  if IsValid() then
    Exit;

  if FEventManager.Has(ETargetEvent.TARGET_INVALID) then
  begin
    for Attempt := 1 to 5 do
    begin
      Sleep(100);

      FEventManager.CallTargetInvalid(Self);
      if IsValid() then
        Exit;
    end;
  end;

  SimbaException('Target is invalid: %s', [ToString()]);
end;

procedure TSimbaTarget.CheckAutoFocus;
begin
  if FOptions.ForceFocus and (not IsFocused()) then
    Focus();
end;

function TSimbaTarget.IsValid: Boolean;
begin
  CheckMethod(FTargetMethods.IsValid, 'IsValid');

  Result := FTargetMethods.IsValid(FTarget);
end;

function TSimbaTarget.IsFocused: Boolean;
begin
  CheckMethod(FTargetMethods.IsFocused, 'IsFocused');

  Result := FTargetMethods.IsFocused(FTarget);
end;

function TSimbaTarget.Focus: Boolean;
begin
  CheckMethod(FTargetMethods.Focus, 'Focus');

  Result := FTargetMethods.Focus(FTarget);
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
  CheckMethod(FTargetMethods.GetDimensions, 'GetDimensions');
  CheckInvalidTarget();

  FTargetMethods.GetDimensions(FTarget, Result.cx, Result.cy);
  CheckResize(Result);
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

function TSimbaTarget.GetImage: TSimbaImage;
begin
  Result := GetImage(TBox.Create(-1,-1,-1,-1));
end;

procedure TSimbaTarget.SetDesktop;
begin
  SetWindow(SimbaNativeInterface.GetDesktopWindow());
end;

procedure TSimbaTarget.SetWindow(Window: TWindowHandle);
begin
  ChangeTarget(ESimbaTargetKind.WINDOW);

  FTargetWindow := Window;
  FTarget := @FTargetWindow;

  FTargetMethods.Focus := @WindowTarget_Focus;
  FTargetMethods.IsFocused := @WindowTarget_IsFocused;
  FTargetMethods.IsValid := @WindowTarget_IsValid;

  FTargetMethods.KeyDown := @WindowTarget_KeyDown;
  FTargetMethods.KeyUp := @WindowTarget_KeyUp;
  FTargetMethods.KeySend := @WindowTarget_KeySend;
  FTargetMethods.KeyPressed := @WindowTarget_KeyPressed;

  FTargetMethods.MouseTeleport := @WindowTarget_MouseTeleport;
  FTargetMethods.MousePosition := @WindowTarget_MousePosition;
  FTargetMethods.MousePressed := @WindowTarget_MousePressed;
  FTargetMethods.MouseDown := @WindowTarget_MouseDown;
  FTargetMethods.MouseUp := @WindowTarget_MouseUp;
  FTargetMethods.MouseScroll := @WindowTarget_MouseScroll;

  FTargetMethods.GetDimensions := @WindowTarget_GetDimensions;
  FTargetMethods.GetImageData := @WindowTarget_GetImageData;

  TargetChanged();
end;

procedure TSimbaTarget.SetImage(Image: TSimbaImage);
begin
  ChangeTarget(ESimbaTargetKind.IMAGE);

  FTargetImage := Image;
  FTarget := FTargetImage;

  FTargetMethods.GetDimensions := @ImageTarget_GetDimensions;
  FTargetMethods.GetImageData := @ImageTarget_GetImageData;
  FTargetMethods.IsValid := @ImageTarget_IsValid;

  TargetChanged();
end;

procedure TSimbaTarget.SetEIOS(FileName, Args: String);
begin
  ChangeTarget(ESimbaTargetKind.EIOS);

  FTargetEIOS := LoadEIOS(FileName, Args);
  FTarget := @FTargetEIOS;

  FTargetMethods.KeyDown := @EIOSTarget_KeyDown;
  FTargetMethods.KeyUp := @EIOSTarget_KeyUp;
  FTargetMethods.KeySend := @EIOSTarget_KeySend;
  FTargetMethods.KeyPressed := @EIOSTarget_KeyPressed;

  FTargetMethods.MouseTeleport := @EIOSTarget_MouseTeleport;
  FTargetMethods.MousePosition := @EIOSTarget_MousePosition;
  FTargetMethods.MousePressed := @EIOSTarget_MousePressed;
  FTargetMethods.MouseDown := @EIOSTarget_MouseDown;
  FTargetMethods.MouseUp := @EIOSTarget_MouseUp;
  FTargetMethods.MouseScroll := @EIOSTarget_MouseScroll;

  FTargetMethods.GetDimensions := @EIOSTarget_GetDimensions;
  FTargetMethods.GetImageData := @EIOSTarget_GetImageData;

  FTargetMethods.IsValid := @EIOSTarget_IsValid;

  TargetChanged();
end;

procedure TSimbaTarget.SetPlugin(FileName, Args: String);
begin
  ChangeTarget(ESimbaTargetKind.PLUGIN);

  FTargetPlugin := LoadPluginTarget(FileName, Args);
  FTarget := @FTargetPlugin;

  FTargetMethods.KeyDown := @PluginTarget_KeyDown;
  FTargetMethods.KeyUp := @PluginTarget_KeyUp;
  FTargetMethods.KeySend := @PluginTarget_KeySend;
  FTargetMethods.KeyPressed := @PluginTarget_KeyPressed;

  FTargetMethods.MouseTeleport := @PluginTarget_MouseTeleport;
  FTargetMethods.MousePosition := @PluginTarget_MousePosition;
  FTargetMethods.MousePressed := @PluginTarget_MousePressed;
  FTargetMethods.MouseDown := @PluginTarget_MouseDown;
  FTargetMethods.MouseUp := @PluginTarget_MouseUp;
  FTargetMethods.MouseScroll := @PluginTarget_MouseScroll;

  FTargetMethods.GetDimensions := @PluginTarget_GetDimensions;
  FTargetMethods.GetImageData := @PluginTarget_GetImageData;

  FTargetMethods.IsValid := @PluginTarget_IsValid;

  TargetChanged();
end;

procedure TSimbaTarget.SetPlugin(FileName, Args: String; out DebugImage: TSimbaExternalCanvas);
begin
  ChangeTarget(ESimbaTargetKind.PLUGIN);

  FTargetPlugin := LoadPluginTarget(FileName, Args, DebugImage);
  FTarget := @FTargetPlugin;

  FTargetMethods.KeyDown := @PluginTarget_KeyDown;
  FTargetMethods.KeyUp := @PluginTarget_KeyUp;
  FTargetMethods.KeySend := @PluginTarget_KeySend;
  FTargetMethods.KeyPressed := @PluginTarget_KeyPressed;

  FTargetMethods.MouseTeleport := @PluginTarget_MouseTeleport;
  FTargetMethods.MousePosition := @PluginTarget_MousePosition;
  FTargetMethods.MousePressed := @PluginTarget_MousePressed;
  FTargetMethods.MouseDown := @PluginTarget_MouseDown;
  FTargetMethods.MouseUp := @PluginTarget_MouseUp;
  FTargetMethods.MouseScroll := @PluginTarget_MouseScroll;

  FTargetMethods.GetDimensions := @PluginTarget_GetDimensions;
  FTargetMethods.GetImageData := @PluginTarget_GetImageData;

  FTargetMethods.IsValid := @PluginTarget_IsValid;

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

function TSimbaTarget.GetImageData(var ABounds: TBox; out Data: PColorBGRA; out DataWidth: Integer): Boolean;

  function GetFrozenData: Boolean;
  begin
    // constrain to our frozen bounds
     if (ABounds.X1 = -1) and (ABounds.Y1 = -1) and (ABounds.X2 = -1) and (ABounds.Y2 = -1) then
       ABounds := FFrozenImage.Bounds
     else
       ABounds := ABounds.Clip(FFrozenImage.Bounds);

    Result := (ABounds.Width > 0) and (ABounds.Height > 0);
    if Result then
    begin
      Data := @FFrozenImage.Data[(ABounds.Y1 - FFrozenImage.Bounds.Y1) * FFrozenImage.DataWidth + (ABounds.X1 - FFrozenImage.Bounds.X1)];
      DataWidth := FFrozenImage.DataWidth;
    end;
  end;

begin
  Data := nil;
  DataWidth := 0;

  CheckMethod(FTargetMethods.GetImageData, 'GetImageData');
  if IsImageFrozen() then
    Result := GetFrozenData()
  else
    Result := ValidateBounds(ABounds) and FTargetMethods.GetImageData(FTarget, ABounds.X1, ABounds.Y1, ABounds.Width, ABounds.Height, Data, DataWidth);
end;

procedure TSimbaTarget.FreeImageData(var Data: PColorBGRA);
begin
  // never free frozen data
  if IsImageFrozen() and (Data = @FFrozenImage.Data[0]) then
    Exit;
  // Only free window since rest of targets are references to buffers or image data.
  if (FTargetKind in [ESimbaTargetKind.WINDOW]) then
    FreeMem(Data);
end;

function TSimbaTarget.IsImageFrozen: Boolean;
begin
  Result := Length(FFrozenImage.Data) > 0;
end;

procedure TSimbaTarget.FreezeImage(ABounds: TBox);
var
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  if GetImageData(ABounds, Data, DataWidth) then
  try
    FFrozenImage.Bounds := ABounds;
    FFrozenImage.DataWidth := DataWidth;

    SetLength(FFrozenImage.Data, DataWidth * ABounds.Height);
    Move(Data^, FFrozenImage.Data[0], Length(FFrozenImage.Data) * SizeOf(TColorBGRA));
  finally
    FreeImageData(Data);
  end;
end;

procedure TSimbaTarget.UnFreezeImage;
begin
  FFrozenImage.Data := [];
end;

function TSimbaTarget.ToString: String;
begin
  Result := Format('%P - ', [Pointer(Self)]);

  case FTargetKind of
    ESimbaTargetKind.IMAGE:
      if (FTargetImage = nil) then
        Result += 'IMAGE: TImage(nil)'
      else
        Result += 'IMAGE: TImage(%P), Size=%dx%d'.Format([Pointer(FTargetImage), FTargetImage.Width, FTargetImage.Height]);
    ESimbaTargetKind.WINDOW:
      Result += 'WINDOW: Handle=%d, Valid: %s'.Format([FTargetWindow, IsValid().ToString]);
    ESimbaTargetKind.EIOS:
      Result += 'EIOS: File="%s" Target=%P'.Format([FTargetEIOS.FileName, FTargetEIOS.Target]);
    ESimbaTargetKind.PLUGIN:
      Result += 'PLUGIN: File="%s" Target=%P'.Format([FTargetPlugin.FileName, FTargetPlugin.Target]);
    else
      Result += TargetName[FTargetKind];
  end;
end;

end.
