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
  simba.base, simba.image, simba.externalimage,
  simba.target_eios, simba.target_window, simba.target_image, simba.target_plugin;

type
  {$PUSH}
  {$SCOPEDENUMS ON}
  ETargetType = (NONE, IMAGE, WINDOW, EIOS, PLUGIN);
  ETargetTypes = set of ETargetType;
  {$POP}

  TTargetMethods = record
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

const
  TargetName: array[ETargetType] of String = ('NONE', 'IMAGE', 'WINDOW', 'EIOS', 'PLUGIN');

type
  PSimbaTarget = ^TSimbaTarget;
  TSimbaTarget = packed record
  public
  type
    TInvalidTargetEvent = procedure(var Target: TSimbaTarget) of object;
    TInvalidTargetEventArray = array of TInvalidTargetEvent;
  private
    FTargetType: ETargetType;
    FTarget: Pointer;
    FTargetImage: TSimbaImage;
    FTargetWindow: TWindowHandle;
    FTargetEIOS: TEIOSTarget;
    FTargetPlugin: TSimbaPluginTarget;
    FMethods: TTargetMethods; // Targets need to provide these. They are filled in SetWindow,SetEIOS etc.
    FCustomClientArea: TBox;
    FAutoSetFocus: Boolean;
    FFrozenDataWidth: Integer;
    FFrozenBounds: TBox;
    FInvalidTargetEvents: TInvalidTargetEventArray;
    FFrozenData: array of TColorBGRA;

    procedure ChangeTarget(TargetType: ETargetType);
    function HasMethod(Method: Pointer; Name: String): Boolean;
    function CallInvalidTargetHandler: Boolean;

    procedure CheckInvalidTarget;
    procedure CheckAutoFocus;

    procedure SetCustomClientArea(B: TBox);
    function GetCustomClientArea: TBox;
    procedure SetAutoSetFocus(Value: Boolean);

    procedure GetDimensions(out W, H: Integer);
  public
    function GetWindowTarget: TWindowHandle;
    function IsWindowTarget: Boolean; overload;
    function IsWindowTarget(out Window: TWindowHandle): Boolean; overload;
    function IsImageTarget: Boolean; overload;
    function IsImageTarget(out Image: TSimbaImage): Boolean; overload;
    function IsEIOSTarget: Boolean;
    function IsPluginTarget: Boolean;

    function IsValid: Boolean;
    function IsFocused: Boolean;
    function Focus: Boolean;

    function Bounds: TBox;
    function Width: Integer;
    function Height: Integer;

    function GetImage(ABounds: TBox): TSimbaImage;

    procedure SetDesktop;
    procedure SetWindow(Window: TWindowHandle);
    procedure SetImage(Image: TSimbaImage);
    procedure SetEIOS(FileName, Args: String);

    procedure SetPlugin(FileName, Args: String); overload;
    procedure SetPlugin(FileName, Args: String; out DebugImage: TSimbaExternalImage); overload;

    function MousePressed(Button: EMouseButton): Boolean;
    function MousePosition: TPoint;
    procedure MouseTeleport(P: TPoint);
    procedure MouseUp(Button: EMouseButton);
    procedure MouseDown(Button: EMouseButton);
    procedure MouseScroll(Scrolls: Integer);

    procedure KeyDown(Key: EKeyCode);
    procedure KeyUp(Key: EKeyCode);
    procedure KeySend(Text: String; SleepTimes: PInt32);
    function KeyPressed(Key: EKeyCode): Boolean;

    function ValidateBounds(var ABounds: TBox): Boolean;
    function GetImageData(var ABounds: TBox; var Data: PColorBGRA; var DataWidth: Integer): Boolean;
    procedure FreeImageData(var Data: PColorBGRA);

    function IsImageFrozen: Boolean; inline;
    procedure FreezeImage(ABounds: TBox);
    procedure UnFreezeImage;

    function AddOnInvalidTargetEvent(Event: TInvalidTargetEvent): TInvalidTargetEvent;
    procedure RemoveOnInvalidTargetEvent(Event: TInvalidTargetEvent);

    procedure ClearCustomClientArea;

    property CustomClientArea: TBox read GetCustomClientArea write SetCustomClientArea;
    property AutoSetFocus: Boolean read FAutoSetFocus write SetAutoSetFocus;

    function ToString: String;

    class operator Initialize(var Self: TSimbaTarget);
  end;

implementation

uses
  simba.nativeinterface;

procedure TSimbaTarget.SetAutoSetFocus(Value: Boolean);
begin
  FAutoSetFocus := Value;
end;

procedure TSimbaTarget.ChangeTarget(TargetType: ETargetType);
begin
  FTargetType := TargetType;
  FMethods := Default(TTargetMethods);
end;

function TSimbaTarget.HasMethod(Method: Pointer; Name: String): Boolean;
begin
  if (Method = nil) then
    SimbaException('Target "%s" cannot %s', [TargetName[FTargetType], Name]);

  Result := True;
end;

function TSimbaTarget.CallInvalidTargetHandler: Boolean;
var
  I: Integer;
begin
  for I := 0 to High(FInvalidTargetEvents) do
  begin
    FInvalidTargetEvents[I](Self);

    if IsValid() then
    begin
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

procedure TSimbaTarget.CheckInvalidTarget;
var
  Attempt: Integer;
begin
  if IsValid() then
    Exit;

  if (Length(FInvalidTargetEvents) > 0) then
  begin
    for Attempt := 1 to 5 do
    begin
      Sleep(100);
      if CallInvalidTargetHandler() then
        Exit;
    end;
  end;

  SimbaException('Target is invalid: %s', [ToString()]);
end;

procedure TSimbaTarget.CheckAutoFocus;
begin
  if (not FAutoSetFocus) or IsFocused() then
    Exit;

  Focus();
end;

function TSimbaTarget.GetWindowTarget: TWindowHandle;
begin
  if (FTargetType = ETargetType.WINDOW) then
    Result := FTargetWindow
  else
    Result := 0;
end;

function TSimbaTarget.IsWindowTarget: Boolean;
begin
  Result := FTargetType = ETargetType.WINDOW;
end;

function TSimbaTarget.IsWindowTarget(out Window: TWindowHandle): Boolean;
begin
  Result := FTargetType = ETargetType.WINDOW;
  if Result then
    Window := FTargetWindow;
end;

function TSimbaTarget.IsImageTarget: Boolean;
begin
  Result := FTargetType = ETargetType.IMAGE;
end;

function TSimbaTarget.IsImageTarget(out Image: TSimbaImage): Boolean;
begin
  Result := FTargetType = ETargetType.IMAGE;
  if Result then
    Image := FTargetImage;
end;

function TSimbaTarget.IsEIOSTarget: Boolean;
begin
  Result := FTargetType = ETargetType.EIOS;
end;

function TSimbaTarget.IsPluginTarget: Boolean;
begin
  Result := FTargetType = ETargetType.PLUGIN;
end;

function TSimbaTarget.IsValid: Boolean;
begin
  if HasMethod(FMethods.IsValid, 'IsValid') then
    Result := FMethods.IsValid(FTarget);
end;

function TSimbaTarget.IsFocused: Boolean;
begin
  if HasMethod(FMethods.IsFocused, 'IsFocused') then
    Result := FMethods.IsFocused(FTarget);
end;

function TSimbaTarget.Focus: Boolean;
begin
  if HasMethod(FMethods.Focus, 'Focus') then
    Result := FMethods.Focus(FTarget);
end;

function TSimbaTarget.Bounds: TBox;
var
  W, H: Integer;
begin
  GetDimensions(W, H);

  Result.X1 := 0;
  Result.Y1 := 0;
  Result.X2 := W-1;
  Result.Y2 := H-1;
end;

function TSimbaTarget.Width: Integer;
var
  _: Integer;
begin
  GetDimensions(Result, _);
end;

function TSimbaTarget.Height: Integer;
var
  _: Integer;
begin
  GetDimensions(_, Result);
end;

procedure TSimbaTarget.GetDimensions(out W, H: Integer);
begin
  CheckInvalidTarget();

  if HasMethod(FMethods.GetDimensions, 'GetDimensions') then
    FMethods.GetDimensions(FTarget, W, H);
end;

function TSimbaTarget.GetImage(ABounds: TBox): TSimbaImage;
var
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  if GetImageData(ABounds, Data, DataWidth) then
  try
    Result := TSimbaImage.CreateFromData(ABounds.Width, ABounds.Height, Data, DataWidth);
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
  ChangeTarget(ETargetType.WINDOW);

  FTargetWindow := Window;
  FTarget := @FTargetWindow;

  FMethods.Focus := @WindowTarget_Focus;
  FMethods.IsFocused := @WindowTarget_IsFocused;
  FMethods.IsValid := @WindowTarget_IsValid;

  FMethods.KeyDown := @WindowTarget_KeyDown;
  FMethods.KeyUp := @WindowTarget_KeyUp;
  FMethods.KeySend := @WindowTarget_KeySend;
  FMethods.KeyPressed := @WindowTarget_KeyPressed;

  FMethods.MouseTeleport := @WindowTarget_MouseTeleport;
  FMethods.MousePosition := @WindowTarget_MousePosition;
  FMethods.MousePressed := @WindowTarget_MousePressed;
  FMethods.MouseDown := @WindowTarget_MouseDown;
  FMethods.MouseUp := @WindowTarget_MouseUp;
  FMethods.MouseScroll := @WindowTarget_MouseScroll;

  FMethods.GetDimensions := @WindowTarget_GetDimensions;
  FMethods.GetImageData := @WindowTarget_GetImageData;
end;

procedure TSimbaTarget.SetImage(Image: TSimbaImage);
begin
  ChangeTarget(ETargetType.IMAGE);

  FTargetImage := Image;
  FTarget := FTargetImage;

  FMethods.GetDimensions := @ImageTarget_GetDimensions;
  FMethods.GetImageData := @ImageTarget_GetImageData;
  FMethods.IsValid := @ImageTarget_IsValid;
end;

procedure TSimbaTarget.SetEIOS(FileName, Args: String);
begin
  ChangeTarget(ETargetType.EIOS);

  FTargetEIOS := LoadEIOS(FileName, Args);
  FTarget := @FTargetEIOS;

  FMethods.KeyDown := @EIOSTarget_KeyDown;
  FMethods.KeyUp := @EIOSTarget_KeyUp;
  FMethods.KeySend := @EIOSTarget_KeySend;
  FMethods.KeyPressed := @EIOSTarget_KeyPressed;

  FMethods.MouseTeleport := @EIOSTarget_MouseTeleport;
  FMethods.MousePosition := @EIOSTarget_MousePosition;
  FMethods.MousePressed := @EIOSTarget_MousePressed;
  FMethods.MouseDown := @EIOSTarget_MouseDown;
  FMethods.MouseUp := @EIOSTarget_MouseUp;
  FMethods.MouseScroll := @EIOSTarget_MouseScroll;

  FMethods.GetDimensions := @EIOSTarget_GetDimensions;
  FMethods.GetImageData := @EIOSTarget_GetImageData;

  FMethods.IsValid := @EIOSTarget_IsValid;
end;

procedure TSimbaTarget.SetPlugin(FileName, Args: String);
begin
  ChangeTarget(ETargetType.PLUGIN);

  FTargetPlugin := LoadPluginTarget(FileName, Args);
  FTarget := @FTargetPlugin;

  FMethods.KeyDown := @PluginTarget_KeyDown;
  FMethods.KeyUp := @PluginTarget_KeyUp;
  FMethods.KeySend := @PluginTarget_KeySend;
  FMethods.KeyPressed := @PluginTarget_KeyPressed;

  FMethods.MouseTeleport := @PluginTarget_MouseTeleport;
  FMethods.MousePosition := @PluginTarget_MousePosition;
  FMethods.MousePressed := @PluginTarget_MousePressed;
  FMethods.MouseDown := @PluginTarget_MouseDown;
  FMethods.MouseUp := @PluginTarget_MouseUp;
  FMethods.MouseScroll := @PluginTarget_MouseScroll;

  FMethods.GetDimensions := @PluginTarget_GetDimensions;
  FMethods.GetImageData := @PluginTarget_GetImageData;

  FMethods.IsValid := @PluginTarget_IsValid;
end;

procedure TSimbaTarget.SetPlugin(FileName, Args: String; out DebugImage: TSimbaExternalImage);
begin
  ChangeTarget(ETargetType.PLUGIN);

  FTargetPlugin := LoadPluginTarget(FileName, Args, DebugImage);
  FTarget := @FTargetPlugin;

  FMethods.KeyDown := @PluginTarget_KeyDown;
  FMethods.KeyUp := @PluginTarget_KeyUp;
  FMethods.KeySend := @PluginTarget_KeySend;
  FMethods.KeyPressed := @PluginTarget_KeyPressed;

  FMethods.MouseTeleport := @PluginTarget_MouseTeleport;
  FMethods.MousePosition := @PluginTarget_MousePosition;
  FMethods.MousePressed := @PluginTarget_MousePressed;
  FMethods.MouseDown := @PluginTarget_MouseDown;
  FMethods.MouseUp := @PluginTarget_MouseUp;
  FMethods.MouseScroll := @PluginTarget_MouseScroll;

  FMethods.GetDimensions := @PluginTarget_GetDimensions;
  FMethods.GetImageData := @PluginTarget_GetImageData;

  FMethods.IsValid := @PluginTarget_IsValid;
end;

function TSimbaTarget.MousePressed(Button: EMouseButton): Boolean;
begin
  if HasMethod(FMethods.MousePressed, 'MousePressed') then
    Result := FMethods.MousePressed(FTarget, Button);
end;

function TSimbaTarget.MousePosition: TPoint;
begin
  if HasMethod(FMethods.MousePosition, 'MousePosition') then
    Result := FMethods.MousePosition(FTarget);

  Result.X -= FCustomClientArea.X1;
  Result.Y -= FCustomClientArea.Y1;
end;

procedure TSimbaTarget.MouseTeleport(P: TPoint);
begin
  CheckAutoFocus();

  P.X += FCustomClientArea.X1;
  P.Y += FCustomClientArea.Y1;

  if HasMethod(FMethods.MouseTeleport, 'MouseTeleport') then
    FMethods.MouseTeleport(FTarget, P);
end;

procedure TSimbaTarget.MouseUp(Button: EMouseButton);
begin
  CheckAutoFocus();

  if HasMethod(FMethods.MouseUp, 'MouseUp') then
    FMethods.MouseUp(FTarget, Button);
end;

procedure TSimbaTarget.MouseDown(Button: EMouseButton);
begin
  CheckAutoFocus();

  if HasMethod(FMethods.MouseDown, 'MouseDown') then
    FMethods.MouseDown(FTarget, Button);
end;

procedure TSimbaTarget.MouseScroll(Scrolls: Integer);
begin
  CheckAutoFocus();

  if HasMethod(FMethods.MouseScroll, 'MouseScroll') then
    FMethods.MouseScroll(FTarget, Scrolls);
end;

procedure TSimbaTarget.KeyDown(Key: EKeyCode);
begin
  CheckAutoFocus();

  if HasMethod(FMethods.KeyDown, 'KeyDown') then
    FMethods.KeyDown(FTarget, Key);
end;

procedure TSimbaTarget.KeyUp(Key: EKeyCode);
begin
  CheckAutoFocus();

  if HasMethod(FMethods.KeyUp, 'KeyUp') then
    FMethods.KeyUp(FTarget, Key);
end;

procedure TSimbaTarget.KeySend(Text: String; SleepTimes: PInt32);
begin
  if (Length(Text) = 0) then
    Exit;
  CheckAutoFocus();

  if HasMethod(FMethods.KeySend, 'KeySend') then
    FMethods.KeySend(FTarget, PChar(Text), Length(Text), SleepTimes);
end;

function TSimbaTarget.KeyPressed(Key: EKeyCode): Boolean;
begin
  if HasMethod(FMethods.KeyPressed, 'KeyPressed') then
    Result := FMethods.KeyPressed(FTarget, Key);
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
  GetDimensions(W, H);

  if FCustomClientArea.IsDefault() then
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

function TSimbaTarget.GetImageData(var ABounds: TBox; var Data: PColorBGRA; var DataWidth: Integer): Boolean;
begin
  if IsImageFrozen() then
  begin
    Data := @FFrozenData[0];
    DataWidth := FFrozenDataWidth;
    ABounds := FFrozenBounds;

    Exit(True);
  end;

  Data := nil;
  if HasMethod(FMethods.GetImageData, 'GetImageData') then
    Result := ValidateBounds(ABounds) and FMethods.GetImageData(FTarget, ABounds.X1, ABounds.Y1, ABounds.Width, ABounds.Height, Data, DataWidth);
end;

procedure TSimbaTarget.FreeImageData(var Data: PColorBGRA);
begin
  if IsImageFrozen() and (Data = @FFrozenData[0]) then
    Exit;
  if (FTargetType in [ETargetType.WINDOW]) then
    FreeMem(Data);
end;

function TSimbaTarget.IsImageFrozen: Boolean;
begin
  Result := Length(FFrozenData) > 0;
end;

procedure TSimbaTarget.FreezeImage(ABounds: TBox);
var
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  if GetImageData(ABounds, Data, DataWidth) then
  try
    FFrozenBounds := ABounds;
    FFrozenDataWidth := DataWidth;

    SetLength(FFrozenData, DataWidth * ABounds.Height);
    Move(Data^, FFrozenData[0], Length(FFrozenData) * SizeOf(TColorBGRA));
  finally
    FreeImageData(Data);
  end;
end;

procedure TSimbaTarget.UnFreezeImage;
begin
  FFrozenData := nil;
end;

function TSimbaTarget.AddOnInvalidTargetEvent(Event: TInvalidTargetEvent): TInvalidTargetEvent;
begin
  Result := Event;

  FInvalidTargetEvents += [Event];
end;

procedure TSimbaTarget.RemoveOnInvalidTargetEvent(Event: TInvalidTargetEvent);
var
  I: Integer;
begin
  for I := High(FInvalidTargetEvents) downto 0 do
    if (Event = FInvalidTargetEvents[I]) then
      Delete(FInvalidTargetEvents, I, 1);
end;

procedure TSimbaTarget.ClearCustomClientArea;
begin
  FCustomClientArea := TBox.Default;
end;

function TSimbaTarget.ToString: String;
begin
  Result := 'ETargetType.' + TargetName[FTargetType];

  case FTargetType of
    ETargetType.IMAGE:
      Result := 'ETargetType.IMAGE: TImage(%P), Size=%dx%d'.Format([Pointer(FTargetImage), FTargetImage.Width, FTargetImage.Height]);
    ETargetType.WINDOW:
      Result := 'ETargetType.WINDOW: Handle=%d, Valid: %s'.Format([FTargetWindow, BoolToStr(IsValid(), True)]);
    ETargetType.EIOS:
      Result := 'ETargetType.EIOS: Target=%P'.Format([FTargetEIOS.Target]);
    ETargetType.PLUGIN:
      Result := 'ETargetType.PLUGIN: Filename="%s" Target=%P'.Format([FTargetPlugin.FileName, FTargetPlugin.Target]);
  end;
end;

procedure TSimbaTarget.SetCustomClientArea(B: TBox);
begin
  FCustomClientArea := B;
end;

function TSimbaTarget.GetCustomClientArea: TBox;
begin
  Result := FCustomClientArea;
end;

class operator TSimbaTarget.Initialize(var Self: TSimbaTarget);
begin
  Self := Default(TSimbaTarget);
end;

end.

