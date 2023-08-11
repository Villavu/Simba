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
  simba.mufasatypes, simba.bitmap,
  simba.target_eios, simba.target_window, simba.target_bitmap;

type
  {$PUSH}
  {$SCOPEDENUMS ON}
  ETargetType = (NONE, IMAGE, WINDOW, EIOS);
  ETargetTypes = set of ETargetType;
  {$POP}

  TTargetMethods = record
    GetDimensions: procedure(Target: Pointer; out W, H: Integer);
    GetImageData: function(Target: Pointer; X, Y, Width, Height: Integer; var Data: PColorBGRA; var DataWidth: Integer): Boolean;

    IsValid: function(Target: Pointer): Boolean;
    IsFocused: function(Target: Pointer): Boolean;
    Focus: function(Target: Pointer): Boolean;

    KeyDown: procedure(Target: Pointer; Key: KeyCode);
    KeyUp: procedure(Target: Pointer; Key: KeyCode);
    KeySend: procedure(Target: Pointer; Key: Char; KeyDownTime, KeyUpTime, ModifierDownTime, ModifierUpTime: Integer);
    KeyPressed: function(Target: Pointer; Key: KeyCode): Boolean;

    MouseTeleport: procedure(Target: Pointer; P: TPoint);
    MousePosition: function(Target: Pointer): TPoint;
    MousePressed: function(Target: Pointer; Button: MouseButton): Boolean;
    MouseDown: procedure(Target: Pointer; Button: MouseButton);
    MouseUp: procedure(Target: Pointer; Button: MouseButton);
    MouseScroll: procedure(Target: Pointer; Scrolls: Integer);
  end;

const
  TargetName: array[ETargetType] of String = ('NONE', 'IMAGE', 'WINDOW', 'EIOS');

type
  PSimbaTarget = ^TSimbaTarget;
  TSimbaTarget = packed record
  public
  type
    PInvalidTargetEvent = ^TInvalidTargetEvent;
    TInvalidTargetEvent = procedure(var Target: TSimbaTarget) of object;
  private
    FTargetType: ETargetType;
    FTarget: Pointer;
    FTargetImage: TSimbaImage;
    FTargetWindow: TWindowHandle;
    FTargetEIOS: TEIOSTarget;
    FMethods: TTargetMethods; // Targets need to provide these. They are filled in SetWindow,SetEIOS etc.
    FCustomClientArea: TBox;
    FAutoSetFocus: Boolean;
    FInvalidTargetHandlers: array[0..9] of TInvalidTargetEvent;

    procedure ChangeTarget(TargetType: ETargetType);
    function HasMethod(Method: Pointer; Name: String): Boolean;

    procedure CheckInvalidTarget;
    procedure CheckAutoFocus;

    procedure SetCustomClientArea(B: TBox);
    function GetCustomClientArea: TBox;
    procedure SetAutoSetFocus(Value: Boolean);
  public
    function GetWindowTarget: TWindowHandle;
    function IsWindowTarget: Boolean; overload;
    function IsWindowTarget(out Window: TWindowHandle): Boolean; overload;
    function IsImageTarget: Boolean; overload;
    function IsImageTarget(out Image: TSimbaImage): Boolean; overload;
    function IsEIOSTarget: Boolean;

    function IsValid: Boolean;
    function IsFocused: Boolean;
    function Focus: Boolean;

    procedure GetDimensions(out W, H: Integer);
    function GetWidth: Integer;
    function GetHeight: Integer;

    function GetImage(Bounds: TBox): TSimbaImage;

    procedure SetDesktop;
    procedure SetWindow(Window: TWindowHandle);
    procedure SetImage(Image: TSimbaImage);
    procedure SetEIOS(FileName, Args: String);

    function MousePressed(Button: MouseButton): Boolean;
    function MousePosition: TPoint;
    procedure MouseTeleport(P: TPoint);
    procedure MouseUp(Button: MouseButton);
    procedure MouseDown(Button: MouseButton);
    procedure MouseScroll(Scrolls: Integer);

    procedure KeyDown(Key: KeyCode);
    procedure KeyUp(Key: KeyCode);
    procedure KeySend(Key: Char; KeyDownTime, KeyUpTime, ModifierDownTime, ModifierUpTime: Integer);
    function KeyPressed(Key: KeyCode): Boolean;

    function ValidateBounds(var Bounds: TBox): Boolean;
    function GetImageData(var Bounds: TBox; var Data: PColorBGRA; var DataWidth: Integer): Boolean;
    procedure FreeImageData(var Data: PColorBGRA);

    function AddHandlerOnInvalidTarget(Event: TInvalidTargetEvent): TInvalidTargetEvent;
    procedure RemoveHandlerOnInvalidTarget(Event: TInvalidTargetEvent);

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

procedure TSimbaTarget.CheckInvalidTarget;

  function HasInvalidTargetHandler: Boolean;
  var
    I: Integer;
  begin
    Result := False;

    for I := 0 to High(FInvalidTargetHandlers) do
      if Assigned(FInvalidTargetHandlers[I]) then
        Exit(True);
  end;

  function CallInvalidTargetHandler: Boolean;
  var
    I: Integer;
  begin
    Result := False;

    for I := 0 to High(FInvalidTargetHandlers) do
      if Assigned(FInvalidTargetHandlers[I]) then
      begin
        FInvalidTargetHandlers[I](Self);
        if IsValid() then
          Exit(True);
      end;
  end;

var
  Attempt: Integer;
begin
  if IsValid() then
    Exit;

  if HasInvalidTargetHandler() then
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

procedure TSimbaTarget.GetDimensions(out W, H: Integer);
begin
  CheckInvalidTarget();

  if HasMethod(FMethods.GetDimensions, 'GetDimensions') then
    FMethods.GetDimensions(FTarget, W, H);
end;

function TSimbaTarget.GetWidth: Integer;
var
  _: Integer;
begin
  GetDimensions(Result, _);
end;

function TSimbaTarget.GetHeight: Integer;
var
  _: Integer;
begin
  GetDimensions(_, Result);
end;

function TSimbaTarget.GetImage(Bounds: TBox): TSimbaImage;
var
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  if GetImageData(Bounds, Data, DataWidth) then
    Result := TSimbaImage.CreateFromData(Bounds.Width, Bounds.Height, Data, DataWidth)
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
end;

function TSimbaTarget.MousePressed(Button: MouseButton): Boolean;
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

procedure TSimbaTarget.MouseUp(Button: MouseButton);
begin
  CheckAutoFocus();

  if HasMethod(FMethods.MouseUp, 'MouseUp') then
    FMethods.MouseUp(FTarget, Button);
end;

procedure TSimbaTarget.MouseDown(Button: MouseButton);
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

procedure TSimbaTarget.KeyDown(Key: KeyCode);
begin
  CheckAutoFocus();

  if HasMethod(FMethods.KeyDown, 'KeyDown') then
    FMethods.KeyDown(FTarget, Key);
end;

procedure TSimbaTarget.KeyUp(Key: KeyCode);
begin
  CheckAutoFocus();

  if HasMethod(FMethods.KeyDown, 'KeyUp') then
    FMethods.KeyDown(FTarget, Key);
end;

procedure TSimbaTarget.KeySend(Key: Char; KeyDownTime, KeyUpTime, ModifierDownTime, ModifierUpTime: Integer);
begin
  CheckAutoFocus();

  if HasMethod(FMethods.KeySend, 'KeySend') then
    FMethods.KeySend(FTarget, Key, KeyDownTime, KeyUpTime, ModifierDownTime, ModifierUpTime);
end;

function TSimbaTarget.KeyPressed(Key: KeyCode): Boolean;
begin
  if HasMethod(FMethods.KeyPressed, 'KeyPressed') then
    Result := FMethods.KeyPressed(FTarget, Key);
end;

function TSimbaTarget.ValidateBounds(var Bounds: TBox): Boolean;

  procedure ValidateBoundsInCustomClientArea;
  begin
    if (Bounds.X1 = -1) and (Bounds.Y1 = -1) and (Bounds.X2 = -1) and (Bounds.Y2 = -1) then
      Bounds := FCustomClientArea
    else
    begin
      Bounds := Bounds.Offset(FCustomClientArea.TopLeft);

      if (Bounds.X1 < FCustomClientArea.X1)  then Bounds.X1 := FCustomClientArea.X1;
      if (Bounds.Y1 < FCustomClientArea.Y1)  then Bounds.Y1 := FCustomClientArea.Y1;
      if (Bounds.X2 >= FCustomClientArea.X2) then Bounds.X2 := FCustomClientArea.X2 - 1;
      if (Bounds.Y2 >= FCustomClientArea.Y2) then Bounds.Y2 := FCustomClientArea.Y2 - 1;
    end;
  end;

var
  Width, Height: Integer;
begin
  GetDimensions(Width, Height);

  if FCustomClientArea.IsDefault() then
    if (Bounds.X1 = -1) and (Bounds.Y1 = -1) and (Bounds.X2 = -1) and (Bounds.Y2 = -1) then
    begin
      Bounds.X1 := 0;
      Bounds.Y1 := 0;
      Bounds.X2 := Width - 1;
      Bounds.Y2 := Height - 1;
    end else
    begin
      if (Bounds.X1 < 0)       then Bounds.X1 := 0;
      if (Bounds.Y1 < 0)       then Bounds.Y1 := 0;
      if (Bounds.X2 >= Width)  then Bounds.X2 := Width - 1;
      if (Bounds.Y2 >= Height) then Bounds.Y2 := Height - 1;
    end
  else
    ValidateBoundsInCustomClientArea();

  Result := (Bounds.Width > 0) and (Bounds.Height > 0);
end;

function TSimbaTarget.GetImageData(var Bounds: TBox; var Data: PColorBGRA; var DataWidth: Integer): Boolean;
begin
  Data := nil;
  if HasMethod(FMethods.GetImageData, 'GetImageData') then
    Result := ValidateBounds(Bounds) and FMethods.GetImageData(FTarget, Bounds.X1, Bounds.Y1, Bounds.Width, Bounds.Height, Data, DataWidth);
end;

procedure TSimbaTarget.FreeImageData(var Data: PColorBGRA);
begin
  if (FTargetType in [ETargetType.WINDOW]) then
    FreeMem(Data);
end;

function TSimbaTarget.AddHandlerOnInvalidTarget(Event: TInvalidTargetEvent): TInvalidTargetEvent;
var
  I: Integer;
begin
  Result := Event;
  for I := 0 to High(FInvalidTargetHandlers) do
    if (FInvalidTargetHandlers[I] = Event) then
      Exit;

  for I := 0 to High(FInvalidTargetHandlers) do
    if (FInvalidTargetHandlers[I] = nil) then
    begin
      FInvalidTargetHandlers[I] := Event;
      Exit;
    end;
end;

procedure TSimbaTarget.RemoveHandlerOnInvalidTarget(Event: TInvalidTargetEvent);
var
  I: Integer;
begin
  for I := 0 to High(FInvalidTargetHandlers) do
    if (FInvalidTargetHandlers[I] = Event) then
      FInvalidTargetHandlers[I] := nil;
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
