{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.target;

{$i simba.inc}

// todo, autoactivate & addhandleroninvalidtarget & clientarea

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.bitmap,
  simba.target_eios, simba.target_window, simba.target_bitmap;

type
  {$PUSH}
  {$SCOPEDENUMS ON}
  ETargetType = (NONE, BITMAP, WINDOW, EIOS);
  ETargetTypes = set of ETargetType;
  {$POP}

  TTargetMethods = record
    GetDimensions: procedure(out W, H: Integer) of object;
    GetImageData: function(X, Y, Width, Height: Integer; var Data: PColorBGRA; var DataWidth: Integer): Boolean of object;

    IsValid: function: Boolean of object;
    IsFocused: function: Boolean of object;
    Focus: function: Boolean of object;

    KeyDown: procedure(Key: KeyCode) of object;
    KeyUp: procedure(Key: KeyCode) of object;
    KeySend: procedure(Key: Char; KeyDownTime, KeyUpTime, ModifierDownTime, ModifierUpTime: Integer) of object;
    KeyPressed: function(Key: KeyCode): Boolean of object;

    MouseTeleport: procedure(P: TPoint) of object;
    MousePosition: function: TPoint of object;
    MousePressed: function(Button: MouseButton): Boolean of object;
    MouseDown: procedure(Button: MouseButton) of object;
    MouseUp: procedure(Button: MouseButton) of object;
    MouseScroll: procedure(Scrolls: Integer) of object;
  end;

const
  TargetName: array[ETargetType] of String = ('NONE', 'BITMAP', 'WINDOW', 'EIOS');

type
  PSimbaTarget = ^TSimbaTarget;
  TSimbaTarget = record
  public
    FTargetType: ETargetType;
    FTargetBitmap: TSimbaBitmapTarget;
    FTargetWindow: TSimbaWindowTarget;
    FTargetEIOS: TEIOSTarget;
    FMethods: TTargetMethods; // Targets need to provide these. They are filled in SetWindow,SetEIOS etc.

    procedure ChangeTarget(TargetType: ETargetType);
    function HasMethod(Method: PMethod; Name: String): Boolean;
  public
    function IsValid: Boolean;
    function IsFocused: Boolean;
    function Focus: Boolean;

    procedure GetDimensions(out W, H: Integer);
    function GetWidth: Integer;
    function GetHeight: Integer;

    function GetImage(Bounds: TBox): TMufasaBitmap;

    procedure SetDesktop;
    procedure SetWindow(Window: TWindowHandle);
    procedure SetBitmap(Bitmap: TMufasaBitmap);
    procedure SetEIOS(FileName, Args: String);

    function MousePressed(Button: MouseButton): Boolean;
    function MousePosition: TPoint;
    procedure MouseTeleport(P: TPoint);
    procedure MouseUp(Button: MouseButton);
    procedure MouseDown(Button: MouseButton);
    procedure MouseClick(Button: MouseButton; ClickPressMin, ClickPressMax: Integer);
    procedure MouseScroll(Scrolls: Integer);

    procedure KeyDown(Key: KeyCode);
    procedure KeyUp(Key: KeyCode);
    procedure KeyPress(Key: KeyCode; KeyPressMin, KeyPressMax: Integer);
    procedure KeySend(Text: String; KeyPressMin, KeyPressMax: Integer);
    function KeyPressed(Key: KeyCode): Boolean;

    function ValidateBounds(var Bounds: TBox): Boolean;
    function GetImageData(var Bounds: TBox; var Data: PColorBGRA; var DataWidth: Integer): Boolean;
    procedure FreeImageData(var Data: PColorBGRA);
  end;

implementation

uses
  simba.nativeinterface, simba.random;

procedure TSimbaTarget.ChangeTarget(TargetType: ETargetType);
begin
  FTargetType := TargetType;
  FMethods := Default(TTargetMethods);
end;

function TSimbaTarget.HasMethod(Method: PMethod; Name: String): Boolean;
begin
  if (Method^.Code = nil) then
    raise Exception.CreateFmt('Target "%s" cannot %s', [TargetName[FTargetType], Name]);

  Result := True;
end;

function TSimbaTarget.IsValid: Boolean;
begin
  if HasMethod(@FMethods.IsValid, 'IsValid') then
    Result := FMethods.IsValid();
end;

function TSimbaTarget.IsFocused: Boolean;
begin
  if HasMethod(@FMethods.IsFocused, 'IsFocused') then
    Result := FMethods.IsFocused();
end;

function TSimbaTarget.Focus: Boolean;
begin
  if HasMethod(@FMethods.Focus, 'Focus') then
    Result := FMethods.Focus();
end;

procedure TSimbaTarget.GetDimensions(out W, H: Integer);
begin
  if HasMethod(@FMethods.GetDimensions, 'GetDimensions') then
    FMethods.GetDimensions(W, H);
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

function TSimbaTarget.GetImage(Bounds: TBox): TMufasaBitmap;
var
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  if GetImageData(Bounds, Data, DataWidth) then
    Result := TMufasaBitmap.CreateFromData(Bounds.Width, Bounds.Height, Data, DataWidth)
  else
    Result := TMufasaBitmap.Create();
end;

procedure TSimbaTarget.SetDesktop;
begin
  SetWindow(SimbaNativeInterface.GetDesktopWindow());
end;

procedure TSimbaTarget.SetWindow(Window: TWindowHandle);
begin
  ChangeTarget(ETargetType.WINDOW);

  FTargetWindow.Handle := Window;

  FMethods.Focus := @FTargetWindow.Focus;
  FMethods.IsFocused := @FTargetWindow.IsFocused;
  FMethods.IsValid := @FTargetWindow.IsValid;

  FMethods.KeyDown := @FTargetWindow.KeyDown;
  FMethods.KeyUp := @FTargetWindow.KeyUp;
  FMethods.KeySend := @FTargetWindow.KeySend;
  FMethods.KeyPressed := @FTargetWindow.KeyPressed;

  FMethods.MouseTeleport := @FTargetWindow.MouseTeleport;
  FMethods.MousePosition := @FTargetWindow.MousePosition;
  FMethods.MousePressed := @FTargetWindow.MousePressed;
  FMethods.MouseDown := @FTargetWindow.MouseDown;
  FMethods.MouseUp := @FTargetWindow.MouseUp;
  FMethods.MouseScroll := @FTargetWindow.MouseScroll;

  FMethods.GetDimensions := @FTargetWindow.GetDimensions;
  FMethods.GetImageData := @FTargetWindow.GetImageData;
end;

procedure TSimbaTarget.SetBitmap(Bitmap: TMufasaBitmap);
begin
  ChangeTarget(ETargetType.BITMAP);

  FTargetBitmap.Bitmap := Bitmap;

  FMethods.GetDimensions := @FTargetBitmap.GetDimensions;
  FMethods.GetImageData := @FTargetBitmap.GetImageData;
end;

procedure TSimbaTarget.SetEIOS(FileName, Args: String);
begin
  ChangeTarget(ETargetType.EIOS);

  FTargetEIOS.Load(FileName, Args);

  FMethods.KeyDown := @FTargetEIOS.KeyDown;
  FMethods.KeyUp := @FTargetEIOS.KeyUp;
  FMethods.KeySend := @FTargetEIOS.KeySend;
  FMethods.KeyPressed := @FTargetEIOS.KeyPressed;

  FMethods.MouseTeleport := @FTargetEIOS.MouseTeleport;
  FMethods.MousePosition := @FTargetEIOS.MousePosition;
  FMethods.MousePressed := @FTargetEIOS.MousePressed;
  FMethods.MouseDown := @FTargetEIOS.MouseDown;
  FMethods.MouseUp := @FTargetEIOS.MouseUp;
  FMethods.MouseScroll := @FTargetEIOS.MouseScroll;

  FMethods.GetDimensions := @FTargetEIOS.GetDimensions;
  FMethods.GetImageData := @FTargetEIOS.GetImageData;
end;

function TSimbaTarget.MousePressed(Button: MouseButton): Boolean;
begin
  if HasMethod(@FMethods.MousePressed, 'MousePressed') then
    Result := FMethods.MousePressed(Button);
end;

function TSimbaTarget.MousePosition: TPoint;
begin
  if HasMethod(@FMethods.MousePosition, 'MousePosition') then
    Result := FMethods.MousePosition();
end;

procedure TSimbaTarget.MouseTeleport(P: TPoint);
begin
  if HasMethod(@FMethods.MouseTeleport, 'MouseTeleport') then
    FMethods.MouseTeleport(P);
end;

procedure TSimbaTarget.MouseUp(Button: MouseButton);
begin
  if HasMethod(@FMethods.MouseUp, 'MouseUp') then
    FMethods.MouseUp(Button);
end;

procedure TSimbaTarget.MouseDown(Button: MouseButton);
begin
  if HasMethod(@FMethods.MouseDown, 'MouseDown') then
    FMethods.MouseDown(Button);
end;

procedure TSimbaTarget.MouseClick(Button: MouseButton; ClickPressMin, ClickPressMax: Integer);
begin
  MouseDown(Button);
  SimbaNativeInterface.PreciseSleep(RandomLeft(ClickPressMin, ClickPressMax));
  MouseUp(Button);
end;

procedure TSimbaTarget.MouseScroll(Scrolls: Integer);
begin
  if HasMethod(@FMethods.MouseScroll, 'MouseScroll') then
    FMethods.MouseScroll(Scrolls);
end;

procedure TSimbaTarget.KeyDown(Key: KeyCode);
begin
  if HasMethod(@FMethods.KeyDown, 'KeyDown') then
    FMethods.KeyDown(Key);
end;

procedure TSimbaTarget.KeyUp(Key: KeyCode);
begin
  if HasMethod(@FMethods.KeyDown, 'KeyUp') then
    FMethods.KeyDown(Key);
end;

procedure TSimbaTarget.KeyPress(Key: KeyCode; KeyPressMin, KeyPressMax: Integer);
begin
  KeyDown(Key);
  SimbaNativeInterface.PreciseSleep(RandomLeft(KeyPressMin, KeyPressMax));
  KeyUp(Key);
end;

procedure TSimbaTarget.KeySend(Text: String; KeyPressMin, KeyPressMax: Integer);
var
  I: Integer;
begin
  if HasMethod(@FMethods.KeySend, 'KeySend') then
    for I := 1 to Length(Text) do
      FMethods.KeySend(Text[I], RandomLeft(KeyPressMin, KeyPressMax), RandomLeft(KeyPressMin, KeyPressMax), RandomLeft(KeyPressMin div 2, KeyPressMax div 2), RandomLeft(KeyPressMin div 2, KeyPressMax div 2));
end;

function TSimbaTarget.KeyPressed(Key: KeyCode): Boolean;
begin
  if HasMethod(@FMethods.KeyPressed, 'KeyPressed') then
    Result := FMethods.KeyPressed(Key);
end;

function TSimbaTarget.ValidateBounds(var Bounds: TBox): Boolean;
var
  Width, Height: Integer;
begin
  GetDimensions(Width, Height);

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
  end;

  Result := (Bounds.Width > 0) and (Bounds.Height > 0);
end;

function TSimbaTarget.GetImageData(var Bounds: TBox; var Data: PColorBGRA; var DataWidth: Integer): Boolean;
begin
  Data := nil;
  if HasMethod(@FMethods.GetImageData, 'GetImageData') then
    Result := ValidateBounds(Bounds) and FMethods.GetImageData(Bounds.X1, Bounds.Y1, Bounds.Width, Bounds.Height, Data, DataWidth);
end;

procedure TSimbaTarget.FreeImageData(var Data: PColorBGRA);
begin
  if (FTargetType in [ETargetType.WINDOW]) then
    FreeMem(Data);
end;

end.

