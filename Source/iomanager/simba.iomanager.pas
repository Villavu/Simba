{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.iomanager;

{
  TIOManager manages multiple targets and allows you to have a seperate FImage and keyboard Target.

  NOTE: `SetTarget` creates a new Target when called so save the index returned. Either free
         with `FreeTarget` or reuse the Target with `SetImageTarget`.
}

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.target, simba.target_exported, simba.bitmap, simba.mufasatypes;

type
  PIOManager = ^TIOManager;
  TIOManager = class(TObject)
  protected
    FKeyMouse: TTarget;
    FImage: TTarget;
    FFrozen: TTarget;
    FPluginPaths: array of String;
    FTargetArray: TTargetArray;

    function AddTarget(Target: TTarget): Integer;

    function SetImageTarget(Target: TTarget): Integer;
    function SetKeyMouseTarget(Target: TTarget): Integer;

    function GetTarget(Index: Integer): TTarget;
    function GetTargetIndex(Target: TTarget): Integer;

    function GetAutoFocus: Boolean;
    procedure SetAutoFocus(Value: Boolean);
  public
    function SetTarget(Data: PRGB32; Size: TPoint): Integer; overload;
    function SetTarget(Bitmap: TMufasaBitmap): Integer; overload;
    function SetTarget(Plugin, Data: String): Integer; overload;
    function SetTarget(Window: TWindowHandle): Integer; overload;

    function TargetValid: Boolean;

    function GetColor(X, Y: Integer): Integer;
    function CopyData(X, Y, Width, Height: Integer): PRGB32;
    function ReturnData(X, Y, Width, Height: Integer): TRetData;
    function ReturnMatrix(X, Y, Width, Height: Integer): TIntegerMatrix;

    procedure GetDimensions(out Width, Height: Integer);
    procedure GetPosition(out Left, Top: Integer);
    procedure ActivateClient;

    function IsFrozen: Boolean;
    procedure SetFrozen(MakeFrozen: Boolean);

    function MouseSetClientArea(X1, Y1, X2, Y2: Integer): Boolean;
    procedure MouseResetClientArea;
    function ImageSetClientArea(X1, Y1, X2, Y2: Integer): Boolean;
    procedure ImageResetClientArea;

    procedure GetMousePos(var X, Y: Integer);
    procedure MoveMouse(X, Y: Integer);
    procedure ScrollMouse(X, Y: Integer; Lines: Integer);
    procedure HoldMouse(X, Y: Integer; Button: TClickType); overload;
    procedure HoldMouse(X, Y, Button: Integer); overload;
    procedure ReleaseMouse(X, Y: Integer; Button: TClickType); overload;
    procedure ReleaseMouse(X, Y, Button: Integer); overload;
    procedure ClickMouse(X, Y: Integer; Button: TClickType); overload;
    procedure ClickMouse(X, Y, Button: Integer); overload;
    function IsMouseButtonDown(Button: TClickType): Boolean; overload;
    function IsMouseButtonDown(Button: Integer): Boolean; overload;

    procedure KeyUp(Key: Word);
    procedure KeyDown(Key: Word);
    procedure PressKey(Key: Word);
    procedure SendText(Text: String; KeyWait, KeyModWait: Integer);
    procedure SendTextEx(Text: String; MinKeyWait, MaxKeyWait: Integer);
    function IsKeyDown(Key: Word): Boolean;
    function GetKeyCode(Character: Char): Integer;

    function GetImageTarget: TTarget;
    function GetKeyMouseTarget: TTarget;
    function ExportImageTarget: TTarget_Exported;
    function ExportKeyMouseTarget: TTarget_Exported;

    procedure GetImageTarget(var Index: Integer);
    procedure GetKeyMouseTarget(var Index: Integer);
    function SetImageTarget(Index: Integer): Integer;
    procedure SetKeyMouseTarget(Index: Integer);
    procedure FreeTarget(Index: Integer);

    function SetImageTargetEx(Window: TWindowHandle): TTarget;
    function SetKeyMouseTargetEx(Window: TWindowHandle): TTarget;

    function SetDesktop: Integer;

    property AutoFocus: Boolean read GetAutoFocus write SetAutoFocus;

    constructor Create;
    constructor Create(PluginPath: String);
    destructor Destroy; override;
  end;

implementation

uses
  math, lazloggerbase,
  simba.files, simba.target_bitmap, simba.target_eios, simba.target_window,
  simba.nativeinterface;

constructor TIOManager.Create;
begin
  inherited Create();

  SetDesktop();
end;

constructor TIOManager.Create(PluginPath: String);
begin
  Create();

  if PluginPath <> '' then
  begin
    SetLength(FPluginPaths, Length(FPluginPaths) + 1);
    FPluginPaths[High(FPluginPaths)] := PluginPath;
  end;
end;

destructor TIOManager.Destroy;
var
  I, Leaks: Integer;
begin
  Leaks := 0;
  for I := 0 to High(FTargetArray) do
    if (FTargetArray[I] <> nil) then
    begin
      FreeAndNil(FTargetArray[I]);

      Inc(Leaks);
    end;

  if (Leaks > 5) then
    DebugLn('Warning: You might be leaking targets! Use FreeTarget');

  inherited Destroy();
end;

function TIOManager.SetDesktop: Integer;
begin
  Result := SetTarget(SimbaNativeInterface.GetDesktopWindow());
end;

procedure TIOManager.FreeTarget(Index: Integer);
begin
  if (not InRange(Index, Low(FTargetArray), High(FTargetArray))) or (FTargetArray[Index] = nil) then
    raise Exception.CreateFmt('TIOManager.FreeTarget: Invalid index "%d" ', [Index]);

  FreeAndNil(FTargetArray[Index]);
end;

function TIOManager.SetImageTargetEx(Window: TWindowHandle): TTarget;
begin
  Result := GetTarget(AddTarget(TWindowTarget.Create(Window)));

  FImage := Result;
end;

function TIOManager.SetKeyMouseTargetEx(Window: TWindowHandle): TTarget;
begin
  Result := GetTarget(AddTarget(TWindowTarget.Create(Window)));

  FKeyMouse := Result;
end;

function TIOManager.SetImageTarget(Target: TTarget): Integer;
begin
  if IsFrozen then
    raise Exception.Create('TIOManager.SetImageTarget: You cannot set a Target when FFrozen');

  Result := GetTargetIndex(Target);
  FImage := Target;
end;

function TIOManager.AddTarget(Target: TTarget): Integer;
var
  I: Integer;
begin
  Result := Length(FTargetArray);

  // Existing index
  for I := 0 to High(FTargetArray) do
    if (FTargetArray[I] = nil) then
    begin
      Result := I;
      Break;
    end;

  if (Result = Length(FTargetArray)) then
    SetLength(FTargetArray, Length(FTargetArray) + 1);

  FTargetArray[Result] := Target;
end;

function TIOManager.GetTarget(Index: Integer): TTarget;
begin
  if (not InRange(Index, Low(FTargetArray), High(FTargetArray))) or (FTargetArray[Index] = nil) then
    raise Exception.CreateFmt('TIOManager.GetTarget: Invalid index "%d" ', [Index]);

  Result := FTargetArray[Index];
end;

function TIOManager.GetTargetIndex(Target: TTarget): Integer;
var
  I: Integer;
begin
  for I := 0 to High(FTargetArray) do
    if (FTargetArray[I] = Target) then
    begin
      Result := I;
      Exit;
    end;

  raise Exception.CreateFmt('TIOManager.GetTargetIndex: Invalid target "%s"', [HexStr(Target)]);
end;

function TIOManager.GetAutoFocus: Boolean;
begin
  Result := (FKeyMouse <> nil) and FKeyMouse.AutoFocus;
end;

procedure TIOManager.SetAutoFocus(Value: Boolean);
begin
  if (FKeyMouse <> nil) then
    FKeyMouse.AutoFocus := Value;
end;

function TIOManager.GetImageTarget: TTarget;
begin
  Result := FImage;
end;

function TIOManager.SetKeyMouseTarget(Target: TTarget): Integer;
begin
  Result := GetTargetIndex(Target);

  FKeyMouse := Target;
end;

function TIOManager.GetKeyMouseTarget: TTarget;
begin
  Result := FKeyMouse;
end;

function TIOManager.ExportImageTarget: TTarget_Exported;
begin
  Result := FImage.ExportImageTarget();
end;

function TIOManager.ExportKeyMouseTarget: TTarget_Exported;
begin
  Result := FKeyMouse.ExportKeyMouseTarget();
end;

procedure TIOManager.SetFrozen(MakeFrozen: Boolean);
var
  Width, Height: Integer;
begin
  if MakeFrozen and IsFrozen() then
    raise Exception.Create('TIOManager.SetFrozen: The window is already Frozen.');

  if MakeFrozen then
  begin
    FFrozen := FImage;
    FFrozen.GetTargetDimensions(Width, Height);
    with FFrozen.ReturnData(0, 0, Width - 1, Height - 1) do
      FImage := TBitmapTarget.Create(Ptr, Width, Height, True);
  end else
  if IsFrozen() then
  begin
    if (FImage <> nil) then
      FreeAndNil(FImage);

    FImage := FFrozen;
  end;
end;

function TIOManager.IsFrozen: Boolean;
begin
  Result := FFrozen <> nil;
end;

function TIOManager.GetColor(X, Y: Integer): Integer;
begin
  Result := FImage.GetColor(X, Y);
end;

function TIOManager.CopyData(X, Y, Width, Height: Integer): PRGB32;
begin
  Result := FImage.CopyData(X, Y, Width, Height);
end;

function TIOManager.ReturnData(X, Y, Width, Height: Integer): TRetData;
begin
  Result := FImage.ReturnData(X, Y, Width, Height);
end;

function TIOManager.ReturnMatrix(X, Y, Width, Height: Integer): TIntegerMatrix;
begin
  Result := FImage.ReturnMatrix(X, Y, Width, Height);
end;

function TIOManager.SetTarget(Data: PRGB32; Size: TPoint): Integer;
begin
  Result := SetImageTarget(AddTarget(TBitmapTarget.Create(Data, Size.X, Size.Y, True)));
end;

function TIOManager.SetTarget(Bitmap: TMufasaBitmap): Integer;
begin
  Result := SetImageTarget(AddTarget(TBitmapTarget.Create(Bitmap)));
end;

function TIOManager.SetTarget(Plugin, Data: String): Integer;
begin
  if not FindPlugin(Plugin, FPluginPaths) then
    raise Exception.CreateFmt('TIOManager.SetTarget: EIOS plugin not found "%s"', [Plugin]);

  Result := AddTarget(TEIOS_Target.Create(Plugin, Data));

  SetImageTarget(Result);
  SetKeyMouseTarget(Result);
end;

function TIOManager.SetTarget(Window: TWindowHandle): Integer;
begin
  Result := AddTarget(TWindowTarget.Create(Window));

  SetImageTarget(Result);
  SetKeyMouseTarget(Result);
end;

function TIOManager.SetImageTarget(Index: Integer): Integer;
begin
  Result := Index;

  FImage := GetTarget(Index);
end;

procedure TIOManager.SetKeyMouseTarget(Index: Integer);
begin
  FKeyMouse := GetTarget(Index);
end;

procedure TIOManager.GetImageTarget(var Index: Integer);
begin
  if IsFrozen then
    raise Exception.Create('TIOManager.GetImageTarget: Cannot get image target when frozen');

  Index := GetTargetIndex(FImage);
end;

procedure TIOManager.GetKeyMouseTarget(var Index: Integer);
begin
  Index := GetTargetIndex(FKeyMouse);
end;

function TIOManager.TargetValid: Boolean;
begin
  Result := False;
  if (FKeyMouse <> nil) and (FImage <> nil) then
    Result := FKeyMouse.TargetValid() and FImage.TargetValid();
end;

procedure TIOManager.GetDimensions(out Width, Height: Integer);
begin
  FImage.GetTargetDimensions(Width, Height)
end;

procedure TIOManager.GetPosition(out Left, Top: Integer);
begin
  FImage.GetTargetPosition(Left, Top);
end;

procedure TIOManager.ActivateClient;
begin
  FKeyMouse.ActivateClient();
  {not sure if FImage needs activation or not, if its a native window FKeyMouse == FImage so it should be good.}
end;

function TIOManager.MouseSetClientArea(X1, Y1, X2, Y2: Integer): Boolean;
begin
  Result := FKeyMouse.MouseSetClientArea(X1, Y1, X2, Y2);
end;

procedure TIOManager.MouseResetClientArea;
begin
  FKeyMouse.MouseResetClientArea();
end;

function TIOManager.ImageSetClientArea(X1, Y1, X2, Y2: Integer): Boolean;
begin
  Result := FImage.ImageSetClientArea(X1, Y1, X2, Y2);
end;

procedure TIOManager.ImageResetClientArea;
begin
  FImage.ImageResetClientArea();
end;

procedure TIOManager.GetMousePos(var X, Y: Integer);
begin
  FKeyMouse.GetMousePosition(X, Y)
end;

procedure TIOManager.MoveMouse(X, Y: Integer);
begin
  FKeyMouse.MoveMouse(X, Y);
end;

procedure TIOManager.ScrollMouse(X, Y: Integer; Lines: Integer);
begin
  FKeyMouse.ScrollMouse(X, Y,lines);
end;

procedure TIOManager.HoldMouse(X, Y: Integer; Button: TClickType);
begin
  FKeyMouse.HoldMouse(X, Y, Button);
end;

procedure TIOManager.HoldMouse(X, Y, Button: Integer);
begin
  FKeyMouse.HoldMouse(X, Y, TClickType(Button));
end;

procedure TIOManager.ReleaseMouse(X, Y: Integer; Button: TClickType);
begin
  FKeyMouse.ReleaseMouse(X, Y, Button);
end;

procedure TIOManager.ReleaseMouse(X, Y, Button: Integer);
begin
  FKeyMouse.ReleaseMouse(X, Y, TClickType(Button));
end;

procedure TIOManager.ClickMouse(X, Y: Integer; Button: TClickType);
begin
  HoldMouse(X, Y, Button);
  //BenLand100 note: probably should wait here
  ReleaseMouse(X, Y, Button);
end;

procedure TIOManager.ClickMouse(X, Y, Button: Integer);
begin
  HoldMouse(X, Y, TClickType(Button));
  //BenLand100 note: probably should wait here
  ReleaseMouse(X, Y, TClickType(Button));
end;

function TIOManager.IsMouseButtonDown(Button: TClickType): Boolean;
begin
  Result := FKeyMouse.IsMouseButtonHeld(Button);
end;

function TIOManager.IsMouseButtonDown(Button: Integer): Boolean;
begin
  Result := FKeyMouse.IsMouseButtonHeld(TClickType(Button));
end;

procedure TIOManager.KeyUp(Key: Word);
begin
  FKeyMouse.ReleaseKey(Key)
end;

procedure TIOManager.KeyDown(Key: Word);
begin
  FKeyMouse.HoldKey(Key)
end;

procedure TIOManager.PressKey(Key: Word);
begin
  KeyDown(Key);
  KeyUp(Key);
end;

procedure TIOManager.SendText(Text: String; KeyWait, KeyModWait: Integer);
begin
  FKeyMouse.SendString(Text, KeyWait, KeyModWait);
end;

procedure TIOManager.SendTextEx(Text: String; MinKeyWait, MaxKeyWait: Integer);
begin
  FKeyMouse.SendStringEx(Text, MinKeyWait, MaxKeyWait);
end;

function TIOManager.IsKeyDown(Key: Word): Boolean;
begin
  Result := FKeyMouse.IsKeyHeld(Key);
end;

function TIOManager.GetKeyCode(Character: Char): Integer;
begin
  Result := FKeyMouse.GetKeyCode(Character);
end;

end.

