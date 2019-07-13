unit simba.iomanager;

{
  TIOManager manages multiple targets and allows you to have a seperate FImage and keyboard Target.

  NOTE: `SetTarget` creates a new Target when called so save the index returned. Either free
         with `FreeTarget` or reuse the Target with `SetImageTarget`.
}

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils,
  simba.target, simba.oswindow, bitmaps, mufasatypes;

type
  TIOManager = class(TObject)
  private
    FKeyMouse: TTarget;
    FImage: TTarget;
    FFrozen: TTarget;

    FTargetArray: array of TTarget;

    function GetAutoFocus: Boolean;
    procedure SetAutoFocus(Value: Boolean);

    function SetImageTarget(Target: TTarget): Int32;
    function SetKeyMouseTarget(Target: TTarget): Int32;
    function SetBothTargets(Target: TTarget): Int32;

    function GetTargetIdx(Target: TTarget): Int32;
    function GetIdxTarget(Index: Int32): TTarget;
  public
    procedure SetTargetEx(Proc: TSysProc);

    function SetTarget(Data: PRGB32; Size: TPoint): Int32; overload;
    function SetTarget(Bitmap: TMufasaBitmap): Int32; overload;
    function SetTarget(Name, Data: String): Int32; overload;
    function SetTarget(Target: TOSWindow): Int32; overload;

    function TargetValid: Boolean;

    function GetColor(X, Y: Int32): Int32;
    function CopyData(X, Y, Width, Height: Int32): PRGB32;
    function ReturnData(X, Y, Width, Height: Int32): TRetData;
    procedure FreeReturnData;

    procedure GetDimensions(out Width, Height: Int32);
    procedure GetPosition(var Left, Top: Int32);
    procedure ActivateClient;

    function IsFrozen: boolean;
    procedure SetFrozen(MakeFrozen: Boolean);

    function MouseSetClientArea(X1, Y1, X2, Y2: Int32): boolean;
    procedure MouseResetClientArea;
    function ImageSetClientArea(X1, Y1, X2, Y2: Int32): boolean;
    procedure ImageResetClientArea;

    procedure GetMousePos(var X, Y: Int32);
    procedure MoveMouse(X, Y: Int32);
    procedure ScrollMouse(X, Y: Int32; Lines: Int32);
    procedure HoldMouse(X, Y: Int32; Button: TClickType); overload;
    procedure HoldMouse(X, Y, Button: Int32); overload;
    procedure ReleaseMouse(X, Y: Int32; Button: TClickType); overload;
    procedure ReleaseMouse(X, Y, Button: Int32); overload;
    procedure ClickMouse(X, Y: Int32; Button: TClickType); overload;
    procedure ClickMouse(X, Y, Button: Int32); overload;
    function IsMouseButtonDown(Button: TClickType): boolean; overload;
    function IsMouseButtonDown(Button: Int32): Boolean; overload;

    procedure KeyUp(Key: Word);
    procedure KeyDown(Key: Word);
    procedure PressKey(Key: Word);
    procedure SendText(Text: String; KeyWait, KeyModWait: Int32);
    function isKeyDown(Key: Word): Boolean;
    function GetKeyCode(Character: Char): Int32;

    function GetImageTarget: TTarget;
    function GetKeyMouseTarget: TTarget;
    function ExportImageTarget: TTarget_Exported;
    function ExportKeyMouseTarget: TTarget_Exported;

    procedure GetImageTarget(var Index: Int32);
    procedure GetKeyMouseTarget(var Index: Int32);
    procedure SetImageTarget(Index: Int32);
    procedure SetKeyMouseTarget(Index: Int32);
    procedure FreeTarget(Index: Int32);

    procedure SetDesktop;

    property AutoFocus: Boolean read GetAutoFocus write SetAutoFocus;

    constructor Create;
    constructor Create(PluginPath: String);
    destructor Destroy; override;
  end;

implementation

uses
  {$IFDEF WINDOWS}
  os_windows
  {$ELSE}
  os_linux
  {$ENDIF};

constructor TIOManager.Create(PluginPath: String);
begin
  inherited Create();

  EIOSController.AddPath(PluginPath);

  SetDesktop();
end;

constructor TIOManager.Create;
begin
  inherited Create();

  SetDesktop();
end;

destructor TIOManager.Destroy;
var
  i: Int32;
begin
  for i := High(FTargetArray) downto 0 do
    FTargetArray[i].Free();
end;

procedure TIOManager.SetDesktop;
begin
  SetBothTargets(TWindow.Create(GetDesktopWindow()));
end;

procedure TIOManager.FreeTarget(Index: Int32);
begin
  if Index > High(FTargetArray) then
    raise Exception.Create('Invalid Target index');
  if FTargetArray[Index] = nil then
    raise Exception.Create('Double free of Target');
  FTargetArray[Index].Free;
  FTargetArray[Index]:= nil;
end;

function TIOManager.GetTargetIdx(Target: TTarget): Int32;
var
  i: Int32;
begin
  Result := -1;
  for i := 0 to High(FTargetArray) do
  begin
    if FTargetArray[i] = Target then
    begin
      Result := i;
      Exit;
    end;
    if (FTargetArray[i] = nil) and (Result = -1) then
      Result := i;
  end;
  if Result = -1 then
  begin
    SetLength(FTargetArray,Length(FTargetArray) + 1);
    Result := High(FTargetArray);
  end;
  FTargetArray[Result]:= Target;
end;

function TIOManager.GetIdxTarget(Index: Int32): TTarget;
begin
  if Index > High(FTargetArray) then
    raise Exception.Create('Invalid Target index');
  if FTargetArray[Index] = nil then
    raise Exception.Create('No Target with specified index');
  Result := FTargetArray[Index];
end;

function TIOManager.SetImageTarget(Target: TTarget): Int32;
begin
  if IsFrozen then
    raise Exception.Create('You cannot set a Target when FFrozen');
  Result := GetTargetIdx(Target);
  FImage := Target;
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

function TIOManager.SetKeyMouseTarget(Target: TTarget): Int32;
begin
  Result := GetTargetIdx(Target);

  FKeyMouse := Target;
end;

function TIOManager.GetKeyMouseTarget: TTarget;
begin
  Result := FKeyMouse;
end;

function TIOManager.ExportImageTarget: TTarget_Exported;
begin
  FillChar(Result, SizeOf(TTarget_Exported) ,0);
  with Result do
  begin
    Target := FImage;

    GetTargetDimensions:= @TTarget_Exported_GetTargetDimensions;
    GetTargetPosition := @TTarget_Exported_GetTargetPosition;
    GetColor:= @TTarget_Exported_GetColor;
    ReturnData := @TTarget_Exported_ReturnData;
    FreeReturnData:= @TTarget_Exported_FreeReturnData;
  end;
end;

function TIOManager.ExportKeyMouseTarget: TTarget_Exported;
begin
  FillChar(Result, SizeOf(TTarget_Exported), 0);
  with Result do
  begin
    Target := FKeyMouse;

    GetMousePosition := @TTarget_Exported_GetMousePosition;
    MoveMouse := @TTarget_Exported_MoveMouse;
    ScrollMouse:= @TTarget_Exported_ScrollMouse;
    HoldMouse := @TTarget_Exported_HoldMouse;
    ReleaseMouse := @TTarget_Exported_ReleaseMouse;

    SendString := @TTarget_Exported_SendString;
    HoldKey := @TTarget_Exported_HoldKey;
    ReleaseKey := @TTarget_Exported_ReleaseKey;
    IsKeyHeld := @TTarget_Exported_IsKeyHeld;
    GetKeyCode := @TTarget_Exported_GetKeyCode;
  end;
end;

function TIOManager.SetBothTargets(Target: TTarget): Int32;
begin
  if IsFrozen then
    raise Exception.Create('You cannot set a Target when FFrozen');
  Result := GetTargetIdx(Target);
  FImage:= Target;
  FKeyMouse:= Target;
end;

procedure TIOManager.SetFrozen(MakeFrozen: Boolean);
var
  W, H: Int32;
begin
  if MakeFrozen and IsFrozen then
    raise Exception.Create('The window is already Frozen.');

  if MakeFrozen then //No need for the FFrozen = nil check, already done above with the exception.
  begin
    FFrozen := FImage;
    FFrozen.GetTargetDimensions(W, H);
    FImage := TRawTarget.Create(FFrozen.ReturnData(0, 0, W, H).Ptr, W, H, True);
    FFrozen.FreeReturnData();
  end else
  begin
    if not IsFrozen() then
      raise Exception.Create('Unfreeze called when the window is not FFrozen.')
    else
    begin
      FImage.Free();
      FImage := FFrozen;
      FFrozen := nil;
    end;
  end;
end;

function TIOManager.IsFrozen: boolean;
begin
  Result := FFrozen <> nil;
end;

function TIOManager.GetColor(X, Y: Int32): Int32;
begin
  Result := FImage.GetColor(X, Y);
end;

function TIOManager.CopyData(X, Y, Width, Height: Int32): PRGB32;
begin
  Result := FImage.CopyData(X, Y, Width, Height);
end;

function TIOManager.ReturnData(X, Y, Width, Height: Int32): TRetData;
begin
  Result := FImage.ReturnData(X, Y, Width, Height);
end;

procedure TIOManager.FreeReturnData;
begin
  FImage.FreeReturnData();
end;

function TIOManager.SetTarget(Data: PRGB32; Size: TPoint): Int32;
begin
  Result := SetImageTarget(TRawTarget.Create(Data, Size.X, Size.Y));
end;

function TIOManager.SetTarget(Bitmap: TMufasaBitmap): Int32;
begin
  Result := SetImageTarget(TBitmapTarget.Create(Bitmap));
end;

function TIOManager.SetTarget(Name, Data: String): Int32;
var
  Client: TEIOS_Client;
begin
  if not EIOSController.ClientExists(name) then
    raise Exception.Create('EIOS Client by specified name does not exist');
  Client := EIOSController.GetClient(name);

  Result := SetBothTargets(TEIOS_Target.Create(Client, Data));
end;

function TIOManager.SetTarget(Target: TOSWindow): Int32;
begin
  Result := SetBothTargets(TWindow.Create(Target));
end;

procedure TIOManager.SetImageTarget(Index: Int32);
begin
  FImage := GetIdxTarget(Index);
end;

procedure TIOManager.SetKeyMouseTarget(Index: Int32);
begin
  FKeyMouse := GetIdxTarget(Index);
end;

procedure TIOManager.GetImageTarget(var Index: Int32);
begin
  if IsFrozen then
    raise Exception.Create('Cannot get image target whilst frozen');

  Index := GetTargetIdx(FImage);
end;

procedure TIOManager.GetKeyMouseTarget(var Index: Int32);
begin
  Index := GetTargetIdx(FKeyMouse);
end;

function TIOManager.TargetValid: Boolean;
begin
  Result := False;
  if (FKeyMouse <> nil) and (FImage <> nil) then
    Result := FKeyMouse.TargetValid() and FImage.TargetValid();
end;

procedure TIOManager.GetDimensions(out Width, Height: Int32);
begin
  FImage.GetTargetDimensions(Width, Height)
end;

procedure TIOManager.GetPosition(var Left, Top: Int32);
begin
  FImage.GetTargetPosition(Left, Top);
end;

procedure TIOManager.ActivateClient;
begin
  FKeyMouse.ActivateClient();
  {not sure if FImage needs activation or not, if its a native window FKeyMouse == FImage so it should be good.}
end;

function TIOManager.MouseSetClientArea(X1, Y1, X2, Y2: Int32): boolean;
begin
  Result := FKeyMouse.MouseSetClientArea(X1, Y1, X2, Y2);
end;

procedure TIOManager.MouseResetClientArea;
begin
  FKeyMouse.MouseResetClientArea();
end;

function TIOManager.ImageSetClientArea(X1, Y1, X2, Y2: Int32): boolean;
begin
  Result := FImage.ImageSetClientArea(X1, Y1, X2, Y2);
end;

procedure TIOManager.ImageResetClientArea;
begin
  FImage.ImageResetClientArea();
end;

procedure TIOManager.GetMousePos(var X, Y: Int32);
begin
  FKeyMouse.GetMousePosition(X, Y)
end;

procedure TIOManager.MoveMouse(X, Y: Int32);
begin
  FKeyMouse.MoveMouse(X, Y);
end;

procedure TIOManager.ScrollMouse(X, Y: Int32; Lines: Int32);
begin
  FKeyMouse.ScrollMouse(X, Y,lines);
end;

procedure TIOManager.HoldMouse(X, Y: Int32; Button: TClickType);
begin
  FKeyMouse.HoldMouse(X, Y, Button);
end;

procedure TIOManager.HoldMouse(X, Y, Button: Int32);
begin
  FKeyMouse.HoldMouse(X, Y, TClickType(Button));
end;

procedure TIOManager.ReleaseMouse(X, Y: Int32; Button: TClickType);
begin
  FKeyMouse.ReleaseMouse(X, Y, Button);
end;

procedure TIOManager.ReleaseMouse(X, Y, Button: Int32);
begin
  FKeyMouse.ReleaseMouse(X, Y, TClickType(Button));
end;

procedure TIOManager.ClickMouse(X, Y: Int32; Button: TClickType);
begin
  HoldMouse(X, Y, Button);
  //BenLand100 note: probably should wait here
  ReleaseMouse(X, Y, Button);
end;

procedure TIOManager.ClickMouse(X, Y, Button: Int32);
begin
  HoldMouse(X, Y, TClickType(Button));
  //BenLand100 note: probably should wait here
  ReleaseMouse(X, Y, TClickType(Button));
end;

function TIOManager.IsMouseButtonDown(Button: TClickType): boolean;
begin
  Result := FKeyMouse.IsMouseButtonHeld(Button);
end;

function TIOManager.IsMouseButtonDown(Button: Int32): Boolean;
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

procedure TIOManager.SendText(Text: String; KeyWait, KeyModWait: Int32);
begin
  FKeyMouse.SendString(Text, KeyWait, KeyModWait);
end;

function TIOManager.isKeyDown(Key: Word): Boolean;
begin
  Result := FKeyMouse.IsKeyHeld(Key);
end;

function TIOManager.GetKeyCode(Character: Char): Int32;
begin
  Result := FKeyMouse.GetKeyCode(Character);
end;

procedure TIOManager.SetTargetEx(Proc: TSysProc);
begin
  SetTarget(Proc.Handle);
end;

end.

