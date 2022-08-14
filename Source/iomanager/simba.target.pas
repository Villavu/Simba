{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
{$i simba.inc}

unit simba.target;

interface

uses
  classes, sysutils,
  simba.mufasatypes;

type
  PTarget = ^TTarget;
  TTarget = class(TObject)
  protected
    FMouseClientAreaSet: Boolean;
    FMouseClientArea: TBox;
    FImageClientAreaSet: Boolean;
    FImageClientArea: TBox;

    function ValidateImageCapture(var X, Y, Width, Height: Integer; out TargetBounds: TBox): Boolean; virtual;

    procedure GetTargetBounds(out Bounds: TBox); virtual; // raw bounds. (no image client area etc)
  public
    // Client area
    procedure MouseClientAreaOffset(var X, Y: Integer); virtual;
    function MouseSetClientArea(X1, Y1, X2, Y2: Integer): Boolean; virtual;
    procedure MouseResetClientArea; virtual;
    procedure ImageClientAreaOffset(var X, Y: Integer); virtual;
    function ImageSetClientArea(X1, Y1, X2, Y2: Integer): Boolean; virtual;
    procedure ImageResetClientArea; virtual;

    // Position, Dimensions
    procedure GetTargetDimensions(out Width, Height: Integer); virtual;
    procedure GetTargetPosition(out Left, Top: Integer); virtual;

    // Colors
    function GetColor(X, Y: Integer): Integer; virtual;
    function CopyData(X, Y, Width, Height: Integer): PRGB32; virtual;
    function ReturnData(X, Y, Width, Height: Integer): TRetData; virtual;
    function ReturnMatrix(X, Y, Width, Height: Integer): TIntegerMatrix; virtual;

    // Mouse
    procedure GetMousePosition(out X, Y: Integer); virtual;
    procedure MoveMouse(X, Y: Integer); virtual;
    procedure ScrollMouse(X, Y: Integer; Lines: Integer); virtual;
    procedure HoldMouse(X, Y: Integer; Button: TClickType); virtual;
    procedure ReleaseMouse(X, Y: Integer; Button: TClickType); virtual;
    function IsMouseButtonHeld(Button: TClickType): Boolean; virtual;

    // Keyboard
    procedure SendString(Text: String; KeyWait, KeyModWait: Integer); virtual;
    procedure SendStringEx(Text: String; MinKeyWait, MaxKeyWait: Integer); virtual;
    procedure HoldKey(Key: Integer); virtual;
    procedure ReleaseKey(Key: Integer); virtual;
    function IsKeyHeld(Key: Integer): Boolean; virtual;
    function GetKeyCode(Character: Char) : Integer; virtual;

    // Activate
    procedure ActivateClient; virtual;

    // Valid
    function TargetValid: Boolean; virtual;
  end;

  TTargetArray = array of TTarget;

implementation

uses
  simba.colormath;

procedure TTarget.GetTargetPosition(out Left, Top: Integer);
var
  Bounds: TBox;
begin
  if FImageClientAreaSet then
  begin
    Left := FImageClientArea.X1;
    Top := FImageClientArea.Y1;
  end else
  begin
    GetTargetBounds(Bounds);

    Left := Bounds.X1;
    Top := Bounds.Y1;
  end;
end;

procedure TTarget.GetTargetDimensions(out Width, Height: Integer);
var
  Bounds: TBox;
begin
  if FImageClientAreaSet then
  begin
    Width := FImageClientArea.Width;
    Height := FImageClientArea.Height;
  end else
  begin
    GetTargetBounds(Bounds);

    Width := Bounds.Width - 1;
    Height := Bounds.Height - 1;
  end;
end;

procedure TTarget.GetTargetBounds(out Bounds: TBox);
begin
  raise Exception.Create('GetTargetBounds not available for this target');
end;

function TTarget.ValidateImageCapture(var X, Y, Width, Height: Integer; out TargetBounds: TBox): Boolean;
begin
  GetTargetBounds(TargetBounds);
  ImageClientAreaOffset(X, Y);

  Result := (TargetBounds.X1 + X >= TargetBounds.X1) and (TargetBounds.Y1 + Y >= TargetBounds.Y1) and (X + Width <= TargetBounds.X2) and (Y + Height <= TargetBounds.Y2);
end;

function TTarget.GetColor(X, Y: Integer): Integer;
var
  Data: TRetData;
begin
  Data := ReturnData(X, Y, 1, 1);
  if (Data.Ptr <> nil) then
    Result := RGBToColor(Data.Ptr^.R, Data.Ptr^.G, Data.Ptr^.B)
  else
    Result := 0;
end;

function TTarget.CopyData(X, Y, Width, Height: Integer): PRGB32;
begin
  raise Exception.Create('CopyData not availble for this target');
end;

function TTarget.ReturnData(X, Y, Width, Height: Integer): TRetData;
begin
  raise Exception.Create('ReturnData not available for this target');
end;

function TTarget.ReturnMatrix(X, Y, Width, Height: Integer): TIntegerMatrix;
var
  Data: TRetData;
  Source: PRGB32;
  SourceInc: Integer;
  LoopX, LoopY: Integer;
  W, H: Integer;
begin
  Result.SetSize(Width, Height);

  Data := ReturnData(X, Y, Width, Height);
  if (Data.Ptr <> nil) then
  begin
    W := Width - 1;
    H := Height - 1;

    Source := Data.Ptr;
    SourceInc := Data.IncPtrWith;

    for LoopY := 0 to H do
    begin
      for LoopX := 0 to W do
      begin
        Result[LoopY][LoopX] := BGRToRGB(Source^);

        Inc(Source);
      end;

      Inc(Source, SourceInc);
    end;
  end;
end;

procedure TTarget.ActivateClient;
begin
  raise Exception.Create('ActivateClient not available for this target');
end;

function TTarget.TargetValid: Boolean;
begin
  Result := True;
end;

function TTarget.MouseSetClientArea(X1, Y1, X2, Y2: Integer): Boolean;
begin
  Result := True;

  FMouseClientAreaSet := True;

  FMouseClientArea.X1 := X1;
  FMouseClientArea.Y1 := Y1;
  FMouseClientArea.X2 := X2;
  FMouseClientArea.Y2 := Y2;
end;

procedure TTarget.MouseResetClientArea;
begin
  FMouseClientAreaSet := False;

  FMouseClientArea.X1 := 0;
  FMouseClientArea.Y1 := 0;
  FMouseClientArea.X2 := 0;
  FMouseClientArea.Y2 := 0;
end;

procedure TTarget.ImageClientAreaOffset(var X, Y: Integer);
begin
  if FImageClientAreaSet then
  begin
    X := X + FImageClientArea.X1;
    Y := Y + FImageClientArea.Y1;
  end;
end;

function TTarget.ImageSetClientArea(X1, Y1, X2, Y2: Integer): Boolean;
begin
  Result := True;

  FImageClientAreaSet := True;

  FImageClientArea.X1 := X1;
  FImageClientArea.Y1 := Y1;
  FImageClientArea.X2 := X2;
  FImageClientArea.Y2 := Y2;
end;

procedure TTarget.ImageResetClientArea;
begin
  FImageClientAreaSet := False;

  FImageClientArea.X1 := 0;
  FImageClientArea.Y1 := 0;
  FImageClientArea.X2 := 0;
  FImageClientArea.Y2 := 0;
end;

procedure TTarget.GetMousePosition(out X, Y: Integer);
begin
  raise Exception.Create('GetMousePosition not available for this target');
end;

procedure TTarget.MoveMouse(X, Y: Integer);
begin
  raise Exception.Create('MoveMouse not available for this target');
end;

procedure TTarget.ScrollMouse(X, Y: Integer; Lines: Integer);
begin
  raise Exception.Create('ScrollMouse is not available for this target');
end;

procedure TTarget.HoldMouse(X, Y: Integer; Button: TClickType);
begin
  raise Exception.Create('HoldMouse not available for this target');
end;

procedure TTarget.ReleaseMouse(X, Y: Integer; Button: TClickType);
begin
  raise Exception.Create('ReleaseMouse not available for this target');
end;

function TTarget.IsMouseButtonHeld(Button: TClickType): Boolean;
begin
  raise Exception.Create('IsMouseButtonHeld not available for this target');
end;

procedure TTarget.SendString(Text: String; KeyWait, KeyModWait: Integer);
begin
  raise Exception.Create('SendString not available for this target');
end;

procedure TTarget.SendStringEx(Text: String; MinKeyWait, MaxKeyWait: Integer);
begin
  raise Exception.Create('SendStringEx not available for this target');
end;

procedure TTarget.HoldKey(Key: Integer);
begin
  raise Exception.Create('HoldKey not available for this target');
end;

procedure TTarget.ReleaseKey(Key: Integer);
begin
  raise Exception.Create('ReleaseKey not available for this target');
end;

function TTarget.IsKeyHeld(Key: Integer): Boolean;
begin
  raise Exception.Create('IsKeyHeld not available for this target');
end;

function TTarget.GetKeyCode(Character: Char): Integer;
begin
  raise Exception.Create('GetKeyCode is not available for this target');
end;

procedure TTarget.MouseClientAreaOffset(var X, Y: Integer);
begin
  if FMouseClientAreaSet then
  begin
    X := X + FMouseClientArea.X1;
    Y := Y + FMouseClientArea.Y1;
  end;
end;


end.

