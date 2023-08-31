﻿{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.imagebox_zoom;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, Graphics,
  simba.image, simba.imagebox;

type
  TSimbaImageBoxZoom = class(TCustomControl)
  protected
    FBitmap: TBitmap;
    FPixelCount: Integer;
    FPixelSize: Integer;
    FTempColor: Integer;

    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer; WithThemeSpace: Boolean); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetTempColor(AColor: Integer);
    procedure SetZoom(PixelCount, PixelSize: Integer);
    procedure MoveTest(ImageBox: TSimbaImageBox; X, Y: Integer);

    procedure Move(Image: TImage; X, Y: Integer); overload;
    procedure Move(Image: TBitmap; X, Y: Integer); overload;
    procedure Move(Image: TSimbaImage; X, Y: Integer); overload;
  end;

implementation

uses
  simba.nativeinterface;

constructor TSimbaImageBoxZoom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FTempColor := -1;
  FBitmap := TBitmap.Create();
  FBitmap.Canvas.AntialiasingMode := amOff;

  SetZoom(5, 5);
  Color := clWindow;
  AutoSize := True;
end;

destructor TSimbaImageBoxZoom.Destroy;
begin
  if (FBitmap <> nil) then
    FreeAndNil(FBitmap);

  inherited Destroy();
end;

procedure TSimbaImageBoxZoom.SetTempColor(AColor: Integer);
begin
  FTempColor := AColor;

  Invalidate();
end;

procedure TSimbaImageBoxZoom.CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer; WithThemeSpace: Boolean);
begin
  PreferredWidth := (FPixelCount * 2) * FPixelSize;
  PreferredHeight := (FPixelCount * 2) * FPixelSize;

  Inc(PreferredWidth, 2);
  Inc(PreferredHeight, 2);

  FBitmap.SetSize(FPixelCount, FPixelCount);
end;

procedure TSimbaImageBoxZoom.Paint;
var
  R: TRect;
begin
  if (FTempColor > -1) then
  begin
    Canvas.Pen.Color := clBlack;
    Canvas.Brush.Color := FTempColor;
    Canvas.Rectangle(ClientRect);

    Exit;
  end;

  R := TRect.Create(ClientRect.CenterPoint);

  with ClientRect.CenterPoint() do
  begin
    R.Left := X - FPixelSize;
    R.Top := Y - FPixelSize;
    R.Right := X + FPixelSize;
    R.Bottom := Y + FPixelSize;
  end;

  Canvas.AntialiasingMode := amOff;
  Canvas.StretchDraw(TRect.Create(1, 1, ClientWidth - 1, ClientHeight - 1), FBitmap);

  Canvas.Pen.Color := clBlack;
  Canvas.Frame(ClientRect);

  Canvas.Pen.Color := clLime;
  Canvas.Frame(R);
end;

procedure TSimbaImageBoxZoom.SetZoom(PixelCount, PixelSize: Integer);
begin
  if not Odd(PixelCount) then
    Inc(PixelCount);

  FPixelCount := PixelCount;
  FPixelSize  := PixelCount + PixelSize;

  DoAutoSize();
end;

procedure TSimbaImageBoxZoom.MoveTest(ImageBox: TSimbaImageBox; X, Y: Integer);
var
  LoopX, LoopY: Integer;
begin
  Dec(X, FPixelCount div 2);
  Dec(Y, FPixelCount div 2);

  for LoopX := 0 to FBitmap.Width - 1 do
    for LoopY := 0 to FBitmap.Height - 1 do
      FBitmap.Canvas.Pixels[LoopX, LoopY] := ImageBox.Background.Canvas.Pixels[X + LoopX, Y + LoopY];

  Invalidate();
end;

procedure TSimbaImageBoxZoom.Move(Image: TImage; X, Y: Integer);
var
  LoopX, LoopY: Integer;
begin
  Dec(X, FPixelCount div 2);
  Dec(Y, FPixelCount div 2);

  for LoopX := 0 to FBitmap.Width - 1 do
    for LoopY := 0 to FBitmap.Height - 1 do
      FBitmap.Canvas.Pixels[LoopX, LoopY] := Image.Canvas.Pixels[X + LoopX, Y + LoopY];

  Invalidate();
end;

procedure TSimbaImageBoxZoom.Move(Image: TBitmap; X, Y: Integer);
var
  LoopX, LoopY: Integer;
begin
  Dec(X, FPixelCount div 2);
  Dec(Y, FPixelCount div 2);

  for LoopX := 0 to FBitmap.Width - 1 do
    for LoopY := 0 to FBitmap.Height - 1 do
      FBitmap.Canvas.Pixels[LoopX, LoopY] := Image.Canvas.Pixels[X + LoopX, Y + LoopY];

  Invalidate();
end;

procedure TSimbaImageBoxZoom.Move(Image: TSimbaImage; X, Y: Integer);
var
  LoopX, LoopY: Integer;
begin
  FTempColor := -1;

  Dec(X, FPixelCount div 2);
  Dec(Y, FPixelCount div 2);

  for LoopX := 0 to FBitmap.Width - 1 do
    for LoopY := 0 to FBitmap.Height - 1 do
      if Image.InImage(X + LoopX, Y + LoopY) then
        FBitmap.Canvas.Pixels[LoopX, LoopY] := Image.Pixel[X + LoopX, Y + LoopY]
      else
        FBitmap.Canvas.Pixels[LoopX, LoopY] := clBlack;

  Invalidate();
end;

end.
