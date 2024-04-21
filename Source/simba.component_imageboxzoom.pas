{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.component_imageboxzoom;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, Graphics, StdCtrls,
  simba.component_imagebox;

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
    procedure Move(ACanvas: TCanvas; X, Y: Integer);
  end;

  TSimbaImageBoxZoomPanel = class(TCustomControl)
  protected
    FZoom: TSimbaImageBoxZoom;
    FLabel: TLabel;

    FImageCanvas: TCanvas;
    FImageX, FImageY: Integer;

    procedure DoUpdate(Data: PtrInt);
    procedure DoFill(Data: PtrInt);
  public
    constructor Create(AOwner: TComponent); override;

    procedure Move(ImgCanvas: TCanvas; ImgX, ImgY: Integer);
    procedure Fill(AColor: TColor);
  end;

implementation

uses
  Forms,
  simba.nativeinterface, simba.colormath;

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
  if (AColor <> FTempColor) then
  begin
    FTempColor := AColor;

    Invalidate();
  end;
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
  if Odd(PixelCount) then
    FPixelCount := PixelCount
  else
    FPixelCount := PixelCount + 1;

  FPixelSize := PixelCount + PixelSize;

  AdjustSize();
end;

procedure TSimbaImageBoxZoom.Move(ACanvas: TCanvas; X, Y: Integer);
var
  LoopX, LoopY: Integer;
begin
  FTempColor := -1;

  Dec(X, FPixelCount div 2);
  Dec(Y, FPixelCount div 2);

  FBitmap.BeginUpdate(True);
  for LoopX := 0 to FBitmap.Width - 1 do
    for LoopY := 0 to FBitmap.Height - 1 do
      FBitmap.Canvas.Pixels[LoopX, LoopY] := ACanvas.Pixels[X + LoopX, Y + LoopY];
  FBitmap.EndUpdate();

  Invalidate();
end;

procedure TSimbaImageBoxZoomPanel.DoUpdate(Data: PtrInt);
var
  Col: TColor;
begin
  if (FImageCanvas <> nil) then
  begin
    Col := FImageCanvas.Pixels[FImageX, FImageY];

    FZoom.Move(FImageCanvas, FImageX, FImageY);
    with Col.ToRGB(), Col.ToHSL() do
      FLabel.Caption := Format('Color: %s', [ColorToStr(Col)])     + LineEnding +
                        Format('RGB: %d, %d, %d', [R, G, B])       + LineEnding +
                        Format('HSL: %.2f, %.2f, %.2f', [H, S, L]);
  end;
end;

procedure TSimbaImageBoxZoomPanel.DoFill(Data: PtrInt);
var
  Col: TColor absolute Data;
begin
  FZoom.SetTempColor(Col);
  with Col.ToRGB(), Col.ToHSL() do
    FLabel.Caption := Format('Color: %s', [ColorToStr(Col)])     + LineEnding +
                      Format('RGB: %d, %d, %d', [R, G, B])       + LineEnding +
                      Format('HSL: %.2f, %.2f, %.2f', [H, S, L]);
end;

constructor TSimbaImageBoxZoomPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  AutoSize := True;

  FZoom := TSimbaImageBoxZoom.Create(Self);
  FZoom.Parent := Self;
  FZoom.SetZoom(4, 5);
  FZoom.BorderSpacing.Around := 10;

  FLabel := TLabel.Create(Self);
  FLabel.Parent := Self;
  FLabel.BorderSpacing.Right := 10;
  FLabel.AnchorToNeighbour(akLeft, 10, FZoom);
end;

procedure TSimbaImageBoxZoomPanel.Move(ImgCanvas: TCanvas; ImgX, ImgY: Integer);
begin
  FImageCanvas := ImgCanvas;
  FImageX := ImgX;
  FImageY := ImgY;

  Application.RemoveAsyncCalls(Self);
  Application.QueueAsyncCall(@DoUpdate, 0);
end;

procedure TSimbaImageBoxZoomPanel.Fill(AColor: TColor);
begin
  Application.RemoveAsyncCalls(Self);
  Application.QueueAsyncCall(@DoFill, AColor);
end;

end.
