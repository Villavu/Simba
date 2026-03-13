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
    FFrameColor: TColor;

    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer; WithThemeSpace: Boolean); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetTempColor(AColor: Integer);
    procedure SetZoom(PixelCount, PixelSize: Integer);
    procedure Move(ACanvas: TCanvas; X, Y: Integer);

    property FrameColor: TColor read FFrameColor write FFrameColor;
  end;

  // Zoom but with text on the right
  TSimbaImageBoxZoomPanel = class(TCustomControl)
  public type
    TTextEvent = function(Sender: TObject; Col: TColor; X, Y: Integer): String of object;
    TTextMeasureEvent = function(Sender: TObject): String of object;
  protected
    FZoom: TSimbaImageBoxZoom;
    FLabel: TLabel;
    FImageCanvas: TCanvas;
    FImageX, FImageY: Integer;
    FOnGetText: TTextEvent;
    FOnGetTextMeasure: TTextMeasureEvent;

    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer; WithThemeSpace: Boolean); override;
    procedure DoUpdate(Data: PtrInt);
    procedure DoFill(Data: PtrInt);
    function GetFrameColor: TColor;
    procedure SetFrameColor(AValue: TColor);
  public
    constructor Create(AOwner: TComponent); override;

    property OnGetText: TTextEvent read FOnGetText write FOnGetText;
    property OnGetTextMeasure: TTextMeasureEvent read FOnGetTextMeasure write FOnGetTextMeasure;
    property FrameColor: TColor read GetFrameColor write SetFrameColor;

    procedure Move(ImgCanvas: TCanvas; ImgX, ImgY: Integer);
    procedure Fill(AColor: TColor);
  end;

implementation

uses
  Forms,
  simba.nativeinterface,
  simba.colormath,
  simba.misc;

constructor TSimbaImageBoxZoom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FTempColor := -1;
  FBitmap := TBitmap.Create();
  FBitmap.Canvas.AntialiasingMode := amOff;
  FFrameColor := clBlack;

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

  Canvas.Pen.Color := FFrameColor;
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

function TSimbaImageBoxZoomPanel.GetFrameColor: TColor;
begin
  Result := FZoom.FrameColor;
end;

procedure TSimbaImageBoxZoomPanel.SetFrameColor(AValue: TColor);
begin
  FZoom.FrameColor := AValue;
end;

procedure TSimbaImageBoxZoomPanel.CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer; WithThemeSpace: Boolean);
var
  MeasureText: String;
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);

  if Assigned(FOnGetTextMeasure) then
    MeasureText := FOnGetTextMeasure(Self)
  else
    MeasureText := 'HSL: 360.00, 100.00, 100.00';

  with TBitmap.Create() do
  try
    Canvas.Font := Self.Font;
    Canvas.Font.Size := GetFontSize(Self, 2); // measure on slightly larger text for padding

    PreferredWidth := (FZoom.BorderSpacing.Around * 2) + FZoom.Width + Canvas.TextWidth(MeasureText) + FLabel.BorderSpacing.Right;
  finally
    Free();
  end;
end;

procedure TSimbaImageBoxZoomPanel.DoUpdate(Data: PtrInt);
var
  Col: TColor;
  HintText: String;
begin
  if (FImageCanvas <> nil) then
  begin
    Col := FImageCanvas.Pixels[FImageX, FImageY];

    if Assigned(FOnGetText) then
      HintText := FOnGetText(Self, Col, FImageX, FImageY)
    else
    begin
      with Col.ToRGB(), Col.ToHSL() do
        HintText := Format(
          'Color: %s' + LineEnding + 'RGB: %d, %d, %d' + LineEnding + 'HSL: %.2f, %.2f, %.2f',
          [ColorToStr(Col), R, G, B, H, S, L]
        );
    end;

    FZoom.Move(FImageCanvas, FImageX, FImageY);
    FLabel.Caption := HintText;
  end;
end;

procedure TSimbaImageBoxZoomPanel.DoFill(Data: PtrInt);
var
  Col: TColor absolute Data;
  HintText: String;
begin
  FZoom.SetTempColor(Col);

  if Assigned(FOnGetText) then
    HintText := FOnGetText(Self, Col, -1, -1)
  else
  begin
    with Col.ToRGB(), Col.ToHSL() do
      HintText := Format(
        'Color: %s' + LineEnding + 'RGB: %d, %d, %d' + LineEnding + 'HSL: %.2f, %.2f, %.2f',
        [ColorToStr(Col), R, G, B, H, S, L]
      );
  end;

  FLabel.Caption := HintText;
end;

constructor TSimbaImageBoxZoomPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  AutoSize := True;
  BorderSpacing.Around := 5;

  FZoom := TSimbaImageBoxZoom.Create(Self);
  FZoom.Parent := Self;
  FZoom.SetZoom(4, 5);
  FZoom.BorderSpacing.Right := 5;
  FZoom.Align := alLeft;

  FLabel := TLabel.Create(Self);
  FLabel.Parent := Self;
  FLabel.AnchorToNeighbour(akLeft, 10, FZoom);
  FLabel.AnchorVerticalCenterTo(FZoom);
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
