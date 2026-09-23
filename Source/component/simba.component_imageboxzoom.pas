{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.component_imageboxzoom;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Graphics, StdCtrls,
  simba.base, simba.image;

type
  TSimbaImageBoxZoom = class(TCustomControl)
  protected
    FBitmap: TBitmap;    // the magnified block, CellCount * CellSize square
    FCellCount: Integer; // cells per side, forced odd so there is a true centre
    FCellSize: Integer;  // screen pixels per cell
    FColor: TColor;      // clNone = zoom. anything else = solid color.
    FFrameColor: TColor;
    FCells: TColorArray; // what the gridc currently shows, so an update paints only what changed
    FCentreColor: TColor;

    procedure FillCell(Col, Row: Integer; AColor: TColor);
    function CentreOutlineColor: TColor;

    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer; WithThemeSpace: Boolean); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetZoom(ACellCount, ACellSize: Integer);
    procedure ShowColor(AColor: TColor);
    procedure ShowPixels(Img: TSimbaImage; X, Y: Integer);

    property FrameColor: TColor read FFrameColor write FFrameColor;
  end;

  TSimbaImageBoxZoomPanel = class(TCustomControl)
  public type
    TTextEvent = function(Sender: TObject; Col: TColor; X, Y: Integer): String of object;
    TTextMeasureEvent = function(Sender: TObject): String of object;
  protected
    FZoom: TSimbaImageBoxZoom;
    FLabel: TLabel;
    FImage: TSimbaImage;         // the source, and where in it, of the last Move
    FImageX, FImageY: Integer;
    FMeasuredText: String;       // the readout the width was last measured for ...
    FMeasuredWidth: Integer;     // ... and that width
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

    procedure Move(Img: TSimbaImage; ImgX, ImgY: Integer);
    procedure Fill(AColor: TColor);
  end;

implementation

uses
  Forms,
  simba.colormath;

function DefaultHintText(Col: TColor): String;
begin
  with Col.ToRGB(), Col.ToHSL() do
    Result := Format(
      'Color: %s' + LineEnding + 'RGB: %d, %d, %d' + LineEnding + 'HSL: %.2f, %.2f, %.2f',
      [ColorToStr(Col), R, G, B, H, S, L]
    );
end;

constructor TSimbaImageBoxZoom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FColor := clNone;
  FFrameColor := clBlack;
  FCentreColor := clLime;
  FBitmap := TBitmap.Create();

  SetZoom(5, 20);

  ParentColor := True;
  AutoSize := True;
end;

destructor TSimbaImageBoxZoom.Destroy;
begin
  FreeAndNil(FBitmap);

  inherited Destroy();
end;

procedure TSimbaImageBoxZoom.SetZoom(ACellCount, ACellSize: Integer);
begin
  FCellCount := ACellCount;
  if not Odd(FCellCount) then
    Inc(FCellCount); // an odd count leaves one cell exactly in the centre

  FCellSize := ACellSize;

  FCells := nil;
  SetLength(FCells, FCellCount * FCellCount);

  FBitmap.SetSize(FCellCount * FCellSize, FCellCount * FCellSize);
  AdjustSize();
end;

procedure TSimbaImageBoxZoom.CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer; WithThemeSpace: Boolean);
begin
  // the grid, plus one pixel of frame on each side
  PreferredWidth  := (FCellCount * FCellSize) + 2;
  PreferredHeight := (FCellCount * FCellSize) + 2;
end;

procedure TSimbaImageBoxZoom.ShowColor(AColor: TColor);
begin
  if (FColor <> AColor) then
  begin
    FColor := AColor;

    Invalidate();
  end;
end;

procedure TSimbaImageBoxZoom.FillCell(Col, Row: Integer; AColor: TColor);
begin
  FBitmap.Canvas.Brush.Color := AColor;
  FBitmap.Canvas.FillRect(Col * FCellSize, Row * FCellSize, (Col + 1) * FCellSize, (Row + 1) * FCellSize);
end;

function TSimbaImageBoxZoom.CentreOutlineColor: TColor;
begin
  // a channel under $80 has its top bit clear: that bit, moved to the bottom and
  // times $FF, is $FF for it and 0 for the others, all three bytes at once
  Result := (((not FCells[(FCellCount div 2) * FCellCount + (FCellCount div 2)]) and $808080) shr 7) * $FF;
end;

procedure TSimbaImageBoxZoom.ShowPixels(Img: TSimbaImage; X, Y: Integer);
var
  Col, Row, Cell: Integer;
  CellColor: TColor;
  Dirty: Boolean;
begin
  Dirty := (FColor <> clNone); // coming back from a solid color
  FColor := clNone;

  Dec(X, FCellCount div 2);
  Dec(Y, FCellCount div 2);

  Cell := 0;
  for Row := 0 to FCellCount - 1 do
    for Col := 0 to FCellCount - 1 do
    begin
      if Img.InImage(X + Col, Y + Row) then
        CellColor := Img.Pixel[X + Col, Y + Row]
      else
        CellColor := clBlack;

      // Only update if changed
      if (FCells[Cell] <> CellColor) then
      begin
        FCells[Cell] := CellColor;
        FillCell(Col, Row, CellColor);

        Dirty := True;
      end;
      Inc(Cell);
    end;

  if (not Dirty) then
    Exit;

  FCentreColor := CentreOutlineColor();

  Invalidate();
end;

procedure TSimbaImageBoxZoom.Paint;
var
  Centre: Integer;
begin
  if (FColor <> clNone) then
  begin
    Canvas.Pen.Color := clBlack;
    Canvas.Brush.Color := FColor;
    Canvas.Rectangle(ClientRect);

    Exit;
  end;

  Canvas.Draw(1, 1, FBitmap);
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := FFrameColor;
  Canvas.Frame(TRect.Create(0, 0, FCellCount * FCellSize + 2, FCellCount * FCellSize + 2));

  // outline the centre cell
  Centre := 1 + (FCellCount div 2) * FCellSize;
  Canvas.Pen.Color := FCentreColor;
  Canvas.Frame(TRect.Create(Centre, Centre, Centre + FCellSize, Centre + FCellSize));
  Canvas.Brush.Style := bsSolid;
end;

function TSimbaImageBoxZoomPanel.GetFrameColor: TColor;
begin
  Result := FZoom.FrameColor;
end;

procedure TSimbaImageBoxZoomPanel.SetFrameColor(AValue: TColor);
begin
  FZoom.FrameColor := AValue;
end;

constructor TSimbaImageBoxZoomPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  AutoSize := True;
  BorderSpacing.Around := 5;

  FZoom := TSimbaImageBoxZoom.Create(Self);
  FZoom.Parent := Self;
  FZoom.SetZoom(5, 18);
  FZoom.BorderSpacing.Right := 5;
  FZoom.AnchorParallel(akLeft, 0, Self);
  FZoom.AnchorVerticalCenterTo(Self);

  FLabel := TLabel.Create(Self);
  FLabel.Parent := Self;
  FLabel.AnchorToNeighbour(akLeft, 10, FZoom);
  FLabel.AnchorVerticalCenterTo(Self);
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

  if (MeasureText <> FMeasuredText) then
  begin
    FMeasuredText := MeasureText;

    with TBitmap.Create() do
    try
      Canvas.Font := Self.Font;

      FMeasuredWidth := Canvas.TextWidth(MeasureText) + Scale96ToScreen(8);
    finally
      Free();
    end;
  end;

  PreferredWidth := (FZoom.BorderSpacing.Around * 2) + FZoom.Width + FMeasuredWidth + FLabel.BorderSpacing.Right;
end;

procedure TSimbaImageBoxZoomPanel.DoUpdate(Data: PtrInt);
var
  Col: TColor;
  Readout: String;
begin
  if (FImage = nil) then
    Exit;

  if FImage.InImage(FImageX, FImageY) then
    Col := FImage.Pixel[FImageX, FImageY]
  else
    Col := clBlack;

  FZoom.ShowPixels(FImage, FImageX, FImageY);

  if Assigned(FOnGetText) then
    Readout := FOnGetText(Self, Col, FImageX, FImageY)
  else
    Readout := DefaultHintText(Col);

  if (Readout <> FLabel.Caption) then
    FLabel.Caption := Readout;
end;

procedure TSimbaImageBoxZoomPanel.DoFill(Data: PtrInt);
var
  Col: TColor;
begin
  Col := TColor(Data);

  FZoom.ShowColor(Col);

  if Assigned(FOnGetText) then
    FLabel.Caption := FOnGetText(Self, Col, -1, -1)
  else
    FLabel.Caption := DefaultHintText(Col);
end;

procedure TSimbaImageBoxZoomPanel.Move(Img: TSimbaImage; ImgX, ImgY: Integer);
begin
  FImage := Img;
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
