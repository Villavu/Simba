{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.component_button;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Buttons, Graphics, LMessages;

type
  {$push}
  {$scopedenums on}
  ESimbaButtonImage = (
    OK,
    CLOSE,
    CLEAR_FILTER,
    SELECT_DIR,
    SELECT_FILE,
    TIME,
    CALC,
    CALENDER
  );
  {$pop}

const
  IMG_TO_LAZ_GLYPH: array[ESimbaButtonImage] of String = (
    'btn_ok',
    'btn_cancel',
    'btnfiltercancel',
    'btnseldir',
    'btnselfile',
    'btntime',
    'btncalculator',
    'btncalendar'
  );

type
  TSimbaButton = class(TSpeedButton)
  protected
    // Use parent font size, but use SimbaTheme.FontStyle and font styles if changed
    procedure CMParentFontChanged(var Message: TLMessage); message CM_PARENTFONTCHANGED;

    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
    procedure PaintBackground(var PaintRect: TRect); override;
  public
    XPadding: Integer;
    YPadding: Integer;

    constructor Create(AOwner: TComponent); override;

    procedure SetImage(Img: ESimbaButtonImage);
  end;

  TSimbaLabeledButton = class(TCustomControl)
  protected
    FLabel: TLabel;
    FButton: TSimbaButton;

    procedure TextChanged; override;
  public
    constructor Create(AOwner: TComponent); override;

    property Button: TSimbaButton read FButton;
  end;

  TSimbaToggleButton = class(TSimbaButton)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TSimbaTransparentButton = class(TSimbaButton)
  protected
    procedure PaintBackground(var PaintRect: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TSimbaCheckButton = class(TSimbaToggleButton)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TSimbaLabeledCheckButton = class(TCustomControl)
  protected
    FLabel: TLabel;
    FCheckButton: TSimbaCheckButton;

    procedure TextChanged; override;
  public
    constructor Create(AOwner: TComponent); override;

    property CheckButton: TSimbaCheckButton read FCheckButton;
  end;

  TSimbaCheckButtonGroup = class(TCustomControl)
  protected
    function GetSelected(Index: Integer): Boolean;
    procedure SetSelected(Index: Integer; Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;

    function Add(ACaption: String): Integer;

    property Checked[Index: Integer]: Boolean read GetSelected write SetSelected;
  end;

implementation

uses
  simba.theme, simba.main,
  ATCanvasPrimitives;

procedure TSimbaTransparentButton.PaintBackground(var PaintRect: TRect);
begin
  if Enabled and (Down or MouseInClient) then
    Canvas.Brush.Color := SimbaTheme.ColorActive
  else
    Canvas.Brush.Color := Color;

  Canvas.FillRect(PaintRect);
  if Down or MouseInClient then
    CanvasPaintRoundedCorners(Canvas, PaintRect, [acckLeftTop, acckRightTop, acckLeftBottom, acckRightBottom], Color, Canvas.Brush.Color, Canvas.Brush.Color);
end;

constructor TSimbaTransparentButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ParentColor := True;
end;

constructor TSimbaCheckButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  XPadding := 0;
  Images := SimbaForm.Images;
  PressedImageIndex := 62;
  SelectedImageIndex := 62;
end;

procedure TSimbaLabeledCheckButton.TextChanged;
begin
  inherited TextChanged();

  if Assigned(FLabel) then
    FLabel.Caption := Text;
end;

constructor TSimbaLabeledCheckButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque];
  Color := SimbaTheme.ColorFrame;
  Font.Color := SimbaTheme.ColorFont;
  AutoSize := True;
  ParentFont := True;

  FLabel := TLabel.Create(Self);
  FLabel.Parent := Self;
  FLabel.Align := alLeft;
  FLabel.AutoSize := True;
  FLabel.Layout := tlCenter;
  FLabel.ParentFont := True;

  FCheckButton := TSimbaCheckButton.Create(Self);
  FCheckButton.Parent := Self;

  FCheckButton.Anchors := [akLeft];
  FCheckButton.AnchorSideLeft.Control := FLabel;
  FCheckButton.AnchorSideLeft.Side := asrRight;
  FCheckButton.AnchorVerticalCenterTo(FLabel);

  FCheckButton.BorderSpacing.Top := 5;
  FCheckButton.BorderSpacing.Bottom := 5;
  FCheckButton.BorderSpacing.Right := 5;
  FCheckButton.BorderSpacing.Left := 8;
end;

procedure TSimbaCheckButtonGroup.SetSelected(Index: Integer; Value: Boolean);
begin
  if (Controls[Index] is TSimbaLabeledCheckButton) then
    TSimbaLabeledCheckButton(Controls[Index]).CheckButton.Down := Value;
end;

function TSimbaCheckButtonGroup.GetSelected(Index: Integer): Boolean;
begin
  if (Controls[Index] is TSimbaLabeledCheckButton) then
    Result := TSimbaLabeledCheckButton(Controls[Index]).CheckButton.Down
  else
    Result := False;
end;

constructor TSimbaCheckButtonGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  AutoSize := True;
end;

function TSimbaCheckButtonGroup.Add(ACaption: String): Integer;
var
  CheckButton: TSimbaLabeledCheckButton;
begin
  CheckButton := TSimbaLabeledCheckButton.Create(Self);
  CheckButton.Parent := Self;
  CheckButton.Caption := ACaption;
  CheckButton.Align := alBottom;

  Result := Self.GetControlIndex(CheckButton);
end;

procedure TSimbaButton.CMParentFontChanged(var Message: TLMessage);
var
  OldStyle: TFontStyles;
begin
  OldStyle := Font.Style;

  inherited;

  if Assigned(Parent) then
  begin
    Font.BeginUpdate();
    Font := Parent.Font;
    Font.Style := OldStyle;
    Font.Color := SimbaTheme.ColorFont;
    Font.EndUpdate();
  end;
end;

procedure TSimbaButton.CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);

  PreferredWidth  += XPadding * 2;
  PreferredHeight += YPadding * 2;
end;

procedure TSimbaButton.PaintBackground(var PaintRect: TRect);
begin
  if Down or MouseInClient then
    Canvas.Brush.Color := SimbaTheme.ColorActive
  else
    Canvas.Brush.Color := SimbaTheme.ColorScrollBarActive;

  Canvas.FillRect(PaintRect);
  CanvasPaintRoundedCorners(Canvas, PaintRect, [acckLeftTop, acckRightTop, acckLeftBottom, acckRightBottom], SimbaTheme.ColorFrame, Canvas.Brush.Color, Canvas.Brush.Color);
end;

constructor TSimbaButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  AutoSize := True;
  XPadding := 5;
end;

procedure TSimbaButton.SetImage(Img: ESimbaButtonImage);
begin
  ButtonGlyph.LCLGlyphName := IMG_TO_LAZ_GLYPH[Img];
end;

procedure TSimbaLabeledButton.TextChanged;
begin
  inherited TextChanged();

  if Assigned(FLabel) then
    FLabel.Caption := Text;
end;

constructor TSimbaLabeledButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque];
  Color := SimbaTheme.ColorBackground;
  AutoSize := True;

  FButton := TSimbaButton.Create(Self);
  FButton.AutoSize := True;
  FButton.Parent := Self;
  FButton.Align := alLeft;

  FLabel := TLabel.Create(Self);
  FLabel.Parent := Self;
  FLabel.AutoSize := True;
  FLabel.Align := alClient;
  FLabel.Layout := tlCenter;
end;

constructor TSimbaToggleButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  AllowAllUp := True;
  GroupIndex := 1 + AOwner.ComponentCount;
end;

end.

