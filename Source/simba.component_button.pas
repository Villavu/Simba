{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.component_button;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls, Buttons, Graphics, LMessages, ImgList,
  simba.base;

type
  {$push}
  {$scopedenums on}
  ESimbaButtonImage = (
    NONE,
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

type
  TSimbaButton = class(TCustomControl)
  protected
    FImageList: TCustomImageList;
    FImageIndex: Integer;
    FImage: ESimbaButtonImage;
    FImageSpacing: Integer;

    FDown: Boolean;
    FXPadding: Integer;
    FYPadding: Integer;

    FMeasureText: String;

    function HasImage: Boolean;
    function ImageSize: TSize;
    function CalculateSize: TPoint;

    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure TextChanged; override;

    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;

    // Use parent font size, but use SimbaComponentTheme.FontStyle and font styles if changed
    procedure CMParentFontChanged(var Message: TLMessage); message CM_PARENTFONTCHANGED;

    procedure SetDown(AValue: Boolean);
    procedure SetXPadding(AValue: Integer);
    procedure SetYPadding(AValue: Integer);
    procedure SetMeasureText(Value: String);
    procedure SetImageSpacing(AValue: Integer);
    procedure SetImage(Img: ESimbaButtonImage);
    procedure SetImageIndex(Index: Integer);
  public
    constructor Create(AOwner: TComponent); override;

    procedure Paint; override;
    procedure Click; override; // make click public

    property ImageList: TCustomImageList read FImageList write FImageList;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Image: ESimbaButtonImage read FImage write SetImage;
    property ImageSpacing: Integer read FImageSpacing write SetImageSpacing;
    property Down: Boolean read FDown write SetDown;
    property XPadding: Integer read FXPadding write SetXPadding;
    property YPadding: Integer read FYPadding write SetYPadding;

    property MeasureText: String read FMeasureText write SetMeasureText;

    property OnMouseDown;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
  end;

  TSimbaToggleButton = class(TSimbaButton)
  public
    procedure Click; override;
  end;

  TSimbaCheckButton = class(TSimbaToggleButton)
    constructor Create(AOwner: TComponent); override;
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

  TSimbaTransparentButton = class(TSimbaButton)
  public
    procedure Paint; override;
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

  TSimbaToggleButtonGroup = class(TFlowPanel)
  protected
    FOnChange: TNotifyEvent;

    function GetSelectedText: String;
    procedure DoButtonClick(Sender: TObject);
  public
    function Add(AText: String): TSimbaToggleButton;
    property SelectedText: String read GetSelectedText;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    constructor Create(AOwner: TComponent); override;
  end;

  TSimbaLabeledToggleButtonGroup = class(TCustomControl)
  protected
    FLabel: TLabel;
    FToggleButtons: TSimbaToggleButtonGroup;
    FLabelMeasure: String;

    procedure TextChanged; override;
    procedure SetLabelMeasure(Value: String);
  public
    constructor Create(AOwner: TComponent); override;

    property ToggleButtons: TSimbaToggleButtonGroup read FToggleButtons;
    property LabelMeasure: String read FLabelMeasure write SetLabelMeasure;
  end;

implementation

uses
  simba.component_theme, simba.component_images, simba.form_main, simba.misc, LCLType,
  ATCanvasPrimitives;

procedure TSimbaTransparentButton.Paint;
begin
  if Assigned(Parent) then
    Color := Parent.Color;

  inherited Paint();
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
  Color := SimbaComponentTheme.ColorFrame;
  Font.Color := SimbaComponentTheme.ColorFont;
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

  Result := GetControlIndex(CheckButton);
end;

function TSimbaToggleButtonGroup.GetSelectedText: String;
var i: Integer;
begin
  for i := 0 to ControlCount-1 do
    if (Controls[i] is TSimbaToggleButton) and TSimbaToggleButton(Controls[i]).Down then
      Exit(TSimbaToggleButton(Controls[i]).Caption);
  Result := '';
end;

procedure TSimbaToggleButtonGroup.DoButtonClick(Sender: TObject);
var i: Integer;
begin
  for i := 0 to ControlCount - 1 do
    if (Controls[i] is TSimbaToggleButton) and (Controls[i] <> Sender) then
      TSimbaToggleButton(Controls[i]).Down := False;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TSimbaToggleButtonGroup.Add(AText: String): TSimbaToggleButton;
begin
  Result := TSimbaToggleButton.Create(Self);
  with Result do
  begin
    Parent := Self;
    Caption := AText;
    BorderSpacing.Around := 3;
    OnClick := @DoButtonClick;
    Down := (Self.ControlCount = 1);
  end;
end;

constructor TSimbaToggleButtonGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  AutoSize := True;
  Color := SimbaComponentTheme.ColorBackground;
  BevelOuter := bvNone;
end;

procedure TSimbaLabeledToggleButtonGroup.SetLabelMeasure(Value: String);
begin
  if (FLabelMeasure = Value) then
    Exit;
  FLabelMeasure := Value;

  if (FLabelMeasure <> '') then
  begin
    FToggleButtons.AnchorSide[akLeft].Control := nil;

    with TBitmap.Create() do
    try
      Canvas.Font := FLabel.Font;
      FToggleButtons.Left := FLabel.Left + Canvas.TextWidth(FLabelMeasure) + (FLabel.BorderSpacing.Right * 2);
    finally
      Free();
    end;
  end else
    FToggleButtons.AnchorSide[akLeft].Control := FLabel;
end;

procedure TSimbaLabeledToggleButtonGroup.TextChanged;
begin
  inherited TextChanged;

  if Assigned(FLabel) then
    FLabel.Caption := Text;
end;

constructor TSimbaLabeledToggleButtonGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque];
  Color := SimbaComponentTheme.ColorFrame;
  Font.Color := SimbaComponentTheme.ColorFont;
  AutoSize := True;
  ParentFont := True;

  FLabel := TLabel.Create(Self);
  FLabel.Parent := Self;
  FLabel.Align := alLeft;
  FLabel.AutoSize := True;
  FLabel.Layout := tlCenter;
  FLabel.ParentFont := True;
  FLabel.BorderSpacing.Left := 5;
  FLabel.BorderSpacing.Right := 5;

  FToggleButtons := TSimbaToggleButtonGroup.Create(Self);
  FToggleButtons.Parent := Self;
  FToggleButtons.ParentColor := True;
  FToggleButtons.Anchors := [akLeft, akTop, akRight, akBottom];
  FToggleButtons.AnchorSide[akLeft].Control := FLabel;
  FToggleButtons.AnchorSide[akLeft].Side := asrRight;
  FToggleButtons.AnchorSide[akTop].Control := Self;
  FToggleButtons.AnchorSide[akTop].Side := asrTop;
  FToggleButtons.AnchorSide[akRight].Control := Self;
  FToggleButtons.AnchorSide[akRight].Side := asrRight;
  FToggleButtons.AnchorSide[akBottom].Control := Self;
  FToggleButtons.AnchorSide[akBottom].Side := asrBottom;
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
    Font.Color := SimbaComponentTheme.ColorFont;
    Font.EndUpdate();
  end;
end;

procedure TSimbaButton.SetDown(AValue: Boolean);
begin
  if FDown = AValue then Exit;
  FDown := AValue;
  Invalidate;
end;

procedure TSimbaButton.SetXPadding(AValue: Integer);
begin
  FXPadding := Scale96ToScreen(AValue);
  if FXPadding = AValue then
    Exit;

  AdjustSize();
end;

procedure TSimbaButton.SetYPadding(AValue: Integer);
begin
  FYPadding := Scale96ToScreen(AValue);
  if FYPadding = AValue then
    Exit;

  AdjustSize();
end;

procedure TSimbaButton.SetMeasureText(Value: String);
begin
  FMeasureText := Value;
  AdjustSize();
end;

procedure TSimbaButton.SetImageSpacing(AValue: Integer);
begin
  FImageSpacing := Scale96ToScreen(AValue);
  if FImageSpacing = AValue then
    Exit;

  AdjustSize();
end;

function TSimbaButton.HasImage: Boolean;
begin
  Result := (FImageIndex > -1);
end;

function TSimbaButton.ImageSize: TSize;
begin
  if (FImageIndex > -1) then
    Result := FImageList.SizeForPPI[FImageList.Width, Font.PixelsPerInch]
  else
    Result := TSize.Create(0, 0);
end;

function TSimbaButton.CalculateSize: TPoint;
var
  Str: String;
begin
  Result.X := 0;
  Result.Y := 0;

  Str := FMeasureText;
  if (Str = '') then
    Str := Caption;
  if (Str <> '') then
    with TBitmap.Create() do
    try
      Canvas.Font := Self.Font;

      Result.X := Canvas.TextWidth(Str) + (BorderWidth * 2);
      Result.Y := Canvas.TextHeight('Fj') + (BorderWidth * 2);
    finally
      Free();
    end;

  if HasImage then
  begin
    Result.X += FImageSpacing;
    Result.X += ImageSize.Width;
    if ImageSize.Height > Result.Y then
      Result.Y := ImageSize.Height;
  end;

  Result.X += XPadding * 2;
  Result.Y += YPadding * 2;
end;

procedure TSimbaButton.CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);

  with CalculateSize() do
  begin
    PreferredWidth := X;
    PreferredHeight := Y;
  end;
end;

procedure TSimbaButton.MouseEnter;
begin
  inherited MouseEnter;

  Invalidate();
end;

procedure TSimbaButton.MouseLeave;
begin
  inherited MouseLeave;

  Invalidate();
end;

procedure TSimbaButton.TextChanged;
begin
  inherited TextChanged;

  AdjustSize();
end;

procedure TSimbaButton.Paint;
var
  Style: TTextStyle;
  R: TRect;
  ImgPoint: TPoint;
begin
  if MouseInClient or FDown then
    Canvas.Brush.Color := SimbaComponentTheme.ColorActive
  else
    Canvas.Brush.Color := Color;

  Canvas.FillRect(ClientRect);
  CanvasPaintRoundedCorners(Canvas, ClientRect, [acckLeftTop, acckRightTop, acckLeftBottom, acckRightBottom], SimbaComponentTheme.ColorFrame, Canvas.Brush.Color, Canvas.Brush.Color);

  R := ClientRect;
  R.Left += XPadding;
  R.Right -= XPadding;
  R.Top += YPadding;
  R.Bottom -= YPadding;

  if HasImage then
  begin
    if (Caption = '') then
    begin
      ImgPoint.X := R.Left + (R.Width div 2)  - (ImageSize.Width div 2);
      ImgPoint.Y := R.Top  + (R.Height div 2) - (ImageSize.Height div 2);
    end else
    begin
      ImgPoint.X := R.Left;
      ImgPoint.Y := R.CenterPoint.Y - (ImageSize.Height div 2);
    end;

    FImageList.DrawForControl(Canvas, ImgPoint.X, ImgPoint.Y, FImageIndex, FImageList.Width, Self, Enabled);
  end;

  Style := Canvas.TextStyle;
  Style.Layout := tlCenter;
  Style.Alignment := taCenter;

  if (Caption <> '') then
  begin
    Canvas.Font.Color := SimbaComponentTheme.ColorFont;

    R := ClientRect;
    if HasImage then
      R.Left += ImageSize.Width + FImageSpacing;

    Canvas.TextRect(R, 0,0, Caption, Style);
  end;

  inherited Paint();
end;

procedure TSimbaButton.Click;
begin
  inherited Click();
end;

constructor TSimbaButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Color := SimbaComponentTheme.ColorScrollBarActive;
  AutoSize := True;

  FImageList := LCLGlyphs;
  FImageIndex := -1;

  ImageSpacing := 5;
  XPadding := 10;
  YPadding := 2;
end;

procedure TSimbaButton.SetImage(Img: ESimbaButtonImage);
begin
  FImageList := LCLGlyphs;
  case Img of
    ESimbaButtonImage.OK:           ImageIndex := LCLGlyphs.GetImageIndex('btn_ok');
    ESimbaButtonImage.CLOSE:        ImageIndex := LCLGlyphs.GetImageIndex('btn_cancel');
    ESimbaButtonImage.CLEAR_FILTER: ImageIndex := LCLGlyphs.GetImageIndex('btnfiltercancel');
    ESimbaButtonImage.SELECT_DIR:   ImageIndex := LCLGlyphs.GetImageIndex('btnseldir');
    ESimbaButtonImage.SELECT_FILE:  ImageIndex := LCLGlyphs.GetImageIndex('btnselfile');
    ESimbaButtonImage.TIME:         ImageIndex := LCLGlyphs.GetImageIndex('btntime');
    ESimbaButtonImage.CALC:         ImageIndex := LCLGlyphs.GetImageIndex('btncalculator');
    ESimbaButtonImage.CALENDER:     ImageIndex := LCLGlyphs.GetImageIndex('btncalendar');
  end;

  AdjustSize();
end;

procedure TSimbaButton.SetImageIndex(Index: Integer);
begin
  FImageIndex := Index;
  FImage := ESimbaButtonImage.NONE;

  AdjustSize();
end;

procedure TSimbaToggleButton.Click;
begin
  if Parent is TSimbaToggleButtonGroup then
    Down := True
  else
    Down := not Down;

  inherited Click();
end;

constructor TSimbaCheckButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ImageList := SimbaComponentImages;
  ImageIndex := SimbaComponentImages.TICK;

  YPadding := 3;
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
  Color := SimbaComponentTheme.ColorBackground;
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

end.

