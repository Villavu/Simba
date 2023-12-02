unit simba.component_toolbar;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, ImgList,
  simba.mufasatypes, simba.component_button;

type
  TSimbaToolbar = class(TCustomControl)
  protected
    FFlowPanel: TFlowPanel;
    FImages: TImageList;
    FButtonSize: Integer;
    FVertical: Boolean;

    procedure DoGetImageWidth(Sender: TCustomImageList; AImageWidth, APPI: Integer; var AResultWidth: Integer);

    function GetSpacing: Integer;
    procedure SetSpacing(Value: Integer);

    procedure SetVertical(Value: Boolean);
    procedure SetButtonSize(Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;

    function AddButton(ImageIndex: Integer; HintText: String = ''; AOnClick: TNotifyEvent = nil): TSimbaTransparentButton;
    function AddDivider: TSimbaTransparentButton;

    property Spacing: Integer read GetSpacing write SetSpacing;
    property ImageWidth: Integer read FButtonSize write SetButtonSize;
    property Vertical: Boolean read FVertical write SetVertical;
  end;

implementation

uses
  simba.main, simba.theme, ATCanvasPrimitives;

type
  TSimbaToolButton = class(TSimbaTransparentButton)
  protected
    FToolbar: TSimbaToolBar;

    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer; WithThemeSpace: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TSimbaToolButtonDivider = class(TSimbaToolButton)
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer; WithThemeSpace: Boolean); override;
    procedure PaintBackground(var PaintRect: TRect); override;
  end;

procedure TSimbaToolButton.CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer; WithThemeSpace: Boolean);
begin
  PreferredWidth  := Round(FToolbar.FButtonSize * 1.5);
  PreferredHeight := Round(FToolbar.FButtonSize * 1.5);
end;

constructor TSimbaToolButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FToolBar := AOwner as TSimbaToolBar;

  BorderSpacing.Around := 2;
end;

procedure TSimbaToolButtonDivider.CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer; WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);

  if FToolbar.Vertical then
    PreferredHeight := 1
  else
    PreferredWidth := 1;
end;

procedure TSimbaToolButtonDivider.PaintBackground(var PaintRect: TRect);
begin
  Canvas.Brush.Color := ColorBlendHalf(SimbaTheme.ColorFrame, SimbaTheme.ColorLine);
  Canvas.FillRect(PaintRect);
end;

procedure TSimbaToolbar.SetButtonSize(Value: Integer);
var
  I: Integer;
begin
  if (Value = FButtonSize) then
    Exit;
  FButtonSize := Value;

  for I := 0 to FFlowPanel.ControlCount - 1 do
  begin
    FFlowPanel.Controls[I].InvalidatePreferredSize();
    FFlowPanel.Controls[I].AdjustSize();
  end;
end;

procedure TSimbaToolbar.SetVertical(Value: Boolean);
var
  I: Integer;
begin
  if (FVertical = Value) then
    Exit;
  FVertical := Value;

  for I := 0 to FFlowPanel.ControlCount - 1 do
  begin
    FFlowPanel.Controls[I].InvalidatePreferredSize();
    FFlowPanel.Controls[I].AdjustSize();
  end;
end;

procedure TSimbaToolbar.DoGetImageWidth(Sender: TCustomImageList; AImageWidth, APPI: Integer; var AResultWidth: Integer);
begin
  AResultWidth := FButtonSize;
end;

function TSimbaToolbar.GetSpacing: Integer;
begin
  Result := FFlowPanel.BorderSpacing.Around;
end;

procedure TSimbaToolbar.SetSpacing(Value: Integer);
begin
  FFlowPanel.BorderSpacing.Around := Scale96ToScreen(Value);
end;

function TSimbaToolbar.AddButton(ImageIndex: Integer; HintText: String; AOnClick: TNotifyEvent): TSimbaTransparentButton;
begin
  Result := TSimbaToolButton.Create(Self);
  Result.Parent := FFlowPanel;
  Result.Images := FImages;
  Result.ImageIndex := ImageIndex;
  Result.Hint := HintText;
  Result.ShowHint := HintText <> '';
  Result.OnClick := AOnClick;
end;

function TSimbaToolbar.AddDivider: TSimbaTransparentButton;
begin
  Result := TSimbaToolButtonDivider.Create(Self);
  Result.Parent := FFlowPanel;
end;

constructor TSimbaToolbar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FButtonSize := 24;

  FImages := TImageList.Create(Self); // Create a copy so ImagesGetWidthForPPI is not used for toolbar
  FImages.Assign(SimbaForm.Images);
  FImages.OnGetWidthForPPI := @DoGetImageWidth;

  FFlowPanel := TFlowPanel.Create(Self);
  FFlowPanel.Parent := Self;
  FFlowPanel.Align := alClient;
  FFlowPanel.BevelOuter := bvNone;

  Spacing := 3;

  AutoSize := True;
end;

end.

