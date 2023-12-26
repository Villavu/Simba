{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.component_toolbar;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, Buttons, ImgList, Menus,
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
    function AddDropdownButton(HintText: String = ''; APopupMenu: TPopupMenu = nil): TSimbaTransparentButton;
    function AddDivider: TSimbaTransparentButton;

    property Spacing: Integer read GetSpacing write SetSpacing;
    property ImageWidth: Integer read FButtonSize write SetButtonSize;
    property Vertical: Boolean read FVertical write SetVertical;
  end;

implementation

uses
  simba.main, simba.theme,
  ATCanvasPrimitives;

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

  TSimbaDropToolButton = class(TSimbaToolButton)
  protected
    FMouseInDropdownArrow: Boolean;

    function ScaleToToolbarSize(Value: Integer): Integer;

    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer; WithThemeSpace: Boolean); override;
    procedure PaintBackground(var PaintRect: TRect); override;
  public
    DropdownMenu: TPopupMenu;

    procedure Click; override;
  end;

function TSimbaDropToolButton.ScaleToToolbarSize(Value: Integer): Integer;
begin
  if (FToolbar.FButtonSize > 24) then
    Result := Round(Value * 2)
  else
  if (FToolBar.FButtonSize > 16) then
    Result := Round(Value * 1.5)
  else
    Result := Value;
end;

procedure TSimbaDropToolButton.CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer; WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);

  PreferredWidth := ScaleToToolbarSize(10);
end;

procedure TSimbaDropToolButton.PaintBackground(var PaintRect: TRect);
begin
  inherited PaintBackground(PaintRect);

  CanvasPaintTriangleDown(Canvas, SimbaTheme.ColorFont, PaintRect.CenterPoint, IfThen(FToolbar.FButtonSize >= 20, 2, 1));
end;

procedure TSimbaDropToolButton.Click;
begin
  inherited Click();

  if (DropdownMenu <> nil) then
    with ClientToScreen(TPoint.Create(0,Height)) do
      DropDownMenu.Popup(X, Y);
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

  BorderSpacing.Left := 2;
  BorderSpacing.Right := 2;
  BorderSpacing.Top := 2;
  BorderSpacing.Bottom := 2;
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

function TSimbaToolbar.AddDropdownButton(HintText: String; APopupMenu: TPopupMenu): TSimbaTransparentButton;
begin
  Result := TSimbaDropToolButton.Create(Self);
  Result.Parent := FFlowPanel;
  Result.Hint := HintText;
  Result.ShowHint := HintText <> '';

  TSimbaDropToolButton(Result).DropdownMenu := APopupMenu;
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

  FImages := TImageList.Create(Self);
  FImages.Assign(SimbaForm.Images); // Create a copy so OnImagesGetWidthForPPI isn't used globally
  FImages.OnGetWidthForPPI := @DoGetImageWidth;

  FFlowPanel := TFlowPanel.Create(Self);
  FFlowPanel.Parent := Self;
  FFlowPanel.Align := alClient;
  FFlowPanel.BevelOuter := bvNone;

  ControlStyle := ControlStyle + [csOpaque];
  AutoSize := True;
  Spacing := 3;
  Color := SimbaTheme.ColorFrame;
end;

end.

