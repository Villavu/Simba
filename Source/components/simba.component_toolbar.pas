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
    function AddDropdownButton(ImageIndex: Integer; HintText: String = ''; AOnClick: TNotifyEvent = nil; APopupMenu: TPopupMenu = nil): TSimbaTransparentButton;
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
    function GetDropdownArrowRect: TRect;

    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer; WithThemeSpace: Boolean); override;
    procedure PaintBackground(var PaintRect: TRect); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
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

function TSimbaDropToolButton.GetDropdownArrowRect: TRect;
begin
  Result.Left := Width - ScaleToToolbarSize(10);
  Result.Right := Width - 2;
  Result.Top := 3;
  Result.Bottom := Height - 3;
end;

procedure TSimbaDropToolButton.CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer; WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);

  PreferredWidth := PreferredWidth + ScaleToToolbarSize(10);
  Margin := ScaleToToolbarSize(5);
end;

procedure TSimbaDropToolButton.PaintBackground(var PaintRect: TRect);
begin
  inherited PaintBackground(PaintRect);

  CanvasPaintTriangleDown(Canvas, SimbaTheme.ColorFont, GetDropdownArrowRect().CenterPoint, IfThen(FToolbar.FButtonSize >= 20, 2, 1));
  //Canvas.Pen.Color := 255;
  //Canvas.Frame(GetDropdownArrowRect());
end;

procedure TSimbaDropToolButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  FMouseInDropdownArrow := GetDropdownArrowRect().Contains(TPoint.Create(X, Y));
end;

procedure TSimbaDropToolButton.Click;
begin
  if FMouseInDropdownArrow then
  begin
    if (DropdownMenu <> nil) then
      with ClientToScreen(TPoint.Create(GetDropdownArrowRect().CenterPoint.X, Height)) do
        DropdownMenu.PopUp(X, Y);

    Exit;
  end;

  inherited Click();
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

function TSimbaToolbar.AddDropdownButton(ImageIndex: Integer; HintText: String; AOnClick: TNotifyEvent; APopupMenu: TPopupMenu): TSimbaTransparentButton;
begin
  Result := TSimbaDropToolButton.Create(Self);
  Result.Parent := FFlowPanel;
  Result.Images := FImages;
  Result.ImageIndex := ImageIndex;
  Result.Hint := HintText;
  Result.ShowHint := HintText <> '';
  Result.OnClick := AOnClick;

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

  Spacing := 3;

  AutoSize := True;
end;

end.

