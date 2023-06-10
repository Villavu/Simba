{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.component_button;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Buttons;

type
  TSimbaButton = class(TSpeedButton)
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
    procedure PaintBackground(var PaintRect: TRect); override;
  public
    VertPadding: Integer;

    constructor Create(AOwner: TComponent); override;

    procedure SetCloseGlyph;
    procedure SetClearFilterGlyph;
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

implementation

uses
  simba.theme,
  ATCanvasPrimitives;

procedure TSimbaTransparentButton.PaintBackground(var PaintRect: TRect);
begin
  if Down or MouseInClient then
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

procedure TSimbaButton.CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);

  PreferredWidth += VertPadding * 2;
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

  VertPadding := 5;
end;

procedure TSimbaButton.SetCloseGlyph;
begin
  ButtonGlyph.LCLGlyphName := 'btn_cancel';
end;

procedure TSimbaButton.SetClearFilterGlyph;
begin
  ButtonGlyph.LCLGlyphName := 'btnfiltercancel';
end;

constructor TSimbaToggleButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  AllowAllUp := True;
  GroupIndex := 1 + AOwner.ComponentCount;

  Font.Color := SimbaTheme.ColorFont;
end;

end.

