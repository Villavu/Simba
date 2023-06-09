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
    Olly: Boolean;

    constructor Create(AOwner: TComponent; LCLGlyphName: String); overload;
  end;

  TSimbaToggleButton = class(TSimbaButton)
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  simba.theme;

procedure TSimbaButton.CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);

  PreferredWidth += VertPadding*2;
end;

procedure TSimbaButton.PaintBackground(var PaintRect: TRect);
begin
  if Olly then
  begin
    if Down or MouseInClient then
      Canvas.Brush.Color := SimbaTheme.ColorActive
    else
      Canvas.Brush.Color := SimbaTheme.ColorBackground;

    Canvas.FillRect(PaintRect);

    Exit;
  end;

  Canvas.FillRect(PaintRect);
  Canvas.Pen.Color := SimbaTheme.ColorFrame;
  if Down or MouseInClient then
    Canvas.Brush.Color := SimbaTheme.ColorActive
  else
    Canvas.Brush.Color := SimbaTheme.ColorScrollBarActive;
  Canvas.Brush.Color := SimbaTheme.ColorBackground;

  Canvas.RoundRect(PaintRect, Width div 5, Width div 5);
end;

constructor TSimbaButton.Create(AOwner: TComponent; LCLGlyphName: String);
begin
  inherited Create(Owner);

  ButtonGlyph.LCLGlyphName := LCLGlyphName;
end;

constructor TSimbaToggleButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  AllowAllUp := True;
  GroupIndex := 1+AOwner.ComponentCount;

  Font.Color := SimbaTheme.ColorFont;
end;

end.

