unit simba.component_button;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Buttons;

type
  TSimbaButton = class(TSpeedButton)
  protected
    procedure PaintBackground(var PaintRect: TRect); override;
  end;

  TSimbaToggleButton = class(TSimbaButton)
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  simba.theme;

procedure TSimbaButton.PaintBackground(var PaintRect: TRect);
begin
  Canvas.Brush.Color := SimbaTheme.ColorFrame;
  Canvas.FillRect(PaintRect);

  Canvas.Pen.Color := SimbaTheme.ColorFrame;
  if Down or MouseInClient then
    Canvas.Brush.Color := SimbaTheme.ColorActive
  else
    Canvas.Brush.Color := SimbaTheme.ColorScrollBarActive;

  Canvas.RoundRect(PaintRect, Width div 5, Width div 5);
end;

constructor TSimbaToggleButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  AllowAllUp := True;
  GroupIndex := 1+AOwner.ComponentCount;

  Font.Color := SimbaTheme.ColorFont;
end;

end.

