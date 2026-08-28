unit simba.component_divider;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls;

type
  TSimbaDivider = class(TCustomControl)
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  ATCanvasPrimitives,
  simba.component_theme;

procedure TSimbaDivider.Paint;
begin
  //Canvas.Brush.Color := SimbaComponentTheme.ColorScrollBarActive;
  //Canvas.FillRect(ClientRect);

  CanvasPaintRoundedCorners(Canvas, ClientRect, [acckLeftTop, acckRightTop, acckLeftBottom, acckRightBottom], SimbaComponentTheme.ColorFrame, Canvas.Brush.Color, Canvas.Brush.Color);
end;

constructor TSimbaDivider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque, csNoFocus];

  Height := 4;
  //BorderSpacing.Around := 5;
  Color := SimbaComponentTheme.ColorScrollBarActive;
end;

end.

