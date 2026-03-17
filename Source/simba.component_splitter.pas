{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Custom drawn splitter
}
unit simba.component_splitter;

{$i simba.inc}

interface

uses
  Classes, SysUtils, ExtCtrls;

type
  TSimbaSplitter = class(TSplitter)
    procedure Paint; override;
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

uses
  simba.component_theme;

procedure TSimbaSplitter.Paint;
begin
  Canvas.Brush.Color := SimbaComponentTheme.ColorFrame;
  Canvas.FillRect(ClientRect);

  if MouseInClient then
  begin
    Canvas.Brush.Color := SimbaComponentTheme.ColorActive;
    Canvas.FillRect(3, 3, Width-3, Height-3);
  end;
end;

constructor TSimbaSplitter.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Width := Scale96ToScreen(7);
end;

end.

