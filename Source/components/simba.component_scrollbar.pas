{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.component_scrollbar;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  ATScrollBar, simba.settings;

type
  TSimbaScrollBar = class(TATScrollBar)
  protected
    procedure DoSettingChange_ScrollBarSize(Setting: TSimbaSetting);
    procedure DoSettingChange_ScrollBarArrowSize(Setting: TSimbaSetting);
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  simba.mufasatypes;

procedure TSimbaScrollBar.DoSettingChange_ScrollBarSize(Setting: TSimbaSetting);
begin
  Theme^.InitialSize := Scale96ToScreen(Setting.Value);

  Update();
end;

procedure TSimbaScrollBar.DoSettingChange_ScrollBarArrowSize(Setting: TSimbaSetting);
begin
  Theme^.ArrowSize := Setting.Value;

  Update();
end;

constructor TSimbaScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  SimbaSettings.RegisterChangeHandler(Self, SimbaSettings.General.ScrollBarSize, @DoSettingChange_ScrollBarSize, True);
  SimbaSettings.RegisterChangeHandler(Self, SimbaSettings.General.ScrollBarArrowSize, @DoSettingChange_ScrollBarArrowSize, True);
end;

end.

