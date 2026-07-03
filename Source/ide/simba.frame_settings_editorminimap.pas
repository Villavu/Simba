unit simba.frame_settings_editorminimap;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Spin;

type
  TEditorMinimapFrame = class(TFrame)
    CheckboxEnabled: TCheckBox;
    CheckboxDocked: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    EditWidth: TSpinEdit;
    EditFontSize: TSpinEdit;
  public
    procedure Load;
    procedure Save;
  end;

implementation

uses
  simba.settings;

procedure TEditorMinimapFrame.Load;
begin
  CheckboxEnabled.Checked := SimbaSettings.MiniMap.Enabled.Value;
  CheckboxDocked.Checked := SimbaSettings.MiniMap.Docked.Value;
  EditWidth.Value := SimbaSettings.MiniMap.Width.Value;
  EditFontSize.Value := SimbaSettings.MiniMap.FontSize.Value;
end;

procedure TEditorMinimapFrame.Save;
begin
  SimbaSettings.MiniMap.Enabled.Value := CheckboxEnabled.Checked;
  SimbaSettings.MiniMap.Docked.Value := CheckboxDocked.Checked;
  SimbaSettings.MiniMap.Width.Value := EditWidth.Value;
  SimbaSettings.MiniMap.FontSize.Value := EditFontSize.Value;
end;

{$R *.lfm}

end.

