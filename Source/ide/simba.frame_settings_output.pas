{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.frame_settings_output;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Spin, ExtCtrls, DividerBevel;

type
  TSimbaOutputBoxFrame = class(TFrame)
    AntiAliasingCheckbox: TCheckBox;
    ButtonReset: TButton;
    CheckBox1: TCheckBox;
    DividerBevel1: TDividerBevel;
    FontNameComboBox: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    FontSizeSpinEdit: TSpinEdit;
    Label5: TLabel;
    Panel1: TPanel;
    procedure AntiAliasingCheckboxChange(Sender: TObject);
    procedure ButtonResetClick(Sender: TObject);
    procedure FontNameComboBoxChange(Sender: TObject);
    procedure FontSizeSpinEditChange(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;

    procedure Load;
    procedure Save;
  end;

implementation

{$R *.lfm}

uses
  simba.settings, simba.fonthelpers;

procedure TSimbaOutputBoxFrame.FontSizeSpinEditChange(Sender: TObject);
begin
  SimbaSettings.OutputBox.FontSize.Value := TSpinEdit(Sender).Value;
end;

procedure TSimbaOutputBoxFrame.FontNameComboBoxChange(Sender: TObject);
begin
  SimbaSettings.OutputBox.FontName.Value := TComboBox(Sender).Text;
end;

procedure TSimbaOutputBoxFrame.AntiAliasingCheckboxChange(Sender: TObject);
begin
  SimbaSettings.OutputBox.FontAntiAliased.Value := TCheckBox(Sender).Checked;
end;

procedure TSimbaOutputBoxFrame.ButtonResetClick(Sender: TObject);
begin
  SimbaSettings.OutputBox.FontSize.SetDefault();
  SimbaSettings.OutputBox.FontName.SetDefault();
  SimbaSettings.OutputBox.FontAntiAliased.SetDefault();

  Load();
end;

constructor TSimbaOutputBoxFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FontNameComboBox.Items.AddStrings(GetFixedFonts());
end;

procedure TSimbaOutputBoxFrame.Load;
begin
  Checkbox1.Checked := SimbaSettings.OutputBox.ClearOnCompile.Value;

  AntiAliasingCheckbox.Checked := SimbaSettings.OutputBox.FontAntiAliased.Value;
  FontSizeSpinEdit.Value := SimbaSettings.OutputBox.FontSize.Value;
  FontNameComboBox.ItemIndex := FontNameComboBox.Items.IndexOf(SimbaSettings.OutputBox.FontName.Value);
end;

procedure TSimbaOutputBoxFrame.Save;
begin
  SimbaSettings.OutputBox.ClearOnCompile.Value := Checkbox1.Checked;
end;

end.

