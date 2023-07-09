{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.settingsform_backup;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Spin, ExtCtrls, DividerBevel;

type
  TSimbaBackupFrame = class(TFrame)
    BackupIntervalLabel: TLabel;
    BackupEnabledCheckbox: TCheckBox;
    DividerBevel1: TDividerBevel;
    Label1: TLabel;
    Label2: TLabel;
    LabelBackupDirectory: TLabel;
    LabelBackup: TLabel;
    BackupIntervalEdit: TSpinEdit;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Load;
    procedure Save;
  end;

implementation

{$R *.lfm}

uses
  simba.env, simba.settings;

constructor TSimbaBackupFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  LabelBackupDirectory.Caption := GetBackupsPath();
end;

procedure TSimbaBackupFrame.Load;
begin
  BackupEnabledCheckbox.Checked := SimbaSettings.ScriptBackup.Enabled.Value;
  BackupIntervalEdit.Value := SimbaSettings.ScriptBackup.Interval.Value;
end;

procedure TSimbaBackupFrame.Save;
begin
  SimbaSettings.ScriptBackup.Enabled.Value := BackupEnabledCheckbox.Checked;
  SimbaSettings.ScriptBackup.Interval.Value := BackupIntervalEdit.Value;
end;

end.

