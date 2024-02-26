{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------

  Automatically save copies of editor contents to a zipped file.
}
unit simba.ide_scriptbackup;

{$i simba.inc}

interface

uses
  Classes, SysUtils, ExtCtrls,
  simba.base, simba.settings;

type
  TSimbaScriptBackup = class(TComponent)
  protected
    FTimer: TTimer;
    FFiles: array of record
      Name: String;
      Contents: String;
    end;

    procedure DoSettingChanged_BackupEnabled(Setting: TSimbaSetting);
    procedure DoSettingChanged_BackupInterval(Setting: TSimbaSetting);

    procedure DoFileCollecting(Sender: TObject);
    procedure DoFileBackuping;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  simba.zip, simba.files, simba.env, simba.ide_initialization, simba.scripttabsform, simba.threading, simba.hash;

function Crc32String(const Str: String): UInt32;
begin
  Result := StrToUInt('$' + HashString(EHashType.CRC32, Str));
end;

procedure TSimbaScriptBackup.DoFileCollecting(Sender: TObject);
var
  I: Integer;
begin
  CheckMainThread('TSimbaScriptBackup');

  SetLength(FFiles, SimbaScriptTabsForm.TabCount);
  for I := 0 to SimbaScriptTabsForm.TabCount - 1 do
  begin
    FFiles[I].Name := SimbaScriptTabsForm.Tabs[I].ScriptTitle;
    if (FFiles[I].Name = '') then
      FFiles[I].Name := 'Untitled';

    FFiles[I].Contents := SimbaScriptTabsForm.Tabs[I].Script;
  end;

  TThread.ExecuteInThread(@DoFileBackuping);
end;

procedure TSimbaScriptBackup.DoFileBackuping;
var
  I: Integer;
  ZipPath: String;
begin
  for I := 0 to High(FFiles) do
  try
    if (FFiles[I].Contents = '') then
      Continue;

    ZipPath := TSimbaPath.PathJoin([SimbaEnv.BackupsPath, FFiles[I].Name + '.zip']);
    if ZipHasEntryCrc(ZipPath, Crc32String(FFiles[I].Contents)) then
      Continue;

    if ZipAppend(ZipPath, '', FFiles[I].Contents) then
      DebugLn('Backed up %s', [FFiles[I].Name])
    else
      DebugLn('Failed to backup %s', [FFiles[I].Name]);
  except
    on E: Exception do
      DebugLn('Failed to backup "%s"', [E.Message]);
  end;
end;

procedure TSimbaScriptBackup.DoSettingChanged_BackupEnabled(Setting: TSimbaSetting);
begin
  FTimer.Enabled := SimbaSettings.ScriptBackup.Enabled.Value;
end;

procedure TSimbaScriptBackup.DoSettingChanged_BackupInterval(Setting: TSimbaSetting);
begin
  FTimer.Interval := SimbaSettings.ScriptBackup.Interval.Value * 60000;
end;

constructor TSimbaScriptBackup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FTimer := TTimer.Create(Self);
  FTimer.OnTimer := @DoFileCollecting;

  SimbaSettings.RegisterChangeHandler(Self, SimbaSettings.ScriptBackup.Enabled, @DoSettingChanged_BackupEnabled, True);
  SimbaSettings.RegisterChangeHandler(Self, SimbaSettings.ScriptBackup.Interval, @DoSettingChanged_BackupInterval, True);
end;

procedure SetupScriptBackup;
begin
  CheckMainThread('SetupScriptBackup');

  TSimbaScriptBackup.Create(SimbaScriptTabsForm);
end;

initialization
  SimbaIDEInitialization_AddBeforeShow(@SetupScriptBackup, 'Create Script Backup');

end.

