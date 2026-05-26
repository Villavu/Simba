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
    FTabNames: TStringArray;
    FTabContents: TStringArray;

    procedure DoSettingChanged_BackupEnabled(Setting: TSimbaSetting);
    procedure DoSettingChanged_BackupInterval(Setting: TSimbaSetting);

    procedure DoFileCollecting(Sender: TObject);
    procedure DoFileBackuping;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  SimbaScriptBackup: TSimbaScriptBackup;

implementation

uses
  simba.zip, simba.fs, simba.env, simba.initializations, simba.threading, simba.hash,
  simba.ide_controller;

procedure TSimbaScriptBackup.DoFileCollecting(Sender: TObject);
begin
  SimbaController.GetTabContents(FTabNames, FTabContents);

  TThread.ExecuteInThread(@DoFileBackuping);
end;

procedure TSimbaScriptBackup.DoFileBackuping;
var
  I: Integer;
  ZipPath: String;
  TabName, TabContents: String;
begin
  for I := 0 to High(FTabContents) do
  try
    if (FTabContents[I] = '') then
      Continue;

    TabName := FTabNames[I];
    TabContents := FTabContents[I];

    ZipPath := TSimbaPath.PathJoin([SimbaEnv.BackupsPath, TabName + '.zip']);
    if ZipHasEntryCrc(ZipPath, CRC32(@TabContents, Length(TabContents))) then
      Continue;

    if ZipAppend(ZipPath, '', TabContents) then
      DebugLn('Backed up %s', [TabName])
    else
      DebugLn('Failed to backup %s', [TabName]);
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

procedure DoCreate;
begin
  SimbaScriptBackup := TSimbaScriptBackup.Create(nil);
end;

procedure DoDestroy;
begin
  FreeAndNil(SimbaScriptBackup);
end;

initialization
  SimbaInitialization_Add(ESimbaInit.IDE_BEFORE_SHOW, @DoCreate, 'ScriptBackup');
  SimbaInitialization_Add(ESimbaInit.IDE_DESTROY, @DoDestroy, 'ScriptBackup');

end.

