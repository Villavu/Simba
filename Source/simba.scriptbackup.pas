{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------

  Automatically save copies of editor contents to a zipped file.
}
unit simba.scriptbackup;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, ExtCtrls,
  simba.scripttabsform, simba.mufasatypes, simba.files,
  simba.zip, simba.ide_initialization, simba.settings, simba.threading, simba.env;

type
  TSimbaScriptBackup = class(TComponent)
  protected
    FTimer: TTimer;
    FFiles: array of record
      FileName: String;
      Contents: String;
    end;

    procedure DoFileCollecting(Sender: TObject);
    procedure DoFileBackuping;

    procedure DoSimbaSettingChanged(Setting: TSimbaSetting);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  LazFileUtils, Zipper,
  crc;

function Crc32String(const Str: String): UInt32;
begin
  Result := crc32(0, nil, 0);
  if (Str <> '') then
    Result := crc32(Result, @Str[1], Length(Str));
end;

procedure TSimbaScriptBackup.DoFileCollecting(Sender: TObject);
var
  I: Integer;
begin
  Assert(GetCurrentThreadID = MainThreadID);

  SetLength(FFiles, SimbaScriptTabsForm.TabCount);
  for I := 0 to SimbaScriptTabsForm.TabCount - 1 do
  begin
    FFiles[I].FileName := SimbaScriptTabsForm.Tabs[I].ScriptFileName;
    FFiles[I].Contents := SimbaScriptTabsForm.Tabs[I].Script;
  end;

  TThread.ExecuteInThread(@DoFileBackuping);
end;

procedure TSimbaScriptBackup.DoFileBackuping;
var
  I, J: Integer;
  ZipPath: String;
  Duplicate: Boolean;
begin
  for I := 0 to High(FFiles) do
  begin
    if (FFiles[I].FileName = '') then
      Continue;

    Duplicate := False;
    ZipPath := GetBackupsPath() + ExtractFileNameOnly(FFiles[I].FileName) + '.zip';
    try
      if FileExists(ZipPath) then
        with TUnZipper.Create() do
        try
          FileName := ZipPath;
          Examine();
          for J := 0 to Entries.Count - 1 do
            if (Entries[J].CRC32 = Crc32String(FFiles[I].Contents)) then
            begin
              Duplicate := True;
              Break;
            end;
        finally
          Free();
        end;

      if Duplicate then
        Continue;

      DebugLn('[TSimbaScriptBackup]: Making a backup of "%s"', [ExtractFileName(FFiles[I].FileName)]);

      with TSimbaZipUpdater.Create() do
      try
        AddFileContents(ZipPath, FFiles[I].Contents, FFiles[I].FileName);
      finally
        Free();
      end;
    except
      on E: Exception do
        DebugLn('[TSimbaScriptBackup]: Backup "%s" failed: %s ', [ExtractFileName(FFiles[I].FileName), E.Message]);
    end;
  end;
end;

procedure TSimbaScriptBackup.DoSimbaSettingChanged(Setting: TSimbaSetting);
begin
  if (Setting = SimbaSettings.ScriptBackup.Enabled) then
    FTimer.Enabled := SimbaSettings.ScriptBackup.Enabled.Value
  else
  if (Setting = SimbaSettings.ScriptBackup.Interval) then
    FTimer.Interval := SimbaSettings.ScriptBackup.Interval.Value * 60000;
end;

constructor TSimbaScriptBackup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FTimer := TTimer.Create(Self);
  FTimer.OnTimer  := @DoFileCollecting;
  FTimer.Interval := SimbaSettings.ScriptBackup.Interval.Value * 60000;
  FTimer.Enabled  := SimbaSettings.ScriptBackup.Enabled.Value;

  SimbaSettings.RegisterChangeHandler(@DoSimbaSettingChanged);
end;

destructor TSimbaScriptBackup.Destroy;
begin
  SimbaSettings.UnRegisterChangeHandler(@DoSimbaSettingChanged);

  inherited Destroy();
end;

procedure SetupScriptBackup;

  procedure Execute;
  begin
    TSimbaScriptBackup.Create(SimbaScriptTabsForm);
  end;

begin
  ExecuteOnMainThread(@Execute); // TTimer needs to be called on main thread
end;

initialization
  SimbaIDEInitialization.RegisterMethodOnAfterCreate(@SetupScriptBackup, 'ScriptBackup');

end.

