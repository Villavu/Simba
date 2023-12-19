{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.env;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes;

type
  SimbaEnv = class
  private
  class var
    FSimbaPath: String;
    FIncludesPath: String;
    FPluginsPath: String;
    FDataPath: String;
    FTempPath: String;
    FDumpsPath: String;
    FScriptsPath: String;
    FPackagesPath: String;
    FScreenshotsPath: String;
    FBackupsPath: String;
  public
    class constructor Create;

    class function FindPlugin(FileName: String; ExtraSearchDirs: TStringArray = nil): String;
    class function FindInclude(FileName: String; ExtraSearchDirs: TStringArray = nil): String;
    class function HasInclude(FileName: String; ExtraSearchDirs: TStringArray = nil): Boolean;
    class function HasPlugin(FileName: String; ExtraSearchDirs: TStringArray = nil): Boolean;

    class property SimbaPath: String read FSimbaPath;
    class property IncludesPath: String read FIncludesPath;
    class property PluginsPath: String read FPluginsPath;
    class property ScriptsPath: String read FScriptsPath;
    class property DataPath: String read FDataPath;
    class property TempPath: String read FTempPath;
    class property PackagesPath: String read FPackagesPath;
    class property DumpsPath: String read FDumpsPath;
    class property ScreenshotsPath: String read FScreenshotsPath;
    class property BackupsPath: String read FBackupsPath;
  end;

implementation

uses
  Forms,
  simba.files;

class function SimbaEnv.FindPlugin(FileName: String; ExtraSearchDirs: TStringArray): String;
const
  {$IF DEFINED(CPUAARCH64)}
  SimbaSuffix = SharedSuffix + '.aarch64'; // lib.aarch64
  {$ELSE}
  SimbaSuffix = {$IFDEF CPU32}'32'{$ELSE}'64'{$ENDIF} + '.' + SharedSuffix; // lib32.dll / lib64.dll
  {$ENDIF}
var
  SearchDir: String;
begin
  Result := '';

  if TSimbaFile.FileExists(FileName) then
    Exit(FileName);

  for SearchDir in ExtraSearchDirs + [PluginsPath, SimbaPath] do
  begin
    Result := TSimbaPath.PathJoin([SearchDir, FileName]);
    if TSimbaFile.FileExists(Result) then
      Exit(Result);

    Result := TSimbaPath.PathJoin([SearchDir, FileName]) + '.' + SharedSuffix;
    if TSimbaFile.FileExists(Result) then
      Exit(Result);

    Result := TSimbaPath.PathJoin([SearchDir, FileName]) + SimbaSuffix;
    if TSimbaFile.FileExists(Result) then
      Exit(Result);
  end;
end;

class function SimbaEnv.FindInclude(FileName: String; ExtraSearchDirs: TStringArray): String;
var
  SearchDir: String;
begin
  Result := '';

  if TSimbaFile.FileExists(FileName) then
    Exit(FileName);

  for SearchDir in ExtraSearchDirs + [IncludesPath, SimbaPath] do
  begin
    Result := TSimbaPath.PathJoin([SearchDir, FileName]);
    if TSimbaFile.FileExists(Result) then
      Exit(Result);

    Result := TSimbaPath.PathJoin([SearchDir, FileName]) + '.simba';
    if TSimbaFile.FileExists(Result) then
      Exit(Result);
  end;
end;

class function SimbaEnv.HasInclude(FileName: String; ExtraSearchDirs: TStringArray): Boolean;
begin
  Result := (FindInclude(FileName, ExtraSearchDirs) <> '');
end;

class function SimbaEnv.HasPlugin(FileName: String; ExtraSearchDirs: TStringArray): Boolean;
begin
  Result := (FindPlugin(FileName, ExtraSearchDirs) <> '');
end;

class constructor SimbaEnv.Create;

  function Init(Dir: String): String;
  begin
    if (not DirectoryExists(Dir)) then
      ForceDirectories(Dir);

    Result := IncludeTrailingPathDelimiter(Dir);
  end;

begin
  FSimbaPath := IncludeTrailingPathDelimiter(Application.Location);

  FIncludesPath    := Init(FSimbaPath + 'Includes');
  FPluginsPath     := Init(FSimbaPath + 'Plugins');
  FScriptsPath     := Init(FSimbaPath + 'Scripts');
  FScreenshotsPath := Init(FSimbaPath + 'Screenshots');
  FDataPath        := Init(FSimbaPath + 'Data');

  FDumpsPath    := Init(FDataPath + 'Dumps');
  FTempPath     := Init(FDataPath + 'Temp');
  FPackagesPath := Init(FDataPath + 'Packages');
  FBackupsPath  := Init(FDataPath + 'Backups');
end;

end.

