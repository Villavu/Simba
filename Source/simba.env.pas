{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.env;

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.mufasatypes;

  function FindInclude(var FileName: String; ExtraSearchDirs: TStringArray): Boolean;
  function FindPlugin(var FileName: String; ExtraSearchDirs: TStringArray): Boolean;
  procedure CopyPlugin(var FileName: String);

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

    class function WriteTempFile(const Contents, Prefix: String): String;

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

function FindFile(var FileName: string; Ext: String; const SearchPaths: array of String): Boolean;
var
  I: Int32;
begin
  Result := False;

  if FileExists(FileName) then
  begin
    FileName := ExpandFileName(FileName);

    Exit(True);
  end;

  for I := 0 to High(SearchPaths) do
    if FileExists(IncludeTrailingPathDelimiter(SearchPaths[I]) + FileName + Ext) then
    begin
      FileName := ExpandFileName(IncludeTrailingPathDelimiter(SearchPaths[I]) + FileName + Ext);

      Exit(True);
    end;
end;

function FindInclude(var FileName: String; ExtraSearchDirs: TStringArray): Boolean;
begin
  Result := FindFile(FileName, '', ExtraSearchDirs + [SimbaEnv.IncludesPath, SimbaEnv.SimbaPath]);
end;

function FindPlugin(var FileName: String; ExtraSearchDirs: TStringArray): Boolean;
const
  {$IF DEFINED(CPUAARCH64)}
  // lib.aarch64
  SimbaSuffix = SharedSuffix + '.aarch64';
  {$ELSE}
  // lib32.dll
  // lib64.dll
  SimbaSuffix = {$IFDEF CPU32}'32'{$ELSE}'64'{$ENDIF} + '.' + SharedSuffix;
  {$ENDIF}
begin
  ExtraSearchDirs := ExtraSearchDirs + [SimbaEnv.PluginsPath, SimbaEnv.SimbaPath];

  Result := FindFile(FileName, '',                 ExtraSearchDirs) or
            FindFile(FileName, '.' + SharedSuffix, ExtraSearchDirs) or
            FindFile(FileName, SimbaSuffix,        ExtraSearchDirs);
end;

// Make a copy of the plugin to data/plugins/ so we can delete/update if it's loaded
procedure CopyPlugin(var FileName: String);
var
  NewFileName: String;
begin
  NewFileName := SimbaEnv.TempPath + TSimbaFile.FileHash(FileName) + TSimbaPath.PathExtractExt(FileName);
  if TSimbaFile.FileExists(NewFileName) or TSimbaFile.FileCopy(FileName, NewFileName) then
    FileName := NewFileName;
end;

class function SimbaEnv.WriteTempFile(const Contents, Prefix: String): String;
var
  Number: Integer = 0;
begin
  Result := Format('%s%s.%d', [SimbaEnv.TempPath, Prefix, Number]);
  while FileExists(Result) do
  begin
    Inc(Number);

    Result := Format('%s%s.%d', [SimbaEnv.TempPath, Prefix, Number]);
  end;

  TSimbaFile.FileWrite(Result, Contents);
end;

class constructor SimbaEnv.Create;

  function Setup(Dir: String): String;
  begin
    if (not DirectoryExists(Dir)) then
      ForceDirectories(Dir);

    Result := IncludeTrailingPathDelimiter(Dir);
  end;

begin
  FSimbaPath := IncludeTrailingPathDelimiter(Application.Location);

  FIncludesPath    := Setup(FSimbaPath + 'Includes');
  FPluginsPath     := Setup(FSimbaPath + 'Plugins');
  FScriptsPath     := Setup(FSimbaPath + 'Scripts');
  FScreenshotsPath := Setup(FSimbaPath + 'Screenshots');
  FDataPath        := Setup(FSimbaPath + 'Data');

  FDumpsPath    := Setup(FDataPath + 'Dumps');
  FTempPath     := Setup(FDataPath + 'Temp');
  FPackagesPath := Setup(FDataPath + 'Packages');
  FBackupsPath  := Setup(FDataPath + 'Backups');
end;

end.

