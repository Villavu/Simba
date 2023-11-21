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

  function FindInclude(var FileName: String; ExtraSearchDirs: TStringArray): Boolean;

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

