{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Handles locating and loading script plugins.
}
unit simba.script_pluginloader;

{$i simba.inc}

interface

uses
  Classes, SysUtils;

function FindPlugin(var FileName: String; ExtraSearchDirs: TStringArray = nil): Boolean;
function LoadPlugin(var FileName: String; ExtraSearchDirs: TStringArray = nil): TLibHandle;

var
  LoadedPlugins: array of record
    OrginalFileName: String;
    FileName: String;
    Handle: TLibHandle;
  end;

implementation

uses
  simba.mufasatypes, simba.files, simba.env;

function FindPlugin(var FileName: String; ExtraSearchDirs: TStringArray): Boolean;
const
  {$IF DEFINED(CPUAARCH64)}
  SimbaSuffix = SharedSuffix + '.aarch64'; // lib.aarch64
  {$ELSE}
  SimbaSuffix = {$IFDEF CPU32}'32'{$ELSE}'64'{$ENDIF} + '.' + SharedSuffix; // lib32.dll / lib64.dll
  {$ENDIF}
var
  OrginalFileName: String;
  SearchDir: String;
begin
  Result := False;

  OrginalFileName := FileName;
  if TSimbaFile.FileExists(OrginalFileName) then
    Exit(True);

  for SearchDir in ExtraSearchDirs + [SimbaEnv.PluginsPath, SimbaEnv.SimbaPath] do
  begin
    FileName := TSimbaPath.PathJoin([SearchDir, OrginalFileName]);
    if TSimbaFile.FileExists(FileName) then
      Exit(True);

    FileName := TSimbaPath.PathJoin([SearchDir, OrginalFileName]) + '.' + SharedSuffix;
    if TSimbaFile.FileExists(FileName) then
      Exit(True);

    FileName := TSimbaPath.PathJoin([SearchDir, OrginalFileName]) + SimbaSuffix;
    if TSimbaFile.FileExists(FileName) then
      Exit(True);
  end;

  FileName := OrginalFileName;
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

function LoadPlugin(var FileName: String; ExtraSearchDirs: TStringArray): TLibHandle;
var
  OrginalFileName: String;
  I: Integer;
begin
  if (not FindPlugin(FileName, ExtraSearchDirs)) then
    SimbaException('Unable to find plugin "%s"', [FileName]);

  OrginalFileName := FileName;
  for I := 0 to High(LoadedPlugins) do
    if (LoadedPlugins[I].OrginalFileName = OrginalFileName) then
    begin
      Result := LoadedPlugins[I].Handle;
      Exit;
    end;

  {$IFDEF WINDOWS}
  CopyPlugin(FileName);
  {$ENDIF}

  Result := LoadLibrary(FileName);

  if (Result = NilHandle) then
  begin
    DebugLn('Loading plugin failed: ' + FileName);
    DebugLn('Error: ' + GetLoadErrorStr());

    SimbaException('Loading plugin failed. Architecture mismatch? (expected a ' + {$IFDEF CPU32}'32'{$ELSE}'64'{$ENDIF} + ' bit plugin)');
  end;

  SetLength(LoadedPlugins, Length(LoadedPlugins) + 1);
  LoadedPlugins[High(LoadedPlugins)].OrginalFileName := OrginalFileName;
  LoadedPlugins[High(LoadedPlugins)].FileName := FileName;
  LoadedPlugins[High(LoadedPlugins)].Handle := Result;
end;

end.

