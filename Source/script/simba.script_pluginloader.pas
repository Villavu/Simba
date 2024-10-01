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
  Classes, SysUtils,
  simba.base;

function LoadPlugin(var FileName: String; ExtraSearchDirs: TStringArray = nil): TLibHandle;

function FindLoadedPlugin(FileName: String): String;
function IsPluginLoaded(FileName: String): Boolean;
function GetLoadedPlugins: TStringArray;

implementation

uses
  simba.fs, simba.env;

var
  LoadedPlugins: array of record
    OrginalFileName: String;
    FileName: String;
    Handle: TLibHandle;
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
  OrginalFileName := FileName;

  FileName := SimbaEnv.FindPlugin(FileName, ExtraSearchDirs);
  if (FileName = '') then
    SimbaException('Plugin "%s" not found', [OrginalFileName]);

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

function FindLoadedPlugin(FileName: String): String;
var
  I: Integer;
begin
  Result := '';

  for I := 0 to High(LoadedPlugins) do
    if (LoadedPlugins[I].OrginalFileName = FileName) then
      Exit(LoadedPlugins[I].FileName);
end;

function IsPluginLoaded(FileName: String): Boolean;
begin
  Result := FindLoadedPlugin(FileName) <> '';
end;

function GetLoadedPlugins: TStringArray;
var
  i: Integer;
begin
  SetLength(Result, Length(LoadedPlugins));
  for I := 0 to High(LoadedPlugins) do
    Result[I] := LoadedPlugins[i].OrginalFileName;
end;

end.

