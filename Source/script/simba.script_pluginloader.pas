unit simba.script_pluginloader;

{$i simba.inc}

interface

uses
  Classes, SysUtils, dynlibs;

function LoadPlugin(FileName: String; ExtraSearchDirs: TStringArray = nil): TLibHandle;

implementation

uses
  simba.mufasatypes, simba.env;

function LoadPlugin(FileName: String; ExtraSearchDirs: TStringArray): TLibHandle;
begin
  if (not FindPlugin(FileName, [GetPluginPath(), GetSimbaPath()] + ExtraSearchDirs)) then
    raise Exception.CreateFmt('Unable to find plugin "%s"', [FileName]);

  Result := LoadLibrary(FileName);

  if (Result = NilHandle) then
  begin
    DebugLn('Loading plugin failed: ' + FileName);
    DebugLn('Error: ' + GetLoadErrorStr());

    raise Exception.Create('Loading plugin failed. Architecture mismatch? (expected a ' + {$IFDEF CPU32}'32'{$ELSE}'64'{$ENDIF} + ' bit plugin)');
  end;
end;

end.

