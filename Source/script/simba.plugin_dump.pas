{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Loads a plugin to gather declarations.
}
unit simba.plugin_dump;

{$i simba.inc}

interface

uses
  Classes, SysUtils;

function DumpPlugin(Plugin: String): TStringList;
function DumpPluginInAnotherProcess(FileName: String): String;

implementation

uses
  simba.base, simba.process, simba.script_plugin;

function DumpPlugin(Plugin: String): TStringList;
begin
  with TSimbaScriptPlugin.Create(Plugin) do
    Result := Dump();
end;

// Calls above but in another Simba process so it's "safe"
function DumpPluginInAnotherProcess(FileName: String): String;
var
  List: TStringList;
begin
  Result := '';

  List := nil;
  try
    List := SimbaProcess.RunDump(FileName, ['--dumpplugin=' + FileName]);

    Result := List.Text;
  except
    on E: Exception do
      DebugLn(E.Message);
  end;

  if (List <> nil) then
    List.Free();
end;

end.

