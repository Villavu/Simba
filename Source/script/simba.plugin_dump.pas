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

implementation

uses
  simba.script_plugin;

function DumpPlugin(Plugin: String): TStringList;
begin
  with TSimbaScriptPlugin.Create(Plugin) do
    Result := Dump();
end;

end.

