{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_codetools_setup;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms;

var
  CodetoolsSetup: Boolean = False;

implementation

uses
  simba.base, simba.ide_codetools_parser, simba.ide_codetools_insight,
  simba.ide_initialization, simba.ide_utils, simba.env, simba.ide_events;

procedure SetupCodeTools;
var
  List: TStringList;
  I: Integer;
  Parser: TCodeParser;
begin
  List := nil;

  try
    List := RunDump(Application.ExeName, ['--dumpcompiler']);

    for I := 0 to List.Count - 1 do
    begin
      if (List.Names[I] = '') then
        Continue;

      Parser := TCodeParser.Create();
      Parser.SetScript(List.ValueFromIndex[I], List.Names[I]);
      Parser.Run();

      TCodeinsight.AddBaseParser(Parser);
    end;
  except
    on E: Exception do
      DebugLn('[TSimbaForm.SetupCodeTools]: ' + E.ToString());
  end;

  if (List <> nil) then
    List.Free();

  CodetoolsSetup := True;

  SimbaIDEEvents.Notify(SimbaIDEEvent.CODETOOLS_SETUP, nil);
end;

initialization
  SimbaIDEInitialization_AddBeforeShow(@SetupCodeTools, 'Setup CodeTools', True);

end.
