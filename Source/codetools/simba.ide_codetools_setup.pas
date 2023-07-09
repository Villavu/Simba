unit simba.ide_codetools_setup;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms;

var
  CodetoolsSetup: Boolean = False;

implementation

uses
  simba.mufasatypes, simba.ide_codetools_parser, simba.ide_codetools_insight,
  simba.ide_initialization, simba.process, simba.env, simba.ide_events;

procedure SetupCodeTools;
var
  List: TStringList;
  I: Integer;
  Parser: TCodeParser;
begin
  List := nil;

  try
    List := SimbaProcess.RunDump(HashFile(Application.ExeName), ['--dumpcompiler']);

    for I := 0 to List.Count - 1 do
    begin
      if (List.Names[I] = '') then
        Continue;

      Parser := TCodeParser.Create();
      Parser.SetScript(List.ValueFromIndex[I], List.Names[I]);
      Parser.Run();

      TCodeinsight.AddBaseInclude(Parser);
    end;
  except
    on E: Exception do
      DebugLn('[TSimbaForm.SetupCodeTools]: ' + E.ToString());
  end;

  if (List <> nil) then
    List.Free();

  CodetoolsSetup := True;

  SimbaIDEEvents.CallOnCodetoolsSetup(nil);
end;

initialization
  SimbaIDEInitialization.RegisterMethodOnAfterCreate(@SetupCodeTools, 'CodeTools');

end.
