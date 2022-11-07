unit simba.ide_codetools_setup;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms;

implementation

uses
  simba.mufasatypes, simba.codeinsight,
  simba.ide_initialization, simba.ci_includecache, simba.process, simba.files, simba.functionlist_simbasection, simba.functionlistform;

procedure SetupCodeTools;
var
  List: TStringList;
  I: Integer;
  Parser: TCodeInsight_Include;
begin
  List := nil;

  try
    List := SimbaProcess.RunDump(HashFile(Application.ExeName), ['--dumpcompiler']);

    for I := 0 to List.Count - 1 do
    begin
      if (List.Names[I] = '') then
        Continue;

      Parser := TCodeInsight_Include.Create();
      Parser.Run(List.ValueFromIndex[I], List.Names[I]);

      TCodeInsight.AddBaseInclude(Parser);
    end;

    SimbaFunctionList_SimbaSection.Load(TCodeInsight.BaseIncludes);
    SimbaFunctionList_SimbaSection.Loaded := True;
  except
    on E: Exception do
      DebugLn('[TSimbaForm.SetupCodeTools]: ' + E.ToString());
  end;

  if (List <> nil) then
    List.Free();
end;

initialization
  SimbaIDEInitialization.RegisterMethodOnAfterCreate(@SetupCodeTools, 'CodeTools');

end.

