{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_codetools_setup;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms,
  simba.base, simba.ide_codetools_parser;

var
  CodetoolsSetup: Boolean = False;
  CodetoolsKeywords: TDeclarationArray = nil;

implementation

uses
  simba.ide_codetools_insight,
  simba.threading,
  simba.initializations, simba.ide_utils, simba.env, simba.ide_events;

procedure DoCreate;

  procedure SetupKeywords;
  const
    Lape_Keywords: TStringArray = (
      'and', 'div', 'in', 'is', 'mod', 'not', 'or', 'shl', 'shr', 'xor', 'at', 'array',
      'begin', 'case', 'const', 'constref', 'deprecated', 'do', 'downto', 'else', 'end',
      'enum', 'except', 'experimental', 'external', 'finally', 'for', 'forward', 'function',
      'if', 'label', 'object', 'operator', 'of', 'out', 'overload', 'override', 'packed',
      'private','procedure', 'program', 'property', 'record', 'repeat', 'set', 'static',
      'strict', 'then', 'to', 'try', 'type', 'union','unimplemented', 'until', 'var', 'while', 'with'
    );
  var
    I: Integer;
  begin
    SetLength(CodetoolsKeywords, Length(Lape_Keywords));
    for I := 0 to High(Lape_Keywords) do
      CodetoolsKeywords[I] := TDeclaration_Keyword.Create(Lape_Keywords[I]);
  end;

  procedure SetupBaseParsers;
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
  end;

begin
  SetupKeywords();
  SetupBaseParsers();

  CodetoolsSetup := True;

  SimbaEvents.Post(ESimbaEvent.CODETOOLS_SETUP, nil);
end;

procedure DoDestroy;
var
  I: Integer;
begin
  for I := 0 to High(CodetoolsKeywords) do
    FreeAndNil(CodetoolsKeywords[I]);
  CodetoolsKeywords := nil;
end;

initialization
  SimbaInitialization_Add(ESimbaInit.IDE_BEFORE_SHOW_BACKGROUND, @DoCreate, 'CodeTools');
  SimbaInitialization_Add(ESimbaInit.IDE_DESTROY, @DoDestroy, 'CodeTools');

end.
