program parser_proc_directive;

{$mode objfpc}{$H+}
{ define {$DEFINE ci_STANDALONE} in v_ideCodeInsight }

uses
  Classes, Windows, Interfaces,
  v_Constants, v_ideCodeInsight, v_ideCodeParser, v_MiscFunctions;

var
  ci: TCodeInsight;
  ms: TMemoryStream;
  s: TStringList;
  i, j: Integer;

  ProcDecl: TciProcedureDeclaration;
begin
  ci := TCodeInsight.Create();
  ms := TMemoryStream.Create();

  s := TStringList.Create;
  s.Add('function Foo(): Boolean; overload;'      + LineEnding +
        'var'                                     + LineEnding +
        '  i: Integer'                            + LineEnding +
        'begin'                                   + LineEnding +
        '  i := 50;'                              + LineEnding +
        'end;'                                    + LineEnding +
        'procedure Writeln(); override;'          + LineEnding +
        'var'                                     + LineEnding +
        '  i: Integer'                            + LineEnding +
        'begin'                                   + LineEnding +
        '  i := 50;'                              + LineEnding +
        'end;'                                    + LineEnding +
        'procedure TTypeMethod.Foo(); overload;'  + LineEnding +
        'var'                                     + LineEnding +
        '  x: Integer'                            + LineEnding +
        'begin'                                   + LineEnding +
        '  x := 50;'                              + LineEnding +
        'end;'                                    + LineEnding +
        'procedure TTypeMethod.Moo();'            + LineEnding +
        'var'                                     + LineEnding +
        '  x: Integer'                            + LineEnding +
        'begin'                                   + LineEnding +
        '  x := 50;'                              + LineEnding +
        'end;'                                    + LineEnding +
        'begin'                                   + LineEnding +
        'end.');

  s.SaveToStream(ms);
  ci.Run(ms);

  for i := 0 to ci.Items.Count - 1 do
    if (ci.Items[i] is TciProcedureDeclaration) then
    begin
      ProcDecl := TciProcedureDeclaration(ci.Items[i]);
      Writeln('Scanning ', ProcDecl.ProcType, ' ', ProcDecl.Name.RawText);

      if (ProcDecl.TypeName = '') then
        Writeln(' - No type attached')
      else
        Writeln(' - Type attached: ', ProcDecl.TypeName);

      Writeln(' -  Override = ', ProcDecl.isOverride);
      Writeln(' -  Overload = ', ProcDecl.isOverload);
    end;

  Windows.MessageBox(0, PChar(''), PChar(''), 0);

  ms.Free();
  ci.Free();
end.

