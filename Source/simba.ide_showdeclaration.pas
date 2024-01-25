unit simba.ide_showdeclaration;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base,
  simba.ide_codetools_parser, simba.ide_codetools_insight, simba.ide_codetools_includes;

procedure FindAndShowDeclaration(Script, ScriptFileName: String; CaretPos: Integer; What: String);

procedure ShowDeclaration(StartPos, EndPos, Line: Integer; FileName: String); overload;
procedure ShowDeclaration(Declaration: TDeclaration); overload;
procedure ShowSimbaDeclaration(Header: String; FileName: String);
procedure ShowPluginDeclaration(Header: String; FileName: String);

procedure ShowDeclarationDialog(Decls: TDeclarationArray);

implementation

uses
  simba.scripttabsform, simba.showdeclarationform;

procedure FindAndShowDeclaration(Script, ScriptFileName: String; CaretPos: Integer; What: String);
var
  Decl: TDeclaration;
  Codeinsight: TCodeinsight;
begin
  Codeinsight := TCodeinsight.Create();

  try
    Codeinsight.SetScript(Script, ScriptFileName, CaretPos);
    Codeinsight.Run();

    Decl := Codeinsight.ScriptParser.Items.GetByPosition(CaretPos);
    if (Decl is TDeclaration_IncludeDirective) then
    begin
      SimbaScriptTabsForm.Open(TDeclaration_IncludeDirective(Decl).FileName);
      Exit;
    end;

    Decl := Codeinsight.ParseExpression(What, []);
    if (Decl <> nil) then
    begin
      if (Decl.ClassType = TDeclaration_Method) and (Length(Codeinsight.GetOverloads(Decl)) > 1) then
        ShowDeclarationDialog(Codeinsight.GetOverloads(Decl))
      else
        ShowDeclaration(Decl);
    end;
  except
    on E: Exception do
      DebugLn('FindAndShowDeclaration: ' + E.ToString());
  end;

  Codeinsight.Free();
end;

procedure ShowDeclaration(StartPos, EndPos, Line: Integer; FileName: String);
begin
  if FileExists(FileName) then
    SimbaScriptTabsForm.Open(FileName);

  with SimbaScriptTabsForm.CurrentEditor do
  begin
    SelStart := StartPos;
    SelEnd := EndPos;
    TopLine := (Line + 1) - (LinesInWindow div 2);
    if CanSetFocus() then
      SetFocus();
  end;
end;

procedure ShowDeclaration(Declaration: TDeclaration);

  function GetDeclarationText: String;
  begin
    if (Declaration is TDeclaration_Method) then
      Result := TDeclaration_Method(Declaration).HeaderString
    else
    if (Declaration is TDeclaration_Var) then
      Result := TDeclaration_Var(Declaration).Name + TDeclaration_Var(Declaration).VarTypeString + TDeclaration_Var(Declaration).VarDefaultString
    else
      Result := Declaration.Text;
  end;

begin
  if (Declaration.Parser is TCodetoolsPlugin) then
  begin
    DebugLn([EDebugLn.FOCUS], 'Declared internally in plugin: %s', [Declaration.Lexer.FileName]);
    DebugLn([EDebugLn.FOCUS], GetDeclarationText());

    Exit;
  end;

  if (Declaration.Lexer.FileName = '') or FileExists(Declaration.Lexer.FileName) then
  begin
    if FileExists(Declaration.Lexer.FileName) then
      SimbaScriptTabsForm.Open(Declaration.Lexer.FileName);

    with SimbaScriptTabsForm.CurrentEditor do
    begin
      SelStart := Declaration.StartPos;
      SelEnd := Declaration.EndPos;
      TopLine := (Declaration.Line + 1) - (LinesInWindow div 2);
      if CanSetFocus() then
        SetFocus();
    end;

    Exit;
  end;

  DebugLn([EDebugLn.FOCUS], 'Declared internally in Simba: %s', [Declaration.Lexer.FileName]);
  DebugLn([EDebugLn.FOCUS], GetDeclarationText());
end;

procedure ShowSimbaDeclaration(Header: String; FileName: String);
begin
  if (Header = '') then
    Exit;

  DebugLn([EDebugLn.FOCUS], 'Declared internally in Simba: %s', [FileName]);
  DebugLn([EDebugLn.FOCUS], 'Declaration: %s', [Header]);
end;

procedure ShowPluginDeclaration(Header: String; FileName: String);
begin
  if (Header = '') then
    Exit;

  DebugLn([EDebugLn.FOCUS], 'Declared internally in plugin: %s', [FileName]);
  DebugLn([EDebugLn.FOCUS], 'Declaration: %s', [Header]);
end;

procedure ShowDeclarationDialog(Decls: TDeclarationArray);
begin
  with TShowDeclarationForm.Create(nil) do
  try
    Execute(Decls);
  finally
    Free();
  end;
end;

end.

