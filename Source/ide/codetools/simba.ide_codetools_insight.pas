{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  The main class for code insight used for things like:

    - AutoComplete
    - ParamHint
    - FunctionList
    - Jump To Declaration
}
unit simba.ide_codetools_insight;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base,
  simba.containers,
  simba.ide_codetools_paslexer,
  simba.ide_codetools_parser;

type
  TCodeinsight = class(TObject)
  protected
  class var
    FBaseParsers: TCodeParserList;
    FBaseSymbolTable: TSymbolTable;
  protected
    FIncludeParsers: TCodeParserList;
    FPluginParsers: TCodeParserList;
    FScriptParser: TCodeParser;

    FIncludedFiles: TStringList;
    FSymbolTable: TSymbolTable;
    FLocalSymbolTable: TSymbolTable;

    class function Hash(Parsers: TCodeParserList): String;

    procedure BuildLocalSymbols;

    procedure DoHandleInclude(Sender: TPasLexer);
    procedure DoHandlePlugin(Sender: TPasLexer);

    procedure Reset;

    function GetIncludesHash: String;
    function GetPluginsHash: String;
  public
    class procedure AddBaseParser(Parser: TCodeParser);
    class property BaseParsers: TCodeParserList read FBaseParsers;
    class constructor Create;
    class destructor Destroy;

    property IncludeParsers: TCodeParserList read FIncludeParsers;
    property IncludesHash: String read GetIncludesHash;

    property PluginParsers: TCodeParserList read FPluginParsers;
    property PluginsHash: String read GetPluginsHash;

    property ScriptParser: TCodeParser read FScriptParser;

    property SymbolTable: TSymbolTable read FSymbolTable;

    procedure SetScript(Script: String; FileName: String; CaretPos: Integer = -1);
    procedure Run;

    function GetLocals: TDeclarationArray; // Local, types, methods
    function GetGlobals: TDeclarationArray; // Global declarations from all parsers

    function ResolveVarType(Decl: TDeclaration): TDeclaration_Type;

    function Get(Name: String): TDeclarationArray;
    function GetTypeMembers(Decl: TDeclaration_Type; Methods: Boolean = True): TDeclarationArray;

    function ParseExpr(Expr: String; out Members: TDeclarationArray): TDeclaration; overload;
    function ParseExpr(Expr: String): TDeclaration; overload;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  simba.settings,
  simba.ide_codetools_includes,
  simba.ide_codetools_exprparser,
  simba.ide_codetools_arrayhelpers;

function TCodeinsight.GetIncludesHash: String;
begin
  Result := TCodeinsight.Hash(FIncludeParsers);
end;

function TCodeinsight.GetPluginsHash: String;
begin
  Result := TCodeinsight.Hash(FPluginParsers);
end;

class function TCodeinsight.Hash(Parsers: TCodeParserList): String;
var
  Builder: TSimbaStringBuilder;
  I: Integer;
begin
  for I := 0 to Parsers.Count - 1 do
    Builder.Append(Parsers[I].Hash);

  Result := Builder.Str;
end;

procedure TCodeinsight.BuildLocalSymbols;
var
  Locals: TDeclarationArray;

  procedure CheckMethod(Decl: TDeclaration);
  var
    SelfDecl, ResultDecl, ParamListDecl: TDeclaration;
  begin
    if (Decl = nil) then
      Exit;
    SelfDecl := nil;
    ResultDecl := nil;

    Locals.Add(Decl.Items.GetByClass(TDeclaration_Method));

    while (Decl is TDeclaration_Method) do
    begin
      if (SelfDecl = nil) then
      begin
        SelfDecl := Decl.Items.GetByClassFirst(TDeclaration_MethodObjectName, True);
        if (SelfDecl <> nil) then
          Locals.Add(SelfDecl);
      end;
      if (ResultDecl = nil) then
      begin
        ResultDecl := Decl.Items.GetByClassFirst(TDeclaration_MethodResult, True);
        if (ResultDecl <> nil) then
          Locals.Add(ResultDecl);
      end;

      Locals.Add(Decl.Items.GetByClass(TDeclaration_Var, True));
      Locals.Add(Decl.Items.GetByClass(TDeclaration_Const, True));
      Locals.Add(Decl.Items.GetByClass(TDeclaration_Type));
      Locals.Add(Decl.Items.GetByClass(TDeclaration_EnumElement));

      ParamListDecl := Decl.Items.GetByClassFirst(TDeclaration_ParamList, True, False);
      if Assigned(ParamListDecl) then
        Locals.Add(ParamListDecl.Items.GetByClass(TDeclaration_Parameter, True, True));

      Decl := Decl.ParentByClass[TDeclaration_Method];
    end;
  end;

  procedure CheckWith(Decl: TDeclaration);
  begin
    if (Decl = nil) then
      Exit;

    while (Decl is TDeclaration_WithStatement) do
    begin
      if (Decl.Items.GetByClassFirst(TDeclaration_WithVariableList) <> nil) then
        Locals.Add(Decl.Items.GetByClassFirst(TDeclaration_WithVariableList).Items.GetByClass(TDeclaration_WithVariable));

      Decl := Decl.ParentByClass[TDeclaration_WithStatement];
    end;
  end;

var
  Decl: TDeclaration;
  I: Integer;
begin
  FLocalSymbolTable.Clear();

  if (FScriptParser.CaretPos > -1) then
  begin
    Decl := FScriptParser.Items.GetByPosition(FScriptParser.CaretPos);
    if (Decl = nil) then
      Exit;

    Locals := [];

    if (Decl is TDeclaration_Method) then
      CheckMethod(Decl)
    else
      CheckMethod(Decl.ParentByClass[TDeclaration_Method]);

    if (Decl is TDeclaration_WithStatement) then
      CheckWith(Decl)
    else
      CheckWith(Decl.ParentByClass[TDeclaration_WithStatement]);

    for I := 0 to High(Locals) do
    begin
      if (Locals[I].Name <> '') then
        FLocalSymbolTable.Add(Locals[I].Name, Locals[I])
      else
      if (Locals[I] is TDeclaration_WithVariable) then // resolve the with vartype and add the symbols
      begin
        Decl := ParseExpr(Locals[I].Text);

        if (Decl is TDeclaration_Var) then
          Decl := ResolveVarType(TDeclaration_Var(Decl).VarType)
        else
        if (Decl is TDeclaration_Method) and (TDeclaration_Method(Decl).ResultType <> nil) then
          Decl := ResolveVarType(TDeclaration_Method(Decl).ResultType);

        if (Decl is TDeclaration_Type) then
          FLocalSymbolTable.Add(GetTypeMembers(Decl as TDeclaration_Type));
      end;
    end;
  end;
end;

procedure TCodeinsight.DoHandleInclude(Sender: TPasLexer);
var
  Parser: TCodeParser;
  Plugin: String;
begin
  Parser := CodetoolsIncludes.GetInclude(Sender, FIncludedFiles);

  if Assigned(Parser) then
  begin
    FIncludeParsers.Add(Parser);

    for Plugin in TCodetoolsPlugin(Parser).Plugins do
    begin
      Parser := CodetoolsIncludes.GetPlugin(Plugin);
      if Assigned(Parser) then
        FPluginParsers.Add(Parser);
    end;
  end;
end;

procedure TCodeinsight.DoHandlePlugin(Sender: TPasLexer);
var
  Parser: TCodeParser;
begin
  Parser := CodetoolsIncludes.GetPlugin(Sender);
  if Assigned(Parser) then
    FPluginParsers.Add(Parser);
end;

procedure TCodeinsight.Reset;
begin
  CodetoolsIncludes.Release(FIncludeParsers);
  CodetoolsIncludes.Release(FPluginParsers);

  FSymbolTable.Clear();
  FLocalSymbolTable.Clear();

  FIncludedFiles.Clear();
  FIncludeParsers.Clear();
  FPluginParsers.Clear();
  FScriptParser.Reset();
end;

class procedure TCodeinsight.AddBaseParser(Parser: TCodeParser);
begin
  FBaseParsers.Add(Parser);

  BuildSymbolTable(FBaseSymbolTable, Parser);
end;

class constructor TCodeinsight.Create;
begin
  FBaseParsers := TCodeParserList.Create(True);
end;

class destructor TCodeinsight.Destroy;
begin
  if Assigned(FBaseParsers) then
    FreeAndNil(FBaseParsers);
end;

procedure TCodeinsight.SetScript(Script: String; FileName: String; CaretPos: Integer);
begin
  Reset();

  FScriptParser.SetScript(Script, FileName);
  FScriptParser.CaretPos := CaretPos;
  if SimbaSettings.CodeTools.IgnoreIDEDirective.Value then
    FScriptParser.Lexer.AddDefine('!CODETOOLS');
end;

procedure TCodeinsight.Run;
var
  Parser: TCodeParser;
begin
  FScriptParser.Run();

  FSymbolTable := FBaseSymbolTable.Copy();
  for Parser in FIncludeParsers.ToArray() + FPluginParsers.ToArray() do
    FSymbolTable.CopyFrom(Parser.SymbolTable);

  BuildSymbolTable(FSymbolTable, FScriptParser);
  BuildLocalSymbols();
end;

function TCodeinsight.GetGlobals: TDeclarationArray;

  function IsGlobal(const Decl: TDeclaration): Boolean; inline;
  begin
    Result := (Decl.ClassType = TDeclaration_Method) or
              (Decl.ClassType = TDeclaration_Var) or
              (Decl.ClassType = TDeclaration_Const) or
              (Decl.ClassType = TDeclaration_EnumElement) or
              (Decl is TDeclaration_Type);
  end;

var
  I, J, Count: Integer;
  Decl: TDeclaration;
begin
  Count := 0;
  SetLength(Result, FSymbolTable.TotalCount);
  for I := 0 to FSymbolTable.Count - 1 do
    for J := 0 to FSymbolTable.Items[I].Count - 1 do
    begin
      Decl := FSymbolTable.Items[I].Decls[J];
      if IsGlobal(Decl) then
      begin
        Result[Count] := FSymbolTable.Items[I].Decls[J];
        Inc(Count);
      end;
    end;
  SetLength(Result, Count);
end;

function TCodeinsight.GetLocals: TDeclarationArray;
var
  i: Integer;
begin
  Result := [];
  for i := 0 to FLocalSymbolTable.Count - 1 do
    Result := Result + Copy(FLocalSymbolTable.Items[i].Decls, 0, FLocalSymbolTable.Items[i].Count);
end;

function TCodeinsight.ResolveVarType(Decl: TDeclaration): TDeclaration_Type;
var
  Decls: TDeclarationArray;
  I: Integer;
begin
  Result := nil;

  if (Decl is TDeclaration_VarType) and (Decl.Items.Count > 0) then
    Decl := Decl.Items[0];

  if (Decl is TDeclaration_TypeNativeMethod) and (Decl.Items.Count > 0) then
    Exit(ResolveVarType(Decl.Items[0]));

  if (Decl is TDeclaration_Type) then
    Exit(Decl as TDeclaration_Type);

  if (Decl is TDeclaration_Identifier) and (Decl.Name <> '') then
  begin
    Decls := Get(Decl.Name);
    for I := 0 to High(Decls) do
      if (Decls[I] is TDeclaration_Type) then
        Exit(ResolveVarType(Decls[I] as TDeclaration_Type));
  end;
end;

function TCodeinsight.Get(Name: String): TDeclarationArray;
begin
  Result := FLocalSymbolTable.Get(Name);
  if (Length(Result) = 0) then
    Result := FSymbolTable.Get(Name);
end;

function TCodeinsight.GetTypeMembers(Decl: TDeclaration_Type; Methods: Boolean): TDeclarationArray;

  function GetParent(Decl: TDeclaration): TDeclaration_Type;
  begin
    if (Decl is TDeclaration_TypeCopy)  then Result := ResolveVarType(TDeclaration_TypeCopy(Decl).VarType)  else
    if (Decl is TDeclaration_TypeAlias) then Result := ResolveVarType(TDeclaration_TypeAlias(Decl).VarType) else
      Result := nil;

    if (Result = Decl) or (not (Result is TDeclaration_Type)) then
      Result := nil;
  end;

var
  Depth, I: Integer;
  Decls: TDeclarationArray;
begin
  Result := GetArrayHelpers(Decl); // start with (possible) array helpers

  Depth := 0;
  while (Decl <> nil) and (Depth < 20) do // max depth of 20 for safety
  begin
    Inc(Depth);
    if Methods then
    begin
      Decls := FSymbolTable.Get(Decl.Name).GetByClass(TDeclaration_MethodOfType);

      if (Depth > 1) then // dont add static parents of a parent type
      begin
        for I := 0 to High(Decls) do
          if not TDeclaration_MethodOfType(Decls[I]).isStatic then
            Result.Add(Decls[I]);
      end else
        Result.Add(Decls);
    end;

    if (Decl is TDeclaration_TypeEnum) then
      Result.Add(TDeclaration_TypeEnum(Decl).Elements)
    else
    if (Decl is TDeclaration_TypeRecord) then
    begin
      Result.Add(TDeclaration_TypeRecord(Decl).Consts);
      Result.Add(TDeclaration_TypeRecord(Decl).Fields);
    end;

    Decl := GetParent(Decl);
  end;
end;

function TCodeinsight.ParseExpr(Expr: String; out Members: TDeclarationArray): TDeclaration;
begin
  Result := ParseExpression(Self, Expr, Members);
end;

function TCodeinsight.ParseExpr(Expr: String): TDeclaration;
var
  _: TDeclarationArray;
begin
  Result := ParseExpression(Self, Expr, _);
end;

constructor TCodeinsight.Create;
begin
  inherited Create();

  FScriptParser := TCodeParser.Create();
  FScriptParser.OnHandleInclude := @DoHandleInclude;
  FScriptParser.OnHandlePlugin := @DoHandlePlugin;

  FIncludedFiles := TStringList.Create();
  FIncludeParsers := TCodeParserList.Create();
  FPluginParsers := TCodeParserList.Create();
end;

destructor TCodeinsight.Destroy;
begin
  Reset();

  FreeAndNil(FScriptParser);
  FreeAndNil(FIncludeParsers);
  FreeAndNil(FPluginParsers);
  FreeAndNil(FIncludedFiles);

  inherited Destroy();
end;

end.

