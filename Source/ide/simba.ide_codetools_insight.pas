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
  mPasLexTypes, mPasLex,
  simba.ide_codetools_parser, simba.ide_codetools_utils;

type
  {$SCOPEDENUMS ON}
  EParseExpressionFlag = (
    WantVarType,      // if found decl is a variable, return the variables type
    WantMethodResult // if found decl is a function, return the result type
  );
  EParseExpressionFlags = set of EParseExpressionFlag;
  {$SCOPEDENUMS OFF}

  TCodeinsight = class(TObject)
  protected class var
    FBaseParsers: TCodeParserList;
  protected
    FIncludeParsers: TCodeParserList;
    FPluginParsers: TCodeParserList;
    FScriptParser: TCodeParser;

    FIncludedFiles: TStringList;

    class function Hash(Parsers: TCodeParserList): String;

    procedure DoHandleInclude(Sender: TmwBasePasLex);
    procedure DoHandlePlugin(Sender: TmwBasePasLex);

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

    procedure SetScript(Script: String; FileName: String; CaretPos: Integer = -1);
    procedure Run;

    // Get methods of a given type.
    // Searches everything, BaseIncludes, Includes, Script, Plugins
    // Does not check parents
    function GetTypeMethods(TypeName: String; RemoveStaticMethods: Boolean): TDeclarationArray;

    // Assuming `Decl` is a type declaration, return all members (methods, fields, enum elements)
    function GetMembersOfType(Decl: TDeclaration): TDeclarationArray;

    // Search locals, then globals
    function GetByName(Name: String): TDeclaration;

    function GetParsers: TCodeParserArray; // BaseParsers, IncludeParsers, PluginParsers, ScriptParser
    function GetLocals: TDeclarationArray; // Local, types, methods
    function GetGlobals: TDeclarationArray; // Global declarations from all parsers
    function GetOverloads(Decl: TDeclaration): TDeclarationArray; // Assuming `Decl` is a method, return all overloads of such method.

    // if `TDeclaration_VarType` resolve to type.
    // if `TDeclaration_Identifier` resolve to type "TPoint" > "record X,Y: Integer; end"
    function EnsureTypeDeclaration(Decl: TDeclaration): TDeclaration;

    function DoArrayIndex(Decl: TDeclaration; Dimensions: Integer): TDeclaration;
    function DoPointerDeref(Decl: TDeclaration): TDeclaration;

    function ParseExpression(Expr: TExpressionItems; Flags: EParseExpressionFlags): TDeclaration; overload;
    function ParseExpression(Expr: String; Flags: EParseExpressionFlags): TDeclaration; overload;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  simba.base, simba.ide_codetools_includes, simba.ide_codetools_arrayhelpers,
  simba.containers, simba.settings;

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

procedure TCodeinsight.DoHandleInclude(Sender: TmwBasePasLex);
var
  Parser: TCodeParser;
  Plugin: String;
begin
  Parser := CodetoolsIncludes.GetInclude(Sender, FIncludedFiles);

  if Assigned(Parser) then
  begin
    FScriptParser.Root.Items.Add(TDeclaration_IncludeDirective.Create(FScriptParser, Parser.Lexer.FileName));
    FIncludeParsers.Add(Parser);

    for Plugin in TCodetoolsPlugin(Parser).Plugins do
    begin
      Parser := CodetoolsIncludes.GetPlugin(Plugin);
      if Assigned(Parser) then
        FPluginParsers.Add(Parser);
    end;
  end;
end;

procedure TCodeinsight.DoHandlePlugin(Sender: TmwBasePasLex);
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

  FIncludedFiles.Clear();
  FIncludeParsers.Clear();
  FPluginParsers.Clear();
  FScriptParser.Reset();
end;

class procedure TCodeinsight.AddBaseParser(Parser: TCodeParser);
begin
  FBaseParsers.Add(Parser);
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
  if SimbaSettings.CodeTools.IgnoreIDEDirective.Value then
    FScriptParser.Lexer.AddDefine('!CODETOOLS');
  FScriptParser.Lexer.CaretPos := CaretPos;
end;

procedure TCodeinsight.Run;
var
  I: Integer;
begin
  FScriptParser.Run();

  for I := 0 to FScriptParser.Locals.Count - 1 do
    if (FScriptParser.Locals[I] is TDeclaration_WithVariable) then
      FScriptParser.Locals.Extend(GetMembersOfType(ParseExpression(FScriptParser.Locals[I].Text, [EParseExpressionFlag.WantMethodResult])));
end;

function TCodeinsight.GetTypeMethods(TypeName: String; RemoveStaticMethods: Boolean): TDeclarationArray;
var
  Parser: TCodeParser;
  I, Count: Integer;
begin
  Result := [];

  if (TypeName <> '') then
  begin
    for Parser in GetParsers() do
      Result := Result + Parser.TypeMethods.Get(TypeName);

    if RemoveStaticMethods then
    begin
      Count := 0;
      for I := 0 to High(Result) do
        if (not Result[I].isStaticMethod) then
        begin
          Result[Count] := Result[I];
          Inc(Count);
        end;
      SetLength(Result, Count);
    end;
  end;
end;

function TCodeinsight.GetByName(Name: String): TDeclaration;
var
  Decls: TDeclarationArray;
begin
  Decls := Filter(GetLocals(), Name);
  if (Length(Decls) = 0) then
    Decls := Filter(GetGlobals(), Name);

  if (Length(Decls) > 0) then
    Result := Decls[0]
  else
    Result := nil;
end;

function TCodeinsight.GetOverloads(Decl: TDeclaration): TDeclarationArray;
begin
  if (Decl is TDeclaration_Method) then
  begin
    if Decl.isObjectMethod then
      Result := Filter(GetMembersOfType(GetByName(TDeclaration_Method(Decl).ObjectName)), Decl.Name, TDeclaration_Method)
    else
      Result := Filter(GetGlobals() + GetLocals(), Decl.Name, TDeclaration_Method);
  end else
    Result := [];
end;

function TCodeinsight.GetGlobals: TDeclarationArray;
var
  Parser: TCodeParser;
begin
  Result := [];
  for Parser in GetParsers() do
    Result := Result + Parser.Globals.ToArray;
end;

function TCodeinsight.GetLocals: TDeclarationArray;
begin
  Result := FScriptParser.Locals.ToArray;
end;

function TCodeinsight.GetParsers: TCodeParserArray;
var
  Count: Integer = 1;

  procedure Add(const Parsers: TCodeParserList); inline;
  var
    I: Integer;
  begin
    for I := 0 to Parsers.Count - 1 do
    begin
      Result[Count] := Parsers[I];
      Inc(Count);
    end;
  end;

begin
  SetLength(Result, 1 + FBaseParsers.Count + FIncludeParsers.Count + FPluginParsers.Count);

  Result[0] := ScriptParser;

  {%H-}Add(FBaseParsers);
  Add(FIncludeParsers);
  Add(FPluginParsers);

  Assert(Count = Length(Result));
end;

function TCodeinsight.GetMembersOfType(Decl: TDeclaration): TDeclarationArray;

  function CheckEnum(Decl: TDeclaration): TDeclarationArray;
  begin
    if (Decl is TDeclaration_TypeSet) then Result := Decl.Items.GetByClass(TDeclaration_EnumElement, False, True)
    else
    if (Decl is TDeclaration_TypeEnum) then
      Result := Decl.Items.GetByClass(TDeclaration_EnumElement)
    else
      Result := [];
  end;

  function CheckRecord(Decl: TDeclaration): TDeclarationArray;
  begin
    if (Decl is TDeclaration_TypeRecord) then
      Result := Decl.Items.GetByClass(TDeclaration_Var)
    else
      Result := [];
  end;

  function CheckMethods(Decl: TDeclaration; RemoveStaticMethods: Boolean): TDeclarationArray;
  var
    Include: TCodeParser;
    I, C: Integer;
  begin
    Result := GetTypeMethods(Decl.Name, RemoveStaticMethods);

    Include := GetArrayHelpers(Decl);
    if (Include <> nil) then
      Result := Result + Include.Items.ToArray;

    // Move functions to top since we likely want to find them for ParseExpression
    C := 0;
    for I := 0 to High(Result) do
      if (Result[I] is TDeclaration_Method) and Assigned(TDeclaration_Method(Result[I]).ResultType) then
      begin
        Swap(Pointer(Result[I]), Pointer(Result[C]));
        Inc(C);
      end;
  end;

  function GetParent(Decl: TDeclaration): TDeclaration;
  begin
    if (Decl is TDeclaration_TypeRecord) then Result := EnsureTypeDeclaration(TDeclaration_TypeRecord(Decl).Parent) else
    if (Decl is TDeclaration_TypeCopy)   then Result := EnsureTypeDeclaration(TDeclaration_TypeCopy(Decl).VarType)  else
    if (Decl is TDeclaration_TypeAlias)  then Result := EnsureTypeDeclaration(TDeclaration_TypeAlias(Decl).VarType) else
      Result := nil;

    if (Result = Decl) then
      Result := nil;
  end;

const
  MAX_DEPTH = 20;
var
  Depth: Integer;
begin
  Result := nil;

  Decl := EnsureTypeDeclaration(Decl);
  if (Decl = nil) then
    Exit;

  for Depth := 1 to MAX_DEPTH do
  begin
    Result := Result + CheckEnum(Decl);
    Result := Result + CheckRecord(Decl);
    Result := Result + CheckMethods(Decl, Depth > 1);

    Decl := GetParent(Decl);
    if (Decl = nil) then
      Exit;
  end;
end;

function TCodeinsight.EnsureTypeDeclaration(Decl: TDeclaration): TDeclaration;
begin
  Result := Decl;
  if (Result is TDeclaration_VarType) and (Result.Items.Count > 0) then
    Result := Result.Items[0];
  if (Result is TDeclaration_Identifier) then
    Result := GetByName(Result.Text);
  if (Result is TDeclaration_TypeNativeMethod) and (Result.Items.Count > 0) then
    Result := Result.Items[0];
end;

function TCodeinsight.DoArrayIndex(Decl: TDeclaration; Dimensions: Integer): TDeclaration;
begin
  Result := Decl;

  if (Result is TDeclaration_TypeAlias) then
  begin
    if Result.IsName('String') then
      Result := EnsureTypeDeclaration(GetByName('Char'))
    else
    if Result.IsName('UnicodeString') or Result.IsName('WideString') then
      Result := EnsureTypeDeclaration(GetByName('WideChar'));
  end
  else
  if (Result is TDeclaration_TypeArray) then
  begin
    Dec(Dimensions, TDeclaration_TypeArray(Result).DimCount);

    Result := EnsureTypeDeclaration(TDeclaration_TypeArray(Result).VarType);
    if (Dimensions > 0) then
      Result := DoArrayIndex(Result, Dimensions);
  end;
end;

function TCodeinsight.DoPointerDeref(Decl: TDeclaration): TDeclaration;
begin
  Result := Decl;
  if (Result is TDeclaration_TypePointer) then
    Result := EnsureTypeDeclaration(TDeclaration_TypePointer(Result).VarType);
end;

function TCodeinsight.ParseExpression(Expr: TExpressionItems; Flags: EParseExpressionFlags): TDeclaration;

  function FindMember(Decl: TDeclaration; Expr: TExpressionItem; isLast: Boolean): TDeclaration;
  begin
    Result := nil;

    for Decl in GetMembersOfType(Decl) do
      if Decl.IsName(Expr.Text) then
      begin
        if isLast and (not Expr.HasSymbols) then // end of the tree, result will change depending on `Flags`
          Result := Decl
        else
        if (Decl is TDeclaration_Var) and (TDeclaration_Var(Decl).VarType <> nil) then
          Result := EnsureTypeDeclaration(TDeclaration_Var(Decl).VarType)
        else
        if (Decl is TDeclaration_Method) and Assigned(TDeclaration_Method(Decl).ResultType) then
          Result := EnsureTypeDeclaration(TDeclaration_Method(Decl).ResultType);

        Exit;
      end;
  end;

  function DoSymbols(Decl: TDeclaration; Expr: TExpressionItem): TDeclaration;
  begin
    Result := Decl;
    if Expr.Symbols.Deref then
      Result := DoPointerDeref(Result);

    if (Expr.Symbols.DimCount > 0) and (not Decl.isProperty) then
    begin
      Result := DoArrayIndex(Result, Expr.Symbols.DimCount);
      if Expr.Symbols.DerefDim then
        Result := DoPointerDeref(Result);
    end;
  end;

var
  Decl: TDeclaration;
  I: Integer;
begin
  Result := nil;
  if (Length(Expr) = 0) then
    Exit;

  Decl := GetByName(Expr[0].Text);
  if (Length(Expr) = 1) and (Flags = []) then
  begin
    Result := Decl;
    Exit;
  end;

  if (Length(Expr) > 1) or Expr[0].HasSymbols then
    if (Decl is TDeclaration_Method) then
     Decl := EnsureTypeDeclaration(TDeclaration_Method(Decl).ResultType)
    else
    if (Decl is TDeclaration_Var) then
      Decl := EnsureTypeDeclaration(TDeclaration_Var(Decl).VarType)
    else
      Decl := EnsureTypeDeclaration(Decl);

  if (Decl = nil) then
  begin
    DebugLn('TCodeinsight.ParseExpression: Unknown starting type "' + Expr[0].Text + '"');
    Exit;
  end;

  Decl := DoSymbols(Decl, Expr[0]);
  if (Decl = nil) then
  begin
    DebugLn('TCodeinsight.ParseExpression: Failed on starting symbols "' + Expr[0].Text + '"');
    Exit;
  end;

  for I := 1 to High(Expr) do
  begin
    Decl := FindMember(Decl, Expr[I], I=High(Expr));
    if (Decl = nil) then
    begin
      DebugLn('TCodeinsight.ParseExpression: Failed on member "' + Expr[I].Text + '"');
      Exit;
    end;

    Decl := DoSymbols(Decl, Expr[I]);
    if (Decl = nil) then
    begin
      DebugLn('TCodeinsight.ParseExpression: Failed on symbols "' + Expr[I].Text + '"');
      Exit;
    end;
  end;

  Result := Decl;

  if (EParseExpressionFlag.WantVarType in Flags) and (Result is TDeclaration_Var) then
    Result := EnsureTypeDeclaration(TDeclaration_Var(Result).VarType)
  else
  if (EParseExpressionFlag.WantMethodResult in Flags) and (Result is TDeclaration_Method) and (TDeclaration_Method(Result).ResultType <> nil) then
    Result := EnsureTypeDeclaration(TDeclaration_Method(Result).ResultType);
end;

function TCodeinsight.ParseExpression(Expr: String; Flags: EParseExpressionFlags): TDeclaration;
begin
  Result := ParseExpression(StringToExpression(Expr), Flags);
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

