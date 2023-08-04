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
    WantVarType,     // if found decl is a variable, return the variables type
    WantMethodResult // if found decl is a function, return the result type
  );
  EParseExpressionFlags = set of EParseExpressionFlag;
  {$SCOPEDENUMS OFF}

  TCodeinsight = class(TObject)
  protected
  class var
    FBaseIncludes: TCodeParserList;
    FBaseDefines: TStringList;
  protected
    FCreatedPlugins: Boolean;
    FPlugins: TCodeParserList;
    FIncludes: TCodeParserList;
    FScriptParser: TCodeParser;

    function DoFindInclude(Sender: TmwBasePasLex; var FileName: String; var Handled: Boolean): Boolean;
    function DoFindPlugin(Sender: TmwBasePasLex; var FileName: String): Boolean;

    function GetIncludes: TCodeParserArray;
    function GetIncludesHash: String;
    function GetPluginsHash: String;
    function GetPlugins: TCodeParserList;

    procedure Reset;
  public
    class procedure AddBaseInclude(Include: TCodeParser);
    class procedure AddBaseDefine(Def: String);
    class property BaseIncludes: TCodeParserList read FBaseIncludes;
    class constructor Create;
    class destructor Destroy;

    property Plugins: TCodeParserList read GetPlugins;
    property PluginsHash: String read GetPluginsHash;

    property Includes: TCodeParserList read FIncludes;
    property IncludesHash: String read GetIncludesHash;

    property ScriptParser: TCodeParser read FScriptParser;

    procedure SetScript(Script: String; FileName: String; CaretPos: Integer = -1);
    procedure Run;

    function FindDecl(S: String): TDeclaration;

    function GetOverloads(Decl: TDeclaration): TDeclarationArray;
    function GetGlobals: TDeclarationArray;
    function GetLocals: TDeclarationArray;
    function GetMethodsOfType(Typ: String): TDeclarationArray;
    function GetMembersOfType(Decl: TDeclaration): TDeclarationArray;

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
  simba.mufasatypes, simba.ide_codetools_includes, simba.ide_codetools_arrayhelpers,
  simba.ide_codetools_plugins;

function TCodeinsight.GetIncludesHash: String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FIncludes.Count - 1 do
    Result := Result + FIncludes[I].Hash;
end;

function TCodeinsight.GetPluginsHash: String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Plugins.Count - 1 do
    Result := Result + Plugins[I].Hash;
end;

function TCodeinsight.GetPlugins: TCodeParserList;
var
  I: Integer;
  Parser: TCodeParser;
  FileNames: TStringList;
begin
  if (not FCreatedPlugins) then
  begin
    FCreatedPlugins := True;

    FileNames := TStringList.Create();
    FileNames.AddStrings(FScriptParser.Plugins);
    for I := 0 to FIncludes.Count - 1 do
      FileNames.AddStrings(FIncludes[I].Plugins);

    for I := 0 to FileNames.Count - 1 do
    begin
      Parser := CodetoolsPlugins.Get(FileNames[I]);
      if Assigned(Parser) then
        FPlugins.Add(Parser);
    end;

    FileNames.Free();
  end;

  Result := FPlugins;
end;

function TCodeinsight.DoFindInclude(Sender: TmwBasePasLex; var FileName: String; var Handled: Boolean): Boolean;
var
  I: Integer;
  Include: TCodeParser;
begin
  Result := False;
  Handled := True;

  if (Sender.TokenID = tokIncludeOnceDirect) then
  begin
    FileName := Sender.DirectiveParamAsFileName;
    for I := 0 to FIncludes.Count - 1 do
      if (FIncludes[I].Lexer.FileName = FileName) then
        Exit;
  end;

  Include := CodetoolsIncludes.Get(Sender);
  if (Include <> nil) then
    FIncludes.Add(Include);
end;

function TCodeinsight.DoFindPlugin(Sender: TmwBasePasLex; var FileName: String): Boolean;
begin
  FileName := FindInclude(Sender);

  Result := FileName <> '';
end;

function TCodeinsight.GetIncludes: TCodeParserArray;
begin
  Result := FBaseIncludes.ToArray() + FIncludes.ToArray();
end;

procedure TCodeinsight.Reset;
begin
  FCreatedPlugins := False;

  CodetoolsIncludes.Release(FIncludes);
  CodetoolsPlugins.Release(FPlugins);

  FPlugins.Clear();
  FIncludes.Clear();
  FScriptParser.Reset();
end;

class procedure TCodeinsight.AddBaseInclude(Include: TCodeParser);
begin
  FBaseIncludes.Add(Include);
end;

class procedure TCodeinsight.AddBaseDefine(Def: String);
begin
  FBaseDefines.Add(Def);
end;

class constructor TCodeinsight.Create;
begin
  FBaseIncludes := TCodeParserList.Create(True);
  FBaseDefines := TStringList.Create();
end;

class destructor TCodeinsight.Destroy;
begin
  FBaseIncludes.Free();
  FBaseDefines.Free();
end;

procedure TCodeinsight.SetScript(Script: String; FileName: String; CaretPos: Integer);
begin
  Reset();

  FScriptParser.SetScript(Script, FileName, FBaseDefines.ToStringArray());
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

function TCodeinsight.FindDecl(S: String): TDeclaration;
var
  Include: TCodeParser;
  Decls: TDeclarationArray;
begin
  Decls := FScriptParser.Locals.GetByName(S);
  if (Length(Decls) > 0) then
  begin
    Result := Decls[0];
    Exit;
  end;

  for Include in GetIncludes() do
  begin
    Decls := Include.Globals.GetByName(S);
    if (Length(Decls) > 0) then
    begin
      Result := Decls[0];
      Exit;
    end;
  end;

  Decls := FScriptParser.Globals.GetByName(S);
  if (Length(Decls) > 0) then
    Result := Decls[0]
  else
    Result := nil;
end;

function TCodeinsight.GetOverloads(Decl: TDeclaration): TDeclarationArray;
var
  n: String;
begin
  if (Decl = nil) then
    Exit(nil);
  n := Decl.Name;

  Result := nil;
  if (Decl is TDeclaration_TypeMethod) then
    Result := [Decl]
  else
  if (Decl is TDeclaration_Method) then
  begin
    if Decl.isObjectMethod then
    begin
      for Decl in GetMethodsOfType(TDeclaration_Method(Decl).ObjectName) do
        if Decl.IsName(n) then
          Result := Result + [Decl];
    end else
      for Decl in GetGlobals() + GetLocals() do
        if Decl.IsName(n) and (Decl is TDeclaration_Method) then
          Result := Result + [Decl];
  end;
end;

function TCodeinsight.GetGlobals: TDeclarationArray;
var
  Include: TCodeParser;
begin
  Result := FScriptParser.Globals.ToArray;
  for Include in GetIncludes() do
    Result := Result + Include.Globals.ToArray;
end;

function TCodeinsight.GetLocals: TDeclarationArray;
begin
  Result := FScriptParser.Locals.ToArray;
end;

function TCodeinsight.GetMethodsOfType(Typ: String): TDeclarationArray;
var
  I, Count: Integer;
begin
  Result := GetMembersOfType(EnsureTypeDeclaration(FindDecl(Typ)));

  Count := 0;
  for I := 0 to High(Result) do
    if (Result[I] is TDeclaration_Method) then
    begin
      Result[Count] := Result[I];
      Inc(Count);
    end;
  SetLength(Result, Count);
end;

function TCodeinsight.GetMembersOfType(Decl: TDeclaration): TDeclarationArray;

  function FilterStaticMethods(const Arr: TDeclarationArray; NeedFilter: Boolean): TDeclarationArray;
  var
    I, C: Integer;
  begin
    if (Length(Arr) = 0) or (not NeedFilter) then
      Exit(Arr);

    C := 0;
    SetLength(Result, Length(Arr));
    for I := 0 to High(Arr) do
      if not Arr[I].isStaticMethod then
      begin
        Result[C] := Arr[I];
        Inc(C);
      end;
    SetLength(Result, C);
  end;

  procedure CheckRecord(Decl: TDeclaration);
  begin
    Result := Result + Decl.Items.GetByClass(TDeclaration_Var);
  end;

  procedure CheckMethods(Decl: TDeclaration; RemoveStatic: Boolean);
  var
    Include: TCodeParser;
  begin
    for Include in GetIncludes() do
      Result := Result + FilterStaticMethods(Include.TypeMethods.Get(Decl.Name), RemoveStatic);
    Result := Result + FilterStaticMethods(FScriptParser.TypeMethods.Get(Decl.Name), RemoveStatic);

    Include := GetArrayHelpers(Decl);
    if (Include <> nil) then
      Result := Result + Include.Items.ToArray;
  end;

  function GetParent(Decl: TDeclaration): TDeclaration;
  begin
    if (Decl is TDeclaration_TypeRecord) then
      Result := EnsureTypeDeclaration(TDeclaration_TypeRecord(Decl).Parent)
    else
    if (Decl is TDeclaration_TypeCopy) then
      Result := EnsureTypeDeclaration(TDeclaration_TypeCopy(Decl).VarType)
    else
    if (Decl is TDeclaration_TypeAlias) then
      Result := EnsureTypeDeclaration(TDeclaration_TypeAlias(Decl).VarType)
    else
      Result := nil;
  end;

var
  ParentDecl: TDeclaration;
  Depth: Integer;
begin
  Result := nil;

  Decl := EnsureTypeDeclaration(Decl);
  if (Decl is TDeclaration_TypeEnum) then
  begin
    Result := TDeclaration_TypeEnum(Decl).Elements;
    Exit;
  end;

  Depth := 0;
  while (Decl <> nil) and (Depth < 20) do
  begin
    if (Decl is TDeclaration_TypeRecord) then
      CheckRecord(Decl);
    CheckMethods(Decl, Depth>0);

    ParentDecl := GetParent(Decl);
    if (ParentDecl <> Decl) then
      Decl := ParentDecl
    else
      Decl := nil;

    Inc(Depth);
  end;

  if (Depth > 20) then
    DebugLn('TCodeinsight.GetMembersOfType: Recursive type');
end;

function TCodeinsight.EnsureTypeDeclaration(Decl: TDeclaration): TDeclaration;
begin
  Result := Decl;
  if (Result is TDeclaration_VarType) and (Result.Items.Count > 0) then
    Result := Result.Items[0];
  if (Result is TDeclaration_Identifier) then
    Result := FindDecl(Result.Text);
  if (Result is TDeclaration_TypeNativeMethod) and (Result.Items.Count > 0) then
    Result := Result.Items[0];
end;

function TCodeinsight.DoArrayIndex(Decl: TDeclaration; Dimensions: Integer): TDeclaration;
begin
  Result := Decl;

  if (Result is TDeclaration_TypeAlias) then
  begin
    if Result.IsName('String') then
      Result := EnsureTypeDeclaration(FindDecl('Char'))
    else
    if Result.IsName('UnicodeString') or Result.IsName('WideString') then
      Result := EnsureTypeDeclaration(FindDecl('WideChar'));
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

  function FindMember(Decl: TDeclaration; Expr: TExpressionItem): TDeclaration;
  begin
    Result := nil;

    for Decl in GetMembersOfType(Decl) do
      if Decl.IsName(Expr.Text) then
      begin
        if (Decl is TDeclaration_Var) and (TDeclaration_Var(Decl).VarType <> nil) then
          Result := EnsureTypeDeclaration(TDeclaration_Var(Decl).VarType)
        else
        if Expr.IsLastItem then
          Result := Decl
        else
        if (Decl is TDeclaration_Method) and Assigned(TDeclaration_Method(Decl).ResultType) then
          Result := EnsureTypeDeclaration(TDeclaration_Method(Decl).ResultType);

        Exit;
      end;
  end;

  function DoSymbols(Decl: TDeclaration; Expr: TExpressionItem): TDeclaration;
  begin
    Result := Decl;
    if Expr.DerefText then
      Result := DoPointerDeref(Result);

    if (Expr.Dimensions > 0) then
    begin
      Result := DoArrayIndex(Result, Expr.Dimensions);
      if Expr.DerefDimension then
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

  Decl := FindDecl(Expr[0].Text);
  if (Length(Expr) = 1) and (Flags * [EParseExpressionFlag.WantMethodResult, EParseExpressionFlag.WantVarType] = []) then
  begin
    Result := Decl;
    Exit;
  end;

  if (Decl is TDeclaration_Method) and (not Expr[0].IsLastItem) then
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
    Decl := FindMember(Decl, Expr[I]);
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

  if (EParseExpressionFlag.WantMethodResult in Flags) then
    if (Result is TDeclaration_Method) and Assigned(TDeclaration_Method(Result).ResultType) then
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
  FScriptParser.OnFindInclude := @DoFindInclude;
  FScriptParser.OnFindPlugin := @DoFindPlugin;

  FIncludes := TCodeParserList.Create();
  FPlugins := TCodeParserList.Create();
end;

destructor TCodeinsight.Destroy;
begin
  Reset();

  if (FScriptParser <> nil) then
    FreeAndNil(FScriptParser);
  if (FIncludes <> nil) then
    FreeAndNil(FIncludes);
  if (FPlugins <> nil) then
    FreeAndNil(FPlugins);

  inherited Destroy();
end;

end.

