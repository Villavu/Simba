unit simba.ide_codetools_insight;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  mPasLexTypes, mPasLex,
  simba.ide_codetools_parser, simba.ide_codetools_utils;

type
  TCodeinsight = class(TObject)
  public
  class var
    FBaseIncludes: TCodeParserList;
    FBaseDefines: TStringList;
  protected
    FIncludes: TCodeParserList;
    FScriptParser: TCodeParser;

    function DoHandleInclude(Sender: TmwBasePasLex): Boolean;
    function GetIncludes: TCodeParserArray;
    function GetIncludesHash: String;

    procedure Reset;
  public
    class procedure AddBaseInclude(Include: TCodeParser);
    class procedure AddBaseDefine(Def: String);
    class constructor Create;
    class destructor Destroy;

    property Includes: TCodeParserList read FIncludes;
    property IncludesHash: String read GetIncludesHash;

    property ScriptParser: TCodeParser read FScriptParser;

    procedure SetScript(Script: String; FileName: String; CaretPos, MaxPos: Integer);
    procedure Run;

    function FindDecl(S: String): TDeclaration;

    function GetOverloads(Decl: TDeclaration): TDeclarationArray;
    function GetGlobals: TDeclarationArray;
    function GetMethodsOfType(Typ: String): TDeclarationArray;
    function GetMembersOfType(Decl: TDeclaration): TDeclarationArray;

    // if `TDeclaration_VarType` resolve to type.
    // if `TDeclaration_Identifier` resolve to type "TPoint" > "record X,Y: Integer; end"
    function EnsureTypeDeclaration(Decl: TDeclaration): TDeclaration;

    function DoArrayIndex(Decl: TDeclaration; Dimensions: Integer): TDeclaration;
    function DoPointerDeref(Decl: TDeclaration): TDeclaration;

    function ParseExpression(Expr: TExpressionItems; WantMethodResult: Boolean): TDeclaration; overload;
    function ParseExpression(Expr: String; WantMethodResult: Boolean): TDeclaration; overload;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  simba.mufasatypes, simba.ide_codetools_cache;

function TCodeinsight.GetIncludesHash: String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FIncludes.Count - 1 do
    Result := Result + FIncludes[I].Hash;
end;

function TCodeinsight.DoHandleInclude(Sender: TmwBasePasLex): Boolean;
var
  I: Integer;
  FileName: String;
begin
  Result := True;

  if (Sender.TokenID = tokIncludeOnceDirect) then
  begin
    FileName := Sender.DirectiveParamAsFileName;
    for I := 0 to FIncludes.Count - 1 do
      if (FIncludes[I].Lexer.FileName = FileName) then
        Exit;
  end;

  FIncludes.Add(GetCachedInclude(Sender));
end;

function TCodeinsight.GetIncludes: TCodeParserArray;
begin
  Result := FBaseIncludes.ToArray() + FIncludes.ToArray();
end;

procedure TCodeinsight.Reset;
begin
  ReleaseCachedIncludes(FIncludes);

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
  FBaseIncludes := TCodeParserList.Create();
  FBaseDefines := TStringList.Create();
end;

class destructor TCodeinsight.Destroy;
var
  Include: TCodeParser;
begin
  for Include in FBaseIncludes do
    Include.Free();
  FBaseIncludes.Free();
  FBaseDefines.Free();
  {$IFDEF PARSER_LEAK_CHECKS}
  if (SimbaProcessType = ESimbaProcessType.IDE) then
    TDeclaration.PrintLeaks();
  {$ENDIF}
  inherited;
end;

procedure TCodeinsight.SetScript(Script: String; FileName: String; CaretPos, MaxPos: Integer);
begin
  Reset();

  FScriptParser.SetScript(Script, FileName, FBaseDefines.ToStringArray());
  FScriptParser.Lexer.CaretPos := CaretPos;
  FScriptParser.Lexer.MaxPos := MaxPos;
end;

procedure TCodeinsight.Run;
begin
  FScriptParser.Run();
end;

function TCodeinsight.FindDecl(S: String): TDeclaration;
var
  Include: TCodeParser;
begin
  for Include in GetIncludes() do
  begin
    Result := Include.GetGlobal(S);
    if (Result <> nil) then
      Exit;
  end;

  Result := FScriptParser.GetGlobal(S);
end;

function TCodeinsight.GetOverloads(Decl: TDeclaration): TDeclarationArray;
var
  n: String;
begin
  n := Decl.Name;

  Result := nil;
  if (Decl is TDeclaration_TypeMethod) then
    Result := [Decl]
  else
  if (Decl is TDeclaration_Method) then
  begin
    if (TDeclaration_Method(Decl).MethodType in [mtObjectFunction, mtObjectProcedure]) then
    begin
      for Decl in GetMethodsOfType(TDeclaration_Method(Decl).ObjectName) do
        if Decl.IsName(n) then
          Result := Result + [Decl];
    end else
      for Decl in GetGlobals() do
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

  procedure CheckRecord(Decl: TDeclaration);
  begin
    Result := Result + Decl.Items.GetItemsOfClass(TDeclaration_Var);
  end;

  procedure CheckMethods(Decl: TDeclaration);
  var
    Include: TCodeParser;
  begin
    for Include in GetIncludes() do
      Result := Result + Include.GetMethodsOfType(Decl.Name);
    Result := Result + FScriptParser.GetMethodsOfType(Decl.Name);
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

  Depth := 0;
  while (Decl <> nil) and (Depth < 20) do
  begin
    if (Decl is TDeclaration_TypeRecord) then
      CheckRecord(Decl);
    CheckMethods(Decl);

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
    Result := Result.Items.First();
  if (Result is TDeclaration_Identifier) then
    Result := FindDecl(Result.Text);
end;

function TCodeinsight.DoArrayIndex(Decl: TDeclaration; Dimensions: Integer): TDeclaration;
begin
  Result := Decl;
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

function TCodeinsight.ParseExpression(Expr: TExpressionItems; WantMethodResult: Boolean): TDeclaration;

  function FindMember(Decl: TDeclaration; Expr: TExpressionItem): TDeclaration;
  begin
    Result := nil;

    for Decl in GetMembersOfType(Decl) do
      if Decl.IsName(Expr.Text) then
      begin
        if (Decl is TDeclaration_Var) and (TDeclaration_Var(Decl).VarType <> nil) then
        begin
          Result := EnsureTypeDeclaration(TDeclaration_Var(Decl).VarType);
          Exit;
        end;

        if (Decl is TDeclaration_Method) then
        begin
          if Expr.IsLastItem then
          begin
            case WantMethodResult of
              True:
                if (TDeclaration_Method(Decl).ResultType <> nil) then
                begin
                  Result := EnsureTypeDeclaration(TDeclaration_Method(Decl).ResultType);
                  Exit;
                end;
              False:
                begin
                  Result := Decl;
                  Exit;
                end;
            end;
          end;

          if (TDeclaration_Method(Decl).ResultType <> nil) then
          begin
            Result := EnsureTypeDeclaration(TDeclaration_Method(Decl).ResultType);
            Exit;
          end;
        end;
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

  if (Decl is TDeclaration_Method) and (not Expr[0].IsLastItem) then
   Decl := EnsureTypeDeclaration(TDeclaration_Method(Decl).ResultType)
  else
  if (Decl is TDeclaration_Var) then
    Decl := EnsureTypeDeclaration(TDeclaration_Var(Decl).VarType);

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
end;

function TCodeinsight.ParseExpression(Expr: String; WantMethodResult: Boolean): TDeclaration;
begin
 Result := ParseExpression(StringToExpression(Expr), WantMethodResult);
end;

constructor TCodeinsight.Create;
begin
  inherited Create();

  FScriptParser := TCodeParser.Create();
  FScriptParser.OnHandleInclude := @DoHandleInclude;

  FIncludes := TCodeParserList.Create();
end;

destructor TCodeinsight.Destroy;
begin
  Reset();

  if (FScriptParser <> nil) then
    FreeAndNil(FScriptParser);
  if (FIncludes <> nil) then
    FreeAndNil(FIncludes);

  inherited Destroy();
end;

end.

