{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.codeinsight;

{$i simba.inc}

interface

uses
  sysutils, classes,
  castaliapaslex, castaliapaslextypes,
  simba.codeparser, simba.parser_misc,
  simba.ci_includecache;

type
  TCodeInsight = class(TCodeParser)
  protected
  class var
    FIncludeCache: TCodeInsight_IncludeCache;
    FFunctionListSections: TCodeInsight_IncludeArray;
    FBaseIncludes: TCodeInsight_IncludeArray;
    FBaseDefines: TStringList;
  protected
    FIncludes: TCodeInsight_IncludeArray;
    FLocals: TDeclarationMap;

    procedure DoInclude(Sender: TObject; FileName: String; var Handled: Boolean);
    procedure DoLibrary(Sender: TObject; FileName: String; var Handled: Boolean);

    procedure Reset;

    function GetIncludesHash: String;

    function GetGlobals: TDeclarationArray;
    function GetGlobalByName(Name: String): TDeclaration;
    function GetGlobalsByName(Name: String): TDeclarationArray;

    function GetLocals: TDeclarationArray;
    function GetLocalByName(Name: String): TDeclaration;
    function GetLocalsByName(Name: String): TDeclarationArray;
  public
    class procedure CreateClassVariables;
    class procedure DestroyClassVariables;
    class procedure AddBaseInclude(Include: TCodeInsight_Include);
    class procedure AddFunctionListSection(Include: TCodeInsight_Include);

    class property FunctionListSections: TCodeInsight_IncludeArray read FFunctionListSections;

    function GetMembersOfType(Declaration: TDeclaration): TDeclarationArray; overload;
    function GetMembersOfType(Declaration: TDeclaration; Name: String): TDeclarationArray; overload;

    function ParseExpression(Expressions: TExpressionArray): TDeclaration;
    function ParseExpression(Expr: String): TDeclaration;

    function FindDeclarations(Expr: String): TDeclarationArray;
    function FindMethods(Expr: String): TDeclarationArray;

    function ResolveType(Declaration: TDeclaration): TDeclaration;
    function ResolveArrayType(Declaration: TDeclaration; Dimensions: Int32): TDeclaration;
    function ResolvePointer(Declaration: TDeclaration): TDeclaration;

    procedure Run; overload; override;

    property Includes: TCodeInsight_IncludeArray read FIncludes;
    property IncludesHash: String read GetIncludesHash;

    property Globals: TDeclarationArray read GetGlobals;
    property GlobalsByName[Name: String]: TDeclarationArray read GetGlobalsByName;
    property GlobalByName[Name: String]: TDeclaration read GetGlobalByName;

    property Locals: TDeclarationArray read GetLocals;
    property LocalsByName[Name: String]: TDeclarationArray read GetLocalsByName;
    property LocalByName[Name: String]: TDeclaration read GetLocalByName;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  simba.settings;

class procedure TCodeInsight.CreateClassVariables;
begin
  FIncludeCache := TCodeInsight_IncludeCache.Create();
  FBaseDefines := TStringList.Create();
end;

class procedure TCodeInsight.DestroyClassVariables;
var
  I: Int32;
begin
  for I := 0 to High(FBaseIncludes) do
    FBaseIncludes[I].Free();

  FIncludeCache.Free();
  FIncludeCache := nil;

  FBaseDefines.Free();
  FBaseDefines := nil;
end;

class procedure TCodeInsight.AddBaseInclude(Include: TCodeInsight_Include);
begin
  FBaseIncludes := FBaseIncludes + [Include];
  FBaseDefines.AddStrings(Include.Lexer.Defines);
end;

class procedure TCodeInsight.AddFunctionListSection(Include: TCodeInsight_Include);
begin
  FFunctionListSections := FFunctionListSections + [Include];
end;

function TCodeInsight.GetGlobalsByName(Name: String): TDeclarationArray;
var
  Include: TCodeInsight_Include;
begin
  Result := nil;

  for Include in FBaseIncludes + FIncludes do
    Result := Result + Include.Globals.GetAll(Name);

  Result := Result + FGlobals.GetAll(Name);
end;

function TCodeInsight.GetGlobalByName(Name: String): TDeclaration;
var
  Include: TCodeInsight_Include;
begin
  Result := nil;

  for Include in FBaseIncludes + FIncludes do
  begin
    Result := Include.Globals.Get(Name);
    if Result <> nil then
      Exit;
  end;

  Result := FGlobals.Get(Name);
end;

function TCodeInsight.GetLocalsByName(Name: String): TDeclarationArray;
begin
  Result := FLocals.GetAll(Name);
end;

function TCodeInsight.GetLocalByName(Name: String): TDeclaration;
begin
  Result := FLocals.Get(Name);
end;

function TCodeInsight.GetGlobals: TDeclarationArray;
var
  Include: TCodeInsight_Include;
begin
  Result := nil;

  for Include in FBaseIncludes + FIncludes do
    Result += Include.Globals.GetAll();

  Result += FGlobals.GetAll();
end;

function TCodeInsight.GetLocals: TDeclarationArray;
begin
  Result := FLocals.GetAll();
end;

procedure TCodeInsight.DoInclude(Sender: TObject; FileName: String; var Handled: Boolean);
var
  Include: TCodeInsight_Include;
begin
  Include := FIncludeCache.GetInclude(Self, FileName);
  if (Include <> nil) then
    FIncludes := FIncludes + [Include];

  Handled := True;
end;

procedure TCodeInsight.DoLibrary(Sender: TObject; FileName: String; var Handled: Boolean);
var
  Include: TCodeInsight_Include;
begin
  if (OnLoadLibrary <> nil) then
  begin
    Include := FIncludeCache.GetLibrary(Self, FileName);
    if (Include <> nil) then
      FIncludes := FIncludes + [Include];
  end;

  Handled := True;
end;

procedure TCodeInsight.Reset;
var
  i: Int32;
begin
  Lexer.Init();

  for i := 0 to High(FIncludes) do
    FIncludeCache.Release(FIncludes[i]);

  FGlobals.Clear();
  FLocals.Clear();
end;

function TCodeInsight.GetIncludesHash: String;
var
  I: Int32;
begin
  Result := '';
  for I := 0 to High(FIncludes) do
    Result := Result + FIncludes[I].Hash;
end;

function TCodeInsight.ParseExpression(Expressions: TExpressionArray): TDeclaration;
var
  i: Int32;
  Base: TExpressionItem;
  Members: TDeclarationArray;
  Declaration: TDeclaration;
begin
  Result := nil;

  Base := Expressions.PopLeft();

  Declaration := LocalByName[Base.Identifier];
  if Declaration = nil then
    Declaration := GlobalByName[Base.Identifier];

  if (Declaration <> nil) then
  begin
    Declaration := ResolveType(Declaration);
    if Declaration = nil then
      Exit;

    if Base.Deref then
    begin
      Declaration := ResolvePointer(Declaration);
      if Declaration = nil then
        Exit;
    end;

    if Base.Dimensions > 0 then
    begin
      Declaration := ResolveArrayType(Declaration, Base.Dimensions);
      if Declaration = nil then
        Exit;

      if Base.DerefArray then
      begin
        Declaration := ResolvePointer(Declaration);
        if Declaration = nil then
          Exit;
      end;
    end;

    for i := 0 to High(Expressions) do
    begin
      Members := GetMembersOfType(Declaration, Expressions[i].Identifier);
      if Length(Members) = 0 then
        Exit;

      Declaration := Members[0];
      Declaration := ResolveType(Declaration);
      if Declaration = nil then
        Exit;

      if Expressions[i].Deref then
      begin
        Declaration := ResolvePointer(Declaration);
        if Declaration = nil then
          Exit;
      end;

      if Expressions[i].Dimensions > 0 then
      begin
        Declaration := ResolveArrayType(Declaration, Expressions[i].Dimensions);
        if Declaration = nil then
          Exit;

        if Expressions[i].DerefArray then
        begin
          Declaration := ResolvePointer(Declaration);
          if Declaration = nil then
            Exit;
        end;
      end;
    end;

    Result := Declaration;
  end;
end;

function TCodeInsight.ParseExpression(Expr: String): TDeclaration;
begin
  Result := ParseExpression(GetExpressionArray(Expr));
end;

function TCodeInsight.GetMembersOfType(Declaration: TDeclaration): TDeclarationArray;

  procedure GetFields(Declaration: TciRecordType);
  begin
    Result := Result + Declaration.Items.GetItemsOfClass(TciClassField);
  end;

  procedure GetEnumElements(Declaration: TciTypeDeclaration);
  begin
    if Declaration.EnumType <> nil then
      Result := Result + Declaration.EnumType.Elements;
  end;

  procedure GetMethods(Declaration: TciTypeDeclaration);
  var
    Parent: TDeclaration;
    Declarations: TDeclarationArray;
    I, Depth: Int32;
  begin
    Depth := 0;

    while (Declaration <> nil) and (Depth < 50) do
    begin
      Declarations := GlobalsByName['!' + Declaration.Name];
      for I := 0 to High(Declarations) do
        if Declarations[I] is TciProcedureDeclaration then
          Result := Result + [Declarations[I]];

      if Declaration.RecordType <> nil then
        GetFields(Declaration.RecordType);

      Parent := nil;

      if Declaration.RecordType <> nil then
        Parent := GlobalByName[Declaration.RecordType.Parent]
      else
      if Declaration.CopyType <> nil then
        Parent := GlobalByName[Declaration.CopyType.Parent]
      else
      if Declaration.AliasType <> nil then
        Parent := GlobalByName[Declaration.AliasType.Parent];

      if (Parent <> nil) and (Parent <> Declaration) and (Parent is TciTypeDeclaration) then
        Declaration := Parent as TciTypeDeclaration
      else
        Declaration := nil;

      Inc(Depth);
    end;

    if Depth >= 50 then
      WriteLn('Recursive type detected');
  end;

begin
  Result := nil;
  if Declaration = nil then
    Exit;

  if Declaration is TciProcedureClassName then
  begin
    Declaration := GlobalByName[Declaration.RawText];
    if Declaration is TciTypeDeclaration then
      GetMethods(Declaration as TciTypeDeclaration);
  end else
  if Declaration is TciTypeKind then
  begin
    with Declaration as TciTypeKind do
    begin
      if RecordType <> nil then
        GetFields(RecordType);

      if IdentifierType <> nil then
      begin
        Declaration := GlobalByName[IdentifierType.RawText];
        if Declaration is TciTypeDeclaration then
          GetMethods(Declaration as TciTypeDeclaration);
      end;
    end;
  end else
  if Declaration is TciTypeDeclaration then
  begin
    GetMethods(Declaration as TciTypeDeclaration);
    GetEnumElements(Declaration as TciTypeDeclaration);
  end else
  if Declaration is TciRecordType then
    GetFields(Declaration as TciRecordType)
  else
    WriteLn('GetMembersOfType: Unexpected type "', Declaration.ClassName, '"');
end;

function TCodeInsight.GetMembersOfType(Declaration: TDeclaration; Name: String): TDeclarationArray;
var
  Declarations: TDeclarationArray;
  i: Int32;
begin
  Result := nil;

  Declarations := GetMembersOfType(Declaration);
  for i := 0 to High(Declarations) do
    if Declarations[i].IsName(Name) then
      Result := Result + [Declarations[i]];
end;

function TCodeInsight.FindDeclarations(Expr: String): TDeclarationArray;
var
  Expressions: TExpressionArray;
  Name: String;
  Declaration: TDeclaration;
begin
  Result := nil;

  Expressions := GetExpressionArray(Expr);
  Name := Expressions.Pop.Identifier;

  if Length(Expressions) > 0 then
  begin
    Declaration := ParseExpression(Expressions);
    if Declaration <> nil then
      Result := GetMembersOfType(Declaration, Name);
  end else
    Result := LocalsByName[Name] + GlobalsByName[Name];
end;

function TCodeInsight.FindMethods(Expr: String): TDeclarationArray;

  function Resolve(Declaration: TDeclaration): TciProcedureDeclaration;
  var
    NativeMethod: TciNativeType;
    Parent: TDeclaration;
  begin
    Result := nil;

    if (Declaration <> nil) and not (Declaration is TciProcedureDeclaration) then
    begin
      Declaration := ResolveType(Declaration);

      if Declaration is TciTypeDeclaration then
      begin
        NativeMethod := TciTypeDeclaration(Declaration).NativeMethodType;

        if NativeMethod <> nil then
        begin
          Parent := GlobalByName[NativeMethod.Parent];
          if Parent is TciTypeDeclaration then
            Declaration := Parent as TciTypeDeclaration;
        end;

        if Declaration is TciTypeDeclaration then
          Declaration := TciTypeDeclaration(Declaration).MethodType;
      end;
    end;

    if Declaration is TciProcedureDeclaration then
      Result := Declaration as TciProcedureDeclaration;
  end;

var
  Declaration: TciProcedureDeclaration;
  Declarations: TDeclarationArray;
  i: Int32;
  Invoke: Boolean;
begin
  Result := nil;

  Invoke := Expr.EndsWith('()');
  Declarations := FindDeclarations(Expr);

  for i := 0 to High(Declarations) do
  begin
    Declaration := Resolve(Declarations[i]);

    if (Declaration <> nil) then
    begin
      if Invoke then
      begin
        if (Declaration.ReturnType <> nil) then
          Declaration := Resolve(Declaration.ReturnType)
        else
          Declaration := nil;
      end;
      if (Declaration <> nil) then
        Result := Result + [Declaration];
    end;
  end;
end;

function TCodeInsight.ResolveType(Declaration: TDeclaration): TDeclaration;
begin
  Result := nil;

  if Declaration is TciTypeIdentifer then
  begin
    Result := GlobalByName[Declaration.RawText];
    Exit;
  end;

  if Declaration is TciProcedureClassName then
  begin
    Result := GlobalByName[Declaration.RawText];
    Exit;
  end;
  if Declaration is TciTypeDeclaration then
    Exit(Declaration);

  if Declaration is TciProcedureDeclaration then
    Declaration := TciProcedureDeclaration(Declaration).ReturnType
  else
  if Declaration is TciVarDeclaration then
    Declaration := TciVarDeclaration(Declaration).VarType;

  if Declaration is TciTypeKind then
  begin
    with Declaration as TciTypeKind do
    begin
      if IdentifierType <> nil then
        Result := GlobalByName[IdentifierType.Name]
      else
        Result := GetType;
    end;
  end else
  begin
    if Declaration = nil then
      WriteLN('ResolveType: Nil')
    else
      WriteLn('ResolveType: Unexpected type "', Declaration.ClassName, '"');
  end;
end;

function TCodeInsight.ResolveArrayType(Declaration: TDeclaration; Dimensions: Int32): TDeclaration;
begin
  Result := nil;

  while (Declaration <> nil) and (Dimensions > 0) do
  begin
    if Declaration is TciTypeDeclaration then
      Declaration := TciTypeDeclaration(Declaration).ArrayType;

    if (Declaration is TciArrayType) then
    begin
      with Declaration as TciArrayType do
      begin
        Dec(Dimensions, GetDimensionCount());
        Declaration := ResolveType(GetType());
      end;
    end else
      Declaration := nil;
  end;

  Result := Declaration;
end;

function TCodeInsight.ResolvePointer(Declaration: TDeclaration): TDeclaration;
begin
  Result := nil;

  if Declaration is TciTypeDeclaration then
    Declaration := TciTypeDeclaration(Declaration).PointerType;

  if (Declaration <> nil) and (Declaration.ClassType = TciPointerType) then
    Result := ResolveType(TciPointerType(Declaration).GetType());
end;

procedure TCodeInsight.Run;

  procedure GetMethodLocals(Method: TciProcedureDeclaration);
  var
    Declaration: TDeclaration;
    Declarations: TDeclarationArray;
    i: Int32;
  begin
    Declarations := nil;

    while (Method <> nil) do
    begin
      Declarations := Declarations + Method.Items.GetItemsOfClass(TciVarDeclaration);
      Declarations := Declarations + Method.Items.GetItemsOfClass(TciTypeDeclaration);
      Declarations := Declarations + Method.Items.GetItemsOfClass(TciProcedureDeclaration);
      Declarations := Declarations + Method.Items.GetItemsOfClass(TciReturnType);
      Declarations := Declarations + Method.GetParamDeclarations();

      if (tokStatic in Method.Directives) then
        Break;

      Declaration := Method.Items.GetFirstItemOfClass(TciProcedureClassName);

      if Declaration <> nil then
      begin
        Declarations := Declarations + [Declaration];
        Declarations := Declarations + GetMembersOfType(Declaration);
      end;

      Method.HasOwnerClass(TciProcedureDeclaration, TDeclaration(Method));
    end;

    for i := 0 to High(Declarations) do
      FLocals.Add(Declarations[i].Name, Declarations[i]);
  end;

  procedure GetWithVariables(Declarations: TDeclarationArray);
  var
    Declaration: TDeclaration;
    i: Int32;
  begin
    for i := 0 to High(Declarations) do
    begin
      Declaration := ParseExpression(Declarations[i].RawText);
      if Declaration <> nil then
        for Declaration in GetMembersOfType(Declaration) do
          FLocals.Add(Declaration.Name, Declaration);
    end;
  end;

  function GetLastDeclaration(Declaration: TDeclaration): TDeclaration;
  begin
    while Declaration.Items.Count > 0 do
      Declaration := Declaration.Items[Declaration.Items.Count - 1];

    Result := Declaration;
  end;

var
  Declaration: TDeclaration;
  Declarations: TDeclarationArray;
  Method: TDeclaration;
  I: Int32;
begin
  Lexer.ClearDefines();
  Lexer.Defines.AddStrings(FBaseDefines);

  inherited Run();

  FLocals.Clear();

  // Find Locals
  if (FItems.Count > 0) and (FLexer.CaretPos > -1) then
  begin
    Declaration := FItems.GetItemInPosition(FLexer.CaretPos);
    if (Declaration = nil) and (FLexer.CaretPos >= FItems[FItems.Count - 1].StartPos) then // Assume declaration isn't fully declared
      Declaration := GetLastDeclaration(FItems[FItems.Count - 1]);

    if (Declaration <> nil) then
    begin
      Method := Declaration;

      if (Method is TciProcedureDeclaration) then
        GetMethodLocals(Method as TciProcedureDeclaration);
      if Method.HasOwnerClass(TciProcedureDeclaration, Method, True) then
        GetMethodLocals(Method as TciProcedureDeclaration);

      Declarations := Declaration.GetOwnersOfClass(TciWithStatement);
      if Declaration is TciWithStatement then
        Declarations := Declarations + [Declaration];

      for I := 0 to High(Declarations) do
        GetWithVariables(Declarations[I].Items.GetItemsOfClass(TciVariable));
    end;
  end;
end;

constructor TCodeInsight.Create;
begin
  inherited Create();

  FLexer.UseCodeToolsIDEDirective := not SimbaSettings.Editor.IgnoreCodeToolsIDEDirective.Value;

  FLocals := TDeclarationMap.Create();

  OnInclude := @DoInclude;
  OnLibrary := @DoLibrary;

  Reset();
end;

destructor TCodeInsight.Destroy;
begin
  Reset();

  FLocals.Free();

  inherited Destroy();
end;

initialization
  TCodeInsight.CreateClassVariables();

finalization
  TCodeInsight.DestroyClassVariables();

end.
