{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_codetools_parser;

{$i simba.inc}

interface

uses
  SysUtils, Classes, nullable,
  simba.base,
  simba.containers,
  simba.ide_codetools_base,
  simba.ide_codetools_paslexer,
  simba.ide_codetools_pasparser;

type
  TCodeParser = class;

  TDeclaration = class;
  TDeclarationClass = class of TDeclaration;
  TDeclarationArray = array of TDeclaration;

  TDeclarationStack = specialize TSimbaStack<TDeclaration>;
  TDeclarationCache = specialize TNullable<TDeclarationArray>;
  TStringCache = specialize TNullable<String>;

  TDeclarationList = class(TObject)
  protected
    FItems: TDeclarationArray;
    FCount: Integer;

    function GetToArray: TDeclarationArray;
    function GetItem(const Index: Integer): TDeclaration;
    procedure SetItem(const Index: Integer; const Value: TDeclaration);
  public
    function GetByClass(const AClass: TDeclarationClass; const ExactClass: Boolean = False; const SubSearch: Boolean = False): TDeclarationArray;
    function GetByClassFirst(const AClass: TDeclarationClass; const ExactClass: Boolean = False; const SubSearch: Boolean = False): TDeclaration;
    function GetByPosition(const Position: Integer): TDeclaration;

    function GetTextOfClass(AClass: TDeclarationClass): String;
    function GetTextOfClassNoComments(AClass: TDeclarationClass): String;
    function GetTextOfClassNoCommentsSingleLine(AClass: TDeclarationClass; Prefix: String = ''): String;

    procedure Add(const Decl: TDeclaration);
    procedure Extend(const Decls: TDeclarationArray);
    procedure Clear(const FreeDecls: Boolean = False);

    property Count: Integer read FCount;
    property Items[Index: Integer]: TDeclaration read GetItem write SetItem; default;
    property ToArray: TDeclarationArray read GetToArray;
  end;

  TDeclaration = class({$IFDEF PARSER_LEAK_CHECKS}TLeakChecker{$ELSE}TObject{$ENDIF})
  protected
    FParser: TCodeParser;
    FLexer: TPasLexer;

    FDocPos: TDocPos;
    FStartPos: Integer;
    FEndPos: Integer;

    FParent: TDeclaration;
    FItems: TDeclarationList;

    FHeader: TStringCache;
    FName: TStringCache;
    FText: TStringCache;
    FTextNoComments: TStringCache;
    FTextNoCommentsSingleLine: TStringCache;

    function GetParentByClass(AClass: TDeclarationClass): TDeclaration;
    function GetText: String;
    function GetTextNoComments: String;
    function GetTextNoCommentsSingleLine: String;
    function GetName: String; virtual;
    function GetFullName: String; virtual;
    function GetHeader: String; virtual;

    procedure SetName(const Value: String); virtual;
  public
    function Dump: String; virtual;
    function DumpTree: String; virtual;

    function IsName(const Value: String): Boolean;

    // character indices in Lexer.Doc
    property StartPos: Integer read FStartPos;
    property EndPos: Integer read FEndPos;

    property DocPos: TDocPos read FDocPos;

    property Parser: TCodeParser read FParser;
    property Parent: TDeclaration read FParent;
    property ParentByClass[AClass: TDeclarationClass]: TDeclaration read GetParentByClass;

    property Items: TDeclarationList read FItems;
    property Name: String read GetName write SetName;
    property FullName: String read GetFullName;
    property Header: String read GetHeader;

    property Text: String read GetText;
    property TextNoComments: String read GetTextNoComments;
    property TextNoCommentsSingleLine: String read GetTextNoCommentsSingleLine;

    constructor Create(AParser: TCodeParser; AParent: TDeclaration; AStart: Integer; AEnd: Integer); virtual; reintroduce;
    destructor Destroy; override;
  end;

  TDeclarationArrayHelper = type helper for TDeclarationArray
  public
    procedure Add(const Decl: TDeclaration); overload;
    procedure Add(const Decls: TDeclarationArray); overload;
    procedure Remove(const Decl: TDeclaration);

    function GetByName(Name: String): TDeclarationArray;
    function GetByClassAndName(Name: String; DeclClass: TDeclarationClass; ExactClass: Boolean = False): TDeclarationArray;
    function GetByClass(DeclClass: TDeclarationClass): TDeclarationArray;
  end;

  TDeclaration_Root = class(TDeclaration)
  public
    constructor Create; reintroduce;
  end;

  TDeclaration_Keyword = class(TDeclaration)
  public
    constructor Create(Keyword: String); reintroduce;
  end;

  TDeclaration_Anchor = class(TDeclaration)
  protected
    function GetHeader: String; override;
  end;

  TDeclaration_WithStatement = class(TDeclaration);
  TDeclaration_WithVariableList = class(TDeclaration);
  TDeclaration_WithVariable = class(TDeclaration);

  TDeclaration_Stub = class(TDeclaration);
  TDeclaration_TypeStub = class(TDeclaration_Stub)
  public
    TempName: String;
  end;
  TDeclaration_VarStub = class(TDeclaration_Stub)
  public
    NameCount: Integer;
    Names: array of record
      Name: String;
      StartPos: Integer;
      EndPos: Integer;
    end;

    DefToken: ELexerToken;
  end;
  TDeclaration_ParamStub = class(TDeclaration_VarStub)
  public
    ParamType: ELexerToken;
  end;
  TDeclaration_ParamList = class(TDeclaration);
  TDeclaration_ParamGroup = class(TDeclaration);

  TDeclaration_Identifier = class(TDeclaration);

  TDeclaration_Type = class(TDeclaration)
  protected
    function GetHeader: String; override;
  end;

  TDeclaration_TypeRecord = class(TDeclaration_Type)
  protected
    FFields: TDeclarationCache;
    FConsts: TDeclarationCache;

    function GetFields: TDeclarationArray;
    function GetConsts: TDeclarationArray;
  public
    property Fields: TDeclarationArray read GetFields;
    property Consts: TDeclarationArray read GetConsts;
  end;

  TDeclaration_TypeArray = class(TDeclaration_Type)
  public
    function Dump: String; override;
    function DimCount: Integer;
    function VarType: TDeclaration;
  end;
  TDeclaration_TypeStaticArray = class(TDeclaration_TypeArray);

  TDeclaration_TypePointer = class(TDeclaration_Type)
  public
    function VarType: TDeclaration;
  end;

  TDeclaration_TypeCopy = class(TDeclaration_Type)
  public
    function VarType: TDeclaration;
  end;

  TDeclaration_TypeAlias = class(TDeclaration_Type)
  public
    function Dump: String; override;

    function VarType: TDeclaration;
  end;

  TDeclaration_EnumElement = class(TDeclaration)
  protected
    function GetName: string; override;
    function GetFullName: String; override;
    function GetHeader: String; override;
  end;

  TDeclaration_EnumElementName = class(TDeclaration)
  protected
    function GetName: string; override;
  end;

  TDeclaration_TypeEnum = class(TDeclaration_Type)
  protected
    FElements: TDeclarationCache;

    function GetElements: TDeclarationArray;
  public
    property Elements: TDeclarationArray read GetElements;
  end;
  TDeclaration_TypeEnumScoped = class(TDeclaration_TypeEnum);

  TDeclaration_TypeSet = class(TDeclaration_Type)
  protected
    FEnumElements: TDeclarationCache;

    function GetEnumElements: TDeclarationArray;
  public
    property EnumElements: TDeclarationArray read GetEnumElements;
  end;

  TDeclaration_Method = class;

  TDeclaration_TypeMethod = class(TDeclaration_Type)
  protected
    function GetMethod: TDeclaration_Method;
  public
    property Method: TDeclaration_Method read GetMethod;
  end;

  TDeclaration_TypeFakeGeneric = class(TDeclaration_Type);
  TDeclaration_TypeNativeMethod = class(TDeclaration_Type);

  TDeclaration_TypeRange = class(TDeclaration_Type);
  TDeclaration_TypeUnion = class(TDeclaration_Type);

  TDeclaration_VarType = class(TDeclaration);
  TDeclaration_VarDefault = class(TDeclaration);
  TDeclaration_Var = class(TDeclaration)
  protected
    FVarTypeString: TStringCache;
    FVarDefaultString: TStringCache;

    function GetHeader: String; override;

    function GetVarType: TDeclaration;
    function GetVarTypeString: String;
    function GetVarDefaultString: String;
  public
    DefToken: ELexerToken;

    property VarType: TDeclaration read GetVarType;
    property VarTypeString: String read GetVarTypeString;
    property VarDefaultString: String read GetVarDefaultString;
  end;

  TDeclaration_VarClass = class of TDeclaration_Var;
  TDeclaration_Const = class(TDeclaration_Var)
  protected
    function GetHeader: String; override;
  end;

  TDeclaration_Field = class(TDeclaration_Var)
  public
    function Dump: String; override;
  end;

  TDeclaration_Method = class(TDeclaration)
  protected
  type
    EMethodType = (mtProc, mtFunc, mtOperator, mtProperty);
    EMethodDirectives = set of (mdStatic, mdOverload, mdOverride);
  protected
    FMethodType: EMethodType;
    FMethodDirectives: EMethodDirectives;

    FParams: TDeclarationCache;

    FParamString: TStringCache;
    FResultString: TStringCache;

    function GetParamString: String;
    function GetResultString: String;
    function GetHeader: String; override;
    function GetParamCount: Integer;
    function GetParams: TDeclarationArray;
    function GetParamVarType(Index: Integer): TDeclaration;
  public
    function isFunc: Boolean;
    function isProc: Boolean;
    function isOperator: Boolean;
    function isProperty: Boolean;

    function isStatic: Boolean;
    function isOverload: Boolean;
    function isOverride: Boolean;

    function ResultType: TDeclaration;
    function Dump: String; override;

    property ParamString: String read GetParamString;
    property ResultString: String read GetResultString;
    property ParamCount: Integer read GetParamCount;
    property Params: TDeclarationArray read GetParams;
    property ParamVarType[Index: Integer]: TDeclaration read GetParamVarType;
  end;

  TDeclaration_MethodOfType = class(TDeclaration_Method)
  protected
    FObjectName: String;

    function GetFullName: String; override;
    function GetHeader: String; override;
  public
    property ObjectName: String read FObjectName;
  end;

  TDeclaration_Property = class(TDeclaration_MethodOfType)
  protected
    function GetIsRead: Boolean;
    function GetIsWrite: Boolean;
    function GetVarType: TDeclaration;
  public
    function Dump: String; override;

    property IsRead: Boolean read GetIsRead;
    property IsWrite: Boolean read GetIsWrite;
    property VarType: TDeclaration read GetVarType;
  end;

  TDeclaration_MethodResult = class(TDeclaration_Var)
  protected
    function GetName: string; override;
  end;

  TDeclaration_MethodObjectName = class(TDeclaration_Var)
  public
    function GetName: string; override;
  end;

  TDeclaration_Parameter = class(TDeclaration_Var)
  public
    ParamType: ELexerToken;

    function Dump: String; override;
  end;

  THandleIncludeEvent = procedure(Sender: TPasLexer) of object;
  THandlePluginEvent = procedure(Sender: TPasLexer) of object;

  {$PUSH}
  {$SCOPEDENUMS ON}
  EParserSourceType = (SCRIPT, INCLUDE, PLUGIN);
  {$POP}

  TSymbolTable = record
  private
  type
    TItem = record
      Name: String;
      NameHash: UInt32;
      Count: Integer;
      Decls: TDeclarationArray;
    end;
  public
    Count: Integer;
    Items: array of TItem;
    TotalCount: Integer;

    function IndexOf(Name: String): Integer;

    function Copy: TSymbolTable;
    function Get(Name: String): TDeclarationArray;

    procedure Add(Name: String; Decl: TDeclaration); overload;
    procedure Add(Decls: TDeclarationArray); overload;
    procedure Clear;

    procedure CopyFrom(Other: TSymbolTable);
  end;

  TCodeParser = class(TPasParser)
  protected
    FSourceType: EParserSourceType;
    FManagedItems: TDeclarationList;

    FRoot: TDeclaration;
    FItems: TDeclarationList;
    FStack: TDeclarationStack;
    FSymbolTable: TSymbolTable;
    FOnHandleInclude: THandleIncludeEvent;
    FOnHandlePlugin: THandlePluginEvent;

    FCaretPos: Integer; // for locals

    FHash: TStringCache;

    FSkipErrorMessages: Boolean;

    procedure OnErrorMessage(Sender: TPasLexer; Message: String); override;

    // Hashes lexers filenames, fileage and defines.
    function GetHash: String; virtual;

    function PushStack(const AClass: TDeclarationClass): TDeclaration; inline;
    function PushStub(const AClass: TDeclarationClass): TDeclaration; inline;
    procedure PopStack; inline;

    procedure EmptyVarStub(const VarStub: TDeclaration_VarStub; const VarClass: TDeclaration_VarClass);
    procedure EmptyParamStub(const ParamStub: TDeclaration_ParamStub);

    function InDeclaration(const AClassType: TDeclarationClass): Boolean; inline;
    function InDeclaration(const AClassType1, AClassType2: TDeclarationClass): Boolean; overload;
    function InDeclaration(const AClassType1, AClassType2, AClassType3: TDeclarationClass): Boolean; overload;
    function InDeclaration(const AClassType1, AClassType2, AClassType3, AClassType4: TDeclarationClass): Boolean; overload;

    procedure OnLibraryDirect(Sender: TPasLexer); override;
    procedure OnIncludeDirect(Sender: TPasLexer); override;

    procedure Anchor; override;

    procedure TypeKind; override;

    // with statement
    procedure WithStatement; override;
    procedure VariableList; override;
    procedure Variable; override;

    // var / consts
    procedure VarDeclaration; override;
    procedure VarName; override;
    procedure ConstantDeclaration; override;
    procedure ConstantName; override;
    procedure ConstantType; override;
    procedure ConstantValue; override;
    procedure ConstantExpression; override;

    // method
    procedure MethodDirective; override;
    procedure Method; override;
    procedure MethodOfType; override;
    procedure MethodName; override;
    procedure MethodTypeName; override;
    procedure MethodResultType; override;

    // params
    procedure ParameterName; override;
    procedure ParameterVarType; override;
    procedure ParameterSection; override;
    procedure Parameters; override;

    // types
    procedure TypeIdentifer; override;
    procedure TypeDeclaration; override;
    procedure TypeName; override;

    // types - alias
    procedure TypeAlias; override;

    // types - copy
    procedure TypeCopy; override;

    // types - pointer
    procedure PointerType; override;

    // types - proc
    procedure ProceduralType; override;

    // types - array
    procedure ArrayType; override;
    procedure ArrayTypeStatic; override;

    // types - native
    procedure NativeType; override;

    procedure FakeGenericType; override;

    // types = record
    procedure UnionType; override;
    procedure RecordType; override;
    procedure RecordField; override;
    procedure FieldName; override;

    // types - set
    procedure SetType; override;

    // types - enum
    procedure EnumType; override;
    procedure EnumTypeScoped; override;
    procedure EnumElement; override;
    procedure EnumElementName; override;
  public
    property SourceType: EParserSourceType read FSourceType write FSourceType;

    property Root: TDeclaration read FRoot;
    property Items: TDeclarationList read FItems;

    property OnHandleInclude: THandleIncludeEvent read FOnHandleInclude write FOnHandleInclude;
    property OnHandlePlugin: THandlePluginEvent read FOnHandlePlugin write FOnHandlePlugin;

    property SkipErrorMessages: Boolean read FSkipErrorMessages write FSkipErrorMessages;
    property CaretPos: Integer read FCaretPos write FCaretPos;
    property SymbolTable: TSymbolTable read FSymbolTable;
    property Hash: String read GetHash;

    procedure Reset; override;
    procedure Run; override;

    constructor Create; override;
    destructor Destroy; override;
  end;

  TCodeParserArray = array of TCodeParser;
  TCodeParserList = specialize TSimbaObjectList<TCodeParser>;

  procedure BuildSymbolTable(var Table: TSymbolTable; From: TCodeParser);

  function DeclarationKind(Decl: TDeclaration): ShortString;
  function DeclarationImage(Decl: TDeclaration): Integer;
  function RemoveDuplicateProperties(Decls: TDeclarationArray): TDeclarationArray;
  function PropertyHeader(Decl: TDeclaration_Property; FullHeader: Boolean = True): String;
  function IndexableProperties(Decls: TDeclarationArray): TDeclarationArray;
  function RemoveOverridenMethods(Decls: TDeclarationArray): TDeclarationArray;

implementation

function TDeclarationList.GetByClass(const AClass: TDeclarationClass; const ExactClass: Boolean; const SubSearch: Boolean): TDeclarationArray;
var
  Size: Integer;

  procedure DoSearch(const Item: TDeclaration);
  var
    I: Integer;
  begin
    Assert(Item <> nil);
    if (Item.ClassType = AClass) or ((not ExactClass) and (Item is AClass)) then
    begin
      if (Size = Length(Result)) then
        SetLength(Result, 4 + (Length(Result) * 2));
      Result[Size] := Item;
      Inc(Size);
    end;

    if SubSearch then
      for I := 0 to Item.Items.Count - 1 do
        DoSearch(Item.Items[I]);
  end;

var
  I: Integer;
begin
  Size := 0;
  for I := 0 to Count - 1 do
    DoSearch(FItems[I]);

  SetLength(Result, Size);
end;

function TDeclarationList.GetByClassFirst(const AClass: TDeclarationClass; const ExactClass: Boolean; const SubSearch: Boolean): TDeclaration;

  procedure DoSearch(const Item: TDeclaration);
  var
    I: Integer;
  begin
    if (Item.ClassType = AClass) or ((not ExactClass) and (Item is AClass)) then
      Result := Item
    else
    if SubSearch then
      for I := 0 to Item.Items.Count - 1 do
      begin
        DoSearch(Item.Items[I]);
        if (Result <> nil) then
          Exit;
      end;
  end;

var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Count - 1 do
  begin
    DoSearch(FItems[I]);
    if (Result <> nil) then
      Exit;
  end;
end;

function TDeclarationList.GetByPosition(const Position: Integer): TDeclaration;

  procedure Search(Declaration: TDeclaration; var Result: TDeclaration);
  var
    I: Integer;
  begin
    if (Position >= Declaration.StartPos) and (Position <= Declaration.EndPos) then
    begin
      Result := Declaration;
      for I := 0 to Declaration.Items.Count - 1 do
        Search(Declaration.Items[I], Result);
    end;
  end;

var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Count - 1 do
    if (Position >= FItems[I].StartPos) and (Position <= FItems[I].EndPos) then
      Search(FItems[I], Result);
end;

function TDeclarationList.GetTextOfClass(AClass: TDeclarationClass): String;
var
  Declaration: TDeclaration;
begin
  Result := '';

  Declaration := GetByClassFirst(AClass);
  if Declaration <> nil then
    Result := Declaration.GetText();
end;

function TDeclarationList.GetTextOfClassNoComments(AClass: TDeclarationClass): String;
var
  Declaration: TDeclaration;
begin
  Result := '';

  Declaration := GetByClassFirst(AClass);
  if Declaration <> nil then
    Result := Declaration.GetTextNoComments();
end;

function TDeclarationList.GetTextOfClassNoCommentsSingleLine(AClass: TDeclarationClass; Prefix: String): String;
var
  Decl: TDeclaration;
begin
  Decl := GetByClassFirst(AClass);

  if (Decl <> nil) then
  begin
    Result := Decl.GetTextNoCommentsSingleLine();
    if (Result <> '') then
      Result := Prefix + Result;
  end else
    Result := '';
end;

function TDeclarationList.GetItem(const Index: Integer): TDeclaration;
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt('TDeclarationList.GetItem: Index %d out of bounds', [Index]);

  Result := FItems[Index];
end;

procedure TDeclarationList.SetItem(const Index: Integer; const Value: TDeclaration);
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt('TDeclarationList.SetItem: Index %d out of bounds', [Index]);

  FItems[Index] := Value;
end;

function TDeclarationList.GetToArray: TDeclarationArray;
begin
  SetLength(Result, FCount);
  if (FCount > 0) then
    Move(FItems[0], Result[0], FCount * SizeOf(TDeclaration));
end;

procedure TDeclarationList.Add(const Decl: TDeclaration);
begin
  if (FCount >= Length(FItems)) then
    SetLength(FItems, 4 + (Length(FItems) * 2));

  FItems[FCount] := Decl;
  Inc(FCount);
end;

procedure TDeclarationList.Extend(const Decls: TDeclarationArray);
begin
  if (Length(Decls) = 0) then
    Exit;

  if (FCount + Length(Decls) >= Length(FItems)) then
    SetLength(FItems, 4 + Length(Decls) + (Length(FItems) * 2));
  Move(Decls[0], FItems[FCount], Length(Decls) * SizeOf(TDeclaration));
  Inc(FCount, Length(Decls));
end;

procedure TDeclarationList.Clear(const FreeDecls: Boolean);
var
  I: Integer;
begin
  if FreeDecls then
    for I := 0 to FCount - 1 do
      FItems[I].Free();

  FCount := 0;
end;

function TDeclaration.GetText: String;
begin
  if FText.IsNull then
    FText := FLexer.CopyDoc(FStartPos, FEndPos);

  Result := FText;
end;

function TDeclaration.GetTextNoComments: String;

  function Filter(const Text: String): String;
  var
    Builder: TSimbaStringBuilder;
    Lexer: TPasLexer;
  begin
    Lexer := TPasLexer.Create(Text);
    Lexer.Next();
    while (Lexer.TokenID <> tokNull) do
    begin
      if (not (Lexer.TokenID in [tokSlashesComment, tokAnsiComment, tokBorComment])) then
        Builder.Append(Lexer.Token);

      Lexer.Next();
    end;
    Lexer.Free();

    Result := Builder.Str;
  end;

begin
  if FTextNoComments.IsNull then
    FTextNoComments := Filter(FLexer.CopyDoc(FStartPos, FEndPos));

  Result := FTextNoComments;
end;

function TDeclaration.GetTextNoCommentsSingleLine: String;

  function Filter(const Text: String): String;
  var
    Builder: TSimbaStringBuilder;
    Lexer: TPasLexer;
  begin
    Lexer := TPasLexer.Create(Text);
    Lexer.Next();
    while (Lexer.TokenID <> tokNull) do
    begin
      case Lexer.TokenID of
        tokWhiteSpace: Builder.Append(' ');
        tokAnsiComment, tokBorComment, tokSlashesComment: { nothing, dont add comments };
        else
          Builder.Append(Lexer.Token);
      end;

      Lexer.Next();
    end;
    Lexer.Free();

    Result := Builder.Str;
  end;

begin
  if FTextNoCommentsSingleLine.IsNull then
    FTextNoCommentsSingleLine := Filter(FLexer.CopyDoc(FStartPos, FEndPos));

  Result := FTextNoCommentsSingleLine;
end;

function TDeclaration.GetName: String;
begin
  Result := FName.ValueOrDefault;
end;

function TDeclaration.GetFullName: String;
begin
  Result := Name;
end;

function TDeclaration.GetHeader: String;
begin
  Result := Text; // default to text
end;

procedure TDeclaration.SetName(const Value: String);
begin
  FName := Value;
end;

function TDeclaration.Dump: String;
begin
  if (Name = '') then
    Result := ClassName
  else
    Result := ClassName + ' (' + Name + ')';
end;

function TDeclaration.DumpTree: String;
var
  Builder: TSimbaStringBuilder;
  Depth: Integer = 1;

  procedure DoDump(Decl: TDeclaration);
  var
    I: Integer;
  begin
    Builder.AppendLine(StringOfChar('-', Depth*2) + ' ' + Decl.Dump());

    Inc(Depth);
    for I := 0 to Decl.Items.Count - 1 do
      DoDump(Decl.Items[I]);
    Dec(Depth);

    if (Decl.Items.Count > 0) then
      Builder.AppendLine(StringOfChar('-', Depth*2) + ' ' + Decl.Dump());
  end;

begin
  DoDump(Self);

  Result := Builder.Str;
end;

function TDeclaration.IsName(const Value: String): Boolean;
begin
  Result := SameText(Name, Value);
end;

function TDeclaration.GetParentByClass(AClass: TDeclarationClass): TDeclaration;
var
  Decl: TDeclaration;
begin
  Decl := FParent;

  while (Decl <> nil) do
  begin
    if (Decl is AClass) then
    begin
      Result := Decl;
      Exit;
    end;

    Decl := Decl.Parent;
  end;

  Result := nil;
end;

constructor TDeclaration.Create(AParser: TCodeParser; AParent: TDeclaration; AStart: Integer; AEnd: Integer);
begin
  inherited Create();

  FParser := AParser;
  if (FParser <> nil) then
  begin
    FDocPos := FParser.Lexer.DocPos;
    FLexer := FParser.Lexer;
    FParser.FManagedItems.Add(Self);
  end;

  FParent := AParent;
  FStartPos := AStart;
  FEndPos := AEnd;

  FItems := TDeclarationList.Create();
end;

destructor TDeclaration.Destroy;
begin
  if (FItems <> nil) then
    FreeAndNil(FItems);

  inherited Destroy();
end;

procedure TDeclarationArrayHelper.Add(const Decl: TDeclaration);
begin
  Self += [Decl];
end;

procedure TDeclarationArrayHelper.Add(const Decls: TDeclarationArray);
begin
  Self += Decls;
end;

procedure TDeclarationArrayHelper.Remove(const Decl: TDeclaration);
var
  I: Integer;
begin
  for I := 0 to High(Self) do
    if (Self[I] = Decl) then
    begin
      Delete(Self, I, 1);
      Exit;
    end;
end;

function TDeclarationArrayHelper.GetByName(Name: String): TDeclarationArray;
var
  I, Count: Integer;
begin
  SetLength(Result, Length(Self));
  Count := 0;

  for I := 0 to High(Self) do
    if Self[I].IsName(Name) then
    begin
      Result[Count] := Self[I];
      Inc(Count);
    end;

  SetLength(Result, Count);
end;

function TDeclarationArrayHelper.GetByClassAndName(Name: String; DeclClass: TDeclarationClass; ExactClass: Boolean): TDeclarationArray;
var
  I, Count: Integer;
begin
  SetLength(Result, Length(Self));
  Count := 0;

  for I := 0 to High(Self) do
    if Self[I].IsName(Name) and ((ExactClass and (Self[I].ClassType = DeclClass)) or ((not ExactClass) and (Self[I] is DeclClass))) then
    begin
      Result[Count] := Self[I];
      Inc(Count);
    end;

  SetLength(Result, Count);
end;

function TDeclarationArrayHelper.GetByClass(DeclClass: TDeclarationClass): TDeclarationArray;
var
  I, Count: Integer;
begin
  Count := 0;

  SetLength(Result, Length(Self));
  for I := 0 to High(Self) do
    if (Self[I] is DeclClass) then
    begin
      Result[Count] := Self[I];
      Inc(Count);
    end;
  SetLength(Result, Count);
end;

function TDeclaration_TypeMethod.GetMethod: TDeclaration_Method;
begin
  Result := TDeclaration_Method(FItems.GetByClassFirst(TDeclaration_Method));
end;

function TDeclaration_MethodResult.GetName: string;
begin
  Result := 'Result';
end;

function TDeclaration_MethodObjectName.GetName: string;
begin
  Result := 'Self';
end;

constructor TDeclaration_Root.Create;
begin
  inherited Create(nil, nil, 0, 0);
end;

constructor TDeclaration_Keyword.Create(Keyword: String);
begin
  inherited Create(nil, nil, 0, 0);

  FName := Keyword;
end;

function TDeclaration_Anchor.GetHeader: String;
begin
  if FHeader.IsNull then
    FHeader := 'Anchor "' + Name + '"';

  Result := FHeader;
end;

function TDeclaration_Type.GetHeader: String;
begin
  if FHeader.IsNull then
    FHeader := 'type ' + Name + ' = ' + TextNoCommentsSingleLine;

  Result := FHeader;
end;

function TDeclaration_EnumElement.GetName: string;
begin
  if FName.IsNull then
    FName := Items.GetTextOfClass(TDeclaration_EnumElementName);

  Result := inherited;
end;

function TDeclaration_EnumElement.GetFullName: String;
begin
  if (FParent is TDeclaration_TypeEnumScoped) then
    Result := FParent.Name + '.' + inherited
  else
    Result := inherited;
end;

function TDeclaration_EnumElement.GetHeader: String;
begin
  if FHeader.IsNull then
    FHeader := IfThen(FParent is TDeclaration_TypeEnumScoped, FParent.Name + '.' + Name, Name);

  Result := FHeader;
end;

function TDeclaration_TypeAlias.Dump: String;
begin
  Result := inherited + ' [' + Items.GetTextOfClassNoCommentsSingleLine(TDeclaration_VarType) + ']';
end;

function TDeclaration_TypeAlias.VarType: TDeclaration;
begin
  Result := Items.GetByClassFirst(TDeclaration_VarType);
end;

function TDeclaration_TypeCopy.VarType: TDeclaration;
begin
  Result := Items.GetByClassFirst(TDeclaration_VarType);
end;

function TDeclaration_TypePointer.VarType: TDeclaration;
begin
  Result := Items.GetByClassFirst(TDeclaration_VarType);
end;

function TDeclaration_TypeArray.Dump: String;
begin
  Result := inherited + ' [' + Items.GetTextOfClassNoCommentsSingleLine(TDeclaration_VarType) + '] <' + IntToStr(DimCount) + '>';
end;

function TDeclaration_TypeArray.DimCount: Integer;
var
  Decl: TDeclaration;
begin
  Result := 1;

  Decl := Items.GetByClassFirst(TDeclaration_TypeArray);
  while (Decl <> nil) and (Decl.Parent is TDeclaration_TypeArray) do
  begin
    Decl := Decl.Items.GetByClassFirst(TDeclaration_TypeArray);

    Inc(Result);
  end;
end;

function TDeclaration_TypeArray.VarType: TDeclaration;
begin
  Result := Items.GetByClassFirst(TDeclaration_VarType);
end;

function TDeclaration_TypeRecord.GetFields: TDeclarationArray;
begin
  if FFields.IsNull then
    FFields := Items.GetByClass(TDeclaration_Field, True);

  Result := FFields;
end;

function TDeclaration_TypeRecord.GetConsts: TDeclarationArray;
begin
  if FConsts.IsNull then
    FConsts := Items.GetByClass(TDeclaration_Const, True);

  Result := FConsts;
end;

function TDeclaration_Var.GetHeader: String;
begin
  if FHeader.IsNull then
    FHeader := 'var ' + Name + VarTypeString + VarDefaultString;

  Result := FHeader;
end;

function TDeclaration_Var.GetVarType: TDeclaration;
begin
  Result := Items.GetByClassFirst(TDeclaration_VarType);
end;

function TDeclaration_Var.GetVarTypeString: String;
begin
  if FVarTypeString.IsNull then
    FVarTypeString := Items.GetTextOfClassNoCommentsSingleLine(TDeclaration_VarType, ': ');

  Result := FVarTypeString;
end;

function TDeclaration_Var.GetVarDefaultString: String;

  function ReplaceUnPrintable(const Str: String): String;
  var
    I: Integer = 1;
  begin
    Result := Str;
    while (I <= Length(Result)) do
    begin
      if (Result[I] < #32) then
      begin
        Insert(IntToStr(Byte(Result[I])), Result, I+1);
        Result[I] := '#';
      end;

      Inc(I);
    end;
  end;

begin
  if FVarDefaultString.IsNull then
  begin
    case DefToken of
      tokAssign: FVarDefaultString := Items.GetTextOfClassNoCommentsSingleLine(TDeclaration_VarDefault, ' := ');
      tokEqual:  FVarDefaultString := Items.GetTextOfClassNoCommentsSingleLine(TDeclaration_VarDefault, ' = ');
      else
        FVarDefaultString := '';
    end;
    FVarDefaultString := ReplaceUnPrintable(FVarDefaultString);
  end;

  Result := FVarDefaultString;
end;

function TDeclaration_Const.GetHeader: String;
begin
  if FHeader.IsNull then
    FHeader := 'const ' + Name + VarDefaultString;

  Result := FHeader;
end;

function TDeclaration_Field.Dump: String;
begin
  Result := inherited;
  if Parent is TDeclaration_TypeRecord then
    Result := Result + ' [' + Parent.Name + ']';
end;

function TDeclaration_EnumElementName.GetName: string;
begin
  if FName.IsNull then
    FName := Text;

  Result := FName;
end;

function TDeclaration_TypeEnum.GetElements: TDeclarationArray;
begin
  if FElements.IsNull then
    FElements := FItems.GetByClass(TDeclaration_EnumElement);

  Result := FElements;
end;

function TDeclaration_TypeSet.GetEnumElements: TDeclarationArray;
begin
  if FEnumElements.IsNull then
    FEnumElements := FItems.GetByClass(TDeclaration_EnumElement);

  Result := FEnumElements;
end;

function TDeclaration_Method.ResultType: TDeclaration;
begin
  Result := Items.GetByClassFirst(TDeclaration_MethodResult);
  if (Result <> nil) then
    Result := Result.Items.GetByClassFirst(TDeclaration_VarType);
end;

function TDeclaration_Method.Dump: String;
begin
  Result := inherited;

  if isProc then
    Result := Result + ' [procedure]';
  if isFunc then
    Result := Result + ' [function]';
  if isOperator then
    Result := Result + ' [operator]';
  if isProperty then
    Result := Result + ' [property]';
  if isOverride then
    Result := Result + ' [override]';
  if isOverload then
    Result := Result + ' [overload]';
  if isStatic then
    Result := Result + ' [static]';
end;

function TDeclaration_Method.isOperator: Boolean;
begin
  Result := FMethodType = mtOperator;
end;

function TDeclaration_Method.isProperty: Boolean;
begin
  Result := FMethodType = mtProperty;
end;

function TDeclaration_Method.isStatic: Boolean;
begin
  Result := mdStatic in FMethodDirectives;
end;

function TDeclaration_Method.isOverload: Boolean;
begin
  Result := mdOverload in FMethodDirectives;
end;

function TDeclaration_Method.isOverride: Boolean;
begin
  Result := mdOverride in FMethodDirectives;
end;

function TDeclaration_Method.isFunc: Boolean;
begin
  Result := FMethodType = mtFunc;
end;

function TDeclaration_Method.isProc: Boolean;
begin
  Result := FMethodType = mtProc;
end;

function TDeclaration_Method.GetParamVarType(Index: Integer): TDeclaration;
begin
  if (Index >= 0) and (Index < ParamCount) then
    Result := Params[Index].Items.GetByClassFirst(TDeclaration_VarType)
  else
    Result := nil;
end;

function TDeclaration_Method.GetParamString: String;
begin
  if FParamString.IsNull then
    FParamString := FItems.GetTextOfClassNoCommentsSingleLine(TDeclaration_ParamList);

  Result := FParamString;
end;

function TDeclaration_Method.GetResultString: String;
begin
  if FResultString.IsNull then
    FResultString := FItems.GetTextOfClassNoCommentsSingleLine(TDeclaration_MethodResult, ': ');

  Result := FResultString;
end;

function TDeclaration_Method.GetHeader: String;
var
  Builder: TSimbaStringBuilder;
begin
  if FHeader.IsNull then
  begin
    if isFunc     then Builder.Append('function')  else
    if isProc     then Builder.Append('procedure') else
    if isOperator then Builder.Append('operator')  else
    if isProperty then Builder.Append('property');

    Builder.Append(' ');
    Builder.Append(Name);
    Builder.Append(ParamString);
    Builder.Append(ResultString);

    FHeader := Builder.Str;
  end;

  Result := FHeader;
end;

function TDeclaration_Method.GetParamCount: Integer;
begin
  Result := Length(Params);
end;

function TDeclaration_Method.GetParams: TDeclarationArray;
var
  Decl: TDeclaration;
begin
  if FParams.IsNull then
  begin
    FParams := [];

    Decl := Items.GetByClassFirst(TDeclaration_ParamList);
    if (Decl <> nil) then
      for Decl in Decl.Items.GetByClass(TDeclaration_ParamGroup) do
        FParams.Value.Add(Decl.Items.GetByClass(TDeclaration_Parameter));
  end;

  Result := FParams;
end;

function TDeclaration_MethodOfType.GetFullName: String;
begin
  Result := FObjectName + '.' + Name;
end;

function TDeclaration_MethodOfType.GetHeader: String;
var
  Builder: TSimbaStringBuilder;
begin
  if FHeader.IsNull then
  begin
    if isFunc     then Builder.Append('function')  else
    if isProc     then Builder.Append('procedure') else
    if isOperator then Builder.Append('operator')  else
    if isProperty then Builder.Append('property');

    Builder.Append(' ');
    Builder.Append(ObjectName + '.' + Name);
    Builder.Append(ParamString);
    Builder.Append(ResultString);

    FHeader := Builder.Str;
  end;

  Result := FHeader;
end;

function TDeclaration_Property.GetIsRead: Boolean;
begin
  Result := ResultType <> nil;
end;

function TDeclaration_Property.GetIsWrite: Boolean;
begin
  Result := ResultType = nil;
end;

function TDeclaration_Property.GetVarType: TDeclaration;
begin
  Result := ResultType;
  if (Result = nil) then
    Result := ParamVarType[0];
end;

function TDeclaration_Property.Dump: String;
begin
  Result := inherited;

  if IsRead then
    Result := Result + ' <read>';
  if IsWrite then
    Result := Result + ' <write>';
end;

function TDeclaration_Parameter.Dump: String;
begin
  Result := inherited;

  case ParamType of
    tokConst:    Result := Result + ' [const]';
    tokConstRef: Result := Result + ' [constref]';
    tokVar:      Result := Result + ' [var]';
    tokOut:      Result := Result + ' [out]';
    tokUnknown:  Result := Result + ' [normal]';
  end;
end;

function TCodeParser.InDeclaration(const AClassType: TDeclarationClass): Boolean;
begin
  Result := (FStack.Top is AClassType);
end;

function TCodeParser.InDeclaration(const AClassType1, AClassType2: TDeclarationClass): Boolean;
begin
  Result := InDeclaration(AClassType1) or InDeclaration(AClassType2);
end;

function TCodeParser.InDeclaration(const AClassType1, AClassType2, AClassType3: TDeclarationClass): Boolean;
begin
  Result := InDeclaration(AClassType1) or InDeclaration(AClassType2) or InDeclaration(AClassType3);
end;

function TCodeParser.InDeclaration(const AClassType1, AClassType2, AClassType3, AClassType4: TDeclarationClass): Boolean;
begin
  Result := InDeclaration(AClassType1) or InDeclaration(AClassType2) or InDeclaration(AClassType3) or InDeclaration(AClassType4);
end;

function TCodeParser.PushStack(const AClass: TDeclarationClass): TDeclaration;
begin
  Result := AClass.Create(Self, FStack.Top, FLexer.TokenPos, FLexer.TokenPos);

  FStack.Top.Items.Add(Result);
  FStack.Push(Result);
end;

function TCodeParser.PushStub(const AClass: TDeclarationClass): TDeclaration;
begin
  Result := AClass.Create(Self, FStack.Top, FLexer.TokenPos, FLexer.TokenPos);

  FStack.Push(Result);
end;

procedure TCodeParser.PopStack();
begin
  FStack.Pop().FEndPos := fLastNoJunkPos;
end;

procedure TCodeParser.OnErrorMessage(Sender: TPasLexer; Message: String);
begin
  if FSkipErrorMessages then
    Exit;

  if (Sender = nil) then
    CodetoolsMessage(Message)
  else
  if (fLexers.Count = 0) or (Sender <> fLexers[0]) then
    CodetoolsMessage(Message, Sender.DocPos.Line + 1, Sender.DocPos.Col, Sender.DocPos.FileName)
  else
  if (FCaretPos = -1) or (Sender.TokenPos < FCaretPos) then // beyond caret, dont report
    CodetoolsMessage(Message, Sender.DocPos.Line + 1, Sender.DocPos.Col, Sender.DocPos.FileName);
end;

function TCodeParser.GetHash: String;
var
  Builder: TSimbaStringBuilder;
  I: Integer;
begin
  if FHash.IsNull then
  begin
    with Lexer.SaveDefines() do
      Builder.Append(Defines + IntToStr(Stack));
    for I := 0 to fLexers.Count - 1 do
      Builder.Append(fLexers[i].FileName + IntToStr(fLexers[i].FileAge));

    FHash := Builder.Str;
  end;

  Result := FHash;
end;

procedure TCodeParser.EmptyVarStub(const VarStub: TDeclaration_VarStub; const VarClass: TDeclaration_VarClass);
var
  VarType, VarDefault: TDeclaration;
  NewDecl: TDeclaration_Var;
  I: Integer;
begin
  VarType := VarStub.Items.GetByClassFirst(TDeclaration_VarType);
  VarDefault := VarStub.Items.GetByClassFirst(TDeclaration_VarDefault);

  for I := 0 to VarStub.NameCount - 1 do
  begin
    NewDecl := VarClass.Create(Self, FStack.Top, FLexer.TokenPos, FLexer.TokenPos);
    NewDecl.Name := VarStub.Names[I].Name;
    if (VarType <> nil) then
      NewDecl.Items.Add(VarType);
    if (VarDefault <> nil) then
      NewDecl.Items.Add(VarDefault);

    NewDecl.DefToken  := VarStub.DefToken;
    NewDecl.FStartPos := VarStub.Names[I].StartPos;
    NewDecl.FEndPos   := VarStub.Names[I].EndPos;

    FStack.Top.Items.Add(NewDecl);
  end;
end;

procedure TCodeParser.EmptyParamStub(const ParamStub: TDeclaration_ParamStub);
var
  VarType, VarDefault: TDeclaration;
  NewDecl: TDeclaration_Parameter;
  I: Integer;
begin
  VarType := ParamStub.Items.GetByClassFirst(TDeclaration_VarType);
  VarDefault := ParamStub.Items.GetByClassFirst(TDeclaration_VarDefault);

  for I := 0 to ParamStub.NameCount - 1 do
  begin
    NewDecl := TDeclaration_Parameter.Create(Self, FStack.Top, FLexer.TokenPos, FLexer.TokenPos);
    NewDecl.Name := ParamStub.Names[I].Name;
    if (VarType <> nil) then
      NewDecl.Items.Add(VarType);
    if (VarDefault <> nil) then
      NewDecl.Items.Add(VarDefault);

    NewDecl.DefToken  := ParamStub.DefToken;
    NewDecl.ParamType := ParamStub.ParamType;
    NewDecl.FStartPos := ParamStub.Names[I].StartPos;
    NewDecl.FEndPos   := ParamStub.Names[I].EndPos;

    FStack.Top.Items.Add(NewDecl);
  end;
end;

constructor TCodeParser.Create;
begin
  inherited Create();

  FManagedItems := TDeclarationList.Create();

  FRoot := TDeclaration_Root.Create();
  FStack := TDeclarationStack.Create();
  FStack.Push(FRoot);

  FItems := FRoot.Items;
  FCaretPos := -1;
end;

destructor TCodeParser.Destroy;
begin
  Reset();

  FManagedItems.Free();
  FStack.Free();
  FRoot.Free();

  inherited Destroy();
end;

procedure TCodeParser.OnLibraryDirect(Sender: TPasLexer);
begin
  if Assigned(FOnHandlePlugin) then
    FOnHandlePlugin(Sender);
end;

procedure TCodeParser.OnIncludeDirect(Sender: TPasLexer);
begin
  if Assigned(FOnHandleInclude) then
    FOnHandleInclude(Sender);
end;

procedure TCodeParser.Anchor;
var
  Decl: TDeclaration_Anchor;
begin
  Decl := TDeclaration_Anchor.Create(Self, FRoot, FLexer.TokenPos, FLexer.TokenPos);
  Decl.Name := Lexer.DirectiveParamOriginal;

  FRoot.Items.Add(Decl);

  inherited;
end;

procedure TCodeParser.WithStatement;
begin
  PushStack(TDeclaration_WithStatement);
  inherited;
  PopStack();
end;

procedure TCodeParser.VariableList;
begin
  PushStack(TDeclaration_WithVariableList);
  inherited;
  PopStack();
end;

procedure TCodeParser.Variable;
begin
  if (not InDeclaration(TDeclaration_WithVariableList)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TDeclaration_WithVariable);
  inherited;
  PopStack();
end;

procedure TCodeParser.ConstantType;
begin
  if (not InDeclaration(TDeclaration_VarStub)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TDeclaration_VarType);
  inherited;
  PopStack();
end;

procedure TCodeParser.ConstantValue;
begin
  if (not InDeclaration(TDeclaration_VarStub)) then
  begin
    inherited;
    Exit;
  end;

  TDeclaration_VarStub(FStack.Top).DefToken := fLastNoJunkTok;

  PushStack(TDeclaration_VarDefault);
  inherited;
  PopStack();
end;

procedure TCodeParser.ConstantExpression;
begin
  if (not InDeclaration(TDeclaration_ParamStub)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TDeclaration_VarDefault);
  inherited;
  PopStack();
end;

procedure TCodeParser.TypeKind;
begin
  if (not InDeclaration(TDeclaration_VarStub, TDeclaration_TypeArray, TDeclaration_MethodResult, TDeclaration_MethodObjectName)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TDeclaration_VarType);
  inherited;
  PopStack();
end;

procedure TCodeParser.ProceduralType;
begin
  if (not InDeclaration(TDeclaration_TypeStub, TDeclaration_VarType)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TDeclaration_TypeMethod);
  PushStack(TDeclaration_Method);
  inherited;
  PopStack();
  PopStack();
end;

procedure TCodeParser.MethodDirective;
begin
  if InDeclaration(TDeclaration_Method) then
    with TDeclaration_Method(FStack.Top) do
      case Lexer.TokenID of
        tokOverload: Include(FMethodDirectives, mdOverload);
        tokOverride: Include(FMethodDirectives, mdOverride);
        tokStatic:   Include(FMethodDirectives, mdStatic);
      end;

  inherited;
end;

procedure TCodeParser.TypeAlias;
begin
  PushStack(TDeclaration_TypeAlias);
  PushStack(TDeclaration_VarType);
  inherited;
  PopStack();
  PopStack();
end;

procedure TCodeParser.TypeIdentifer;
begin
  PushStack(TDeclaration_Identifier).Name := Lexer.Token;
  inherited;
  PopStack();
end;

procedure TCodeParser.TypeDeclaration;
var
  Decl, NewDecl: TDeclaration;
begin
  Decl := PushStub(TDeclaration_TypeStub);
  inherited;
  PopStack();

  if Decl.Items.Count > 0 then
  begin
    NewDecl := Decl.Items[0];
    NewDecl.Name := TDeclaration_TypeStub(Decl).TempName;

    FStack.Top.Items.Add(NewDecl);
  end;
end;

procedure TCodeParser.TypeName;
begin
  if InDeclaration(TDeclaration_TypeStub) then
    TDeclaration_TypeStub(FStack.Top).TempName := Lexer.Token;

  inherited;
end;

procedure TCodeParser.TypeCopy;
begin
  PushStack(TDeclaration_TypeCopy);
  PushStack(TDeclaration_VarType);
  inherited;
  PopStack();
  PopStack();
end;

procedure TCodeParser.PointerType;
begin
  PushStack(TDeclaration_TypePointer);
  PushStack(TDeclaration_VarType);
  inherited PointerType();
  PopStack();
  PopStack();
end;

procedure TCodeParser.VarDeclaration;
var
  Decl: TDeclaration;
begin
  Decl := PushStub(TDeclaration_VarStub);
  inherited;
  PopStack();

  EmptyVarStub(TDeclaration_VarStub(Decl), TDeclaration_Var);
end;

procedure TCodeParser.VarName;
begin
  if InDeclaration(TDeclaration_VarStub) then
    with TDeclaration_VarStub(FStack.Top) do
    begin
      if (NameCount >= Length(Names)) then
        SetLength(Names, 4+Length(Names)*2);

      Names[NameCount].Name := Lexer.Token;
      Names[NameCount].StartPos := Lexer.TokenPos;
      Names[NameCount].EndPos := Lexer.TokenPos+Lexer.TokenLen;

      Inc(NameCount);
    end;

  inherited;
end;

procedure TCodeParser.ConstantDeclaration;
var
  Decl: TDeclaration;
begin
  Decl := PushStub(TDeclaration_VarStub);
  inherited;
  PopStack();

  EmptyVarStub(TDeclaration_VarStub(Decl), TDeclaration_Const);
end;

procedure TCodeParser.ConstantName;
begin
  VarName();
end;

procedure TCodeParser.Method;
var
  Decl: TDeclaration_Method;
begin
  Decl := PushStack(TDeclaration_Method) as TDeclaration_Method;
  case Lexer.TokenID of
    tokFunction:  Decl.FMethodType := mtFunc;
    tokProcedure: Decl.FMethodType := mtProc;
    tokOperator:  Decl.FMethodType := mtOperator;
  end;

  inherited;
  PopStack();
end;

procedure TCodeParser.MethodOfType;
var
  Decl: TDeclaration_MethodOfType;
begin
  if (Lexer.TokenID = tokProperty) then
    Decl := PushStack(TDeclaration_Property) as TDeclaration_MethodOfType
  else
    Decl := PushStack(TDeclaration_MethodOfType) as TDeclaration_MethodOfType;

  case Lexer.TokenID of
    tokFunction:  Decl.FMethodType := mtFunc;
    tokProcedure: Decl.FMethodType := mtProc;
    tokOperator:  Decl.FMethodType := mtOperator;
    tokProperty:  Decl.FMethodType := mtProperty;
  end;

  inherited;
  PopStack();
end;

procedure TCodeParser.MethodName;
begin
  if InDeclaration(TDeclaration_Method) then
    TDeclaration_Method(FStack.Top).Name := Lexer.Token;

  inherited;
end;

procedure TCodeParser.MethodTypeName;
begin
  if InDeclaration(TDeclaration_MethodOfType) then
    TDeclaration_MethodOfType(FStack.Top).FObjectName := Lexer.Token;

  PushStack(TDeclaration_MethodObjectName);
  TypeKind();
  PopStack();
end;

procedure TCodeParser.MethodResultType;
begin
  if (not InDeclaration(TDeclaration_Method, TDeclaration_TypeMethod)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TDeclaration_MethodResult);
  TypeKind();
  PopStack();
end;

procedure TCodeParser.ParameterName;
begin
  VarName();
end;

procedure TCodeParser.ParameterVarType;
begin
  PushStack(TDeclaration_VarType);
  inherited;
  PopStack();
end;

procedure TCodeParser.ParameterSection;
var
  Decl: TDeclaration;
begin
  Decl := PushStub(TDeclaration_ParamStub);
  if (FLexer.TokenID in [tokConst, tokConstRef, tokVar, tokOut]) then
    TDeclaration_ParamStub(Decl).ParamType := FLexer.TokenID;
  inherited;
  PopStack();

  PushStack(TDeclaration_ParamGroup);
  EmptyParamStub(TDeclaration_ParamStub(Decl));
  PopStack();
end;

procedure TCodeParser.Parameters;
begin
  PushStack(TDeclaration_ParamList);
  inherited;
  PopStack();
end;

procedure TCodeParser.ArrayType;
begin
  PushStack(TDeclaration_TypeArray);
  inherited;
  PopStack();
end;

procedure TCodeParser.ArrayTypeStatic;
begin
  PushStack(TDeclaration_TypeStaticArray);
  inherited ArrayTypeStatic;
  PopStack();
end;

procedure TCodeParser.NativeType;
begin
  PushStack(TDeclaration_TypeNativeMethod);
  inherited;
  PopStack();
end;

procedure TCodeParser.FakeGenericType;
begin
  PushStack(TDeclaration_TypeFakeGeneric);
  inherited;
  PopStack();
end;

procedure TCodeParser.RecordType;
begin
  PushStack(TDeclaration_TypeRecord);
  inherited;
  PopStack();
end;

procedure TCodeParser.UnionType;
begin
  PushStack(TDeclaration_TypeUnion);
  inherited;
  PopStack();
end;

procedure TCodeParser.RecordField;
var
  Decl: TDeclaration;
begin
  if (not InDeclaration(TDeclaration_TypeRecord, TDeclaration_TypeUnion)) then
  begin
    inherited;
    Exit;
  end;

  Decl := PushStub(TDeclaration_VarStub);
  inherited;
  PopStack();

  EmptyVarStub(TDeclaration_VarStub(Decl), TDeclaration_Field);
end;

procedure TCodeParser.FieldName;
begin
  VarName();
end;

procedure TCodeParser.SetType;
begin
  PushStack(TDeclaration_TypeSet);
  inherited;
  PopStack();
end;

procedure TCodeParser.EnumType;
begin
  if InDeclaration(TDeclaration_TypeSet) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TDeclaration_TypeEnum);
  inherited;
  PopStack();
end;

procedure TCodeParser.EnumTypeScoped;
begin
  PushStack(TDeclaration_TypeEnumScoped);
  inherited;
  PopStack();
end;

procedure TCodeParser.EnumElement;
begin
  if (not InDeclaration(TDeclaration_TypeSet, TDeclaration_TypeEnum, TDeclaration_TypeEnumScoped)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TDeclaration_EnumElement);
  inherited;
  PopStack();
end;

procedure TCodeParser.EnumElementName;
begin
  if (not InDeclaration(TDeclaration_EnumElement)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TDeclaration_EnumElementName);
  inherited;
  PopStack();
end;

procedure TCodeParser.Reset;
begin
  inherited Reset();

  FHash.Clear();
  FManagedItems.Clear(True);

  FRoot.Items.Clear();
  FStack.Clear();
  FStack.Push(FRoot);

  FCaretPos := -1;
end;

procedure TCodeParser.Run;
begin
  inherited Run;

  BuildSymbolTable(FSymbolTable, Self);
end;

function TSymbolTable.IndexOf(Name: String): Integer;
var
  TheName: String;
  TheHash: UInt32;
  I: Integer;
begin
  if (Name <> '') then
  begin
    TheName := UpperCase(Name);
    TheHash := HashStr(TheName);
    for I := 0 to Count - 1 do
      if (Items[I].NameHash = TheHash) and (Items[I].Name = TheName) then
        Exit(I);
  end;

  Result := -1;
end;

function TSymbolTable.Copy: TSymbolTable;
var
  I: Integer;
begin
  Result := Self;
  Result.Items := System.Copy(Result.Items);
  for I := 0 to Result.Count - 1 do
    Result.Items[I].Decls := System.Copy(Result.Items[I].Decls);
end;

function TSymbolTable.Get(Name: String): TDeclarationArray;
var
  Index: Integer;
begin
  Index := IndexOf(Name);
  if (Index > -1) then
    Result := System.Copy(Items[Index].Decls, 0, Items[Index].Count)
  else
    Result := [];
end;

procedure TSymbolTable.Add(Name: String; Decl: TDeclaration);
var
  TheName: String;
  TheHash: UInt32;
  I: Integer;
begin
  Inc(TotalCount);

  TheName := UpperCase(Name);
  TheHash := HashStr(TheName);

  // add to existing?
  for I := 0 to Count - 1 do
    if (Items[I].NameHash = TheHash) and (Items[I].Name = TheName) then
      with Items[I] do
      begin
        if (Count >= High(Decls)) then
          SetLength(Decls, (4 + Length(Decls) * 2));
        Decls[Count] := Decl;
        Inc(Count);

        Exit;
      end;

  // new entry
  if (Count >= High(Items)) then
    SetLength(Items, 4 + (Length(Items) * 2));

  Items[Count].Name := TheName;
  Items[Count].NameHash := TheHash;
  Items[Count].Count := 1;
  Items[Count].Decls := [Decl];

  Inc(Count);
end;

procedure TSymbolTable.Add(Decls: TDeclarationArray);
var
  I: Integer;
begin
  for I := 0 to High(Decls) do
    if (Decls[I].Name <> '') then
      Add(Decls[I].Name, Decls[I]);
end;

procedure TSymbolTable.Clear;
begin
  Self := Default(TSymbolTable);
end;

procedure TSymbolTable.CopyFrom(Other: TSymbolTable);

  procedure Append(var Item: TItem; const Decls: TDeclarationArray; const Count: Integer);
  begin
    if (Item.Count + Count >= High(Decls)) then
      SetLength(Item.Decls, (Item.Count + Count) * 2);

    Move(Decls[0], Item.Decls[Item.Count], Count * SizeOf(TDeclaration));

    Inc(Item.Count, Count);
    Inc(TotalCount, Count);
  end;

var
  I, J: Integer;
label
  Next;
begin
  for I := 0 to Other.Count - 1 do
  begin
    for J := 0 to Self.Count - 1 do
      if (Other.Items[I].NameHash = Self.Items[J].NameHash) then
      begin
        Append(Self.Items[J], Other.Items[I].Decls, Other.Items[I].Count);

        goto Next;
      end;

    // new entry
    if (Count >= High(Items)) then
      SetLength(Items, 4 + (Length(Items) * 2));
    Items[Count] := Other.Items[I];
    Inc(Count);
    Inc(TotalCount, Other.Items[I].Count);

    Next:
  end;
end;

procedure BuildSymbolTable(var Table: TSymbolTable; From: TCodeParser);
var
  I: Integer;
  Decl: TDeclaration;
begin
  for I := 0 to From.Items.Count - 1 do
  begin
    Decl := From.Items[I];

    if (Decl is TDeclaration_MethodOfType) then // add it under the type
      Table.Add(TDeclaration_MethodOfType(Decl).ObjectName, Decl)
    else
    begin
      Table.Add(Decl.Name, Decl);

      if (Decl.ClassType = TDeclaration_TypeEnum) then
        Table.Add(TDeclaration_TypeEnum(Decl).Elements)
      else
      if (Decl.ClassType = TDeclaration_TypeSet) then
        Table.Add(TDeclaration_TypeSet(Decl).EnumElements);
    end;
  end;
end;

function DeclarationKind(Decl: TDeclaration): ShortString;
begin
  Result := '';

  if (Decl is TDeclaration_Method) then
  begin
    if TDeclaration_Method(Decl).isFunc     then Exit('function');
    if TDeclaration_Method(Decl).isProc     then Exit('procedure');
    if TDeclaration_Method(Decl).isOperator then Exit('operator');
    if TDeclaration_Method(Decl).isProperty then Exit('property');
  end else
  begin
    if (Decl is TDeclaration_Type)        then Exit('type');
    if (Decl is TDeclaration_Const)       then Exit('const');
    if (Decl is TDeclaration_Var)         then Exit('var');
    if (Decl is TDeclaration_EnumElement) then Exit('enumelement');
    if (Decl is TDeclaration_Keyword)     then Exit('keyword');
    if (Decl is TDeclaration_Anchor)      then Exit('anchor');
  end;
end;

function DeclarationImage(Decl: TDeclaration): Integer;
begin
  Result := -1;

  case DeclarationKind(Decl) of
    'property':    Result := 63;
    'function':    Result := 43;
    'procedure':   Result := 43;
    'operator':    Result := 43;
    'type':        Result := 45;
    'const':       Result := 47;
    'var':         Result := 46;
    'enumelement': Result := 48;
    'anchor':      Result := 49;
    else
      Result := -1;
  end;
end;

function RemoveDuplicateProperties(Decls: TDeclarationArray): TDeclarationArray;

  function HasProperty(Prop: TDeclaration_Property; Items: TDeclarationArray; Count: Integer): Boolean;
  var
    I: Integer;
  begin
    // check we dont a write method of this
    if Prop.IsRead then
    begin
      for I := 0 to Count - 1 do
        if (Items[I] is TDeclaration_Property) and (Items[I].FullName = Prop.FullName) and
           TDeclaration_Property(Items[I]).IsWrite and (TDeclaration_Property(Items[I]).ParamCount = Prop.ParamCount + 1) then
           begin
             Result := True;
             Exit;
           end;
    end else
    // check we dont a read method of this
    begin
      for I := 0 to Count - 1 do
        if (Items[I] is TDeclaration_Property) and (Items[I].FullName = Prop.FullName) and
           TDeclaration_Property(Items[I]).IsRead and (TDeclaration_Property(Items[I]).ParamCount = Prop.ParamCount - 1) then
           begin
             Result := True;
             Exit;
           end;
    end;

    Result := False;
  end;

var
  I, Count: Integer;
begin
  Count := 0;
  SetLength(Result, Length(Decls));
  for I := 0 to High(Decls) do
  begin
    if (Decls[I] is TDeclaration_Property) then
      Continue;
    Result[Count] := Decls[I];
    Inc(Count);
  end;

  // add write first, they're more useful
  for I := 0 to High(Decls) do
    if (Decls[I] is TDeclaration_Property) and TDeclaration_Property(Decls[I]).IsWrite then
    begin
      Result[Count] := Decls[I];
      Inc(Count);
    end;
  // add read if write does not exist
  for I := 0 to High(Decls) do
     if (Decls[I] is TDeclaration_Property) and TDeclaration_Property(Decls[I]).IsRead and
        (not HasProperty(Decls[I] as TDeclaration_Property, Result, Count)) then
     begin
       Result[Count] := Decls[I];
       Inc(Count);
     end;

  SetLength(Result, Count);
end;

function PropertyHeader(Decl: TDeclaration_Property; FullHeader: Boolean): String;
var
  I: Integer;
begin
  if FullHeader then
    Result := 'property ' + Decl.FullName
  else
    Result := '';

  with Decl do
  begin
    // getter
    if Assigned(Decl.ResultType) then
    begin
      if (Length(Params) > 0) then
      begin
        Result += '[';
        for I := 0 to High(Params) do
        begin
          if (I > 0) then
            Result += '; ';
          Result += Params[I].Name + Params[I].Items.GetTextOfClassNoCommentsSingleLine(TDeclaration_VarType, ': ');
        end;
        Result += ']';
      end;
      Result += Decl.ResultString;
    end else
    // setter
    begin
      if (Length(Params) > 1) then
      begin
        Result += '[';
        for I := 0 to High(Params) - 1 do
        begin
          if (I > 0) then
            Result += '; ';
          Result += Params[I].Name + Params[I].Items.GetTextOfClassNoCommentsSingleLine(TDeclaration_VarType, ': ');
        end;
        Result += ']';
      end;
      if Length(Params) > 0 then
        Result += ': ' + Params[High(Params)].Items.GetTextOfClassNoCommentsSingleLine(TDeclaration_VarType);
    end;
  end;
end;

function IndexableProperties(Decls: TDeclarationArray): TDeclarationArray;
var
  I: Integer;
begin
  Result := [];
  for I := 0 to High(Decls) do
    if (Decls[I] is TDeclaration_Property) and (TDeclaration_Property(Decls[I]).ParamCount > 1) then
      Result.Add(Decls[I]);
end;

function RemoveOverridenMethods(Decls: TDeclarationArray): TDeclarationArray;
var
  I: Integer;
begin
  Result := [];
  for I := 0 to High(Decls) do
    if (Decls[I] is TDeclaration_Method) and (not TDeclaration_Method(Decls[I]).isOverride) then
      Result.Add(Decls[I]);
end;

end.
