{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_codetools_parser;

{$DEFINE PARSER_SEPERATE_VARIABLES}

{$i simba.inc}

interface

uses
  SysUtils, Classes,
  mPasLexTypes, mPasLex, mSimplePasPar,
  Generics.Collections, Generics.Defaults;

type
  TDeclaration = class;
  TDeclarationArray = array of TDeclaration;
  TDeclarationClass = class of TDeclaration;

  TDeclarationStack = specialize TStack<TDeclaration>;

  TDeclarationList = class(specialize TObjectList<TDeclaration>)
  public
    function GetItemsOfClass(AClass: TDeclarationClass; SubSearch: Boolean = False): TDeclarationArray;
    function GetFirstItemOfClass(AClass: TDeclarationClass; SubSearch: Boolean = False): TDeclaration;
    function GetItemInPosition(Position: Integer): TDeclaration;

    function GetTextOfClass(AClass: TDeclarationClass): String;
    function GetTextOfClassNoComments(AClass: TDeclarationClass): String;
    function GetTextOfClassNoCommentsSingleLine(AClass: TDeclarationClass): String;
  end;

  TDeclarationMap = class
  protected
  type
    TDict = specialize TDictionary<String, TDeclarationArray>;
  protected
    FDictionary: TDict;
  public
    procedure Clear;

    procedure Add(Name: String; Declaration: TDeclaration);

    function GetAll: TDeclarationArray; overload;
    function GetAll(Name: String): TDeclarationArray; overload;
    function Get(Name: String): TDeclaration;

    constructor Create;
    destructor Destroy; override;
  end;

  TciTypeDeclaration = class;
  TciProcedureDeclaration = class;
  TciTypeKind = class;

  TDeclaration = class
  private
    FOwner: TDeclaration;
    FStartPos: Integer;
    FEndPos: Integer;
    FItems: TDeclarationList;
    FName: String;
    FLine: Integer;
    FLexer: TmwPasLex;

    FText: String;
    FTextNoComments: String;
    FTextNoCommentsSingleLine: String;

    function DoGetName: string; virtual;

    function GetText: String;
    function GetTextNoComments: String;
    function GetTextNoCommentsSingleLine: String;

    function GetName: string;
  public
    function HasOwnerClass(AClass: TDeclarationClass; out Declaration: TDeclaration; Recursive: Boolean = False): Boolean;
    function GetOwnersOfClass(AClass: TDeclarationClass): TDeclarationArray;

    function Clone(AOwner: TDeclaration): TDeclaration; inline;
    function IsName(const Value: String): Boolean; inline;

    property Lexer: TmwPasLex read FLexer;
    property Owner: TDeclaration read FOwner write FOwner;

    property StartPos: Integer read FStartPos write FStartPos;
    property EndPos: Integer read FEndPos write FEndPos;
    property Items: TDeclarationList read FItems;
    property Name: String read GetName;
    property Line: Integer read FLine;

    property Text: String read GetText;
    property TextNoComments: String read GetTextNoComments;
    property TextNoCommentsSingleLine: String read GetTextNoCommentsSingleLine;

    constructor Create(ALexer: TmwPasLex; AOwner: TDeclaration; AStart: Integer; AEnd: Integer = -1); overload; virtual;
    constructor Create(From: TDeclaration); overload; virtual;
    destructor Destroy; override;
  end;

  TciNativeType = class(TDeclaration)
  protected
    function GetParent: String;
  public
    property Parent: String read GetParent;
  end;

  TciTypeCopy = class(TDeclaration)
  protected
    function GetParent: String;
  public
    property Parent: String read GetParent;
  end;

  TciTypeAlias = class(TDeclaration)
  protected
    function GetParent: String;
  public
    property Parent: String read GetParent;
  end;

  TciTypeName = class(TDeclaration);
  TciTypeIdentifer = class(TDeclaration)
  protected
    function DoGetName: string; override;
  end;

  TciPointerType = class(TDeclaration)
  public
    function GetType: TDeclaration;
  end;

  TciEnumType = class(TDeclaration)
  protected
    function GetElements: TDeclarationArray;
  public
    Scoped: Boolean;

    property Elements: TDeclarationArray read GetElements;
  end;

  TciEnumElement = class(TDeclaration)
  protected
    function DoGetName: string; override;
  end;

  TciSetType = class(TDeclaration);
  TciRecordType = class(TDeclaration)
  protected
    function GetParent: String;
  public
    property Parent: String read GetParent;
  end;

  TciArrayType = class(TDeclaration)
  public
    function GetDimensionCount: Integer;
    function GetType: TciTypeKind;
    function IsStatic: Boolean;
  end;

  TciTypeKind = class(TDeclaration)
  protected
    function GetRecordType: TciRecordType;
    function GetIdentifierType: TciTypeIdentifer;
  public
    function GetType: TDeclaration;
    property RecordType: TciRecordType read GetRecordType;
    property IdentifierType: TciTypeIdentifer read GetIdentifierType;
  end;

  TciProcedureName = class(TDeclaration);
  TciProcedureClassName = class(TciTypeKind)
  protected
    function DoGetName: string; override;
  end;

  TciReturnType = class(TciTypeKind)
  protected
    function DoGetName: string; override;
  end;

  EProcedureDirectives = set of TptTokenKind;

  TciProcedureDeclaration = class(TDeclaration)
  private
    FObjectName: String;
    FProcedureDirectives: EProcedureDirectives;
    FIsFunction: Boolean;
    FIsOperator: Boolean;
    FIsMethodOfType: Boolean;
    FHeader: String;

    function GetHeader: String;

    function DoGetName: String; override;
    function GetObjectName: String;
    function GetReturnType: TciReturnType;
  public
    function GetParamDeclarations: TDeclarationArray;

    property IsFunction: Boolean read FIsFunction write FIsFunction;
    property IsOperator: Boolean read FIsOperator write FIsOperator;
    property IsMethodOfType: Boolean read FIsMethodOfType write FIsMethodOfType;
    property ReturnType: TciReturnType read GetReturnType;
    property ObjectName: String read GetObjectName;
    property Directives: EProcedureDirectives read FProcedureDirectives write FProcedureDirectives;
    property Header: String read GetHeader;
  end;

  TciProceduralType = class(TciProcedureDeclaration);

  TciInclude = class(TDeclaration);


  TciCompoundStatement = class(TDeclaration);
  TciWithStatement = class(TDeclaration);
  TciSimpleStatement = class(TDeclaration);
  TciVariable = class(TDeclaration);

  TciTypedConstant = class(TDeclaration);
  TciExpression = class(TDeclaration);

  TciTypeDeclaration = class(TDeclaration)
  protected
    function GetEnumType: TciEnumType;
    function GetMethodType: TciProceduralType;
    function GetNativeMethodType: TciNativeType;
    function GetRecordType: TciRecordType;
    function GetAliasType: TciTypeAlias;
    function GetCopyType: TciTypeCopy;
    function GetArrayType: TciArrayType;
    function GetPointerType: TciPointerType;
    function DoGetName: String; override;
  public
    function GetType: TDeclaration;

    property AliasType: TciTypeAlias read GetAliasType;
    property CopyType: TciTypeCopy read GetCopyType;
    property NativeMethodType: TciNativeType read GetNativeMethodType;
    property MethodType: TciProceduralType read GetMethodType;
    property RecordType: TciRecordType read GetRecordType;
    property ArrayType: TciArrayType read GetArrayType;
    property EnumType: TciEnumType read GetEnumType;
    property PointerType: TciPointerType read GetPointerType;
  end;

  TciVarName = class(TDeclaration);
  TciVarDeclaration = class(TDeclaration)
  protected
    FVarType: TDeclaration;

    function GetValue: TDeclaration;
    function GetVarType: TciTypeKind;
    function DoGetName: String; override;
  public
    // For parameter hints so we can restructure.
    // var
    //   a, b, c: Integer; (group 0)
    //   d: Extended;    (group 1)
    Group: Integer;

    property VarType: TciTypeKind read GetVarType;
    property Value: TDeclaration read GetValue;
  end;

  TciConstantDeclaration = class(TciVarDeclaration);

  TciLabelDeclaration = class(TDeclaration);
  TciLabelName = class(TDeclaration);

  TciForward = class(TciTypeKind);

  TciParameterList = class(TDeclaration);
  TciParameter = class(TciVarDeclaration);
  TciConstRefParameter = class(TciParameter);
  TciConstParameter = class(TciParameter);
  TciOutParameter = class(TciParameter);
  TciFormalParameter = class(TciParameter);
  TciInParameter = class(TciParameter);
  TciVarParameter = class(TciParameter);

  TciArrayConstant = class(TDeclaration);
  TciUnionType = class(TDeclaration);

  TciClassField = class(TciVarDeclaration);

  TciRecordConstant = class(TDeclaration);
  TciRecordFieldConstant = class(TDeclaration);

  TciAncestorId = class(TDeclaration);
  TciIdentifier = class(TDeclaration);

  TciOrdinalType = class(TDeclaration);

  TOnFindInclude = function(Sender: TObject; var FileName: string): Boolean of object;
  TOnInclude = procedure(Sender: TObject; FileName: String; var Handled: Boolean) of object;

  TOnFindLibrary = function(Sender: TObject; var FileName: String): Boolean of object;
  TOnLoadLibrary = procedure(Sender: TObject; FileName: String; var Contents: String) of object;
  TOnLibrary = procedure(Sender: TObject; FileName: String; var Handled: Boolean) of object;

  TCodeParser = class(TmwSimplePasPar)
  protected
    FStack: TDeclarationStack;
    FItems: TDeclarationList;
    FOnFindInclude: TOnFindInclude;
    FOnInclude: TOnInclude;
    FOnFindLibrary: TOnFindInclude;
    FOnLoadLibrary: TOnLoadLibrary;
    FOnLibrary: TOnLibrary;
    FGlobals: TDeclarationMap;

    function GetCaretPos: Integer;
    function GetMaxPos: Integer;
    procedure SetCaretPos(Value: Integer);
    procedure SetMaxPos(Value: Integer);

    procedure SeperateVariables(Variables: TciVarDeclaration);

    function InDeclaration(AClass: TDeclarationClass): Boolean;
    function InDeclarations(AClassArray: array of TDeclarationClass): Boolean;
    function PushStack(AClass: TDeclarationClass; AStart: Integer = -1): TDeclaration;
    procedure PopStack(AEnd: Integer = -1);

    procedure ParseFile; override;
    procedure OnLibraryDirect(Sender: TmwBasePasLex); override;
    procedure OnIncludeDirect(Sender: TmwBasePasLex); override;                  //Includes

    procedure CompoundStatement; override;                                      //Begin-End
    procedure WithStatement; override;                                          //With
    procedure SimpleStatement; override;                                        //Begin-End + With
    procedure Variable; override;                                               //With

    procedure TypeKind; override;                                               //Var + Const + Array + Record
    procedure TypedConstant; override;                                          //Var + Procedure/Function Parameters
    procedure Expression; override;                                             //Var + Const + ArrayConst
    procedure ProceduralType; override;                                         //Var + Procedure/Function Parameters

    procedure TypeAlias; override;                                              //Type
    procedure TypeIdentifer; override;                                          //Type
    procedure TypeDeclaration; override;                                        //Type
    procedure TypeName; override;                                               //Type
    procedure ExplicitType; override;                                           //Type
    procedure PointerType; override;

    procedure VarDeclaration; override;                                         //Var
    procedure VarName; override;                                                //Var

    procedure ConstantDeclaration; override;                                    //Const
    procedure ConstantName; override;                                           //Const

    procedure LabelDeclarationSection; override;                                //Label
    procedure LabelId; override;                                                //Label

    procedure ProceduralDirective; override;                                    //Procedure/Function directives
    procedure ProcedureDeclarationSection; override;                            //Procedure/Function
    procedure FunctionProcedureName; override;                                  //Procedure/Function
    procedure ObjectNameOfMethod; override;                                     //Class Procedure/Function
    procedure ReturnType; override;                                             //Function Result
    procedure ForwardDeclaration; override;                                     //Forwarding
    procedure ConstRefParameter; override;                                      //Procedure/Function Parameters
    procedure ConstParameter; override;                                         //Procedure/Function Parameters
    procedure OutParameter; override;                                           //Procedure/Function Parameters
    procedure ParameterFormal; override;                                        //Procedure/Function Parameters
    procedure InParameter; override;                                            //Procedure/Function Parameters
    procedure VarParameter; override;                                           //Procedure/Function Parameters
    procedure ParameterName; override;                                          //Procedure/Function Parameters
    procedure OldFormalParameterType; override;                                 //Procedure/Function Parameters
    procedure FormalParameterList; override;                                    //Procedure/Function Parameter List
    procedure ArrayType; override;                                              //Array
    procedure ArrayConstant; override;                                          //Array Const

    procedure NativeType; override;                                             //Lape Native Method

    procedure RecordType; override;                                             //Record
    procedure UnionType; override;                                              //Union
    procedure ClassField; override;                                             //Record + Class
    procedure FieldName; override;                                              //Record + Class

    procedure AncestorId; override;                                             //Class

    procedure SetType; override;                                                //Set
    procedure OrdinalType; override;                                            //Set + Array Range

    procedure EnumeratedType; override;                                         //Enum
    procedure EnumeratedScopedType; override;                                   //Enum
    procedure QualifiedIdentifier; override;                                    //Enum
  public
    property Items: TDeclarationList read FItems;
    property Globals: TDeclarationMap read FGlobals;

    property OnFindInclude: TOnFindInclude read FOnFindInclude write FOnFindInclude;
    property OnInclude: TOnInclude read FOnInclude write FOnInclude;

    property OnFindLibrary: TOnFindInclude read FOnFindLibrary write FOnFindLibrary;
    property OnLoadLibrary: TOnLoadLibrary read FOnLoadLibrary write FOnLoadLibrary;
    property OnLibrary: TOnLibrary read FOnLibrary write FOnLibrary;

    property CaretPos: Integer read GetCaretPos write SetCaretPos;
    property MaxPos: Integer read GetMaxPos write SetMaxPos;

    procedure Run; override;

    procedure Assign(From: TObject); override;

    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  simba.mufasatypes;

procedure TDeclarationMap.Clear;
begin
  FDictionary.Clear();
end;

procedure TDeclarationMap.Add(Name: String; Declaration: TDeclaration);
var
  Declarations: TDeclarationArray;
begin
  Name := UpperCase(Name);
  if FDictionary.TryGetValue(Name, Declarations) then
    Declarations := Declarations + [Declaration]
  else
    Declarations := [Declaration];

  FDictionary.AddOrSetValue(Name, Declarations);
end;

function TDeclarationMap.GetAll: TDeclarationArray;
var
  Declarations: TDeclarationArray;
begin
  Result := nil;

  for Declarations in FDictionary.Values.ToArray() do
    Result := Result + Declarations;
end;

function TDeclarationMap.GetAll(Name: String): TDeclarationArray;
begin
  Result := nil;

  Name := UpperCase(Name);
  if FDictionary.ContainsKey(Name) then
    Result := FDictionary.Items[Name];
end;

function TDeclarationMap.Get(Name: String): TDeclaration;
begin
  Result := nil;

  Name := UpperCase(Name);
  if FDictionary.ContainsKey(Name) then
    Result := FDictionary.Items[Name][0];
end;

constructor TDeclarationMap.Create;
begin
  FDictionary := TDict.Create();
end;

destructor TDeclarationMap.Destroy;
begin
  FDictionary.Free();

  inherited Destroy();
end;

function TciPointerType.GetType: TDeclaration;
begin
  Result := FItems.GetFirstItemOfClass(TciTypeIdentifer);
end;

function TciTypeCopy.GetParent: String;
begin
  Result := FItems.GetTextOfClass(TciTypeIdentifer);
end;

function TciTypeAlias.GetParent: String;
begin
  Result := FItems.GetTextOfClass(TciTypeIdentifer);
end;

function TciRecordType.GetParent: String;
begin
  Result := FItems.GetTextOfClass(TciAncestorId);
end;

function TciNativeType.GetParent: String;
begin
  Result := FItems.GetTextOfClass(TciAncestorId);
end;

function TciEnumType.GetElements: TDeclarationArray;
begin
  Result := FItems.GetItemsOfClass(TciEnumElement);
end;

function TciTypeIdentifer.DoGetName: string;
begin
  Result := GetText();
end;

function TciTypeKind.GetRecordType: TciRecordType;
begin
  Result := FItems.GetFirstItemOfClass(TciRecordType) as TciRecordType;
end;

function TciTypeKind.GetIdentifierType: TciTypeIdentifer;
begin
  Result := FItems.GetFirstItemOfClass(TciTypeIdentifer) as TciTypeIdentifer;
end;

function TciTypeKind.GetType: TDeclaration;
begin
  Result := nil;
  if Items.Count > 0 then
    Result := Items[0];
end;

function TciEnumElement.DoGetName: string;
begin
  Result := GetText();
end;

function TciProcedureClassName.DoGetName: string;
begin
  Result := 'Self';
end;

function TciReturnType.DoGetName: string;
begin
  Result := 'Result';
end;

function TciArrayType.GetDimensionCount: Integer;
var
  Declaration: TDeclaration;
begin
  Result := 0;

  Declaration := Self;

  while Declaration <> nil do
  begin
    Result := Result + 1;

    Declaration := Declaration.Items.GetFirstItemOfClass(TciTypeKind);
    if Declaration <> nil then
      Declaration := Declaration.Items.GetFirstItemOfClass(TciArrayType)
  end;
end;

function TciArrayType.GetType: TciTypeKind;
var
  Declarations: TDeclarationArray;
  Index: Integer;
begin
  Result := nil;

  Declarations := FItems.GetItemsOfClass(TciTypeKind, True);
  Index := GetDimensionCount() - 1;
  if (Index >= 0) and (Index <= High(Declarations)) then
    Result := Declarations[Index] as TciTypeKind;
end;

function TciArrayType.IsStatic: Boolean;
begin
  Result := FItems.GetFirstItemOfClass(TciOrdinalType) <> nil;
end;

function TciVarDeclaration.GetValue: TDeclaration;
begin
  Result := FItems.GetFirstItemOfClass(TciExpression);
end;

function TciVarDeclaration.GetVarType: TciTypeKind;
begin
  Result := FItems.GetFirstItemOfClass(TciTypeKind) as TciTypeKind;
end;

function TciVarDeclaration.DoGetName: string;
var
  Declaration: TDeclaration;
begin
  Declaration := FItems.GetFirstItemOfClass(TciVarName);
  if (Declaration <> nil) then
    Result := Declaration.GetText()
  else
    Result := '';
end;

function TciTypeDeclaration.GetAliasType: TciTypeAlias;
var
  Declaration: TDeclaration;
begin
  Result := nil;

  Declaration := GetType();
  if (Declaration <> nil) and (Declaration is TciTypeAlias) then
    Result := TciTypeAlias(Declaration);
end;

function TciTypeDeclaration.GetEnumType: TciEnumType;
var
  Declaration: TDeclaration;
begin
  Result := nil;

  Declaration := GetType();
  if (Declaration <> nil) and (Declaration is TciEnumType) then
    Result := TciEnumType(Declaration);
end;

function TciTypeDeclaration.GetMethodType: TciProceduralType;
var
  Declaration: TDeclaration;
begin
  Result := nil;

  Declaration := GetType();
  if (Declaration <> nil) and (Declaration is TciProceduralType) then
    Result := TciProceduralType(Declaration);
end;

function TciTypeDeclaration.GetNativeMethodType: TciNativeType;
var
  Declaration: TDeclaration;
begin
  Result := nil;

  Declaration := GetType();
  if (Declaration <> nil) and (Declaration is TciNativeType) then
    Result := TciNativeType(Declaration);
end;

function TciTypeDeclaration.GetRecordType: TciRecordType;
var
  Declaration: TDeclaration;
begin
  Result := nil;

  Declaration := GetType();
  if (Declaration <> nil) and (Declaration is TciRecordType) then
    Result := TciRecordType(Declaration);
end;

function TciTypeDeclaration.GetCopyType: TciTypeCopy;
var
  Declaration: TDeclaration;
begin
  Result := nil;

  Declaration := GetType();
  if (Declaration <> nil) and (Declaration is TciTypeCopy) then
    Result := TciTypeCopy(Declaration);
end;

function TciTypeDeclaration.GetArrayType: TciArrayType;
var
  Declaration: TDeclaration;
begin
  Result := nil;

  Declaration := GetType();
  if (Declaration <> nil) and (Declaration is TciArrayType) then
    Result := TciArrayType(Declaration);
end;

function TciTypeDeclaration.GetPointerType: TciPointerType;
var
  Declaration: TDeclaration;
begin
  Result := nil;

  Declaration := GetType();
  if (Declaration <> nil) and (Declaration is TciPointerType) then
    Result := TciPointerType(Declaration);
end;

function TciTypeDeclaration.DoGetName: String;
var
  Declaration: TDeclaration;
begin
  Declaration := FItems.GetFirstItemOfClass(TciTypeName);
  if (Declaration <> nil) then
    Result := Declaration.GetText()
  else
    Result := '';
end;

function TciTypeDeclaration.GetType: TDeclaration;
var
  Declaration: TDeclaration;
begin
  Result := nil;

  Declaration := FItems.GetFirstItemOfClass(TciTypeKind);
  if (Declaration <> nil) and (Declaration.Items.Count > 0) then
    Result := Declaration.Items[0];
end;

function TDeclarationList.GetItemsOfClass(AClass: TDeclarationClass; SubSearch: Boolean = False): TDeclarationArray;

  procedure SearchItem(
    AClass: TDeclarationClass;
    SubSearch: Boolean;
    Item: TDeclaration;
    var Res: TDeclarationArray;
    var ResIndex: Integer);
  var
    i: Integer;
  begin
    if ((Item = nil) and (AClass = nil)) or (Item is AClass) then
    begin
      SetLength(Res, ResIndex + 1);
      Res[ResIndex] := Item;
      Inc(ResIndex);
    end;
    if SubSearch then
      for i := 0 to Item.Items.Count - 1 do
        SearchItem(AClass, SubSearch, Item.Items[i], Res, ResIndex);
  end;

var
  i, l: Integer;
begin
  l := 0;
  SetLength(Result, 0);

  for i := 0 to FLength - 1 do
    SearchItem(AClass, SubSearch, FItems[i], Result, l);
end;

function TDeclarationList.GetFirstItemOfClass(AClass: TDeclarationClass; SubSearch: Boolean = False): TDeclaration;

  function SearchItem(AClass: TDeclarationClass; SubSearch: Boolean; Item: TDeclaration; out Res: TDeclaration): Boolean;
  var
    i: Integer;
  begin
    Res := nil;
    Result := False;
    if ((Item = nil) and (AClass = nil)) or (Item is AClass) then
    begin
      Res := Item;
      Result := True;
      Exit;
    end;
    if SubSearch then
      for i := 0 to Item.Items.Count - 1 do
        if SearchItem(AClass, SubSearch, Item.Items[i], Res) then
        begin
          Result := True;
          Break;
        end;
  end;

var
  i: Integer;
begin
  Result := nil;

  for i := 0 to FLength - 1 do
    if SearchItem(AClass, SubSearch, FItems[i], Result) then
      Exit;
end;

function TDeclarationList.GetItemInPosition(Position: Integer): TDeclaration;

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

  for I := 0 to FLength - 1 do
    if (Position >= FItems[I].StartPos) and (Position <= FItems[I].EndPos) then
      Search(FItems[I], Result);
end;

function TDeclarationList.GetTextOfClass(AClass: TDeclarationClass): String;
var
  Declaration: TDeclaration;
begin
  Result := '';

  Declaration := GetFirstItemOfClass(AClass);
  if Declaration <> nil then
    Result := Declaration.GetText();
end;

function TDeclarationList.GetTextOfClassNoComments(AClass: TDeclarationClass): String;
var
  Declaration: TDeclaration;
begin
  Result := '';

  Declaration := GetFirstItemOfClass(AClass);
  if Declaration <> nil then
    Result := Declaration.GetTextNoComments();
end;

function TDeclarationList.GetTextOfClassNoCommentsSingleLine(AClass: TDeclarationClass): String;
var
  Declaration: TDeclaration;
begin
  Result := '';

  Declaration := GetFirstItemOfClass(AClass);
  if Declaration <> nil then
    Result := Declaration.GetTextNoCommentsSingleLine();
end;

function TDeclaration.GetName: string;
begin
  if (FName = #0) then
    FName := DoGetName();

  Result := FName;
end;

function TDeclaration.GetText: String;
begin
  if (FText = #0) then
    FText := FLexer.CopyDoc(FStartPos, FEndPos);

  Result := FText;
end;

function TDeclaration.GetTextNoComments: String;

  function Filter(const Text: String): String;
  var
    Builder: TStringBuilder;
    Lexer: TmwPasLex;
  begin
    Builder := nil;

    Lexer := TmwPasLex.Create(Text);
    Lexer.Next();
    while (Lexer.TokenID <> tokNull) do
    begin
      if (not (Lexer.TokenID in [tokSlashesComment, tokAnsiComment, tokBorComment])) then
      begin
        if (Builder = nil) then
          Builder := TStringBuilder.Create(Length(Text));

        Builder.Append(Lexer.Token);
      end;
      Lexer.Next();
    end;
    Lexer.Free();

    if (Builder <> nil) then
    begin
      Result := Builder.ToString();

      Builder.Free();
    end else
      Result := Text;
  end;

begin
  if (FTextNoComments = #0) then
    FTextNoComments := Filter(FLexer.CopyDoc(FStartPos, FEndPos));

  Result := FTextNoComments;
end;

function TDeclaration.GetTextNoCommentsSingleLine: String;

  function Filter(const Text: String): String;
  var
    Builder: TStringBuilder;
    Lexer: TmwPasLex;
  begin
    Builder := nil;

    Lexer := TmwPasLex.Create(Text);
    Lexer.Next();
    while (Lexer.TokenID <> tokNull) do
    begin
      if (not (Lexer.TokenID in [tokSlashesComment, tokAnsiComment, tokBorComment, tokCRLF, tokCRLFCo])) then
      begin
        if (Builder = nil) then
          Builder := TStringBuilder.Create(Length(Text));

        Builder.Append(Lexer.Token);
      end;
      Lexer.Next();
    end;
    Lexer.Free();

    if (Builder <> nil) then
    begin
      Result := Builder.ToString();

      Builder.Free();
    end else
      Result := Text;
  end;

begin
  if (FTextNoCommentsSingleLine = #0) then
    FTextNoCommentsSingleLine := Filter(FLexer.CopyDoc(FStartPos, FEndPos));

  Result := FTextNoCommentsSingleLine;
end;

function TDeclaration.DoGetName: string;
begin
  Result := '';
end;

function TDeclaration.HasOwnerClass(AClass: TDeclarationClass; out Declaration: TDeclaration; Recursive: Boolean = False): Boolean;

  function IsOwner(Item: TDeclaration; AClass: TDeclarationClass; out Decl: TDeclaration; Recursive: Boolean): Boolean;
  begin
    if ((AClass = nil) and (Item.Owner = nil)) or (Item.Owner <> nil) and (Item.Owner is AClass) then
    begin
      Result := True;
      Decl := Item.Owner;
    end
    else if (Item.Owner <> nil) and Recursive then
      Result := IsOwner(Item.Owner, AClass, Decl, True)
    else
      Result := False;
  end;

begin
  Declaration := nil;

  if (AClass = nil) then
    Result := True
  else
    Result := IsOwner(Self, Aclass, Declaration, Recursive);
end;

function TDeclaration.GetOwnersOfClass(AClass: TDeclarationClass): TDeclarationArray;

  procedure IsOwner(
    AClass: TDeclarationClass;
    Item: TDeclaration;
    var Res: TDeclarationArray;
    var ResIndex: Integer);
  begin
    if ((AClass = nil) and (Item.Owner = nil)) or (Item.Owner is AClass) then
    begin
      SetLength(Res, ResIndex + 1);
      Res[ResIndex] := Item.Owner;
      Inc(ResIndex);
    end;
    if (Item.Owner <> nil) then
      IsOwner(AClass, Item.Owner, Res, ResIndex);
  end;

var
  l: Integer;
begin
  l := 0;
  SetLength(Result, 0);

  IsOwner(AClass, Self, Result, l);
end;

function TDeclaration.Clone(AOwner: TDeclaration): TDeclaration;
var
  i: Integer;
begin
  Result := TDeclarationClass(Self.ClassType).Create(Self);
  Result.Owner := AOwner;
  for i := 0 to Self.Items.Count - 1 do
    Result.Items.Add(Self.Items[i].Clone(Result));
end;

function TDeclaration.IsName(const Value: String): Boolean;
begin
  Result := SameText(Name, Value);
end;

constructor TDeclaration.Create(ALexer: TmwPasLex; AOwner: TDeclaration; AStart: Integer; AEnd: Integer);
begin
  inherited Create();

  FText := #0;
  FTextNoComments := #0;
  FTextNoCommentsSingleLine := #0;

  FName := #0;

  FLexer := ALexer;
  FLine := FLexer.LineNumber;
  FOwner := AOwner;
  FStartPos := AStart;
  if (AEnd > -1) then
    FEndPos := AEnd
  else
    FEndPos := AStart;

  FItems := TDeclarationList.Create(True);
end;

constructor TDeclaration.Create(From: TDeclaration);
begin
  Create(From.Lexer, From.Owner, From.StartPos, From.EndPos);
end;

destructor TDeclaration.Destroy;
begin
  FItems.Free();

  inherited;
end;

function TciProcedureDeclaration.GetHeader: String;
var
  Directive: TptTokenKind;
begin
  if (FHeader = '') then
  begin
    if IsOperator then
      FHeader := 'operator '
    else
    if IsFunction then
      FHeader := 'function '
    else
      FHeader := 'procedure ';

    if IsMethodOfType then
      FHeader := FHeader + ObjectName + '.';

    FHeader := FHeader + Name;

    FHeader := FHeader + Items.GetTextOfClassNoCommentsSingleLine(TciParameterList);
    if ReturnType <> nil then
      FHeader := FHeader + ': ' + ReturnType.GetText();

    FHeader := FHeader + ';';

    for Directive in Directives do
      if (Directive <> tokExternal) then
        FHeader := FHeader + ' ' + LowerCase(TokenName(Directive)) + ';';
  end;

  Result := FHeader;
end;

function TciProcedureDeclaration.DoGetName: String;
var
  Declaration: TDeclaration;
begin
  Declaration := FItems.GetFirstItemOfClass(TciProcedureName);
  if (Declaration <> nil) then
    Result := Declaration.GetText()
  else
    Result := '';
end;

function TciProcedureDeclaration.GetObjectName: String;
var
  Declaration: TDeclaration;
begin
  if FIsMethodOfType and (FObjectName = '') then
  begin
    Declaration := FItems.GetFirstItemOfClass(TciProcedureClassName);
    if Declaration <> nil then
      FObjectName := Declaration.GetText();
  end;

  Result := FObjectName;
end;

function TciProcedureDeclaration.GetReturnType: TciReturnType;
begin
  Result := FItems.GetFirstItemOfClass(TciReturnType) as TciReturnType;
end;

function TciProcedureDeclaration.GetParamDeclarations: TDeclarationArray;
var
  i: Integer;
  Declaration: TDeclaration;
begin
  SetLength(Result, 0);

  Declaration := FItems.GetFirstItemOfClass(TciParameterList);

  if Declaration <> nil then
    for i := 0 to Declaration.Items.Count - 1 do
      if (Declaration.Items[i] is TciParameter) then
        Result := Result + [Declaration.Items[i]];
end;

function TCodeParser.InDeclaration(AClass: TDeclarationClass): Boolean;
begin
  if (AClass = nil) then
    Result := (FStack.Count = 0)
  else
    Result := (FStack.Count > 0) and (FStack.Peek() is AClass);
end;

function TCodeParser.InDeclarations(AClassArray: array of TDeclarationClass): Boolean;
var
  AClass: TDeclarationClass;
begin
  Result := False;

  for AClass in AClassArray do
    if InDeclaration(AClass) then
    begin
      Result := True;

      Exit;
    end;
end;

function TCodeParser.PushStack(AClass: TDeclarationClass; AStart: Integer): TDeclaration;
begin
  if (AStart = -1) then
    AStart := FLexer.TokenPos;

  if (FStack.Count > 0) then
  begin
    Result := AClass.Create(FLexer, FStack.Peek(), AStart);

    FStack.Peek().Items.Add(Result);
  end else
  begin
    Result := AClass.Create(FLexer, nil, AStart);

    FItems.Add(Result);
  end;

  FStack.Push(Result);
end;

function TCodeParser.GetCaretPos: Integer;
begin
  if (fLexer = nil) then
    Result := -1
  else
    Result := fLexer.CaretPos;
end;

function TCodeParser.GetMaxPos: Integer;
begin
  if (fLexer = nil) then
    Result := -1
  else
    Result := fLexer.MaxPos;
end;

procedure TCodeParser.SetCaretPos(Value: Integer);
begin
  if (fLexer <> nil) then
    fLexer.CaretPos := Value;
end;

procedure TCodeParser.SetMaxPos(Value: Integer);
begin
  if (fLexer <> nil) then
    fLexer.MaxPos := Value;
end;

procedure TCodeParser.SeperateVariables(Variables: TciVarDeclaration);
var
  i: Integer;
  Index: Integer;
  Names: TDeclarationArray;
  Declaration: TciVarDeclaration;
  Kind: TDeclaration;
  Value: TDeclaration;
  Destination: TDeclarationList;
begin
  Names := Variables.Items.GetItemsOfClass(TciVarName);
  Kind := Variables.Items.GetFirstItemOfClass(TciTypeKind);
  Value := Variables.Items.GetFirstItemOfClass(TciExpression);

  if Length(Names) > 0 then
  begin
    if Variables.Owner <> nil then
      Destination := Variables.Owner.Items
    else
      Destination := FItems;

    Index := Destination.Count;

    for i := 0 to High(Names) do
    begin
      Declaration := TDeclarationClass(Variables.ClassType).Create(Variables) as TciVarDeclaration;
      Declaration.Owner := Variables.Owner;
      Declaration.Group := Index;
      Declaration.Items.Add(Names[i].Clone(Declaration));

      if Kind <> nil then
        Declaration.Items.Add(Kind.Clone(Declaration));
      if Value <> nil then
        Declaration.Items.Add(Value.Clone(Declaration));

      Destination.Add(Declaration);
    end;

    Destination.Delete(Index - 1);
  end;
end;

procedure TCodeParser.PopStack(AEnd: Integer = -1);
begin
  if (AEnd = -1) then
    AEnd := fLastNoJunkPos;

  if (FStack.Count > 0) then
    FStack.Pop().EndPos := AEnd;
end;

constructor TCodeParser.Create;
begin
  inherited Create();

  FStack := TDeclarationStack.Create();
  FItems := TDeclarationList.Create(True);
  FGlobals := TDeclarationMap.Create();
end;

destructor TCodeParser.Destroy;
begin
  FStack.Free();
  FItems.Free();
  FGlobals.Free();

  inherited Destroy();
end;

procedure TCodeParser.ParseFile;
begin
  Lexer.Next();

  SkipJunk();
  if (Lexer.TokenID = tokProgram) then
  begin
    NextToken();
    Expected(tokIdentifier);
    SemiColon();
  end;

  while (Lexer.TokenID in [tokBegin, tokConst, tokFunction, tokOperator, tokLabel, tokProcedure, tokType, tokVar]) do
  begin
    if (Lexer.TokenID = tokBegin) then
    begin
      CompoundStatement();
      if (Lexer.TokenID = tokSemiColon) then
        Expected(tokSemiColon);
    end else
      DeclarationSection();

    if (Lexer.TokenID = tok_DONE) then
      Break;
  end;
end;

procedure TCodeParser.OnLibraryDirect(Sender: TmwBasePasLex);
var
  FileName, Contents: String;
  Handled: Boolean;
begin
  Contents := '';

  if (not Sender.IsJunk) then
  try
    FileName := SetDirSeparators(Sender.DirectiveParamOriginal);

    if (FOnFindLibrary <> nil) then
    begin
      if FOnFindLibrary(Self, FileName) then
      begin
        Handled := False;

        if (FOnLibrary <> nil) then
          FOnLibrary(Self, FileName, Handled);

        if not Handled then
        begin
          if (FOnLoadLibrary <> nil) then
          begin
            FOnLoadLibrary(Self, FileName, Contents);

            PushLexer(TmwPasLex.Create(Contents, FileName));

            FLexer.IsLibrary := True;
            //FLexer.FileName := FileName;
            //FLexer.Script := Contents;
            //FLexer.Next();
          end;
        end;
      end else
        DebugLn('Library "' + FileName + '" not found');
    end;
  except
    on E: Exception do
      OnErrorMessage(fLexer, E.Message);
  end;
end;

procedure TCodeParser.OnIncludeDirect(Sender: TmwBasePasLex);
var
  FileName: String;
  Handled: Boolean;
begin
  if (not Sender.IsJunk) then
  try
    FileName := SetDirSeparators(Sender.DirectiveParamOriginal);

    if (FOnFindInclude <> nil) then
    begin
      if FOnFindInclude(Self, FileName) then
      begin
        Handled := (Sender.TokenID = tokIncludeOnceDirect) and HasFile(FileName);
        if not Handled then
        begin
          if (FOnInclude <> nil) then
            FOnInclude(Self, FileName, Handled);

          if not Handled then
          begin
            PushLexer(TmwPasLex.CreateFromFile(FileName));

            //with TStringList.Create() do
            //try
            //  LoadFromFile(FileName);
            //
            //  FLexer.FileName := FileName;
            //  FLexer.Script := Text;
            //  FLexer.Next();
            //finally
            //  Free();
            //end;
          end;
        end;
      end else
        DebugLn('Include "' + FileName + '" not found');
    end;
  except
    on E: Exception do
      OnErrorMessage(fLexer, E.Message);
  end;
end;

procedure TCodeParser.CompoundStatement;
begin
  if (not InDeclarations([nil, TciProcedureDeclaration, TciWithStatement])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciCompoundStatement);
  inherited;
  PopStack;
end;

procedure TCodeParser.WithStatement;
begin
  if (not InDeclarations([TciProcedureDeclaration, TciCompoundStatement])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciWithStatement);
  inherited;
  PopStack;
end;

procedure TCodeParser.SimpleStatement;
begin
  if (not InDeclaration(TciWithStatement)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciSimpleStatement);
  inherited;
  PopStack;
end;

procedure TCodeParser.Variable;
begin
  if (not InDeclaration(TciWithStatement)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciVariable);
  inherited;
  PopStack;
end;

procedure TCodeParser.TypeKind;
begin
  if (not InDeclarations([TciVarDeclaration, TciConstantDeclaration, TciTypeDeclaration, TciArrayType, TciClassField])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciTypeKind);
  inherited;
  PopStack;
end;

procedure TCodeParser.TypedConstant;
begin
  if (not InDeclarations([TciVarDeclaration, TciConstParameter, TciOutParameter, TciFormalParameter, TciInParameter, TciVarParameter, TciConstRefParameter])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciTypedConstant);
  inherited;
  PopStack;
end;

procedure TCodeParser.Expression;
var
  Decl: TDeclaration;
begin
  if (not InDeclarations([TciVarDeclaration, TciConstantDeclaration, TciOrdinalType])) then
  begin
    inherited;
    Exit;
  end;

  if InDeclarations([TciConstantDeclaration, TciVarDeclaration]) and (Lexer.TokenID = tokIdentifier) then
  begin
    Decl := PushStack(TciTypeKind);
    Decl.Items.Add(TciTypeIdentifer.Create(Lexer, Decl, Lexer.TokenPos, Lexer.TokenPos+Lexer.TokenLen));
    PopStack();
  end;

  PushStack(TciExpression);
  inherited;
  PopStack;
end;

procedure TCodeParser.ProceduralType;
begin
  if (not InDeclaration(TciTypeKind)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciProceduralType);
  inherited;
  PopStack;
end;

procedure TCodeParser.ProceduralDirective;
begin
  if InDeclaration(TciProcedureDeclaration) then
    with FStack.Peek as TciProcedureDeclaration do
      Directives := Directives + [Lexer.ExID];

  inherited ProceduralDirective;
end;

procedure TCodeParser.TypeAlias;
begin
  PushStack(TciTypeKind);
  PushStack(TciTypeAlias);
  inherited TypeAlias;
  PopStack;
  PopStack;
end;

procedure TCodeParser.TypeIdentifer;
begin
  PushStack(TciTypeIdentifer);
  inherited TypeIdentifer;
  PopStack;
end;

procedure TCodeParser.TypeDeclaration;
begin
  PushStack(TciTypeDeclaration);
  inherited;
  PopStack;
end;

procedure TCodeParser.TypeName;
begin
  if (not InDeclaration(TciTypeDeclaration)) then
  begin
    inherited;
    Exit;
  end;
  PushStack(TciTypeName);
  inherited;
  PopStack;
end;

procedure TCodeParser.ExplicitType;
begin
  PushStack(TciTypeKind);
  PushStack(TciTypeCopy);
  inherited ExplicitType;
  PopStack;
  PopStack;
end;

procedure TCodeParser.PointerType;
begin
  PushStack(TciPointerType);
  inherited PointerType();
  PopStack;
end;

procedure TCodeParser.VarDeclaration;
var
  Declaration: TDeclaration;
begin
  Declaration := PushStack(TciVarDeclaration);
  inherited;
  PopStack;

  {$IFDEF PARSER_SEPERATE_VARIABLES}
  SeperateVariables(Declaration as TciVarDeclaration);
  {$ENDIF}
end;

procedure TCodeParser.VarName;
begin
  if (not InDeclaration(TciVarDeclaration)) then
    Exit;
  PushStack(TciVarName);
  inherited;
  PopStack;
end;

procedure TCodeParser.ConstantDeclaration;
var
  Declaration: TDeclaration;
begin
  Declaration := PushStack(TciConstantDeclaration);
  inherited;
  PopStack;

  {$IFDEF PARSER_SEPERATE_VARIABLES}
  SeperateVariables(Declaration as TciVarDeclaration);
  {$ENDIF}
end;

procedure TCodeParser.ConstantName;
begin
  if (not InDeclaration(TciConstantDeclaration)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciVarName);
  inherited;
  PopStack;
end;

procedure TCodeParser.LabelDeclarationSection;
begin
  PushStack(TciLabelDeclaration);
  inherited;
  PopStack;
end;

procedure TCodeParser.LabelId;
begin
  if (not InDeclaration(TciLabelDeclaration)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciLabelName);
  inherited;
  PopStack;
end;

procedure TCodeParser.ProcedureDeclarationSection;
var
  Declaration: TciProcedureDeclaration;
begin
  Declaration := PushStack(TciProcedureDeclaration) as TciProcedureDeclaration;
  case Lexer.TokenID of
    tokOperator: Declaration.IsOperator := True;
    tokFunction: Declaration.IsFunction := True;
  end;

  inherited;
  PopStack;
end;

procedure TCodeParser.FunctionProcedureName;
begin
  if (not InDeclaration(TciProcedureDeclaration)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciProcedureName);
  inherited;
  PopStack;
end;

procedure TCodeParser.ObjectNameOfMethod;
begin
  if (not InDeclaration(TciProcedureDeclaration)) then
  begin
    inherited;
    Exit;
  end;

  TciProcedureDeclaration(FStack.Peek).IsMethodOfType := True;

  PushStack(TciProcedureClassName);
  inherited;
  PopStack;
end;

procedure TCodeParser.ReturnType;
begin
  if (not InDeclarations([TciProcedureDeclaration, TciProceduralType])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciReturnType);
  TypeKind;
  PopStack;
end;

procedure TCodeParser.ForwardDeclaration;
begin
   if (not InDeclaration(TciProcedureDeclaration)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciForward);
  inherited;
  PopStack;
end;

procedure TCodeParser.ConstRefParameter;
var
  Declaration: TDeclaration;
begin
  if (not InDeclaration(TciParameterList)) then
  begin
    inherited;
    Exit;
  end;

  Declaration := PushStack(TciConstRefParameter);
  inherited;
  PopStack;

  {$IFDEF PARSER_SEPERATE_VARIABLES}
  SeperateVariables(Declaration as TciVarDeclaration);
  {$ENDIF}
end;

procedure TCodeParser.ConstParameter;
var
  Declaration: TDeclaration;
begin
  if (not InDeclaration(TciParameterList)) then
  begin
    inherited;
    Exit;
  end;

  Declaration := PushStack(TciConstParameter);
  inherited;
  PopStack();

  {$IFDEF PARSER_SEPERATE_VARIABLES}
  SeperateVariables(Declaration as TciVarDeclaration);
  {$ENDIF}
end;

procedure TCodeParser.OutParameter;
var
  Declaration: TDeclaration;
begin
  if (not InDeclaration(TciParameterList)) then
  begin
    inherited;
    Exit;
  end;

  Declaration := PushStack(TciOutParameter);
  inherited;
  PopStack;

  {$IFDEF PARSER_SEPERATE_VARIABLES}
  SeperateVariables(Declaration as TciVarDeclaration);
  {$ENDIF}
end;

procedure TCodeParser.ParameterFormal;
var
  Declaration: TDeclaration;
begin
  if (not InDeclaration(TciParameterList)) then
  begin
    inherited;
    Exit;
  end;

  Declaration := PushStack(TciFormalParameter);
  inherited;
  PopStack;

  {$IFDEF PARSER_SEPERATE_VARIABLES}
  SeperateVariables(Declaration as TciVarDeclaration);
  {$ENDIF}
end;

procedure TCodeParser.InParameter;
var
  Declaration: TDeclaration;
begin
  if (not InDeclaration(TciParameterList)) then
  begin
    inherited;
    Exit;
  end;

  Declaration := PushStack(TciInParameter);
  inherited;
  PopStack;

  {$IFDEF PARSER_SEPERATE_VARIABLES}
  SeperateVariables(Declaration as TciVarDeclaration);
  {$ENDIF}
end;

procedure TCodeParser.VarParameter;
var
  Declaration: TDeclaration;
begin
  if (not InDeclaration(TciParameterList)) then
  begin
    inherited;
    Exit;
  end;

  Declaration := PushStack(TciVarParameter);
  inherited;
  PopStack;

  {$IFDEF PARSER_SEPERATE_VARIABLES}
  SeperateVariables(Declaration as TciVarDeclaration);
  {$ENDIF}
end;

procedure TCodeParser.ParameterName;
begin
  if (not InDeclarations([TciConstParameter, TciOutParameter, TciFormalParameter, TciInParameter, TciVarParameter, TciConstRefParameter])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciVarName);
  inherited;
  PopStack;
end;

procedure TCodeParser.OldFormalParameterType;
begin
  if (not InDeclarations([TciConstParameter, TciOutParameter, TciFormalParameter, TciInParameter, TciVarParameter, TciConstRefParameter])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciTypeKind);
  //inherited;
  TypeKind;
  PopStack;
end;

procedure TCodeParser.FormalParameterList;
begin
  PushStack(TciParameterList);
  inherited;
  PopStack;
end;

procedure TCodeParser.ArrayType;
begin
  PushStack(TciArrayType);
  inherited;
  PopStack;
end;

procedure TCodeParser.ArrayConstant;
begin
  if (not InDeclaration(TciTypedConstant)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciArrayConstant);
  inherited;
  PopStack;
end;

procedure TCodeParser.NativeType;
begin
  PushStack(TciNativeType);
  inherited NativeType;
  PopStack;
end;

procedure TCodeParser.RecordType;
begin
  PushStack(TciRecordType);
  inherited;
  PopStack;
end;

procedure TCodeParser.UnionType;
begin
  PushStack(TciUnionType);
  inherited;
  PopStack;
end;

procedure TCodeParser.ClassField;
var
  Declaration: TDeclaration;
begin
  if (not InDeclarations([TciRecordType, TciUnionType])) then
  begin
    inherited;
    Exit;
  end;

  Declaration := PushStack(TciClassField);
  inherited;
  PopStack();

  {$IFDEF PARSER_SEPERATE_VARIABLES}
  SeperateVariables(Declaration as TciVarDeclaration);
  {$ENDIF}
end;

procedure TCodeParser.FieldName;
begin
  if (not InDeclaration(TciClassField)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciVarName);
  inherited;
  PopStack;
end;

procedure TCodeParser.AncestorId;
begin
  if (not InDeclarations([TciRecordType, TciNativeType])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciAncestorID);
  inherited;
  PopStack;
end;

procedure TCodeParser.SetType;
begin
  PushStack(TciSetType);
  inherited;
  PopStack;
end;

procedure TCodeParser.OrdinalType;
begin
  if (not InDeclaration(TciArrayType)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciOrdinalType);
  inherited;
  PopStack;
end;

procedure TCodeParser.EnumeratedType;
var
  Declaration: TDeclaration;
begin
  Declaration := PushStack(TciEnumType);

  if Lexer.Defines.IndexOf('!SCOPEDENUMS') > -1 then
    TciEnumType(Declaration).Scoped := True;
  inherited;
  PopStack;
end;

procedure TCodeParser.EnumeratedScopedType;
var
  Declaration: TDeclaration;
begin
  Declaration := PushStack(TciEnumType);
  TciEnumType(Declaration).Scoped := True;
  inherited;
  PopStack;
end;

procedure TCodeParser.QualifiedIdentifier;
begin
  if (not InDeclaration(TciEnumType)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciEnumElement);
  inherited;
  PopStack;
end;

procedure TCodeParser.Run;
var
  Declaration: TDeclaration;
  I: Integer;
begin
  inherited Run();

  for I := 0 to FItems.Count - 1 do
  begin
    Declaration := FItems[I];
    if Declaration.Name = '' then
      Continue;

    if (Declaration is TciProcedureDeclaration) and TciProcedureDeclaration(Declaration).IsMethodOfType then
      FGlobals.Add('!' + TciProcedureDeclaration(Declaration).ObjectName, Declaration)
    else
    if (Declaration is TciTypeDeclaration) and (TciTypeDeclaration(Declaration).EnumType <> nil) then
    begin
      FGlobals.Add(Declaration.Name, Declaration);
      if TciTypeDeclaration(Declaration).EnumType.Scoped then
        Continue;

      for Declaration in TciTypeDeclaration(Declaration).EnumType.Elements do
        FGlobals.Add(Declaration.Name, Declaration);
    end
    else
      FGlobals.Add(Declaration.Name, Declaration);
  end;
end;

procedure TCodeParser.Assign(From: TObject);
begin
  inherited Assign(From);

  if From is TCodeParser then
  begin
    FOnInclude := TCodeParser(From).OnInclude;
    FOnFindInclude := TCodeParser(From).OnFindInclude;
    FOnFindLibrary := TCodeParser(From).OnFindLibrary;
    FOnLoadLibrary := TCodeParser(From).OnLoadLibrary;
  end;
end;

end.
