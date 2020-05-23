unit simba.codeparser;

{$DEFINE PARSER_SEPERATE_VARIABLES}

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
  SysUtils, Classes,
  CastaliaPasLex, CastaliaSimplePasPar, CastaliaPasLexTypes,
  simba.generics;

type
  TDeclaration = class;
  TDeclarationArray = array of TDeclaration;
  TDeclarationClass = class of TDeclaration;

  TDeclarationStack = class(specialize TSimbaStack<TDeclaration>)
  protected
    function getCurItem: _T; override;
  end;

  TDeclarationList = class(specialize TSimbaObjectList<TDeclaration>)
  public
    function GetItemsOfClass(AClass: TDeclarationClass; SubSearch: Boolean = False): TDeclarationArray;
    function GetFirstItemOfClass(AClass: TDeclarationClass; SubSearch: Boolean = False): TDeclaration;
    function GetItemInPos(AStart, AEnd: Integer; SubSearch: Boolean = False): TDeclaration;

    function GetRawText(AClass: TDeclarationClass): String;
    function GetCleanText(AClass: TDeclarationClass): String;
    function GetShortText(AClass: TDeclarationClass): String;
  end;

  TciTypeDeclaration = class;
  TciProcedureDeclaration = class;

  TDeclarationTypeMap = specialize TSimbaStringMap<TciTypeDeclaration>;
  TDeclarationMethodMap = specialize TSimbaStringMap<TDeclaration>;

  TDeclaration = class
  private
    FParser: TmwSimplePasPar;
    FOwner: TDeclaration;
    FOrigin: PAnsiChar;
    FRawText: String;
    FCleanText: String;
    FShortText: String;
    FStartPos: Integer;
    FEndPos: Integer;
    FItems: TDeclarationList;
    FName: String;
    FNameCached: Boolean;
    FNameHash: UInt32;
    FLine: Int32;

    function GetRawText: string; virtual;
    function GetCleanText: string; virtual;
    function GetShortText: string; virtual;
    function GetName: string; virtual;
    function GetNameHash: UInt32;
  public
    function HasOwnerClass(AClass: TDeclarationClass; out Declaration: TDeclaration; Recursive: Boolean = False): Boolean;
    function GetOwnersOfClass(AClass: TDeclarationClass): TDeclarationArray;

    function Clone(Owner: TDeclaration): TDeclaration;
    function IsName(Hash: UInt32): Boolean; inline;
    function IsVisible: Boolean; inline;

    property Parser: TmwSimplePasPar read FParser;
    property Owner: TDeclaration read FOwner write FOwner;
    property Origin: PAnsiChar read FOrigin;

    property RawText: string read GetRawText write FRawText;
    property CleanText: string read GetCleanText;
    property ShortText: string read GetShortText;
    property StartPos: Integer read FStartPos write FStartPos;
    property EndPos: Integer read FEndPos write FEndPos;
    property Items: TDeclarationList read FItems;
    property Name: String read GetName;
    property NameHash: UInt32 read GetNameHash;
    property Line: Int32 read FLine;

    constructor Create(AParser: TmwSimplePasPar; AOwner: TDeclaration; AOrigin: PAnsiChar; AStart: Integer; AEnd: Integer = -1); overload; virtual;
    constructor Create(From: TDeclaration); overload; virtual;
    destructor Destroy; override;
  end;

  TciTypeKind = class(TDeclaration);
  TciProcedureName = class(TDeclaration);
  TciProcedureClassName = class(TciTypeKind)
  protected
    function GetName: string; override;
  end;

  TciReturnType = class(TciTypeKind)
  protected
    function GetName: string; override;
  end;

  EProcedureDirectives = set of TptTokenKind;

  TciProcedureDeclaration = class(TDeclaration)
  private
    fObjectName: String;
    fObjectNameCached: Boolean;
    FProcedureDirectives: EProcedureDirectives;
    FIsFunction: Boolean;
    FIsOperator: Boolean;
    FIsMethodOfType: Boolean;
    FHeader: String;

    function GetHeader: String;
    function GetName: String; override;
    function GetObjectName: String;
    function GetReturnType: TciReturnType;
  public
    function GetParamDeclarations: TDeclarationArray;
    function IsObjectName(Value: String): Boolean;

    property IsFunction: Boolean read FIsFunction write FIsFunction;
    property IsOperator: Boolean read FIsOperator write FIsOperator;
    property IsMethodOfType: Boolean read FIsMethodOfType write FIsMethodOfType;
    property ReturnType: TciReturnType read GetReturnType;
    property ObjectName: String read GetObjectName;
    property Directives: EProcedureDirectives read FProcedureDirectives write FProcedureDirectives;
    property Header: String read GetHeader;
  end;

  TciInclude = class(TDeclaration);
  TciJunk = class(TDeclaration);

  TciCompoundStatement = class(TDeclaration);
  TciWithStatement = class(TDeclaration);
  TciSimpleStatement = class(TDeclaration);
  TciVariable = class(TDeclaration);

  TciTypedConstant = class(TDeclaration);
  TciExpression = class(TDeclaration);
  TciProceduralType = class(TciProcedureDeclaration);
  TciNativeType = class(TDeclaration);

  TciTypeCopy = class(TDeclaration);
  TciTypeAlias = class(TDeclaration);
  TciTypeName = class(TDeclaration);
  TciTypeIdentifer = class(TDeclaration);

  TciEnumType = class(TDeclaration);
  TciEnumElement = class(TDeclaration)
  protected
    function GetName: string; override;
  end;

  TciSetType = class(TDeclaration);
  TciRecordType = class(TDeclaration);

  TciTypeDeclaration = class(TDeclaration)
  protected
    FParent: String;
    FParentCached: Boolean;

    function GetName: String; override;
  public
    function GetParent: String;
    function GetType: TDeclaration;
    function GetFields: TDeclarationArray;
    function GetEnumElements: TDeclarationArray;
  end;

  TciVarName = class(TDeclaration);
  TciVarDeclaration = class(TDeclaration)
  protected
    FVarType: TDeclaration;

    function GetValue: TDeclaration;
    function GetVarType: TDeclaration;
    function GetName: string; override;
  public
    // For parameter hints so we can restructure.
    // var
    //   a, b, c: Int32; (group 0)
    //   d: Extended;    (group 1)
    Group: Int32;

    property VarType: TDeclaration read GetVarType;
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

  TciArrayType = class(TDeclaration)
  public
    function GetDimensionCount: Int32;
    function GetType: TciTypeKind;
  end;

  TciArrayConstant = class(TDeclaration);
  TciUnionType = class(TDeclaration);

  TciClassField = class(TciVarDeclaration);

  TciRecordConstant = class(TDeclaration);
  TciRecordFieldConstant = class(TDeclaration);

  TciAncestorId = class(TDeclaration);
  TciIdentifier = class(TDeclaration);

  TciOrdinalType = class(TDeclaration);

  TCodeParser = class(TmwSimplePasPar)
  protected
    FStack: TDeclarationStack;
    FItems: TDeclarationList;
    FTypes: TDeclarationTypeMap;
    FTypeMethods: TDeclarationMethodMap;
    FTokenPos: Int32;

    procedure SeperateVariables(Variables: TciVarDeclaration);

    function InDeclaration(AClass: TDeclarationClass): Boolean;
    function InDeclarations(AClassArray: array of TDeclarationClass): Boolean;
    function PushStack(AClass: TDeclarationClass; AStart: Integer = -1): TDeclaration;
    procedure PopStack(AEnd: Integer = -1);

    procedure ParseFile; override;
    procedure OnInclude(Sender: TmwBasePasLex); virtual;                        //Includes
    procedure NextToken; override;                                              //Junk
    procedure OnDirect(Sender: TmwBasePasLex);                                  //Junk

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
    procedure NewFormalParameterType; override;                                 //Procedure/Function Parameters
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
    procedure QualifiedIdentifier; override;                                    //Enum
  public
    property Items: TDeclarationList read FItems;
    property Types: TDeclarationTypeMap read FTypes;
    property TypeMethods: TDeclarationMethodMap read FTypeMethods;

    constructor Create;
    destructor Destroy; override;
  end;

operator + (Left: TDeclarationArray; Right: TDeclaration): TDeclarationArray;
operator + (Left: TDeclarationArray; Right: TDeclarationArray): TDeclarationArray;

implementation

operator + (Left: TDeclarationArray; Right: TDeclaration): TDeclarationArray;
begin
  SetLength(Result, Length(Left) + 1);

  if Length(Left) > 0 then
    Move(Left[0], Result[0], Length(Left) * SizeOf(TDeclaration));

  Result[High(Result)] := Right;
end;

operator + (Left: TDeclarationArray; Right: TDeclarationArray): TDeclarationArray;
begin
  SetLength(Result, Length(Left) + Length(Right));

  if Length(Result) > 0 then
  begin
    if Length(Left) > 0 then
      Move(Left[0], Result[0], Length(Left) * SizeOf(TDeclarationArray));
    if Length(Right) > 0 then
      Move(Right[0], Result[Length(Left)], Length(Right) * SizeOf(TDeclarationArray));
  end;
end;

function TDeclarationStack.getCurItem: _T;
begin
  if FCur >= 0 then
    Result := FArr[FCur]
  else
    Result := nil;
end;

function TciEnumElement.GetName: string;
begin
  if (not FNameCached) then
  begin
    FNameCached := True;
    FName := RawText;
  end;

  Result := FName;
end;

function TciProcedureClassName.GetName: string;
begin
  Result := 'Self';
end;

function TciReturnType.GetName: string;
begin
  Result := 'Result';
end;

function TciArrayType.GetDimensionCount: Int32;
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
  Index: Int32;
begin
  Result := nil;

  Declarations := FItems.GetItemsOfClass(TciTypeKind, True);
  Index := GetDimensionCount() - 1;
  if (Index >= 0) and (Index <= High(Declarations)) then
    Result := Declarations[Index] as TciTypeKind;
end;

function TciVarDeclaration.GetValue: TDeclaration;
begin
  Result := FItems.GetFirstItemOfClass(TciExpression);
end;

function TciVarDeclaration.GetVarType: TDeclaration;
begin
  Result := FItems.GetFirstItemOfClass(TciTypeKind);
end;

function TciVarDeclaration.GetName: string;
var
  Declaration: TDeclaration;
begin
  if (not FNameCached) then
  begin
    FNameCached := True;

    Declaration := FItems.GetFirstItemOfClass(TciVarName);
    if Declaration <> nil then
      FName := Declaration.CleanText;
  end;

  Result := FName;
end;

function TciTypeDeclaration.GetName: String;
var
  Declaration: TDeclaration;
begin
  if (not FNameCached) then
  begin
    FNameCached := True;

    Declaration := FItems.GetFirstItemOfClass(TciTypeName);
    if Declaration <> nil then
      FName := Declaration.CleanText;
  end;

  Result := FName;
end;

function TciTypeDeclaration.GetParent: String;
begin
  if (not FParentCached) then
  begin
    FParentCached := True;

    if (GetType() is TciRecordType) or (GetType() is TciNativeType) then
      FParent := GetType().Items.GetRawText(TciAncestorId)
    else
    if (GetType() is TciTypeCopy) or (GetType() is TciTypeAlias) then
      FParent := GetType().Items.GetRawText(TciTypeIdentifer);

    if UpperCase(FParent) = UpperCase(Self.Name) then  // type Pointer = Pointer
      FParent := '';
  end;

  Result := FParent;
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

function TciTypeDeclaration.GetFields: TDeclarationArray;
var
  Declaration: TDeclaration;
begin
  Result := nil;

  Declaration := GetType();
  if (Declaration <> nil) and (Declaration is TciRecordType) then
    Result := Declaration.Items.GetItemsOfClass(TciClassField);
end;

function TciTypeDeclaration.GetEnumElements: TDeclarationArray;
var
  Declaration: TDeclaration;
begin
  Result := nil;

  Declaration := GetType();
  if (Declaration <> nil) and (Declaration is TciEnumType) then
    Result := Declaration.Items.GetItemsOfClass(TciEnumElement, True);
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

  for i := 0 to FCount - 1 do
    SearchItem(AClass, SubSearch, FItems[i], Result, l);
end;

function TDeclarationList.GetFirstItemOfClass(AClass: TDeclarationClass; SubSearch: Boolean = False): TDeclaration;

  function SearchItem(AClass: TDeclarationClass; SubSearch: Boolean; Item: TDeclaration; out Res: TDeclaration): Boolean;
  var
    i: Integer;
  begin
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

  for i := 0 to FCount - 1 do
    if SearchItem(AClass, SubSearch, FItems[i], Result) then
      Exit;
end;

function TDeclarationList.GetItemInPos(AStart,  AEnd: Integer; SubSearch: Boolean = False): TDeclaration;

  function SearchItem(AStart, AEnd: Integer; SubSearch: Boolean; Item: TDeclaration; out Res: TDeclaration): Boolean;
  var
    i: Integer;
    b: Boolean;
  begin
    Result := False;;

    b := (AStart >= Item.StartPos) and (AEnd <= Item.EndPos);
    if b and ((Item.Items.Count < 1) or (not SubSearch)) then
    begin
      Res := Item;
      Result := True;
      Exit;
    end;
    if SubSearch and b then
      for i := 0 to Item.Items.Count - 1 do
        if SearchItem(AStart, AEnd, SubSearch, Item.Items[i], Res) then
        begin
          Result := True;
          Break;
        end;
    if b and (not Result) then
    begin
      Res := Item;
      Result := True;
    end;
  end;

var
  i: Integer;
begin
  Result := nil;

  for i := 0 to FCount - 1 do
    SearchItem(AStart, AEnd, SubSearch, FItems[i], Result);
end;

function TDeclarationList.GetRawText(AClass: TDeclarationClass): String;
var
  Declaration: TDeclaration;
begin
  Result := '';

  Declaration := GetFirstItemOfClass(AClass);
  if Declaration <> nil then
    Result := Declaration.RawText;
end;

function TDeclarationList.GetCleanText(AClass: TDeclarationClass): String;
var
  Declaration: TDeclaration;
begin
  Result := '';

  Declaration := GetFirstItemOfClass(AClass);
  if Declaration <> nil then
    Result := Declaration.CleanText;
end;

function TDeclarationList.GetShortText(AClass: TDeclarationClass): String;
var
  Declaration: TDeclaration;
begin
  Result := '';

  Declaration := GetFirstItemOfClass(AClass);
  if Declaration <> nil then
    Result := Declaration.ShortText;
end;

function TDeclaration.GetRawText: string;
begin
  if FRawText = '' then
  begin
    SetLength(FRawText, FEndPos - FStartPos);
    if Length(FRawText) > 0 then
      Move(FOrigin[FStartPos], FRawText[1], Length(FRawText));
  end;

  Result := FRawText;
end;

function TDeclaration.GetCleanText: string;
var
  i: Integer;
  a: TDeclarationArray;
  s: string;
begin
  if (FCleanText = '') and (FStartPos <> FEndPos) and (FOrigin <> nil) then
  begin
    s := RawText;
    a := Items.GetItemsOfClass(TciJunk, True);
    for i := High(a) downto 0 do
    begin
      Delete(s, a[i].StartPos - FStartPos + 1, a[i].EndPos - a[i].StartPos);
      if (Pos(LineEnding, a[i].GetRawText) > 0) then
        Insert(LineEnding, s, a[i].StartPos - FStartPos + 1)
      else
        Insert(' ', s, a[i].StartPos - FStartPos + 1);
    end;
    FCleanText := s;
  end;

  Result := FCleanText;
end;

function TDeclaration.GetShortText: string;

  function SingleLine(const S: String): String;
  begin
    Result := S;

    while (Pos(LineEnding, Result) > 0) do
      Result := StringReplace(Result, LineEnding, #32, [rfReplaceAll]);
    while (Pos(#9, Result) > 0) do
      Result := StringReplace(Result, #9, #32, [rfReplaceAll]);
    while (Pos(#32#32, Result) > 0) do
      Result := StringReplace(Result, #32#32, #32, [rfReplaceAll]);
  end;

begin
  if (FShortText = '') then
    FShortText := SingleLine(CleanText);

  Result := FShortText;
end;

function TDeclaration.GetName: string;
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

function TDeclaration.Clone(Owner: TDeclaration): TDeclaration;
var
  i: Int32;
begin
  Result := TDeclarationClass(Self.ClassType).Create(Self);
  Result.Owner := Owner;
  for i := 0 to Self.Items.Count - 1 do
    Result.Items.Add(Self.Items[i].Clone(Result));
end;

function TDeclaration.GetNameHash: UInt32;
begin
  if FNameHash = 0 then
    FNameHash := HashString(UpperCase(Self.Name));

  Result := FNameHash;
end;

function TDeclaration.IsName(Hash: UInt32): Boolean;
begin
  Result := Self.NameHash = Hash;
end;

function TDeclaration.IsVisible: Boolean;
begin
  if (Self.Parser.Lexer.MaxPos > -1) then
  begin
    Result := ((Self.ClassType = TciProcedureDeclaration) and (TciProcedureDeclaration(Self).IsMethodOfType)) or // Lape type method forwarding.
               (Self.EndPos < Self.Parser.Lexer.MaxPos);
  end else
    Result := True;
end;

constructor TDeclaration.Create(AParser: TmwSimplePasPar; AOwner: TDeclaration; AOrigin: PAnsiChar; AStart: Integer; AEnd: Integer);
begin
  inherited Create;

  FParser := AParser;
  FLine := AParser.Lexer.LineNumber;
  FOwner := AOwner;
  FOrigin := AOrigin;
  FRawText := '';
  FCleanText := '';
  FStartPos := AStart;
  if (AEnd > -1) then
    FEndPos := AEnd
  else
    FEndPos := AStart;

  FItems := TDeclarationList.Create(True);
end;

constructor TDeclaration.Create(From: TDeclaration);
begin
  Create(From.Parser, From.Owner, From.Origin, From.StartPos, From.EndPos);
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

    FHeader := FHeader + Items.GetCleanText(TciParameterList);
    if ReturnType <> nil then
      FHeader := FHeader + ': ' + ReturnType.CleanText;

    FHeader := FHeader + ';';

    for Directive in Directives do
      FHeader := FHeader + ' ' + TokenName(Directive) + ';';
  end;

  Result := FHeader;
end;

function TciProcedureDeclaration.GetName: String;
var
  Declaration: TDeclaration;
begin
  if (not FNameCached) then
  begin
    FNameCached := True;

    Declaration := FItems.GetFirstItemOfClass(TciProcedureName);
    if (Declaration <> nil) then
      FName := Declaration.CleanText;
  end;

  Result := FName;
end;

function TciProcedureDeclaration.GetObjectName: String;
var
  Declaration: TDeclaration;
begin
  if (not fObjectNameCached) then
  begin
    fObjectNameCached := True;

    Declaration := FItems.GetFirstItemOfClass(TciProcedureClassName);
    if Declaration <> nil then
      fObjectName := Declaration.CleanText;
  end;

  Result := fObjectName;
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
        Result := Result + Declaration.Items[i];
end;

function TciProcedureDeclaration.IsObjectName(Value: String): Boolean; inline;
begin
  Result := UpperCase(Value) = UpperCase(Self.ObjectName);
end;

function TCodeParser.InDeclaration(AClass: TDeclarationClass): Boolean;
begin
  if (FStack.Top = nil) then
    Result := (AClass = nil)
  else
    Result := (FStack.Top is AClass);
end;

function TCodeParser.InDeclarations(AClassArray: array of TDeclarationClass): Boolean;
var
  i: Integer;
  t: TDeclaration;
begin
  Result := False;
  t := FStack.Top;
  if (t = nil) then
  begin
    for i := Low(AClassArray) to High(AClassArray) do
      if (AClassArray[i] = nil) then
      begin
        Result := True;
        Break;
      end;
    Exit;
  end;
  for i := Low(AClassArray) to High(AClassArray) do
    if (t is AClassArray[i]) then
    begin
      Result := True;
      Break;
    end;
end;

function TCodeParser.PushStack(AClass: TDeclarationClass; AStart: Integer): TDeclaration;
begin
  if (AStart = -1) then
    AStart := Lexer.TokenPos;

  Result := AClass.Create(Self, FStack.Top, Lexer.Origin, AStart);

  if (FStack.Top <> nil) then
    FStack.Top.Items.Add(Result)
  else
    FItems.Add(Result);

  FStack.Push(Result);
end;

procedure TCodeParser.SeperateVariables(Variables: TciVarDeclaration);
var
  i: Int32;
  Index: Int32;
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
    AEnd := FTokenPos;

  if (FStack.Top <> nil) then
    FStack.Top.EndPos := AEnd;

  FStack.Pop();
end;

constructor TCodeParser.Create;
begin
  inherited;

  FStack := TDeclarationStack.Create;
  FItems := TDeclarationList.Create(True);
  FTypes := TDeclarationTypeMap.Create(nil, dupAccept, False);
  FTypeMethods := TDeclarationMethodMap.Create(nil, dupAccept, False);

  Lexer.OnIncludeDirect := {$IFDEF FPC}@{$ENDIF}OnInclude;
  Lexer.OnDefineDirect := {$IFDEF FPC}@{$ENDIF}OnDirect;
  Lexer.OnElseDirect := {$IFDEF FPC}@{$ENDIF}OnDirect;
  Lexer.OnEndIfDirect := {$IFDEF FPC}@{$ENDIF}OnDirect;
  Lexer.OnIfDefDirect := {$IFDEF FPC}@{$ENDIF}OnDirect;
  Lexer.OnIfNDefDirect := {$IFDEF FPC}@{$ENDIF}OnDirect;
  Lexer.OnUnDefDirect := {$IFDEF FPC}@{$ENDIF}OnDirect;
  Lexer.OnIfDirect := {$IFDEF FPC}@{$ENDIF}OnDirect;
  Lexer.OnIfEndDirect := {$IFDEF FPC}@{$ENDIF}OnDirect;
  Lexer.OnElseIfDirect := {$IFDEF FPC}@{$ENDIF}OnDirect;
end;

destructor TCodeParser.Destroy;
begin
  FStack.Free();
  FItems.Free();
  FTypes.Free();
  FTypeMethods.Free();

  inherited;
end;

procedure TCodeParser.ParseFile;
begin
  SkipJunk;
  case GenID of
    tokLibrary:
      begin
        LibraryFile;
      end;
    tokPackage:
      begin
        PackageFile;
      end;
    tokUnit:
      begin
        UnitFile;
      end;
    else
    begin
      if (Lexer.GenID = TokProgram) then
      begin
        Expected(TokProgram);
        QualifiedIdentifier;
        if TokenID = TokRoundOpen then
        begin
          NextToken;
          IdentifierList;
          Expected(TokRoundClose);
        end;
        SemiColon;
      end;
      if (TokenID = TokUses) then
        MainUsesClause;

      while (TokenID in [TokClass, TokConst, TokConstructor, TokDestructor, TokExports, TokFunction, TokLabel, TokProcedure, TokResourceString, TokThreadVar, TokType, TokVar]) or (Lexer.ExID = tokOperator) do
        DeclarationSection;

      if (TokenID = TokBegin) then
      begin
        CompoundStatement;

        if (TokenID = tokSemiColon) then
        begin
          Expected(tokSemiColon);
          ParseFile();
        end
        else
          Expected(tokPoint);
      end;
    end;
  end;
end;

procedure TCodeParser.OnInclude(Sender: TmwBasePasLex);
begin
  if (not Sender.IsJunk) then
  begin
    PushStack(TciInclude, Sender.TokenPos);
    FStack.Top.RawText := Sender.DirectiveParamOriginal;
    PopStack(Sender.TokenPos + Sender.TokenLen);
  end;

  Sender.Next;
end;

procedure TCodeParser.NextToken;
begin
  Lexer.Next;

  FTokenPos := Lexer.TokenPos;

  if Lexer.IsJunk and (not InDeclaration(TciJunk)) then
  begin
    repeat
      if (Lexer.TokenID in [tokAnsiComment, tokBorComment, tokSlashesComment]) then
      begin
        if (not InDeclaration(TciJunk)) then
          PushStack(TciJunk);
      end
      else if InDeclaration(TciJunk) then
        PopStack(Lexer.TokenPos);

      Lexer.Next;
    until (not Lexer.IsJunk);

    if InDeclaration(TciJunk) then
      PopStack(Lexer.TokenPos);
  end;
end;

procedure TCodeParser.OnDirect(Sender: TmwBasePasLex);
begin
  if (Sender.TokenID = TokElseDirect) then
  begin
    Sender.Next;
    Exit;
  end;
  if InDeclaration(TciJunk) then
    Exit;
  if (not InDeclaration(nil)) then
    PushStack(TciJunk, Sender.TokenPos);

  if (not (Sender.TokenID in [TokEndIfDirect, TokIfEndDirect])) then
    if Sender = Lexer then
      NextToken
    else
      Sender.Next;

  if InDeclaration(TciJunk) then
    PopStack(Sender.TokenPos + Sender.TokenLen);
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
begin
  if (not InDeclarations([TciVarDeclaration, TciConstantDeclaration, TciOrdinalType])) then
  begin
    inherited;
    Exit;
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
    with FStack.Top as TciProcedureDeclaration do
      Directives := Directives + [ExID];

  inherited ProceduralDirective;
end;

procedure TCodeParser.TypeAlias;
begin
  PushStacK(TciTypeKind);
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
var
  Declaration: TDeclaration;
begin
  Declaration := PushStack(TciTypeDeclaration);
  inherited;
  PopStack;

  if FStack.Top = nil then
    FTypes.Add(Declaration.Name, Declaration as TciTypeDeclaration);
end;

procedure TCodeParser.TypeName;
begin
  if (not InDeclaration(TciTypeDeclaration)) then
    Exit;
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
    tokFunction: Declaration.IsFunction := True;
    tokOperator: Declaration.IsOperator := True;
  end;

  inherited;
  PopStack;

  if (FStack.Top = nil) and (Declaration.ObjectName <> '') then
    FTypeMethods.Add(Declaration.ObjectName, Declaration);
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

  TciProcedureDeclaration(FStack.Top).IsMethodOfType := True;

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

procedure TCodeParser.NewFormalParameterType;
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
  if (not InDeclarations([TciArrayType])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciOrdinalType);
  inherited;
  PopStack;
end;

procedure TCodeParser.EnumeratedType;
begin
  PushStack(TciEnumType);
  inherited;
  PopStack;
end;

procedure TCodeParser.QualifiedIdentifier;
begin
  if (not InDeclarations([TciEnumType])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciEnumElement);
  inherited;
  PopStack;
end;

end.
