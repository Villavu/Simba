{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_codetools_parser;

{$i simba.inc}

interface

uses
  SysUtils, Classes,
  mPasLexTypes, mPasLex, mSimplePasPar,
  simba.mufasatypes, simba.ide_codetools_utils,
  simba.stack, simba.list, simba.stringbuilder;

type
  TCodeParser = class;

  TDeclaration = class;
  TDeclarationArray = array of TDeclaration;
  TDeclarationClass = class of TDeclaration;

  TDeclarationList = class(TObject)
  protected
    FItems: TDeclarationArray;
    FCount: Integer;

    function GetItem(Index: Integer): TDeclaration;
  public
    function ToArray: TDeclarationArray;
    function GetByClassAndName(AClass: TDeclarationClass; AName: String; SubSearch: Boolean = False): TDeclarationArray;
    function GetByName(AName: String; SubSearch: Boolean = False): TDeclarationArray;

    function GetItemsOfClass(AClass: TDeclarationClass; SubSearch: Boolean = False): TDeclarationArray;
    function GetFirstItemOfClass(AClass: TDeclarationClass; SubSearch: Boolean = False): TDeclaration;
    function GetItemInPosition(Position: Integer; CheckEnd: Boolean = True): TDeclaration;

    function GetTextOfClass(AClass: TDeclarationClass): String;
    function GetTextOfClassNoComments(AClass: TDeclarationClass): String;
    function GetTextOfClassNoCommentsSingleLine(AClass: TDeclarationClass; Prefix: String = ''): String;

    procedure Add(Decl: TDeclaration);
    procedure Extend(Decls: TDeclarationArray);
    procedure Clear(FreeDecls: Boolean = False);

    property Count: Integer read FCount;
    property Items[Index: Integer]: TDeclaration read GetItem; default;
  end;

  TDeclarationMap = class(TObject)
  protected
    FList: TStringList;
  public
    procedure Add(Name: String; Decl: TDeclaration);
    function Get(Name: String): TDeclarationArray;

    procedure Clear;

    constructor Create;
    destructor Destroy; override;
  end;

  TDeclarationStack = specialize TSimbaStack<TDeclaration>;

  {$IFDEF PARSER_LEAK_CHECKS}
  TLeakChecker = class(TObject)
  protected
  class var
    FList: TList;
  public
    class constructor Create;
    class destructor Destroy;

    constructor Create; virtual;
    destructor Destroy; override;
  end;
  {$ENDIF}

  TDeclaration = class({$IFDEF PARSER_LEAK_CHECKS}TLeakChecker{$ELSE}TObject{$ENDIF})
  protected
    FParser: TCodeParser;
    FOwner: TDeclaration;
    FStartPos: Integer;
    FEndPos: Integer;
    FItems: TDeclarationList;
    FLine: Integer;
    FLexer: TmwPasLex;

    FName: TNullableString;
    FText: TNullableString;
    FTextNoComments: TNullableString;
    FTextNoCommentsSingleLine: TNullableString;

    FFlags: UInt16;

    procedure SetFlags(const Index: Integer; const AValue: Boolean);
    function GetFlags(const Index: Integer): Boolean;

    function GetText: String;
    function GetTextNoComments: String;
    function GetTextNoCommentsSingleLine: String;

    function GetName: string; virtual;
    procedure SetName(Value: String); virtual;
  public
    function Dump: String; virtual;

    function IsName(const Value: String): Boolean; inline;

    property Lexer: TmwPasLex read FLexer;
    property Owner: TDeclaration read FOwner;

    property StartPos: Integer read FStartPos;
    property EndPos: Integer read FEndPos;
    property Items: TDeclarationList read FItems;
    property Name: String read GetName write SetName;
    property Line: Integer read FLine;

    property Text: String read GetText;
    property TextNoComments: String read GetTextNoComments;
    property TextNoCommentsSingleLine: String read GetTextNoCommentsSingleLine;

    function GetOwnerByClass(AClass: TDeclarationClass): TDeclaration;

    // Simple helper flags
    property isProcedure: Boolean index 1 read GetFlags write SetFlags;
    property isFunction: Boolean index 2 read GetFlags write SetFlags;
    property isObjectMethod: Boolean index 3 read GetFlags write SetFlags;
    property isOperatorMethod: Boolean index 4 read GetFlags write SetFlags;
    property isOverrideMethod: Boolean index 5 read GetFlags write SetFlags;

    property isVar: Boolean index 6 read GetFlags write SetFlags;
    property isConst: Boolean index 7 read GetFlags write SetFlags;
    property isType: Boolean index 8 read GetFlags write SetFlags;

    constructor Create(AParser: TCodeParser; AOwner: TDeclaration; AStart: Integer; AEnd: Integer); reintroduce;
    destructor Destroy; override;
  end;

  TDeclaration_Root = class(TDeclaration)
  public
    constructor Create; reintroduce;
  end;

  TDeclaration_CompoundStatement = class(TDeclaration);
  TDeclaration_WithStatement = class(TDeclaration);
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

    DefToken: TptTokenKind;
  end;
  TDeclaration_ParamStub = class(TDeclaration_VarStub)
  public
    ParamType: TptTokenKind;
  end;
  TDeclaration_ParamList = class(TDeclaration);
  TDeclaration_ParamGroup = class(TDeclaration);

  TDeclaration_ParentType = class(TDeclaration);
  TDeclaration_Identifier = class(TDeclaration);
  TDeclaration_OrdinalType = class(TDeclaration);

  TDeclaration_Type = class(TDeclaration);

  TDeclaration_TypeRecord = class(TDeclaration_Type)
  public
    function Parent: TDeclaration;
    function Fields: TDeclarationArray;
  end;

  TDeclaration_TypeUnion = class(TDeclaration_Type);
  TDeclaration_TypeArray = class(TDeclaration_Type)
  public
    function Dump: String; override;
    function DimCount: Integer;
    function VarType: TDeclaration;
  end;

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
    function VarType: TDeclaration;
  end;

  TDeclaration_TypeSet = class(TDeclaration_Type);
  TDeclaration_TypeRange = class(TDeclaration_Type);
  TDeclaration_TypeNativeMethod = class(TDeclaration_Type);

  TDeclaration_EnumElement = class(TDeclaration)
  public
    function GetName: string; override;
  end;

  TDeclaration_EnumElementName = class(TDeclaration)
  protected
    function GetName: string; override;
  end;

  TDeclaration_TypeEnum = class(TDeclaration_Type)
  public
    function Elements: TDeclarationArray;
  end;

  TDeclaration_TypeEnumScoped = class(TDeclaration_Type);

  TDeclaration_VarType = class(TDeclaration);
  TDeclaration_VarDefault = class(TDeclaration);
  TDeclaration_Var = class(TDeclaration)
  protected
    FVarTypeString: TNullableString;
    FVarDefaultString: TNullableString;

    function GetVarType: TDeclaration;
    function GetVarTypeString: String;
    function GetVarDefaultString: String;
  public
    DefToken: TptTokenKind;

    property VarType: TDeclaration read GetVarType;
    property VarTypeString: String read GetVarTypeString;
    property VarDefaultString: String read GetVarDefaultString;
  end;

  TDeclaration_Const = class(TDeclaration_Var);
  TDeclaration_Field = class(TDeclaration_Var);

  EMethodDirectives = TptTokenSet;

  TDeclaration_Method = class(TDeclaration)
  protected
    FParamString: TNullableString;
    FResultString: TNullableString;
    FHeaderString: TNullableString;

    function GetParamString: String;
    function GetResultString: String;
    function GetHeaderString: String;
  public
    ObjectName: String;
    Directives: EMethodDirectives;

    function ResultType: TDeclaration;
    function Dump: String; override;

    property ParamString: String read GetParamString;
    property ResultString: String read GetResultString;
    property HeaderString: String read GetHeaderString;
  end;

  TDeclaration_TypeMethod = class(TDeclaration_Method);

  TDeclaration_MethodResult = class(TDeclaration);
  TDeclaration_Parameter = class(TDeclaration)
  public
    ParamType: TptTokenKind;
    DefToken: TptTokenKind;

    function VarTypeString: String;
    function DefaultValueString: String;
    function Dump: String; override;
  end;

  TOnFindInclude = function(Sender: TmwBasePasLex; var FileName: string): Boolean of object;
  TOnHandleInclude = function(Sender: TmwBasePasLex): Boolean of object;
  TOnHandleLibrary = function(Sender: TmwBasePasLex): Boolean of object;

  TCodeParser = class(TmwSimplePasPar)
  protected
    FManagedItems: TDeclarationList;

    FRoot: TDeclaration;
    FItems: TDeclarationList;
    FStack: TDeclarationStack;
    FOnFindInclude: TOnFindInclude;
    FOnHandleInclude: TOnHandleInclude;
    FOnHandleLibrary: TOnHandleLibrary;

    FLocals: TDeclarationList;
    FGlobals: TDeclarationList;
    FTypeMethods: TDeclarationMap;

    FHash: String;

    procedure FindLocals;
    procedure FindGlobals;

    // Hashes lexers filenames, fileage and defines.
    function GetHash: String; virtual;

    function GetCaretPos: Integer;
    function GetMaxPos: Integer;
    procedure SetCaretPos(const Value: Integer);
    procedure SetMaxPos(const Value: Integer);

    function InDeclaration(AClass: TDeclarationClass): Boolean;
    function InDeclarations(AClassArray: array of TDeclarationClass): Boolean;
    function PushStack(AClass: TDeclarationClass): TDeclaration;
    procedure PopStack;

    procedure ParseFile; override;
    procedure OnLibraryDirect(Sender: TmwBasePasLex); override;
    procedure OnIncludeDirect(Sender: TmwBasePasLex); override;                 //Includes

    procedure CompoundStatement; override;                                      //Begin-End
    procedure WithStatement; override;                                          //With
    procedure Variable; override;                                               //With

    procedure ConstantType; override;
    procedure ConstantValue; override;
    procedure ConstantExpression; override;
    procedure TypeKind; override;                                               //Var + Const + Array + Record
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

    procedure ProceduralDirective; override;                                    //Procedure/Function directives
    procedure ProcedureDeclarationSection; override;                            //Procedure/Function
    procedure FunctionProcedureName; override;                                  //Procedure/Function
    procedure ObjectNameOfMethod; override;                                     //Class Procedure/Function
    procedure ReturnType; override;                                             //Function Result
    procedure ConstRefParameter; override;                                      //Procedure/Function Parameters
    procedure ConstParameter; override;                                         //Procedure/Function Parameters
    procedure OutParameter; override;                                           //Procedure/Function Parameters
    procedure NormalParameter; override;                                        //Procedure/Function Parameters
    procedure VarParameter; override;                                           //Procedure/Function Parameters
    procedure ParameterName; override;                                          //Procedure/Function Parameters
    procedure ParameterVarType; override;                                       //Procedure/Function Parameters
    procedure FormalParameterSection; override;
    procedure FormalParameterList; override;
    procedure ArrayType; override;                                              //Array

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
    procedure EnumeratedTypeItem; override;                                     //Enum Element
    procedure QualifiedIdentifier; override;                                    //Enum Element Name
  public
    property Items: TDeclarationList read FItems;
    property Locals: TDeclarationList read FLocals;
    property Globals: TDeclarationList read FGlobals;
    property TypeMethods: TDeclarationMap read FTypeMethods;

    property OnFindInclude: TOnFindInclude read FOnFindInclude write FOnFindInclude;
    property OnHandleInclude: TOnHandleInclude read FOnHandleInclude write FOnHandleInclude;
    property OnHandleLibrary: TOnHandleLibrary read FOnHandleLibrary write FOnHandleLibrary;

    property CaretPos: Integer read GetCaretPos write SetCaretPos;
    property MaxPos: Integer read GetMaxPos write SetMaxPos;

    property Hash: String read GetHash;

    procedure Reset; override;
    procedure Run; override;

    function DebugTree: String;
    function DebugGlobals: String;

    constructor Create; override;
    destructor Destroy; override;
  end;
  TCodeParserArray = array of TCodeParser;
  TCodeParserList = specialize TSimbaList<TCodeParser>;

implementation

procedure TDeclarationMap.Add(Name: String; Decl: TDeclaration);
var
  Index: Integer;
  Decls: TDeclarationList;
begin
  if FList.Find(Name, Index) then
    Decls := TDeclarationList(FList.Objects[Index])
  else
    Decls := TDeclarationList(FList.Objects[FList.AddObject(Name, TDeclarationList.Create())]);

  Decls.Add(Decl);
end;

function TDeclarationMap.Get(Name: String): TDeclarationArray;
var
  Index: Integer;
begin
  if FList.Find(Name, Index) then
    Result := TDeclarationList(FList.Objects[Index]).ToArray()
  else
    Result := nil;
end;

procedure TDeclarationMap.Clear;
begin
  FList.Clear();
end;

constructor TDeclarationMap.Create;
begin
  inherited Create();

  FList := TStringList.Create();
  FList.Sorted := True;
  FList.OwnsObjects := True;
  FList.UseLocale := False;
end;

destructor TDeclarationMap.Destroy;
begin
  if (FList <> nil) then
    FreeAndNil(FList);

  inherited Destroy();
end;

constructor TDeclaration_Root.Create;
begin
  inherited Create(nil, nil, 0, 0);
end;

function TDeclaration_EnumElement.GetName: string;
begin
  if FName.IsNull then
    FName.Value := Items.GetTextOfClass(TDeclaration_EnumElementName);

  Result := inherited;
end;

function TDeclaration_TypeAlias.VarType: TDeclaration;
begin
  Result := Items.GetFirstItemOfClass(TDeclaration_VarType);
end;

function TDeclaration_TypeCopy.VarType: TDeclaration;
begin
  Result := Items.GetFirstItemOfClass(TDeclaration_VarType);
end;

function TDeclaration_TypePointer.VarType: TDeclaration;
begin
  Result := Items.GetFirstItemOfClass(TDeclaration_VarType);
end;

function TDeclaration_TypeArray.Dump: String;
begin
  Result := inherited + ' [' + IntToStr(DimCount) + ']';
end;

function TDeclaration_TypeArray.DimCount: Integer;
var
  Decl: TDeclaration;
begin
  Result := 1;

  Decl := Items.GetFirstItemOfClass(TDeclaration_TypeArray);
  while (Decl <> nil) and (Decl.Owner is TDeclaration_TypeArray) do
  begin
    Decl := Decl.Items.GetFirstItemOfClass(TDeclaration_TypeArray);

    Inc(Result);
  end;
end;

function TDeclaration_TypeArray.VarType: TDeclaration;
begin
  Result := Items.GetFirstItemOfClass(TDeclaration_VarType);
end;

function TDeclaration_TypeRecord.Parent: TDeclaration;
begin
  Result := Items.GetFirstItemOfClass(TDeclaration_ParentType);
  if (Result <> nil) then
    Result := Result.Items.GetFirstItemOfClass(TDeclaration_VarType);
end;

function TDeclaration_TypeRecord.Fields: TDeclarationArray;
begin
  Result := Items.GetItemsOfClass(TDeclaration_Field);
end;

function TDeclaration_Var.GetVarType: TDeclaration;
begin
  Result := Items.GetFirstItemOfClass(TDeclaration_VarType);
end;

function TDeclaration_Var.GetVarTypeString: String;
begin
  if FVarTypeString.IsNull then
    FVarTypeString.Value := Items.GetTextOfClassNoCommentsSingleLine(TDeclaration_VarType, ': ');

  Result := FVarTypeString.Value;
end;

function TDeclaration_Var.GetVarDefaultString: String;
begin
  if FVarDefaultString.IsNull then
    case DefToken of
      tokAssign: FVarDefaultString.Value := Items.GetTextOfClassNoCommentsSingleLine(TDeclaration_VarDefault, ' := ');
      tokEqual:  FVarDefaultString.Value := Items.GetTextOfClassNoCommentsSingleLine(TDeclaration_VarDefault, ' = ');
    end;

  Result := FVarDefaultString.Value;
end;

function TDeclaration_EnumElementName.GetName: string;
begin
  if FName.IsNull then
    FName.Value := Text;

  Result := FName.Value;
end;

function TDeclaration_TypeEnum.Elements: TDeclarationArray;
begin
  Result := Items.GetItemsOfClass(TDeclaration_EnumElement);
end;

function TDeclaration_Method.ResultType: TDeclaration;
var
  Decl: TDeclaration;
begin
  Decl := Items.GetFirstItemOfClass(TDeclaration_MethodResult);
  if (Decl <> nil) then
    Result := Decl.Items.GetFirstItemOfClass(TDeclaration_VarType)
  else
    Result := nil;
end;

function TDeclaration_Method.Dump: String;
begin
  Result := inherited;

  //case MethodType of
  //  mtProcedure:       Result := Result + ' [procedure]';
  //  mtFunction:        Result := Result + ' [function]';
  //  mtObjectProcedure: Result := Result + ' [procedure of object]';
  //  mtObjectFunction:  Result := Result + ' [function of object]';
  //  mtOperator:        Result := Result + ' [operator]';
  //end;

  if (Directives <> []) then
    Result := Result + ' ' + TokenNames(Directives);
end;

function TDeclaration_Method.GetParamString: String;
begin
  if FParamString.IsNull then
    FParamString.Value := FItems.GetTextOfClassNoCommentsSingleLine(TDeclaration_ParamList);

  Result := FParamString.Value;
end;

function TDeclaration_Method.GetResultString: String;
begin
  if FResultString.IsNull then
    FResultString.Value := FItems.GetTextOfClassNoCommentsSingleLine(TDeclaration_MethodResult, ': ');

  Result := FResultString.Value;
end;

function TDeclaration_Method.GetHeaderString: String;
var
  Builder: TSimbaStringBuilder;
begin
  if FHeaderString.IsNull then
  begin
    if isFunction       then Builder.Append('function')  else
    if isProcedure      then Builder.Append('procedure') else
    if isOperatorMethod then Builder.Append('operator');

    Builder.Append(' ');
    if isObjectMethod then
      Builder.Append(ObjectName + '.' + Name)
    else
      Builder.Append(Name);
    Builder.Append(ParamString);
    Builder.Append(ResultString);

    FHeaderString.Value := Builder.Str;
  end;

  Result := FHeaderString.Value;
end;

function TDeclaration_Parameter.VarTypeString: String;
begin
  Result := FItems.GetTextOfClassNoCommentsSingleLine(TDeclaration_VarType);
end;

function TDeclaration_Parameter.DefaultValueString: String;
begin
  Result := FItems.GetTextOfClassNoCommentsSingleLine(TDeclaration_VarDefault);
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

{$IFDEF PARSER_LEAK_CHECKS}
class constructor TLeakChecker.Create;
begin
  FList := TList.Create();
end;

class destructor TLeakChecker.Destroy;
begin
  if (SimbaProcessType = ESimbaProcessType.IDE) then
  begin
    DebugLn('TLeakChecker.PrintLeaks: ' + IntToStr(FList.Count) + ' (should be zero)');
    DebugLn('Press enter to close...');

    ReadLn;
  end;

  FList.Free();
end;

constructor TLeakChecker.Create;
begin
  inherited Create();

  FList.Add(Self);
end;

destructor TLeakChecker.Destroy;
begin
  FList.Remove(Self);

  inherited Destroy();
end;
{$ENDIF}

function TDeclarationList.GetByClassAndName(AClass: TDeclarationClass; AName: String; SubSearch: Boolean): TDeclarationArray;
var
  Size: Integer;

  procedure DoSearch(const Item: TDeclaration);
  var
    I: Integer;
  begin
    if (Item is AClass) and (Item.IsName(AName)) then
    begin
      if (Size = Length(Result)) then
        SetLength(Result, 4 + Length(Result) * 2);
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

function TDeclarationList.GetByName(AName: String; SubSearch: Boolean): TDeclarationArray;
var
  Size: Integer;

  procedure DoSearch(const Item: TDeclaration);
  var
    I: Integer;
  begin
    if Item.IsName(AName) then
    begin
      if (Size = Length(Result)) then
        SetLength(Result, 4 + Length(Result) * 2);
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

function TDeclarationList.GetItemsOfClass(AClass: TDeclarationClass; SubSearch: Boolean = False): TDeclarationArray;
var
  Size: Integer;

  procedure DoSearch(const Item: TDeclaration);
  var
    I: Integer;
  begin
    if (Item is AClass) then
    begin
      if (Size = Length(Result)) then
        SetLength(Result, 8 + Length(Result) * 2);
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

  for i := 0 to Count - 1 do
    if SearchItem(AClass, SubSearch, FItems[i], Result) then
      Exit;
end;

function TDeclarationList.GetItemInPosition(Position: Integer; CheckEnd: Boolean): TDeclaration;

  procedure Search(Declaration: TDeclaration; var Result: TDeclaration);
  var
    I: Integer;
  begin
    if (Position >= Declaration.StartPos) and ((not CheckEnd) or (Position <= Declaration.EndPos)) then
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
    if (Position >= FItems[I].StartPos) and ((not CheckEnd) or (Position <= FItems[I].EndPos)) then
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

function TDeclarationList.GetTextOfClassNoCommentsSingleLine(AClass: TDeclarationClass; Prefix: String): String;
var
  Decl: TDeclaration;
begin
  Decl := GetFirstItemOfClass(AClass);

  if (Decl <> nil) then
  begin
    Result := Decl.GetTextNoCommentsSingleLine();
    if (Result <> '') then
      Result := Prefix + Result;
  end else
    Result := '';
end;

function TDeclarationList.GetItem(Index: Integer): TDeclaration;
begin
  Result := FItems[Index];
end;

function TDeclarationList.ToArray: TDeclarationArray;
begin
  SetLength(Result, FCount);
  if (FCount > 0) then
    Move(FItems[0], Result[0], FCount * SizeOf(TDeclaration));
end;

procedure TDeclarationList.Add(Decl: TDeclaration);
begin
  if (FCount >= Length(FItems)) then
    SetLength(FItems, 4 + (Length(FItems) * 2));

  FItems[FCount] := Decl;
  Inc(FCount);
end;

procedure TDeclarationList.Extend(Decls: TDeclarationArray);
begin
  if (Length(Decls) = 0) then
    Exit;

  if (FCount + Length(Decls) >= Length(FItems)) then
    SetLength(FItems, 4 + Length(Decls) + (Length(FItems) * 2));
  Move(Decls[0], FItems[FCount], Length(Decls) * SizeOf(TDeclaration));
  Inc(FCount, Length(Decls));
end;

procedure TDeclarationList.Clear(FreeDecls: Boolean);
var
  I: Integer;
begin
  if FreeDecls then
    for I := 0 to FCount - 1 do
      FItems[I].Free();

  FCount := 0;
end;

function TDeclaration.GetName: string;
begin
  Result := FName.Value;
end;

procedure TDeclaration.SetName(Value: String);
begin
  FName.Value := Value;
end;

function TDeclaration.Dump: String;
begin
  if (Name = '') then
    Result := ClassName
  else
    Result := ClassName + ' (' + Name + ')';
end;

procedure TDeclaration.SetFlags(const Index: Integer; const AValue: Boolean);
begin
  if AValue then
    FFlags := FFlags or (1 shl Index)
  else
    FFlags := FFlags and not (1 shl Index);
end;

function TDeclaration.GetFlags(const Index: Integer): Boolean;
begin
  Result := (FFlags and (1 shl Index)) <> 0;
end;

function TDeclaration.GetText: String;
begin
  if FText.IsNull then
    FText.Value := FLexer.CopyDoc(FStartPos, FEndPos);

  Result := FText.Value;
end;

function TDeclaration.GetTextNoComments: String;

  function Filter(const Text: String): String;
  var
    Builder: TSimbaStringBuilder;
    Lexer: TmwPasLex;
  begin
    Lexer := TmwPasLex.Create(Text);
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
    FTextNoComments.Value := Filter(FLexer.CopyDoc(FStartPos, FEndPos));

  Result := FTextNoComments.Value;
end;

function TDeclaration.GetTextNoCommentsSingleLine: String;

  function Filter(const Text: String): String;
  var
    Builder: TSimbaStringBuilder;
    Lexer: TmwPasLex;
  begin
    Lexer := TmwPasLex.Create(Text);
    Lexer.Next();
    while (Lexer.TokenID <> tokNull) do
    begin
      case Lexer.TokenID of
        tokCRLF, tokCRLFCo: Builder.Append(' ');
        tokSlashesComment, tokAnsiComment, tokBorComment: { nothing };
        else
          Builder.Append(Lexer.Token);
      end;

      Lexer.Next();
    end;
    Lexer.Free();

    Result := Builder.Str;

    while Result.Contains('  ') do
      Result := Result.Replace('  ', ' ');
  end;

begin
  if FTextNoCommentsSingleLine.IsNull then
    FTextNoCommentsSingleLine.Value := Filter(FLexer.CopyDoc(FStartPos, FEndPos));

  Result := FTextNoCommentsSingleLine.Value;
end;

function TDeclaration.IsName(const Value: String): Boolean;
begin
  Result := SameText(Name, Value);
end;

function TDeclaration.GetOwnerByClass(AClass: TDeclarationClass): TDeclaration;
var
  Decl: TDeclaration;
begin
  Decl := FOwner;

  while (Decl <> nil) do
  begin
    if (Decl is AClass) then
    begin
      Result := Decl;
      Exit;
    end;

    Decl := Decl.Owner;
  end;

  Result := nil;
end;

constructor TDeclaration.Create(AParser: TCodeParser; AOwner: TDeclaration; AStart: Integer; AEnd: Integer);
begin
  inherited Create();

  FParser := AParser;
  if (FParser <> nil) then
  begin
    FLexer := FParser.Lexer;
    FLine := FLexer.LineNumber;
    FParser.FManagedItems.Add(Self);
  end;

  FOwner := AOwner;
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

function TCodeParser.InDeclaration(AClass: TDeclarationClass): Boolean;
begin
  if (AClass = nil) then
    Result := (FStack.Count = 1)
  else
    Result := (FStack.Count > 1) and (FStack.Top is AClass);
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

function TCodeParser.PushStack(AClass: TDeclarationClass): TDeclaration;
begin
  Result := AClass.Create(Self, FStack.Top, FLexer.TokenPos, FLexer.TokenPos);

  FStack.Top.Items.Add(Result);
  FStack.Push(Result);
end;

procedure TCodeParser.PopStack();
begin
  if (Lexer.TokenID = tok_DONE) then
    FStack.Pop()
  else
    FStack.Pop().fEndPos := fLastNoJunkPos;
end;

procedure TCodeParser.FindLocals;

  procedure CheckMethod(Decl: TDeclaration);
  begin
    if (Decl = nil) then
      Exit;

    FLocals.Extend(Decl.Items.GetItemsOfClass(TDeclaration_Method));

    while (Decl is TDeclaration_Method) do
    begin
      FLocals.Extend(Decl.Items.GetItemsOfClass(TDeclaration_Var));
      FLocals.Extend(Decl.Items.GetItemsOfClass(TDeclaration_Type));
      FLocals.Extend(Decl.Items.GetItemsOfClass(TDeclaration_EnumElement));
      if (FItems.GetFirstItemOfClass(TDeclaration_ParamList) <> nil) then
        FLocals.Extend(FItems.GetFirstItemOfClass(TDeclaration_ParamList).Items.GetItemsOfClass(TDeclaration_Parameter, True));

      Decl := Decl.GetOwnerByClass(TDeclaration_Method);
    end;
  end;

  procedure CheckWith(Decl: TDeclaration);
  begin
    if (Decl = nil) then
      Exit;

    while (Decl is TDeclaration_WithStatement) do
    begin
      FLocals.Extend(Decl.Items.GetItemsOfClass(TDeclaration_WithVariable));

      Decl := Decl.GetOwnerByClass(TDeclaration_WithStatement);
    end;
  end;

var
  Decl: TDeclaration;
begin
  FLocals.Clear();

  if (CaretPos > -1) then
  begin
    Decl := FItems.GetItemInPosition(CaretPos);
    if (Decl = nil) then
      Decl := FItems.GetItemInPosition(CaretPos, False);
    if (Decl = nil) then
      Exit;

    if (Decl is TDeclaration_Method) then
      CheckMethod(Decl)
    else
      CheckMethod(Decl.GetOwnerByClass(TDeclaration_Method));

    if (Decl is TDeclaration_WithStatement) then
      CheckWith(Decl)
    else
      CheckWith(Decl.GetOwnerByClass(TDeclaration_WithStatement));
  end;
end;

procedure TCodeParser.FindGlobals;
var
  I: Integer;
  Decl: TDeclaration;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    Decl := FItems[I];
    if (Decl.Name = '') then
      Continue;

    if (Decl is TDeclaration_Method) and Decl.isObjectMethod then
    begin
      FTypeMethods.Add(TDeclaration_Method(Decl).ObjectName, Decl);

      Continue;
    end;

    FGlobals.Add(Decl);
  end;
end;

function TCodeParser.GetHash: String;
var
  Builder: TSimbaStringBuilder;
  I: Integer;
begin
  if (FHash = '') then
  begin
    with Lexer.SaveDefines() do
      Builder.Append(Defines + IntToStr(Stack));
    for I := 0 to fLexers.Count - 1 do
      Builder.Append(fLexers[i].FileName + IntToStr(fLexers[i].FileAge));

    FHash := Builder.Str;
  end;

  Result := FHash;
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

procedure TCodeParser.SetCaretPos(const Value: Integer);
begin
  if (fLexer <> nil) then
    fLexer.CaretPos := Value;
end;

procedure TCodeParser.SetMaxPos(const Value: Integer);
begin
  if (fLexer <> nil) then
    fLexer.MaxPos := Value;
end;

constructor TCodeParser.Create;
begin
  inherited Create();

  FManagedItems := TDeclarationList.Create();
  FLocals := TDeclarationList.Create();
  FGlobals := TDeclarationList.Create();
  FTypeMethods := TDeclarationMap.Create();

  FRoot := TDeclaration_Root.Create();
  FStack := TDeclarationStack.Create();
  FStack.Push(FRoot);

  FItems := FRoot.Items;
end;

destructor TCodeParser.Destroy;
begin
  Reset();

  FLocals.Free();
  FGlobals.Free();
  FTypeMethods.Free();

  FManagedItems.Free();
  FStack.Free();
  FRoot.Free();

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
begin
  if Sender.IsJunk or (Assigned(FOnHandleLibrary) and FOnHandleLibrary(Sender)) then
    Exit;

  DebugLn('Library "' + Sender.DirectiveParamAsFileName + '" not handled');
end;

procedure TCodeParser.OnIncludeDirect(Sender: TmwBasePasLex);
var
  FileName: String;
begin
  if Sender.IsJunk or (Assigned(FOnHandleInclude) and FOnHandleInclude(Sender)) then
    Exit;

  FileName := Sender.DirectiveParamAsFileName;
  if Assigned(FOnFindInclude) and FOnFindInclude(Sender, FileName) then
  begin
    if (Sender.TokenID = tokIncludeOnceDirect) and HasFile(FileName) then
      Exit;

    PushLexer(TmwPasLex.CreateFromFile(FileName));
  end else
    DebugLn('Include "' + Sender.DirectiveParamAsFileName + '" not handled');
end;

procedure TCodeParser.CompoundStatement;
begin
  if (not InDeclarations([nil, TDeclaration_Method, TDeclaration_WithStatement])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TDeclaration_CompoundStatement);
  inherited;
  PopStack();
end;

procedure TCodeParser.WithStatement;
begin
  if (not InDeclarations([TDeclaration_Method, TDeclaration_CompoundStatement])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TDeclaration_WithStatement);
  inherited;
  PopStack();
end;

procedure TCodeParser.Variable;
begin
  if (not InDeclaration(TDeclaration_WithStatement)) then
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
  if (not InDeclarations([TDeclaration_VarStub, TDeclaration_TypeArray, TDeclaration_MethodResult])) then
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
  if (not InDeclarations([TDeclaration_TypeStub, TDeclaration_VarType])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TDeclaration_TypeMethod);
  inherited;
  PopStack();
end;

procedure TCodeParser.ProceduralDirective;
begin
  if InDeclaration(TDeclaration_Method) then
    Include(TDeclaration_Method(FStack.Top).Directives, Lexer.ExID);
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
  Decl := PushStack(TDeclaration_TypeStub);
  inherited;
  PopStack();

  if (Decl.Items.Count > 0) then
  begin
    NewDecl := Decl.Items[0];
    NewDecl.isType := True;
    NewDecl.Name := TDeclaration_TypeStub(Decl).TempName;

    FStack.Top.Items.Add(NewDecl);
    if (NewDecl is TDeclaration_TypeEnum) then
      for NewDecl in TDeclaration_TypeEnum(NewDecl).Elements do
        FStack.Top.Items.Add(NewDecl);
  end else
    OnErrorMessage(Lexer, 'Invalid type declaration');
end;

procedure TCodeParser.TypeName;
begin
  if InDeclaration(TDeclaration_TypeStub) then
    TDeclaration_TypeStub(FStack.Top).TempName := Lexer.Token;

  inherited;
end;

procedure TCodeParser.ExplicitType;
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
  VarDecl, VarType, VarDefault: TDeclaration;
  I: Integer;
begin
  Decl := PushStack(TDeclaration_VarStub);
  inherited;
  PopStack();

  VarType := Decl.Items.GetFirstItemOfClass(TDeclaration_VarType);
  VarDefault := Decl.Items.GetFirstItemOfClass(TDeclaration_VarDefault);

  with TDeclaration_VarStub(Decl) do
    for I := 0 to NameCount - 1 do
    begin
      VarDecl := PushStack(TDeclaration_Var);
      VarDecl.isVar := True;
      VarDecl.Name := Names[I].Name;
      if (VarType <> nil) then
        VarDecl.Items.Add(VarType);
      if (VarDefault <> nil) then
        VarDecl.Items.Add(VarDefault);

      TDeclaration_Var(VarDecl).DefToken := TDeclaration_VarStub(Decl).DefToken;
      TDeclaration_Var(VarDecl).FStartPos := Names[I].StartPos;
      TDeclaration_Var(VarDecl).FEndPos := Names[I].EndPos;

      FStack.Pop();
    end;
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
  VarDecl, VarType, VarDefault: TDeclaration;
  I: Integer;
begin
  Decl := PushStack(TDeclaration_VarStub);
  inherited;
  PopStack();

  VarType := Decl.Items.GetFirstItemOfClass(TDeclaration_VarType);
  VarDefault := Decl.Items.GetFirstItemOfClass(TDeclaration_VarDefault);

  with TDeclaration_VarStub(Decl) do
    for I := 0 to NameCount - 1 do
    begin
      VarDecl := PushStack(TDeclaration_Const);
      VarDecl.isConst := True;
      VarDecl.Name := Names[I].Name;
      if (VarType <> nil) then
        VarDecl.Items.Add(VarType);
      if (VarDefault <> nil) then
        VarDecl.Items.Add(VarDefault);

      TDeclaration_Var(VarDecl).DefToken := TDeclaration_VarStub(Decl).DefToken;
      TDeclaration_Var(VarDecl).FStartPos := Names[I].StartPos;
      TDeclaration_Var(VarDecl).FEndPos := Names[I].EndPos;
      FStack.Pop();
      //PopStack();
    end;

  {
  for TempName in TDeclaration_VarStub(Decl).TempNames do
  begin
    VarDecl := PushStack(TDeclaration_Const);
    VarDecl.Name := TempName;
    if (VarType <> nil) then
      VarDecl.Items.Add(VarType);
    if (VarDefault <> nil) then
      VarDecl.Items.Add(VarDefault);

    TDeclaration_Var(VarDecl).DefToken := TDeclaration_VarStub(Decl).DefToken;

    PopStack();
  end;
  }
end;

procedure TCodeParser.ConstantName;
begin
  VarName();
end;

procedure TCodeParser.ProcedureDeclarationSection;
var
  Decl: TDeclaration;
begin
  Decl := PushStack(TDeclaration_Method);
  case Lexer.TokenID of
    tokFunction:  Decl.isFunction := True;
    tokProcedure: Decl.isProcedure := True;
    tokOperator:  Decl.isOperatorMethod := True;
  end;

  inherited;
  PopStack();
end;

procedure TCodeParser.FunctionProcedureName;
begin
  if InDeclaration(TDeclaration_Method) then
    TDeclaration_Method(FStack.Top).Name := Lexer.Token;

  inherited;
end;

procedure TCodeParser.ObjectNameOfMethod;
var
  Decl: TDeclaration_Method;
begin
  if InDeclaration(TDeclaration_Method) then
  begin
    Decl := TDeclaration_Method(FStack.Top);
    Decl.ObjectName := Lexer.Token;
    Decl.isObjectMethod := True;
  end;

  inherited;
end;

procedure TCodeParser.ReturnType;
begin
  if (not InDeclaration(TDeclaration_Method)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TDeclaration_MethodResult);
  TypeKind();
  PopStack();
end;

procedure TCodeParser.ConstRefParameter;
begin
  if InDeclaration(TDeclaration_ParamStub) then
   TDeclaration_ParamStub(FStack.Top).ParamType := tokConstRef;

  inherited;
end;

procedure TCodeParser.ConstParameter;
begin
  if InDeclaration(TDeclaration_ParamStub) then
   TDeclaration_ParamStub(FStack.Top).ParamType := tokConst;

  inherited;
end;

procedure TCodeParser.OutParameter;
begin
  if InDeclaration(TDeclaration_ParamStub) then
    TDeclaration_ParamStub(FStack.Top).ParamType := tokOut;

  inherited;
end;

procedure TCodeParser.NormalParameter;
begin
  if InDeclaration(TDeclaration_ParamStub) then
    TDeclaration_ParamStub(FStack.Top).ParamType := tokUnknown;

  inherited;
end;

procedure TCodeParser.VarParameter;
begin
  if InDeclaration(TDeclaration_ParamStub) then
    TDeclaration_ParamStub(FStack.Top).ParamType := tokVar;

  inherited;
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

procedure TCodeParser.FormalParameterSection;
var
  Decl, ParamDecl, VarType, VarDef: TDeclaration;
  I: Integer;
begin
  Decl := PushStack(TDeclaration_ParamStub);
  inherited;
  PopStack();

  PushStack(TDeclaration_ParamGroup);

  VarType := Decl.Items.GetFirstItemOfClass(TDeclaration_VarType);
  VarDef := Decl.Items.GetFirstItemOfClass(TDeclaration_VarDefault);

  with TDeclaration_VarStub(Decl) do
    for I := 0 to NameCount - 1 do
    begin
      ParamDecl := PushStack(TDeclaration_Parameter);
      ParamDecl.Name := Names[I].Name;
      TDeclaration_Parameter(ParamDecl).ParamType := TDeclaration_ParamStub(Decl).ParamType;
      if (VarType <> nil) then
        ParamDecl.Items.Add(VarType);
      if (VarDef <> nil) then
        ParamDecl.Items.Add(VarDef);

      TDeclaration_Parameter(ParamDecl).DefToken := TDeclaration_VarStub(Decl).DefToken;
      TDeclaration_Parameter(ParamDecl).FStartPos := Names[I].StartPos;
      TDeclaration_Parameter(ParamDecl).FEndPos := Names[I].EndPos;

      PopStack();
    end;

  PopStack();
end;

procedure TCodeParser.FormalParameterList;
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

procedure TCodeParser.NativeType;
begin
  PushStack(TDeclaration_TypeNativeMethod);
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

procedure TCodeParser.ClassField;
var
  Decl, FieldDecl, VarType: TDeclaration;
  I: Integer;
begin
  if (not InDeclarations([TDeclaration_TypeRecord, TDeclaration_TypeUnion])) then
  begin
    inherited;
    Exit;
  end;

  Decl := PushStack(TDeclaration_VarStub);
  inherited;
  PopStack();

  VarType := Decl.Items.GetFirstItemOfClass(TDeclaration_VarType);

  with TDeclaration_VarStub(Decl) do
    for I := 0 to NameCount - 1 do
    begin
      FieldDecl := PushStack(TDeclaration_Field);
      FieldDecl.Name := Names[I].Name;
      if (VarType <> nil) then
        FieldDecl.Items.Add(VarType);

      PopStack();
    end;
end;

procedure TCodeParser.FieldName;
begin
  VarName();
end;

procedure TCodeParser.AncestorId;
begin
  if (not InDeclarations([TDeclaration_TypeRecord, TDeclaration_TypeNativeMethod])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TDeclaration_ParentType);
  PushStack(TDeclaration_VarType);
  PushStack(TDeclaration_Identifier);
  inherited;
  PopStack();
  PopStack();
  PopStack();
end;

procedure TCodeParser.SetType;
begin
  PushStack(TDeclaration_TypeSet);
  inherited;
  PopStack();
end;

procedure TCodeParser.OrdinalType;
begin
  if (not InDeclarations([TDeclaration_TypeSet, TDeclaration_TypeArray, TDeclaration_TypeStub])) then
  begin
    inherited;
    Exit;
  end;

  if InDeclaration(TDeclaration_TypeStub) then
    PushStack(TDeclaration_TypeRange);

  PushStack(TDeclaration_OrdinalType);
  inherited;
  PopStack();

  if InDeclaration(TDeclaration_TypeRange) then
    PopStack();
end;

procedure TCodeParser.EnumeratedType;
begin
  if Lexer.IsDefined('!SCOPEDENUMS') then
    PushStack(TDeclaration_TypeEnumScoped)
  else
    PushStack(TDeclaration_TypeEnum);
  inherited;
  PopStack();
end;

procedure TCodeParser.EnumeratedScopedType;
begin
  PushStack(TDeclaration_TypeEnumScoped);
  inherited;
  PopStack();
end;

procedure TCodeParser.EnumeratedTypeItem;
var
  Decl: TDeclaration;
begin
  if (not InDeclarations([TDeclaration_TypeEnum, TDeclaration_TypeEnumScoped])) then
  begin
    inherited;
    Exit;
  end;

  Decl := PushStack(TDeclaration_EnumElement);
  inherited;
  PopStack();
end;

procedure TCodeParser.QualifiedIdentifier;
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

  FHash := '';

  FManagedItems.Clear(True);

  FGlobals.Clear();
  FLocals.Clear();
  FTypeMethods.Clear();

  FRoot.Items.Clear();
  FStack.Clear();
  FStack.Push(FRoot);
end;

procedure TCodeParser.Run;
begin
  inherited Run();

  FindGlobals();
  FindLocals();
end;

function TCodeParser.DebugTree: String;
var
  Builder: TAnsiStringBuilder;
  Depth: Integer = 1;

  procedure Dump(Decl: TDeclaration);
  var
    I: Integer;
  begin
    if (Decl is TDeclaration_Stub) then
      Exit;
    Builder.AppendLine(StringOfChar('-', Depth*2) + ' ' + Decl.Dump());

    Inc(Depth);
    for I := 0 to Decl.Items.Count - 1 do
      Dump(Decl.Items[I]);
    Dec(Depth);

    if (Decl.Items.Count > 0) then
      Builder.AppendLine(StringOfChar('-', Depth*2) + ' ' + Decl.Dump());
  end;

var
  I, Count: Integer;
begin
  Builder := TAnsiStringBuilder.Create();

  Count := 0;
  for I := 0 to Items.Count - 1 do
  begin
    if (Items[I] is TDeclaration_Stub) then
      Continue;

    Builder.AppendLine(IntToStr(Count) + ')');
    Dump(Items[I]);
    Inc(Count);
  end;

  Result := Builder.ToString().Trim();

  Builder.Free();
end;

function TCodeParser.DebugGlobals: String;
var
  Builder: TAnsiStringBuilder;
  List: TDeclarationList;
  Decl: TDeclaration;
begin
  Builder := TAnsiStringBuilder.Create();

  //for List in FGlobalMap.Values do
  //  for Decl in List do
  //    Builder.AppendLine(Decl.ClassName + ' -> ' + Decl.Name);
  Result := Builder.ToString().Trim();

  Builder.Free();
end;

end.
