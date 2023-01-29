{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_codetools_parser_new;

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

  TDeclarationList = class(specialize TList<TDeclaration>)
  public
    function GetByClassAndName(AClass: TDeclarationClass; AName: String; SubSearch: Boolean = False): TDeclarationArray;

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

  {$IFDEF PARSER_LEAK_CHECKS}
  TLeakChecker = class(TObject)
  public
  class var
    Creates: Integer;
    Destroys: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;
  {$ENDIF}

  TDeclaration = class({$IFDEF PARSER_LEAK_CHECKS}TLeakChecker{$ELSE}TObject{$ENDIF})
  protected
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

    function GetText: String;
    function GetTextNoComments: String;
    function GetTextNoCommentsSingleLine: String;

    function GetName: string; virtual;
  public
    function Dump: String; virtual;

    function HasOwnerClass(AClass: TDeclarationClass; out Declaration: TDeclaration; Recursive: Boolean = False): Boolean;
    function GetOwnersOfClass(AClass: TDeclarationClass): TDeclarationArray;

    function IsName(const Value: String): Boolean; inline;

    property Lexer: TmwPasLex read FLexer;
    property Owner: TDeclaration read FOwner write FOwner;

    property StartPos: Integer read FStartPos write FStartPos;
    property EndPos: Integer read FEndPos write FEndPos;
    property Items: TDeclarationList read FItems;
    property Name: String read GetName write FName;
    property Line: Integer read FLine;

    property Text: String read GetText;
    property TextNoComments: String read GetTextNoComments;
    property TextNoCommentsSingleLine: String read GetTextNoCommentsSingleLine;

    constructor Create(ALexer: TmwPasLex; AOwner: TDeclaration; AStart: Integer; AEnd: Integer = -1); overload;
    destructor Destroy; override;
  end;

  TciCompoundStatement = class(TDeclaration);
  TciWithStatement = class(TDeclaration);
  TciSimpleStatement = class(TDeclaration);
  TciVariable = class(TDeclaration);
  TciExpression = class(TDeclaration);

  TDeclaration_Root = class(TDeclaration)
  public
    constructor Create; reintroduce;
  end;

  TDeclaration_Stub = class(TDeclaration);
  TDeclaration_TypeStub = class(TDeclaration_Stub)
  public
    TempName: String;
  end;
  TDeclaration_VarStub = class(TDeclaration_Stub)
  public
    TempNames: TStringArray;
  end;
  TDeclaration_ParamStub = class(TDeclaration_VarStub)
  public
    ParamType: TptTokenKind;
  end;

  TDeclaration_ParentType = class(TDeclaration);
  TDeclaration_Identifier = class(TDeclaration);
  TDeclaration_OrdinalType = class(TDeclaration);

  TDeclaration_TypeRecord = class(TDeclaration)
  public
    function Fields: TDeclarationArray;
  end;

  TDeclaration_TypeUnion = class(TDeclaration);
  TDeclaration_TypeArray = class(TDeclaration);
  TDeclaration_TypePointer = class(TDeclaration);
  TDeclaration_TypeCopy = class(TDeclaration);
  TDeclaration_TypeAlias = class(TDeclaration);
  TDeclaration_TypeSet = class(TDeclaration);
  TDeclaration_TypeRange = class(TDeclaration);
  TDeclaration_TypeNativeMethod = class(TDeclaration);

  TDeclaration_EnumElement = class(TDeclaration);
  TDeclaration_EnumElementName = class(TDeclaration)
  protected
    function GetName: string; override;
  end;

  TDeclaration_TypeEnum = class(TDeclaration)
  public
    function Elements: TDeclarationArray;
  end;

  TDeclaration_TypeEnumScoped = class(TDeclaration);

  TDeclaration_VarType = class(TDeclaration);
  TDeclaration_VarDefault = class(TDeclaration);
  TDeclaration_Var = class(TDeclaration)
  public
    function VarType: TDeclaration;
  end;

  TDeclaration_Const = class(TDeclaration_Var);
  TDeclaration_Field = class(TDeclaration_Var);

  EMethodType = (mtProcedure, mtFunction, mtObjectProcedure, mtObjectFunction, mtOperator);
  EMethodDirectives = TptTokenSet;

  TDeclaration_Method = class(TDeclaration)
  public
    ObjectName: String;
    MethodType: EMethodType;
    Directives: EMethodDirectives;

    function ResultType: TDeclaration;
    function Dump: String; override;
  end;
  TDeclaration_TypeMethod = class(TDeclaration_Method);

  TDeclaration_MethodResult = class(TDeclaration);
  TDeclaration_Parameter = class(TDeclaration)
  public
    ParamType: TptTokenKind;

    function Dump: String; override;
  end;

  TOnFindInclude = function(Sender: TObject; var FileName: string): Boolean of object;
  TOnInclude = procedure(Sender: TObject; FileName: String; var Handled: Boolean) of object;

  TOnFindLibrary = function(Sender: TObject; var FileName: String): Boolean of object;
  TOnLoadLibrary = procedure(Sender: TObject; FileName: String; var Contents: String) of object;
  TOnLibrary = procedure(Sender: TObject; FileName: String; var Handled: Boolean) of object;

  TCodeParser = class(TmwSimplePasPar)
  protected
    FRoot: TDeclaration;
    FItems: TDeclarationList;
    FStack: TDeclarationStack;
    FOnFindInclude: TOnFindInclude;
    FOnInclude: TOnInclude;
    FOnFindLibrary: TOnFindInclude;
    FOnLoadLibrary: TOnLoadLibrary;
    FOnLibrary: TOnLibrary;
    FGlobals: TDeclarationMap;
    FGlobalTypeMethods: TDeclarationMap;

    function GetGlobalTypeMethods(const AType: String): TDeclarationArray;
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
    procedure SimpleStatement; override;                                        //Begin-End + With
    procedure Variable; override;                                               //With

    procedure TypeKind; override;                                               //Var + Const + Array + Record
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
    property Globals: TDeclarationMap read FGlobals;
    property GlobalTypeMethods[AType: String]: TDeclarationArray read GetGlobalTypeMethods;

    property OnFindInclude: TOnFindInclude read FOnFindInclude write FOnFindInclude;
    property OnInclude: TOnInclude read FOnInclude write FOnInclude;

    property OnFindLibrary: TOnFindInclude read FOnFindLibrary write FOnFindLibrary;
    property OnLoadLibrary: TOnLoadLibrary read FOnLoadLibrary write FOnLoadLibrary;
    property OnLibrary: TOnLibrary read FOnLibrary write FOnLibrary;

    property CaretPos: Integer read GetCaretPos write SetCaretPos;
    property MaxPos: Integer read GetMaxPos write SetMaxPos;

    procedure Reset; override;
    procedure Run; override;

    function DebugTree: String;
    function DebugGlobals: String;

    procedure Assign(From: TObject); override;

    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  simba.mufasatypes;

function TDeclaration_TypeRecord.Fields: TDeclarationArray;
begin
  Result := Items.GetItemsOfClass(TDeclaration_Field);
end;

function TDeclaration_Var.VarType: TDeclaration;
begin
  Result := Items.GetFirstItemOfClass(TDeclaration_VarType);
end;

function TDeclaration_EnumElementName.GetName: string;
begin
  if (FName = #0) then
    FName := Text;

  Result := FName;
end;

function TDeclaration_TypeEnum.Elements: TDeclarationArray;
begin
  Result := Items.GetItemsOfClass(TDeclaration_EnumElementName, True);
end;

function TDeclaration_Method.ResultType: TDeclaration;
begin
  if (MethodType in [mtFunction, mtObjectFunction]) then
  begin
    Result := Items.GetFirstItemOfClass(TDeclaration_MethodResult);
    if (Result <> nil) then
      Result := Result.Items.GetFirstItemOfClass(TDeclaration_VarType);
  end else
    Result := nil;
end;

function TDeclaration_Method.Dump: String;
begin
  Result := inherited;

  case MethodType of
    mtProcedure:       Result := Result + ' [procedure]';
    mtFunction:        Result := Result + ' [function]';
    mtObjectProcedure: Result := Result + ' [procedure of object]';
    mtObjectFunction:  Result := Result + ' [function of object]';
    mtOperator:        Result := Result + ' [operator]';
  end;

  if (Directives <> []) then
    Result := Result + ' ' + TokenNames(Directives);
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

constructor TLeakChecker.Create;
begin
  inherited Create();

  Inc(Creates);
end;

destructor TLeakChecker.Destroy;
begin
  Inc(Destroys);

  inherited Destroy();
end;

constructor TDeclaration_Root.Create;
begin
  inherited Create();

  FItems := TDeclarationList.Create();
end;

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
        SetLength(Result, Length(Result) * 2);
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
  SetLength(Result, 16);

  Size := 0;
  for I := 0 to FLength - 1 do
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
        SetLength(Result, Length(Result) * 2);
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
  SetLength(Result, 16);

  Size := 0;
  for I := 0 to FLength - 1 do
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
    Result := ''
  else
    Result := FName;
end;

function TDeclaration.Dump: String;
begin
  if (Name = '') then
    Result := ClassName
  else
    Result := ClassName + ' (' + Name + ')';
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

  FItems := TDeclarationList.Create();
end;

destructor TDeclaration.Destroy;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    if (Items[I].Owner = Self) then
      Items[I].Free();
  FItems.Free();

  inherited Destroy();
end;

function TCodeParser.InDeclaration(AClass: TDeclarationClass): Boolean;
begin
  if (AClass = nil) then
    Result := (FStack.Count = 1)
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

function TCodeParser.PushStack(AClass: TDeclarationClass): TDeclaration;
begin
  Result := AClass.Create(FLexer, FStack.Peek(), FLexer.TokenPos);

  FStack.Peek().Items.Add(Result);
  FStack.Push(Result);
end;

procedure TCodeParser.PopStack();
begin
  FStack.Pop().EndPos := fLastNoJunkPos;
end;

function TCodeParser.GetGlobalTypeMethods(const AType: String): TDeclarationArray;
begin
  Result := FGlobalTypeMethods.GetAll(AType);
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

  FRoot := TDeclaration_Root.Create();
  FItems := FRoot.Items;
  FStack := TDeclarationStack.Create();
  FStack.Push(FRoot);
  FGlobals := TDeclarationMap.Create();
  FGlobalTypeMethods := TDeclarationMap.Create();
end;

destructor TCodeParser.Destroy;
begin
  FRoot.Free();
  FStack.Free();
  FGlobals.Free();
  FGlobalTypeMethods.Free();

  {$IFDEF PARSER_LEAK_CHECKS}
  DebugLn('Leakage: ' + IntToStr(TLeakChecker.Creates - TLeakChecker.Destroys) + ' (should be zero)');
  {$ENDIF}

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
  if (not InDeclarations([nil, TDeclaration_Method, TciWithStatement])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciCompoundStatement);
  inherited;
  PopStack();
end;

procedure TCodeParser.WithStatement;
begin
  if (not InDeclarations([TDeclaration_Method, TciCompoundStatement])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciWithStatement);
  inherited;
  PopStack();
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
  PopStack();
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
  PopStack();
end;

procedure TCodeParser.TypeKind;
begin
  if (not InDeclarations([TDeclaration_VarStub, TDeclaration_MethodResult])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TDeclaration_VarType);
  inherited;
  PopStack();
end;

procedure TCodeParser.Expression;
begin
  if (not InDeclarations([TDeclaration_VarStub])) then
  begin
    inherited;
    Exit;
  end;

  // var x := Integer(123) .. hacky but simple.
  if (Lexer.TokenID = tokIdentifier) then
  begin
    PushStack(TDeclaration_VarType);
    PushStack(TDeclaration_Identifier).Name := Lexer.Token;
    PopStack();
    PopStack();
  end;

  PushStack(TDeclaration_VarDefault);
  inherited;
  PopStack();
end;

procedure TCodeParser.ProceduralType;
begin
  if (not InDeclaration(TDeclaration_TypeStub)) then
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
    Include(TDeclaration_Method(FStack.Peek).Directives, Lexer.ExID);
  inherited;
end;

procedure TCodeParser.TypeAlias;
begin
  PushStack(TDeclaration_TypeAlias);
  inherited;
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
    NewDecl := Decl.Items.First;
    NewDecl.Name := TDeclaration_TypeStub(Decl).TempName;

    FStack.Peek.Items.Add(NewDecl);
  end else
    OnErrorMessage(Lexer, 'Invalid type declaration');
end;

procedure TCodeParser.TypeName;
begin
  if InDeclaration(TDeclaration_TypeStub) then
    TDeclaration_TypeStub(FStack.Peek()).TempName := Lexer.Token;

  inherited;
end;

procedure TCodeParser.ExplicitType;
begin
  PushStack(TDeclaration_TypeCopy);
  inherited;
  PopStack();
end;

procedure TCodeParser.PointerType;
begin
  PushStack(TDeclaration_TypePointer);
  inherited PointerType();
  PopStack();
end;

procedure TCodeParser.VarDeclaration;
var
  Decl: TDeclaration;
  TempName: String;
  VarDecl, VarType, VarDefault: TDeclaration;
begin
  Decl := PushStack(TDeclaration_VarStub);
  inherited;
  PopStack();

  VarType := Decl.Items.GetFirstItemOfClass(TDeclaration_VarType);
  VarDefault := Decl.Items.GetFirstItemOfClass(TDeclaration_VarDefault);

  for TempName in TDeclaration_VarStub(Decl).TempNames do
  begin
    VarDecl := PushStack(TDeclaration_Var);
    VarDecl.Name := TempName;
    if (VarType <> nil) then
      VarDecl.Items.Add(VarType);
    if (VarDefault <> nil) then
      VarDecl.Items.Add(VarDefault);

    PopStack();
  end;
end;

procedure TCodeParser.VarName;
begin
  if InDeclaration(TDeclaration_VarStub) then
    TDeclaration_VarStub(FStack.Peek).TempNames += [Lexer.Token];

  inherited;
end;

procedure TCodeParser.ConstantDeclaration;
var
  Decl: TDeclaration;
  TempName: String;
  VarDecl, VarType, VarDefault: TDeclaration;
begin
  Decl := PushStack(TDeclaration_VarStub);
  inherited;
  PopStack();

  VarType := Decl.Items.GetFirstItemOfClass(TDeclaration_VarType);
  VarDefault := Decl.Items.GetFirstItemOfClass(TDeclaration_VarDefault);

  for TempName in TDeclaration_VarStub(Decl).TempNames do
  begin
    VarDecl := PushStack(TDeclaration_Const);
    VarDecl.Name := TempName;
    if (VarType <> nil) then
      VarDecl.Items.Add(VarType);
    if (VarDefault <> nil) then
      VarDecl.Items.Add(VarDefault);

    PopStack();
  end;
end;

procedure TCodeParser.ConstantName;
begin
  VarName();
end;

procedure TCodeParser.ProcedureDeclarationSection;
var
  Decl: TDeclaration_Method;
begin
  Decl := TDeclaration_Method(PushStack(TDeclaration_Method));
  case Lexer.TokenID of
    tokFunction:  Decl.MethodType := mtFunction;
    tokProcedure: Decl.MethodType := mtProcedure;
    tokOperator:  Decl.MethodType := mtOperator;
  end;

  inherited;
  PopStack();
end;

procedure TCodeParser.FunctionProcedureName;
begin
  if InDeclaration(TDeclaration_Method) then
    TDeclaration_Method(FStack.Peek).Name := Lexer.Token;
  inherited;
end;

procedure TCodeParser.ObjectNameOfMethod;
var
  Decl: TDeclaration_Method;
begin
  if InDeclaration(TDeclaration_Method) then
  begin
    Decl := TDeclaration_Method(FStack.Peek);
    Decl.ObjectName := Lexer.Token;

    case Decl.MethodType of
      mtFunction:  Decl.MethodType := mtObjectFunction;
      mtProcedure: Decl.MethodType := mtObjectProcedure;
    end;
  end;
  inherited;
end;

procedure TCodeParser.ReturnType;
begin
  if (not InDeclarations([TDeclaration_Method])) then
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
   TDeclaration_ParamStub(FStack.Peek).ParamType := tokConstRef;

  inherited;
end;

procedure TCodeParser.ConstParameter;
begin
  if InDeclaration(TDeclaration_ParamStub) then
   TDeclaration_ParamStub(FStack.Peek).ParamType := tokConst;

  inherited;
end;

procedure TCodeParser.OutParameter;
begin
  if InDeclaration(TDeclaration_ParamStub) then
    TDeclaration_ParamStub(FStack.Peek).ParamType := tokOut;

  inherited;
end;

procedure TCodeParser.NormalParameter;
begin
  if InDeclaration(TDeclaration_ParamStub) then
    TDeclaration_ParamStub(FStack.Peek).ParamType := tokUnknown;

  inherited;
end;

procedure TCodeParser.VarParameter;
begin
  if InDeclaration(TDeclaration_ParamStub) then
    TDeclaration_ParamStub(FStack.Peek).ParamType := tokVar;

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
  Decl, ParamDecl, VarType: TDeclaration;
  TempName: String;
begin
  Decl := PushStack(TDeclaration_ParamStub);
  inherited;
  PopStack();

  VarType := Decl.Items.GetFirstItemOfClass(TDeclaration_VarType);
  for TempName in TDeclaration_VarStub(Decl).TempNames do
  begin
    ParamDecl := PushStack(TDeclaration_Parameter);
    ParamDecl.Name := TempName;
    TDeclaration_Parameter(ParamDecl).ParamType := TDeclaration_ParamStub(Decl).ParamType;
    if (VarType <> nil) then
      ParamDecl.Items.Add(VarType);

    PopStack();
  end;
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
  TempName: String;
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
  for TempName in TDeclaration_VarStub(Decl).TempNames do
  begin
    FieldDecl := PushStack(TDeclaration_Field);
    FieldDecl.Name := TempName;
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
  inherited;
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
begin
  if (not InDeclarations([TDeclaration_TypeEnum, TDeclaration_TypeEnumScoped])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TDeclaration_EnumElement);
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

  //FRoot.Items.Clear();
  //FStack.Clear();
  //FStack.Push(FRoot);
end;

procedure TCodeParser.Run;
var
  Decl: TDeclaration;
  I: Integer;
begin
  inherited Run();

  for I := 0 to FItems.Count - 1 do
  begin
    Decl := FItems[I];
    if (Decl.Name = '') then
      Continue;

    if (Decl is TDeclaration_Method) and (TDeclaration_Method(Decl).MethodType in [mtObjectFunction, mtObjectProcedure]) then
      FGlobalTypeMethods.Add(TDeclaration_Method(Decl).ObjectName, Decl)
    else
      FGlobals.Add(Decl.Name, Decl);

    if (Decl is TDeclaration_TypeEnum) then
      for Decl in TDeclaration_TypeEnum(Decl).Elements do
        FGlobals.Add(Decl.Name, Decl);
  end;
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
  Decl: TDeclaration;
begin
  Builder := TAnsiStringBuilder.Create();

  for Decl in FGlobals.GetAll() do
    Builder.AppendLine(Decl.ClassName + ' -> ' + Decl.Name);
  Result := Builder.ToString().Trim();

  Builder.Free();
end;

procedure TCodeParser.Assign(From: TObject);
begin
  inherited Assign(From);

  if (From is TCodeParser) then
  begin
    FOnInclude     := TCodeParser(From).OnInclude;
    FOnFindInclude := TCodeParser(From).OnFindInclude;
    FOnFindLibrary := TCodeParser(From).OnFindLibrary;
    FOnLoadLibrary := TCodeParser(From).OnLoadLibrary;
  end;
end;

end.
