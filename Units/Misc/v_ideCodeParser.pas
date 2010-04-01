unit v_ideCodeParser;

{$include ValistusDefines.inc}

interface

uses
  SysUtils, Classes,
  CastaliaPasLex, CastaliaSimplePasPar,
  v_MiscFunctions;

type
  TDeclaration = class;
  TDeclarationArray = array of TDeclaration;
  TDeclarationClass = class of TDeclaration;

  TVarBase = (vbName, vbType);

  TDeclarationStack = class
  private
    fItems: TList;
    fTop: TDeclaration;
  public
    procedure Push(Item: TDeclaration);
    function Pop: TDeclaration;

    constructor Create;
    destructor Destroy; override;

    property Top: TDeclaration read fTop;
  end;

  TDeclarationList = class
  private
    fItems: TList;

    function GetItem(Index: Integer): TDeclaration;
    function GetCount: Integer;
  public
    procedure AddItem(AItem: TDeclaration);
    procedure DeleteItem(AItem: TDeclaration);
    function GetItemsOfClass(AClass: TDeclarationClass; SubSearch: Boolean = False): TDeclarationArray;
    function GetFirstItemOfClass(AClass: TDeclarationClass; SubSearch: Boolean = False): TDeclaration;
    function GetItemsInPos(AStart, AEnd: Integer; SubSearch: Boolean = False): TDeclarationArray;
    function GetItemInPos(AStart, AEnd: Integer; SubSearch: Boolean = False): TDeclaration;

    procedure Clear;

    constructor Create;
    destructor Destroy; override;

    property Items[Index: Integer]: TDeclaration read GetItem; default;
    property Count: Integer read GetCount;
  end;

  TDeclaration = class
  private
    fParser: TmwSimplePasPar;
    fOwner: TDeclaration;
    fOrigin: PAnsiChar;
    fRawText: string;
    fCleanText: string;
    fShortText: string;
    fStartPos: Integer;
    fEndPos: Integer;
    fItems: TDeclarationList;

    function GetRawText: string; virtual;
    function GetCleanText: string; virtual;
    function GetShortText: string; virtual;
  public
    function HasOwnerClass(AClass: TDeclarationClass; out Declaration: TDeclaration; Recursive: Boolean = False): Boolean;
    function GetOwnersOfClass(AClass: TDeclarationClass): TDeclarationArray;

    constructor Create(AParser: TmwSimplePasPar; AOwner: TDeclaration; AOrigin: PAnsiChar; AStart: Integer; AEnd: Integer = -1); overload; virtual;
    constructor Create(AssignFrom: TDeclaration); overload; virtual;
    destructor Destroy; override;

    property Parser: TmwSimplePasPar read fParser;
    property Owner: TDeclaration read fOwner;
    property Origin: PAnsiChar read fOrigin;

    property RawText: string read GetRawText write fRawText;
    property CleanText: string read GetCleanText;
    property ShortText: string read GetShortText;
    property StartPos: Integer read fStartPos write fStartPos;
    property EndPos: Integer read fEndPos write fEndPos;
    property Items: TDeclarationList read fItems;
  end;

  TciStruct = class(TDeclaration)
  private
    function GetShortText: string; override;
  public
    function HasField(Name: string; out Decl: TDeclaration; Return: TVarBase; var ArrayCount: Integer): Boolean; overload;
    function HasField(Name: string; out Decl: TDeclaration; Return: TVarBase = vbName): Boolean; overload;
    function GetDefault(Return: TVarBase = vbName): TDeclaration;
  end;

  TciTypeKind = class(TDeclaration)
  private
    function GetShortText: string; override;
  public
    function GetRealType(var ArrayCount: Integer): TDeclaration; overload;
    function GetRealType: TDeclaration; overload;
  end;

  TciProcedureName = class(TDeclaration);

  { TciProcedureDeclaration }
  TciProcedureDeclaration = class(TDeclaration)
  private
    fProcType: string;
    fParams: string;
    fSynParams: string;
    fName : TciProcedureName;
    fCleanDecl : string;

    function GetCleanDeclaration: string;
    function GetName: TciProcedureName;
    function GetProcType: string;
    function GetParams: string;
    function GetSynParams: string;

    function GetShortText: string; override;
  public
    function GetParamDeclarations: TDeclarationArray;

    property CleanDeclaration : string read GetCleanDeclaration;
    property Name : TciProcedureName read GetName;
    property ProcType: string read GetProcType;
    property Params: string read GetParams;
    property SynParams: string read GetSynParams;
  end;

  TciUsedUnit = class(TDeclaration);                                        //Included Units
  TciInclude = class(TDeclaration);                                         //Includes
  TciJunk = class(TDeclaration);                                            //Junk

  TciCompoundStatement = class(TDeclaration);                               //Begin-End
  TciWithStatement = class(TDeclaration);                                   //With
  TciSimpleStatement = class(TDeclaration);                                 //Begin-End + With
  TciVariable = class(TDeclaration);                                        //With

  //TciTypeKind = class(TciVarType);                                          //Var + Const + Array + Record
  TciTypedConstant = class(TDeclaration);                                   //Var + Procedure/Function Parameters
  TciExpression = class(TDeclaration);                                      //Var + Const + Enum
  TciProceduralType = class(TciProcedureDeclaration);                       //Var + Tciype + Procedure/Function Parameters

  TciTypeDeclaration = class(TDeclaration);                                 //Type
  TciTypeName = class(TDeclaration);                                        //Type

  TciVarDeclaration = class(TDeclaration);                                  //Var
  TciVarName = class(TDeclaration);                                         //Var

  TciConstantDeclaration = class(TDeclaration);                             //Const
  TciConstantName = class(TDeclaration);                                    //Const

  TciLabelDeclaration = class(TDeclaration);                                //Label
  TciLabelName = class(TDeclaration);                                       //Label

  //TciProcedureDeclaration = class(TDeclaration);                            //Procedure/Function
  //TciProcedureName = class(TDeclaration);                                   //Procedure/Function
  TciProcedureClassName = class(TDeclaration);                              //Class Procedure/Function
  TciReturnType = class(TciTypeKind);                                       //Function Result
  TciForward = class(TciTypeKind);                                          //Forwarding
  TciConstParameter = class(TDeclaration);                                  //Procedure/Function Parameters
  TciOutParameter = class(TDeclaration);                                    //Procedure/Function Parameters
  TciFormalParameter = class(TDeclaration);                                 //Procedure/Function Parameters
  TciInParameter = class(TDeclaration);                                     //Procedure/Function Parameters
  TciVarParameter = class(TDeclaration);                                    //Procedure/Function Parameters
  TciParameterName = class(TDeclaration);                                   //Procedure/Function Parameters
  TciParameterType = class(TciTypeKind);                                    //Procedure/Function Parameters

  TciArrayType = class(TDeclaration);                                       //Array
  TciArrayConstant = class(TDeclaration);                                   //Array

  TciRecordType = class(TciStruct);                                         //Record
  TciClassField = class(TDeclaration);                                      //Record
  TciFieldName = class(TDeclaration);                                       //Record
  TciRecordConstant = class(TDeclaration);                                  //Record
  TciRecordFieldConstant = class(TDeclaration);                             //Record

  TciClassType = class(TciStruct);                                          //Class
  TciAncestorId = class(TDeclaration);                                      //Class
  TciClassMethodHeading = class(TciProcedureDeclaration);                   //Record + Class
  TciClassProperty = class(TDeclaration);                                   //Record + Class
  TciPropertyDefault = class(TDeclaration);                                 //Record + Class
  TciIdentifier = class(TDeclaration);                                      //Record + Class
  TciPropertyParameterList = class(TDeclaration);                           //Record + Class

  TciSetType = class(TDeclaration);                                         //Set
  TciOrdinalType = class(TDeclaration);                                     //Set

  TciEnumType = class(TDeclaration);                                        //Enum
  TciQualifiedIdentifier = class(TDeclaration);                             //Enum

  TCodeParser = class(TmwSimplePasPar)
  protected
    fStack: TDeclarationStack;
    fItems: TDeclarationList;

    function InDeclaration(AClass: TDeclarationClass): Boolean;
    function InDeclarations(AClassArray: array of TDeclarationClass): Boolean;
    procedure PushStack(AClass: TDeclarationClass; AStart: Integer = -1);
    procedure PopStack(AEnd: Integer = -1);

    procedure ParseFile; override;
    procedure OnInclude(Sender: TmwBasePasLex); virtual;                        //Includes
    procedure UsedUnitName; override;                                           //Included Units
    procedure NextToken; override;                                              //Junk
    procedure OnDirect(Sender: TmwBasePasLex);                                  //Junk

    procedure CompoundStatement; override;                                      //Begin-End
    procedure WithStatement; override;                                          //With
    procedure SimpleStatement; override;                                        //Begin-End + With
    procedure Variable; override;                                               //With

    procedure TypeKind; override;                                               //Var + Const + Array + Record
    procedure TypedConstant; override;                                          //Var + Procedure/Function Parameters
    procedure Expression; override;                                             //Var + Const + ArrayConst
    procedure ProceduralType; override;                                         //Var + Tciype + Procedure/Function Parameters

    procedure TypeDeclaration; override;                                        //Type
    procedure TypeName; override;                                               //Type

    procedure VarDeclaration; override;                                         //Var
    procedure VarName; override;                                                //Var

    procedure ConstantDeclaration; override;                                    //Const
    procedure ConstantName; override;                                           //Const

    procedure LabelDeclarationSection; override;                                //Label
    procedure LabelId; override;                                                //Label

    procedure ProcedureDeclarationSection; override;                            //Procedure/Function
    procedure FunctionProcedureName; override;                                  //Procedure/Function
    procedure ObjectNameOfMethod; override;                                     //Class Procedure/Function
    procedure ReturnType; override;                                             //Function Result
    procedure ForwardDeclaration; override;                                     //Forwarding
    procedure ConstParameter; override;                                         //Procedure/Function Parameters
    procedure OutParameter; override;                                           //Procedure/Function Parameters
    procedure ParameterFormal; override;                                        //Procedure/Function Parameters
    procedure InParameter; override;                                            //Procedure/Function Parameters
    procedure VarParameter; override;                                           //Procedure/Function Parameters
    procedure ParameterName; override;                                          //Procedure/Function Parameters
    procedure NewFormalParameterType; override;                                 //Procedure/Function Parameters

    procedure ArrayType; override;                                              //Array
    procedure ArrayConstant; override;                                          //Array Const

    procedure RecordType; override;                                             //Record
    procedure ClassField; override;                                             //Record + Class
    procedure FieldName; override;                                              //Record + Class
    procedure RecordConstant; override;                                         //Record Const
    procedure RecordFieldConstant; override;                                    //Record Const

    procedure ClassType; override;                                              //Class
    procedure AncestorId; override;                                             //Class
    procedure ClassMethodHeading; override;                                     //Class
    procedure ConstructorName; override;                                        //Class
    procedure DestructorName; override;                                         //Class
    procedure FunctionMethodName; override;                                     //Class
    procedure ProcedureMethodName; override;                                    //Class
    procedure ClassProperty; override;                                          //Record + Class
    procedure PropertyName; override;                                           //Record + Class
    procedure TypeId; override;                                                 //Record + Class
    procedure PropertyDefault; override;                                        //Record + Class
    procedure Identifier; override;                                             //Record + Class
    procedure PropertyParameterList; override;                                  //Record + Class

    procedure SetType; override;                                                //Set
    procedure OrdinalType; override;                                            //Set + Array Range

    procedure EnumeratedType; override;                                         //Enum
    procedure QualifiedIdentifier; override;                                    //Enum
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property Items: TDeclarationList read fItems;
  end;

implementation

uses
  CastaliaPasLexTypes;

procedure TDeclarationStack.Push(Item: TDeclaration);
begin
  fItems.Add(Item);
  fTop := Item;
end;

function TDeclarationStack.Pop: TDeclaration;
begin
  with fItems do
  begin
    if (Count > 0) then
      Delete(Count - 1);
    if (Count > 0) then
      fTop := TDeclaration(Items[Count - 1])
    else
      fTop := nil;
  end;
  Result := fTop;
end;

constructor TDeclarationStack.Create;
begin
  fItems := TList.Create;
  fTop := nil;
end;

destructor TDeclarationStack.Destroy;
begin
  FreeAndNil(fItems);

  inherited;
end;

function TDeclarationList.GetItem(Index: Integer): TDeclaration;
begin
  Result := TDeclaration(fItems[Index]);
end;

function TDeclarationList.GetCount: Integer;
begin
  Result := fItems.Count;
end;

procedure TDeclarationList.AddItem(AItem: TDeclaration);
begin
  fItems.Add(AItem);
end;

procedure TDeclarationList.DeleteItem(AItem: TDeclaration);
var
  i: Integer;
begin
  for i := 0 to fItems.Count - 1 do
    if (TDeclaration(fItems[i]) = AItem) then
    begin
      TDeclaration(fItems[i]).Free;
      fItems.Delete(i);
      Break;
    end;
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
    if (Item is AClass) then
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

  for i := 0 to fItems.Count - 1 do
    SearchItem(AClass, SubSearch, TDeclaration(fItems[i]), Result, l);
end;

function TDeclarationList.GetFirstItemOfClass(AClass: TDeclarationClass; SubSearch: Boolean = False): TDeclaration;

  function SearchItem(AClass: TDeclarationClass; SubSearch: Boolean; Item: TDeclaration; out Res: TDeclaration): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    if (Item is AClass) then
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
  for i := 0 to fItems.Count - 1 do
    SearchItem(AClass, SubSearch, TDeclaration(fItems[i]), Result);
end;

function TDeclarationList.GetItemsInPos(AStart, AEnd: Integer; SubSearch: Boolean = False): TDeclarationArray;

  procedure SearchItem(
    AStart, AEnd: Integer;
    SubSearch: Boolean;
    Item: TDeclaration;
    var Res: TDeclarationArray;
    var ResIndex: Integer);
  var
    i: Integer;
    b: Boolean;
  begin
    b := False;
    if (AStart >= Item.StartPos) and (AEnd <= Item.EndPos) then
    begin
      SetLength(Res, ResIndex + 1);
      Res[ResIndex] := Item;
      Inc(ResIndex);
      b := True;
    end;
    if SubSearch and b then
      for i := 0 to Item.Items.Count - 1 do
        SearchItem(AStart, AEnd, SubSearch, Item.Items[i], Res, ResIndex);
  end;

var
  i, l: Integer;
begin
  l := 0;
  SetLength(Result, 0);

  for i := 0 to fItems.Count - 1 do
    SearchItem(AStart, AEnd, SubSearch, TDeclaration(fItems[i]), Result, l);
end;

function TDeclarationList.GetItemInPos(AStart,  AEnd: Integer; SubSearch: Boolean = False): TDeclaration;

  function SearchItem(AStart, AEnd: Integer; SubSearch: Boolean; Item: TDeclaration; out Res: TDeclaration): Boolean;
  var
    i: Integer;
    b: Boolean;
  begin
    Result := False;
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
  for i := 0 to fItems.Count - 1 do
    SearchItem(AStart, AEnd, SubSearch, TDeclaration(fItems[i]), Result);
end;

procedure TDeclarationList.Clear;
var
  i: Integer;
begin
  for i := 0 to fItems.Count - 1 do
    if (Assigned(fItems[i])) then
      TDeclaration(fItems[i]).Free;
  fItems.Clear;
end;

constructor TDeclarationList.Create;
begin
  fItems := TList.Create;
end;

destructor TDeclarationList.Destroy;
begin
  Clear;

  FreeAndNil(fItems);
  inherited;
end;

function TDeclaration.GetRawText: string;
begin
  Result := '';
  if (fRawText <> '') then
    Result := fRawText
  else if (fStartPos <> fEndPos) and (fOrigin <> nil) then
  begin
    SetString(fRawText, fOrigin + fStartPos, fEndPos - fStartPos);
    Result := fRawText;
  end;
end;

function TDeclaration.GetCleanText: string;
var
  i: Integer;
  a: TDeclarationArray;
begin
  Result := '';
  if (fCleanText <> '') then
    Result := fCleanText
  else if (fStartPos <> fEndPos) and (fOrigin <> nil) then
  begin
    fCleanText := RawText;
    a := Items.GetItemsOfClass(TciJunk, True);
    for i := High(a) downto 0 do
    begin
      Delete(fCleanText, a[i].StartPos - fStartPos + 1, a[i].EndPos - a[i].StartPos);
      if (Pos(LineEnding, a[i].GetRawText) > 0) then
        Insert(LineEnding, fCleanText, a[i].StartPos - fStartPos + 1)
      else
        Insert(' ', fCleanText, a[i].StartPos - fStartPos + 1);
    end;
    Result := fCleanText;
  end;
end;

function TDeclaration.GetShortText: string;
begin
  if (fShortText = '') then
    fShortText := CompressWhiteSpace(CleanText);
  Result := fShortText;
end;

function TDeclaration.HasOwnerClass(AClass: TDeclarationClass; out Declaration: TDeclaration; Recursive: Boolean = False): Boolean;

  function IsOwner(Item: TDeclaration; AClass: TDeclarationClass; out Decl: TDeclaration; Recursive: Boolean): Boolean;
  begin
    if (Item.Owner is AClass) then
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
    if (Item.Owner is AClass) then
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

constructor TDeclaration.Create(AParser: TmwSimplePasPar; AOwner: TDeclaration; AOrigin: PAnsiChar; AStart, AEnd: Integer);
begin
  fParser := AParser;
  fOwner := AOwner;
  fOrigin := AOrigin;
  fRawText := '';
  fCleanText := '';
  fStartPos := AStart;
  if (AEnd > -1) then
    fEndPos := AEnd
  else
    fEndPos := AStart;

  fItems := TDeclarationList.Create;
end;

constructor TDeclaration.Create(AssignFrom: TDeclaration);
begin
  if (AssignFrom <> nil) then
    Create(AssignFrom.Parser, AssignFrom.Owner, AssignFrom.Origin, AssignFrom.StartPos, AssignFrom.EndPos)
  else
    Create(nil, nil, nil, -1);
end;

destructor TDeclaration.Destroy;
begin
  FreeAndNil(fItems);

  inherited;
end;

function TciStruct.GetShortText: string;
begin
  if (fShortText = '') then
    fShortText := GetFirstWord(CleanText);
  Result := fShortText;
end;

function TciStruct.HasField(Name: string; out Decl: TDeclaration; Return: TVarBase; var ArrayCount: Integer): Boolean;
var
  a, b: TDeclarationArray;
  i, ii: Integer;
  t: TDeclaration;
begin
  Result := False;
  Name := PrepareString(Name);

  a := fItems.GetItemsOfClass(TciClassField);
  for i := Low(a) to High(a) do
  begin
    b := a[i].Items.GetItemsOfClass(TciFieldName);
    for ii := Low(b) to High(b) do
    begin
      if (PrepareString(b[ii].CleanText) = Name) then
      begin
        Result := True;
        if (Return = vbType) then
          Decl := b[ii].Owner.Items.GetFirstItemOfClass(TciTypeKind)
        else
          Decl := b[ii];
        Exit;
      end;
    end;
  end;

  a := fItems.GetItemsOfClass(TciClassProperty);
  for i := Low(a) to High(a) do
  begin
    b := a[i].Items.GetItemsOfClass(TciFieldName);
    for ii := Low(b) to High(b) do
    begin
      if (PrepareString(b[ii].CleanText) = Name) then
      begin
        Result := True;

        t := a[i].Items.GetFirstItemOfClass(TciPropertyParameterList);
        if (t <> nil) then
          //ArrayCount := ArrayCount + t.Items.Count;   [a, b] Indices count as 1
          Inc(ArrayCount);

        if (Return = vbType) then
          Decl := b[ii].Owner.Items.GetFirstItemOfClass(TciTypeKind)
        else
          Decl := b[ii];
        Exit;
      end;
    end;
  end;

  a := fItems.GetItemsOfClass(TciClassMethodHeading);
  for i := Low(a) to High(a) do
  begin
    b := a[i].Items.GetItemsOfClass(TciProcedureName);
    for ii := Low(b) to High(b) do
      if (PrepareString(b[ii].CleanText) = Name) then
      begin
        Result := True;
        if (Return = vbType) then
          Decl := b[ii].Owner
        else
          Decl := b[ii];
        Exit;
      end;
  end;
end;

function TciStruct.HasField(Name: string; out Decl: TDeclaration; Return: TVarBase = vbName): Boolean;
var
  a: Integer;
begin
  Result := HasField(Name, Decl, Return, a);
end;

function TciStruct.GetDefault(Return: TVarBase = vbName): TDeclaration;
var
  d: TDeclaration;
begin
  Result := nil;
  d := fItems.GetFirstItemOfClass(TciPropertyDefault, True);
  if (d <> nil) then
    if (Return = vbType) then
      Result := d.Owner.Items.GetFirstItemOfClass(TciTypeKind)
    else
      Result := d.Owner.Items.GetFirstItemOfClass(TciFieldName)
end;

function TciTypeKind.GetShortText: string;
var
  d: TDeclaration;
begin
  if (fShortText = '') then
  begin
    d := GetRealType;
    if (d = nil) or (not (d is TciStruct)) then
      fShortText := CompressWhiteSpace(CleanText)
    else
      fShortText := CompressWhiteSpace(StringReplace(CleanText, d.CleanText, GetFirstWord(d.CleanText), []));
  end;
  Result := fShortText;
end;

function TciTypeKind.GetRealType(var ArrayCount: Integer): TDeclaration;
var
  d, t: TDeclaration;
begin
  d := Self;
  while (d <> nil) do
  begin
    if (d.Items.Count > 0) then
    begin
      d := d.Items[0];
      if (d is TciArrayType) then
      begin
        d := d.Items.GetFirstItemOfClass(TciTypeKind);
        Inc(ArrayCount);
        Continue;
      end;

      if (d is TciProceduralType) then
      begin
        t := d.Items.GetFirstItemOfClass(TciReturnType);
        if (t <> nil) then
        begin
          d := t;
          Continue;
        end
        else
          Break;
      end;

      if (d is TciTypeKind) then
        Continue;
    end;

    Break;
  end;

  Result := d;
end;

function TciTypeKind.GetRealType: TDeclaration;
var
  a: Integer;
begin
  Result := GetRealType(a);
end;

function TciProcedureDeclaration.GetProcType: string;
var
  s: string;
begin
  if (fProcType = '') then
  begin
    s := CleanText;
    fProcType := GetFirstWord(s);
    if (LowerCase(fProcType) = 'class') then
    begin
      Delete(s, 1, 6);
      fProcType := GetFirstWord(s);
    end;

    if (fProcType = '') then
      if (Items.GetFirstItemOfClass(TciReturnType) <> nil) then
        fProcType := 'function'
      else
        fProcType := 'procedure';
  end;

  Result := fProcType;
end;

function TciProcedureDeclaration.GetName: TciProcedureName;
var
  ProcedureName : TciProcedureName;
begin
  if (fName <> nil) then
    result := fName
  else
  begin
    ProcedureName := TciProcedureName(fItems.GetFirstItemOfClass(TciProcedureName));
    if ProcedureName <> nil then
      result := ProcedureName
    else
      Result := nil;
    fName := result;
  end;
end;

function TciProcedureDeclaration.GetCleanDeclaration: string;
var
  Return : TciReturnType;
begin
  if (fCleanDecl <> '') then
    result := fCleanDecl
  else
  begin
    result := '';
    if Name = nil then
      exit;
    result := proctype + ' ' + Name.ShortText;
    if Params <> '' then
      result := result + '(' + params + ')';
    Return := fItems.GetFirstItemOfClass(TciReturnType) as TciReturnType;
    if (Return <> nil) then
      result := result + ': ' + Return.ShortText
    else
      result := result + ';';
  end;
end;


function TciProcedureDeclaration.GetParams: string;
var
  i: Integer;
  a: TDeclarationArray;
begin
  Result := '';
  if (fParams <> '') then
    Result := fParams
  else if (fItems.Count > 0) then
  begin
    a := GetParamDeclarations;
    for i := Low(a) to High(a) do
      if (fParams <> '') then
        fParams := fParams + '; ' + a[i].ShortText
      else
        fParams := fParams + a[i].ShortText;
    Result := fParams;
  end;
end;

function TciProcedureDeclaration.GetSynParams: string;
var
  i, ii: Integer;
  a, b: TDeclarationArray;
  d: TDeclaration;
  s, t: string;
begin
  Result := '';
  if (fSynParams <> '') then
    Result := fSynParams
  else if (fItems.Count > 0) then
  begin
    a := GetParamDeclarations;
    for i := Low(a) to High(a) do
    begin
      if (a[i] is TciConstParameter) then
        s := 'const '
      else if (a[i] is TciOutParameter) then
        s := 'out '
      else if (a[i] is TciInParameter) then
        s := 'in '
      else if (a[i] is TciVarParameter) then
        s := 'var '
      else
        s := '';
      d := a[i].Items.GetFirstItemOfClass(TciParameterType);
      if (d <> nil) then
        t := ': ' + d.ShortText
      else
        t := '';
      b := a[i].Items.GetItemsOfClass(TciParameterName);
      for ii := Low(b) to High(b) do
      begin
        if (fSynParams <> '') then
          fSynParams := fSynParams + ';","' + s + b[ii].ShortText + t
        else
          fSynParams := '"' + s + b[ii].ShortText + t;
      end;
    end;
    if (fSynParams <> '') then
      fSynParams := fSynParams + '"';
    Result := fSynParams;
  end;
end;

function TciProcedureDeclaration.GetShortText: string;
begin
  if (fShortText = '') then
    fShortText := ProcType;
  Result := fShortText;
end;

function TciProcedureDeclaration.GetParamDeclarations: TDeclarationArray;
var
  i: Integer;
begin
  SetLength(Result, 0);

  for i := 0 to fItems.Count - 1 do
    if (fItems[i] is TciConstParameter) or
       (fItems[i] is TciOutParameter) or
       (fItems[i] is TciFormalParameter) or
       (fItems[i] is TciInParameter) or
       (fItems[i] is TciVarParameter) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := fItems[i];
    end;
end;

function TCodeParser.InDeclaration(AClass: TDeclarationClass): Boolean;
begin
  if (fStack.Top = nil) then
    Result := (AClass = nil)
  else
    Result := (fStack.Top is AClass);
end;

function TCodeParser.InDeclarations(AClassArray: array of TDeclarationClass): Boolean;
var
  i: Integer;
  t: TDeclaration;
begin
  Result := False;
  t := fStack.Top;
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

procedure TCodeParser.PushStack(AClass: TDeclarationClass; AStart: Integer = -1);
var
  t: TDeclaration;
begin
  if (AStart = -1) then
    AStart := Lexer.TokenPos;
  t := AClass.Create(Self, fStack.Top, Lexer.Origin, AStart);
  if (fStack.Top <> nil) then
    fStack.Top.Items.AddItem(t)
  else
    fItems.AddItem(t);
  fStack.Push(t);
end;

procedure TCodeParser.PopStack(AEnd: Integer = -1);
begin
  if (AEnd = -1) then
    AEnd := Lexer.TokenPos;
  if (fStack.Top <> nil) then
    fStack.Top.EndPos := AEnd;
  fStack.Pop;
end;

constructor TCodeParser.Create;
begin
  inherited;

  fStack := TDeclarationStack.Create;
  fItems := TDeclarationList.Create;

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
  FreeAndNil(fStack);
  FreeAndNil(fItems);

  inherited;
end;

procedure TCodeParser.ParseFile;
begin
  SkipJunk;
  case GenID of
    TokLibrary:
      begin
        LibraryFile;
      end;
    TokPackage:
      begin
        PackageFile;
      end;
    TokUnit:
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
        SEMICOLON;
      end;
      if (TokenID = TokUses) then
        MainUsesClause;

      while TokenID in [TokClass, TokConst, TokConstructor, TokDestructor, TokExports,
        TokFunction, TokLabel, TokProcedure, TokResourceString, TokThreadVar, TokType,
        TokVar{$IFDEF D8_NEWER}, TokSquareOpen{$ENDIF}] do
      begin
        DeclarationSection;
      end;

      if (TokenID = TokBegin) then
      begin
        CompoundStatement;
        Expected(TokPoint);
      end;
    end;
  end;
end;

procedure TCodeParser.OnInclude(Sender: TmwBasePasLex);
begin
  if (not Sender.IsJunk) then
  begin
    PushStack(TciInclude, Sender.TokenPos);
    fStack.Top.RawText := Sender.DirectiveParamOriginal;
    PopStack(Sender.TokenPos + Sender.TokenLen);
  end;

  Sender.Next;
end;

procedure TCodeParser.UsedUnitName;
begin
  PushStack(TciUsedUnit);
  inherited;
  PopStack;
end;

procedure TCodeParser.NextToken;
var
  ValidJunk: Boolean;
begin
  ValidJunk := False;
  Lexer.Next;
  if Lexer.IsJunk and (not InDeclaration(TciJunk)) then
  begin
    {if (not InDeclaration(nil)) then
      PushStack(TciJunk);
    while (Lexer.IsJunk) do
    begin
      if (not (Lexer.TokenID in [TokCRLF, TokCRLFCo, TokSpace])) then
        ValidJunk := True;
      Lexer.Next;
    end;
    if InDeclaration(TciJunk) then
    begin
      if ValidJunk then
        PopStack
      else
      begin
        if (fStack.Top.Owner <> nil) then
          fStack.Top.Owner.Items.DeleteItem(fStack.Top)
        else
          fStack.Top.Free;
        fStack.Pop;
      end;
    end;}

    while Lexer.IsJunk do
    begin
      if (Lexer.TokenID in [tokAnsiComment, tokBorComment, tokSlashesComment]) then
      begin
        if (not InDeclaration(TciJunk)) then
          PushStack(TciJunk);
      end
      else if InDeclaration(TciJunk) then
        PopStack;
      Lexer.Next;
    end;
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
var
  n: Boolean;
begin
  if (not InDeclarations([TciVarDeclaration, TciConstantDeclaration, TciTypeDeclaration, TciArrayType, TciClassField])) then
  begin
    inherited;
    Exit;
  end;

  n := (InDeclaration(TciArrayType)) and (TokenID = tokConst);
  PushStack(TciTypeKind);
  if n then
    NextToken
  else
    inherited;
  PopStack;
end;

procedure TCodeParser.TypedConstant;
begin
  if (not InDeclarations([TciVarDeclaration, TciConstParameter, TciOutParameter, TciFormalParameter, TciInParameter, TciVarParameter])) then
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

procedure TCodeParser.TypeDeclaration;
begin
  PushStack(TciTypeDeclaration);
  inherited;
  PopStack;
end;

procedure TCodeParser.TypeName;
begin
  if (not InDeclaration(TciTypeDeclaration)) then
    Exit;
  PushStack(TciTypeName);
  inherited;
  PopStack;
end;

procedure TCodeParser.VarDeclaration;
begin
  PushStack(TciVarDeclaration);
  inherited;
  PopStack;
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
begin
  PushStack(TciConstantDeclaration);
  inherited;
  PopStack;
end;

procedure TCodeParser.ConstantName;
begin
  if (not InDeclaration(TciConstantDeclaration)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciConstantName);
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
begin
  PushStack(TciProcedureDeclaration);
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

  PushStack(TciProcedureClassName);
  inherited;
  PopStack;
end;

procedure TCodeParser.ReturnType;
begin
  if (not InDeclarations([TciProcedureDeclaration, TciProceduralType, TciClassMethodHeading])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciReturnType);
  //inherited;
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

procedure TCodeParser.ConstParameter;
begin
  if (not InDeclarations([TciProcedureDeclaration, TciProceduralType, TciClassMethodHeading])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciConstParameter);
  inherited;
  PopStack;
end;

procedure TCodeParser.OutParameter;
begin
  if (not InDeclarations([TciProcedureDeclaration, TciProceduralType, TciClassMethodHeading])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciOutParameter);
  inherited;
  PopStack;
end;

procedure TCodeParser.ParameterFormal;
begin
  if (not InDeclarations([TciProcedureDeclaration, TciProceduralType, TciClassMethodHeading])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciFormalParameter);
  inherited;
  PopStack;
end;

procedure TCodeParser.InParameter;
begin
  if (not InDeclarations([TciProcedureDeclaration, TciProceduralType, TciClassMethodHeading])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciInParameter);
  inherited;
  PopStack;
end;

procedure TCodeParser.VarParameter;
begin
  if (not InDeclarations([TciProcedureDeclaration, TciProceduralType, TciClassMethodHeading])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciVarParameter);
  inherited;
  PopStack;
end;

procedure TCodeParser.ParameterName;
begin
  if (not InDeclarations([TciConstParameter, TciOutParameter, TciFormalParameter, TciInParameter, TciVarParameter])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciParameterName);
  inherited;
  PopStack;
end;

procedure TCodeParser.NewFormalParameterType;
begin
  if (not InDeclarations([TciConstParameter, TciOutParameter, TciFormalParameter, TciInParameter, TciVarParameter])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciParameterType);
  //inherited;
  TypeKind;
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

procedure TCodeParser.RecordType;
begin
  PushStack(TciRecordType);
  inherited;
  PopStack;
end;

procedure TCodeParser.ClassField;
begin
  if (not InDeclarations([TciRecordType, TciClassType])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciClassField);
  inherited;
  PopStack;
end;

procedure TCodeParser.FieldName;
begin
  if (not InDeclaration(TciClassField)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciFieldName);
  inherited;
  PopStack;
end;

procedure TCodeParser.RecordConstant;
begin
  if (not InDeclarations([TciTypedConstant, TciArrayConstant])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciRecordConstant);
  inherited;
  PopStack;
end;

procedure TCodeParser.RecordFieldConstant;
begin
  if (not InDeclaration(TciRecordConstant)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciRecordFieldConstant);
  inherited;
  PopStack;
end;

procedure TCodeParser.ClassType;
begin
  PushStack(TciClassType);
  inherited;
  PopStack;
end;

procedure TCodeParser.AncestorId;
begin
  if (not InDeclaration(TciClassType)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciAncestorID);
  inherited;
  PopStack;
end;

procedure TCodeParser.ClassMethodHeading;
begin
  if (not InDeclarations([TciRecordType, TciClassType])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciClassMethodHeading);
  inherited;
  PopStack;
end;

procedure TCodeParser.ConstructorName;
begin
  if (not InDeclaration(TciClassMethodHeading)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciProcedureName);
  inherited;
  PopStack;
end;

procedure TCodeParser.DestructorName;
begin
  if (not InDeclaration(TciClassMethodHeading)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciProcedureName);
  inherited;
  PopStack;
end;

procedure TCodeParser.FunctionMethodName;
begin
  if (not InDeclaration(TciClassMethodHeading)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciProcedureName);
  inherited;
  PopStack;
end;

procedure TCodeParser.ProcedureMethodName;
begin
  if (not InDeclaration(TciClassMethodHeading)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciProcedureName);
  inherited;
  PopStack;
end;

procedure TCodeParser.ClassProperty;
begin
  if (not InDeclarations([TciRecordType, TciClassType])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciClassProperty);
  inherited;
  PopStack;
end;

procedure TCodeParser.PropertyName;
begin
  if (not InDeclaration(TciClassProperty)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciFieldName);
  inherited;
  PopStack;
end;

procedure TCodeParser.TypeId;
begin
  if (not InDeclarations([TciClassProperty, TciPropertyParameterList])) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciTypeKind);
  inherited;
  PopStack;
end;

procedure TCodeParser.PropertyDefault;
begin
  if (not InDeclaration(TciClassProperty)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciPropertyDefault);
  inherited;
  PopStack;
end;

procedure TCodeParser.Identifier;
begin
  if (not InDeclaration(TciPropertyParameterList)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciIdentifier);
  inherited;
  PopStack;
end;

procedure TCodeParser.PropertyParameterList;
begin
  if (not InDeclaration(TciClassProperty)) then
  begin
    inherited;
    Exit;
  end;

  PushStack(TciPropertyParameterList);
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
  if (not InDeclarations([TciSetType, TciArrayType])) then
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

  PushStack(TciQualifiedIdentifier);
  inherited;
  PopStack;
end;

end.
