unit simba.codeinsight;

{$modeswitch nestedprocvars}

interface

uses
  sysutils, classes,
  castaliapaslex, castaliapaslextypes,
  simba.codeparser, simba.generics;

type
  TCodeInsight = class;
  TCodeInsightArray = array of TCodeInsight;
  TCodeInsightMap = specialize TSimbaStringMap<TCodeInsight>;

  TOnFindInclude = function(Sender: TObject; var FileName: string): Boolean of object;
  TOnLoadLibrary = function(Sender: TObject; var FileName: string; out ci: TCodeInsight): Boolean of object;

  TCodeInsightSearch = procedure(CodeInsight: TCodeInsight) is nested;

  TCodeInsight = class(TCodeParser)
  protected
    FRefCount: Integer;

    FFileName: string;
    FMemoryStream: TMemoryStream;
    FOwnStream: Boolean;

    FPos: Integer;
    FDeclarationAtPos: TDeclaration;

    FOnFindInclude: TOnFindInclude;
    FOnLoadLibrary: TOnLoadLibrary;
    FIncludes: TCodeInsightArray;

    FInMethod: TciProcedureDeclaration;
    FInWith: TDeclarationArray;

    FCachedGlobals: TDeclarationArray;
    FCachedLocals: TDeclarationArray;

    function GetIncludesHash: String;
    function GetLibrary: Boolean;

    procedure SetPos(Value: Integer);

    procedure Reset;
    procedure Init;

    procedure AddInclude(const ci: TCodeInsight);
    procedure FreeIncludes();

    function FindInclude(var AFileName: string): Boolean;
    procedure ParseInclude(AFileName: string);
    function LoadLibrary(var LibName: string): Boolean;
    procedure OnInclude(Sender: TmwBasePasLex); override;
  public
    function ParseExpression(Expr: String; constref Declarations: TDeclarationArray; out Members: TDeclarationArray): TDeclaration; overload;
    function ParseExpression(Expr: String; out Members: TDeclarationArray): TDeclaration; overload;
    function ParseExpression(Expr: String): TDeclaration; overload;

    procedure Search(Callback: TCodeInsightSearch);

    function getGlobalTypeMembers(Declaration: TciTypeDeclaration): TDeclarationArray; overload;
    function getGlobalTypeMembers(Declaration: TciTypeDeclaration; Name: String): TDeclarationArray; overload;

    function getDeclarations(Name: String): TDeclarationArray;
    function getLocals: TDeclarationArray;
    function getGlobals: TDeclarationArray;
    function getGlobalType(Name: String): TciTypeDeclaration;

    procedure Assign(From: TObject); override;
    procedure AddRef;
    procedure DecRef;

    procedure Run(SourceStream: TCustomMemoryStream = nil; ManageStream: Boolean = False; MaxPos: Integer = -1); reintroduce; overload;
    procedure Run(const Source: String; MaxPos: Integer = -1); reintroduce; overload;

    property FileName: String read FFileName write FFileName;
    property OnFindInclude: TOnFindInclude read FOnFindInclude write FOnFindInclude;
    property OnLoadLibrary: TOnLoadLibrary read FOnLoadLibrary write FOnLoadLibrary;
    property Position: Integer read FPos write SetPos;
    property DeclarationAtPos: TDeclaration read FDeclarationAtPos;
    property Includes: TCodeInsightArray read FIncludes;
    property IncludesHash: String read GetIncludesHash;
    property IsLibrary: Boolean read GetLibrary;
    property InMethod: TciProcedureDeclaration read FInMethod;
    property InWith: TDeclarationArray read FInWith;

    constructor Create(AFileName: String = ''); reintroduce;
    destructor Destroy; override;
  end;

  TIncludeBuffer = record
    DefinesIn, DefinesOut: TSaveDefinesRec;
    LastUsed, LastChanged: Integer;
    CodeInsight: TCodeInsight;
  end;
  TIncludeBufferArray = array of TIncludeBuffer;

var
  CoreBuffer: TCodeInsightArray;
  CoreDefines: TStringList;

operator +(Left: TCodeInsightArray; Right: TCodeInsight): TCodeInsightArray;

implementation

uses
  simba.settings, syncobjs, simba.parser_misc, md5;

var
  IncludeBuffer: TIncludeBufferArray;
  IncludeBufferCS: TCriticalSection;

const
  LibPrefix = 'lib:';
  PurgeThreshold = 500;

procedure ClearCoreBuffer;
var
  i: Integer;
begin
  for i := 0 to High(CoreBuffer) do
    CoreBuffer[i].DecRef();
end;

procedure ClearIncludeBuffer;
var
  i: Integer;
begin
  IncludeBufferCS.Acquire();

  try
    for i := 0 to High(IncludeBuffer) do
      IncludeBuffer[i].CodeInsight.DecRef();

    SetLength(IncludeBuffer, 0);
  finally
    IncludeBufferCS.Release();
  end;
end;

function GetIncludeBuffer(out Buffer: TIncludeBuffer; FileName: string; LastChanged: Integer; Defines: PSaveDefinesRec = nil): Boolean;
var
  i, l: Integer;
begin
  // Should already be in critical section (!)
  IncludeBufferCS.Acquire();

  try
    Result := False;
    l := High(IncludeBuffer);

    for i := l downto 0 do
    begin
      if (IncludeBuffer[i].CodeInsight <> nil) and (IncludeBuffer[i].CodeInsight.FileName = FileName) then
        if (IncludeBuffer[i].LastChanged <> LastChanged) then
          IncludeBuffer[i].LastUsed := PurgeThreshold
        else if (Defines <> nil) and ((IncludeBuffer[i].DefinesIn.Stack <> Defines^.Stack) or (IncludeBuffer[i].DefinesIn.Defines <> Defines^.Defines)) then
          Inc(IncludeBuffer[i].LastUsed, 3)
        else
        begin
          IncludeBuffer[i].LastUsed := 0;
          Buffer := IncludeBuffer[i];
          Result := True;
        end
      else
        Inc(IncludeBuffer[i].LastUsed, 1);

      if (IncludeBuffer[i].LastUsed >= PurgeThreshold) then
      begin
        WriteLn('Purging from CI include cache: ' + IncludeBuffer[i].CodeInsight.FileName);
        IncludeBuffer[i].CodeInsight.DecRef();
        IncludeBuffer[i] := IncludeBuffer[l];
        Dec(l);
      end;
    end;

    SetLength(IncludeBuffer, l+1);

  finally
    IncludeBufferCS.Release();
  end;
end;

procedure AddIncludeBuffer(const Buffer: TIncludeBuffer);
var
  l: Integer;
begin
  // Should already be in critical section (!)
  IncludeBufferCS.Acquire();

  try
    l := Length(IncludeBuffer);
    SetLength(IncludeBuffer, l + 1);

    Buffer.CodeInsight.AddRef();
    IncludeBuffer[l] := Buffer;
  finally
    IncludeBufferCS.Release();
  end;
end;

operator +(Left: TCodeInsightArray; Right: TCodeInsight): TCodeInsightArray;
begin
  SetLength(Result, Length(Left) + 1);

  if Length(Left) > 0 then
    Move(Left[0], Result[0], Length(Left) * SizeOf(TCodeInsight));

  Result[High(Result)] := Right;
end;

procedure TCodeInsight.AddInclude(const ci: TCodeInsight);
var
  l: Integer;
begin
  l := Length(FIncludes);
  SetLength(FIncludes, l + 1);

  ci.AddRef();
  FIncludes[l] := ci;
end;

procedure TCodeInsight.FreeIncludes();
var
  i: Integer;
begin
  for i := 0 to High(FIncludes) do
    FIncludes[i].DecRef();
  SetLength(FIncludes, 0);
end;

function TCodeInsight.FindInclude(var AFileName: string): Boolean;
var
  s: string;
begin
  s := AFileName;
  if Assigned(OnFindInclude) and OnFindInclude(Self, s) then
  begin
    AFileName := ExpandFileName(s);
    Exit(True);
  end;

  s := ExtractFilePath(FFileName);
  if (s <> '') and FileExists(s + AFileName) then
  begin
    AFileName := ExpandFileName(s + AFileName);
    Exit(True);
  end;

  Result := False;
end;

procedure TCodeInsight.ParseInclude(AFileName: string);
var
  i, Age: Integer;
  Def: TSaveDefinesRec;
  Buf: TIncludeBuffer;
begin
  IncludeBufferCS.Acquire();

  try
    Age := FileAge(AFileName);
    Def := Lexer.SaveDefines;

    if GetIncludeBuffer(Buf, AFileName, Age, @Def) then
    begin
      Lexer.LoadDefines(Buf.DefinesOut);
      AddInclude(Buf.CodeInsight);
      Exit;
    end;

    with Buf do
    begin
      DefinesIn := Def;
      LastChanged := Age;
      LastUsed := 0;

      CodeInsight := TCodeInsight.Create(AFileName);
      CodeInsight.Assign(Self);

      if (CodeInsight.Lexer.Defines.IndexOf('IS_INCLUDE') < 0) then
        i := CodeInsight.Lexer.Defines.Add('IS_INCLUDE')
      else
        i := -1;

      CodeInsight.Run;

      if (i > -1) then
      begin
        i := CodeInsight.Lexer.Defines.IndexOf('IS_INCLUDE');

        if (i > -1) then
          CodeInsight.Lexer.Defines.Delete(i);
      end;

      Lexer.CloneDefinesFrom(CodeInsight.Lexer);
      DefinesOut := Lexer.SaveDefines();

      AddInclude(CodeInsight);
      AddIncludeBuffer(Buf);

      CodeInsight.DecRef();
    end;
  finally
    IncludeBufferCS.Release();
  end;
end;

function TCodeInsight.LoadLibrary(var LibName: string): Boolean;
var
  i, Age: Integer;
  s: string;
  ci: TCodeInsight;
  Buf: TIncludeBuffer;
begin
  Result := False;

  for i := High(FIncludes) downto 0 do
    if (FIncludes[i].FileName = LibPrefix+LibName) then
      Exit(True);

  Age := FileAge(LibName);
  IncludeBufferCS.Acquire();

  try

    if GetIncludeBuffer(Buf, LibPrefix + LibName, Age, nil) then
    begin
      AddInclude(Buf.CodeInsight);
      Exit(True);
    end;

    s := LibName;
    if Assigned(OnLoadLibrary) and OnLoadLibrary(Self, s, ci) and (ci <> nil) then
    begin
      with Buf do
      begin
        LastChanged := Age;
        LastUsed := 0;

        CodeInsight := ci;
        CodeInsight.FileName := LibPrefix + LibName;

        AddInclude(CodeInsight);
        AddIncludeBuffer(Buf);

        CodeInsight.DecRef();
      end;

      Exit(True);
    end;
  finally
    IncludeBufferCS.Release();
  end;
end;

procedure TCodeInsight.OnInclude(Sender: TmwBasePasLex);
var
  Param: string;
  i, p: Integer;
begin
  Param := Sender.DirectiveParamOriginal;
  {$IFDEF FPC}
  Param := SetDirSeparators(param);
  {$ELSE}
  Param := StringReplace(Param, '/', '\', [rfReplaceAll]);
  {$ENDIF}
  if (not Sender.IsJunk) and (Param <> '') then
  begin
    p := Pos('loadlib', LowerCase(Sender.Token));
    if (p > 0) and (p <= 3) then
    begin
      if LoadLibrary(Param) then
        Param := '';
    end
    else if FindInclude(Param) then
    begin
      p := Pos('include_once', LowerCase(Sender.Token));
      if (p > 0) and (p <= 3) then
        for i := High(FIncludes) downto 0 do
          if (FIncludes[i].FileName = Param) then
          begin
            Param := '';
            Break;
          end;

      if (Param <> '') then
      begin
        ParseInclude(Param);
        Param := '';
      end;
    end;
  end;

  inherited;
end;

function TCodeInsight.GetLibrary: Boolean;
begin
  Result := Copy(FFileName, 1, 4) = 'lib:';
end;

function TCodeInsight.GetIncludesHash: String;
var
  i: Int32;
  Files: String = '';
begin
  for i := 0 to High(FIncludes) do
    Files := Files + FIncludes[i].FileName;

  Result := MD5Print(MD5String(Files));
end;

procedure TCodeInsight.SetPos(Value: Integer);
begin
  if (FPos = Value) then
    Exit;

  Reset;
  FPos := Value;
  Init;
end;

procedure TCodeInsight.Init;
var
  Declarations: TDeclarationArray;
  i: Int32;
begin
  if (FPos > -1) then
    FDeclarationAtPos := fItems.GetItemInPos(FPos, FPos, True);

  if (FDeclarationAtPos <> nil) and (not (FDeclarationAtPos is TciJunk)) then
  begin
    FDeclarationAtPos.HasOwnerClass(TciProcedureDeclaration, TDeclaration(FInMethod), True);

    Declarations := FDeclarationAtPos.GetOwnersOfClass(TciWithStatement);
    for i := 0 to High(Declarations) do
      FInWith := FInWith + Declarations[i].Items.GetItemsOfClass(TciVariable);
  end;
end;

procedure TCodeInsight.Reset;
begin
  Lexer.Init();

  FInMethod := nil;
  FDeclarationAtPos := nil;

  SetLength(FInWith, 0);
end;

function TCodeInsight.ParseExpression(Expr: String; constref Declarations: TDeclarationArray; out Members: TDeclarationArray): TDeclaration;

  function ParseType(Declaration: TDeclaration): TDeclaration;
  begin
    Result := nil;

    if Declaration.Items.GetFirstItemOfClass(TciRecordType) <> nil then
      Result := Declaration.Items.GetFirstItemOfClass(TciRecordType)
    else
    if Declaration.Items.GetFirstItemOfClass(TciArrayType) <> nil then
      Result := Declaration.Items.GetFirstItemOfClass(TciArrayType)
    else
    if Declaration.Items.GetFirstItemOfClass(TciTypeIdentifer) <> nil then
    begin
      Declaration := Declaration.Items.GetFirstItemOfClass(TciTypeIdentifer);

      if Declaration <> nil then
      begin
        Result := getGlobalType(Declaration.CleanText);
        if Result = nil then
          Result := Declaration;
      end;
    end;
  end;

  // Parse array. Get the correct element
  function ParseArray(Declaration: TDeclaration; var Dimensions: Int32): TDeclaration;
  begin
    Result := nil;

    if Declaration is TciTypeDeclaration then
    begin
      Declaration := Declaration.Items.GetFirstItemOfClass(TciTypeKind);
      if Declaration = nil then
        Exit;
      Declaration := Declaration.Items.GetFirstItemOfClass(TciArrayType);
      if Declaration = nil then
        Exit;
    end;

    if Declaration is TciArrayType then
      with Declaration as TciArrayType do
      begin
        Dimensions := Dimensions - GetDimensionCount();
        Result := GetType();
      end;
  end;

  // Type declaration. Find member (field, method) while searching all parents.
  function FindMember(Declaration: TciTypeDeclaration; Identifier: String): TDeclaration;
  var
    i: Int32;
  begin
    Result := nil;

    Members := Self.getGlobalTypeMembers(Declaration, Identifier);

    for i := 0 to High(Members) do
    begin
      if (Members[i] is TciClassField) then
      begin
        Result := TciClassField(Members[i]).VarType;
        if Result <> nil then
          Exit;
      end;

      if (Members[i] is TciProcedureDeclaration) then
      begin
        Result := TciProcedureDeclaration(Members[i]).ReturnType;
        if Result <> nil then
          Exit;
      end;
    end;
  end;

  // Find field on inline record: var r: record a, b, c: Int32; end;
  function FindField(Declaration: TciRecordType; Identifer: String; Dimensions: Int32): TDeclaration;
  var
    i: Int32;
    Hash: UInt32;
  begin
    Result := nil;

    Hash := HashString(UpperCase(Identifer));
    Members := Declaration.Items.GetItemsOfClass(TciClassField);

    for i := 0 to High(Members) do
      if Members[i].IsName(Hash) then
      begin
        Result := TciClassField(Members[i]).VarType;
        if Result <> nil then
          Exit;
      end;
  end;

var
  Strings: TStringArray;
  i: Int32;
  Hash: UInt32;
  Expressions: array of record
    Text: String;
    Dimensions: Int32;
  end;
begin
  Result := nil;

  Strings := SplitExpression(Expr);
  if Length(Strings) = 0 then
    Exit;

  SetLength(Expressions, Length(Strings));

  for i := 0 to High(Strings) do
  begin
    if Strings[i].Contains('[') then
    begin
      Expressions[i].Dimensions := Strings[i].CountChar('[') + Strings[i].CountChar(',');
      Expressions[i].Text := Copy(Strings[i], 1, Strings[i].IndexOf('['));
    end else
      Expressions[i].Text := Strings[i];
  end;

  Hash := HashString(UpperCase(Expressions[0].Text));
  for i := 0 to High(Declarations) do
  begin
    if Declarations[i].IsName(Hash) then
    begin
      Result := Declarations[i];
      Break;
    end;
  end;

  if Result = nil then
    Exit;

  for i := 0 to High(Expressions) do
  begin
    if Result is TciProcedureDeclaration then // inline method: var x: function: Int32;
      Result := TciProcedureDeclaration(Result).ReturnType
    else
    if Result is TciProcedureClassName then // Self
      Result := getGlobalType(Result.CleanText)
    else
    if Result is TciVarDeclaration then
      Result := TciVarDeclaration(Result).VarType
    else
    if Result is TciTypeDeclaration and (i > 0) then
      Result := FindMember(Result as TciTypeDeclaration, Expressions[i].Text)
    else
    if Result is TciRecordType and (i > 0) then
      Result := FindField(Result as TciRecordType, Expressions[i].Text, Expressions[i].Dimensions);

    if Result = nil then
      Exit;

    if (Result is TciTypeKind) then
    begin
      Result := ParseType(Result as TciTypeKind);
      if Result = nil then
        Exit;
    end;

    while (Expressions[i].Dimensions > 0) do
    begin
      Result := ParseArray(Result, Expressions[i].Dimensions);
      if Result = nil then
        Exit;

      Result := ParseType(Result as TciTypeKind);
      if Result = nil then
        Exit;
    end;
  end;
end;

function TCodeInsight.ParseExpression(Expr: String; out Members: TDeclarationArray): TDeclaration;
begin
  Result := ParseExpression(Expr, getLocals() + getGlobals(), Members);
end;

function TCodeInsight.ParseExpression(Expr: String): TDeclaration;
var
  Members: TDeclarationArray;
begin
  Result := ParseExpression(Expr, Members);
end;

procedure TCodeInsight.Search(Callback: TCodeInsightSearch);
var
  List: TSimbaList_String;

  procedure Searching(Parser: TCodeInsight);
  begin
    if List.Add(Parser.FileName) > -1 then
      Callback(Parser);

    for Parser in Parser.Includes do
      if List.Add(Parser.FileName) > -1 then
        Callback(Parser);
  end;

var
  Parser: TCodeInsight;
begin
  List := TSimbaStringList.Create('', dupIgnore, False, True);

  try
    for Parser in CoreBuffer do
      Searching(Parser);

    Searching(Self);
  finally
    List.Free();
  end;
end;

function TCodeInsight.getGlobalTypeMembers(Declaration: TciTypeDeclaration): TDeclarationArray;
var
  Name: String;

  procedure GetMethods(Parser: TCodeInsight);
  begin
    Result := Result + Parser.TypeMethods.ItemsOfKey(Name);
  end;

begin
  Result := nil;

  while (Declaration <> nil) do
  begin
    Name := Declaration.Name;
    if Name <> '' then
      Search(@GetMethods);

    Result := Result + Declaration.GetFields();
    Result := Result + Declaration.GetEnumElements();

    Declaration := getGlobalType(Declaration.GetParent);
  end;
end;

function TCodeInsight.getGlobalTypeMembers(Declaration: TciTypeDeclaration; Name: String): TDeclarationArray;
var
  Declarations: TDeclarationArray;
  Hash: UInt32;
  i: Int32;
begin
  Result := nil;

  Hash := HashString(UpperCase(Name));
  Declarations := getGlobalTypeMembers(Declaration);

  for i := 0 to High(Declarations) do
    if Declarations[i].IsName(Hash) then
      Result := Result + Declarations[i];
end;

function TCodeInsight.getDeclarations(Name: String): TDeclarationArray;
var
  List: TDeclarationList;
  Declarations: TDeclarationArray;
  Hash: UInt32;
  i: Int32;
begin
  List := TDeclarationList.Create(False);
  Hash := HashString(UpperCase(Name));

  try
    Declarations := getLocals();

    for i := 0 to High(Declarations) do
      if Declarations[i].IsName(Hash) then
        List.Add(Declarations[i]);

    Declarations := getGlobals();

    for i := 0 to High(Declarations) do
    begin
      if (Declarations[i] is TciProcedureDeclaration) and TciProcedureDeclaration(Declarations[i]).IsMethodOfType then
        Continue;

      if Declarations[i].IsName(Hash) then
        List.Add(Declarations[i]);
    end;

    Result := List.ExportToArray();
  finally
    List.Free();
  end;
end;

function TCodeInsight.getGlobalType(Name: String): TciTypeDeclaration;

  procedure GetType(Parser: TCodeInsight);
  var
    Declaration: TDeclaration;
  begin
    if Result = nil then
    begin
      Declaration := Parser.Types[Name];
      if Declaration <> nil then
        Result := Declaration as TciTypeDeclaration;
    end;
  end;

begin
  Result := nil;
  if Name = '' then
    Exit;

  Search(@GetType);
end;

function TCodeInsight.getGlobals: TDeclarationArray;
var
  List: TDeclarationList;

  procedure Callback(Parser: TCodeInsight);
  var
    i: Int32;
  begin
    for i := 0 to Parser.Items.Count - 1 do
    begin
      if (not Parser.Items[i].IsVisible()) then
        Continue;

      if (Parser.Items[i] is TciTypeDeclaration) then
        with Parser.Items[i] as TciTypeDeclaration do
          List.Extend(GetEnumElements());

      List.Add(Parser.Items[i]);
    end;
  end;

begin
  List := TDeclarationList.Create(False);

  try
    Search(@Callback);
  finally
    Result := List.ExportToArray;

    List.Free();
  end;
end;

function TCodeInsight.getLocals: TDeclarationArray;
var
  Method: TciProcedureDeclaration;
  Declaration, Expr: TDeclaration;
  Globals, Members: TDeclarationArray;
begin
  Result := nil;

  Globals := getGlobals();
  Method := FInMethod;

  while Method <> nil do
  begin
    if tokStatic in Method.Directives then
      Break;

    Result := Result + Method.Items.GetItemsOfClass(TciVarDeclaration);
    Result := Result + Method.Items.GetItemsOfClass(TciTypeDeclaration);
    Result := Result + Method.Items.GetItemsOfClass(TciProcedureDeclaration);
    Result := Result + Method.Items.GetItemsOfClass(TciReturnType);
    Result := Result + Method.Items.GetItemsOfClass(TciProcedureClassName);
    Result := Result + Method.GetParamDeclarations();

    Method.HasOwnerClass(TciProcedureDeclaration, TDeclaration(Method));
  end;

  for Declaration in Result do
  begin
    if Declaration is TciTypeDeclaration then
      Result := Result + TciTypeDeclaration(Declaration).GetEnumElements()
    else
    if Declaration is TciProcedureClassName then
      FInWith := FInWith + Declaration;
  end;

  for Declaration in FInWith do
  begin
    Expr := ParseExpression(Declaration.CleanText, Globals + Result, Members);
    if (Expr <> nil) and (Expr is TciTypeDeclaration) then
      Result := Result + getGlobalTypeMembers(Expr as TciTypeDeclaration);
  end;
end;

constructor TCodeInsight.Create(AFileName: String);
begin
  inherited Create();

  FFileName := AFileName;

  FOnFindInclude := nil;
  FOnLoadLibrary := nil;

  FRefCount := 1;
  FPos := -1;

  Reset();

  Lexer.Defines.AddStrings(CoreDefines);
 // if (not SimbaSettings.CodeInsight.ShowHidden.GetDefValue(False)) then
 //   Lexer.Defines.Add('CODEINSIGHT');

  FOwnStream := FileExists(FFileName);
  if FOwnStream then
  begin
    FMemoryStream := TMemoryStream.Create;
    with TStringList.Create do
    begin
      LoadFromFile(AFileName);  //Converts the line-endings.
      SaveToStream(FMemoryStream);
      Free;
    end;
  end
  else
    FMemoryStream := nil;
end;

destructor TCodeInsight.Destroy;
begin
  if InterlockedCompareExchange(FRefCount, 0, 1) <> 1 then
    raise Exception.Create('Trying to destroy TCodeInsight with refcount > 1');

  FreeIncludes();
  if FOwnStream then
    FreeAndNil(FMemoryStream);

  inherited;
end;

procedure TCodeInsight.Assign(From: TObject);
begin
  if (From is TCodeInsight) then
    with From as TCodeInsight do
    begin
      Self.OnFindInclude := OnFindInclude;
      Self.OnLoadLibrary := OnLoadLibrary;
    end;

  inherited;
end;

procedure TCodeInsight.AddRef;
begin
  InterLockedIncrement(FRefCount);
end;

procedure TCodeInsight.DecRef;
begin
  if (InterLockedDecrement(FRefCount) = 0) then
  begin
    AddRef();
    Free();
  end;
end;

procedure TCodeInsight.Run(SourceStream: TCustomMemoryStream; ManageStream: Boolean; MaxPos: Integer);
begin
  FreeIncludes();

  if ManageStream then
  begin
    if (SourceStream <> nil) then
    begin
      if FOwnStream then
        FreeAndNil(FMemoryStream);
      FMemoryStream := TMemoryStream(SourceStream);
    end;
    FOwnStream := True;
  end;

  if FOwnStream then
    inherited Run(FMemoryStream, MaxPos - 1)
  else if Assigned(SourceStream) then
    inherited Run(SourceStream, MaxPos - 1);

  if (MaxPos > -1) then
    Position := MaxPos;
end;

procedure TCodeInsight.Run(const Source: String; MaxPos: Integer);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create();
  if Length(Source) > 0 then
    Stream.Write(Source[1], Length(Source));

  Run(Stream, True, MaxPos);
end;

initialization
  CoreDefines := TStringList.Create();
  IncludeBufferCS := TCriticalSection.Create();

finalization
  ClearCoreBuffer;
  CoreDefines.Free();

  ClearIncludeBuffer;
  IncludeBufferCS.Free()

end.
