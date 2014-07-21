unit v_ideCodeInsight;

{$include ValistusDefines.inc}

interface

uses
  SysUtils, Classes,
  CastaliaPasLex,
  CastaliaPasLexTypes,
  v_ideCodeParser;

type
  TCodeInsight = class;
  TCodeInsightArray = array of TCodeInsight;

  TOnFindInclude = function(Sender: TObject; var FileName: string): Boolean of object;
  TOnLoadLibrary = function(Sender: TObject; var FileName: string; out ci: TCodeInsight): Boolean of object;

  { TCodeInsight }

  TCodeInsight = class(TCodeParser)
  protected
    fFileName: string;
    fMemoryStream: TMemoryStream;
    fOwnStream: Boolean;

    fPos: Integer;
    fDeclarationAtPos: TDeclaration;

    fOnFindInclude: TOnFindInclude;
    fOnLoadLibrary: TOnLoadLibrary;
    fIncludes: TCodeInsightArray;

    InFunc: TDeclarationArray;
    InWith: TDeclarationArray;
    InClassFunction: Integer;

    Proposal_Filled: Boolean;
    Proposal_ItemList: TStrings;
    Proposal_InsertList: TStrings;

    procedure SetPos(APos: Integer);

    procedure Reset;
    procedure Init;

    function FindInclude(var FileName: string): Boolean;
    procedure ParseInclude(FileName: string);
    function LoadLibrary(var LibName: string): Boolean;
    procedure OnInclude(Sender: TmwBasePasLex); override;

    function GetVarType(s: string; out Decl: TDeclaration; Return: TVarBase): Boolean;
    function GetFuncType(FuncName, FuncClass: string; out Decl: TDeclaration; Return: TVarBase): Boolean;
    function FindStruct(s: string; out Decl: TDeclaration; Return: TVarBase; var ArrayCount: Integer): Boolean;
  public
    function GetExpressionAtPos(var BraceCount, BracketCount, CommaCount: Integer; out sp, ssp, bp: Integer; IgnoreBrackets: Boolean = False): string; overload;
    function GetExpressionAtPos(var BraceCount, BracketCount, CommaCount: Integer; IgnoreBrackets: Boolean = False): string; overload;
    function GetExpressionAtPos: string; overload;
    function FindVarBase(s: string; GetStruct: Boolean = False; Return: TVarBase = vbName): TDeclaration;

    constructor Create(FileName: string = ''); reintroduce;
    destructor Destroy; override;
    procedure Assign(From: TObject); override;
    procedure Run(SourceStream: TCustomMemoryStream = nil; BaseDefines: TStringList = nil; MaxPos: Integer = -1; ManageStream: Boolean = False); reintroduce;

    procedure Proposal_AddDeclaration(Item: TDeclaration; ItemList, InsertList: TStrings; ShowTypeMethods: Boolean = False);
    procedure GetProcedures(Headers, Names: TStrings; FindTypeProcs: Boolean = False);
    function GetTypeProcs(Names: TStrings; const Prefix: string): TDeclarationArray;
    function FindProcedure(ProcNameToFind: string; out Decl: TDeclaration; out HasParams: Boolean): boolean;
    procedure FillProposal;
    procedure FillSynCompletionProposal(ItemList, InsertList: TStrings; Prefix: string = '');

    property OnFindInclude: TOnFindInclude read fOnFindInclude write fOnFindInclude;
    property OnLoadLibrary: TOnLoadLibrary read fOnLoadLibrary write fOnLoadLibrary;
    property FileName: string read fFileName write fFileName;
    property Position: Integer read fPos write SetPos;
    property DeclarationAtPos: TDeclaration read fDeclarationAtPos;
    property Includes: TCodeInsightArray read fIncludes;
  end;

  TIncludeBuffer = record
    Script: string;
    DefinesIn, DefinesOut: TSaveDefinesRec;
    LastChanged: Integer;
    CodeInsight: TCodeInsight;
  end;
  TIncludeBufferArray = array of TIncludeBuffer;

const
  LibPrefix = 'lib:';

var
  CoreBuffer: TCodeInsightArray;
  IncludeBuffer: TIncludeBufferArray;
  CoreDefines: TStringList;

implementation

uses
  v_Constants, v_MiscFunctions, newsimbasettings, mufasabase;

procedure ClearCoreBuffer;
var
  i: Integer;
begin
  for i := 0 to High(CoreBuffer) do
    FreeAndNil(CoreBuffer[i]);
  SetLength(IncludeBuffer, 0);
end;

procedure DeleteIncludeBufferIndex(Index: Integer);
var
  i: Integer;
  tmp : TCodeInsight;
begin
  tmp := IncludeBuffer[Index].CodeInsight;
  for i := Index to High(IncludeBuffer) - 1 do
    IncludeBuffer[i] := IncludeBuffer[i + 1];
  SetLength(IncludeBuffer, Length(IncludeBuffer) - 1);
  tmp.free;
end;

procedure ClearIncludeBuffer;
var
  i: Integer;
begin
  for i := 0 to High(IncludeBuffer) do
    IncludeBuffer[i].CodeInsight.Free;

  SetLength(IncludeBuffer, 0);
end;

function GetIncludeBuffer(FileName: string; ci: TCodeInsight): TIncludeBuffer;
var
  i, l, lc: Integer;
  Defines: TSaveDefinesRec;
  DefineMatch: Boolean;
  NewBuf : TIncludeBuffer;
  CS : TRTLCriticalSection;
begin
  lc := 1;//FileAge(FileName);
  Defines := ci.Lexer.SaveDefines;
  FileName := Trim(FileName);

  for i := Length(IncludeBuffer) - 1 downto 0 do
  begin
    if (IncludeBuffer[i].CodeInsight <> nil) and (IncludeBuffer[i].CodeInsight.FileName = FileName) then
    begin
      DefineMatch := (IncludeBuffer[i].DefinesIn.Defines = Defines.Defines) and (IncludeBuffer[i].DefinesIn.Stack = Defines.Stack);

      if (ci.FileName = IncludeBuffer[i].Script) then
      begin
        if (DefineMatch) and (IncludeBuffer[i].LastChanged = lc) then
        begin
          ci.Lexer.LoadDefines(IncludeBuffer[i].DefinesOut);
          Result := IncludeBuffer[i];
          Exit;
        end;

        DeleteIncludeBufferIndex(i);
        Break;
      end;
    end;
  end;

  with NewBuf do
  begin
    Script := ci.FileName;
    DefinesIn := Defines;
    LastChanged := lc;
    CodeInsight := TCodeInsight.Create(FileName);

    with CodeInsight do
    begin
      Assign(ci);

      if (ci.Lexer.Defines.IndexOf('IS_INCLUDE') < 0) then
        i := ci.Lexer.Defines.Add('IS_INCLUDE')
      else
        i := -1;

      Run;

      if (i > -1) then
      begin
        i := ci.Lexer.Defines.IndexOf('IS_INCLUDE');

        if (i > -1) then
          ci.Lexer.Defines.Delete(i);
      end;

      ci.Lexer.CloneDefinesFrom(Lexer);
    end;
  end;

  InitCriticalSection(cs);
  EnterCriticalsection(cs);

  try
    l := Length(IncludeBuffer);
    SetLength(IncludeBuffer, l + 1);
    IncludeBuffer[l] := NewBuf;
  finally
    LeaveCriticalsection(cs);
  end;

  DoneCriticalsection(cs);

  IncludeBuffer[l].DefinesOut := IncludeBuffer[l].CodeInsight.Lexer.SaveDefines;
  Result := IncludeBuffer[l];
end;

function TCodeInsight.FindInclude(var FileName: string): Boolean;
var
  s: string;
begin
  s := FileName;
  if Assigned(OnFindInclude) and OnFindInclude(Self, s) then
  begin
    FileName := ExpandFileName(s);
    Exit(True);
  end;

  s := ExtractFilePath(fFileName);
  if (s <> '') and FileExists(s + FileName) then
  begin
    FileName := ExpandFileName(s + FileName);
    Exit(True);
  end;

  {s := ExtractFilePath(ParamStr(0));
  if (s <> '') and FileExists(s + FileName) then
  begin
    FileName := s + FileName;
    Exit(True);
  end;

  Result := FileExists(FileName);}

  Result := False;
end;

procedure TCodeInsight.ParseInclude(FileName: string);
var
  L: LongInt;
begin
  L := Length(fIncludes);
  SetLength(fIncludes, L + 1);
  fIncludes[L] := GetIncludeBuffer(FileName, Self).CodeInsight;
end;

function TCodeInsight.LoadLibrary(var LibName: string): Boolean;
var
  i: Integer;
  s: string;
  ci: TCodeInsight;
  tmp : TIncludeBuffer;
  CS : TRTLCriticalSection;
begin
  Result := False;

  for i := High(fIncludes) downto 0 do
    if (fIncludes[i].FileName = LibPrefix+LibName) then
      Exit(True);

  for i := High(IncludeBuffer) downto 0 do
    if (IncludeBuffer[i].Script = LibPrefix+LibName) then
    begin
      SetLength(fIncludes, Length(fIncludes) + 1);
      fIncludes[High(fIncludes)] := IncludeBuffer[i].CodeInsight;

      Exit(True);
    end;

  s := LibName;
  if Assigned(OnLoadLibrary) and OnLoadLibrary(Self, s, ci) and (ci <> nil) then
  begin
    LibName := s;

    SetLength(fIncludes, Length(fIncludes) + 1);
    fIncludes[High(fIncludes)] := ci;

    with tmp do
    begin
      Script := LibPrefix+LibName;
      CodeInsight := ci;
      CodeInsight.FileName := LibPrefix+LibName;
    end;

    InitCriticalSection(cs);
    EnterCriticalsection(cs);
    try
      SetLength(IncludeBuffer, Length(IncludeBuffer) + 1);
      IncludeBuffer[High(IncludeBuffer)] := tmp;
    finally
      LeaveCriticalsection(cs);
    end;
    DoneCriticalsection(cs);
    Exit(True);
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
        for i := High(fIncludes) downto 0 do
          if (fIncludes[i].FileName = Param) then
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

    if (Param <> '') and Assigned(OnMessage) then
      OnMessage(Self, meError, Format(ci_UnknownInclude, [Param]), Sender.PosXY.X, Sender.PosXY.Y);
  end;

  inherited;
end;

procedure TCodeInsight.SetPos(APos: Integer);
begin
  if (fPos = APos) then
    Exit;
  Reset;
  fPos := APos;
  Init;
end;

procedure TCodeInsight.Init;
var
  a, b: TDeclarationArray;
  d: TDeclaration;
  i, ii: Integer;
  s: string;
begin
  if (fPos >= 0) then
    fDeclarationAtPos := fItems.GetItemInPos(fPos, fPos, True)
  else
    fDeclarationAtPos  := nil;

  (*if (fDeclarationatPos is TciJunk) and (fDeclarationatPos.Owner <> nil) {and (fDeclarationatPos.EndPos = fPos)} then
    fDeclarationAtPos := fDeclarationatPos.Owner;*)

  if (fDeclarationAtPos <> nil) and (not (fDeclarationAtPos is TciJunk)) then
  begin
    InFunc := fDeclarationAtPos.GetOwnersOfClass(TciProcedureDeclaration);
    if (fDeclarationAtPos is TciProcedureDeclaration) then
    begin
      SetLength(InFunc, Length(InFunc) + 1);
      InFunc[High(InFunc)] := fDeclarationAtPos;
    end;

    if (Length(InFunc) > 0) and (InFunc[0].Owner = nil) then
    begin
      d := InFunc[0].Items.GetFirstItemOfClass(TciProcedureClassName);
      if (d <> nil) then
        d := FindVarBase(d.CleanText, True, vbType);
      if (d <> nil) and (d is TciStruct) then
      begin
        SetLength(InWith, Length(InWith) + 1);
        InClassFunction := High(InWith);
        InWith[InClassFunction] := d;
      end;
    end;

    a := fDeclarationAtPos.GetOwnersOfClass(TciWithStatement);
    if (fDeclarationAtPos is TciWithStateMent) then
    begin
      SetLength(a, Length(a) + 1);
      a[High(a)] := fDeclarationAtPos;
    end
    else if fDeclarationAtPos.HasOwnerClass(TciClassType, d, True) then
    begin
      SetLength(InWith, Length(InWith) + 1);
      InWith[High(InWith)] := d;
    end;

    for i := High(a) downto Low(a) do
    begin
      b := a[i].Items.GetItemsOfClass(TciVariable);
      for ii := Low(b) to High(b) do
      begin
        if (fDeclarationAtPos is TciVariable) and (b[ii].EndPos >= fPos) then
          Continue;
        s := b[ii].CleanText;
        d := FindVarBase(s, True, vbType);
        if (d <> nil) and (d is TciStruct) then
        begin
          SetLength(InWith, Length(InWith) + 1);
          InWith[High(InWith)] := d;
        end;
      end;
    end;
  end;
end;

procedure TCodeInsight.Reset;
begin
  Lexer.Init;

  //SetLength(fIncludes, 0);
  SetLength(InFunc, 0);
  SetLength(InWith, 0);
  InClassFunction := -1;

  Proposal_ItemList.Clear;
  Proposal_InsertList.Clear;
  Proposal_Filled := False;

  fDeclarationAtPos := nil;
end;

function TCodeInsight.GetVarType(s: string; out Decl: TDeclaration; Return: TVarBase): Boolean;
var
  a, b: TDeclarationArray;
  i, ii, iii, iiii, FuncOffset, SetOffset: Integer;
  c: array [1..3] of TDeclarationClass;
  d: TDeclaration;
begin
  Result := False;
  FuncOffset := 0;
  SetOffset := 0;

  if (s = 'RESULT') and (Length(InFunc) > 0) then
  begin
    Decl := InFunc[Low(InFunc)].Items.GetFirstItemOfClass(TciReturnType);
    if (Decl <> nil) then
    begin
      Result := True;
      Exit;
    end;
  end;

  if (s = 'SELF') and (InClassFunction <> -1) and InWith[InClassFunction].HasOwnerClass(TciTypeDeclaration, d, True) then
  begin
    Decl := d.Items.GetFirstItemOfClass(TciTypeName);
    if (Decl <> nil) then
    begin
      Result := True;
      Exit;
    end;
  end;

  for i := Low(InFunc) to High(InFunc) + 1 do
  begin
    for iiii := 1 to 3 do
    begin
      case iiii of
        1:
          begin
            c[1] := TciVarDeclaration;
            c[2] := TciVarName;
            c[3] := TciTypeKind;
          end;
        2:
          begin
            if (Return = vbType) then
              Continue;
            c[1] := TciConstantDeclaration;
            c[2] := TciConstantName;
            c[3] := TciExpression;
          end;
        3:
          begin
            if (Return = vbType) then
              Continue;
            c[1] := TciLabelDeclaration;
            c[2] := TciLabelName;
            c[3] := nil;
          end;
        else
          Break;
      end;
     if (i = High(InFunc) + 1) then
        a := fItems.GetItemsOfClass(c[1])
      else
        a := InFunc[i].Items.GetItemsOfClass(c[1]);
      for ii := Low(a) to High(a) do
      begin
        b := a[ii].Items.GetItemsOfClass(c[2]);
        for iii := Low(b) to High(b) do
        begin
          if (PrepareString(b[iii].CleanText) = s) then
          begin
            Result := True;
            if (Return = vbType) and (c[3] <> nil) then
              Decl := a[ii].Items.GetFirstItemOfClass(c[3])
            else
              Decl := b[iii];
            Exit;
          end;
        end;
      end;
    end;

    if (Return = vbName) then
    begin
      if (i = High(InFunc) + 1) then
        a := fItems.GetItemsOfClass(TciQualifiedIdentifier, True)
      else
        a := InFunc[i].Items.GetItemsOfClass(TciQualifiedIdentifier, True);
      for ii := Low(a) to High(a) - SetOffset do
      begin
        if (i = High(InFunc) + 1) then
          if a[ii].HasOwnerClass(TciTypeDeclaration, d, True) then
          begin
            if d.HasOwnerClass(TciProcedureDeclaration, d, True) then
              Continue;
          end
          else if a[ii].HasOwnerClass(TciProcedureDeclaration, d, True) and (d.Owner <> nil) then
            Continue;

        if (PrepareString(a[ii].CleanText) = s) then
        begin
          Result := True;
          Decl := a[ii];
          Exit;
        end;
      end;
      SetOffset := Length(a);
    end;

    if (i <= High(InFunc)) then
    begin
      a := InFunc[i].Items.GetItemsOfClass(TciParameterName, True);
      for ii := Low(a) to High(a) - FuncOffset do
        if (PrepareString(a[ii].CleanText) = s) then
        begin
          Result := True;
          if (Return = vbType) then
            Decl := a[ii].Owner.Items.GetFirstItemOfClass(TciParameterType)
           else
            Decl := a[ii];
          Exit;
        end;
      FuncOffset := Length(a);
    end;
  end;
end;

function TCodeInsight.GetFuncType(FuncName, FuncClass: string; out Decl: TDeclaration; Return: TVarBase): Boolean;
var
  a: TDeclarationArray;
  b: TDeclaration;
  i, ii: Integer;
begin
  Result := False;

  for ii := Low(InFunc) to High(InFunc) + 1 do
  begin
    if (ii = High(InFunc) + 1) then
      a := fItems.GetItemsOfClass(TciProcedureDeclaration)
    else
      a := InFunc[ii].Items.GetItemsOfClass(TciProcedureDeclaration);
   for i := Low(a) to High(a) do
    begin
      b := nil;
      if (FuncClass <> '') then
        b := a[i].Items.GetFirstItemOfClass(TciProcedureClassName);
      if ((FuncClass = '') and (b = nil)) or ((b <> nil) and (PrepareString(b.CleanText) = FuncClass)) then
      begin
        b := a[i].Items.GetFirstItemOfClass(TciProcedureName);
        if (b <> nil) and (PrepareString(b.CleanText) = FuncName) then
        begin
          Result := True;
          if (Return = vbType) then
            Decl := a[i].Items.GetFirstItemOfClass(TciReturnType)
          else
            Decl := b;

          if (a[i].Items.GetFirstItemOfClass(TciForward) = nil) then
            Exit;
        end;
      end;
    end;
  end;
end;

function TCodeInsight.FindStruct(s: string; out Decl: TDeclaration; Return: TVarBase; var ArrayCount: Integer): Boolean;
var
  a, b: TDeclarationArray;
  i, ii: Integer;
begin
  Result := False;
  s := PrepareString(s);
  a := fItems.GetItemsOfClass(TciTypeDeclaration);

  for i := Low(a) to High(a) do
  begin
    b := a[i].Items.GetItemsOfClass(TciTypeName);
    for ii := Low(b) to High(b) do
    begin
      if (PrepareString(b[ii].CleanText) = s) then
      begin
        Result := True;
        if (Return = vbType) then
        begin
          Decl := a[i].Items.GetFirstItemOfClass(TciTypeKind);
          if (Decl = nil) then
            Decl := a[i].Items.GetFirstItemOfClass(TciClassType);
          if (Decl is TciTypeKind) then
            Decl := TciTypeKind(Decl).GetRealType(ArrayCount);

          if (Decl is TciStruct) then
            Result := True
          else
            Result := (Decl <> nil) and (PrepareString(Decl.CleanText) <> s) and FindStruct(Decl.CleanText, Decl, Return, ArrayCount);
        end
        else
          Decl := b[ii];

        if (Decl <> nil) then
          Exit;
      end;
    end;
  end;
end;

function TCodeInsight.GetExpressionAtPos(var BraceCount, BracketCount, CommaCount: Integer; out sp, ssp, bp: Integer; IgnoreBrackets: Boolean): string;
var
  i, StartPos, EndPos, Dif: Integer;
  s: string;
  a: TDeclarationArray;
  d: TDeclaration;
  LastWasDot: Boolean;
begin
  Result := '';
  d := nil;
  sp := -1;
  ssp := -1;
  bp := -1;
  if (fDeclarationAtPos = nil) or
     (fDeclarationAtPos is TciJunk) or
     (not (
       (fDeclarationAtPos.HasOwnerClass(TciCompoundStatement, d, True) or (fDeclarationAtPos is TciCompoundStatement)) or
       (fDeclarationAtPos.HasOwnerClass(TciConstantDeclaration, d, True) or (fDeclarationAtPos is TciConstantDeclaration)) or
       (fDeclarationAtPos.HasOwnerClass(TciVarDeclaration, d, True) or (fDeclarationAtPos is TciVarDeclaration)) or
       (fDeclarationAtPos.HasOwnerClass(TciLabelDeclaration, d, True) or (fDeclarationAtPos is TciLabelDeclaration)) or
       (fDeclarationAtPos.HasOwnerClass(TciTypeDeclaration, d, True) or (fDeclarationAtPos is TciTypeDeclaration)) or
       (fDeclarationAtPos.HasOwnerClass(TciProcedureDeclaration, d, True) or (fDeclarationAtPos is TciProcedureDeclaration))
     )) then
    Exit;
  if (d = nil) then
    d := fDeclarationAtPos;
  a := d.Items.GetItemsOfClass(TciJunk, True);
  EndPos := fPos - d.StartPos;
  s := d.CleanText;

  Dif := EndPos;
  for i := Low(a) to High(a) do
    if (fPos > a[i].EndPos) then
      if (Pos(LineEnding, a[i].RawText) > 0) then
        EndPos := EndPos - a[i].EndPos + a[i].StartPos + Length(LineEnding)
      else
        EndPos := EndPos - a[i].EndPos + a[i].StartPos + 1;
  Dif := Dif - EndPos;

  StartPos := EndPos;
  LastWasDot := False;
  if (StartPos <= Length(s)) then
    while (StartPos > 0) do
    begin
      if (BraceCount = 0) and ((BracketCount = 0) or IgnoreBrackets) and (s[StartPos] in ['a'..'z', 'A'..'Z', '0'..'9', '_']) then
        {nothing}
      else if (BraceCount = 0) and ((BracketCount = 0) or IgnoreBrackets) and (s[StartPos] in [#10, #11, #13, #32]) then
      begin
        i := StartPos;
        Dec(StartPos);
        while (StartPos > 0) and (s[StartPos] in [#10, #11, #13, #32]) do
          Dec(StartPos);
        if (StartPos > 0) and (not ((LastWasDot and (s[StartPos] in ['a'..'z', 'A'..'Z', '0'..'9', '_', ']', ')'])) or ((not LastWasDot) and (s[StartPos] = '.')))) then
        begin
          StartPos := i - BracketCount - BraceCount;
          if (ssp = -1) then ssp := StartPos;
          Break;
        end;
        Inc(StartPos);
      end
      else if (s[StartPos] = '.') then
      begin
        if (ssp = -1) then ssp := StartPos;
        LastWasDot := True;
        Dec(StartPos);
        Continue;
      end
      else if (s[StartPos] = ']') then
        Inc(BracketCount)
      else if (s[StartPos] = '[') then
      begin
        Dec(BracketCount);
        LastWasDot := True;
        Dec(StartPos);
        Continue;
      end
      else if (s[StartPos] = ')') then
        Inc(BraceCount)
      else if (s[StartPos] = '(') then
      begin
        Dec(BraceCount);
        LastWasDot := True;
        Dec(StartPos);
        if (BraceCount = 0) then
          bp := StartPos;
        Continue;
      end
      else if (BraceCount = 1) and (BracketCount = 0) and (s[StartPos] = ',')  then
        Inc(CommaCount)
      else if (BraceCount = 0) and ((BracketCount = 0) or IgnoreBrackets) then
        Break;

      if (BraceCount < 0) or ((BracketCount < 0) and (not IgnoreBrackets)) then
      begin
        Dec(StartPos, BraceCount);
        Dec(StartPos, BracketCount);
        Break;
      end;

      LastWasDot := False;
      Dec(StartPos);
    end;
  if (ssp = -1) then ssp := StartPos;
  if (bp = -1) then bp := EndPos;
  sp := StartPos + d.StartPos + Dif + 1;
  ssp := ssp + d.StartPos + Dif + 1;
  bp := bp + d.StartPos + Dif + 1;
  if (EndPos > Length(s)) then
    Result := ''
  else
    Result := CompressWhiteSpace(Copy(s, StartPos + 1, EndPos - StartPos));
end;

function TCodeInsight.GetExpressionAtPos(var BraceCount, BracketCount, CommaCount: Integer; IgnoreBrackets: Boolean = False): string;
var
  sp, ssp, bp: integer;
begin
  result := GetExpressionAtPos(bracecount,bracketcount,commacount,sp,ssp,bp,ignorebrackets);
end;

function TCodeInsight.GetExpressionAtPos: string;
var
  bcc, bkc, cc: Integer;
begin
  bcc := 0;
  bkc := 0;
  cc := 0;
  Result := GetExpressionAtPos(bcc, bkc, cc);
end;

function TCodeInsight.FindVarBase(s: string; GetStruct: Boolean = False; Return: TVarBase = vbName): TDeclaration;

  function PartOfWith(s: string; out Decl: TDeclaration; Return: TVarBase; CheckClass: Boolean; var ArrayCount: Integer): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := High(InWith) downto Low(InWith) do
      if CheckClass xor (i <> InClassFunction) then
        if TciStruct(InWith[i]).HasField(s, Decl, Return, ArrayCount) then
        begin
          Result := True;
          Break;
        end;
  end;

  function DoGetVarType(s: string; out Decl: TDeclaration; Return: TVarBase): Boolean;

    function CheckIt(s: string; Item: TCodeInsight; out Decl: TDeclaration; Return: TVarBase; const CheckCore: Boolean; var Checked: TCodeInsightArray): Boolean;
    var
      i: Integer;
    begin
      Result := Item.GetVarType(s, Decl, Return);
      if Result then
        Exit();

      for i := High(Checked) downto 0 do
        if (Checked[i] = Item) then
          Exit;

      SetLength(Checked, Length(Checked) + 1);
      Checked[High(Checked)] := Item;

      for i := High(Item.Includes) downto Low(Item.Includes) do
      begin
        Result := CheckIt(s, Item.Includes[i], Decl, Return, False, Checked);
        if Result then
          Exit();
      end;

      if (not CheckCore) then
        Exit;

      for i := High(CoreBuffer) downto Low(CoreBuffer) do
      begin
        Result := CheckIt(s, CoreBuffer[i], Decl, Return, False, Checked);
        if Result then
          Exit();
      end;
    end;
  var
    CheckedArr: TCodeInsightArray;
  begin
    Result := CheckIt(s, Self, Decl, Return, True, CheckedArr);
  end;

  function DoGetFuncType(FuncName, FuncClass: string; out Decl: TDeclaration; Return: TVarBase): Boolean;

    function CheckIt(FuncName, FuncClass: string; Item: TCodeInsight; out Decl: TDeclaration; Return: TVarBase; const CheckCore: Boolean; var Checked: TCodeInsightArray): Boolean;
    var
      i: Integer;
    begin
      Result := False;

      for i := High(Checked) downto 0 do
        if (Checked[i] = Item) then
          Exit;

      SetLength(Checked, Length(Checked) + 1);
      Checked[High(Checked)] := Item;

      if Item.GetFuncType(FuncName, FuncClass, Decl, Return) then
        Exit(True);

      for i := High(Item.Includes) downto Low(Item.Includes) do
        if CheckIt(FuncName, FuncClass, Item.Includes[i], Decl, Return, False, Checked) then
          Exit(True);

      if (not CheckCore) then
        Exit;

      for i := High(CoreBuffer) downto Low(CoreBuffer) do
        if CheckIt(FuncName, FuncClass, CoreBuffer[i], Decl, Return, False, Checked) then
          Exit(True);
    end;
  var
    CheckedArr: TCodeInsightArray;
  begin
    Result := CheckIt(FuncName, FuncClass, Self, Decl, Return, True, CheckedArr);
  end;

    function DoFindStruct(s: string; out Decl: TDeclaration; Return: TVarBase; var ArrayCount: Integer): Boolean;

    function CheckIt(s: string; Item: TCodeInsight; out Decl: TDeclaration; Return: TVarBase; var ArrayCount: Integer; const CheckCore: Boolean; var Checked: TCodeInsightArray): Boolean;
    var
      i: Integer;
    begin
      Result := False;

      for i := High(Checked) downto 0 do
        if (Checked[i] = Item) then
          Exit;

      SetLength(Checked, Length(Checked) + 1);
      Checked[High(Checked)] := Item;
      try
        if Item.FindStruct(s, Decl, Return, ArrayCount) then
          Exit(True);

       for i := High(Item.Includes) downto Low(Item.Includes) do
          if CheckIt(s, Item.Includes[i], Decl, Return, ArrayCount, False, Checked) then
            Exit(True);

        if (not CheckCore) then
          Exit;

        for i := High(CoreBuffer) downto Low(CoreBuffer) do
          if CheckIt(s, CoreBuffer[i], Decl, Return, ArrayCount, False, Checked) then
            Exit(True);
      except
        on e : exception do
          WriteLn('Codeinsight[CheckIt]: ' + e.message);
      end;
    end;
  var
    CheckedArr: TCodeInsightArray;
  begin
    SetLength(CheckedArr, 0);
    Result := CheckIt(s, Self, Decl, Return, ArrayCount, True, CheckedArr);
  end;

var
  f, NameClass: string;
  sa: TStringArray;
  d: TDeclaration;
  i, NeedArrayCount, ArrayCount: Integer;
  InStruct: TciStruct;
  VarBase: TVarBase;
  Found, CheckVar, IsForward: Boolean;
begin
  Result := nil;
  if (fDeclarationAtPos is TciJunk) or (PrepareString(s) = '') then
    Exit;

  InStruct := nil;
  NameClass := '';
  IsForward := False;
  CheckVar := (fPos < 0);
  sa := Explode('.', PrePrepareString(s));

  if (fDeclarationAtPos <> nil) then
  begin
    if (fDeclarationAtPos is TciProcedureName) and (fDeclarationAtPos.Owner <> nil) then
    begin

      if fDeclarationAtPos.HasOwnerClass(TciClassType, d, True) and (d.Owner <> nil) then
      begin
        d := d.Owner.Items.GetFirstItemOfClass(TciTypeName);
        if (d <> nil) then
          NameClass := PrepareString(d.CleanText);
      end
      else if (fDeclarationAtPos.Owner.Items.GetFirstItemOfClass(TciForward) <> nil) then
        IsForward := True;
    end;

    if (Return = vbName) and (
         (fDeclarationAtPos is TciVarName) or
         (fDeclarationAtPos is TciConstantName) or
         (fDeclarationAtPos is TciTypeName) or
         (fDeclarationAtPos is TciLabelName) or
         ((fDeclarationAtPos is TciProcedureName) and
           (NameClass = '') and
           (fDeclarationAtPos.Owner <> nil) and
           (not IsForward) and
           (fDeclarationAtPos.Owner.Items.GetFirstItemOfClass(TciProcedureClassName) = nil)) or
         (fDeclarationAtPos is TciParameterName) or
         (fDeclarationAtPos is TciFieldName) or
         (fDeclarationAtPos is TciQualifiedIdentifier)) then
      Exit;
    CheckVar := (NameClass = '') and ((
      (Length(InFunc) > 0) or
        (Length(InWith) > 0) or
        (fDeclarationAtPos is TciCompoundStatement) or
        (fDeclarationAtPos is TciExpression) or
        fDeclarationAtPos.HasOwnerClass(TciCompoundStatement, d, True)) or (
      (Return = vbType) and (
        (fDeclarationAtPos is TciTypeKind) or
        (fDeclarationAtPos is TciParameterType) or
        (fDeclarationAtPos is TciReturnType) or
        (fDeclarationAtPos is TciQualifiedIdentifier) or
        (fDeclarationAtPos is TciExpression))));
  end;

  for i := Low(sa) to High(sa) do
  begin
    f := PrepareString(sa[i], ArrayCount);
    NeedArrayCount := 0;

    if (Return = vbName) and (i = High(sa)) then
      VarBase := vbName
    else
      VarBase := vbType;

    if (InStruct <> nil) then
      Found := InStruct.HasField(f, Result, VarBase, NeedArrayCount)
    else
    begin
      Found := CheckVar and PartOfWith(f, Result, VarBase, False, NeedArrayCount);
      if (not Found) and (i = Low(sa)) then
      begin
        Found :=
          (CheckVar and DoGetVarType(f, Result, VarBase)) or
          ((CheckVar or (NameClass <> '') or IsForward) and DoGetFuncType(f, NameClass, Result, VarBase)) or
          DoFindStruct(f, Result, VarBase, NeedArrayCount);
      end;
      if (not Found) and CheckVar then
        Found := PartOfWith(f, Result, VarBase, True, NeedArrayCount);
    end;

    if Found and (Result is TciTypeKind) then
      Result := TciTypeKind(Result).GetRealType(NeedArrayCount);

    if Found and (Result <> nil) and (not (Result is TciStruct)) and ((i < High(sa)) or GetStruct) then
      Found := DoFindStruct(Result.CleanText, Result, VarBase, NeedArrayCount);

    if Found and (Result <> nil) and (ArrayCount <> NeedArrayCount) and ((Result is TciStruct) or (InStruct <> nil)) and ((i < High(sa)) or GetStruct) then
    begin
      if (InStruct <> nil) then
        Result := InStruct.GetDefault(VarBase)
      else
        Result := TciStruct(Result).GetDefault(VarBase);

      Found := (Result <> nil);

      if Found and (Result is TciTypeKind) then
        Result := TciTypeKind(Result).GetRealType(NeedArrayCount);

      if Found and (not (Result is TciStruct)) and ((i < High(sa)) or GetStruct) then
        Found := DoFindStruct(Result.CleanText, Result, VarBase, NeedArrayCount);

      if (not Found) then
      begin
        if Assigned(OnMessage) then
          OnMessage(Self, meError, Format(ci_RangeError, [sa[i], s]), -1, -1);
        Result := nil;
        Exit;
      end;
    end;

    if (not Found) or (Result = nil) then
    begin
      if Assigned(OnMessage) then
        if (i > Low(sa)) then
          OnMessage(Self, meError, Format(ci_UnknownMember, [sa[i], s]), -1, -1)
        else
          OnMessage(Self, meError, Format(ci_UnknownStruct, [sa[i], s]), -1, -1);
      Result := nil;
      Exit;
    end;

    InStruct := TciStruct(Result);
  end;
end;

constructor TCodeInsight.Create(FileName: string = '');
begin
  inherited Create;

  Proposal_InsertList := TStringList.Create;
  TStringList(Proposal_InsertList).Sorted := True;
  Proposal_ItemList := TStringList.Create;

  fOnFindInclude := nil;
  fOnLoadLibrary := nil;
  fFileName := FileName;
  if (fFileName <> '') and (not FileExists(fFileName)) then
    fFileName := '';
  fPos := -1;
  Reset;

  Lexer.Defines.AddStrings(CoreDefines);
  if (not SimbaSettings.CodeInsight.ShowHidden.GetDefValue(False)) then
    Lexer.Defines.Add('CODEINSIGHT');

  fOwnStream := (fFileName <> '');
  if fOwnStream then
  begin
    fMemoryStream := TMemoryStream.Create;
    with TStringList.Create do
    begin
      LoadFromFile(filename);  //Converts the line-endings.
      SaveToStream(fMemoryStream);
      Free;
    end;
  end
  else
    fMemoryStream := nil;
end;

destructor TCodeInsight.Destroy;
begin
  if fOwnStream then
    FreeAndNil(fMemoryStream);

  FreeAndNil(Proposal_InsertList);
  FreeAndNil(Proposal_ItemList);

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

procedure TCodeInsight.Run(SourceStream: TCustomMemoryStream = nil; BaseDefines: TStringList = nil; MaxPos: Integer = -1; ManageStream: Boolean = False);
begin
  if Assigned(BaseDefines) then
  begin
    Lexer.ClearDefines;
    Lexer.Defines.AddStrings(CoreDefines);
    Lexer.Defines.AddStrings(BaseDefines);

    if (not SimbaSettings.CodeInsight.ShowHidden.GetDefValue(False)) then
      Lexer.Defines.Add('CODEINSIGHT');
  end;
  SetLength(fIncludes, 0);

  if ManageStream then
  begin
    if (SourceStream <> nil) then
    begin
      if fOwnStream then
        FreeAndNil(fMemoryStream);
      fMemoryStream := TMemoryStream(SourceStream);
    end;
    fOwnStream := True;
  end;

  if fOwnStream then
    inherited Run(fMemoryStream, MaxPos - 1)
  else if Assigned(SourceStream) then
    inherited Run(SourceStream, MaxPos - 1);

  if (MaxPos > -1) then
    Position := MaxPos;
end;

procedure TCodeInsight.Proposal_AddDeclaration(Item: TDeclaration; ItemList, InsertList: TStrings; ShowTypeMethods: Boolean = False);

  function FormatFirstColumn(s: string): string; inline;
  begin
    Result := '{#clNavy}' + LowerCase(s) + '{#0} {|}'
  end;

  function FormatTypeName(s: string): string; inline;
  begin
    Result := '{#clBlue}' + s;
  end;

  function FormatMainName(s: string): string; inline;
  begin
    Result := '{B+}' + s + '{B-}';
  end;

  function FormatMainExtra(s: string): string; inline;
  begin
    Result := '{I+}' + s + '{I-}'
  end;

  function GetEnumTypeName(Item: TDeclaration): string;
  begin
    Result := '';
    if (Item.Items.Count > 0) then
    begin
      Result := Result + '(' + Item.Items[0].ShortText;
      if (Item.Items.Count > 1) then
        Result := Result + '..' + Item.Items[Item.Items.Count - 1].ShortText;
      Result := Result + ')';
    end;
  end;

  procedure AddEnums(Item: {TCodeInsight}TDeclaration; ItemList, InsertList: TStrings); overload;
  var
    {i,} ii, SetOffset: Integer;
    a: TDeclarationArray;
    d: TDeclaration;
    s: string;
  begin
    SetOffset := 0;
    //for i := Low(Item.InFunc) to High(Item.InFunc) + 1 do
    begin
      //if (i = High(Item.InFunc) + 1) then
        a := Item.Items.GetItemsOfClass(TciQualifiedIdentifier, True);
      //else
      //  a := Item.InFunc[i].Items.GetItemsOfClass(TciQualifiedIdentifier, True);
      for ii := Low(a) to High(a) - SetOffset do
      begin
        {if (i = High(Item.InFunc) + 1) then
          if a[ii].HasOwnerClass(TciTypeDeclaration, d, True) then
          begin
            if d.HasOwnerClass(TciProcedureDeclaration, d, True) then
              Continue;
          end
          else if a[ii].HasOwnerClass(TciProcedureDeclaration, d, True) and (d.Owner <> nil) then
            Continue;}

        {$IFDEF ciCHECKDUPLICATES}
        if (InsertList.IndexOf(a[ii].ShortText) > -1) then
          Continue;
        {$ENDIF}

        s := FormatFirstColumn('enum') + FormatMainName(a[ii].ShortText);
        if a[ii].HasOwnerClass(TciTypeDeclaration, d, True) then
        begin
          d := d.Items.GetFirstItemOfClass(TciTypeName);
          if (d <> nil) then
            s := s + ' ' + FormatMainExtra('(' + d.ShortText + ')');
        end;
        ItemList.Insert(InsertList.Add(a[ii].ShortText), s);
      end;
      //SetOffset := Length(a);
    end;
  end;

  procedure AddEnums(a: TDeclarationArray; ItemList, InsertList: TStrings); overload;
  var
    i: Integer;
  begin
    for i := Low(a) to High(a) do
      AddEnums(a[i], ItemList, InsertList);
  end;

  procedure AddFuncDeclaration(Item: TciProcedureDeclaration; ItemList, InsertList: TStrings);
  var
    s, n: string;
    d: TDeclaration;
  begin
    s := FormatFirstColumn(Item.ProcType);
    d := Item.Items.GetFirstItemOfClass(TciProcedureName);
    if (d = nil) then
      Exit;
    n := d.ShortText;

    {$IFDEF ciCHECKDUPLICATES}
    if (InsertList.IndexOf(n) > -1) then
      Exit;
    {$ENDIF}

    s := s + FormatMainName(n);
    if (Item.Params <> '') then
    begin
      s := s + FormatMainExtra('(' + Item.Params + ')');
      //n := n {+ '('};
    end;
    d := Item.Items.GetFirstItemOfClass(TciReturnType);
    if (d <> nil) then
      s := s + ': ' + FormatTypeName(d.ShortText)
    {else
      s := s + ';'};
    ItemList.Insert(InsertList.Add(n), s);

    AddEnums(Item.GetParamDeclarations, ItemList, InsertList);
    if (d <> nil) then
      AddEnums(d, ItemList, InsertList);
  end;

  function PropertyIndex(Item: TciClassProperty): string;
  var
    i: Integer;
    d: TDeclaration;
    a: TDeclarationArray;
  begin
    d := Item.Items.GetFirstItemOfClass(TciPropertyParameterList);
    Result := '';

    if (d <> nil) then
    begin
      a := d.Items.GetItemsOfClass(TciIdentifier);
      for i := Low(a) to High(a) do
      begin
        if (Result <> '') then
          Result := Result + ', ';
        Result := Result + a[i].ShortText;
      end;

      d := d.Items.GetFirstItemOfClass(TciTypeKind);
      if (d <> nil) then
      begin
        if (Result <> '') then
          Result := Result + ': ';
        Result := Result + d.ShortText;
      end;
    end;

    if (Result <> '') then
      Result := '['+Result+']';
  end;

var
  i: Integer;
  FirstColumn, s, n: string;
  a: TDeclarationArray;
  b: array[1..2] of TDeclaration;
  c: array[0..2] of TDeclarationClass;
begin
  if (Item = nil) then
    Exit;

  if (Item is TciProcedureDeclaration) then
  begin
    if (ShowTypeMethods) or (Item.Items.GetFirstItemOfClass(TciProcedureClassName) = nil) then
      AddFuncDeclaration(TciProcedureDeclaration(Item), ItemList, InsertList);
    Exit;
  end;

  c[0] := nil;
  c[1] := nil;
  c[2] := nil;
  if (Item is TciVarDeclaration) then
  begin
    FirstColumn := FormatFirstColumn('variable');
    c[0] := TciVarName;
    c[2] := TciTypeKind;
  end
  else if (Item is TciConstantDeclaration) then
  begin
    FirstColumn := FormatFirstColumn('constant');
    c[0] := TciConstantName;
    c[1] := TciExpression;
  end
  else if (Item is TciLabelDeclaration) then
  begin
    FirstColumn := FormatFirstColumn('label');
    c[0] := TciLabelName;
  end
  else if (Item is TciTypeDeclaration) then
  begin
    FirstColumn := FormatFirstColumn('type');
    c[0] := TciTypeName;
    c[2] := TciTypeKind;
  end
  else if (Item is TciClassField) then
  begin
    FirstColumn := FormatFirstColumn('variable');
    c[0] := TciFieldName;
    c[2] := TciTypeKind;
  end
  else if (Item is TciClassProperty) then
  begin
    FirstColumn := FormatFirstColumn('property');
    c[0] := TciFieldName;
    c[2] := TciTypeKind;
  end
  else if (Item is TciConstParameter) or
          (Item is TciOutParameter) or
          (Item is TciFormalParameter) or
          (Item is TciInParameter) or
          (Item is TciVarParameter) then
  begin
    FirstColumn := FormatFirstColumn('param');
    c[0] := TciParameterName;
    c[2] := TciParameterType;
  end;

  if (c[0] = nil) then
    Exit;

  a := Item.Items.GetItemsOfClass(c[0]);
  if (c[1] <> nil) then
    b[1] := Item.Items.GetFirstItemOfClass(c[1])
  else
    b[1] := nil;
  if (c[2] <> nil) then
  begin
    b[2] := Item.Items.GetFirstItemOfClass(c[2]);
    if (b[2] is TciTypeKind) and (b[2].Items.Count > 0) and (not (b[2].Items[0] is TciArrayType)) then
      b[2] := b[2].Items[0];
  end
  else
    b[2] := nil;
  for i := Low(a) to High(a) do
  begin
    n := a[i].ShortText;

    {$IFDEF ciCHECKDUPLICATES}
    if (InsertList.IndexOf(n) > -1) then
      Continue;
    {$ENDIF}
    s := FirstColumn + FormatMainName(n);
    if (Item is TciClassProperty) then
      s := s + FormatMainExtra(PropertyIndex(TciClassProperty(Item)));
    if (b[1] <> nil) then
      s := s + FormatMainExtra(' = ' + b[1].ShortText);
    if (b[2] <> nil) then
      if (b[2] is TciEnumType) then
        s := s + ': ' + FormatTypeName(GetEnumTypeName(b[2]))
      else
        s := s + ': ' + FormatTypeName(b[2].ShortText)
    {else
      s := s + ';'};

    ItemList.Insert(InsertList.Add(n), s);
    if (b[2] <> nil) and (b[2].Items.Count > 0) then
      AddEnums(b[2], ItemList, InsertList);
  end;
end;

procedure TCodeInsight.FillProposal;
var
  i: Integer;
begin
  if (not Proposal_Filled) then
  begin
    Proposal_ItemList.BeginUpdate;
    Proposal_InsertList.BeginUpdate;

    try
      for i := 0 to Items.Count - 1 do
        Proposal_AddDeclaration(Items[i], Proposal_ItemList, Proposal_InsertList);
      Proposal_Filled := True;
    finally
      Proposal_ItemList.EndUpdate;
      Proposal_InsertList.EndUpdate;
    end;
  end;
end;

(*
 * Returns Headers and Names of all functions/procedures found in self.
 * eg: Headers = ['procedure foo;', 'function HiDgby: boolean;']
 *     Names = [foo, HiDgby]
*)
procedure TCodeInsight.GetProcedures(Headers, Names: TStrings; FindTypeProcs: Boolean = False);
var
  ProcItem: TCIProcedureDeclaration;
  Decl, d: TDeclaration;
  i, c: integer;
  s: string;
begin
  if (Headers = nil) or (Names = nil) then
    Exit();

  c := (Self.Items.Count - 1);
  if (c < 1) then
    Exit();

  Headers.BeginUpdate(); // No drawing here, so who knows if this does anything.
  Names.BeginUpdate();

  for i := 0 to c do
  begin
    if (Items[i] = nil) then
      Continue;

    if (Items[i] is TCIProcedureDeclaration) then
      ProcItem := TCIProcedureDeclaration(Items[i])
    else
      Continue;

    Decl := ProcItem.Items.GetFirstItemOfClass(TciProcedureName);

    if (Decl = nil) or (ProcItem.Items.Count = 0) then // nil stuff
      Continue;

    if (not FindTypeProcs) then
      if (Boolean(ProcItem.Items[0].ClassType = TciProcedureClassName)) then // Is a type linked to the proc?
        Continue;

    s := '';
    s := ProcItem.ProcType + ' ' + Decl.ShortText; //procedure foo
    if (ProcItem.Params <> '') then // add any params
      s += '(' + ProcItem.Params + ')';

    d := ProcItem.Items.GetFirstItemOfClass(TciReturnType);
    if (d <> nil) then // if returns something, lets add it
      s += ': ' + d.ShortText;

    s += ';'; // dont forget the ";"!

    Headers.Add(s);
    Names.Add(Decl.ShortText);
  end;

  Headers.EndUpdate();
  Names.EndUpdate();
end;

(*
 * Returns all functions / procedures names and item of the type found by Prefix.
 * The following will return all functions/procedures of the TMufasaBitmap.
 * var bmp: TMufasaBitmap
 * FoundItems := ci.GetTypeProcs(NamesList, 'bmp');
*)
function TCodeInsight.GetTypeProcs(Names: TStrings; const Prefix: string): TDeclarationArray;

  // Returns Info of the proc. ie: Foo(var: string);
  function GetInfo(Item: TDeclaration): string;
  var
    ProcItem: TCIProcedureDeclaration;
  begin
    if (Item is TCIProcedureDeclaration) then
    begin
      ProcItem := TCIProcedureDeclaration(Item);
      Result := ProcItem.Name.CleanText;

      if (ProcItem.Params <> '') then  // has params
        Result += '(' + ProcItem.Params + ')';
    end;
  end;

  procedure Scan(_include: TCodeInsight; _dType: String; Names: TStrings; var fItems: TDeclarationArray);
  var
    i, ii, Len: Integer;
    s: string;
  begin
    for i := 0 to (_include.Items.Count - 1) do
      for ii := 0 to (_include.Items[i].Items.Count - 1) do
         if (_include.Items[i].Items[ii].ClassType = TciProcedureClassName) and (Lowercase(_include.Items[i].Items[ii].ShortText) = Lowercase(_dType)) then
         begin
           s := GetInfo(_include.Items[i]);

           if (s <> '') then
           begin
             Names.Add(s);

             Len := Length(fItems);
             SetLength(fItems, Len + 1);
             fItems[Len] := _Include.Items[i];
           end;
         end;

     if (Length(_include.Includes) > 0) then
       for i := 0 to length(_include.Includes) - 1 do
         Scan(_include.Includes[i], _dType, Names, fItems);
   end;

var
  i: integer;
  dDecl: TDeclaration;
  dType: string;
begin
  if (Prefix = '') then
    Exit();

  dDecl := FindVarBase(Prefix, False, vbType);

  if (dDecl = nil) then // No luck. :(
    Exit();

  dType := dDecl.CleanText; // Var type; ie TBox.
  Scan(Self, dType, Names, Result); // Scan current script + (includes ?)

  for i := High(CoreBuffer) downto Low(CoreBuffer) do // Scan simbas internals
    Scan(CoreBuffer[i], dType, Names, Result);
end;

(*
 * Returns true if we find the procedure *NON TYPE* in the ci, and Simbas internals.
 * Also returns the Item if found in Decl and if the Item has params or not.
*)
function TCodeInsight.FindProcedure(ProcNameToFind: string; out Decl: TDeclaration; out HasParams: Boolean): boolean;

  // Search the ci for the procedure!
  function FindMatch(_include: TCodeInsight; ProcName: String; out TheItem: TDeclaration; out Params: Boolean): Boolean;
  var
    i, ii: Integer;
    LProcName: string;
    ProcItem: TCIProcedureDeclaration;
  begin
    Result := False;
    HasParams := False;
    LProcName := Lowercase(ProcName);

    for i := 0 to (_include.Items.Count - 1) do
      for ii := 0 to (_include.Items[i].Items.Count - 1) do
        if (_include.Items[i] is TCIProcedureDeclaration) then
        begin
          ProcItem := TCIProcedureDeclaration(_include.Items[i]);

          if (ProcItem.Items.Count = 0) then
            Continue;

          if (Boolean(ProcItem.Items[0].ClassType = TciProcedureClassName)) then // we dont want type funcs.
            Continue;

          if (Lowercase(ProcItem.Name.CleanText) = LProcName) then
           begin
             Params := Boolean(ProcItem.Params <> '');
             TheItem := _include.Items[i];
             Exit(True);
           end;
        end;

     if (Length(_include.Includes) > 0) then
       for i := 0 to Length(_include.Includes) - 1 do
         if (FindMatch(_include.Includes[i], ProcName, TheItem, Params)) then
           Exit(True);
  end;

var
  i: Integer;
begin
  if (FindMatch(Self, ProcNameToFind, Decl, HasParams)) then // scan ci
    Exit(True);

  for i := High(CoreBuffer) downto Low(CoreBuffer) do // Scan simbas internals
    if (FindMatch(CoreBuffer[i], ProcNameToFind, Decl, HasParams)) then
      Exit(True);
end;

procedure TCodeInsight.FillSynCompletionProposal(ItemList, InsertList: TStrings; Prefix: string = '');

  procedure AddFile(Item: TCodeInsight; ItemList, InsertList: TStrings);
  var
    i: Integer;
  begin
    if (item = nil) or (ItemList = nil) or (InsertList = nil) or (Item.Proposal_InsertList = nil) or (Item.Proposal_ItemList = nil) then
      Exit;

    if (not Item.Proposal_Filled) then
      Item.FillProposal;

    {$IFNDEF ciCHECKDUPLICATES}
    ItemList.AddStrings(Item.Proposal_ItemList);
    InsertList.AddStrings(Item.Proposal_InsertList);
    {$ELSE}
    for i := 0 to Item.Proposal_InsertList.Count - 1 do
      if (InsertList.IndexOf(Item.Proposal_InsertList[i]) = -1) then
        ItemList.Insert(InsertList.Add(Item.Proposal_InsertList[i]), Item.Proposal_ItemList[i]);
    {$ENDIF}

    for i := Low(Item.Includes) to High(Item.Includes) do
      AddFile(Item.Includes[i], ItemList, InsertList);
  end;

  function GetParentType(const Item: TciTypeKind; out Name: string): Boolean;
  var
    i, j: Integer;
    Str: String;
  begin
    if (Item = nil) or (Item.Owner = nil) then
      Exit();

    if (Item.Owner.Items.Count < 1) then
      Exit();

    Str := Trim(Lowercase(Item.Owner.Items[1].CleanText));
    if (Str = '') then
      Exit();

    Name := '';

    if (Pos(' ', Str) = 0) then // type TObject...
      Name := Str
    else if (Pos('record', Str) = 1) then // Record(TObject)
    begin
      i := 7;

      while (i < Length(Str)) and (Str[i] = ' ') do
        Inc(i);

      if (Str[i] = '(') then
      begin
        j := i + 1;
        while (i < Length(Str)) and (Str[i] <> ')') do
          inc(i);

        if (Str[i] = ')') then
          Name := Copy(Str, j, i - j);
      end;
    end;

    Result := (Name <> '') and (Name <> 'pointer') and (Name <> 'tobject');
  end;

  function checkInclude(Incl: TCodeInsight; _dType: String): string;
  var
    i, ii, c, cc, Len: Integer;
    TypeToFind, ParentName, ParentName2, Str: string;
    FoundParent: Boolean;
  begin
    Result := '';
    FoundParent := False;
    TypeToFind := Lowercase(_dType);

    c := Incl.Items.Count - 1;
    for i := 0 to c do
    begin
      cc := Incl.Items[i].Items.Count - 1;

      for ii := 0 to cc do
      begin
        if (Lowercase(Incl.Items[i].Items[ii].ShortText) <> TypeToFind) then
          Continue;

        if (Incl.items[i] is TciProcedureDeclaration) then
        begin
          if (Incl.Items[i].Items[ii].ClassType = TciProcedureClassName) then
            Proposal_AddDeclaration(Incl.Items[i], ItemList, InsertList, True);
        end else
          if (not FoundParent) then
            if (Incl.Items[i].Items[ii] is TciTypeName) and (ii < cc) then // "foo" = record
              if (Incl.Items[i].Items[ii + 1] is TciTypeKind) then // foo = "record"
                if (GetParentType(TciTypeKind(Incl.Items[i].Items[ii + 1]), ParentName)) then
                begin
                  FoundParent := True;
                  Result := ParentName;
                end;
      end;
    end;

    ParentName2 := '';
    Len := Length(Incl.Includes);

    if (Len > 0) then
      for i := 0 to (Len - 1) do
      begin
        Str := checkInclude(Incl.Includes[i], _dType);
        if (Str <> '') then
          ParentName2 := Str;
      end;

    if (ParentName2 <> '') then
      Result := ParentName2;
  end;

var
  i, ii, p, pp: Integer;
  d, dDecl: TDeclaration;
  FoundItems: TDeclarationArray;
  NamesList: TStringList;
  dType, Parent, Str, TypeStr: string;
begin
  ItemList.BeginUpdate;
  InsertList.BeginUpdate;

  try
    ItemList.Clear;
    InsertList.Clear;

    if (Prefix <> '') then
    begin
      if (Lowercase(Prefix) = 'self') then
        Prefix := 'SELF';

      d := FindVarBase(Prefix, True, vbType);

      if (d <> nil) then
        for i := 0 to d.Items.Count - 1 do
          Proposal_AddDeclaration(d.Items[i], ItemList, InsertList);

      dDecl := FindVarBase(Prefix, False, vbType);

      if (dDecl = nil) and (d = nil) then
      begin
        p := LastDelimiter('.', Prefix);
        Str := LowerCase(Copy(Prefix, 1, p - 1));

        pp := LastDelimiter('.', Str);
        TypeStr := Copy(Str, pp + 1, (Length(Str) - pp) + 1);

        if (p > 0) and ((p + 1) <= Length(Prefix)) and (Str <> '') and (TypeStr <> '') then
        begin
          Prefix := PrePrepareString(Copy(Prefix, p + 1, (Length(Prefix) - p) + 1));
          mDebugLn('CC: Prefix|Type [%s, %s]', [Prefix, TypeStr]);
          NamesList := TStringList.Create();

          try
            FoundItems := Self.GetTypeProcs(NamesList, TypeStr);

            for i := 0 to (NamesList.Count - 1) do
              if (SameText(NamesList[i], Prefix)) then
              begin
                dDecl := TciProcedureDeclaration(FoundItems[i]).Items.GetFirstItemOfClass(TciReturnType);
                if (dDecl <> nil) then
                begin
                  d := Self.FindVarBase(Prefix, True, vbType);

                  if (d <> nil) then // adding properties
                    for ii := 0 to (d.Items.Count - 1) do
                      Proposal_AddDeclaration(d.Items[ii], ItemList, InsertList);
                end;

                Break; // Now we will break from the loop, contiune.. and add methods!
              end;
          finally
            NamesList.Free();
          end;
        end;
      end;


      if (dDecl <> nil) then
      begin
        dType := Trim(dDecl.CleanText);
        Parent := '';
        Parent := checkInclude(Self, dType); // Scan script + includes

        if (Parent <> '') then
          while (Parent <> '') do
          begin
            //mDebugLn('CC: %s is a parent for %s, adding...', [Parent, dType]);

            d := FindVarBase(Parent, True, vbType);

            if (d <> nil) then
              for i := 0 to d.Items.Count - 1 do
                Proposal_AddDeclaration(d.Items[i], ItemList, InsertList);
            Parent := checkInclude(Self, Parent);
          end;

        for i := High(CoreBuffer) downto Low(CoreBuffer) do
        begin
          Parent := '';
          Parent := checkInclude(CoreBuffer[i], dType);

          if (Parent <> '') then
          while (Parent <> '') do
          begin
            //mDebugLn('CC: %s is a parent for %s, adding...', [Parent, dType]);
            Parent := checkInclude(CoreBuffer[i], Parent);
          end;
        end;
      end;
    end else
    begin
      for i := High(InWith) downto Low(InWith) do
        if (i <> InClassFunction) then
          for ii := 0 to InWith[i].Items.Count - 1 do
            Proposal_AddDeclaration(InWith[i].Items[ii], ItemList, InsertList);

      for i := Low(InFunc) to High(InFunc) do
        for ii := 0 to InFunc[i].Items.Count - 1 do
           Proposal_AddDeclaration(InFunc[i].Items[ii], ItemList, InsertList);

      if (InClassFunction <> -1) then
        for i := 0 to InWith[InClassFunction].Items.Count - 1 do
          Proposal_AddDeclaration(InWith[InClassFunction].Items[i], ItemList, InsertList);

      AddFile(Self, ItemList, InsertList);
      for i := 0 to High(CoreBuffer) do
        AddFile(CoreBuffer[i], ItemList, InsertList);
    end;
  finally
    ItemList.EndUpdate;
    InsertList.EndUpdate;
  end;
end;

initialization
  {nothing}
  CoreDefines := TStringList.Create;
finalization
  ClearIncludeBuffer;
  ClearCoreBuffer;
  CoreDefines.Free;

end.
