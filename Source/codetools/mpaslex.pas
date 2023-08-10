{ + --------------------------------------------------------------------------+
  | Class:       TmwPasLex
  | Created:     07.98 - 10.98
  | Author:      Martin Waldenburg
  | Description: A very fast Pascal tokenizer.
  | Version:     1.32
  | Copyright (c) 1998, 1999 Martin Waldenburg
  | All rights reserved.
  |
  | DISCLAIMER:
  |
  | THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS'.
  |
  | ALL EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
  | THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
  | PARTICULAR PURPOSE ARE DISCLAIMED.
  |
  | IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
  | INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  | (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  | OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  | INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
  | WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  | NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
  | THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  |
  |  Martin.Waldenburg@T-Online.de
  +--------------------------------------------------------------------------+ }

// Heavily modified over the years for Simba

unit mPasLex;

{$i simba.Inc}

interface

uses
  SysUtils, Classes,
  simba.mufasatypes, simba.list, simba.stack,
  mPasLexTypes;

type
  TmwBasePasLex = class;
  TDirectiveEvent = procedure(Sender: TmwBasePasLex) of object;
  TErrorMessageEvent = procedure(Sender: TmwBasePasLex; Message: String) of object;

  PDefineRec = ^TDefineRec;
  TDefineRec = record
    Defined: Boolean;
    StartCount: Integer;
    Next: PDefineRec;
  end;
  TDefineRecArray = array of TDefineRec;

  PSaveDefinesRec = ^TSaveDefinesRec;
  TSaveDefinesRec = record
    RecArray: TDefineRecArray;
    Stack: Integer;
    Defines: string;
  end;

  TmwBasePasLex = class(TObject)
  protected
    fCommentState: TCommentState;
    fDoc: String;
    fFileName: String;
    fFileAge: Integer;
    fRun: Integer;
    fTokenPos: Integer;
    fLineNumber: Integer;
    FTokenID: TptTokenKind;
    fLinePos: Integer;
    fExID: TptTokenKind;
    fOnCompDirect: TDirectiveEvent;
    fOnElseDirect: TDirectiveEvent;
    fOnEndIfDirect: TDirectiveEvent;
    fOnIfDefDirect: TDirectiveEvent;
    fOnIfNDefDirect: TDirectiveEvent;
    fOnResourceDirect: TDirectiveEvent;
    fOnIncludeDirect: TDirectiveEvent;
    fOnLibraryDirect: TDirectiveEvent;
    fOnDefineDirect: TDirectiveEvent;
    fOnIfOptDirect: TDirectiveEvent;
    fOnIfDirect: TDirectiveEvent;
    fOnElseIfDirect: TDirectiveEvent;
	  fOnUnDefDirect: TDirectiveEvent;
    fOnErrorMessage: TErrorMessageEvent;

    FDirectiveParamOrigin: PAnsiChar;

    FDefines: TStringList;
    FDefineStack: Integer;
    FTopDefineRec: PDefineRec;
    FUseDefines: Boolean;

    FIdentBuffer: PChar;
    FIdentBufferUpper: PtrUInt;

    function getChar(const Pos: Integer): Char; inline;

    function GetPosXY: TTokenPoint;
    procedure SetRunPos(Value: Integer);
    procedure AddressOpProc;
    procedure AmpersandOpProc;
    procedure AsciiCharProc;
    procedure AnsiProc;
    procedure BorProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure ColonProc;
    procedure CommaProc;
    procedure CRProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure IntegerProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure NullProc;
    procedure NumberProc;
    procedure PlusProc;
    procedure PointerSymbolProc;
    procedure PointProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure SemiColonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SquareCloseProc;
    procedure SquareOpenProc;
    procedure StarProc;
    procedure StringProc;
    procedure StringDQProc;
    procedure SymbolProc;
    procedure UnknownProc;
    function GetToken: string;
    function GetTokenLen: Integer;
    function GetCompilerDirective: string;
    function GetDirectiveKind: TptTokenKind;
    function GetIDEDirectiveKind: TptTokenKind;
    function GetDirectiveParam: string;
    function GetDirectiveParamOriginal: string;
    function GetDirectiveParamAsFileName: string;
    function GetIsJunk: Boolean;
    function GetIsSpace: Boolean;
    function GetIsCompilerDirective: Boolean;

    procedure EnterDefineBlock(ADefined: Boolean);
    procedure ExitDefineBlock;
  protected
    procedure Error(Message: String); virtual;

    procedure SetOnCompDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnDefineDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnElseDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnEndIfDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnIfDefDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnIfNDefDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnIfOptDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnIncludeDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnLibraryDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnResourceDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnUnDefDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnIfDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnElseIfDirect(const Value: TDirectiveEvent); virtual;
  public
    OnErrorMessage: TErrorMessageEvent;

    CaretPos: Integer;

    constructor Create(Doc: String; AFileName: String = ''); virtual;
    constructor CreateFromFile(AFileName: String); virtual;
    destructor Destroy; override;
    procedure Next; inline;
    procedure NextNoJunk; inline;

    function CopyDoc(const StartPos, EndPos: Integer): String;

    procedure AddDefine(const ADefine: string);
    procedure RemoveDefine(const ADefine: string);
    procedure ClearDefines;
    procedure CloneDefinesFrom(ALexer: TmwBasePasLex);
    function SaveDefines: TSaveDefinesRec;
    procedure LoadDefines(From: TSaveDefinesRec);
    function IsDefined(const ADefine: string): Boolean;

    property FileName: String read fFileName;
    property FileAge: Integer read fFileAge;
    property CompilerDirective: string read GetCompilerDirective;
    property DirectiveParam: string read GetDirectiveParam;
    property DirectiveParamOriginal: string read GetDirectiveParamOriginal;
    property DirectiveParamAsFileName: string read GetDirectiveParamAsFileName;
	  property IsJunk: Boolean read GetIsJunk;
    property IsSpace: Boolean read GetIsSpace;
    property LineNumber: Integer read fLineNumber write fLineNumber;
    property LinePos: Integer read fLinePos write fLinePos;
    property PosXY: TTokenPoint read GetPosXY;
    property RunPos: Integer read fRun write SetRunPos;
    property Token: string read GetToken;
    property TokenLen: Integer read GetTokenLen;
    property TokenPos: Integer read fTokenPos;
    property TokenID: TptTokenKind read FTokenID;
    property ExID: TptTokenKind read fExID;
    property IsCompilerDirective: Boolean read GetIsCompilerDirective;
    property OnCompDirect: TDirectiveEvent read fOnCompDirect write SetOnCompDirect;
    property OnDefineDirect: TDirectiveEvent read fOnDefineDirect write SetOnDefineDirect;
    property OnElseDirect: TDirectiveEvent read fOnElseDirect write SetOnElseDirect;
    property OnEndIfDirect: TDirectiveEvent read fOnEndIfDirect write SetOnEndIfDirect;
    property OnIfDefDirect: TDirectiveEvent read fOnIfDefDirect write SetOnIfDefDirect;
    property OnIfNDefDirect: TDirectiveEvent read fOnIfNDefDirect write SetOnIfNDefDirect;
    property OnIfOptDirect: TDirectiveEvent read fOnIfOptDirect write SetOnIfOptDirect;
    property OnIncludeDirect: TDirectiveEvent read fOnIncludeDirect write SetOnIncludeDirect;
    property OnLibraryDirect: TDirectiveEvent read fOnLibraryDirect write SetOnLibraryDirect;
    property OnIfDirect: TDirectiveEvent read fOnIfDirect write SetOnIfDirect;
    property OnElseIfDirect: TDirectiveEvent read fOnElseIfDirect write SetOnElseIfDirect;
	  property OnResourceDirect: TDirectiveEvent read fOnResourceDirect write SetOnResourceDirect;
	  property OnUnDefDirect: TDirectiveEvent read fOnUnDefDirect write SetOnUnDefDirect;

    property DirectiveParamOrigin: PAnsiChar read FDirectiveParamOrigin;

    property UseDefines: Boolean read FUseDefines write FUseDefines;

    property Defines: TStringList read FDefines;
  end;

  TmwPasLex = class(TmwBasePasLex)
  private
    fAheadLex: TmwBasePasLex;

    function GetAheadExID: TptTokenKind;
    function GetAheadToken: string;
    function GetAheadTokenID: TptTokenKind;
  public
    constructor Create(Doc: String; AFileName: String = ''); override;
    destructor Destroy; override;
    procedure InitAhead;
    procedure AheadNext;
    property AheadLex: TmwBasePasLex read fAheadLex;
    property AheadToken: string read GetAheadToken;
    property AheadTokenID: TptTokenKind read GetAheadTokenID;
    property AheadExID: TptTokenKind read GetAheadExID;
  end;

  TLexerStack = specialize TSimbaStack<TmwPasLex>;
  TLexerList = specialize TSimbaObjectList<TmwPasLex>;

implementation

procedure TmwBasePasLex.ClearDefines;
var
  Frame: PDefineRec;
begin
  while FTopDefineRec <> nil do
  begin
    Frame := FTopDefineRec;
    FTopDefineRec := Frame^.Next;
    Dispose(Frame);
  end;
  FDefines.Clear;
  FDefineStack := 0;
  FTopDefineRec := nil;
end;

procedure TmwBasePasLex.CloneDefinesFrom(ALexer: TmwBasePasLex);
var
  Frame, LastFrame, SourceFrame: PDefineRec;
begin
  ClearDefines;
  FDefines.Assign(ALexer.FDefines);
  FDefineStack := ALexer.FDefineStack;

  Frame := nil;
  LastFrame := nil;
  SourceFrame := ALexer.FTopDefineRec;
  while SourceFrame <> nil do
  begin
    New(Frame);
    if FTopDefineRec = nil then
      FTopDefineRec := Frame
    else
      LastFrame^.Next := Frame;
    Frame^.Defined := SourceFrame^.Defined;
    Frame^.StartCount := SourceFrame^.StartCount;
    LastFrame := Frame;

    SourceFrame := SourceFrame^.Next;
  end;
  if Frame <> nil then
    Frame^.Next := nil;
end;

function TmwBasePasLex.SaveDefines: TSaveDefinesRec;
var
  Frame: PDefineRec;
begin
  Result.Defines := FDefines.CommaText;
  Result.Stack := FDefineStack;

  Frame := FTopDefineRec;
  while (Frame <> nil) do
  begin
    SetLength(Result.RecArray, Length(Result.RecArray) + 1);
    Result.RecArray[High(Result.RecArray)] := Frame^;
    Result.RecArray[High(Result.RecArray)].Next := nil;
    Frame := Frame^.Next;
  end;
end;

procedure TmwBasePasLex.LoadDefines(From: TSaveDefinesRec);
var
  Frame, LastFrame: PDefineRec;
  i: Integer;
begin
  ClearDefines;
  FDefines.CommaText := From.Defines;
  FDefineStack := From.Stack;

  Frame := nil;
  LastFrame := nil;
  for i := 0 to High(From.RecArray) do
  begin
    New(Frame);
    if (i = 0) then
      FTopDefineRec := Frame
    else
      LastFrame^.Next := Frame;

    Frame^ := From.RecArray[i];
    LastFrame := Frame;
  end;

  if (Frame <> nil) then
    Frame^.Next := nil;
end;

function TmwBasePasLex.getChar(const Pos: Integer): Char;
begin
  if (Pos >= 1) and (Pos <= Length(fDoc)) then
    Result := fDoc[Pos]
  else
    Result := #0;
end;

function TmwBasePasLex.GetPosXY: TTokenPoint;
begin
  Result.X := FTokenPos - FLinePos;
  Result.Y := FLineNumber;
end;

constructor TmwBasePasLex.Create(Doc: String; AFileName: String = '');
begin
  inherited Create();

  fDoc := Doc;
  fCommentState := csNo;
  fRun := 1;
  fFileName := AFileName;
  if (fFileName <> '') and FileExists(fFileName) then
    fFileAge := SysUtils.FileAge(fFileName);

  fExID := tokUnKnown;

  FIdentBuffer := GetMem(MaxTokenNameLength + 1);
  FIdentBufferUpper := PtrUInt(@FIdentBuffer[MaxTokenNameLength]);

  FUseDefines := True;
  FDefines := TStringList.Create();
  FDefines.UseLocale := False;
  FDefines.Duplicates := dupIgnore;

  CaretPos := -1;
end;

constructor TmwBasePasLex.CreateFromFile(AFileName: String);
var
  Contents: String;
begin
  Contents := '';
  if FileExists(AFileName) then
    with TStringList.Create() do
    try
      LoadFromFile(AFileName);

      Contents := Text;
    finally
      Free();
    end;

  Create(Contents, AFileName);
end;

destructor TmwBasePasLex.Destroy;
begin
  ClearDefines();
  FDefines.Free();

  FreeMem(FIdentBuffer);

  inherited Destroy();
end;

procedure TmwBasePasLex.SetRunPos(Value: Integer);
begin
  fRun := Value;
  Next;
end;

procedure TmwBasePasLex.AddDefine(const ADefine: string);
begin
  FDefines.Add(ADefine);
end;

procedure TmwBasePasLex.AddressOpProc;
begin
  case getChar(fRun + 1) of
    '@':
      begin
        fTokenID := tokDoubleAddressOp;
        Inc(fRun, 2);
      end;
  else
    begin
      fTokenID := tokAddressOp;
      Inc(fRun);
    end;
  end;
end;

procedure TmwBasePasLex.AsciiCharProc;
begin
  fTokenID := tokAsciiChar;
  Inc(fRun);
  if getChar(fRun) = '$' then
  begin
    Inc(fRun);
    while getChar(fRun) in ['0'..'9', 'A'..'F', 'a'..'f'] do Inc(fRun);
  end else
  begin
    while getChar(fRun) in ['0'..'9'] do
      Inc(fRun);
  end;
end;

procedure TmwBasePasLex.BraceCloseProc;
begin
  Inc(fRun);
  fTokenId := tokNull;

  Error('Illegal character');
end;

procedure TmwBasePasLex.BraceOpenProc;
var
  Param, Def: string;
begin
  case getChar(fRun + 1) of
    '$':
      begin
        BorProc(); // Skip comment
        fTokenID := GetDirectiveKind;
      end;
    '%':
      begin
        BorProc();
        fTokenID := GetIDEDirectiveKind;
      end;
    else
      FCommentState := csBor;
  end;

  if (fCommentState = csNo) then
  begin
    case fTokenID of
      tokIDECodeTools:
        begin
          if (not IsDefined('!IGNORECODETOOLS')) then
          begin
            if (DirectiveParam = 'OFF') then
              EnterDefineBlock(False)
            else
            if (DirectiveParam = 'ON') then
              ExitDefineBlock();
          end;

          Next();
        end;

      tokCompDirect:
        begin
          if FUseDefines and (FDefineStack = 0) then
          begin
            Def := CompilerDirective;
            Param := DirectiveParam;

            if (Def = 'SCOPEDENUMS') or (Def = 'S') then
            begin
              if (Param = 'ON')  or (Param = '+') then AddDefine('!SCOPEDENUMS') else
              if (Param = 'OFF') or (Param = '-') then RemoveDefine('!SCOPEDENUMS');
            end else
            if (Def = 'EXPLICTSELF') then
            begin
              if (Param = 'ON')  then AddDefine('!EXPLICTSELF') else
              if (Param = 'OFF') then RemoveDefine('!EXPLICTSELF');
            end;
          end;

          if Assigned(fOnCompDirect) and (FDefineStack = 0) then
            fOnCompDirect(Self);
        end;
      tokDefineDirect:
        begin
          if FUseDefines and (FDefineStack = 0) then
            AddDefine(DirectiveParam);
          if Assigned(fOnDefineDirect) then
            fOnDefineDirect(Self);
        end;
      tokElseDirect:
        begin
          if FUseDefines then
          begin
            if FTopDefineRec <> nil then
            begin
              if FTopDefineRec^.Defined then
                Inc(FDefineStack)
              else
                if FDefineStack > 0 then
                  Dec(FDefineStack);
            end;
          end;
          if Assigned(fOnElseDirect) then
            fOnElseDirect(Self);
        end;
      tokEndIfDirect:
        begin
          if FUseDefines then
            ExitDefineBlock;
          if Assigned(fOnEndIfDirect) then
            fOnEndIfDirect(Self);
        end;
      tokIfDefDirect:
        begin
          if FUseDefines then
            EnterDefineBlock(IsDefined(DirectiveParam));
          if Assigned(fOnIfDefDirect) then
            fOnIfDefDirect(Self);
        end;
      tokIfNDefDirect:
        begin
          if FUseDefines then
            EnterDefineBlock(not IsDefined(DirectiveParam));
    		  if Assigned(fOnIfNDefDirect) then
            fOnIfNDefDirect(Self);
        end;
      tokIfOptDirect:
        begin
          if Assigned(fOnIfOptDirect) then
            fOnIfOptDirect(Self);
        end;
      tokIfDirect:
        begin
          if FUseDefines then
          begin
            Param := DirectiveParam;
            if Pos('DEFINED', Param) = 1 then
            begin
              Def := Copy(Param, 9, Length(Param) - 9);
              EnterDefineBlock(IsDefined(Def));
            end;
          end;
          if Assigned(fOnIfDirect) then
            fOnIfDirect(Self);
        end;
      tokElseIfDirect:
        begin
          if FUseDefines then
          begin
            if FTopDefineRec <> nil then
            begin
              if FTopDefineRec^.Defined then
                Inc(FDefineStack)
              else
              begin
                if FDefineStack > 0 then
                  Dec(FDefineStack);
                Param := DirectiveParam;
                if Pos('DEFINED', Param) = 1 then
                begin
                  Def := Copy(Param, 9, Length(Param) - 9);
                  EnterDefineBlock(IsDefined(Def));
                end;
              end;
            end;
          end;
          if Assigned(fOnElseIfDirect) then
            fOnElseIfDirect(Self);
        end;
      tokIncludeDirect, tokIncludeOnceDirect:
        begin
          if Assigned(fOnIncludeDirect) and (FDefineStack = 0) then
            fOnIncludeDirect(Self);
        end;
      tokLibraryDirect:
        begin
          if Assigned(fOnLibraryDirect) and (FDefineStack = 0) then
            fOnLibraryDirect(Self);
        end;
      tokResourceDirect:
        begin
          if Assigned(fOnResourceDirect) and (FDefineStack = 0) then
            fOnResourceDirect(Self);
        end;
      tokUndefDirect:
        begin
          if FUseDefines and (FDefineStack = 0) then
            RemoveDefine(DirectiveParam);
          if Assigned(fOnUndefDirect) then
            fOnUndefDirect(Self);
        end;
    end;
  end;

  Next();
end;

procedure TmwBasePasLex.ColonProc;
begin
  if (getChar(fRun + 1) = '=') then
  begin
    Inc(fRun, 2);
    fTokenID := tokAssign;
	end else
  begin
    Inc(fRun);
    fTokenID := tokColon;
  end;
end;

procedure TmwBasePasLex.CommaProc;
begin
  Inc(fRun);
  fTokenID := tokComma;
end;

procedure TmwBasePasLex.CRProc;
begin
  case fCommentState of
    csBor: fTokenID := tokCRLFCo;
    csAnsi: fTokenID := tokCRLFCo;
    else
      fTokenID := tokCRLF;
  end;

  case getChar(fRun + 1) of
    #10: Inc(fRun, 2);
    else
      Inc(fRun);
  end;

  Inc(fLineNumber);
  fLinePos := fRun;
end;

procedure TmwBasePasLex.EnterDefineBlock(ADefined: Boolean);
var
  StackFrame: PDefineRec;
begin
  New(StackFrame);
  StackFrame^.Next := FTopDefineRec;
  StackFrame^.Defined := ADefined;
  StackFrame^.StartCount := FDefineStack;
  FTopDefineRec := StackFrame;
  if not ADefined then
    Inc(FDefineStack);
end;

procedure TmwBasePasLex.EqualProc;
begin
  Inc(fRun);
  fTokenID := tokEqual;
end;

procedure TmwBasePasLex.ExitDefineBlock;
var
  StackFrame: PDefineRec;
begin
  StackFrame := FTopDefineRec;
  if StackFrame <> nil then
  begin
    FDefineStack := StackFrame^.StartCount;
    FTopDefineRec := StackFrame^.Next;
    Dispose(StackFrame);
  end;
end;

procedure TmwBasePasLex.GreaterProc;
begin
  case getChar(fRun + 1) of
    '=':
      begin
        Inc(fRun, 2);
        fTokenID := tokGreaterEqual;
      end;
  else
    begin
      Inc(fRun);
      fTokenID := tokGreater;
	end;
  end;
end;

procedure TmwBasePasLex.Error(Message: String);
begin
  if Assigned(OnErrorMessage) then
    OnErrorMessage(Self, Message)
  else
  if (FileName <> '') then
    DebugLn('[Codetools]: "%s" at line %d, column %d in file "%s"', [Message, PosXY.Y + 1, PosXY.X, FileName])
  else
    DebugLn('[Codetools]: "%s" at line %d, column %d', [Message, PosXY.Y + 1, PosXY.X]);
end;

procedure TmwBasePasLex.IdentProc;
var
  Ptr: PChar;
begin
  Ptr := FIdentBuffer;
  while (getChar(fRun) in ['0'..'9', 'A'..'Z', '_', 'a'..'z']) do
  begin
    if (PtrUInt(Ptr) < FIdentBufferUpper) then
    begin
      Ptr^ := getChar(fRun);
      if (Ptr^ in [#65..#90]) then // change to lowercase
        Ptr^ := Char(Ord(Ptr^) + 32);
      Inc(Ptr);
    end;

    Inc(fRun);
  end;
  Ptr^ := #0;

  fTokenID := KeywordDictionary[FIdentBuffer];
  if (fTokenID in ExTokens) then
  begin
    fExID := fTokenID;
    fTokenID := tokIdentifier;
  end;
end;

procedure TmwBasePasLex.IntegerProc;
begin
  Inc(fRun);
  fTokenID := tokIntegerConst;
  while getChar(fRun) in ['0'..'9', 'A'..'F', 'a'..'f'] do
    Inc(fRun);
end;

function TmwBasePasLex.IsDefined(const ADefine: string): Boolean;
begin
  Result := FDefines.IndexOf(ADefine) > -1;
end;

procedure TmwBasePasLex.LFProc;
begin
  case fCommentState of
	  csBor: fTokenID := tokCRLFCo;
	  csAnsi: fTokenID := tokCRLFCo;
    else
      fTokenID := tokCRLF;
  end;
  Inc(fRun);
  Inc(fLineNumber);
  fLinePos := fRun;
end;

procedure TmwBasePasLex.LowerProc;
begin
  case getChar(fRun + 1) of
    '=':
      begin
        Inc(fRun, 2);
        fTokenID := tokLowerEqual;
      end;
    '>':
      begin
        Inc(fRun, 2);
        fTokenID := tokNotEqual;
      end
  else
    begin
      Inc(fRun);
      fTokenID := tokLower;
    end;
  end;
end;

procedure TmwBasePasLex.MinusProc;
begin
  Inc(fRun);
  if getChar(fRun) = '=' then
  begin
    Inc(fRun);
    fTokenID := tokMinusAsgn;
  end else
    fTokenID := tokMinus;
end;

procedure TmwBasePasLex.NullProc;
begin
  fTokenID := tokNull;
end;

procedure TmwBasePasLex.NumberProc;
begin
  Inc(fRun);
  fTokenID := tokIntegerConst;
  while getChar(fRun) in ['0'..'9', '.', 'e', 'E'] do
  begin
    case getChar(fRun) of
      '.':
        if getChar(fRun + 1) = '.' then
          break
        else fTokenID := tokFloat
    end;
    Inc(fRun);
  end;
end;

procedure TmwBasePasLex.PlusProc;
begin
  Inc(fRun);
  if getChar(fRun) = '=' then
  begin
    Inc(fRun);
    fTokenID := tokPlusAsgn;
  end else
    fTokenID := tokPlus;
end;

procedure TmwBasePasLex.PointerSymbolProc;
begin
  Inc(fRun);
  fTokenID := tokPointerSymbol;
end;

procedure TmwBasePasLex.PointProc;
begin
  case getChar(fRun + 1) of
    '.':
      begin
        Inc(fRun, 2);
        fTokenID := tokDotDot;
      end;
    ')':
      begin
        Inc(fRun, 2);
        fTokenID := tokSquareClose;
      end;
  else
    begin
      Inc(fRun);
      fTokenID := tokPoint;
    end;
  end;
end;

procedure TmwBasePasLex.RemoveDefine(const ADefine: string);
var
  I: Integer;
begin
  I := FDefines.IndexOf(ADefine);
  if (I > -1) then
    FDefines.Delete(I);
end;

procedure TmwBasePasLex.RoundCloseProc;
begin
  Inc(fRun);
  fTokenID := tokRoundClose;
end;

procedure TmwBasePasLex.AnsiProc;
var
  Depth: Integer = 0;
begin
  fTokenID := tokAnsiComment;

  while getChar(fRun) <> #0 do
  begin
    case getChar(fRun) of
      '(':
        begin
          if (getChar(fRun + 1) = '*') then
          begin
            Inc(fRun);
            Inc(Depth);
          end;
          Inc(fRun);
        end;

      '*':
        begin
          if (getChar(fRun + 1) = ')') then
          begin
            Inc(fRun);
            Dec(Depth);
          end;
          Inc(fRun);
          if (Depth <= 0) then
          	Break;
        end;

	    #10:
		    begin
			    Inc(fRun);
			    Inc(fLineNumber);
			    fLinePos := fRun;
		    end;

	    #13:
		    begin
			    Inc(fRun);
			    if getChar(fRun) = #10 then
            Inc(fRun);
			    Inc(fLineNumber);
			    fLinePos := fRun;
        end;

	    else
        Inc(fRun);
    end;
  end;

  fCommentState := csNo;
end;

procedure TmwBasePasLex.BorProc;
var
  Depth: Integer = 0;
begin
  fTokenID := tokBorComment;

  while getChar(fRun) <> #0 do
	  case getChar(fRun) of
      '{':
        begin
          Inc(fRun);
          Inc(Depth);
        end;

	    '}':
		    begin
          Inc(fRun);
          Dec(Depth);
          if (Depth <= 0) then
		        Break;
		    end;

	    #10:
		    begin
			    Inc(fRun);
			    Inc(fLineNumber);

			    fLinePos := fRun;
		    end;

      #13:
		    begin
			    Inc(fRun);
			    if getChar(fRun) = #10 then
            Inc(fRun);
			    Inc(fLineNumber);

			    fLinePos := fRun;
		    end;

	    else
        Inc(fRun);
	  end;

  fCommentState := csNo;
end;

procedure TmwBasePasLex.RoundOpenProc;
begin
  if (getChar(fRun + 1) = '*') then
  begin
    FCommentState := csAnsi;
    Next();
  end else
  begin
    Inc(fRun);
    fTokenID := tokRoundOpen;
  end;
end;

procedure TmwBasePasLex.SemiColonProc;
begin
  Inc(fRun);
  fTokenID := tokSemiColon;
end;

procedure TmwBasePasLex.SlashProc;
begin
  case getChar(fRun + 1) of
    '/':
      begin
        Inc(fRun, 2);
        fTokenID := tokSlashesComment;
        while getChar(fRun) <> #0 do
        begin
          case getChar(fRun) of
            #10, #13: break;
          end;
          Inc(fRun);
        end;
      end;
    '=': 
      begin
        Inc(fRun,2);
        fTokenID := tokDivAsgn;
      end;
  else
    begin
      Inc(fRun);
      fTokenID := tokSlash;
    end;
  end;
end;

procedure TmwBasePasLex.SpaceProc;
begin
  Inc(fRun);
  fTokenID := tokSpace;
  while getChar(fRun) in [#1..#9, #11, #12, #14..#32] do
    Inc(fRun);
end;

procedure TmwBasePasLex.SquareCloseProc;
begin
  Inc(fRun);
  fTokenID := tokSquareClose;
end;

procedure TmwBasePasLex.SquareOpenProc;
begin
  Inc(fRun);
  fTokenID := tokSquareOpen;
end;

procedure TmwBasePasLex.StarProc;
begin
  Inc(fRun);
  case getChar(fRun)  of
    '=':
      begin
        Inc(fRun);
        fTokenID := tokMulAsgn;
      end;
    '*':
      begin
        Inc(fRun);
        if getChar(fRun) = '=' then
        begin
          Inc(fRun);
          fTokenID := tokPowAsgn;
        end else
          fTokenID := tokStarStar;
      end;
    else
      fTokenID := tokStar;
  end;
end;

procedure TmwBasePasLex.StringProc;
begin
  fTokenID := tokStringConst;
  repeat
    Inc(fRun);
    case getChar(fRun) of
      #0, #10, #13:
        begin
          Error('Unterminated string');
          Break;
        end;
      #39:
        while (getChar(fRun) = #39) and (getChar(fRun + 1) = #39) do
          Inc(fRun, 2);
    end;
  until (getChar(fRun) = #39);

  if (getChar(fRun) = #39) then
  begin
    Inc(fRun);
    if (TokenLen = 3) then
      fTokenID := tokAsciiChar;
  end;
end;

procedure TmwBasePasLex.SymbolProc;
begin
  Inc(fRun);
  fTokenID := tokSymbol;
end;

procedure TmwBasePasLex.UnknownProc;
begin
  Inc(fRun);
  fTokenID := tokUnknown;
  Error('Unknown Character');
end;

procedure TmwBasePasLex.Next;
begin
  fExID := tokUnknown;
  fTokenPos := fRun;

  case fCommentState of
    csAnsi: AnsiProc;
    csBor: BorProc;
    csNo:
      begin
        case getChar(fRun) of
          #0: NullProc();
          #10: LFProc();
          #13: CRProc();
          #1..#9, #11, #12, #14..#32: SpaceProc();
          #34: StringDQProc();
          #39: StringProc();
          '0'..'9': NumberProc();
          'A'..'Z', 'a'..'z', '_': IdentProc();
          '{': BraceOpenProc();
          '}': BraceCloseProc();
          '(': RoundOpenProc();
          ')': RoundCloseProc();
          '*': StarProc();
          '+': PlusProc();
          ',': CommaProc();
          '-': MinusProc();
          '.': PointProc();
          '/': SlashProc();
          ':': ColonProc();
          ';': SemiColonProc();
          '<': LowerProc();
          '=': EqualProc();
          '>': GreaterProc();
          '@': AddressOpProc();
          '[': SquareOpenProc();
          ']': SquareCloseProc();
          '^': PointerSymbolProc();
          '#': AsciiCharProc();
          '$': IntegerProc();
          '?', '`', '!', '%', '&', '\': SymbolProc();
          else
            UnknownProc();
        end;
    end;
  end;

  //if (MaxPos > -1) and (fTokenPos > MaxPos) and (not IsJunk) then
  // fTokenID := tok_DONE;
end;

function TmwBasePasLex.GetIsJunk: Boolean;
begin
  result := (fTokenID in JunkTokens) or (FUseDefines and (FDefineStack > 0) and (TokenID <> tokNull){ and (TokenID <> tok_DONE)});
end;

function TmwBasePasLex.GetIsSpace: Boolean;
begin
  Result := fTokenID in [tokCRLF, tokSpace];
end;

function TmwBasePasLex.GetToken: string;
begin
  Result := Copy(FDoc, FTokenPos, GetTokenLen);
end;

function TmwBasePasLex.GetTokenLen: Integer;
begin
  Result := fRun - fTokenPos;
end;

procedure TmwBasePasLex.NextNoJunk;
begin
  repeat
    Next;
  until not IsJunk;
end;

function TmwBasePasLex.CopyDoc(const StartPos, EndPos: Integer): String;
begin
  Result := Copy(FDoc, StartPos, EndPos - StartPos);
end;

function TmwBasePasLex.GetCompilerDirective: string;
var
  StartPos, EndPos: Integer;
begin
  if (TokenID <> tokCompDirect) then
    Result := ''
  else
  begin
    StartPos := fTokenPos;
    while (not (getChar(StartPos) in [#0, '$'])) do
      Inc(StartPos);
    StartPos := StartPos + 1;
    EndPos := StartPos;
    while (not (getChar(EndPos) in [#0, ' ', '}'])) do
      Inc(EndPos);

    Result := UpperCase(Copy(fDoc, StartPos, EndPos - StartPos));
  end;
end;

function TmwBasePasLex.GetDirectiveKind: TptTokenKind;
var
  StartPos, EndPos: Integer;
  Directive: String;
begin
  Result := tokCompDirect;

  StartPos := fTokenPos;
  while (not (getChar(StartPos) in [#0, '$'])) do
    Inc(StartPos);
  StartPos := StartPos + 1;
  EndPos := StartPos;
  while (not (getChar(EndPos) in [#0, ' ', '}'])) do
    Inc(EndPos);

  Directive := UpperCase(Copy(fDoc, StartPos, EndPos - StartPos));
  if (Length(Directive) > 0) then
  begin
    if (Directive = 'I')            then Result := tokIncludeDirect     else
    if (Directive = 'IF')           then Result := tokIfDirect          else
    if (Directive = 'IFDEF')        then Result := tokIfDefDirect       else
    if (Directive = 'ENDIF')        then Result := tokEndIfDirect       else
    if (Directive = 'ELSE')         then Result := tokElseDirect        else
    if (Directive = 'DEFINE')       then Result := tokDefineDirect      else
    if (Directive = 'IFNDEF')       then Result := tokIfNDefDirect      else
    if (Directive = 'UNDEF')        then Result := tokUndefDirect       else
    if (Directive = 'LOADLIB')      then Result := tokLibraryDirect     else
    if (Directive = 'ELSEIF')       then Result := tokElseIfDirect      else
    if (Directive = 'IFOPT')        then Result := tokIfOptDirect       else
    if (Directive = 'INCLUDE')      then Result := tokIncludeDirect     else
    if (Directive = 'INCLUDE_ONCE') then Result := tokIncludeOnceDirect;
  end;
end;

function TmwBasePasLex.GetIDEDirectiveKind: TptTokenKind;
var
  StartPos, EndPos: Integer;
  Directive: String;
begin
  StartPos := fTokenPos;
  while (not (getChar(StartPos) in [#0, '%'])) do
    Inc(StartPos);
  StartPos := StartPos + 1;
  EndPos := StartPos;
  while (not (getChar(EndPos) in [#0, ' ', '}'])) do
    Inc(EndPos);

  Directive := UpperCase(Copy(fDoc, StartPos, EndPos - StartPos));
  if (Directive = 'CODETOOLS') then
    Result := tokIDECodeTools
  else
    Result := tokCompDirect;
end;

function TmwBasePasLex.GetDirectiveParamOriginal: string;
var
  StartPos, EndPos: Integer;
begin
  StartPos := fTokenPos;
  while (not (getChar(StartPos) in [#0, ' '])) do
    Inc(StartPos);

  EndPos := StartPos + 1;
  while (not (getChar(EndPos) in [#0, '}'])) do
    Inc(EndPos);

  Result := Copy(fDoc, StartPos + 1, (EndPos - StartPos) - 1);
end;

function TmwBasePasLex.GetDirectiveParam: string;
begin
  Result := UpperCase(GetDirectiveParamOriginal());
end;

function TmwBasePasLex.GetDirectiveParamAsFileName: string;
var
  i: Integer;
begin
  Result := GetDirectiveParamOriginal;
  for i:=1 to Length(Result) do
    {$IFDEF Windows}
    if Result[i]='/' then
      Result[i]:='\';
    {$ELSE}
    if Result[i]='\' then
      Result[i]:='/';
    {$ENDIF}
end;

function TmwBasePasLex.GetIsCompilerDirective: Boolean;
begin
  Result := fTokenID in [tokCompDirect, tokDefineDirect, tokElseDirect,
    tokEndIfDirect, tokIfDefDirect, tokIfNDefDirect, tokIfOptDirect,
    tokIncludeDirect, tokIncludeOnceDirect, tokResourceDirect, tokUndefDirect, tokLibraryDirect];
end;

constructor TmwPasLex.Create(Doc: String; AFileName: String = '');
begin
  inherited;
  fAheadLex := TmwBasePasLex.Create(Doc, AFileName);
end;

destructor TmwPasLex.Destroy;
begin
  fAheadLex.Free;
  inherited Destroy;
end;

procedure TmwPasLex.AheadNext;
begin
  fAheadLex.NextNoJunk;
end;

function TmwPasLex.GetAheadExID: TptTokenKind;
begin
  Result := fAheadLex.ExID;
end;

function TmwPasLex.GetAheadToken: string;
begin
  Result := fAheadLex.Token;
end;

function TmwPasLex.GetAheadTokenID: TptTokenKind;
begin
  Result := fAheadLex.TokenID;
end;

procedure TmwPasLex.InitAhead;
begin
  fAheadLex.RunPos := RunPos;
  FAheadLex.fLineNumber := FLineNumber;
  FAheadLex.FLinePos := FLinePos;
  FAheadLex.CloneDefinesFrom(Self);
  while fAheadLex.IsJunk do
    fAheadLex.Next;
end;

procedure TmwBasePasLex.SetOnCompDirect(const Value: TDirectiveEvent);
begin
  fOnCompDirect := Value;
end;

procedure TmwBasePasLex.SetOnDefineDirect(const Value: TDirectiveEvent);
begin
  fOnDefineDirect := Value;
end;

procedure TmwBasePasLex.SetOnElseDirect(const Value: TDirectiveEvent);
begin
  fOnElseDirect := Value;
end;

procedure TmwBasePasLex.SetOnElseIfDirect(const Value: TDirectiveEvent);
begin
  fOnElseIfDirect := Value;
end;

procedure TmwBasePasLex.SetOnEndIfDirect(const Value: TDirectiveEvent);
begin
  fOnEndIfDirect := Value;
end;

procedure TmwBasePasLex.SetOnIfDefDirect(const Value: TDirectiveEvent);
begin
  fOnIfDefDirect := Value;
end;

procedure TmwBasePasLex.SetOnIfDirect(const Value: TDirectiveEvent);
begin
  FOnIfDirect := Value;
end;

procedure TmwBasePasLex.SetOnIfNDefDirect(const Value: TDirectiveEvent);
begin
  fOnIfNDefDirect := Value;
end;

procedure TmwBasePasLex.SetOnIfOptDirect(const Value: TDirectiveEvent);
begin
  fOnIfOptDirect := Value;
end;

procedure TmwBasePasLex.SetOnIncludeDirect(const Value: TDirectiveEvent);
begin
  fOnIncludeDirect := Value;
end;

procedure TmwBasePasLex.SetOnLibraryDirect(const Value: TDirectiveEvent);
begin
  fOnLibraryDirect := Value;
end;

procedure TmwBasePasLex.SetOnResourceDirect(const Value: TDirectiveEvent);
begin
  fOnResourceDirect := Value;
end;

procedure TmwBasePasLex.SetOnUnDefDirect(const Value: TDirectiveEvent);
begin
  fOnUnDefDirect := Value;
end;

procedure TmwBasePasLex.StringDQProc;
begin
  fTokenID := tokStringConst;
  repeat
    Inc(fRun);
    case getChar(fRun) of
      #0{, #10, #13}:
        begin
          Error('Unterminated string');
          break;
        end;
      #34:
        while (getChar(fRun) = #34) and (getChar(fRun + 1) = #34) do
          Inc(fRun, 2);
    end;
  until (getChar(fRun) = #34);

  if (getChar(fRun) = #34) then
  begin
    Inc(fRun);
    if TokenLen = 3 then
      fTokenID := tokAsciiChar;
  end;
end;

procedure TmwBasePasLex.AmpersandOpProc;
begin
  FTokenID := tokAmpersand;
  Inc(fRun);
  while getChar(fRun) in ['a'..'z', 'A'..'Z','0'..'9'] do
    Inc(fRun);
  FTokenID := tokIdentifier;
end;

end.

