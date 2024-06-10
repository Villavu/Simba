{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_codetools_paslexer;

{$i simba.inc}

interface

uses
  SysUtils, Classes, TypInfo,
  simba.base,
  simba.containers,
  simba.ide_codetools_base;

type
  ELexerToken = (
    tokUnknown,

    tokAdd,
    tokAddressOp,
    tokAnd,
    tokAnsiComment,
    tokArray,
    tokAs,
    tokAsciiChar,
    tokAssign,
    tokAt,
    tokBegin,
    tokBorComment,
    tokBraceClose,
    tokBraceOpen,
    tokCase,
    tokColon,
    tokComma,
    tokCompilerDirective,
    tokConst,
    tokConstRef,
    tokDefineDirect,
    tokDeprecated,
    tokDiv,
    tokDo,
    tokDotDot,
    tokDoubleAddressOp,
    tokDownto,
    tokElse,
    tokElseDirect,
    tokEnd,
    tokEndIfDirect,
    tokEnum,
    tokEqual,
    tokExcept,
    tokExternal,
    tokFinally,
    tokFloat,
    tokFor,
    tokForward,
    tokFunction,
    tokGoto,
    tokGreater,
    tokGreaterEqual,
    tokIdentifier,
    tokIf,
    tokIfDirect,
    tokElseIfDirect,
    tokIfDefDirect,
    tokIfNDefDirect,
    tokIfOptDirect,
    tokIDEDirective,
    tokIn,
    tokIncludeDirect,
    tokIncludeOnceDirect,
    tokLibraryDirect,
    tokIntegerConst,
    tokIs,
    tokLabel,
    tokLower,
    tokLowerEqual,
    tokMinus,
    tokMod,
    tokNot,
    tokNotEqual,
    tokNull,
    tokObject,
    tokOf,
    tokOperator,
    tokOr,
    tokOut,
    tokOverload,
    tokOverride,
    tokPacked,
    tokPlus,
    tokPoint,
    tokPointerSymbol,
    tokPrivate,
    tokProcedure,
    tokProgram,
    tokProperty,
    tokRaise,
    tokRecord,
    tokUnion,
    tokRepeat,
    tokRoundClose,
    tokRoundOpen,
    tokSemiColon,
    tokSet,
    tokShl,
    tokShr,
    tokSlash,
    tokSlashesComment,
    tokWhiteSpace,
    tokSquareClose,
    tokSquareOpen,
    tokStar,
    tokStarStar,
    tokStatic,
    tokStrict,
    tokStringConst,
    tokThen,
    tokTo,
    tokTry,
    tokType,
    tokUndefDirect,
    tokUntil,
    tokVar,
    tokWhile,
    tokWith,
    tokXor,
    tokNative,
    tokDivAsgn,
    tokMulAsgn,
    tokPlusAsgn,
    tokMinusAsgn,
    tokPowAsgn
  );

  TCommentState = (csAnsi, csBor, csNo);

  TDocPos = record
    Line: Integer;
    Col: Integer;
    FileName: String;
  end;

  TPasLexer = class;
  TDirectiveEvent = procedure(Sender: TPasLexer) of object;

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

  TPasLexer = class(TObject)
  protected
    fAheadLexer: TPasLexer;
    fCommentState: TCommentState;
    fDoc: String;
    fFileName: String;
    fFileAge: Integer;
    fRun: Integer;
    fTokenPos: Integer;
    fLineNumber: Integer;
    fTokenID: ELexerToken;
    fLinePos: Integer;
    fOnCompilerDirective: TDirectiveEvent;
    fOnIDEDirective: TDirectiveEvent;
    fOnElseDirect: TDirectiveEvent;
    fOnEndIfDirect: TDirectiveEvent;
    fOnIfDefDirect: TDirectiveEvent;
    fOnIfNDefDirect: TDirectiveEvent;
    fOnIncludeDirect: TDirectiveEvent;
    fOnLibraryDirect: TDirectiveEvent;
    fOnDefineDirect: TDirectiveEvent;
    fOnIfOptDirect: TDirectiveEvent;
    fOnIfDirect: TDirectiveEvent;
    fOnElseIfDirect: TDirectiveEvent;
	  fOnUnDefDirect: TDirectiveEvent;

    FDirectiveParamOrigin: PAnsiChar;

    FDefines: TStringList;
    FDefineStack: Integer;
    FTopDefineRec: PDefineRec;
    FUseDefines: Boolean;

    FIdentBuffer: PChar;
    FIdentBufferUpper: PtrUInt;

    procedure MaybeHandleCompilerDirective(Name, Value: String);
    procedure MaybeHandleIDEDirective(Name, Value: String);

    function getChar(const Pos: Integer): Char; inline;

    function getDocPos: TDocPos;
    procedure SetRunPos(Value: Integer);
    procedure AddressOpProc;
    procedure AsciiCharProc;
    procedure AnsiProc;
    procedure BorProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure ColonProc;
    procedure CommaProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure IntegerProc;
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
    procedure UnknownProc;
    function GetToken: string;
    function GetTokenLen: Integer;
    function GetCompilerDirective: string;
    function GetDirectiveKind: ELexerToken;
    function GetDirectiveParam: string;
    function GetDirectiveParamOriginal: string;
    function GetDirectiveParamAsFileName: string;
    function GetIsJunk: Boolean;

    procedure EnterDefineBlock(ADefined: Boolean);
    procedure ExitDefineBlock;

    function GetAheadTokenID: ELexerToken;
  public
    constructor Create(Doc: String; AFileName: String = ''); virtual;
    constructor CreateFromFile(AFileName: String); virtual;
    destructor Destroy; override;

    procedure Next;
    procedure NextNoJunk;

    procedure InitAhead;
    procedure AheadNext;
    property AheadTokenID: ELexerToken read GetAheadTokenID;
    property Doc: String read fDoc;

    function CopyDoc(const StartPos, EndPos: Integer): String;

    procedure AddDefine(const ADefine: string);
    procedure RemoveDefine(const ADefine: string);
    procedure ClearDefines;
    procedure CloneDefinesFrom(ALexer: TPasLexer);
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

    property LineNumber: Integer read fLineNumber write fLineNumber;
    property LinePos: Integer read fLinePos write fLinePos;
    property DocPos: TDocPos read GetDocPos;
    property RunPos: Integer read fRun write SetRunPos;
    property Token: string read GetToken;
    property TokenLen: Integer read GetTokenLen;
    property TokenPos: Integer read fTokenPos;
    property TokenID: ELexerToken read FTokenID;

    property OnCompilerDirective: TDirectiveEvent read fOnCompilerDirective write fOnCompilerDirective;
    property OnIDEDirective: TDirectiveEvent read fOnIDEDirective write fOnIDEDirective;
    property OnDefineDirect: TDirectiveEvent read fOnDefineDirect write fOnDefineDirect;
    property OnElseDirect: TDirectiveEvent read fOnElseDirect write fOnElseDirect;
    property OnEndIfDirect: TDirectiveEvent read fOnEndIfDirect write fOnEndIfDirect;
    property OnIfDefDirect: TDirectiveEvent read fOnIfDefDirect write fOnIfDefDirect;
    property OnIfNDefDirect: TDirectiveEvent read fOnIfNDefDirect write fOnIfNDefDirect;
    property OnIfOptDirect: TDirectiveEvent read fOnIfOptDirect write fOnIfOptDirect;
    property OnIncludeDirect: TDirectiveEvent read fOnIncludeDirect write fOnIncludeDirect;
    property OnLibraryDirect: TDirectiveEvent read fOnLibraryDirect write fOnLibraryDirect;
    property OnIfDirect: TDirectiveEvent read fOnIfDirect write fOnIfDirect;
    property OnElseIfDirect: TDirectiveEvent read fOnElseIfDirect write fOnElseIfDirect;
	  property OnUnDefDirect: TDirectiveEvent read fOnUnDefDirect write fOnUnDefDirect;

    property DirectiveParamOrigin: PAnsiChar read FDirectiveParamOrigin;

    property UseDefines: Boolean read FUseDefines write FUseDefines;
    property Defines: TStringList read FDefines;
  end;

  TLexerStack = specialize TSimbaStack<TPasLexer>;
  TLexerList = specialize TSimbaObjectList<TPasLexer>;

const
  KeywordTokens = [
    tokIf, tokDo, tokAnd, tokAs, tokOf, tokEnd, tokIn, tokCase, tokIs, tokLabel, tokMod, tokOr, tokTo, tokDiv, tokBegin, tokFor, tokShl, tokPacked, tokVar, tokElse, tokSet, tokShr, tokThen, tokNot, tokEnum, tokObject, tokOut, tokWhile, tokXor, tokGoto, tokWith, tokArray, tokTry, tokRecord, tokRepeat, tokType, tokConst, tokNative, tokStatic, tokExcept, tokUnion, tokUntil, tokFinally, tokDeprecated, tokForward, tokProgram, tokStrict, tokDownto, tokOverload, tokOverride, tokExternal, tokConstref, tokFunction, tokProcedure, tokOperator, tokProperty, tokRaise
  ];

  JunkTokens = [
    tokAnsiComment, tokBorComment, tokSlashesComment,
    tokWhiteSpace,
    tokIfDirect, tokElseIfDirect, tokIfDefDirect, tokIfNDefDirect, tokEndIfDirect, tokIfOptDirect, tokUndefDirect
  ];

  function TokenName(const Value: ELexerToken): String;

implementation

var
  KeywordDict: specialize TKeywordDictionary<ELexerToken>;

procedure TPasLexer.ClearDefines;
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

procedure TPasLexer.CloneDefinesFrom(ALexer: TPasLexer);
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

function TPasLexer.SaveDefines: TSaveDefinesRec;
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

procedure TPasLexer.LoadDefines(From: TSaveDefinesRec);
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

function TPasLexer.getChar(const Pos: Integer): Char;
begin
  if (Pos >= 1) and (Pos <= Length(fDoc)) then
    Result := fDoc[Pos]
  else
    Result := #0;
end;

function TPasLexer.getDocPos: TDocPos;
begin
  Result.Col := fTokenPos - fLinePos;
  Result.Line := fLineNumber;
  Result.FileName := fFileName;
end;

constructor TPasLexer.Create(Doc: String; AFileName: String = '');
begin
  inherited Create();

  fDoc := Doc;
  fCommentState := csNo;
  fRun := 1;
  fFileName := AFileName;
  if (fFileName <> '') and FileExists(fFileName) then
    fFileAge := SysUtils.FileAge(fFileName);

  FIdentBuffer := GetMem(KeywordDict.MaxKeyLength + 1);
  FIdentBufferUpper := PtrUInt(@FIdentBuffer[KeywordDict.MaxKeyLength]);

  FUseDefines := True;
  FDefines := TStringList.Create();
  FDefines.UseLocale := False;
  FDefines.Duplicates := dupIgnore;
end;

constructor TPasLexer.CreateFromFile(AFileName: String);
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

destructor TPasLexer.Destroy;
begin
  ClearDefines();
  if (FDefines <> nil) then
    FreeAndNil(FDefines);
  if (FIdentBuffer <> nil) then
    FreeMemAndNil(FIdentBuffer);
  if (fAheadLexer <> nil) then
    FreeAndNil(fAheadLexer);

  inherited Destroy();
end;

procedure TPasLexer.SetRunPos(Value: Integer);
begin
  fRun := Value;
  Next;
end;

procedure TPasLexer.AddDefine(const ADefine: string);
begin
  FDefines.Add(ADefine);
end;

procedure TPasLexer.AddressOpProc;
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

procedure TPasLexer.AsciiCharProc;
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

procedure TPasLexer.BraceCloseProc;
begin
  Inc(fRun);
  fTokenId := tokNull;

  //Error('Illegal character');
end;

procedure TPasLexer.MaybeHandleCompilerDirective(Name, Value: String);
begin
  case Name of
    'SCOPEDENUMS', 'S':
      begin
        if (Value = 'ON')  or (Value = '+') then AddDefine('!SCOPEDENUMS') else
        if (Value = 'OFF') or (Value = '-') then RemoveDefine('!SCOPEDENUMS');
      end;

    'EXPLICTSELF':
      begin
        if (Value = 'ON')  then AddDefine('!EXPLICTSELF') else
        if (Value = 'OFF') then RemoveDefine('!EXPLICTSELF');
      end;
  end;
end;

procedure TPasLexer.MaybeHandleIDEDirective(Name, Value: String);
begin
  case Name of
    'CODETOOLS':
      begin
        if (Value = 'OFF') then
          EnterDefineBlock(IsDefined('!CODETOOLS')) // ifdef !CODETOOLS
        else
        if (Value = 'ON') then
          ExitDefineBlock();                        // endif !CODETOOLS
      end;
  end;
end;

procedure TPasLexer.BraceOpenProc;
var
  Param, Def: string;
begin
  case getChar(fRun + 1) of
    '$', '%':
      begin
        BorProc(); // Skip comment
        fTokenID := GetDirectiveKind;
      end;
    else
      FCommentState := csBor;
  end;

  if (fCommentState = csNo) then
  begin
    case fTokenID of
      tokIDEDirective:
        begin
          MaybeHandleIDEDirective(CompilerDirective, DirectiveParam);

          if Assigned(fOnIDEDirective) then
            fOnIDEDirective(Self);
        end;

      tokCompilerDirective:
        begin
          if FUseDefines and (FDefineStack = 0) then
            MaybeHandleCompilerDirective(CompilerDirective, DirectiveParam);

          if Assigned(fOnCompilerDirective) and (FDefineStack = 0) then
            fOnCompilerDirective(Self);
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
          if Assigned(fOnIncludeDirect) and (FDefineStack = 0) and (not IsJunk) then
            fOnIncludeDirect(Self);
        end;
      tokLibraryDirect:
        begin
          if Assigned(fOnLibraryDirect) and (FDefineStack = 0) and (not IsJunk) then
            fOnLibraryDirect(Self);
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

procedure TPasLexer.ColonProc;
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

procedure TPasLexer.CommaProc;
begin
  Inc(fRun);
  fTokenID := tokComma;
end;

procedure TPasLexer.EnterDefineBlock(ADefined: Boolean);
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

procedure TPasLexer.EqualProc;
begin
  Inc(fRun);
  fTokenID := tokEqual;
end;

procedure TPasLexer.ExitDefineBlock;
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

procedure TPasLexer.GreaterProc;
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

procedure TPasLexer.IdentProc;
var
  Ptr: PChar;
begin
  Ptr := FIdentBuffer;
  while (getChar(fRun) in ['_', '0'..'9', 'A'..'Z', 'a'..'z']) do
  begin
    if (PtrUInt(Ptr) < FIdentBufferUpper) then
    begin
      Ptr^ := getChar(fRun);
      if (Ptr^ in [#65..#90]) then // change to lowercase (keyword dict is built that way)
        Ptr^ := Char(Ord(Ptr^) + 32);
      Inc(Ptr);
    end;

    Inc(fRun);
  end;
  Ptr^ := #0;

  if (fRun - fTokenPos <= KeywordDict.MaxKeyLength) then
    fTokenID := KeywordDict[FIdentBuffer]
  else
    fTokenID := tokIdentifier;
end;

procedure TPasLexer.IntegerProc;
begin
  Inc(fRun);
  fTokenID := tokIntegerConst;
  while getChar(fRun) in ['_', '0'..'9', 'A'..'F', 'a'..'f'] do
    Inc(fRun);
end;

function TPasLexer.IsDefined(const ADefine: string): Boolean;
begin
  Result := FDefines.IndexOf(ADefine) > -1;
end;

procedure TPasLexer.LowerProc;
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

procedure TPasLexer.MinusProc;
begin
  Inc(fRun);
  if getChar(fRun) = '=' then
  begin
    Inc(fRun);
    fTokenID := tokMinusAsgn;
  end else
    fTokenID := tokMinus;
end;

procedure TPasLexer.NullProc;
begin
  fTokenID := tokNull;
end;

procedure TPasLexer.NumberProc;
begin
  Inc(fRun);
  fTokenID := tokIntegerConst;
  while getChar(fRun) in ['_', '0'..'9', '.', 'e', 'E'] do
  begin
    case getChar(fRun) of
      '.':
        if getChar(fRun + 1) = '.' then
          Break
        else
          fTokenID := tokFloat
    end;
    Inc(fRun);
  end;
end;

procedure TPasLexer.PlusProc;
begin
  Inc(fRun);
  if getChar(fRun) = '=' then
  begin
    Inc(fRun);
    fTokenID := tokPlusAsgn;
  end else
    fTokenID := tokPlus;
end;

procedure TPasLexer.PointerSymbolProc;
begin
  Inc(fRun);
  fTokenID := tokPointerSymbol;
end;

procedure TPasLexer.PointProc;
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

procedure TPasLexer.RemoveDefine(const ADefine: string);
var
  I: Integer;
begin
  I := FDefines.IndexOf(ADefine);
  if (I > -1) then
    FDefines.Delete(I);
end;

procedure TPasLexer.RoundCloseProc;
begin
  Inc(fRun);
  fTokenID := tokRoundClose;
end;

procedure TPasLexer.AnsiProc;
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

procedure TPasLexer.BorProc;
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

procedure TPasLexer.RoundOpenProc;
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

procedure TPasLexer.SemiColonProc;
begin
  Inc(fRun);
  fTokenID := tokSemiColon;
end;

procedure TPasLexer.SlashProc;
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

procedure TPasLexer.SpaceProc;
begin
  Inc(fRun);
  fTokenID := tokWhiteSpace;
  while getChar(fRun) in [#1..#32] do
    case getChar(fRun) of
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

procedure TPasLexer.SquareCloseProc;
begin
  Inc(fRun);
  fTokenID := tokSquareClose;
end;

procedure TPasLexer.SquareOpenProc;
begin
  Inc(fRun);
  fTokenID := tokSquareOpen;
end;

procedure TPasLexer.StarProc;
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

procedure TPasLexer.StringProc;
begin
  fTokenID := tokStringConst;
  repeat
    Inc(fRun);
    case getChar(fRun) of
      #0, #10, #13:
        begin
          //Error('Unterminated string');
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

procedure TPasLexer.UnknownProc;
begin
  Inc(fRun);
  fTokenID := tokUnknown;
  //Error('Unknown Character');
end;

procedure TPasLexer.Next;
begin
  fTokenPos := fRun;

  case fCommentState of
    csAnsi: AnsiProc;
    csBor: BorProc;
    csNo:
      begin
        case getChar(fRun) of
          #0: NullProc();
          #1..#32: SpaceProc();
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
          '$', '%': IntegerProc();
          else
            UnknownProc();
        end;
    end;
  end;
end;

function TPasLexer.GetIsJunk: Boolean;
begin
  result := (fTokenID in JunkTokens) or (FUseDefines and (FDefineStack > 0) and (TokenID <> tokNull));
end;

function TPasLexer.GetToken: string;
begin
  Result := Copy(FDoc, FTokenPos, GetTokenLen);
end;

function TPasLexer.GetTokenLen: Integer;
begin
  Result := fRun - fTokenPos;
end;

procedure TPasLexer.NextNoJunk;
begin
  repeat
    Next;
  until not IsJunk;
end;

procedure TPasLexer.InitAhead;
begin
  if (fAheadLexer = nil) then
    fAheadLexer := TPasLexer.Create(fDoc, fFileName);

  fAheadLexer.RunPos := RunPos;
  fAheadLexer.fLineNumber := FLineNumber;
  fAheadLexer.fLinePos := FLinePos;
  fAheadLexer.CloneDefinesFrom(Self);
  while fAheadLexer.IsJunk do
    fAheadLexer.Next;
end;

procedure TPasLexer.AheadNext;
begin
  if (fAheadLexer <> nil) then
    fAheadLexer.NextNoJunk;
end;

function TPasLexer.GetAheadTokenID: ELexerToken;
begin
  if (fAheadLexer <> nil) then
    Result := fAheadLexer.TokenID
  else
    Result := tokNull;
end;

function TPasLexer.CopyDoc(const StartPos, EndPos: Integer): String;
begin
  Result := Copy(FDoc, StartPos, EndPos - StartPos);
end;

function TPasLexer.GetCompilerDirective: string;
var
  StartPos, EndPos: Integer;
begin
  if (TokenID in [tokCompilerDirective, tokIDEDirective]) then
  begin
    StartPos := fTokenPos;
    while (not (getChar(StartPos) in [#0, '$', '%'])) do
      Inc(StartPos);
    StartPos := StartPos + 1;
    EndPos := StartPos;
    while (not (getChar(EndPos) in [#0, ' ', '}'])) do
      Inc(EndPos);

    Result := UpperCase(Copy(fDoc, StartPos, EndPos - StartPos));
  end;
end;

function TPasLexer.GetDirectiveKind: ELexerToken;
var
  StartPos, EndPos: Integer;
  Directive: String;
begin
  StartPos := fTokenPos;
  while (not (getChar(StartPos) in [#0, '$', '%'])) do
    Inc(StartPos);

  case getChar(StartPos) of
    '$': Result := tokCompilerDirective;
    '%': Result := tokIDEDirective;
  end;

  StartPos := StartPos + 1;
  EndPos := StartPos;
  while (not (getChar(EndPos) in [#0, ' ', '}'])) do
    Inc(EndPos);

  if (Result = tokCompilerDirective) then
  begin
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
end;

function TPasLexer.GetDirectiveParamOriginal: string;
var
  StartPos, EndPos: Integer;
begin
  StartPos := fTokenPos;
  while (not (getChar(StartPos) in [#0, ' ', '}'])) do
    Inc(StartPos);

  if (getChar(startPos) <> '}') then
  begin
    EndPos := StartPos + 1;
    while (not (getChar(EndPos) in [#0, '}'])) do
      Inc(EndPos);

    Result := Copy(fDoc, StartPos + 1, (EndPos - StartPos) - 1);
  end else
    Result := '';
end;

function TPasLexer.GetDirectiveParam: string;
begin
  Result := UpperCase(GetDirectiveParamOriginal());
end;

function TPasLexer.GetDirectiveParamAsFileName: string;
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

procedure TPasLexer.StringDQProc;
begin
  fTokenID := tokStringConst;
  repeat
    Inc(fRun);
    case getChar(fRun) of
      #0:
        begin
          //Error('Unterminated string');
          Break;
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

function TokenName(const Value: ELexerToken): String;
begin
  Result := Copy(GetEnumName(TypeInfo(ELexerToken), Ord(Value)), 4);
end;

procedure InitKeywordDictionary;
var
  Tok: ELexerToken;
begin
  KeywordDict := specialize TKeywordDictionary<ELexerToken>.Create();
  KeywordDict.Seed := 123;
  KeywordDict.Size := 1024;
  KeywordDict.InvalidVal := tokIdentifier;
  for Tok in KeywordTokens do
    KeywordDict.Add(TokenName(Tok), Tok);
end;

initialization
  InitKeywordDictionary();

end.

