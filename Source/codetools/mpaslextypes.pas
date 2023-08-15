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

unit mPasLexTypes;

{$i simba.inc}

interface

uses
  Classes, SysUtils, TypInfo;

type
  TCommentState = (csAnsi, csBor, csNo);

  TTokenPoint = packed record
    X: integer;
    Y: integer;
  end;

  TptTokenKind = (
    tokUnknown,

    tokAbsolute,
    tokAbstract,
    tokAdd,
    tokAddressOp,
    tokAmpersand,
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
    tokBreak,
    tokCase,
    tokCdecl,
    tokClass,
    tokClassForward,
    tokClassFunction,
    tokClassProcedure,
    tokColon,
    tokComma,
    tokCompilerDirective,
    tokConst,
    tokConstRef,
    tokConstructor,
    tokContinue,
    tokCRLF,
    tokCRLFCo,
    tokDefault,
    tokDefineDirect,
    tokDeprecated,
    tokDestructor,
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
    tokExit,
    tokExport,
    tokExports,
    tokExternal,
    tokFinal,
    tokFinalization,
    tokFinally,
    tokFloat,
    tokFor,
    tokForward,
    tokFunction,
    tokGoto,
    tokGreater,
    tokGreaterEqual,
    tokHalt,
    tokHelper,
    tokIdentifier,
    tokIf,
    tokIfDirect,
    tokElseIfDirect,
    tokIfDefDirect,
    tokIfNDefDirect,
    tokIfOptDirect,
    tokIDEDirective,
    tokImplementation,
    tokImplements,
    tokIn,
    tokIncludeDirect,
    tokIncludeOnceDirect,
    tokLibraryDirect,
    tokIndex,
    tokInitialization,
    tokInline,
    tokIntegerConst,
    tokInterface,
    tokIs,
    tokLabel,
    tokLibrary,
    tokLower,
    tokLowerEqual,
    tokMessage,
    tokMinus,
    tokMod,
    tokName,
    tokNoDefault,
    tokNot,
    tokNotEqual,
    tokNull,
    tokObject,
    tokOf,
    tokOn,
    tokOperator,
    tokOr,
    tokOut,
    tokOverload,
    tokOverride,
    tokPackage,
    tokPacked,
    tokPlatform,
    tokPlus,
    tokPoint,
    tokPointerSymbol,
    tokPrivate,
    tokProcedure,
    tokProgram,
    tokProperty,
    tokProtected,
    tokPublic,
    tokPublished,
    tokRaise,
    tokRead,
    tokRecord,
    tokUnion,
    tokRegister,
    tokReintroduce,
    tokRepeat,
    tokRequires,
    tokRoundClose,
    tokRoundOpen,
    tokRunError,
    tokSafeCall,
    tokSealed,
    tokSemiColon,
    tokSet,
    tokShl,
    tokShr,
    tokSlash,
    tokSlashesComment,
    tokSpace,
    tokSquareClose,
    tokSquareOpen,
    tokStar,
    tokStarStar,
    tokStatic,
    tokStdcall,
    tokStored,
    tokStrict,
    tokStringConst,
    tokSymbol,
    tokThen,
    tokThreadvar,
    tokTo,
    tokTry,
    tokType,
    tokUndefDirect,
    tokUnit,
    tokUnsafe,
    tokUntil,
    tokUses,
    tokVar,
    tokWrite,
    tokVirtual,
    tokWhile,
    tokWith,
    tokXor,
    tokNative,
    tokDivAsgn,
    tokMulAsgn,
    tokPlusAsgn,
    tokMinusAsgn,
    tokPowAsgn
    //tok_DONE
  );

  TptTokenSet = set of TptTokenKind;

type
  // "Perfect Hashing"
  // A hashtable with no collisions.
  TKeywordDictionary = class(TObject)
  protected
  const
    SEED = 123;
  protected
  type
    TBucketArray = array of record Key: String; Value: TptTokenKind; end;
  protected
    FBuckets: TBucketArray;
    FSize: UInt32;
    FCount: Integer;
    FItems: TBucketArray;

    function Hash(Key: PChar): UInt32;

    procedure Build;

    procedure setValue(const Key: PChar; const Value: TptTokenKind);
    function getValue(const Key: PChar): TptTokenKind;
  public
    InvalidVal: TptTokenKind;

    constructor Create(InvalidValue: TptTokenKind; Size: Integer = 2048);

    property Value[Key: PChar]: TptTokenKind read getValue write setValue; default;
  end;

var
  KeywordDictionary: TKeywordDictionary = nil;

const
  KeywordTokens = [
    tokAdd, tokIf, tokDo, tokAnd, tokAs, tokOf, tokAt, tokEnd, tokIn, tokCdecl, tokRead, tokCase, tokIs, tokOn, tokLabel, tokMod, tokOr, tokTo, tokDiv, tokBegin, tokBreak, tokFor, tokShl, tokPacked, tokVar, tokElse, tokFinal, tokSet, tokShr, tokSealed, tokThen, tokNot, tokRaise, tokEnum, tokClass, tokObject, tokIndex, tokOut, tokWhile, tokXor, tokGoto, tokExit, tokSafecall, tokWith, tokPublic, tokArray, tokTry, tokRecord, tokInline, tokHelper, tokUses, tokUnit, tokRepeat, tokType, tokDefault, tokMessage, tokStdcall, tokConst, tokNative, tokStatic, tokExcept, tokUnion, tokWrite, tokUntil, tokFinally, tokStored, tokInterface, tokDeprecated, tokAbstract, tokLibrary, tokForward, tokProgram, tokStrict, tokDownto, tokPrivate, tokOverload, tokAbsolute, tokOverride, tokPublished, tokThreadvar, tokExport, tokExternal, tokConstref, tokRegister, tokPlatform, tokContinue, tokFunction, tokVirtual, tokProcedure, tokProtected, tokOperator, tokExports, tokImplements, tokReintroduce, tokProperty, tokFinalization, tokDestructor, tokConstructor, tokImplementation, tokInitialization
  ];

  ExTokens = [
    tokAdd, tokAt, tokCdecl, tokRead, tokOn, tokBreak, tokFinal, tokIndex, tokOut, tokExit, tokSafecall, tokPublic, tokHelper, tokDefault, tokMessage, tokStdcall, tokStatic, tokWrite, tokStored, tokDeprecated, tokAbstract, tokForward, tokPrivate, tokOverload, tokAbsolute, tokOverride, tokPublished, tokExport, tokExternal, tokRegister, tokPlatform, tokContinue, tokVirtual, tokProtected, tokImplements, tokReintroduce
  ];

  JunkTokens = [
    tokAnsiComment, tokBorComment, tokSlashesComment,
    tokCRLF, tokCRLFCo, tokSpace,
    tokIfDirect, tokElseIfDirect, tokIfDefDirect, tokIfNDefDirect, tokEndIfDirect, tokIfOptDirect, tokUndefDirect
  ];

const
  MaxTokenNameLength: Integer = 0;

function TokenName(const Value: TptTokenKind): String;
function TokenNames(const Value: TptTokenSet): String;

procedure SetupKeywordDictionary;

implementation

uses
  simba.ide_initialization;

function TokenName(const Value: TptTokenKind): String;
begin
  Result := Copy(GetEnumName(TypeInfo(TptTokenKind), Ord(Value)), 4);
end;

function TokenNames(const Value: TptTokenSet): String;
var
  Tok: TptTokenKind;
begin
  Result := '[';
  for Tok in Value do
  begin
    if (Result <> '[') then
      Result := Result + ', ';
    Result := Result + TokenName(Tok);
  end;
  Result := Result + ']';
end;

constructor TKeywordDictionary.Create(InvalidValue: TptTokenKind; Size: Integer);
begin
  inherited Create();

  FSize := Size - 1;
  SetLength(FBuckets, FSize + 1);

  InvalidVal := InvalidValue;
end;

// Fowler–Noll–Vo
function TKeywordDictionary.Hash(Key: PChar): UInt32; inline;
begin
  {$UNDEF REDO_Q}{$IFOPT Q+}{$Q-}{$DEFINE REDO_Q}{$ENDIF}
  {$UNDEF REDO_R}{$IFOPT R+}{$R-}{$DEFINE REDO_R}{$ENDIF}
  Result := SEED;
  while (Key^ <> #0) do
  begin
    Result := (Result xor Byte(Key^)) * 16777619;

    Inc(Key);
  end;
  Result := Result and FSize;
  {$IFDEF REDO_Q}{$Q+}{$ENDIF}
  {$IFDEF REDO_R}{$R+}{$ENDIF}
end;

procedure TKeywordDictionary.setValue(const Key: PChar; const Value: TptTokenKind);
begin
  SetLength(FItems, Length(FItems) + 1);

  FItems[High(FItems)].Value := Value;
  FItems[High(FItems)].Key := Key;
end;

function TKeywordDictionary.getValue(const Key: PChar): TptTokenKind;

  function ExactStr(Left, Right: PChar): Boolean; inline;
  begin
    while (Left^ = Right^) and (Left^ <> #0) and (Right^ <> #0) do
    begin
      Inc(Left);
      Inc(Right);
    end;

    Result := (Left^ = Right^);
  end;

var
  Bucket: Integer;
begin
  if (Length(FItems) <> FCount) then
    raise Exception.Create('TKeywordDictionary: Need rebuild');

  Bucket := Self.Hash(Key);
  if ExactStr(PChar(FBuckets[Bucket].Key), Key) then
    Result := FBuckets[Bucket].Value
  else
    Result := InvalidVal;
end;

procedure TKeywordDictionary.Build;
var
  HashList: TStringList;
  i, Bucket: Integer;
  HashValue: String;
begin
  FCount := Length(FItems);

  HashList := TStringList.Create();
  HashList.Sorted := True;

  while (HashList.Count < FCount) do
  begin
    HashList.Clear();

    for i := 0 to High(FItems) do
    begin
      HashValue := IntToStr(Self.Hash(PChar(FItems[I].Key)));
      if (HashList.IndexOf(HashValue) > -1) then
        raise Exception.Create('TKeywordDictionary: Different seed (or more buckets) required');

      HashList.Add(HashValue);
    end;
  end;

  HashList.Free();

  for i := 0 to High(FItems) do
  begin
    Bucket := Self.Hash(PChar(FItems[I].Key));

    FBuckets[Bucket].Key := FItems[I].Key;
    FBuckets[Bucket].Value := FItems[I].Value;
  end;
end;

procedure SetupKeywordDictionary;
var
  Token: TptTokenKind;
  Str: String;
begin
  KeywordDictionary := TKeywordDictionary.Create(tokIdentifier);

  for Token in TptTokenKind do
  begin
    Str := LowerCase(TokenName(Token));
    if (Length(Str) > MaxTokenNameLength) then
      MaxTokenNameLength := Length(Str);

    if (Token in KeywordTokens) then
      KeywordDictionary[PChar(Str)] := Token;
  end;

  KeywordDictionary.Build();
end;

initialization
  SimbaIDEInitialization.RegisterMethodOnBeforeCreate(@SetupKeywordDictionary, 'Keyword Dictionary');

finalization
  if (KeywordDictionary <> nil) then
    FreeAndNil(KeywordDictionary);

end.
