{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	Tokenizer/Parser objects.
}
unit lpparser;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes;

type
  EParserToken = (
    //General
    tk_NULL,
    tk_Unkown,
    tk_Identifier,
    tk_Comment,
    tk_Directive,
    tk_WhiteSpace,
    tk_NewLine,

    //Keywords
    tk_kw_Array,
    tk_kw_Begin,
    tk_kw_Case,
    tk_kw_Const,
    tk_kw_Do,
    tk_kw_DownTo,
    tk_kw_Else,
    tk_kw_End,
    tk_kw_Except,
    tk_kw_External,
    tk_kw_Finally,
    tk_kw_For,
    tk_kw_Forward,
    tk_kw_Function,
    tk_kw_If,
    tk_kw_Of,
    tk_kw_Out,
    tk_kw_Overload,
    tk_kw_Override,
    tk_kw_Packed,
    tk_kw_Private,
    tk_kw_Procedure,
    tk_kw_Program,
    tk_kw_Record,
    tk_kw_Repeat,
    tk_kw_Set,
    tk_kw_Step,
    tk_kw_Then,
    tk_kw_To,
    tk_kw_Try,
    tk_kw_Type,
    tk_kw_Union,
    tk_kw_Until,
    tk_kw_Var,
    tk_kw_While,

    //Operators
    //Same order as lptypes.EOperator
    tk_cmp_Equal,
    tk_cmp_GreaterThan,
    tk_cmp_GreaterThanOrEqual,
    tk_cmp_LessThan,
    tk_cmp_LessThanOrEqual,
    tk_cmp_NotEqual,

    tk_op_Addr,
    tk_op_AND,
    tk_op_Assign,
    tk_op_Deref,
    tk_op_DIV,
    tk_op_Divide,
    tk_op_Dot,
    tk_op_IN,
    tk_op_Index,
    tk_op_Minus,
    tk_op_MOD,
    tk_op_Multiply,
    tk_op_NOT,
    tk_op_OR,
    tk_op_Plus,
    tk_op_Power,
    tk_op_SHL,
    tk_op_SHR,
    tk_op_XOR,

    //Symbols
    tk_sym_BracketClose,
    //tk_sym_BracketOpen,  = tk_op_Index,
    //tk_sym_Caret,        = tk_op_Deref,
    tk_sym_Colon,
    tk_sym_Comma,
    //tk_sym_Dot,          = tk_op_Dot,
    tk_sym_DotDot,
    //tk_sym_Equals,       = tk_cmp_Equal,
    tk_sym_ParenthesisClose,
    tk_sym_ParenthesisOpen,
    tk_sym_SemiColon,

    //Types
    tk_typ_Float,
    tk_typ_Integer,
    tk_typ_Integer_Hex,
    tk_typ_Integer_Bin,
    tk_typ_String,
    tk_typ_Char);
  EParserTokenSet = set of EParserToken;

  PTokenizerState = ^TTokenizerState;
  TTokenizerState = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
    TokStart, Pos: Integer;
    LastTok, Tok: EParserToken;
    InPeek: Boolean;
    DocPos: TDocPos;
  end;

  TLapeTokenizerBase = class;
  TLapeParseDirective = function(Sender: TLapeTokenizerBase): Boolean of object;
  TLapeHandleDirective = function(Sender: TLapeTokenizerBase; Directive, Argument: lpString): Boolean of object;

  TLapeTokenizerBase = class(TLapeBaseClass)
  protected
    FFileName: lpString;
    FLastTok: EParserToken;
    FTok: EParserToken;
    FTokStart: Integer;
    FPos: Integer;
    FDocPos: TDocPos;
    FLen: Integer;
    FInPeek: Boolean;

    FOnParseDirective: TLapeParseDirective;
    FOnHandleDirective: TLapeHandleDirective;

    function Compare(Key: lpString): Boolean; virtual; abstract;
    function Identify: EParserToken; virtual;
    function HandleDirective: Boolean; virtual;

    function setTok(ATok: EParserToken): EParserToken; virtual;
    function getTokString: lpString; virtual; abstract;
    function getTokInt: Integer; virtual;
    function getTokInt64: Int64; virtual;
    function getTokFloat: Extended; virtual;
    function getTokChar: WideChar; virtual;
    function getTokLen: Integer; virtual;
    function getCurChar: lpChar; virtual;
    procedure setPos(APos: Integer); virtual;
    function getDocPos: TDocPos;
  public
    constructor Create(AFileName: lpString = ''); reintroduce; virtual;
    procedure Reset(ClearDoc: Boolean = False); virtual;
    function getState: Pointer; virtual;
    procedure setState(const State: Pointer; DoFreeState: Boolean = True); virtual;
    procedure freeState(const State: Pointer); virtual;
    function getChar(Offset: Integer = 0): lpChar; virtual; abstract;
    function tempRollBack: EParserToken; virtual;

    function Next: EParserToken; virtual;
    function NextNoWhiteSpace: EParserToken; virtual;
    function NextNoJunk: EParserToken; virtual;
    function Peek: EParserToken; virtual;
    function PeekNoWhiteSpace: EParserToken; virtual;
    function PeekNoJunk: EParserToken; virtual;
    function Expect(Token: EParserToken; NextBefore: Boolean = True; NextAfter: Boolean = False): EParserToken; overload; virtual;
    function Expect(Tokens: EParserTokenSet; NextBefore: Boolean = True; NextAfter: Boolean = False): EParserToken; overload; virtual;

    property FileName: lpString read FFileName;
    property LastTok: EParserToken read FLastTok;
    property Tok: EParserToken read FTok;
    property TokString: lpString read getTokString;
    property TokInteger: Integer read getTokInt;
    property TokInt64: Int64 read getTokInt64;
    property TokFloat: Extended read getTokFloat;
    property TokChar: WideChar read getTokChar;
    property TokStart: Integer read FTokStart;
    property TokLen: Integer read getTokLen;
    property CurChar: lpChar read getCurChar;
    property Pos: Integer read FPos write setPos;
    property DocPos: TDocPos read getDocPos;
    property Len: Integer read FLen;
    property InPeek: Boolean read FInPeek;
  published
    property OnParseDirective: TLapeParseDirective read FOnParseDirective write FOnParseDirective;
    property OnHandleDirective: TLapeHandleDirective read FOnHandleDirective write FOnHandleDirective;
  end;

  TLapeTokenizerString = class(TLapeTokenizerBase)
  protected
    FDoc: lpString;
    function Compare(Key: lpString): Boolean; override;
    function getTokString: lpString; override;
    procedure setDoc(const ADoc: lpString); virtual;
  public
    procedure Reset(ClearDoc: Boolean = False); override;
    constructor Create(ADoc: lpString; AFileName: lpString = ''); reintroduce; virtual;
    function getChar(Offset: Integer = 0): lpChar; override;
  published
    property Doc: lpString read FDoc write setDoc;
  end;

  TLapeTokenizerFile = class(TLapeTokenizerString)
  public
    constructor Create(AFileName: lpString = ''); reintroduce; virtual;
  end;

  TLapeKeyword = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
    Keyword: lpString;
    Token: EParserToken;
  end;

const
  tk_sym_BracketOpen = tk_op_Index;
  tk_sym_Caret = tk_op_Deref;
  tk_sym_Dot = tk_op_Dot;
  tk_sym_Equals = tk_cmp_Equal;

  TokWhiteSpace = [tk_WhiteSpace, tk_NewLine];
  TokJunk = TokWhiteSpace + [tk_Comment, tk_Directive];

  ParserToken_FirstOperator = tk_cmp_Equal;
  ParserToken_LastOperator = tk_op_XOR;
  ParserToken_Operators = [ParserToken_FirstOperator..ParserToken_LastOperator];
  ParserToken_Keywords = [tk_kw_Array..tk_kw_While];
  ParserToken_Symbols = [tk_sym_BracketClose..tk_sym_SemiColon];
  ParserToken_Types = [tk_typ_Float..tk_typ_Char];

  Lape_Keywords: array[0..43] of TLapeKeyword = (
      (Keyword: 'AND';          Token: tk_op_AND),
      (Keyword: 'DIV';          Token: tk_op_DIV),
      (Keyword: 'IN';           Token: tk_op_IN),
      (Keyword: 'MOD';          Token: tk_op_MOD),
      (Keyword: 'NOT';          Token: tk_op_NOT),
      (Keyword: 'OR';           Token: tk_op_OR),
      (Keyword: 'SHL';          Token: tk_op_SHL),
      (Keyword: 'SHR';          Token: tk_op_SHR),
      (Keyword: 'XOR';          Token: tk_op_XOR),

      (Keyword: 'ARRAY';        Token: tk_kw_Array),
      (Keyword: 'BEGIN';        Token: tk_kw_Begin),
      (Keyword: 'CASE';         Token: tk_kw_Case),
      (Keyword: 'CONST';        Token: tk_kw_Const),
      (Keyword: 'DO';           Token: tk_kw_Do),
      (Keyword: 'DOWNTO';       Token: tk_kw_DownTo),
      (Keyword: 'ELSE';         Token: tk_kw_Else),
      (Keyword: 'END';          Token: tk_kw_End),
      (Keyword: 'EXCEPT';       Token: tk_kw_Except),
      (Keyword: 'EXTERNAL';     Token: tk_kw_External),
      (Keyword: 'FINALLY';      Token: tk_kw_Finally),
      (Keyword: 'FOR';          Token: tk_kw_For),
      (Keyword: 'FORWARD';      Token: tk_kw_Forward),
      (Keyword: 'FUNCTION';     Token: tk_kw_Function),
      (Keyword: 'IF';           Token: tk_kw_If),
      (Keyword: 'OF';           Token: tk_kw_Of),
      (Keyword: 'OUT';          Token: tk_kw_Out),
      (Keyword: 'OVERLOAD';     Token: tk_kw_Overload),
      (Keyword: 'OVERRIDE';     Token: tk_kw_Override),
      (Keyword: 'PACKED';       Token: tk_kw_Packed),
      (Keyword: 'PRIVATE';      Token: tk_kw_Private),
      (Keyword: 'PROCEDURE';    Token: tk_kw_Procedure),
      (Keyword: 'PROGRAM';      Token: tk_kw_Program),
      (Keyword: 'RECORD';       Token: tk_kw_Record),
      (Keyword: 'REPEAT';       Token: tk_kw_Repeat),
      (Keyword: 'SET';          Token: tk_kw_Set),
      (Keyword: 'STEP';         Token: tk_kw_Step),
      (Keyword: 'THEN';         Token: tk_kw_Then),
      (Keyword: 'TO';           Token: tk_kw_To),
      (Keyword: 'TRY';          Token: tk_kw_Try),
      (Keyword: 'TYPE';         Token: tk_kw_Type),
      (Keyword: 'UNION';        Token: tk_kw_Union),
      (Keyword: 'UNTIL';        Token: tk_kw_Until),
      (Keyword: 'VAR';          Token: tk_kw_Var),
      (Keyword: 'WHILE';        Token: tk_kw_While)
    );

var
  {$IFDEF Lape_DoubleKeywordsCache}
  Lape_KeywordsCache: array[2..10, Byte] of array of TLapeKeyword;
  {$ELSE}
  Lape_KeywordsCache: array[Byte] of array of TLapeKeyword;
  {$ENDIF}

function LapeTokenToString(Token: EParserToken): lpString; {$IFDEF Lape_Inline}inline;{$ENDIF}
function ParserTokenToOperator(Token: EParserToken): EOperator; {$IFDEF Lape_Inline}inline;{$ENDIF}
function StrToFloatDot(s: lpString): Extended; {$IFDEF Lape_Inline}inline;{$ENDIF}
function StrToFloatDotDef(s: lpString; Default: Extended): Extended; {$IFDEF Lape_Inline}inline;{$ENDIF}
function DetermineIntType(s: lpString): ELapeBaseType; overload;
function DetermineIntType(i: Int64): ELapeBaseType; overload;

implementation

uses
  typinfo,
  lpexceptions;

function LapeTokenToString(Token: EParserToken): lpString;
begin
  Result := getEnumName(TypeInfo(EParserToken), Ord(Token));
  if (Token in ParserToken_Symbols + ParserToken_Types) then
    Delete(Result, 1, 7)
  else if (Token in ParserToken_Keywords) then
    Delete(Result, 1, 6)
  else
    Delete(Result, 1, 3);
end;

function ParserTokenToOperator(Token: EParserToken): EOperator;
begin
  if (not (Token in ParserToken_Operators)) then
    Result := op_Unknown
  else
    Result := EOperator(Integer(Token) - Integer(ParserToken_FirstOperator) + 1);
end;

function StrToFloatDot(s: lpString): Extended;
begin
  Result := StrToFloat(StringReplace(s, '.', DecimalSeparator, []));
end;

function StrToFloatDotDef(s: lpString; Default: Extended): Extended;
begin
  Result := StrToFloatDef(StringReplace(s, '.', DecimalSeparator, []), Default);
end;

function DetermineIntType(s: lpString): ELapeBaseType;

  function PadZ(s: lpString; Len: Integer): lpString;
  begin
    if (Length(s) >= Len) then
      Result := s
    else
      Result := StringOfChar('0', Len - Length(s));
  end;

  function PadComp(s1, s2: lpString): Integer;
  var
    l1, l2: Integer;
  begin
    l1 := Length(s1);
    if (l1 > 0) and (s1[1] = '-') then
    begin
      Delete(s1, 1, 1);
      Dec(l1);
    end;

    l2 := Length(s2);
    if (l2 > 0) and (s2[1] = '-') then
    begin
      Delete(s2, 1, 1);
      Dec(l2);
    end;
    if (l2 > l1) then
      l1 := l2;

    Result := CompareStr(PadZ(s1, l1), PadZ(s2, l1));
  end;

var
  Negative: Boolean;
begin
  s := StringReplace(s, ' ', '', [rfReplaceAll]);
  if (Length(s) < 1) then
    Exit(ltUnknown);
  Negative := (s[1] = '-');
  if Negative then
    Delete(s, 1, 1);
  if (Length(s) < 1) then
    Exit(ltUnknown);

  if (s[1] = '0') or (((not Negative) and (PadComp(s, IntToStr(High(Int8))) <= 0)) or (Negative and (PadComp(s, IntToStr(Low(Int8))) <= 0))) then
    Result := ltInt8
  else if (not Negative) and (PadComp(s, IntToStr(High(UInt8))) <= 0) then
    Result := ltUInt8
  else if ((not Negative) and (PadComp(s, IntToStr(High(Int16))) <= 0)) or (Negative and (PadComp(s, IntToStr(Low(Int16))) <= 0)) then
    Result := ltInt16
  else if (not Negative) and (PadComp(s, IntToStr(High(UInt16))) <= 0) then
    Result := ltUInt16
  else if ((not Negative) and (PadComp(s, IntToStr(High(Int32))) <= 0)) or (Negative and (PadComp(s, IntToStr(Low(Int32))) <= 0)) then
      Result := ltInt32
  else if (not Negative) and (PadComp(s, IntToStr(High(UInt32))) <= 0) then
    Result := ltUInt32
  else if ((not Negative) and (PadComp(s, IntToStr(High(Int64))) <= 0)) or (Negative {and (PadComp(s, IntToStr(Low(Int64))) <= 0)}) then
    Result := ltInt64
  else {if (not Negative) and (PadComp(s, IntToStr(High(UInt64))) <= 0) then}
    Result := ltUInt64
end;

function DetermineIntType(i: Int64): ELapeBaseType;
begin
  Result := DetermineIntType(IntToStr(i));
end;

function Lape_HashKeyword(const b: lpString): Byte;
var
  i: Integer;
begin
  //b := UpperCase(b);
  Result := Length(b);
  for i := 1 to Length(b) do
    Result := Byte(Result - 128) xor Byte((Ord(b[i]) - 65) shl ((i + 2) mod 8));
end;

function Lape_IsKeyword(b: lpString; out Token: EParserToken): Boolean;
var
  Hash: Byte;
  i: Integer;
  {$IFDEF Lape_DoubleKeywordsCache}
  l: Integer;
  {$ENDIF}
begin
  {$IFDEF Lape_DoubleKeywordsCache}
  l := Length(b);
  if (l < Low(Lape_KeywordsCache)) or (l > High(Lape_KeywordsCache)) then
    Exit(False);
  {$ENDIF}

  b := UpperCase(b);
  Hash := Lape_HashKeyword(b);

  {$IFDEF Lape_DoubleKeywordsCache}
  for i := High(Lape_KeywordsCache[l][Hash]) downto 0 do
    if (AnsiCompareStr(Lape_KeywordsCache[l][Hash][i].Keyword, b) = 0) then
    begin
      Token := Lape_KeywordsCache[l][Hash][i].Token;
      Exit(True);
    end;
  {$ELSE}
  for i := High(Lape_KeywordsCache[Hash]) downto 0 do
    if (AnsiCompareStr(Lape_KeywordsCache[Hash][i].Keyword, b) = 0) then
    begin
      Token := Lape_KeywordsCache[Hash][i].Token;
      Exit(True);
    end;
  {$ENDIF}
  Result := False;
end;

procedure Lape_InitKeywordsCache;
var
  Hash: Byte;
  i: Integer;
  {$IFDEF Lape_DoubleKeywordsCache}
  ii, Len: Integer;
  {$ENDIF}
begin
  {$IFDEF Lape_DoubleKeywordsCache}
  for i := Low(Lape_KeywordsCache) to High(Lape_KeywordsCache) do
    for ii := Low(Lape_KeywordsCache[i]) to High(Lape_KeywordsCache[i]) do
      SetLength(Lape_KeywordsCache[i][ii], 0);

  for i := Low(Lape_Keywords) to High(Lape_Keywords) do
  begin
    Hash := Lape_HashKeyword(UpperCase(Lape_Keywords[i].Keyword));
    Len := Length(Lape_Keywords[i].Keyword);
    SetLength(Lape_KeywordsCache[Len][Hash], Length(Lape_KeywordsCache[Len][Hash]) + 1);
    Lape_KeywordsCache[Len][Hash][High(Lape_KeywordsCache[Len][Hash])] := Lape_Keywords[i];
  end;
  {$ELSE}
  for i := Low(Lape_KeywordsCache) to High(Lape_KeywordsCache) do
    SetLength(Lape_KeywordsCache[i], 0);

  for i := Low(Lape_Keywords) to High(Lape_Keywords) do
  begin
    Hash := Lape_HashKeyword(UpperCase(Lape_Keywords[i].Keyword));
    SetLength(Lape_KeywordsCache[Hash], Length(Lape_KeywordsCache[Hash]) + 1);
    Lape_KeywordsCache[Hash][High(Lape_KeywordsCache[Hash])] := Lape_Keywords[i];
  end;
  {$ENDIF}
end;

function TLapeTokenizerBase.Identify: EParserToken;
var
  c: lpChar;
  s: lpString;
  Token: EParserToken;
begin
  FTokStart := FPos;
  c := CurChar;

  case c of
    #0: Result := setTok(tk_NULL);
    #9, #32:
      begin
        while (getChar(1) in [#9, #32]) do
          Inc(FPos);
        Result := setTok(tk_WhiteSpace);
      end;
    #10: Result := setTok(tk_NewLine);
    #13:
      begin
        if (getChar(1) = #10) then
          Inc(FPos);
        Result := setTok(tk_NewLine);
      end;

    //Compare Operators
    '=': Result := setTok(tk_sym_Equals);
    '>':
      begin
        if (getChar(1) = '=') then
        begin
          Result := setTok(tk_cmp_GreaterThanOrEqual);
          Inc(FPos);
        end
        else
          Result := setTok(tk_cmp_GreaterThan);
      end;
    '<':
      begin
        case getChar(1) of
          '=':
            begin
              Result := setTok(tk_cmp_LessThanOrEqual);
              Inc(FPos);
            end;
          '>':
            begin
              Result := setTok(tk_cmp_NotEqual);
              Inc(FPos);
            end;
          else
            Result := setTok(tk_cmp_LessThan);
        end;
      end;

    //Operators
    ':':
      begin
        if (getChar(1) = '=') then
        begin
          Result := setTok(tk_op_Assign);
          Inc(FPos);
        end
        else
          Result := setTok(tk_sym_Colon);
      end;
    '/':
      begin
        if (getChar(1) = '/') then
        begin
          Inc(FPos);
          while (not (getChar(1) in [#13, #10, #0])) do Inc(FPos);
          Result := setTok(tk_Comment);
        end
        else
          Result := setTok(tk_op_Divide);
      end;
    '-': Result := setTok(tk_op_Minus);
    '*':
      begin
        if (getChar(1) = '*') then
        begin
          Result := setTok(tk_op_Power);
          Inc(FPos);
        end
        else
          Result := setTok(tk_op_Multiply);
      end;
    '+': Result := setTok(tk_op_Plus);
    '@': Result := setTok(tk_op_Addr);
    '^': Result := setTok(tk_sym_Caret);

    //Symbols
    ']': Result := setTok(tk_sym_BracketClose);
    '[': Result := setTok(tk_sym_BracketOpen);
    ',': Result := setTok(tk_sym_Comma);
    '.':
      begin
        if (getChar(1) = '.') then
        begin
          Inc(FPos);
          Result := setTok(tk_sym_DotDot);
        end
        else
          Result := setTok(tk_op_Dot);
      end;
    ')': Result := setTok(tk_sym_ParenthesisClose);
    '(':
      begin
        if (getChar(1) = '*') then
        begin
          Inc(FPos);
          while (not ((getChar(1) in ['*', #0]) and (getChar(2) in [')', #0]))) do Inc(FPos);
          Result := setTok(tk_Comment);
        end
        else
          Result := setTok(tk_sym_ParenthesisOpen);
      end;
    ';': Result := setTok(tk_sym_SemiColon);
    '{':
      begin
        Inc(FPos);
        if (CurChar = '$') then
        begin
          HandleDirective();
          Result := setTok(tk_Directive);
        end
        else
        begin
          while (not (CurChar in ['}', #0])) do Inc(FPos);
          Result := setTok(tk_Comment);
        end;
      end;

    //Integer and Float
    '0'..'9':
      begin
        c := getChar(1);
        while (c in ['0'..'9']) do
        begin
          Inc(FPos);
          c := getChar(1);
        end;
        if (c <> '.') or (not (getChar(2) in ['0'..'9'])) then
          Result := setTok(tk_typ_Integer)
        else
        begin
          Inc(FPos, 2);
          while (getChar(1) in ['0'..'9']) do
            Inc(FPos);
          Result := setTok(tk_typ_Float);
        end;
      end;
    '$':
      begin
        if (getChar(1) in ['0'..'9', 'A'..'F', 'a'..'f']) then
        begin
          Inc(FPos);
          while (getChar(1) in ['0'..'9', 'A'..'F', 'a'..'f']) do Inc(FPos);
          Result := setTok(tk_typ_Integer_Hex);
        end
        else
          Result := setTok(tk_Unkown);
      end;
    '%':
      begin
        if (getChar(1) in ['0'..'1']) then
        begin
          Inc(FPos);
          while (getChar(1) in ['0'..'1']) do Inc(FPos);
          Result := setTok(tk_typ_Integer_Bin);
        end
        else
          Result := setTok(tk_Unkown);
      end;
    'A'..'Z', '_', 'a'..'z':
      begin
        s := c;
        c := getChar(1);
        while (c in ['0'..'9', 'A'..'Z', '_', 'a'..'z']) do
        begin
          Inc(FPos);
          s := s + c;
          c := getChar(1);
        end;

        if Lape_IsKeyword(s, Token) then
          Result := setTok(Token)
        else
          Result := setTok(tk_Identifier);
      end;
    #39:
      begin
        Inc(FPos);
        while (not (CurChar in [#39, #0])) do Inc(FPos);
        Result := setTok(tk_typ_String);
      end;
    '#':
      begin
        Inc(FPos);
        case CurChar of
          '0'..'9':
            begin
              while (getChar(1) in ['0'..'9']) do Inc(FPos);
              Result := setTok(tk_typ_Char);
            end;
          '$':
            begin
              if (getChar(1) in ['0'..'9', 'A'..'F', 'a'..'f']) then
              begin
                Inc(FPos);
                while (getChar(1) in ['0'..'9', 'A'..'F', 'a'..'f']) do Inc(FPos);
                Result := setTok(tk_typ_Char);
              end
              else
                Result := setTok(tk_Unkown);
            end;
          else
            Result := setTok(tk_Unkown);
        end;
      end;
    else
    begin
      Result := setTok(tk_Unkown);
    end;
  end;
end;

function TLapeTokenizerBase.HandleDirective: Boolean;
var
  d, a: lpString;
begin
  try
    if ({$IFNDEF FPC}@{$ENDIF}FOnParseDirective <> nil) then
      if FOnParseDirective(Self) then
        Exit(True);

    if ({$IFNDEF FPC}@{$ENDIF}FOnHandleDirective <> nil) then
    begin
      Next();
      Expect([tk_Identifier] + ParserToken_Keywords, False, False);
      d := TokString;

      NextNoWhiteSpace();
      if (CurChar = '}') then
        a := ''
      else
      begin
        while (not (getChar(1) in ['}', #0])) do Inc(FPos);
        a := TokString;
        Inc(FPos);
      end;

      if FOnHandleDirective(Self, d, a) then
        Exit(True);
    end
    else
      while (not (CurChar in ['}', #0])) do Inc(FPos);

    Result := False;
  finally
    if (CurChar <> '}') then
      LapeException(lpeExpectedOther, [LapeTokenToString(FTok), '}'], DocPos);
  end;
end;

function TLapeTokenizerBase.setTok(ATok: EParserToken): EParserToken;
begin
  FLastTok := FTok;
  FTok := ATok;
  Result := ATok;
end;

function TLapeTokenizerBase.getTokInt: Integer;
begin
  Result := getTokInt64();
end;

function TLapeTokenizerBase.getTokInt64: Int64;
  function Bin2Dec(s: lpString): Int64;
  var
    i: Integer;
  begin
    Result := 0;
    if (Length(s) > 0) and (s[1] <> '%') then
      s := '%' + s;
    for i := 2 to Length(s) do
      Result := (Result shl 1) + Int64(Ord(s[i])) - Int64(Ord('0'));
  end;
begin
  case FTok of
    {StrToInt should take care of hex!
    tk_typ_Integer_Hex: Result := Hex2Dec(getTokString());}
    tk_typ_Integer_Bin: Result := Bin2Dec(getTokString());
    else Result := StrToInt64Def(getTokString(), -1);
  end;
end;

function TLapeTokenizerBase.getTokFloat: Extended;
begin
  Result := StrToFloatDotDef(getTokString(), -1);
end;

function TLapeTokenizerBase.getTokChar: WideChar;
var
  s: lpString;
begin
  s := getTokString();
  Delete(s, 1, 1);
  Result := Chr(StrToIntDef(s, 0));
end;

function TLapeTokenizerBase.getTokLen: Integer;
begin
  Result := FPos - FTokStart + 1;
end;

function TLapeTokenizerBase.getCurChar: lpChar;
begin
  Result := getChar(0);
end;

procedure TLapeTokenizerBase.setPos(APos: Integer);
begin
  Assert(APos >= 0);
  Assert(APos < FLen);

  FTokStart := APos;
  FPos := APos - 1;
  Next();
end;

function TLapeTokenizerBase.getDocPos: TDocPos;
begin
  Result.Line := FDocPos.Line;
  if (FDocPos.Col > FTokStart) then
    Result.Col := 0
  else
    Result.Col := FTokStart - FDocPos.Col + 1;
  Result.FileName := FFileName;
end;

constructor TLapeTokenizerBase.Create(AFileName: lpString = '');
begin
  inherited Create();

  FFileName := AFileName;
  FOnParseDirective := nil;
  FOnHandleDirective := nil;

  Reset();
end;

procedure TLapeTokenizerBase.Reset(ClearDoc: Boolean = False);
begin
  FLastTok := tk_NULL;
  FTok := tk_NULL;
  FTokStart := 0;
  FPos := -1;
  FInPeek := False;
  if ClearDoc then
    FLen := 0;

  with FDocPos do
  begin
    Line := 1;
    Col := 0;
  end;
end;

function TLapeTokenizerBase.getState: Pointer;
begin
  New(PTokenizerState(Result));
  with PTokenizerState(Result)^ do
  begin
    TokStart := FTokStart;
    Pos := FPos;
    LastTok := FLastTok;
    Tok := FTok;
    InPeek := FInPeek;
    DocPos := FDocPos;
  end;
end;

procedure TLapeTokenizerBase.setState(const State: Pointer; DoFreeState: Boolean = True);
begin
  with PTokenizerState(State)^ do
  begin
    FTokStart := TokStart;
    FPos := Pos;
    FLastTok := LastTok;
    FTok := Tok;
    FInPeek := InPeek;
    FDocPos := DocPos;
  end;
  if DoFreeState then
    freeState(State);
end;

procedure TLapeTokenizerBase.freeState(const State: Pointer);
begin
  Dispose(PTokenizerState(State));
end;

function TLapeTokenizerBase.TempRollBack: EParserToken;
begin
  Result := FTok;
  FTok := FLastTok;
  FLastTok := tk_NULL;
  FPos := FTokStart - 1;
  FTokStart := FPos;
end;

function TLapeTokenizerBase.Next: EParserToken;
begin
  Inc(FPos);
  if (FTok = tk_NewLine) then
  begin
    FDocPos.Col := FPos;
    Inc(FDocPos.Line);
  end;

  if (FPos < 0) or (FPos >= FLen) then
    Result := setTok(tk_NULL)
  else
    Result := Identify();
end;

function TLapeTokenizerBase.NextNoWhiteSpace: EParserToken;
var
  lTok: EParserToken;
begin
  lTok := FTok;
  repeat
    Result := Next();
  until (not (Result in TokWhiteSpace));
  FLastTok := lTok;
end;

function TLapeTokenizerBase.NextNoJunk: EParserToken;
var
  lTok: EParserToken;
begin
  lTok := FTok;
  repeat
    Result := Next();
  until (not (Result in TokJunk));
  FLastTok := lTok;
end;

function TLapeTokenizerBase.Peek: EParserToken;
var
  p: Pointer;
begin
  p := getState();
  try
    FInPeek := True;
    Result := Next();
  finally
    setState(p);
  end;
end;

function TLapeTokenizerBase.PeekNoWhiteSpace: EParserToken;
var
  p: Pointer;
begin
  p := getState();
  try
    FInPeek := True;
    Result := NextNoWhiteSpace();
  finally
    setState(p);
  end;
end;

function TLapeTokenizerBase.PeekNoJunk: EParserToken;
var
  p: Pointer;
begin
  p := getState();
  try
    FInPeek := True;
    Result := NextNoJunk();
  finally
    setState(p);
  end;
end;

function TLapeTokenizerBase.Expect(Token: EParserToken; NextBefore: Boolean = True; NextAfter: Boolean = False): EParserToken;
begin
  if NextBefore then
    NextNoJunk();
  Result := FTok;
  if (FTok <> Token) then
    LapeException(lpeExpectedOther, [LapeTokenToString(FTok), LapeTokenToString(Token)], DocPos);
  if NextAfter then
    NextNoJunk();
end;

function TLapeTokenizerBase.Expect(Tokens: EParserTokenSet; NextBefore: Boolean = True; NextAfter: Boolean = False): EParserToken;
begin
  if NextBefore then
    NextNoJunk();
  Result := FTok;
  if (not (FTok in Tokens)) then
    LapeException(lpeUnexpectedToken, [LapeTokenToString(FTok)], DocPos);
  if NextAfter then
    NextNoJunk();
end;

function TLapeTokenizerString.Compare(Key: lpString): Boolean;
var
  KeyLen: Integer;
begin
  KeyLen := Length(Key);
  if (FPos < 0) or (FPos >= FLen) or (FPos + KeyLen - 1 >= FLen) then
    Exit(False);

 Result := AnsiSameText(Key, Copy(FDoc, FPos + 1, KeyLen));
 if Result then
   FPos := FPos + KeyLen - 1
end;

function TLapeTokenizerString.getTokString: lpString;
begin
  if (FPos < 0) or (FPos >= FLen) then
    Result := ''
  else
    Result := Copy(FDoc, FTokStart + 1, FPos - FTokStart + 1);
end;

procedure TLapeTokenizerString.setDoc(const ADoc: lpString);
begin
  Reset(True);
  FDoc := ADoc;
  FLen := Length(FDoc);
end;

procedure TLapeTokenizerString.Reset(ClearDoc: Boolean = False);
begin
  inherited;

  if ClearDoc then
    FDoc := '';
end;

constructor TLapeTokenizerString.Create(ADoc: lpString; AFileName: lpString = '');
begin
  inherited Create(AFileName);

  FDoc := ADoc;
  FLen := Length(FDoc);
end;

function TLapeTokenizerString.getChar(Offset: Integer = 0): lpChar;
begin
  if (FPos + Offset < 0) or (FPos + Offset >= FLen) then
    Result := #0
  else
    Result := FDoc[FPos + Offset + 1];
end;

constructor TLapeTokenizerFile.Create(AFileName: lpString = '');
var
  s: TStringList;
begin
  s := TStringList.Create();
  try
    s.LoadFromFile(AFileName);
    inherited Create(s.Text, AFileName);
  finally
    s.Free();
  end;
end;

initialization
  Lape_InitKeywordsCache;
finalization
end.

