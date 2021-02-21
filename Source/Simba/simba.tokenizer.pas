{
  Author: Niels A.D
  Project: Lape (http://code.google.com/p/la-pe/)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Tokenizer/Parser objects.
}
unit simba.tokenizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  SimbaTokenizerException = class(Exception);

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
    tk_kw_ConstRef,
    tk_kw_Deprecated,
    tk_kw_Do,
    tk_kw_DownTo,
    tk_kw_Else,
    tk_kw_End,
    tk_kw_Except,
    tk_kw_Experimental,
    tk_kw_External,
    tk_kw_Finally,
    tk_kw_For,
    tk_kw_Forward,
    tk_kw_Function,
    tk_kw_If,
    tk_kw_Label,
    tk_kw_Of,
    tk_kw_Object,
    tk_kw_Operator,
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
    tk_kw_Static,
    tk_kw_Then,
    tk_kw_To,
    tk_kw_Try,
    tk_kw_Type,
    tk_kw_Union,
    tk_kw_UnImplemented,
    tk_kw_Until,
    tk_kw_Var,
    tk_kw_While,
    tk_kw_With,

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
    tk_op_AssignDiv,
    tk_op_AssignMinus,
    tk_op_AssignMul,
    tk_op_AssignPlus,
    tk_op_Deref,
    tk_op_DIV,
    tk_op_Divide,
    tk_op_Dot,
    tk_op_IN,
    tk_op_IS,
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
    tk_typ_HereString,
    tk_typ_Char);

  EParserTokenSet = set of EParserToken;

  PDocPos = ^TDocPos;
  TDocPos = record
    Line, Col: UInt32;
    FileName: String;
  end;

  PTokenizerState = ^TTokenizerState;
  TTokenizerState = record
    TokStart, Pos: Integer;
    LastTok, Tok: EParserToken;
    InPeek: Boolean;
    DocPos: TDocPos;
  end;

  TSimbaTokenizerBase = class;
  TSimbaParseDirective = function(Sender: TSimbaTokenizerBase): Boolean of object;
  TSimbaHandleDirective = function(Sender: TSimbaTokenizerBase; Directive, Argument: String): Boolean of object;

  TSimbaTokenizerBase = class
  protected
    FFileName: String;
    FLastTok: EParserToken;
    FTok: EParserToken;
    FTokStart: Integer;
    FPos: Integer;
    FDocPos: TDocPos;
    FLen: Integer;
    FInPeek: Boolean;

    FOnParseDirective: TSimbaParseDirective;
    FOnHandleDirective: TSimbaHandleDirective;

    function Compare(Key: String): Boolean; virtual; abstract;
    function Identify: EParserToken; virtual;
    function HandleDirective: Boolean; virtual;

    function setTok(ATok: EParserToken): EParserToken; virtual;
    function getTokString: String; virtual; abstract;
    function getTokInt: Integer; virtual;
    function getTokUInt64: UInt64; virtual;
    function getTokFloat: Extended; virtual;
    function getTokChar: WideChar; virtual;
    function getTokLen: Integer; virtual;
    function getCurChar: Char; virtual;
    procedure setPos(APos: Integer); virtual;
    function getDocPos: TDocPos;
  public
    OverridePos: PDocPos;
    NullPos: TDocPos;

    constructor Create(AFileName: String = ''); reintroduce; virtual;
    procedure Reset(ClearDoc: Boolean = False); virtual;
    function getState: Pointer; virtual;
    procedure setState(const State: Pointer; DoFreeState: Boolean = True); virtual;
    procedure freeState(const State: Pointer); virtual;
    function getChar(Offset: Integer = 0): Char; virtual; abstract;
    function tempRollBack: EParserToken; virtual;

    function Next: EParserToken; virtual;
    function NextNoWhiteSpace: EParserToken; virtual;
    function NextNoJunk: EParserToken; virtual;
    function Peek: EParserToken; virtual;
    function PeekNoWhiteSpace: EParserToken; virtual;
    function PeekNoJunk: EParserToken; virtual;
    function Expect(Token: EParserToken; NextBefore: Boolean = True; NextAfter: Boolean = False): EParserToken; overload; virtual;
    function Expect(Tokens: EParserTokenSet; NextBefore: Boolean = True; NextAfter: Boolean = False): EParserToken; overload; virtual;

    property FileName: String read FFileName;
    property LastTok: EParserToken read FLastTok;
    property Tok: EParserToken read FTok;
    property TokString: String read getTokString;
    property TokInteger: Integer read getTokInt;
    property TokUInt64: UInt64 read getTokUInt64;
    property TokFloat: Extended read getTokFloat;
    property TokChar: WideChar read getTokChar;
    property TokStart: Integer read FTokStart;
    property TokLen: Integer read getTokLen;
    property CurChar: Char read getCurChar;
    property Pos: Integer read FPos write setPos;
    property Len: Integer read FLen;
    property InPeek: Boolean read FInPeek;

    property DocPos: TDocPos read getDocPos;
  published
    property OnParseDirective: TSimbaParseDirective read FOnParseDirective write FOnParseDirective;
    property OnHandleDirective: TSimbaHandleDirective read FOnHandleDirective write FOnHandleDirective;
  end;

  TSimbaTokenizerString = class(TSimbaTokenizerBase)
  protected
    FDoc: String;
    function Compare(Key: String): Boolean; override;
    function getTokString: String; override;
    procedure setDoc(const ADoc: String); virtual;
  public
    procedure Reset(ClearDoc: Boolean = False); override;
    constructor Create(ADoc: String; AFileName: String = ''); reintroduce; virtual;
    function getChar(Offset: Integer = 0): Char; override;
  published
    property Doc: String read FDoc write setDoc;
  end;

  TSimbaTokenizerFile = class(TSimbaTokenizerString)
  public
    constructor Create(AFileName: UnicodeString = ''); reintroduce; overload; virtual;
    constructor Create(AFileName: AnsiString = ''); reintroduce; overload; virtual;
  end;

  TSimbaKeyword = record
    Keyword: String;
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
  ParserToken_Keywords = [tk_kw_Array..tk_kw_With];
  ParserToken_Symbols = [tk_sym_BracketClose..tk_sym_SemiColon];
  ParserToken_Types = [tk_typ_Float..tk_typ_Char];

  Keywords: array[0..52] of TSimbaKeyword = (
      (Keyword: 'AND';           Token: tk_op_AND),
      (Keyword: 'DIV';           Token: tk_op_DIV),
      (Keyword: 'IN';            Token: tk_op_IN),
      (Keyword: 'IS';            Token: tk_op_IS),
      (Keyword: 'MOD';           Token: tk_op_MOD),
      (Keyword: 'NOT';           Token: tk_op_NOT),
      (Keyword: 'OR';            Token: tk_op_OR),
      (Keyword: 'SHL';           Token: tk_op_SHL),
      (Keyword: 'SHR';           Token: tk_op_SHR),
      (Keyword: 'XOR';           Token: tk_op_XOR),

      (Keyword: 'ARRAY';         Token: tk_kw_Array),
      (Keyword: 'BEGIN';         Token: tk_kw_Begin),
      (Keyword: 'CASE';          Token: tk_kw_Case),
      (Keyword: 'CONST';         Token: tk_kw_Const),
      (Keyword: 'CONSTREF';      Token: tk_kw_ConstRef),
      (Keyword: 'DEPRECATED';    Token: tk_kw_Deprecated),
      (Keyword: 'DO';            Token: tk_kw_Do),
      (Keyword: 'DOWNTO';        Token: tk_kw_DownTo),
      (Keyword: 'ELSE';          Token: tk_kw_Else),
      (Keyword: 'END';           Token: tk_kw_End),
      (Keyword: 'EXCEPT';        Token: tk_kw_Except),
      (Keyword: 'EXPERIMENTAL';  Token: tk_kw_Experimental),
      (Keyword: 'EXTERNAL';      Token: tk_kw_External),
      (Keyword: 'FINALLY';       Token: tk_kw_Finally),
      (Keyword: 'FOR';           Token: tk_kw_For),
      (Keyword: 'FORWARD';       Token: tk_kw_Forward),
      (Keyword: 'FUNCTION';      Token: tk_kw_Function),
      (Keyword: 'IF';            Token: tk_kw_If),
      (Keyword: 'LABEL';         Token: tk_kw_Label),
      (Keyword: 'OBJECT';        Token: tk_kw_Object),
      (Keyword: 'OPERATOR';      Token: tk_kw_Operator),
      (Keyword: 'OF';            Token: tk_kw_Of),
      (Keyword: 'OUT';           Token: tk_kw_Out),
      (Keyword: 'OVERLOAD';      Token: tk_kw_Overload),
      (Keyword: 'OVERRIDE';      Token: tk_kw_Override),
      (Keyword: 'PACKED';        Token: tk_kw_Packed),
      (Keyword: 'PRIVATE';       Token: tk_kw_Private),
      (Keyword: 'PROCEDURE';     Token: tk_kw_Procedure),
      (Keyword: 'PROGRAM';       Token: tk_kw_Program),
      (Keyword: 'RECORD';        Token: tk_kw_Record),
      (Keyword: 'REPEAT';        Token: tk_kw_Repeat),
      (Keyword: 'SET';           Token: tk_kw_Set),
      (Keyword: 'STATIC';        Token: tk_kw_Static),
      (Keyword: 'THEN';          Token: tk_kw_Then),
      (Keyword: 'TO';            Token: tk_kw_To),
      (Keyword: 'TRY';           Token: tk_kw_Try),
      (Keyword: 'TYPE';          Token: tk_kw_Type),
      (Keyword: 'UNION';         Token: tk_kw_Union),
      (Keyword: 'UNIMPLEMENTED'; Token: tk_kw_UnImplemented),
      (Keyword: 'UNTIL';         Token: tk_kw_Until),
      (Keyword: 'VAR';           Token: tk_kw_Var),
      (Keyword: 'WHILE';         Token: tk_kw_While),
      (Keyword: 'WITH';          Token: tk_kw_With)
    );

  NullDocPos: TDocPos = (Line: 0; Col: 0; FileName: '');

var
  KeywordsCache: array[2..13, Byte] of array of TSimbaKeyword;

  FormatSettings: TFormatSettings absolute DefaultFormatSettings;

implementation

uses
  typinfo;

function LapeTokenToString(Token: EParserToken): String;
begin
  Result := String(GetEnumName(TypeInfo(EParserToken), Ord(Token)));
  if (Token in ParserToken_Symbols + ParserToken_Types) then
    Delete(Result, 1, 7)
  else if (Token in ParserToken_Keywords) then
    Delete(Result, 1, 6)
  else
    Delete(Result, 1, 3);
end;

function StrToFloatDot(Str: string): Extended;
begin
  Result := StrToFloat(StringReplace(Str, '.', FormatSettings.DecimalSeparator, []));
end;

function StrToFloatDotDef(Str: string; Default: Extended): Extended;
begin
  Result := StrToFloatDef(StringReplace(Str, '.', FormatSettings.DecimalSeparator, []), Default);
end;

function StrToUInt64(Str: string): UInt64;
begin
  Result := StrToQWord(Str);
end;

function StrToUInt64Def(Str: string; const Default: UInt64): UInt64;
begin
  Result := StrToQWordDef(Str, Default);
end;

function HashKeyword(const Str: String): Byte;
var
  i: Integer;
begin
  Result := Length(Str);
  for i := 1 to Length(Str) do
    Result := Byte(Result - 128) xor Byte((Ord(Str[i]) - 65) shl ((i + 2) mod 8));
end;

function IsKeyword(Str: String; out Token: EParserToken): Boolean;
var
  Hash: Byte;
  i: Integer;
  StrLen: Integer;
begin
  StrLen := Length(Str);
  if (StrLen < Low(KeywordsCache)) or (StrLen > High(KeywordsCache)) then
    Exit(False);

  Str := UpperCase(Str);
  Hash := HashKeyword(Str);

  for i := High(KeywordsCache[StrLen][Hash]) downto 0 do
    if (CompareStr(KeywordsCache[StrLen][Hash][i].Keyword, Str) = 0) then
    begin
      Token := KeywordsCache[StrLen][Hash][i].Token;
      Exit(True);
    end;

  Result := False;
end;

procedure ClearKeywordsCache;
var
  i: Integer;
  ii: Integer;
begin
  for i := Low(KeywordsCache) to High(KeywordsCache) do
    for ii := Low(KeywordsCache[i]) to High(KeywordsCache[i]) do
      KeywordsCache[i][ii] := nil;
end;

procedure InitKeywordsCache;
var
  Hash: Byte;
  i: Integer;
  StrLen: Integer;
begin
  ClearKeywordsCache();

  for i := Low(Keywords) to High(Keywords) do
  begin
    Hash := HashKeyword(UpperCase(Keywords[i].Keyword));
    StrLen := Length(Keywords[i].Keyword);
    SetLength(KeywordsCache[StrLen][Hash], Length(KeywordsCache[StrLen][Hash]) + 1);
    KeywordsCache[StrLen][Hash][High(KeywordsCache[StrLen][Hash])] := Keywords[i];
  end;
end;

function TSimbaTokenizerBase.Identify: EParserToken;
var
  Char: System.Char;

  procedure NextPos_CountLines;
  begin
    repeat
      case CurChar of
        #10: Inc(FDocPos.Line);
        #13:
          begin
            if (getChar(1) = #10) then
              Inc(FPos);
            Inc(FDocPos.Line);
          end;
      end;
      Inc(FPos);
    until (not (CurChar in [#9, #32, #10, #13]));
  end;

  function Alpha: EParserToken;
  var
    Str: String;
    Token: EParserToken;
  begin
    Str := Char;
    Char := getChar(1);
    while (Char in ['0'..'9', 'A'..'Z', '_', 'a'..'z']) do
    begin
      Inc(FPos);
      Str := Str + Char;
      Char := getChar(1);
    end;

    if IsKeyword(Str, Token) then
      Result := setTok(Token)
    else
      Result := setTok(tk_Identifier);
  end;

begin
  FTokStart := FPos;
  Char := CurChar;

  case Char of
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

    {Divide, Comment, AssignDiv}
    '/':
      case getChar(1) of
        '/':
          begin
            Inc(FPos);
            while (not (getChar(1) in [#13, #10, #0])) do Inc(FPos);
            Result := setTok(tk_Comment);
          end;
        '=':
          begin
            Result := setTok(tk_op_AssignDiv);
            Inc(FPos);
          end;
        else
          Result := setTok(tk_op_Divide);
      end;


    {Minus, AssignMinus}
    '-':if (getChar(1) = '=') then begin
          Result := setTok(tk_op_AssignMinus);
          Inc(FPos);
        end else
          Result := setTok(tk_op_Minus);

    {Multiply, Power, AssignMul}
    '*':
      case getChar(1) of
        '*':
          begin
            Inc(FPos);
            Result := setTok(tk_op_Power);
          end;
        '=':
          begin
            Result := setTok(tk_op_AssignMul);
            Inc(FPos);
          end;
        else
          Result := setTok(tk_op_Multiply);
      end;


    {Plus, AssignPlus}
    '+':if (getChar(1) = '=') then begin
          Result := setTok(tk_op_AssignPlus);
          Inc(FPos);
        end else
          Result := setTok(tk_op_Plus);

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
          Inc(FPos, 2);
          while (not ((CurChar in ['*', #0]) and (getChar(1) in [')', #0]))) do
            NextPos_CountLines();

          Result := setTok(tk_Comment);
          if (CurChar = '*') then
            Inc(FPos);
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
          Result := setTok(tk_Directive);
          if (not HandleDirective()) then
            raise SimbaTokenizerException.Create('Unknown directive');
        end
        else
        begin
          while (not (CurChar in ['}', #0])) do NextPos_CountLines();
          Result := setTok(tk_Comment);
        end;
      end;

    //Integer and Float
    '0'..'9':
      begin
        Char := getChar(1);
        while (Char in ['0'..'9', '_']) do
        begin
          Inc(FPos);
          Char := getChar(1);
        end;

        if (Char <> '.') or (not (getChar(2) in ['0'..'9'])) then
          Result := setTok(tk_typ_Integer)
        else
        begin
          Inc(FPos, 2);
          Char := getChar(1);
          while (Char in ['0'..'9', '_']) do
          begin
            Inc(FPos);
            Char := getChar(1);
          end;
          if (Char in ['e', 'E']) and (getChar(2) in ['+', '-']) and (getChar(3) in ['0'..'9']) then
          begin
            Inc(FPos, 3);
            while (getChar(1) in ['0'..'9', '_']) do
              Inc(FPos);
          end;

          Result := setTok(tk_typ_Float);
        end;
      end;
    '$':
      begin
        if (getChar(1) in ['0'..'9', 'A'..'F', 'a'..'f']) then
        begin
          Inc(FPos);
          while (getChar(1) in ['0'..'9', 'A'..'F', 'a'..'f', '_']) do Inc(FPos);
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
          while (getChar(1) in ['0'..'1', '_']) do Inc(FPos);
          Result := setTok(tk_typ_Integer_Bin);
        end
        else
          Result := setTok(tk_Unkown);
      end;
    'A'..'Z', '_', 'a'..'z': Result := Alpha();
    #34: //heredoc string
      begin
        Inc(FPos);
        while (not (CurChar in [#34, #0])) do NextPos_CountLines();
        if (CurChar in [#0]) then
          raise SimbaTokenizerException.Create('Unclosed string');
        Result := setTok(tk_typ_HereString);
      end;
    #39:
      begin
        Inc(FPos);
        while (not (CurChar in [#39, #0, #13, #10])) do Inc(FPos);
        if not (CurChar in [#39]) then
          raise SimbaTokenizerException.Create('Unclosed string');
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

function TSimbaTokenizerBase.HandleDirective: Boolean;
var
  Directive, Argument: String;
begin
  try
    if ({$IFNDEF FPC}@{$ENDIF}FOnParseDirective <> nil) then
      if FOnParseDirective(Self) then
        Exit(True);

    if ({$IFNDEF FPC}@{$ENDIF}FOnHandleDirective <> nil) then
    begin
      Next();
      Expect([tk_Identifier] + ParserToken_Keywords, False, False);
      Directive := TokString;

      NextNoWhiteSpace();
      if (CurChar = '}') then
        Argument := ''
      else
      begin
        while (not (getChar(1) in ['}', #0])) do
        begin
          Inc(FPos);
          if CurChar in [#10,#13] then
          begin
            if (CurChar = #13) and (getChar(1) = #10) then Inc(FPos);
            Inc(FDocPos.Line);
          end;
        end;
        Argument := TokString;
        Inc(FPos);
      end;

      if FOnHandleDirective(Self, Directive, Argument) then
        Exit(True);
    end
    else
      while (not (CurChar in ['}', #0])) do
      begin
        Inc(FPos);
        if CurChar in [#10,#13] then
        begin
          if (CurChar = #13) and (getChar(1) = #10) then Inc(FPos);
          Inc(FDocPos.Line);
        end;
      end;

    Result := False;
  finally
    if (CurChar <> '}') then
      raise SimbaTokenizerException.Create('Expected "}"');
  end;
end;

function TSimbaTokenizerBase.setTok(ATok: EParserToken): EParserToken;
begin
  FLastTok := FTok;
  FTok := ATok;
  Result := ATok;
end;

function TSimbaTokenizerBase.getTokInt: Integer;
begin
  Result := Integer(getTokUInt64());
end;

function TSimbaTokenizerBase.getTokUInt64: UInt64;
  function Bin2Dec(s: String): UInt64;
  var
    i: Integer;
  begin
    Result := 0;
    if (Length(s) > 0) and (s[1] <> '%') then
      s := '%' + s;
    for i := 2 to Length(s) do
      Result := (Result shl 1) + UInt64(Ord(s[i])) - UInt64(Ord('0'));
  end;
var
  TokStr: String;
begin
  TokStr := StringReplace(getTokString(), String('_'), String(''), [rfReplaceAll]);
  case FTok of
    tk_typ_Integer_Bin: Result := Bin2Dec(TokStr);
    else Result := StrToUInt64Def(string(TokStr), UInt64(-1));
  end;
end;

function TSimbaTokenizerBase.getTokFloat: Extended;
begin
  Result := StrToFloatDotDef(StringReplace(string(getTokString()), '_', '', [rfReplaceAll]), -1);
end;

function TSimbaTokenizerBase.getTokChar: WideChar;
var
  Str: String;
begin
  Str := getTokString();
  Delete(Str, 1, 1);
  Result := Chr(StrToIntDef(string(Str), 0));
end;

function TSimbaTokenizerBase.getTokLen: Integer;
begin
  Result := FPos - FTokStart + 1;
end;

function TSimbaTokenizerBase.getCurChar: Char;
begin
  Result := getChar(0);
end;

procedure TSimbaTokenizerBase.setPos(APos: Integer);
begin
  Assert(APos >= 0);
  Assert(APos < FLen);

  FTokStart := APos;
  FPos := APos - 1;
  Next();
end;

function TSimbaTokenizerBase.getDocPos: TDocPos;
begin
  if (OverridePos <> nil) then
    Exit(OverridePos^);

  Result.Line := FDocPos.Line + NullPos.Line;
  if (Integer(FDocPos.Col) > FTokStart) then
    Result.Col := 0
  else
    Result.Col := NullPos.Col + UInt32(FTokStart) - FDocPos.Col;
  if (FFileName <> '') then
    Result.FileName := FFileName
  else
    Result.FileName := NullPos.FileName;
end;

constructor TSimbaTokenizerBase.Create(AFileName: String = '');
begin
  inherited Create();

  FFileName := AFileName;
  FOnParseDirective := nil;
  FOnHandleDirective := nil;

  OverridePos := nil;
  NullPos := NullDocPos;
  with NullPos do
  begin
    Line := 1;
    Col := 1;
  end;

  Reset();
end;

procedure TSimbaTokenizerBase.Reset(ClearDoc: Boolean = False);
begin
  FLastTok := tk_NULL;
  FTok := tk_NULL;
  FTokStart := 0;
  FPos := -1;
  FInPeek := False;
  FDocPos := NullDocPos;
  if ClearDoc then
    FLen := 0;
end;

function TSimbaTokenizerBase.getState: Pointer;
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

procedure TSimbaTokenizerBase.setState(const State: Pointer; DoFreeState: Boolean = True);
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

procedure TSimbaTokenizerBase.freeState(const State: Pointer);
begin
  Dispose(PTokenizerState(State));
end;

function TSimbaTokenizerBase.TempRollBack: EParserToken;
begin
  Result := FTok;
  FTok := FLastTok;
  FLastTok := tk_NULL;
  FPos := FTokStart - 1;
  FTokStart := FPos;
end;

function TSimbaTokenizerBase.Next: EParserToken;
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

function TSimbaTokenizerBase.NextNoWhiteSpace: EParserToken;
var
  PrevTok: EParserToken;
begin
  PrevTok := FTok;
  repeat
    Result := Next();
  until (not (Result in TokWhiteSpace));
  FLastTok := PrevTok;
end;

function TSimbaTokenizerBase.NextNoJunk: EParserToken;
var
  PrevTok: EParserToken;
begin
  PrevTok := FTok;
  repeat
    Result := Next();
  until (not (Result in TokJunk));
  FLastTok := PrevTok;
end;

function TSimbaTokenizerBase.Peek: EParserToken;
var
  OldSate: Pointer;
begin
  OldSate := getState();
  try
    FInPeek := True;
    Result := Next();
  finally
    setState(OldSate);
  end;
end;

function TSimbaTokenizerBase.PeekNoWhiteSpace: EParserToken;
var
  OldState: Pointer;
begin
  OldState := getState();
  try
    FInPeek := True;
    Result := NextNoWhiteSpace();
  finally
    setState(OldState);
  end;
end;

function TSimbaTokenizerBase.PeekNoJunk: EParserToken;
var
  OldState: Pointer;
begin
  OldState := getState();
  try
    FInPeek := True;
    Result := NextNoJunk();
  finally
    setState(OldState);
  end;
end;

function TSimbaTokenizerBase.Expect(Token: EParserToken; NextBefore: Boolean = True; NextAfter: Boolean = False): EParserToken;
begin
  if NextBefore then
    NextNoJunk();
  Result := FTok;
  if (FTok <> Token) then
    raise SimbaTokenizerException.Create('Unexpected token "' + LapeTokenToString(FTok) + '"');
  if NextAfter then
    NextNoJunk();
end;

function TSimbaTokenizerBase.Expect(Tokens: EParserTokenSet; NextBefore: Boolean = True; NextAfter: Boolean = False): EParserToken;
begin
  if NextBefore then
    NextNoJunk();
  Result := FTok;
  if (not (FTok in Tokens)) then
    raise SimbaTokenizerException.Create('Unexpected token "' + LapeTokenToString(FTok) + '"');
  if NextAfter then
    NextNoJunk();
end;

function TSimbaTokenizerString.Compare(Key: String): Boolean;
var
  KeyLen: Integer;
begin
  KeyLen := Length(Key);
  if (FPos < 0) or (FPos >= FLen) or (FPos + KeyLen - 1 >= FLen) then
    Exit(False);

 Result := SameText(Key, Copy(FDoc, FPos + 1, KeyLen));
 if Result then
   FPos := FPos + KeyLen - 1
end;

function TSimbaTokenizerString.getTokString: String;
begin
  if (FPos < 0) or (FPos >= FLen) then
    Result := ''
  else
    Result := Copy(FDoc, FTokStart + 1, FPos - FTokStart + 1);
end;

procedure TSimbaTokenizerString.setDoc(const ADoc: String);
begin
  Reset(True);
  FDoc := ADoc;
  FLen := Length(FDoc);
end;

procedure TSimbaTokenizerString.Reset(ClearDoc: Boolean = False);
begin
  inherited;

  if ClearDoc then
    FDoc := '';
end;

constructor TSimbaTokenizerString.Create(ADoc: String; AFileName: String = '');
begin
  inherited Create(AFileName);

  FDoc := ADoc;
  FLen := Length(FDoc);
end;

function TSimbaTokenizerString.getChar(Offset: Integer = 0): Char;
begin
  if (FPos + Offset < 0) or (FPos + Offset >= FLen) then
    Result := #0
  else
    Result := FDoc[FPos + Offset + 1];
end;

constructor TSimbaTokenizerFile.Create(AFileName: UnicodeString = '');
var
  StrList: TStringList;
begin
  StrList := TStringList.Create();
  try
    StrList.LoadFromFile(string(AFileName));
    inherited Create(String(StrList.Text), String(AFileName));
  finally
    StrList.Free();
  end;
end;

constructor TSimbaTokenizerFile.Create(AFileName: AnsiString = '');
begin
  Create(UnicodeString(AFileName));
end;

initialization
  InitKeywordsCache();

finalization
  ClearKeywordsCache();

end.

