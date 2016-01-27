unit scriptformatter;
{
    Original Code Thread: http://villavu.com/forum/showthread.php?t=35513
    by Nielsie95

    Modified by Dgby714.
    Modified for Lazarus by CynicRus
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
const
  tkConst        = 99194018;
  tkType         = 33489835;
  tkVar          = 302492;
  tkBegin        = 206891307;
  tkEnd          = 422887;
  tkArray        = 241981220;
  tkSet          = 408192;
  tkRecord       = 154572379;
  tkIf           = 9113;
  tkThen         = 27389149;
  tkElse         = 34909855;
  tkWhile        = 246933539;
  tkFor          = 516020;
  tkTo           = 10916;
  tkDownto       = 196948847;
  tkDo           = 12477;
  tkWith         = 31996795;
  tkRepeat       = 117892746;
  tkUntil        = 29229899;
  tkCase         = 24377488;
  tkOf           = 9445;
  tkGoto         = 42051300;
  tkLabel        = 81388075;
  tkProcedure    = 91883435;
  tkFunction     = 150673595;
  tkNil          = 373079;
  tkAnd          = 420541;
  tkOr           = 10818;
  tkNot          = 439860;
  tkXor          = 469321;
  tkDiv          = 504470;
  tkMod          = 352779;
  tkExternal     = 100911611;
  tkStdcall      = 110854811;
  tkCdecl        = 150234651;
  tkRaise        = 68853785;
  tkTry          = 504179;
  tkExcept       = 71323914;
  tkOn           = 10369;
  tkFinally      = 252725930;
  tkAs           = 12984;
  tkIn           = 9998;
  tkIs           = 13191;
  tkOut          = 472998;
  tkProgram      = 73755563;
  tkShr          = 412495;
  tkShl          = 382541;
  tkForward      = 244372219;
  tkString       = -12;
  tkPlus         = -13;
  tkBraceOpen    = -14;
  tkBraceClose   = -15;
  tkColon        = -16;
  tkSemiColon    = -17;
  tkDot          = -18;
  tkComma        = -19;
  tkBracketOpen  = -20;
  tkBracketClose = -21;
  tkEquation     = -22;
  tkChar         = -23;
  tkCompDir      = -24;
  tkInclude      = -25;
  tkAssignment   = -26;
  tkInteger      = -27;
  tkFloat        = -28;
  tkHex          = -29;
  tkLineComment  = -30;
  tkComment      = -31;
  tkDivide       = -32;
  tkMultiply     = -33;
  tkMinus        = -34;
  tkNull         = -999;

  Keywords: array [0..48] of integer = (tkConst, tkType, tkVar, tkBegin, tkEnd, tkArray, tkSet, tkRecord,
               tkIf, tkThen, tkElse, tkWhile, tkFor, tkTo, tkDownto, tkDo, tkWith,
               tkRepeat, tkUntil, tkCase, tkOf, tkGoto, tkLabel, tkProcedure,
               tkFunction, tkNil, tkAnd, tkOr, tkNot, tkXor, tkDiv, tkMod, {}tkAssignment{},
               tkExternal, tkStdcall, tkCdecl, tkRaise, tkTry, tkExcept, tkOn,
               tkFinally, tkAs, tkIn, tkIs, tkOut, tkProgram, tkShr, tkShl, tkForward);

type

  { TCodeParser }
  tInType = (itNone = 0, itInFunc = 1, itInFuncDecl = 2, itInVar = 3);
  tIndents = array [0..500] of Byte;
  StrExtr =(Numbers, Letters, Others);
  TProcessingType = (ptFormatter = 0, ptObfuscator = 1);

  TCodeParser = class
    private
      FRunPos: integer;
      FTokenID: integer;
      FToken: string;
      FScript: string;
      FOutputScript: string;
      FAllowComments: boolean;
      FReturnComments: boolean;
      FLastAsSpace: boolean;
      FFirstInLine: boolean;
      FLastWasKeyword: boolean;
      FLastWasChar: boolean;
      FLastWasComment: Boolean;
      FBeginCount: Integer;
      FInType: tInType;
      FIndents: tIndents;
      FInDent: ShortInt;
      FElseIndent: ShortInt;
      procedure Clear;
      procedure AddComment;
      procedure NextToken;
      procedure NewLine;
      procedure Add;
      function CharsToString(a: array of ShortInt): string;
      function StringToChars(s: string): string;
      function StringHash(Value: string): Integer;
      procedure ProcessChars;virtual;abstract;
    public
      constructor Create;
      procedure ProcessScript;virtual;abstract;
      property RunPos: integer read FRunPos write FRunPos;
      property TokenID: integer read FTokenID write FTokenID;
      property Token: string read FToken write FToken;
      property Script: string read FScript write FScript;
      property OutputScript: string read FOutputScript write FOutputScript;
      property AllowComments: boolean read FAllowComments write FAllowComments;
      property ReturnComments: boolean read FReturnComments write FReturnComments;
      property LastAsSpace: boolean read FLastAsSpace write FLastAsSpace;
      property FirstInLine: boolean read FFirstInLine write FFirstInLine;
      property LastWasKeyword: boolean read FLastWasKeyword write FLastWasKeyword;
      property LastWasChar: boolean read FLastWasChar write FLastWasChar;
      property LastWasComment: boolean read FLastWasComment write FLastWasComment;
      property BeginCount: integer read FBeginCount write FBeginCount;
      property InType: tInType read FInType write FInType;
      property Indents: tIndents read FIndents write FIndents;
      property InDent: ShortInt read FInDent write FInDent;
      property ElseIndent: ShortInt read FElseIndent write FElseIndent;
    end;

  { TCodeFormatter }

  TCodeFormatter = class(TCodeParser)
    private
      procedure ProcessChars;override;
    public
      procedure ProcessScript;override;
      property RunPos;
      property TokenID;
      property Token;
      property AllowComments;
      property ReturnComments;
      property Script;
      property OutputScript;
      property LastAsSpace;
      property FirstInLine;
      property LastWasKeyword;
      property LastWasChar;
      property LastWasComment;
      property BeginCount;
      property InType;
      property Indents;
      property InDent;
      property ElseIndent;
  end;

  { TCodeObfuscator }

  TCodeObfuscator = class(TCodeParser)
    private
      procedure ProcessChars;override;
    public
      procedure ProcessScript;override;
      property RunPos;
      property TokenID;
      property Token;
      property AllowComments;
      property ReturnComments;
      property Script;
      property OutputScript;
      property LastAsSpace;
      property FirstInLine;
      property LastWasKeyword;
      property LastWasChar;
      property LastWasComment;
      property BeginCount;
      property InType;
      property Indents;
      property InDent;
      property ElseIndent;
  end;

  { TCodeFactory }

  TCodeFactory = class
    public
      class function GetCodeParser(const ProcessingType: TProcessingType): TCodeParser;
  end;
  function ExtractFromStr( Str : string; Extract : StrExtr) : string;
  function InIntArray(const a: array of integer; Number: Integer): Boolean;
  function ExtendInteger(Int: Integer): string;
implementation
   uses StrUtils,Math;
function ExtractFromStr(Str: string; Extract: StrExtr): string;
var
  Range: set of char;
  i: integer;
begin
  case Extract of
    Numbers: Range := ['0'..'9'];
    Letters: Range := ['A'..'Z', 'a'..'z'];
    Others: Range := [#0..#255] - ['0'..'9', 'A'..'Z', 'a'..'z'];
  end;
  Result := '';
  for i := length(str) downto 1 do
    if str[i] in Range then
       result := str[i] + result;
end;

function InIntArray(const a: array of integer; Number: Integer): Boolean;
var
  i, l: Integer;
begin
  Result := False;
  l := High(a);
  for i := 0 to l do
    if (a[i] = Number) then
    begin
      Result := True;
      Exit;
    end;
end;

function ExtendInteger(Int: Integer): string;
var
  i, c, q1, q2: Integer;
begin
  i := 0;
  c := -1;
  Result := '(';
  while (i < 3) and (c <> Int) do
  begin
    q1 := 0;
    q2 := 0;
    while (q1 = 0) do
      q1 := Random(Max(Int, 10));
    while (q2 = 0) do
      q2 := Random(Max(Int, 10));
    if (Random(15) = 0) then
      q1 := -q1;
    if (Random(15) = 0) then
      q2 := -q2;
    if (Result = '(') then
    begin
      c := q1 + q2;
      Result := Result + '('+IntToStr(q1)+'+'+IntToStr(q2)+')';
    end
    else
      case Random(4) of
        0:
          begin
            c := c + (q1 + q2);
            Result := Result + '+('+IntToStr(q1)+'+'+IntToStr(q2)+')';
          end;
        1:
          begin
            c := c - (q1 - q2);
            Result := Result + '-('+IntToStr(q1)+'-'+IntToStr(q2)+')';
          end;
        2:
          begin
            c := c + (q1 * q2);
            Result := Result + '+('+IntToStr(q1)+'*'+IntToStr(q2)+')';
          end;
        3:
          begin
            c := c - Trunc(q1 / q2);
            Result := Result + '-('+IntToStr(q1)+'/'+IntToStr(q2)+')';
          end;
      end;
    Inc(i);
  end;
  if (c <> Int) then
  begin
    q1 := Int - c;
    Result := Result + '+' + IntToStr(q1);
  end;
  Result := Result + ')';
end;

{ TCodeFactory }

class function TCodeFactory.GetCodeParser(const ProcessingType: TProcessingType
  ): TCodeParser;
begin
  case ProcessingType of
    ptFormatter: result:=TCodeFormatter.Create;
    ptObfuscator: result:=TCodeObfuscator.Create;
  end;
end;

{ TCodeObfuscator }

procedure TCodeObfuscator.ProcessChars;
begin
  case TokenID of
    tkString:
      begin
        Token := StringToChars(Token);
        TokenID := tkChar;
      end;
    tkInteger: Token := ExtendInteger(StrToInt(Token));
  end;
  Add;
end;

procedure TCodeObfuscator.ProcessScript;
var
  BraceCount: Integer;
begin
  RunPos := 1;
  BeginCount := 0;
  InType := itNone;
  FirstInLine := True;
  NextToken;
  while (TokenID <> tkNull) do
  begin
    case TokenID of
      tkInclude:
        begin
          if (not FirstInLine) then
            NewLine;
          ProcessChars;
          NewLine;
        end;
      tkProcedure, tkFunction:
        begin
          ProcessChars;
          NextToken;
          //WriteLn(Token);
          ProcessChars;
          InType := itInFunc;
        end;
      tkBraceOpen:
        begin
          if (InType = itInVar) then
            LastWasKeyword := True;
          ProcessChars;
          NextToken;
          BraceCount := 1;
          while (TokenID <> tkNull) and (BraceCount > 0) do
          begin
            if (TokenID = tkBraceOpen) then
              BraceCount:= BraceCount + 1
            else if (TokenID = tkBraceClose) then
              BraceCount:= BraceCount - 1;
            ProcessChars;
            NextToken;
          end;
          Continue;
        end;
      else
        ProcessChars;
    end;
    NextToken;
  end;
end;

{ TCodeFormatter }

procedure TCodeFormatter.ProcessChars;
var
  cChar: Integer;
  Chars: array of ShortInt;
  s: string;
begin
  SetLength(Chars, 1);
  cChar := 0;
  Chars[0] := StrToIntDef(Copy(Token, 2, Length(Token) - 1), -1);
  NextToken;
  while (TokenID = tkChar) do
  begin
    Inc(cChar);
    SetLength(Chars, cChar + 1);
    Chars[cChar] := StrToIntDef(Copy(Token, 2, Length(Token) - 1), -1);
    NextToken;
  end;
  cChar := TokenID;
  s := Token;
  Token := CharsToString(Chars);
  TokenID := tkString;
  Add;
  TokenID := cChar;
  Token := s;
end;

procedure TCodeFormatter.ProcessScript;
var
  BraceCount: Integer;
begin
  RunPos := 1;
  InDent := 0;
  BeginCount := 0;
  InType := itNone;
  FirstInLine := True;
  NextToken;
  while (TokenID <> tkNull) do
  begin
    case TokenID of
      tkChar:
        begin
          ProcessChars;
          Continue;
        end;
      tkInclude:
        begin
          if (not FirstInLine) then
            NewLine;
          Add;
          NewLine;
        end;
      tkCompDir:
        begin
          Add;
          NewLine;
        end;
      tkProcedure, tkFunction:
        begin
          InDent := 0;
          ElseIndent := 0;
          NewLine;
          Add;
          NextToken;
          //Writeln(Token);
          Add;
          InType := itInFuncDecl;
        end;
      tkBraceOpen:
        begin
          if (InType = itInVar) then
            LastWasKeyword := True;
          Add;
          NextToken;
          BraceCount := 1;
          while (TokenID <> tkNull) and (BraceCount > 0) do
          begin
            if (TokenID = tkBraceOpen) then
              Inc(BraceCount)
            else if (TokenID = tkBraceClose) then
              Dec(BraceCount)
            else if (TokenID = tkChar) then
            begin
              ProcessChars;
              Continue;
            end;
            Add;
            NextToken;
          end;
          Continue;
        end;
      tkDo, tkThen:
        begin
          Add;
          NextToken;
          if (TokenID = tkBegin) then
            Continue
          else if (TokenID = tkTry) or (TokenID = tkRepeat) then
          begin
            ElseIndent:=ElseIndent +1;
            Continue;
          end
          else
          begin
            ElseIndent:=ElseIndent +1;
            NewLine;
            Add;
          end;
        end;
      tkElse:
        begin
          if (not FirstInLine) then
            NewLine;
          if (ElseIndent > 0) then
            ElseIndent:=ElseIndent -1;
          Add;
          NextToken;
          if (TokenID = tkBegin) or (TokenID = tkIf) then
            Continue
          else if (TokenID = tkTry) or (TokenID = tkRepeat) then
          begin
            ElseIndent:=ElseIndent +1;
            Continue;
          end
          else
          begin
            ElseIndent:=ElseIndent +1;
            NewLine;
            Add;
          end;
        end;
      tkColon:
        begin
          Add;
          if (InType = itInFunc) and (BeginCount > 1) then
          begin
            NextToken;
            if InIntArray(Keywords, TokenID) then
            begin
              ElseIndent:=ElseIndent +1;
              NewLine;
            end;
            Continue;
          end;
        end;
      tkOf:
        begin
          if (InType = itInVar) or (InType = itInFuncDecl) then
            LastAsSpace := False;
          Add;
          if (InType <> itInVar) and (InType <> itInFuncDecl) then
          begin
            BeginCount:=BeginCount +1;
            InDent:=InDent+1;
            Indents[InDent] := Indents[InDent - 1] + ElseIndent + 1;
            NewLine;
            ElseIndent := 0;
          end;
        end;
      tkBegin, tkRepeat:
        begin
          if (InType <> itInFunc) or (BeginCount = 0) then
          begin
            Indent := 0;
            ElseIndent := 0;
          end;
          if (not FirstInLine) or (InType = itNone) then
            NewLine;
          Add;
          BeginCount:=BeginCount +1;
          InDent:=InDent+1;
          Indents[InDent] := Indents[InDent - 1] + ElseIndent + 1;
          NewLine;
          ElseIndent := 0;
          InType := itInFunc;
        end;
      tkTry:
        begin
          if (not FirstInLine) then
            NewLine;
          Add;
          BeginCount:=BeginCount +1;
          InDent:=InDent+1;
          Indents[InDent] := Indents[InDent - 1] + ElseIndent + 1;
          NewLine;
          ElseIndent := 0;
        end;
      tkExcept, tkFinally:
        begin
          if (not FirstInLine) then
            NewLine;
          ElseIndent := -1;
          Add;
          ElseIndent := 0;
          NewLine;
        end;
      tkEnd, tkUntil:
        begin
          if (BeginCount > 0) then
            BeginCount:=BeginCount-1;
          if (BeginCount = 0) and (InType = itInFunc) then
            InType := itNone;
          if (not FirstInLine) then
            NewLine;
          ElseIndent := 0;
          Add;
          if (Indent > 0) then
            Indent:=Indent-1;
          if (TokenID = tkEnd) then
          begin
            NextToken;
            if (TokenID <> tkSemiColon) and (TokenID <> tkDot) then
              NewLine;
            Continue;
          end;
        end;
      tkVar, tkType, tkLabel, tkConst:
        begin
          InDent := 0;
          ElseIndent := 0;
          if (InType <> itInFuncDecl) then
            NewLine
          else if (not FirstInLine) then
            NewLine;
          InType := itInVar;
          Add;
          InDent:=InDent +1;
          Indents[InDent] := Indents[InDent - 1] + ElseIndent + 1;
          NewLine;
        end;
      tkRecord:
        begin
          ElseIndent := 0;
          InType := itInVar;
          Add;
          InDent:=InDent +1;
          Indents[InDent] := Indents[InDent - 1] + ElseIndent + 1;
          NewLine;
        end;
      tkSemiColon:
        begin
          if (not FirstInLine) then
          begin
            Add;
            NextToken;
            if (TokenID = tkExternal) or (TokenID = tkForward) then
              InType := itNone
            else
            begin
              ElseIndent := 0;
              NewLine;
            end;
            Continue;
          end;
        end;
      else
        Add;
    end;
    NextToken;
  end;
end;

{ TCodeParser }

procedure TCodeParser.Clear;
begin
  FRunPos:= -1;
  FTokenID:=-1;
  FToken:='';
  FScript:='';
  FOutPutScript:= '';
  FAllowComments:= false;
  FReturnComments:=false;
  FLastAsSpace:=false;
  FFirstInLine:=true;
  FLastWasKeyword:=false;
  FLastWasChar:=false;
  FLastWasComment:=false;
  FBeginCount:=-1;
  fInType:=itNone;
  fInDent:=0;
  fElseIndent:=0;
end;

procedure TCodeParser.AddComment;
begin
  case TokenID of
    tkComment:
      begin
        if (not LastWasComment) and (InType <> itInFunc) and (not FirstInLine) then
          NewLine;
        if (InType <> itInFunc) then
          NewLine
        else if (not FirstInLine) then
          NewLine;
        OutputScript := OutputScript + Token;
      end;
    tkLineComment:
      begin
        if (not FirstInLine) then
          NewLine;
        OutputScript := OutputScript + StringOfChar(' ', 2 * (Indents[InDent] + ElseIndent)) + Token;
      end;
  end;

  FirstInLine := False;
  LastAsSpace := False;
  LastWasKeyword := False;
  LastWasChar := False;
  LastWasComment := True;
end;

procedure TCodeParser.NextToken;
var
  OldRun, t: Integer;
begin
  OldRun := RunPos;
  Token := '';
  TokenId := tkNull;
  while (RunPos <= Length(Script)) do
    case Script[RunPos] of
      'a'..'z', 'A'..'Z', '0'..'9', '_': RunPos:=RunPos+1;
      else Break;
    end;
  if (RunPos = OldRun) and (RunPos < Length(Script)) then
  begin
    case Script[RunPos] of
      #9, #32, #13, #10:
        begin
          RunPos:=RunPos+1;
          NextToken;
          Exit;
        end;
      #39:
        begin
          RunPos:=RunPos+1;
          t := PosEx(#39, Script, RunPos);
          if (t > 0) then
            TokenID := tkString
          else
            Exit;
          RunPos := t + 1;
        end;
      '(':
        begin
          if (RunPos < Length(Script)) then
            if (Script[RunPos + 1] <> '*') then
              TokenID := tkBraceOpen
            else
            begin
              t := PosEx('*)', Script, RunPos);
              if (t <= 0) then
                Exit;
              RunPos := t + 2;
              TokenID := tkComment;
            end;
        end;
      '{':
        begin
          if (RunPos < Length(Script)) then
            if (Script[RunPos + 1] = '$') then
              TokenID := tkCompDir
            else if (RunPos + 1 + Length('.include ') < Length(Script)) then
              if (UpperCase(Copy(Script, RunPos + 1, Length('.include '))) = '.INCLUDE ') then
                  TokenID := tkInclude;
          t := PosEx('}', Script, RunPos);
          if (t <= 0) then
            Exit;
          RunPos := t + 1;
          if (TokenID = tkNull) then
            TokenID := tkComment;
        end;
      '/':
        begin
          if (RunPos < Length(Script)) then
            if (Script[RunPos + 1] = '/') then
            begin
              t := PosEx(#13, Script, RunPos);
              if (t <= 0) then
                Exit;
              RunPos := t;
              TokenID := tkLineComment;
            end;
          if (TokenID = tkNull) then
            TokenID := tkDivide;
        end;
      ':':
        begin
          if (RunPos < Length(Script)) then
             if (Script[RunPos + 1] <> '=') then
               TokenID := tkColon
             else
             begin
               RunPos:=RunPos+2;
               TokenID := tkAssignment;
             end;
        end;
      ')': TokenID := tkBraceClose;
      ';': TokenID := tkSemiColon;
      '+': TokenID := tkPlus;
      '-': TokenID := tkMinus;
      '*': TokenID := tkMultiply;
      '.': TokenID := tkDot;
      ',': TokenID := tkComma;
      '[': TokenID := tkBracketOpen;
      ']': TokenID := tkBracketClose;
      '>', '<', '=':
        begin
          if (RunPos < Length(Script)) and (Script[RunPos] <> '=') then
             if (Script[RunPos + 1] = '=') or ((Script[RunPos] = '<') and (Script[RunPos + 1] = '>')) then
               RunPos:=RunPos+2;
          TokenID := tkEquation;
        end;
      '#':
        begin
          RunPos:=RunPos+1;
          while (RunPos <= Length(Script)) do
            case Script[RunPos] of
              '0'..'9': RunPos:=RunPos+1;
              else Break;
            end;
          if (RunPos > (OldRun + 1)) then
            TokenID := tkChar;
        end;
      '$':
        begin
          RunPos:=RunPos+1;
          while (RunPos <= Length(Script)) do
            case Script[RunPos] of
              '0'..'9', 'a'..'f', 'A'..'F': RunPos:=RunPos+1;
              else Break;
            end;
          TokenID := tkHex;
        end;
    end;
    if (RunPos = OldRun) then
      RunPos:=RunPos+1;
  end
  else if (RunPos > Length(Script)) then
    Exit;
  Token := Copy(Script, OldRun, RunPos - OldRun);
  if ((TokenID = tkLineComment) or (TokenID = tkComment)) and (not ReturnComments) then
  begin
    if AllowComments then
      AddComment;
    NextToken;
    Exit;
  end;
  if (ExtractFromStr(Token, Numbers) = Token) and (RunPos < Length(Script)) then
  begin
    TokenID := tkInteger;
    if (Script[RunPos] = '.') then
    begin
      RunPos:=RunPos+1;
      while (RunPos <= Length(Script)) do
        case Script[RunPos] of
          '0'..'9': RunPos:=RunPos+1;
          else Break;
        end;
      TokenID := tkFloat;
      Token := Copy(Script, OldRun, RunPos - OldRun);
    end;
  end;
  if (TokenID = tkNull) and (Token <> '') then
    TokenID := StringHash(Token);
end;

procedure TCodeParser.NewLine;
begin
  OutputScript := OutputScript + #13#10;
  FirstInLine := True;
end;

procedure TCodeParser.Add;
var
  IsKeyword: Boolean;
begin
  IsKeyword := InIntArray(Keywords, TokenID) or (LowerCase(Token) = 'string');
  if LastWasComment and (not FirstInLine) then
    NewLine;
  LastWasComment := False;
  if FirstInLine then
    if (TokenID = tkInclude) then
      OutputScript := OutputScript + '  '
    else if (TokenID = tkEnd) or (TokenID = tkUntil) then
      OutputScript := OutputScript + StringOfChar(' ', 2 * (Indents[InDent] + ElseIndent - 1))
    else
      OutputScript := OutputScript + StringOfChar(' ', 2 * (Indents[InDent] + ElseIndent));
  if IsKeyWord then
  begin
    if (FirstInLine or LastAsSpace) and (TokenID <> tkAssignment) then
      OutputScript := OutputScript + LowerCase(Token)
    else
      OutputScript := OutputScript + ' ' + LowerCase(Token);
    LastAsSpace := False;
  end
  else case TokenID of
    tkBraceOpen, tkBracketOpen:
      begin
        if LastWasKeyword then
          OutputScript := OutputScript + ' ' + Token
        else
          OutputScript := OutputScript + Token;
        LastAsSpace := True;
      end;
    tkDot:
      begin
        OutputScript := OutputScript + Token;
        LastAsSpace := True;
      end;
    tkChar:
      begin
        if LastWasChar or FirstInLine or LastAsSpace then
          OutputScript := OutputScript + Token
        else
          OutputScript := OutputScript + ' ' + Token;
        LastAsSpace := False;
      end;
    tkComma:
      begin
        OutputScript := OutputScript + Token;
        FirstInLine := False;
        LastAsSpace := False;
        LastWasKeyword := True;
        LastWasChar := False;
        Exit;
      end;
    tkColon, tkSemiColon, tkBraceClose, tkBracketClose:
      begin
        OutputScript := OutputScript + Token;
        LastAsSpace := False;
      end;
    tkEquation, tkDivide, tkMultiply, tkMinus, tkPlus:
      begin
        OutputScript := OutputScript + ' ' + Token;
        FirstInLine := False;
        LastAsSpace := False;
        LastWasKeyword := True;
        LastWasChar := False;
        Exit;
      end;
    else
    begin
      if FirstInLine or LastAsSpace then
        OutputScript := OutputScript + Token
      else
        OutputScript := OutputScript + ' ' + Token;
      LastAsSpace := False;
    end;
  end;

  FirstInLine := False;
  LastWasKeyword := IsKeyword;
  LastWasChar := (TokenID = tkChar);
end;

function TCodeParser.CharsToString(a: array of ShortInt): string;
var
  i: Integer;
  ALastWasChar: Boolean;
begin
  Result := '';
  ALastWasChar := True;
  for i := 0 to High(a) do
    if (a[i] > -1) then
    begin
      if InRange(a[i], 32, 126) and (a[i] <> 39) then
      begin
        if ALastWasChar then
          Result := Result + #39 + Chr(a[i])
        else
          Result := Result + Chr(a[i]);
        ALastWasChar := False;
      end
      else
      begin
        if ALastWasChar then
          Result := Result + '#' + IntToStr(a[i])
        else
          Result := Result + #39 + '#' + IntToStr(a[i]);
        ALastWasChar := True;
      end;
    end;
  if (not ALastWasChar) then
    Result := Result + #39;
end;

function TCodeParser.StringToChars(s: string): string;
var
  i: Integer;
begin
  Result := '';
  if (Length(s) <= 2) then
    Result := #39#39
  else
    for i := 2 to Length(s) - 1 do
      Result := Result + '#' + IntToStr(Ord(s[i]));
end;

function TCodeParser.StringHash(Value: string): Integer;
var
  i, x: Integer;
begin
  Result := 0;
  Value := UpperCase(Value);
  for i := 1 to Length(Value) do
  begin
    x := Ord(Value[i]);
    Result := ((((Result + x) shl i) xor $ff) * x) shr 4;
  end;
end;

constructor TCodeParser.Create;
begin
  Clear;
end;

end.

