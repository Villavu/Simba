{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
// TPasLexer by Martin Waldenburg
unit simba.paslex;

{$i simba.inc}

interface

uses
  Classes, SysUtils;

type
  EPasToken = (
    tkNull, tkUnknown,

    // Whitespace
    tkSpace, tkLineEnding,

    // Comment
    tkAnsiComment, tkBorComment, tkSlashesComment,

    // Directive
    tkDirective,

    //Identifer
    tkIdentifier,

    // Variables
    tkFloat, tkChar, tkString, tkIntegerHex, tkInteger,

    // Symbols
    tkDot, tkDotDot, tkComma, tkRoundClose, tkRoundOpen, tkSemiColon, tkSquareClose, tkSquareOpen,

    // Keywords
    tkAnd, tkArray, tkAs, tkBegin, tkCase, tkConst, tkConstRef, tkDiv, tkDo,
    tkDownto, tkElse, tkEnd, tkExcept, tkExternal, tkFinally, tkFor, tkForward,
    tkFunction, tkGoto, tkIf, tkIn, tkInherited, tkIs, tkLabel, tkMod, tkNot,
    tkOf, tkOperator, tkOr, tkOut, tkOverload, tkOverride, tkPacked, tkPrivate,
    tkProcedure, tkProgram, tkProperty, tkProtected, tkRaise, tkRecord,
    tkRepeat, tkSet, tkShl, tkShr, tkStatic, tkThen, tkTo, tkTry, tkType,
    tkUntil, tkVar, tkWhile, tkWith, tkXor,

    // Operators
    tkAddress, tkAssign, tkAssignDiv, tkAssignMinus, tkAssignMul, tkAssignPlus,
    tkColon, tkEqual, tkGreater, tkGreaterEqual, tkLower, tkLowerEqual, tkMinus,
    tkNotEqual, tkPlus, tkPointerSymbol, tkSlash, tkStar
  );

const
  PasTokens_Symbols = [tkDot..tkSquareOpen];
  PasTokens_Keywords = [tkAnd..tkXor];
  PasTokens_Operators = [tkAddress..tkStar];

type
  ECommentState = (csAnsi, csBor, csNo);

  TPasLexer = class(TObject)
  private
    FComment: ECommentState;
    FOrigin: PChar;
    FProcTable: array[#0..#255] of procedure of object;
    FIdentFuncTable: array[0..133] of function: EPasToken of object;
    FRun: Integer;
    FTemp: PChar;
    FRoundCount: Integer;
    FSquareCount: Integer;
    FStringLen: Integer;
    FToIdent: PChar;
    FTokenPos: Integer;
    FLineNumber: Integer;
    FTokenID: EPasToken;
    FLastIdentPos: Integer;
    FLastNoSpace: EPasToken;
    FLastNoSpacePos: Integer;
    FLinePos: Integer;

    function KeyHash(ToHash: PChar): Integer; inline;
    function KeyComp(const Key: string): Boolean; inline;

    function Func15: EPasToken;
    function Func19: EPasToken;
    function Func20: EPasToken;
    function Func21: EPasToken;
    function Func23: EPasToken;
    function Func28: EPasToken;
    function Func32: EPasToken;
    function Func33: EPasToken;
    function Func35: EPasToken;
    function Func37: EPasToken;
    function Func39: EPasToken;
    function Func40: EPasToken;
    function Func41: EPasToken;
    function Func44: EPasToken;
    function Func45: EPasToken;
    function Func47: EPasToken;
    function Func49: EPasToken;
    function Func52: EPasToken;
    function Func56: EPasToken;
    function Func57: EPasToken;
    function Func60: EPasToken;
    function Func63: EPasToken;
    function Func65: EPasToken;
    function Func66: EPasToken;
    function Func71: EPasToken;
    function Func72: EPasToken;
    function Func73: EPasToken;
    function Func76: EPasToken;
    function Func79: EPasToken;
    function Func85: EPasToken;
    function Func88: EPasToken;
    function Func91: EPasToken;
    function Func92: EPasToken;
    function Func96: EPasToken;
    function Func99: EPasToken;
    function Func100: EPasToken;
    function Func102: EPasToken;
    function Func105: EPasToken;
    function Func106: EPasToken;
    function Func108: EPasToken;
    function Func133: EPasToken;
    function AltFunc: EPasToken;
    procedure InitIdentTables;
    function IdentKind(const MayBe: PChar): EPasToken; inline;
    procedure SetOrigin(const NewValue: PChar);
    procedure SetRunPos(const Value: Integer);
    procedure MakeMethodTables;
    procedure AddressProc;
    procedure CharProc;
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
    procedure HexNumberProc;
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
    procedure UnknownProc;
    function GetToken: string; inline;
    function InSymbols(const aChar: Char): Boolean;
    function CharAhead(const Count: Integer): Char;
  public
    constructor Create;

    procedure Next();
    procedure NextID(const ID: EPasToken);
    procedure NextNoJunk;

    property LastIdentPos: Integer read FLastIdentPos;
    property LastNoSpace: EPasToken read FLastNoSpace;
    property LastNoSpacePos: Integer read FLastNoSpacePos;
    property LineNumber: Integer read FLineNumber;
    property LinePos: Integer read FLinePos;
    property Origin: PChar read FOrigin write SetOrigin;
    property RunPos: Integer read FRun write SetRunPos;
    property TokenPos: Integer read FTokenPos;
    property Token: string read GetToken;
    property TokenID: EPasToken read FTokenID;
  end;

implementation

var
  Identifiers: array[#0..#255] of Boolean;
  HashTable: array[#0..#255] of Integer;

procedure MakeIdentTable;
var
  I, J: Char;
begin
  for I := #0 to #255 do
  begin
    case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z':
        Identifiers[I] := True;
      else
        Identifiers[I] := False;
    end;

    J := UpCase(I);
    case I of
      'a'..'z', 'A'..'Z', '_':
        HashTable[I] := Ord(J) - 64;
      else
        HashTable[Char(I)] := 0;
    end;
  end;
end;

procedure TPasLexer.InitIdentTables;
var
  I: Integer;
begin
  for I := 0 to High(FIdentFuncTable) do
    case I of
      15: FIdentFuncTable[I] := @Func15;
      19: FIdentFuncTable[I] := @Func19;
      20: FIdentFuncTable[I] := @Func20;
      21: FIdentFuncTable[I] := @Func21;
      23: FIdentFuncTable[I] := @Func23;
      28: FIdentFuncTable[I] := @Func28;
      32: FIdentFuncTable[I] := @Func32;
      33: FIdentFuncTable[I] := @Func33;
      35: FIdentFuncTable[I] := @Func35;
      37: FIdentFuncTable[I] := @Func37;
      39: FIdentFuncTable[I] := @Func39;
      40: FIdentFuncTable[I] := @Func40;
      41: FIdentFuncTable[I] := @Func41;
      44: FIdentFuncTable[I] := @Func44;
      45: FIdentFuncTable[I] := @Func45;
      47: FIdentFuncTable[I] := @Func47;
      49: FIdentFuncTable[I] := @Func49;
      52: FIdentFuncTable[I] := @Func52;
      56: FIdentFuncTable[I] := @Func56;
      57: FIdentFuncTable[I] := @Func57;
      60: FIdentFuncTable[I] := @Func60;
      63: FIdentFuncTable[I] := @Func63;
      65: FIdentFuncTable[I] := @Func65;
      66: FIdentFuncTable[I] := @Func66;
      71: FIdentFuncTable[I] := @Func71;
      72: FIdentFuncTable[I] := @Func72;
      73: FIdentFuncTable[I] := @Func73;
      76: FIdentFuncTable[I] := @Func76;
      79: FIdentFuncTable[I] := @Func79;
      85: FIdentFuncTable[I] := @Func85;
      88: FIdentFuncTable[I] := @Func88;
      91: FIdentFuncTable[I] := @Func91;
      92: FIdentFuncTable[I] := @Func92;
      96: FIdentFuncTable[I] := @Func96;
      99: FIdentFuncTable[I] := @Func99;
      100: FIdentFuncTable[I] := @Func100;
      102: FIdentFuncTable[I] := @Func102;
      108: FIdentFuncTable[I] := @Func108;
      105: FIdentFuncTable[I] := @Func105;
      106: FIdentFuncTable[I] := @Func106;
      133: FIdentFuncTable[I] := @Func133;
      else
        FIdentFuncTable[I] := @AltFunc;
    end;
end;

function TPasLexer.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['a'..'z', 'A'..'Z'] do
  begin
    Inc(Result, HashTable[ToHash^]);
    Inc(ToHash);
  end;
  if ToHash^ in ['_', '0'..'9'] then Inc(ToHash);
  FStringLen := ToHash - fToIdent;
end;

function TPasLexer.KeyComp(const Key: string): boolean;
var
  I: Integer;
begin
  FTemp := fToIdent;
  if Length(Key) = FStringLen then
  begin
    Result := True;
    for i := 1 to FStringLen do
    begin
      if HashTable[FTemp^] <> HashTable[Key[i]] then
      begin
        Result := False;
        Break;
      end;
      Inc(FTemp);
    end;
  end
  else
    Result := False;
end;

function TPasLexer.Func15: EPasToken;
begin
  if KeyComp('If') then
    Result := tkIf
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func19: EPasToken;
begin
  if KeyComp('Do') then
    Result := tkDo
  else
  if KeyComp('And') then
    Result := tkAnd
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func20: EPasToken;
begin
  if KeyComp('As') then
    Result := tkAs
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func21: EPasToken;
begin
  if KeyComp('Of') then
    Result := tkOf
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func23: EPasToken;
begin
  if KeyComp('End') then
    Result := tkEnd
  else
  if KeyComp('In') then
    Result := tkIn
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func28: EPasToken;
begin
  if KeyComp('case') then
    Result := tkCase
  else
  if KeyComp('Is') then
    Result := tkIs
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func32: EPasToken;
begin
  if KeyComp('Label') then
    Result := tkLabel
  else
  if KeyComp('Mod') then
    Result := tkMod
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func33: EPasToken;
begin
  if KeyComp('Or') then
    Result := tkOr
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func35: EPasToken;
begin
  if KeyComp('To') then
    Result := tkTo
  else
  if KeyComp('Div') then
    Result := tkDiv
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func37: EPasToken;
begin
  if KeyComp('Begin') then
    Result := tkBegin
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func39: EPasToken;
begin
  if KeyComp('For') then
    Result := tkFor
  else
  if KeyComp('Shl') then
    Result := tkShl
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func40: EPasToken;
begin
  if KeyComp('Packed') then
    Result := tkPacked
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func41: EPasToken;
begin
  if KeyComp('Else') then
    Result := tkElse
  else
  if KeyComp('Var') then
    Result := tkVar
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func44: EPasToken;
begin
  if KeyComp('Set') then
    Result := tkSet
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func45: EPasToken;
begin
  if KeyComp('Shr') then
    Result := tkShr
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func47: EPasToken;
begin
  if KeyComp('Then') then
    Result := tkThen
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func49: EPasToken;
begin
  if KeyComp('Not') then
    Result := tkNot
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func52: EPasToken;
begin
  if KeyComp('Raise') then
    Result := tkRaise
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func56: EPasToken;
begin
  if KeyComp('Out') then
    Result := tkOut
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func57: EPasToken;
begin
  if KeyComp('While') then
    Result := tkWhile
  else
  if KeyComp('Goto') then
    Result := tkGoto
  else
  if KeyComp('Xor') then
    Result := tkXor
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func60: EPasToken;
begin
  if KeyComp('With') then
    Result := tkWith
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func63: EPasToken;
begin
  if KeyComp('Record') then
    Result := tkRecord
  else
  if KeyComp('Try') then
    Result := tkTry
  else
  if KeyComp('Array') then
    Result := tkArray
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func65: EPasToken;
begin
  if KeyComp('Repeat') then
    Result := tkRepeat
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func66: EPasToken;
begin
  if KeyComp('Type') then
    Result := tkType
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func71: EPasToken;
begin
  if KeyComp('Const') then
    Result := tkConst
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func72: EPasToken;
begin
  if KeyComp('Static') then
    Result := tkStatic
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func73: EPasToken;
begin
  if KeyComp('Except') then
    Result := tkExcept
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func76: EPasToken;
begin
  if KeyComp('Until') then
    Result := tkUntil
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func79: EPasToken;
begin
  if KeyComp('Finally') then
    Result := tkFinally
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func85: EPasToken;
begin
  if KeyComp('Forward') then
    Result := tkForward
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func88: EPasToken;
begin
  if KeyComp('Program') then
    Result := tkProgram
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func91: EPasToken;
begin
  if KeyComp('Private') then
  begin
    if InSymbols(CharAhead(FStringLen)) then
      Result := tkIdentifier
    else
      Result := tkPrivate;
  end else
  if KeyComp('Downto') then
    Result := tkDownto
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func92: EPasToken;
begin
  if KeyComp('overload') then
    Result := tkOverload
  else
  if KeyComp('Inherited') then
    Result := tkInherited
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func96: EPasToken;
begin
  if KeyComp('Override') then
    Result := tkOverride
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func99: EPasToken;
begin
  if KeyComp('External') then
    Result := tkExternal
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func100: EPasToken;
begin
  if KeyComp('constref') then
    Result := tkConstRef
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func102: EPasToken;
begin
  if KeyComp('function') then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func105: EPasToken;
begin
  if KeyComp('Procedure') then
    Result := tkProcedure
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func106: EPasToken;
begin
  if KeyComp('Protected') then
  begin
    if InSymbols(CharAhead(FStringLen)) then
      Result := tkIdentifier
    else
      Result := tkProtected;
  end
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func108: EPasToken;
begin
  if KeyComp('Operator') then
    Result := tkOperator
  else
    Result := tkIdentifier;
end;

function TPasLexer.Func133: EPasToken;
begin
  if KeyComp('Property') then
    Result := tkProperty
  else
    Result := tkIdentifier;
end;

function TPasLexer.AltFunc: EPasToken;
begin
  Result := tkIdentifier;
end;

function TPasLexer.IdentKind(const MayBe: PChar): EPasToken;
var
  HashKey: Integer;
begin
  FToIdent := Maybe;
  HashKey := KeyHash(Maybe);
  if HashKey <= High(FIdentFuncTable) then
    Result := FIdentFuncTable[HashKey]()
  else
    Result := tkIdentifier;
end;

procedure TPasLexer.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #0:
        FProcTable[I] := @NullProc;
      #10:
        FProcTable[I] := @LFProc;
      #13:
        FProcTable[I] := @CRProc;
      #1..#9, #11, #12, #14..#32:
        FProcTable[I] := @SpaceProc;
      #39:
        FProcTable[I] := @StringProc;
      '#':
        FProcTable[I] := @CharProc;
      '$':
        FProcTable[I] := @HexNumberProc;
      '0'..'9':
        FProcTable[I] := @NumberProc;
      'A'..'Z', 'a'..'z', '_':
        FProcTable[I] := @IdentProc;
      '{':
        FProcTable[I] := @BraceOpenProc;
      '}':
        FProcTable[I] := @BraceCloseProc;
      '!', '"', '%', '&', '('..'/', ':'..'@', '['..'^':
        case I of
          '(': FProcTable[I] := @RoundOpenProc;
          ')': FProcTable[I] := @RoundCloseProc;
          '*': FProcTable[I] := @StarProc;
          '+': FProcTable[I] := @PlusProc;
          ',': FProcTable[I] := @CommaProc;
          '-': FProcTable[I] := @MinusProc;
          '.': FProcTable[I] := @PointProc;
          '/': FProcTable[I] := @SlashProc;
          ':': FProcTable[I] := @ColonProc;
          ';': FProcTable[I] := @SemiColonProc;
          '<': FProcTable[I] := @LowerProc;
          '=': FProcTable[I] := @EqualProc;
          '>': FProcTable[I] := @GreaterProc;
          '@': FProcTable[I] := @AddressProc;
          '[': FProcTable[I] := @SquareOpenProc;
          ']': FProcTable[I] := @SquareCloseProc;
          '^': FProcTable[I] := @PointerSymbolProc;
        end;
      else
        FProcTable[I] := @UnknownProc;
    end;
end;

constructor TPasLexer.Create;
begin
  inherited Create();

  InitIdentTables();
  MakeMethodTables();

  //Writeln(KeyHash(PChar('static')));
end;

procedure TPasLexer.SetOrigin(const NewValue: PChar);
begin
  FOrigin := NewValue;
  FComment := csNo;
  FLineNumber := 0;
  FLinePos := 0;
  FRun := 0;
  Next();
end;

procedure TPasLexer.SetRunPos(const Value: Integer);
begin
  FRun := Value;
  Next();
end;

procedure TPasLexer.AddressProc;
begin
  FTokenID := tkAddress;
  Inc(FRun);
end;

procedure TPasLexer.CharProc;
begin
  FTokenID := tkChar;
  Inc(FRun);
  while FOrigin[FRun] in ['0'..'9'] do
    Inc(FRun);
end;

procedure TPasLexer.BraceCloseProc;
begin
  Inc(FRun);
  FTokenId := tkUnknown;
end;

procedure TPasLexer.BorProc;
begin
  FTokenID := tkBorComment;
  case FOrigin[FRun] of
    #0:
      begin
        NullProc;
        Exit;
      end;
    #10:
      begin
        LFProc;
        Exit;
      end;
    #13:
      begin
        CRProc;
        Exit;
      end;
  end;

  while FOrigin[FRun] <> #0 do
    case FOrigin[FRun] of
      '}':
        begin
          FComment := csNo;
          Inc(FRun);
          Break;
        end;
      #10, #13:
        Break;
      else
        Inc(FRun);
    end;
end;

procedure TPasLexer.BraceOpenProc;
begin
  case FOrigin[FRun + 1] of
    '$': FTokenID := tkDirective;
    else
    begin
      FTokenID := tkBorComment;
      FComment := csBor;
    end;
  end;

  Inc(FRun);

  while FOrigin[FRun] <> #0 do
    case FOrigin[FRun] of
      '}':
        begin
          FComment := csNo;
          Inc(FRun);
          Break;
        end;
      #10, #13:
        Break;
      else
        Inc(FRun);
    end;
end;

procedure TPasLexer.ColonProc;
begin
  case FOrigin[FRun + 1] of
    '=':
      begin
        Inc(FRun, 2);
        FTokenID := tkAssign;
      end;
    else
    begin
      Inc(FRun);
      FTokenID := tkColon;
    end;
  end;
end;

procedure TPasLexer.CommaProc;
begin
  Inc(FRun);
  FTokenID := tkComma;
end;

procedure TPasLexer.CRProc;
begin
  FTokenID := tkLineEnding;

  case FOrigin[FRun + 1] of
    #10: Inc(FRun, 2);
    else
      Inc(FRun);
  end;
  Inc(FLineNumber);
  FLinePos := FRun;
end;

procedure TPasLexer.EqualProc;
begin
  Inc(FRun);
  FTokenID := tkEqual;
end;

procedure TPasLexer.GreaterProc;
begin
  case FOrigin[FRun + 1] of
    '=':
      begin
        Inc(FRun, 2);
        FTokenID := tkGreaterEqual;
      end;
    else
    begin
      Inc(FRun);
      FTokenID := tkGreater;
    end;
  end;
end;

function TPasLexer.InSymbols(const aChar: Char): boolean;
begin
  Result := aChar in ['#', '$', '&', #39, '(', ')', '*', '+', ',', '.', '/', ':', ';', '<', '=', '>', '@', '[', ']', '^'];
end;

function TPasLexer.CharAhead(const Count: Integer): Char;
begin
  FTemp := fOrigin + FRun + Count;
  while FTemp^ in [#1..#9, #11, #12, #14..#32] do
    Inc(FTemp);
  Result := FTemp^;
end;

procedure TPasLexer.IdentProc;
begin
  FTokenID := IdentKind(fOrigin + FRun);
  Inc(FRun, FStringLen);
  while Identifiers[fOrigin[FRun]] do Inc(FRun);
end;

procedure TPasLexer.HexNumberProc;
begin
  Inc(FRun);
  FTokenID := tkIntegerHex;
  while FOrigin[FRun] in ['0'..'9', 'A'..'F', 'a'..'f'] do
    Inc(FRun);
end;

procedure TPasLexer.LFProc;
begin
  FTokenID := tkLineEnding;
  Inc(FRun);
  Inc(FLineNumber);
  FLinePos := FRun;
end;

procedure TPasLexer.LowerProc;
begin
  case FOrigin[FRun + 1] of
    '=':
      begin
        Inc(FRun, 2);
        FTokenID := tkLowerEqual;
      end;
    '>':
      begin
        Inc(FRun, 2);
        FTokenID := tkNotEqual;
      end
    else
    begin
      Inc(FRun);
      FTokenID := tkLower;
    end;
  end;
end;

procedure TPasLexer.MinusProc;
begin
  case FOrigin[FRun + 1] of
    '=':
      begin
        Inc(FRun, 2);
        FTokenID := tkAssignPlus;
      end;
    else
    begin
      Inc(FRun);
      FTokenID := tkMinus;
    end;
  end;
end;

procedure TPasLexer.NullProc;
begin
  FTokenID := tkNull;
end;

procedure TPasLexer.NumberProc;
begin
  Inc(FRun);
  FTokenID := tkInteger;
  while FOrigin[FRun] in ['0'..'9', '.', 'e', 'E'] do
  begin
    case FOrigin[FRun] of
      '.':
        if FOrigin[FRun + 1] = '.' then
          Break
        else
          FTokenID := tkFloat
    end;
    Inc(FRun);
  end;
end;

procedure TPasLexer.PlusProc;
begin
  case FOrigin[FRun + 1] of
    '=':
      begin
        Inc(FRun, 2);
        FTokenID := tkAssignPlus;
      end;
    else
    begin
      Inc(FRun);
      FTokenID := tkPlus;
    end;
  end;
end;

procedure TPasLexer.PointerSymbolProc;
begin
  Inc(FRun);
  FTokenID := tkPointerSymbol;
end;

procedure TPasLexer.PointProc;
begin
  case FOrigin[FRun + 1] of
    '.':
      begin
        Inc(FRun, 2);
        FTokenID := tkDotDot;
      end;
    ')':
      begin
        Inc(FRun, 2);
        FTokenID := tkSquareClose;
        Dec(FSquareCount);
      end;
    else
    begin
      Inc(FRun);
      FTokenID := tkDot;
    end;
  end;
end;

procedure TPasLexer.RoundCloseProc;
begin
  Inc(FRun);
  FTokenID := tkRoundClose;
  Dec(FRoundCount);
end;

procedure TPasLexer.AnsiProc;
begin
  FTokenID := tkAnsiComment;
  case FOrigin[FRun] of
    #0:
      begin
        NullProc;
        Exit;
      end;
    #10:
      begin
        LFProc;
        Exit;
      end;
    #13:
      begin
        CRProc;
        Exit;
      end;
  end;

  while FOrigin[FRun] <> #0 do
    case FOrigin[FRun] of
      '*':
        if FOrigin[FRun + 1] = ')' then
        begin
          FComment := csNo;
          Inc(FRun, 2);

          Break;
        end else
          Inc(FRun);

      #10, #13:
        Break;
      else
        Inc(FRun);
    end;
end;

procedure TPasLexer.RoundOpenProc;
begin
  Inc(FRun);
  case FOrigin[FRun] of
    '*':
      begin
        FTokenID := tkAnsiComment;
        if FOrigin[FRun + 1] = '$' then
          FTokenID := tkDirective
        else
          FComment := csAnsi;

        Inc(FRun);

        while FOrigin[FRun] <> #0 do
          case FOrigin[FRun] of
            '*':
              if FOrigin[FRun + 1] = ')' then
              begin
                FComment := csNo;
                Inc(FRun, 2);
                Break;
              end
              else
                Inc(FRun);
            #10: Break;
            #13: Break;
            else
              Inc(FRun);
          end;
      end;

    '.':
      begin
        Inc(FRun);
        FTokenID := tkSquareOpen;
        Inc(FSquareCount);
      end;
    else
    begin
      FTokenID := tkRoundOpen;
      Inc(FRoundCount);
    end;
  end;
end;

procedure TPasLexer.SemiColonProc;
begin
  Inc(FRun);
  FTokenID := tkSemiColon;
end;

procedure TPasLexer.SlashProc;
begin
  case FOrigin[FRun + 1] of
    '/':
      begin
        Inc(FRun, 2);
        FTokenID := tkSlashesComment;
        while FOrigin[FRun] <> #0 do
        begin
          case FOrigin[FRun] of
            #10, #13: Break;
          end;
          Inc(FRun);
        end;
      end;

    '=':
      begin
        Inc(FRun, 2);
        FTokenID := tkAssignPlus;
      end;
    else
    begin
      Inc(FRun);
      FTokenID := tkSlash;
    end;
  end;
end;

procedure TPasLexer.SpaceProc;
begin
  Inc(FRun);
  FTokenID := tkSpace;
  while FOrigin[FRun] in [#1..#9, #11, #12, #14..#32] do
    Inc(FRun);
end;

procedure TPasLexer.SquareCloseProc;
begin
  Inc(FRun);
  FTokenID := tkSquareClose;
  Dec(FSquareCount);
end;

procedure TPasLexer.SquareOpenProc;
begin
  Inc(FRun);
  FTokenID := tkSquareOpen;
  Inc(FSquareCount);
end;

procedure TPasLexer.StarProc;
begin
  case FOrigin[FRun + 1] of
    '=':
      begin
        Inc(FRun, 2);
        FTokenID := tkAssignPlus;
      end;
    else
    begin
      Inc(FRun);
      FTokenID := tkStar;
    end;
  end;
end;

procedure TPasLexer.StringProc;
begin
  FTokenID := tkString;
  if (FOrigin[FRun + 1] = #39) and (FOrigin[FRun + 2] = #39) then Inc(FRun, 2);
  repeat
    case FOrigin[FRun] of
      #0, #10, #13: Break;
    end;
    Inc(FRun);
  until FOrigin[FRun] = #39;
  if FOrigin[FRun] <> #0 then Inc(FRun);
end;

procedure TPasLexer.UnknownProc;
begin
  Inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TPasLexer.Next();
begin
  case FTokenID of
    tkIdentifier:
      begin
        FLastIdentPos := fTokenPos;
        FLastNoSpace := FTokenID;
        FLastNoSpacePos := fTokenPos;
      end;
    tkSpace:
      begin
        FLastNoSpace := FTokenID;
        FLastNoSpacePos := fTokenPos;
      end;
  end;
  FTokenPos := FRun;
  case FComment of
    csNo: FProcTable[fOrigin[FRun]];
    else
      case FComment of
        csBor: BorProc;
        csAnsi: AnsiProc;
      end;
  end;
end;

function TPasLexer.GetToken: string;
var
  Len: Integer;
begin
  Result := '';
  Len := FRun - fTokenPos;
  SetString(Result, (FOrigin + fTokenPos), Len);
end;

procedure TPasLexer.NextID(const ID: EPasToken);
begin
  repeat
    case FTokenID of
      tkNull: Break;
      else
        Next();
    end;
  until (FTokenID = ID);
end;

procedure TPasLexer.NextNoJunk;
begin
  repeat
    Next();
  until not (FTokenID in [tkSlashesComment, tkAnsiComment, tkBorComment, tkLineEnding, tkSpace]);
end;

initialization
  MakeIdentTable();

end.
