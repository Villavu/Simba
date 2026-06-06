{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Base codetool methods for a tab.
}
unit simba.ide_editor_codetools;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base,
  simba.ide_codetools_insight;

type
  TSimbaEditor_Codetools = class(TComponent)
  public
    (*
      Returns a "normalized" string of the found expression at given X,Y
      Examples ('|' being CaretX/CaretY):
        "MyObject.Property|"            -> "MyObject.Property"
        "MyFoo.Tes|tin_1[0]"            -> "MyFoo.Testin_1"
        "Obj.Method(1, 2).Field|"       -> "Obj.Method().Field"
        "Matrix[X, GetY(), Z]^.Call()|" -> "Matrix[,,]^.Call()"
        "MyFunction(123, Target.| )"    -> "Target."
    *)
    function GetExpressionAt(CaretX, CaretY: Integer): String;
    function GetExpressionAtCaret: String;
    function GetAndRunParser: TCodeinsight;
  end;

implementation

uses
  simba.ide_editor,
  simba.ide_codetools_paslexer;

type
  TSimbaEditor_CodetoolsHelper = class helper for TSimbaEditor_Codetools
    function Editor: TSimbaEditor; inline;
  end;

function TSimbaEditor_CodetoolsHelper.Editor: TSimbaEditor;
begin
  Result := TSimbaEditor(Owner);
end;

function TSimbaEditor_Codetools.GetExpressionAt(CaretX, CaretY: Integer): String;
const
  MAX_LINES_BACK = 5;
var
  Tokens: TLexerTokenArray;
  CursorIndex: Integer;

  function FindStart: Integer;
  var
    I, NestRound, NestSquare: Integer;
  begin
    Result := CursorIndex;
    NestRound := 0;
    NestSquare := 0;

    for I := CursorIndex downto 0 do
    begin
      case Tokens[I].ID of
        tokRoundClose:  Inc(NestRound);
        tokSquareClose: Inc(NestSquare);
        tokRoundOpen:
          begin
            Dec(NestRound);
            if NestRound < 0 then Break; // Stop at unowned opening parenthesis
          end;

        tokSquareOpen:
          begin
            Dec(NestSquare);
            if NestSquare < 0 then Break; // Stop at unowned opening bracket
          end;

        tokIdentifier, tokPointerSymbol, tokPoint,
        tokStringConst, tokIntegerConst, tokFloat, tokAsciiChar: begin { valid } end;

        else if (NestRound <= 0) and (NestSquare <= 0) then
          Break;
      end;
      Result := I;
    end;
  end;

  function Reconstruct(StartIdx, EndIdx: Integer): String;
  var
    I, NestRound, NestSquare: Integer;
  begin
    Result := '';
    NestRound := 0;
    NestSquare := 0;

    for I := StartIdx to EndIdx do
    begin
      case Tokens[I].ID of
        tokRoundOpen:
          begin
            Result := Result + '(';
            Inc(NestRound);
          end;

        tokRoundClose:
          begin
            Dec(NestRound);
            Result := Result + ')';
          end;

        tokSquareOpen:
          begin
            Result := Result + '[';
            Inc(NestSquare);
          end;

        tokSquareClose:
          begin
            Dec(NestSquare);
            Result := Result + ']';
          end;

        tokComma:
          begin
            // Add commas to know dimensions
            if ((NestSquare = 1) and (NestRound = 0)) or
               ((NestRound = 1)  and (NestSquare = 0)) or
               ((NestRound <= 0) and (NestSquare <= 0)) then
              Result := Result + ',';
          end;

        else if (NestRound <= 0) and (NestSquare <= 0) then // Append if not in bracket
          Result := Result + Tokens[I].Text;
      end;
    end;
  end;

var
  StartLine, I, TargetPos, TokenEnd: Integer;
  Chunk: String;
  Lex: TPasLexer;
begin
  Result := '';

  CaretY := CaretY - 1;
  if (CaretY < 0) or (CaretY >= Editor.Lines.Count) then
    Exit;
  StartLine := CaretY - MAX_LINES_BACK;
  if (StartLine < 0) then
    StartLine := 0;

  TargetPos := 1;
  Chunk := '';
  for I := StartLine to CaretY do
  begin
    if I = CaretY then
      TargetPos := Length(Chunk) + CaretX;
    Chunk := Chunk + Editor.Lines[I] + LineEnding;
  end;
  if (Chunk = '') then
    Exit;

  Lex := TPasLexer.Create(Chunk);
  try
    Tokens := Lex.GetTokenStream();
  finally
    Lex.Free();
  end;
  if (Length(Tokens) = 0) then
    Exit;

  CursorIndex := -1;
  for I := 0 to High(Tokens) do
  begin
    TokenEnd := Tokens[I].Pos + Length(Tokens[I].Text);
    if (TargetPos >= Tokens[I].Pos) and (TargetPos <= TokenEnd) then
    begin
      CursorIndex := I;
      Break;
    end;

    if (Tokens[I].Pos > TargetPos) then
    begin
      CursorIndex := I - 1;
      if CursorIndex < 0 then CursorIndex := 0;
      Break;
    end;
  end;
  if (CursorIndex = -1) then
    CursorIndex := High(Tokens);

  Result := Reconstruct(FindStart(), CursorIndex);
end;

function TSimbaEditor_Codetools.GetExpressionAtCaret: String;
begin
  Result := GetExpressionAt(Editor.CaretX, Editor.CaretY);
end;

function TSimbaEditor_Codetools.GetAndRunParser: TCodeinsight;
begin
  Result := TCodeinsight.Create();
  Result.SetScript(Editor.Text, Editor.FileName, Editor.RowColToCharIndex(Editor.CaretXY));
  Result.Run();
end;

end.

