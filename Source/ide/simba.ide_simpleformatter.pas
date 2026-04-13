{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Simple code formatter. Simple being does not do a proper parse (like JCF) just tries to tidy code up token by token.
}
unit simba.ide_simpleformatter;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base;

function FormatScript(const Script: String): String;

implementation

uses
  simba.containers,
  simba.ide_simplelexer;

function FormatScript(const Script: String): String;
type
  EBlockType = (None, InFuncBlock, InFuncHeaderBlock, InVarBlock);
  EFormatFlag = (
    fsFirstInLine,
    fsLastAsSpace,
    fsLastWasKeyword,
    fsLastWasComment,
    fsLastWasTypeGuard,
    fsInDoubleQuotes
  );
  TFormatState = set of EFormatFlag;
var
  Builder: TSimbaStringBuilder;
  Lexer: TPasLexer;

  InBlock: EBlockType;
  State: TFormatState;

  Indents: array of Integer;
  Indent: Integer;
  ProcIndents: array of Integer;
  ProcIndent: Integer;
  ElseIndent: Integer;
  DirectiveIndent: Integer;

  BeginCount: Integer;
  BraceCount: Integer;

  procedure AddLine;
  begin
    if (Builder.Count > 0) and (not Builder.EndsWith(LineEnding)) then
      Builder.Append(LineEnding);

    Include(State, fsFirstInLine);
  end;

  procedure Add;
  var
    IsKeyword: Boolean;
    TokenStr: String;
    SpaceCount: Integer;
  begin
    TokenStr := Lexer.Token;

    if (Lexer.TokenID in [tkAnsiComment, tkBorComment, tkSlashesComment]) then
    begin
      if (fsFirstInLine in State) then
        Builder.Append(StringOfChar(' ', 2 * (Indents[Indent] + ElseIndent)))
      else
        Builder.Append(' ');

      Builder.Append(TokenStr);
      Include(State, fsLastWasComment);
      Exclude(State, fsFirstInLine);
      Exit;
    end;

    if (Lexer.TokenID in [tkEqual, tkColon, tkLower, tkPointerSymbol, tkOf]) then
      Include(State, fsLastWasTypeGuard)
    else
      Exclude(State, fsLastWasTypeGuard);

    IsKeyword := Lexer.TokenID in PasTokens_Keywords;

    if (fsLastWasComment in State) and not (fsFirstInLine in State) then
      AddLine();

    Exclude(State, fsLastWasComment);

    if (fsFirstInLine in State) then
    begin
      SpaceCount := Indents[Indent] + ElseIndent;

      if Lexer.TokenID in [tkEnd, tkUntil] then
        Dec(SpaceCount)
      else if Lexer.TokenID = tkDirective then
        Inc(SpaceCount, DirectiveIndent);

      if SpaceCount > 0 then
        Builder.Append(StringOfChar(' ', 2 * SpaceCount));
    end;

    if IsKeyword then
    begin
      if not ((fsFirstInLine in State) or (fsLastAsSpace in State)) then
        Builder.Append(' ');

      Builder.Append(LowerCase(TokenStr));
      Exclude(State, fsLastAsSpace);
    end else
    begin
      case Lexer.TokenID of
        tkRoundOpen, tkSquareOpen:
          begin
            if (fsLastWasKeyword in State) and not (fsLastAsSpace in State) then
              Builder.Append(' ');

            Builder.Append(TokenStr);
            Include(State, fsLastAsSpace);
          end;

        tkDot, tkDotDot, tkAddress:
          begin
            Builder.Append(TokenStr);
            Include(State, fsLastAsSpace);
          end;

        tkComma:
          begin
            Builder.Append(TokenStr);
            Exclude(State, fsFirstInLine);
            Exclude(State, fsLastAsSpace);
            Include(State, fsLastWasKeyword);
            Exit;
          end;

        tkColon, tkSemiColon, tkRoundClose, tkSquareClose:
          begin
            Builder.Append(TokenStr);
            Exclude(State, fsLastAsSpace);
          end;

        tkSlash, tkStar, tkMinus, tkPlus:
          begin
            if not (fsFirstInLine in State) then
              Builder.Append(' ');
            Builder.Append(TokenStr);

            if (fsFirstInLine in State) or (fsLastAsSpace in State) or (fsLastWasKeyword in State) then
              Include(State, fsLastAsSpace)
            else
              Exclude(State, fsLastAsSpace);

            Exclude(State, fsFirstInLine);
            Exclude(State, fsLastWasKeyword);
            Exit;
          end;

        tkAssign, tkAssignPlus, tkAssignDiv, tkAssignMinus, tkAssignMul, tkEqual:
          begin
            if not ((fsFirstInLine in State) or (fsLastAsSpace in State)) then
              Builder.Append(' ');

            Builder.Append(TokenStr);
            Builder.Append(' ');

            Exclude(State, fsFirstInLine);
            Include(State, fsLastAsSpace);
            Exclude(State, fsLastWasKeyword);
            Exit;
          end;
        else
        begin
          if (TokenStr = '"') then
          begin
            if not (fsInDoubleQuotes in State) then
            begin
              if not ((fsFirstInLine in State) or (fsLastAsSpace in State)) then
                Builder.Append(' ');
              Builder.Append(TokenStr);
              Include(State, fsLastAsSpace);
              Include(State, fsInDoubleQuotes);
            end else
            begin
              Builder.Append(TokenStr);
              Exclude(State, fsInDoubleQuotes);
              Exclude(State, fsLastAsSpace);
            end;
          end else
          begin
            if not ((fsFirstInLine in State) or (fsLastAsSpace in State)) then
            begin
              if not (fsInDoubleQuotes in State) then
                Builder.Append(' ');
            end;
            Builder.Append(TokenStr);

            if (fsInDoubleQuotes in State) then
              Include(State, fsLastAsSpace)
            else
              Exclude(State, fsLastAsSpace);
          end;
        end;
      end;
    end;

    Exclude(State, fsFirstInLine);

    if IsKeyword then
      Include(State, fsLastWasKeyword)
    else
      Exclude(State, fsLastWasKeyword);
  end;

  procedure NextToken(AllowEmptyLine: Boolean = True);
  var
    LineCount: Integer = 0;
  begin
    repeat
      Lexer.Next();
      if (Lexer.TokenID = tkLineEnding) then
        Inc(LineCount);
    until (not (Lexer.TokenID in [tkLineEnding, tkSpace]));

    if (LineCount > 0) then
      Include(State, fsFirstInLine);

    if (LineCount > 1) and AllowEmptyLine then
    begin
      if not Builder.EndsWith(LineEnding) then
        Builder.Append(LineEnding);
      Builder.Append(LineEnding);
    end;
  end;

  procedure PushIndent;
  begin
    Inc(Indent);
    if (Indent >= Length(Indents)) then
      SetLength(Indents, Length(Indents) + 32);

    Indents[Indent] := Indents[Indent - 1] + ElseIndent + 1;
  end;

begin
  Result := '';

  State := [fsFirstInLine];

  BeginCount := 0;
  InBlock := None;
  Indent := 0;
  ElseIndent := 0;
  ProcIndent := 0;
  DirectiveIndent := 0;

  SetLength(Indents, 32);
  SetLength(ProcIndents, 16);

  Lexer := TPasLexer.Create();
  try
    Lexer.Origin := PChar(Script);

    while (Lexer.TokenID <> tkNull) do
    begin
      case Lexer.TokenID of

        tkDirective:
          begin
            if (Pos('{$ENDIF', UpperCase(Lexer.Token)) = 1) or (Pos('{$ELSE', UpperCase(Lexer.Token)) = 1) then
            begin
              if (DirectiveIndent > 0) then
                Dec(DirectiveIndent);
            end;

            if (fsFirstInLine in State) then
              AddLine();

            Add();
            NextToken();

            if (fsFirstInLine in State) and (Lexer.TokenID <> tkNull) then
              AddLine();

            if (Pos('{$IF', UpperCase(Lexer.Token)) = 1) or (Pos('{$ELSE', UpperCase(Lexer.Token)) = 1) then
              Inc(DirectiveIndent);

            Continue;
          end;

        tkAnsiComment, tkBorComment, tkSlashesComment:
          begin
            Add();
            NextToken();
            if (fsFirstInLine in State) and (Lexer.TokenID <> tkNull) then
              AddLine();
            Continue;
          end;

        tkProcedure, tkFunction, tkOperator, tkProperty:
          begin
            if (fsLastWasTypeGuard in State) then
              Add()
            else
            begin
              if (InBlock = InVarBlock) and (Indent > 0) then
                Dec(Indent);

              ElseIndent := 0;

              if (ProcIndent = 0) then
                Indents[Indent] := 0
              else
                Indents[Indent] := Indents[Indent] + 1;

              Inc(ProcIndent);
              if (ProcIndent >= Length(ProcIndents)) then
                SetLength(ProcIndents, Length(ProcIndents) + 16);

              ProcIndents[ProcIndent] := Indents[Indent];

              AddLine();
              Add();
              NextToken(False);
              Add();
              InBlock := InFuncHeaderBlock;
            end;
          end;

        tkRoundOpen, tkSquareOpen:
          begin
            if (InBlock = InVarBlock) then
              Include(State, fsLastWasKeyword);

            Add();
            NextToken(False);
            BraceCount := 1;

            while (Lexer.TokenID <> tkNull) and (BraceCount > 0) do
            begin
              if (fsFirstInLine in State) then
              begin
                AddLine();
                if not (Lexer.TokenID in [tkRoundClose, tkSquareClose]) then
                  Builder.Append('  ');
              end;

              if (Lexer.TokenID in [tkRoundOpen, tkSquareOpen]) then
                Inc(BraceCount)
              else if (Lexer.TokenID in [tkRoundClose, tkSquareClose]) then
                Dec(BraceCount);

              Add();
              NextToken();
            end;
            Continue;
          end;

        tkIf, tkWhile, tkFor, tkCase, tkWith:
          begin
            Add();
            NextToken(False);
            Continue;
          end;

        tkDo, tkThen:
          begin
            Add();
            NextToken(False);
            if (Lexer.TokenID in [tkBegin, tkTry, tkRepeat]) then
            begin
              AddLine();
            end else
            begin
              Inc(ElseIndent);
              if (fsFirstInLine in State) then
                AddLine()
              else
                Exclude(State, fsLastAsSpace);
            end;
            Continue;
          end;

        tkElse:
          begin
            AddLine();

            if (ElseIndent > 0) then
              Dec(ElseIndent);

            Add();
            NextToken(False);

            if (Lexer.TokenID = tkIf) then
            begin
              Exclude(State, fsFirstInLine);
              Exclude(State, fsLastAsSpace);
            end else if (Lexer.TokenID in [tkBegin, tkTry, tkRepeat]) then
            begin
              AddLine();
            end else
            begin
              Inc(ElseIndent);
              if (fsFirstInLine in State) then
                AddLine()
              else
                Exclude(State, fsLastAsSpace);
            end;
            Continue;
          end;

        tkColon:
          begin
            Add();
            if (InBlock = InFuncBlock) and (BeginCount > 1) then
            begin
              NextToken();
              if (Lexer.TokenID in PasTokens_Keywords) then
              begin
                Inc(ElseIndent);
                AddLine();
              end;
              Continue;
            end;
          end;

        tkOf:
          begin
            if (InBlock = InVarBlock) or (InBlock = InFuncHeaderBlock) then
              Exclude(State, fsLastAsSpace);

            Add();

            if (InBlock <> InVarBlock) and (InBlock <> InFuncHeaderBlock) then
            begin
              Inc(BeginCount);
              PushIndent();
              AddLine();
              ElseIndent := 0;
            end;
          end;

        tkBegin, tkRepeat:
          begin
            if (InBlock <> InFuncBlock) or (BeginCount = 0) then
            begin
              Indent := 0;
              ElseIndent := 0;
            end;

            AddLine();
            Add();
            Inc(BeginCount);
            PushIndent();
            AddLine();
            ElseIndent := 0;
            InBlock := InFuncBlock;
          end;

        tkTry:
          begin
            AddLine();
            Add();
            Inc(BeginCount);
            PushIndent();
            AddLine();
            ElseIndent := 0;
          end;

        tkExcept, tkFinally:
          begin
            AddLine();
            ElseIndent := -1;
            Add();
            ElseIndent := 0;
            AddLine();
          end;

        tkEnd, tkUntil:
          begin
            if (BeginCount > 0) then
              Dec(BeginCount);

            if (BeginCount = 0) and (InBlock = InFuncBlock) then
              InBlock := None;

            AddLine();
            ElseIndent := 0;
            Add();

            if (Indent > 0) then
              Dec(Indent);

            if (ProcIndent > 0) and (Indents[Indent] = ProcIndents[ProcIndent]) then
            begin
              if (Indents[Indent] > 0) then
                Indents[Indent] := Indents[Indent] - 1;
              Dec(ProcIndent);
            end;

            if (Lexer.TokenID = tkEnd) then
            begin
              NextToken();
              if (Lexer.TokenID <> tkSemiColon) and (Lexer.TokenID <> tkDot) then
              begin
                if not ((Lexer.TokenID in [tkAnsiComment, tkBorComment, tkSlashesComment]) and not (fsFirstInLine in State)) then
                  AddLine();
              end;

              Continue;
            end;
          end;

        tkVar, tkConst, tkType, tkLabel:
          begin
            Indent := 0;
            ElseIndent := 0;

            AddLine();
            InBlock := InVarBlock;
            Add();
            PushIndent();
            AddLine();
          end;

        tkRecord:
          begin
            ElseIndent := 0;
            InBlock := InVarBlock;

            Add();
            PushIndent();
            AddLine();
          end;

        tkSemiColon:
          begin
            Add();
            NextToken();

            if (Lexer.TokenID in [tkExternal, tkForward, tkOverload, tkOverride, tkStatic, tkConstRef]) then
            begin
              if (Lexer.TokenID in [tkExternal, tkForward]) then
              begin
                InBlock := None;

                if (Indents[Indent] > 0) then
                  Indents[Indent] := Indents[Indent] - 1;

                if (ProcIndent > 0) then
                  Dec(ProcIndent);
              end else
                InBlock := InFuncHeaderBlock;
            end else
            begin
              ElseIndent := 0;
              if not ((Lexer.TokenID in [tkAnsiComment, tkBorComment, tkSlashesComment]) and not (fsFirstInLine in State)) then
                AddLine();
            end;

            Continue;
          end;
        else
          Add();
      end;

      NextToken();
    end;

    Result := Builder.Str;
  finally
    Lexer.Free();
  end;
end;

end.

