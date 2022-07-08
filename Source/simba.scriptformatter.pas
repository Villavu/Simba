{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
// Originally by Nielsie95 at http://villavu.com/forum/showthread.php?t=35513
unit simba.scriptformatter;

{$i simba.inc}

interface

uses
  classes, sysutils;

function FormatScript(const Script: String): String;

implementation

uses
  simba.paslex;

type
  TSimbaCodeFormatter = class
  protected
  type
    EBlockType = (None, InFuncBlock, InFuncHeaderBlock, InVarBlock);
  protected
    FOutput: String;
    FLexer: TPasLexer;

    FInBlock: EBlockType;

    FIndents: array[0..1000] of Integer;
    FIndent: Integer;
    FProcIndents: array[0..1000] of Integer;
    FProcIndent: Integer;
    FElseIndent: Integer;

    FBeginCount: Integer;

    FFirstInLine: Boolean;

    FLastAsSpace: Boolean;
    FLastWasKeyword: Boolean;
    FLastWasComment: Boolean;

    procedure Add;
    procedure AddLine;
    procedure AddDirective;
  public
    function Format(Script: String): String;

    constructor Create;
    destructor Destroy; override;
  end;

procedure TSimbaCodeFormatter.Add;
var
  IsKeyword: Boolean;
begin
  IsKeyword := FLexer.TokenID in PasTokens_Keywords;
  if FLastWasComment and (not FFirstInLine) then
    AddLine();

  FLastWasComment := False;

  if FFirstInLine then
    if (FLexer.TokenID in [tkEnd, tkUntil]) then
      FOutput := FOutput + StringOfChar(' ', 2 * (FIndents[FIndent] + FElseIndent - 1))
    else
      FOutput := FOutput + StringOfChar(' ', 2 * (FIndents[FIndent] + FElseIndent));

  if IsKeyWord then
  begin
    if (FFirstInLine or FLastAsSpace) and (not (FLexer.TokenID in [tkAssign, tkAssignPlus, tkAssignDiv, tkAssignMinus, tkAssignMul])) then
      FOutput := FOutput + LowerCase(FLexer.Token)
    else
      FOutput := FOutput + ' ' + LowerCase(FLexer.Token);

    FLastAsSpace := False;
  end else
  begin
    case FLexer.TokenID of
      tkRoundOpen, tkSquareOpen:
        begin
          if FLastWasKeyword then
            FOutput := FOutput + ' ' + FLexer.Token
          else
            FOutput := FOutput + FLexer.Token;

          FLastAsSpace := True;
        end;

      tkDot, tkDotDot:
        begin
          FOutput := FOutput + FLexer.Token;

          FLastAsSpace := True;
        end;

      tkComma:
        begin
          FOutput := FOutput + FLexer.Token;

          FFirstInLine := False;
          FLastAsSpace := False;
          FLastWasKeyword := True;

          Exit;
        end;

      tkColon, tkSemiColon, tkRoundClose, tkSquareClose:
        begin
          FOutput := FOutput + FLexer.Token;

          FLastAsSpace := False;
        end;

      tkDiv, tkStar, tkMinus, tkPlus:
        begin
          //if (FLexer.TokenPos > 1) and (FLexer.Origin[FLexer.TokenPos - 1] <> #32) then
          //  FOutput := FOutput + FLexer.Token
          //else
            FOutput := FOutput + ' ' + FLexer.Token;

          FFirstInLine := False;
          FLastAsSpace := False;
          FLastWasKeyword := False;

          Exit;
        end;

      tkAssign, tkAssignPlus, tkAssignDiv, tkAssignMinus, tkAssignMul:
        begin
          FOutput := FOutput + ' ' + FLexer.Token;
          FFirstInLine := False;
          FLastAsSpace := False;
          FLastWasKeyword := True;

          Exit;
        end;
      else
      begin
        if FFirstInLine or FLastAsSpace then
          FOutput := FOutput + FLexer.Token
        else
          FOutput := FOutput + ' ' + FLexer.Token;

        FLastAsSpace := False;
      end;
    end;
  end;

  FFirstInLine := False;
  FLastWasKeyword := IsKeyword;
end;

procedure TSimbaCodeFormatter.AddLine;
begin
  if (FOutput <> '') and (not FOutput.EndsWith(LineEnding+LineEnding)) then
    FOutput := FOutput + LineEnding;

  FFirstInLine := True;
end;

procedure TSimbaCodeFormatter.AddDirective;
begin
  FOutput := FOutput + FLexer.Token;

  FFirstInLine := False;
  FLastAsSpace := False;
  FLastWasKeyword := False;

  AddLine();
end;

function TSimbaCodeFormatter.Format(Script: String): String;

  procedure Reset;
  begin
    FOutput := '';

    FLastAsSpace := False;
    FLastWasKeyword := False;
    FLastWasComment := False;

    FBeginCount := 0;
    FInBlock := None;
    FFirstInLine := True;

    FElseIndent := 0;
    FProcIndent := 0;
    FIndent := 0;

    FillDWord(FIndents[0], Length(FIndents), 0);
    FillDWord(FProcIndents[0], Length(FProcIndents), 0);
  end;

  procedure Next;
  var
    EmptyLineCount: Integer = 0;
  begin
    repeat
      FLexer.Next();
      if (FLexer.TokenID = tkLineEnding) then
        Inc(EmptyLineCount);
    until (not (FLexer.TokenID in [tkLineEnding, tkSpace]));

    if (EmptyLineCount > 1) then
    begin
      if not FOutput.EndsWith(LineEnding) then
        FOutput += LineEnding;
      FOutput += LineEnding;
    end;
  end;

var
  BraceCount: Integer;
begin
  Reset();

  FLexer.Origin := PChar(Script);

  while (FLexer.TokenID <> tkNull) do
  begin
    case FLexer.TokenID of
      tkDirective:
        AddDirective();

      tkProcedure, tkFunction, tkOperator:
        begin
          if (FInBlock = InVarBlock) and (FIndent > 0) then
            Dec(FIndent);

          FElseIndent := 0;

          if FProcIndent = 0 then
            FIndents[FIndent] := 0
          else
            FIndents[FIndent] := FIndents[FIndent] + 1;

          Inc(FProcIndent);
          FProcIndents[FProcIndent] := FIndents[FIndent];

          AddLine();
          Add();
          Next();
          Add();

          FInBlock := InFuncHeaderBlock;
        end;

      tkRoundOpen:
        begin
          if (FInBlock = InVarBlock) then
            FLastWasKeyword := True;

          Add();
          Next();
          BraceCount := 1;

          while (FLexer.TokenID <> tkNull) and (BraceCount > 0) do
          begin
            if (FLexer.TokenID = tkRoundOpen) then
              Inc(BraceCount)
            else if (FLexer.TokenID = tkRoundClose) then
              Dec(BraceCount);

            Add();
            Next();
          end;

          Continue;
        end;

      tkDo, tkThen:
        begin
          Add();
          Next();

          if (FLexer.TokenID = tkBegin) then
            Continue
          else if (FLexer.TokenID in [tkTry, tkRepeat]) then
          begin
            Inc(FElseIndent);
            Continue;
          end else
          begin
            Inc(FElseIndent);

            AddLine();
            Add();
          end;
        end;

      tkElse:
        begin
          if (not FFirstInLine) then
            AddLine();
          if (FElseIndent > 0) then
            Dec(FElseIndent);

          Add();
          Next();

          if (FLexer.TokenID in [tkBegin, tkIf]) then
            Continue
          else if (FLexer.TokenID in [tkTry, tkRepeat]) then
          begin
            Inc(FElseIndent);
            Continue;
          end else
          begin
            Inc(FElseIndent);
            AddLine();
            Add();
          end;
        end;

      tkColon:
        begin
          Add();

          if (FInBlock = InFuncBlock) and (FBeginCount > 1) then
          begin
            Next();
            if (FLexer.TokenID in PasTokens_Keywords) then
            begin
              Inc(FElseIndent);
              AddLine();
            end;

            Continue;
          end;
        end;

      tkOf:
        begin
          if (FInBlock = InVarBlock) or (FInBlock = InFuncHeaderBlock) then
            FLastAsSpace := False;

          Add();

          if (FInBlock <> InVarBlock) and (FInBlock <> InFuncHeaderBlock) then
          begin
            Inc(FBeginCount);
            Inc(FIndent);
            FIndents[FIndent] := FIndents[FIndent - 1] + FElseIndent + 1;
            AddLine();
            FElseIndent := 0;
          end;
        end;

      tkBegin, tkRepeat:
        begin
          if (FInBlock <> InFuncBlock) or (FBeginCount = 0) then
          begin
            FIndent := 0;
            FElseIndent := 0;
          end;

          if (not FFirstInLine) or (FInBlock = None) then
            AddLine();

          Add();
          Inc(FBeginCount);
          Inc(FIndent);
          FIndents[FIndent] := FIndents[FIndent - 1] + FElseIndent + 1;
          AddLine();
          FElseIndent := 0;
          FInBlock := InFuncBlock;
        end;

      tkTry:
        begin
          if (not FFirstInLine) then
            AddLine();

          Add();
          Inc(FBeginCount);
          Inc(FIndent);
          FIndents[FIndent] := FIndents[FIndent - 1] + FElseIndent + 1;
          AddLine();
          FElseIndent := 0;
        end;

      tkExcept, tkFinally:
        begin
          if (not FFirstInLine) then
            AddLine();

          FElseIndent := -1;
          Add();
          FElseIndent := 0;
          AddLine();
        end;

      tkEnd, tkUntil:
        begin
          if (FBeginCount > 0) then
            Dec(FBeginCount);
          if (FBeginCount = 0) and (FInBlock = InFuncBlock) then
            FInBlock := None;

          if (not FFirstInLine) then
            AddLine();

          FElseIndent := 0;

          Add();
          if (FIndent > 0) then
            Dec(FIndent);

          if (FProcIndent > 0) and (FIndents[FIndent] = FProcIndents[FProcIndent]) then
          begin
            if (FIndents[FIndent] > 0) then
              FIndents[FIndent] := FIndents[FIndent] - 1;
            Dec(FProcIndent);
          end;

          if (FLexer.TokenID = tkEnd) then
          begin
            Next();
            if (FLexer.TokenID <> tkSemiColon) and (FLexer.TokenID <> tkDot) then
              AddLine();

            Continue;
          end;
        end;

      tkVar, tkConst, tkType, tkLabel:
        begin
          FIndent := 0;
          FElseIndent := 0;

          if (FInBlock <> InFuncHeaderBlock) then
            AddLine()
          else
          if (not FFirstInLine) then
            AddLine();

          FInBlock := InVarBlock;

          Add();
          Inc(FIndent);
          FIndents[FIndent] := FIndents[FIndent - 1] + FElseIndent + 1;
          AddLine();
        end;

      tkRecord:
        begin
          FElseIndent := 0;
          FInBlock := InVarBlock;

          Add();
          Inc(FIndent);
          FIndents[FIndent] := FIndents[FIndent - 1] + FElseIndent + 1;
          AddLine();
        end;

      tkSemiColon:
        begin
          if (not FFirstInLine) then
          begin
            Add();
            Next();

            if (FLexer.TokenID in [tkExternal, tkForward, tkOverload, tkOverride, tkStatic, tkConstRef]) then
            begin
              if (FLexer.TokenID in [tkExternal, tkForward]) then
              begin
                FInBlock := None;

                if (FIndents[FIndent] > 0) then
                  FIndents[FIndent] := FIndents[FIndent] - 1;

                if (FProcIndent > 0) then
                  Dec(FProcIndent);
              end else
                FInBlock := InFuncHeaderBlock;
            end else
            begin
              FElseIndent := 0;

              AddLine();
            end;

            Continue;
          end;
        end;
      else
        Add();
    end;

    Next();
  end;

  Result := FOutput;
end;

constructor TSimbaCodeFormatter.Create;
begin
  FLexer := TPasLexer.Create();
end;

destructor TSimbaCodeFormatter.Destroy;
begin
  if (FLexer <> nil) then
    FreeAndNil(FLexer);

  inherited Destroy;
end;

function FormatScript(const Script: String): String;
var
  Formatter: TSimbaCodeFormatter;
begin
  Result := '';

  Formatter := TSimbaCodeFormatter.Create();
  try
    Result := Formatter.Format(Script);
  finally
    Formatter.Free();
  end;
end;

end.

