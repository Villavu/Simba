{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_codetools_utils;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  mPasLexTypes, mPasLex;

type
  TNullableString = object
  private
    FValue: String;
    FHasValue: Boolean;

    function GetIsNull: Boolean;
    procedure SetIsNull(const AValue: Boolean);
    procedure SetValue(const AValue: String);
  public
    property IsNull: Boolean read GetIsNull write SetIsNull;
    property Value: String read FValue write SetValue;
  end;

type
  TExpressionItem = record
    Text: String;
    HasSymbols: Boolean;
    Symbols: record
      DimCount: Integer; // [0,0] or [0][0] is `DimCount=2`
      Deref: Boolean;    // arr^
      DerefDim: Boolean; // arr[0]^
    end;
  end;
  TExpressionItems = array of TExpressionItem;

function StringToExpression(const Str: String): TExpressionItems;

implementation

uses
  simba.mufasatypes, simba.env;

procedure TNullableString.SetValue(const AValue: String);
begin
  FValue := AValue;
  FHasValue := True;
end;

procedure TNullableString.SetIsNull(const AValue: Boolean);
begin
  FHasValue := not AValue;
end;

function TNullableString.GetIsNull: Boolean;
begin
  Result := not FHasValue;
end;

function StringToExpression(const Str: String): TExpressionItems;
var
  Lex: TmwPasLex;
  InSquare, InRound: Integer;
  LastToken: TptTokenKind;
  I: Integer;
begin
  Result := [];

  InSquare := 0;
  InRound := 0;

  Lex := TmwPasLex.Create(Str);
  try
    Lex.NextNoJunk();
    if (not (Lex.TokenID in [tokIdentifier, tokStringConst])) then
      Exit;

    LastToken := Lex.TokenID;
    while (Lex.TokenID <> tokNull) do
    begin
      case Lex.TokenID of
        tokIdentifier:
          begin
            if (InRound = 0) and (InSquare = 0) then
            begin
              SetLength(Result, Length(Result) + 1);
              Result[High(Result)].Text := Lex.Token;
            end;
          end;
        tokSquareOpen:
          begin
            Inc(InSquare);
          end;
        tokSquareClose:
          begin
            if (InSquare > 0) and (InRound = 0) and (Length(Result) > 0) then
              Inc(Result[High(Result)].Symbols.DimCount);

            Dec(InSquare);
          end;
        tokComma:
          begin
            if (InSquare > 0) and (InRound = 0) and (Length(Result) > 0) then
              Inc(Result[High(Result)].Symbols.DimCount);
          end;
        tokRoundOpen:
          begin
            Inc(InRound);
          end;
        tokRoundClose:
          begin
            Dec(InRound);
          end;
        tokPointerSymbol:
          begin
            if (InRound = 0) and (InSquare = 0) and (Length(Result) > 0) then
            begin
              if (LastToken = tokSquareClose) then
                Result[High(Result)].Symbols.DerefDim := True
              else
                Result[High(Result)].Symbols.Deref := True;
            end;
          end;
        tokStringConst:
          begin
            if (InRound = 0) and (InSquare = 0) then
            begin
              SetLength(Result, Length(Result) + 1);
              Result[High(Result)].Text := 'String';
            end;
          end;
      end;

      LastToken := Lex.TokenID;
      Lex.NextNoJunk();
    end;

    for I := 0 to High(Result) do
      Result[I].HasSymbols := (Result[I].Symbols.DimCount > 0) or Result[I].Symbols.Deref or Result[I].Symbols.DerefDim;
  finally
    Lex.Free();
  end;
end;

end.
