{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_codetools_exprparser;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.ide_codetools_base, simba.ide_codetools_insight, simba.ide_codetools_parser;

function ParseExpression(Codeinsight: TCodeinsight; Expr: String; out Members: TDeclarationArray): TDeclaration;

implementation

uses
  simba.ide_codetools_paslexer;

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
var
  Lex: TPasLexer;
  InSquare, InRound: Integer;
  LastToken: ELexerToken;
  I: Integer;
begin
  Result := [];

  InSquare := 0;
  InRound := 0;

  Lex := TPasLexer.Create(Str);
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

function ParseExpression(Codeinsight: TCodeinsight; Expr: String; out Members: TDeclarationArray): TDeclaration;

  function Resolve(decls: TDeclarationArray; var Item: TExpressionItem): TDeclaration;
  var
    I: Integer;
  begin
    Result := nil;

    for i := 0 to High(Decls) do
      if Decls[i].IsName(Item.Text) then
      begin
        if Decls[i] is TDeclaration_Type then
        begin
          Result := Decls[i];
          Exit;
        end;

        if Decls[i] is TDeclaration_Method then
          if TDeclaration_Method(Decls[i]).ResultType <> nil then
          begin
            // indexable property.. clear symbol.
            if (Decls[i] is TDeclaration_Property) and (TDeclaration_Property(Decls[i]).ParamCount > 0) then
            begin
              Item.Symbols.DimCount := 0;
              Item.HasSymbols := Item.Symbols.Deref or Item.Symbols.DerefDim;
            end;

            Result := Codeinsight.ResolveVarType(TDeclaration_Method(Decls[i]).ResultType);
            Exit;
          end;

        if (Decls[i] is TDeclaration_Var) then
        begin
          Result := Codeinsight.ResolveVarType(TDeclaration_Var(Decls[i]).VarType);
          Exit;
        end;
      end;
  end;

  function ResolveSymbols(Decl: TDeclaration; item: TExpressionItem): TDeclaration;

    function DoPointerDerefence(Decl: TDeclaration): TDeclaration;
    begin
      Result := Decl;
      if (Result is TDeclaration_TypePointer) then
        Result := Codeinsight.ResolveVarType(TDeclaration_TypePointer(Result).VarType);
    end;

    function DoStringIndex(Decl: TDeclaration): TDeclaration;
    var
      Decls: TDeclarationArray;
    begin
      if Decl.IsName('String') then
        Decls := Codeinsight.Get('Char')
      else
        Decls := Codeinsight.Get('WideChar');

      Decls := Decls.GetByClass(TDeclaration_Type);
      if (Length(Decls) > 0) then
        Result := Codeinsight.ResolveVarType(Decls[0])
      else
        Result := nil;
    end;

    function DoArrayIndex(Decl: TDeclaration; Dimensions: Integer): TDeclaration;
    begin
      Result := nil;

      while (Decl is TDeclaration_TypeArray) and (Dimensions > 0) do
      begin
        Result := Codeinsight.ResolveVarType(TDeclaration_TypeArray(Decl).VarType);
        if (Result = Decl) then // protect against infinite loop
          Break;

        Dimensions := Dimensions - TDeclaration_TypeArray(Decl).DimCount;
        Decl := Result;
      end;

      if (Dimensions = 1) then
        if (Decl is TDeclaration_TypeAlias) and (Decl.IsName('String') or Decl.IsName('UnicodeString') or Decl.IsName('WideString')) then
          Result := DoStringIndex(Decl);
    end;

  begin
    Result := Decl;

    if Item.HasSymbols then
    begin
      if Item.Symbols.Deref        then Result := DoPointerDerefence(Result);
      if Item.Symbols.DimCount > 0 then Result := DoArrayIndex(Result, Item.Symbols.DimCount);
      if Item.Symbols.DerefDim     then Result := DoPointerDerefence(Result);
    end;
  end;

var
  Items: TExpressionItems;
  Decl: TDeclaration;
  I: Integer;
  Last: TExpressionItem;
begin
  Result := nil;

  Items := StringToExpression(Expr);
  if Length(Items) = 0 then
    Exit;
  Members := Codeinsight.Get(Items[0].Text);

  for i := 0 to High(Items) - 1 do
  begin
    Decl := Resolve(Members, Items[I]);
    if (Decl = nil) then
    begin
      CodetoolsMessage('Failed resolving ' + Items[I].Text);
      Break;
    end;

    Decl := ResolveSymbols(Decl, Items[I]);
    if (Decl = nil) then
    begin
      CodetoolsMessage('Failed resolving symbols ' + Items[I].Text);
      Break;
    end;

    if (Decl is TDeclaration_Type) then
      Members := Codeinsight.GetTypeMembers(Decl as TDeclaration_Type)
    else
      Members := Codeinsight.Get(Decl.Name);
  end;

  // last item is special
  Last := Items[High(Items)];
  if Last.HasSymbols then
    Result := ResolveSymbols(Resolve(Members, Last), Last)
  else
  begin
    for I := 0 to High(Members) do
      if Members[I].IsName(Last.Text) then
      begin
        Result := Members[I];
        Break;
      end;
  end;
end;

end.

