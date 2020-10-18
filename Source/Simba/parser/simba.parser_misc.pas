unit simba.parser_misc;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
  sysutils;

type
  TExpressionItem = record Identifier: String; Dimensions: Int32; Deref: Boolean; DerefArray: Boolean; end;
  TExpressionArray = array of TExpressionItem;
  TExpressionArrayHelper = type Helper for TExpressionArray
    function Pop: TExpressionItem;
    function PopLeft: TExpressionItem;
  end;

function GetExpression(Text: String; Start: Int32): String;
function GetExpressionArray(Expression: String): TExpressionArray;

implementation

function GetExpression(Text: String; Start: Int32): String;
var
  i: Int32;
  InIndex, InParams: Int32;
begin
  Result := '';
  if (Start > Length(Text)) then
    Exit;

  InIndex := 0;
  InParams := 0;

  for i := Start downto 1 do
  begin
    case Text[i] of
      ')':
        begin
          if InParams = 0 then
            Result := Text[i] + Result;

          InParams += 1;
        end;

      '(':
        begin
          if InParams = 1 then
            Result := Text[i] + Result;
          if InParams = 0 then
            Break;

          InParams -= 1;
        end;

      ']':
        begin
          if (InParams = 0) then
          begin
            Result := Text[i] + Result;

            InIndex += 1;
          end;
        end;

      '[':
        begin
          if InIndex <= 0 then
            Break;

          if (InParams = 0) then
          begin
            Result := Text[i] + Result;

            InIndex -= 1;
          end;
        end;

      ',':
        if (InIndex > 0) and (InParams = 0) then
        begin
          Result := Text[i] + Result;
        end;

      else
      begin
        if (InParams = 0) and (InIndex = 0) then
        begin
          if (not (Text[i] in ['a'..'z', 'A'..'Z', '0'..'9', '_', '.', '^'])) then
            Break;

          Result := Text[i] + Result
        end;
      end;
    end;
  end;
end;

function GetExpressionArray(Expression: String): TExpressionArray;
var
  Identifier: String;
  Dimensions: Int32;
  Deref: Boolean;
  DerefArray: Boolean;

  procedure Push;
  var
    i: Int32;
  begin
    if (Identifier <> '') then
    begin
      SetLength(Result, Length(Result) + 1);
      for i := High(Result) downto 1 do
        Result[i] := Result[i - 1];

      Result[0].Identifier := Identifier;
      Result[0].Dimensions := Dimensions;
      Result[0].Deref := Deref;
      Result[0].DerefArray := DerefArray;
    end;

    Identifier := '';
    Dimensions := 0;
    Deref := False;
    DerefArray := False;
  end;

var
  i: Int32;
  IndexCount, ParameterCount: Int32;
begin
  Result := nil;

  IndexCount := 0;
  ParameterCount := 0;

  Identifier := '';
  Dimensions := 0;
  Deref := False;
  DeRefArray := False;

  for i := Length(Expression) downto 1 do
  begin
    case Expression[i] of
      '(':
        begin
          Inc(ParameterCount);
        end;

      ')':
        begin
          Dec(ParameterCount);
        end;

      '[':
        begin
          if (ParameterCount = 0) then
            Dec(IndexCount);
        end;

      ']':
        if (ParameterCount = 0) then
        begin
          Inc(Dimensions);
          Inc(IndexCount);
        end;

      ',':
        if (IndexCount > 0) and (ParameterCount = 0) then
          Inc(Dimensions);

      '^':
        if (IndexCount = 0) and (ParameterCount = 0) then
        begin
          if Expression[i - 1] = ']' then
            DerefArray := True
          else
            Deref := True;
        end;

      else
        if (IndexCount = 0) and (ParameterCount = 0) then
        begin
          if (Expression[i] = '.') then
            Push()
          else
            Identifier := UpCase(Expression[i]) + Identifier;
        end;
    end;
  end;

  Push();
end;

function TExpressionArrayHelper.Pop: TExpressionItem;
begin
  if Length(Self) = 0 then
    Exit(Default(TExpressionItem));

  Result := Self[High(Self)];
  Self := Copy(Self, 0, Length(Self) - 1);
end;

function TExpressionArrayHelper.PopLeft: TExpressionItem;
begin
  if Length(Self) = 0 then
    Exit(Default(TExpressionItem));

  Result := Self[0];
  Self := Copy(Self, 1);
end;

end.
