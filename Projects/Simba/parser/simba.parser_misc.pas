unit simba.parser_misc;

{$mode objfpc}{$H+}

interface

uses
  sysutils;

function GetExpression(constref Text: String; Start: Int32): String;
function CleanExpression(constref Text: String): String;
function SplitExpression(constref Text: String): TStringArray;

implementation

function GetExpression(constref Text: String; Start: Int32): String;
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
        if (InParams = 0) then
        begin
          Result := Text[i] + Result;

          InIndex += 1;
        end;

      '[':
        if (InParams = 0) then
        begin
          Result := Text[i] + Result;

          InIndex -= 1;
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
          if (not (Text[i] in ['a'..'z', 'A'..'Z', '0'..'9', '_', '.'])) then
            Break;

          Result := Text[i] + Result
        end;
      end;
    end;
  end;
end;

// Remove parameters and index contents
function CleanExpression(constref Text: String): String;
var
  i, InIndex, InParameter: Int32;
begin
  Result := '';

  InIndex := 0;
  InParameter := 0;

  for i := Length(Text) downto 1 do
  begin
    if Text[i] = ')' then
      InParameter += 1
    else
    if Text[i] = '(' then
      InParameter -= 1
    else
    if (Text[i] = ']') and (InParameter = 0) then
    begin
      Result := Text[i] + Result;
      InIndex += 1;
    end else
    if (Text[i] = '[') and (InParameter = 0) then
    begin
      Result := Text[i] + Result;
      InIndex -= 1;
    end else
    if (Text[i] = ',') and (InIndex > 0) and (InParameter = 0) then
      Result := Text[i] + Result
    else
    if (InIndex = 0) and (InParameter = 0) then
      Result := Text[i] + Result;
  end;

  Result := Result.ToUpper();
end;

function SplitExpression(constref Text: String): TStringArray;
begin
  Result := CleanExpression(Text).Split('.');
end;

end.
