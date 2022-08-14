{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.parser_misc;

{$i simba.inc}

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
  InStr, InMultiStr: Boolean;
begin
  Result := '';
  if (Start > Length(Text)) then
    Exit;

  InIndex := 0;
  InParams := 0;
  InStr := False;
  InMultiStr := False;

  for i := Start downto 1 do
  begin
    if InStr then
    begin
      if (Text[I] <> #39) then
        Continue;

      InStr := False;
      Result := 'String' + Result;
    end;

    if InMultiStr then
    begin
      if (Text[I] <> #34) then
        Continue;

      InMultiStr := False;
      Result := 'String' + Result;
    end;

    case Text[i] of
      ')':
        begin
          if (InParams = 0) and (InIndex = 0) then
            Result := Text[i] + Result;

          InParams += 1;
        end;

      '(':
        begin
          if (InParams = 1) and (InIndex = 0) then
            Result := Text[i] + Result;
          if InParams = 0 then
            Break;

          InParams -= 1;
        end;

      ']':
        begin
          InIndex += 1;
          if (InParams = 0) and (InIndex = 1) then
            Result := Text[i] + Result;
        end;

      '[':
        begin
          InIndex -= 1;
          if (InParams = 0) and (InIndex = 0) then
            Result := Text[i] + Result;
        end;

      ',':
        if (InIndex > 0) and (InParams = 0) and (InIndex = 1) then
        begin
          Result := Text[i] + Result;
        end;

      else
        if (InParams = 0) and (InIndex = 0) then
        begin
          case Text[I] of
            #39: InStr := True;
            #34: InMultiStr := True;
            'a'..'z',
            'A'..'Z',
            '0'..'9',
            '_', '.', '^':
              Result := Text[i] + Result;
            else
              Break;
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
