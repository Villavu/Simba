unit v_MiscFunctions;

interface

type
  TStringArray = array of string;

function Explode(Sep, s: string): TStringArray;
function PrePrepareString(s: string): string;
function PrepareString(s: string; out ArrayCount: Integer): string; overload;
function PrepareString(s: string): string; overload;
function GetFirstWord(s: string): string;
function CompressWhiteSpace(s: string): string;

implementation

uses
  SysUtils, StrUtils;

function Explode(Sep, s: string): TStringArray;
var
  p1, p2: Integer;
begin
  SetLength(Result, 0);
  s := s + Sep;
  p1 := 1;
  p2 := Pos(Sep, s);
  while (p1 > 0) and (p2 > 0) do
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := Copy(s, p1, p2 - p1);
    p1 := p2 + Length(Sep);
    p2 := PosEx(Sep, s, p1 + 1);
  end;
end;

function PrePrepareString(s: string): string;
var
  i: Integer;
  BraceCount: Integer;
begin
  Result := '';
  BraceCount := 0;
  for i := 1 to Length(s) do
    if (BraceCount = 0) and (s[i] in ['a'..'z', 'A'..'Z', '0'..'9', '_', '.', '[', ']']) then
      Result := Result + s[i]
    else if (s[i] = '(') then
      Inc(BraceCount)
    else if (s[i] = ')') then
      Dec(BraceCount);
end;

function PrepareString(s: string; out ArrayCount: Integer): string; overload;
var
  i: Integer;
  BraceCount, BracketCount: Integer;
begin
  Result := '';
  BraceCount := 0;
  BracketCount := 0;
  ArrayCount := 0;
  s := UpperCase(s);
  for i := 1 to Length(s) do
    if (BraceCount = 0) and (BracketCount = 0) and (s[i] in ['A'..'Z', '0'..'9', '_']) then
      Result := Result + s[i]
    else if (s[i] = '[') then
    begin
      if (BracketCount = 0) then
        Inc(ArrayCount);
      Inc(BracketCount);
    end
    else if (s[i] = ']') then
      Dec(BracketCount)
    else if (s[i] = '(') then
      Inc(BraceCount)
    else if (s[i] = ')') then
      Dec(BraceCount);
end;

function PrepareString(s: string): string; overload;
var
  a: Integer;
begin
  Result := PrepareString(s, a);
end;

function GetFirstWord(s: string): string;
var
  i: Integer;
begin
  i := 1;
  while (i <= Length(s)) and (s[i] in ['a'..'z', 'A'..'Z'])  do
    Inc(i);
  Result := Copy(s, 1, i - 1);
end;

function CompressWhiteSpace(s: string): string;

  function GetNextChar(s: string; i: Integer; out c: Char): Boolean;
  begin
    Result := True;
    c := #0;

    while (i <= Length(s)) and (s[i] in [#10, #11, #13, #32]) do
      Inc(i);
    if (i <= Length(s)) then
      c := s[i]
    else
      Result := False;
  end;

var
  i: Integer;
  c: Char;
  LastSpace: Boolean;
begin
  Result := '';
  i := 1;
  LastSpace := False;
  while (i <= Length(s)) do
  begin
    if (s[i] in [#10, #11, #13, #32]) then
    begin
      if (not LastSpace) and (GetNextChar(s, i, c) and (c in ['a'..'z', 'A'..'Z', '0'..'9', '_'])) then
        Result := Result + ' ';
      Inc(i);
      while (i <= Length(s)) and (s[i] in [#10, #11, #13, #32]) do
        Inc(i);
      LastSpace := True;
    end
    else
    begin
      LastSpace := False;
      Result := Result + s[i];
      if (s[i] in [':', ';']) then
      begin
        Result := Result + ' ';
        LastSpace := True;
      end;
      Inc(i);
    end;
  end;
end;

end.
