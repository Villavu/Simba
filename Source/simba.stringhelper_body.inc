{%MainUnit simba.mufasatypes}

function TStringHelper.Before(const Value: String): String;
var
  P: Integer;
begin
  P := Pos(Value, Self);
  if (P = 0) then
    Result := ''
  else
    Result := System.Copy(Self, 1, P - 1);
end;

function TStringHelper.After(const Value: String): String;
var
  P: Integer;
begin
  P := Pos(Value, Self);
  if (P = 0) then
    Result := ''
  else
    Result := System.Copy(Self, P + System.Length(Value), (System.Length(Self) - System.Length(Value)) + 1);
end;

function TStringHelper.Between(const S1, S2: String): String;
var
  I, J: Integer;
begin
  Result := '';

  I := System.Pos(S1, Self);
  if (I > 0) then
  begin
    I := I + System.Length(S1);
    J := System.Pos(S2, Self, I);
    if (J > 0) then
      Result := System.Copy(Self, I, J-I);
  end;
end;

function TStringHelper.BetweenAll(const S1, S2: String): TStringArray;

  procedure Add(var Offset: Integer);
  var
    I: Integer;
  begin
    Offset := System.Pos(S1, Self, Offset);

    if (Offset > 0) then
    begin
      Offset := Offset + System.Length(S1);
      I := System.Pos(S2, Self, Offset);
      if (I > 0) then
        Result := Result + [System.Copy(Self, Offset, I - Offset)];
    end;
  end;

var
  Offset: Integer;
begin
  Result := Default(TStringArray);

  Offset := 1;
  while (Offset > 0) do
    Add(Offset);
end;

function TStringHelper.RegExprSplit(const Pattern: String): TStringArray;
var
  PrevPos: PtrInt;
  RegExpr: TRegExpr;
begin
  Result := Default(TStringArray);

  PrevPos := 1;

  RegExpr := TRegExpr.Create(Pattern);
  try
    if RegExpr.Exec(Self) then
    repeat
      Result := Result + [System.Copy(Self, PrevPos, RegExpr.MatchPos[0] - PrevPos)];

      PrevPos := RegExpr.MatchPos[0] + RegExpr.MatchLen[0];
    until not RegExpr.ExecNext();
  finally
    RegExpr.Free();
  end;

  Result := Result + [System.Copy(Self, PrevPos)];
end;

function TStringHelper.RegExprFind(const Pattern: String): TRegExprMatch;

  function Match(const RegExpr: TRegExpr): TRegExprMatch;
  begin
    Result.Match    := RegExpr.Match[0];
    Result.Length   := RegExpr.MatchLen[0];
    Result.Position := RegExpr.MatchPos[0];
  end;

var
  RegExpr: TRegExpr;
begin
  Result := Default(TRegExprMatch);

  RegExpr := TRegExpr.Create(Pattern);
  try
    if RegExpr.Exec(Self) then
      Result := Match(RegExpr);
  finally
    RegExpr.Free();
  end;
end;

function TStringHelper.RegExprExists(const Pattern: String): Boolean;
var
  RegExpr: TRegExpr;
begin
  RegExpr := TRegExpr.Create(Pattern);
  try
    Result := RegExpr.Exec(Self);
  finally
    RegExpr.Free();
  end;
end;

function TStringHelper.RegExprFindAll(const Pattern: String): TRegExprMatchArray;

  function Match(const RegExpr: TRegExpr): TRegExprMatch;
  begin
    Result.Match    := RegExpr.Match[0];
    Result.Length   := RegExpr.MatchLen[0];
    Result.Position := RegExpr.MatchPos[0];
  end;

var
  RegExpr: TRegExpr;
begin
  Result := Default(TRegExprMatchArray);

  RegExpr := TRegExpr.Create(Pattern);
  try
    if RegExpr.Exec(Self) then
    repeat
      Result := Result + [Match(RegExpr)];
    until not RegExpr.ExecNext();
  finally
    RegExpr.Free();
  end;
end;

function TStringHelper.IndexOfAny(const Values: TStringArray): Integer;
begin
  Result := IndexOfAny(Values, 1);
end;

function TStringHelper.IndexOfAny(const Values: TStringArray; Offset: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;

  for I := 0 to High(Values) do
  begin
    Result := Self.IndexOf(Values[I], Offset);
    if (Result > 0) then
      Exit;
  end;
end;

function TStringHelper.IndexOf(const Value: String): Integer;
begin
  Result := Pos(Value, Self);
end;

function TStringHelper.IndexOf(const Value: String; Offset: Integer): Integer;
begin
  Result := Pos(Value, Self, Offset);
end;

function TStringHelper.LastIndexOf(const Value: String): Integer;
begin
  Result := RPos(Value, Self);
end;

function TStringHelper.LastIndexOf(const Value: String; Offset: Integer): Integer;
begin
  Result := RPosEx(Value, Self, Offset);
end;

function TStringHelper.IndicesOfChar(const Value: Char; Offset: Integer): TIntegerArray;
var
  Matches: specialize TSimbaOverAllocateArray<Integer>;
  I: Integer;
begin
  Matches.Init(32);

  if (Offset > 0) then
    for I := Offset to Length(Self) do
      if (Self[I] = Value) then
        Matches.Add(I);

  Result := Matches.Trim();
end;

function TStringHelper.IndicesOf(const Value: String): TIntegerArray;
var
  Matches: SizeIntArray;
  I: Integer;
begin
  Result := Default(TIntegerArray);
  if (Self = '') then
    Exit;

  if Length(Value) = 1 then
    Result := IndicesOfChar(Value[1], 1)
  else
  if FindMatchesBoyerMooreCaseSensitive(Self, Value, Matches, True) then
  begin
    SetLength(Result, System.Length(Matches));
    for I := 0 to High(Matches) do
      Result[I] := Matches[I];
  end;
end;

function TStringHelper.IndicesOf(const Value: String; Offset: Integer): TIntegerArray;
var
  Matches: SizeIntArray;
  I: Integer;
begin
  Result := Default(TIntegerArray);
  if (Self = '') then
    Exit;

  if Length(Value) = 1 then
    Result := IndicesOfChar(Value[1], Offset)
  else
  if FindMatchesBoyerMooreCaseSensitive(Self.Copy(Offset, MaxInt), Value, Matches, True) then
  begin
    SetLength(Result, System.Length(Matches));
    for I := 0 to High(Matches) do
      Result[I] := Matches[I];
  end;
end;

class function TStringHelper.NumberChars: String;
begin
  Result := '0123456789';
end;

class function TStringHelper.AlphaChars: String;
begin
  Result := LowerChars() + UpperChars();
end;

class function TStringHelper.LowerChars: String;
begin
 Result := 'abcdefghijklmnopqrstuvwxyz';
end;

class function TStringHelper.UpperChars: String;
begin
 Result := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
end;

function TStringHelper.ToUpper: String;
begin
  Result := UpperCase(Self);
end;

function TStringHelper.ToLower: String;
begin
  Result := LowerCase(Self);
end;

function TStringHelper.Capitalize: String;
var
  I: Integer;
begin
  SetLength(Result, Length(Self));

  for I := 1 to Length(Self) do
    if (I = 1) then
      Result[I] := UpCase(Self[I])
    else
      Result[I] := LowerCase(Self[I]);
end;

function TStringHelper.Extract(const Characters: String): String;
var
  I, Count: Integer;
begin
  SetLength(Result, System.Length(Self));
  Count := 1;

  for I := 1 to System.Length(Self) do
    if Pos(Self[I], Characters) > 0 then
    begin
      Result[Count] := Self[I];
      Inc(Count);
    end;

  SetLength(Result, Count - 1);
end;

function TStringHelper.ExtractNumber(Default: Int64): Int64;
begin
  Result := StrToIntDef(Extract('0123456789-.'), Default);
end;

function TStringHelper.ExtractNumber(const ANumberChars: String; Default: Int64): Int64;
begin
  Result := StrToIntDef(Extract(ANumberChars), Default);
end;

function TStringHelper.Trim: String;
begin
  Result := SysUtils.Trim(Self);
end;

function TStringHelper.Trim(const TrimChars: array of Char): String;
begin
  Result := Self.TrimLeft(TrimChars).TrimRight(TrimChars);
end;

function TStringHelper.TrimLeft: String;
begin
  Result := SysUtils.TrimLeft(Self);
end;

function TStringHelper.TrimLeft(const TrimChars: array of Char): String;

  function IsTrimChar(C: Char): Boolean; inline;
  var
    I: Integer;
  begin
    for I := 0 to High(TrimChars) do
      if (C = TrimChars[I]) then
        Exit(True);
    Result := False;
  end;

var
  I, Len: SizeInt;
begin
  I := 1;
  Len := System.Length(Self);
  while (I <= Len) and IsTrimChar(Self[I]) do
    Inc(I);

  if (I = 1) then
    Result := Self
  else if (I > Len) then
    Result := ''
  else
    Result := System.Copy(Self, I, Len-I+1);
end;

function TStringHelper.TrimRight: String;
begin
  Result := SysUtils.TrimRight(Self);
end;

function TStringHelper.TrimRight(const TrimChars: array of Char): String;

  function IsTrimChar(C: Char): Boolean; inline;
  var
    I: Integer;
  begin
    for I := 0 to High(TrimChars) do
      if (C = TrimChars[I]) then
        Exit(True);
    Result := False;
  end;

var
  I, Len: SizeInt;
begin
  Len := System.Length(Self);
  I := Len;
  while (I >= 1) and IsTrimChar(Self[I]) do
    Dec(I);

  if (I < 1) then
    Result := ''
  else if (I = Len) then
    Result := Self
  else
    Result := System.Copy(Self, 1, I);
end;

function TStringHelper.StartsWith(const Value: String; CaseSenstive: Boolean): Boolean;
begin
  case CaseSenstive of
    False: Result := SameText(Copy(1, Length(Value)), Value);
    True:  Result := System.Copy(Self, 1, Length(Value)) = Value;
  end;
end;

function TStringHelper.EndsWith(const Value: String; CaseSenstive: Boolean): Boolean;
begin
  case CaseSenstive of
    False: Result := SameText(System.Copy(Self, Length(Self) - Length(Value), Length(Value)), Value);
    True:  Result := System.Copy(Self, Length(Self) - Length(Value), Length(Value)) = Value;
  end;
end;

function TStringHelper.Replace(const OldValue: String; const NewValue: String): String;
begin
  Result := StringReplace(Self, OldValue, NewValue, [rfReplaceAll]);
end;

function TStringHelper.Replace(const OldValue: String; const NewValue: String; ReplaceFlags: TReplaceFlags): String;
begin
  Result := StringReplace(Self, OldValue, NewValue, ReplaceFlags)
end;

function TStringHelper.Contains(const Value: String; CaseSenstive: Boolean): Boolean;
begin
  Result := ContainsAny([Value], CaseSenstive);
end;

function TStringHelper.ContainsAny(const Values: TStringArray; CaseSenstive: Boolean): Boolean;
var
  I: Integer;
begin
  case CaseSenstive of
    True:
      for I := 0 to High(Values) do
        if Self.IndexOf(Values[I]) > 0 then
          Exit(True);

    False:
      for I := 0 to High(Values) do
        if Self.ToLower().IndexOf(Values[I].ToLower()) > 0 then
          Exit(True);
  end;

  Result := False;
end;

function TStringHelper.Count(const Value: String): Integer;
begin
  Result := Length(IndicesOf(Value));
end;

function TStringHelper.Split(const Seperator: String): TStringArray;
var
  Len: SizeInt;

  function NextSep(const StartIndex: SizeInt): SizeInt;
  begin
    Result := Self.IndexOf(Seperator, StartIndex);
  end;

  procedure Append(const S: String);
  begin
    if (Len >= Length(Result)) then
      SetLength(Result, Length(Result) * 2);
    Result[Len] := S;
    Inc(Len);
  end;

var
  Sep, LastSep: SizeInt;
begin
  SetLength(Result, 16);

  Len := 0;
  LastSep := 1;
  Sep := NextSep(1);
  while (Sep > 0) do
  begin
    Append(Copy(LastSep, Sep - LastSep));

    LastSep := Sep + Length(Seperator);
    Sep := NextSep(LastSep);
  end;

  if (LastSep <= Length(Self)) then
    Append(Copy(LastSep, MaxInt));

  if (Len > 0) and (Result[Len - 1] = '') then
    Dec(Len);

  SetLength(Result, Len);
end;

function TStringHelper.Copy: String;
begin
  Result := System.Copy(Self, 1, Length(Self));
end;

function TStringHelper.Copy(StartIndex, Count: Integer): String;
begin
  Result := System.Copy(Self, StartIndex, Count);
end;

function TStringHelper.Copy(StartIndex: Integer): String;
begin
  Result := System.Copy(Self, StartIndex, MaxInt);
end;

function TStringHelper.CopyRange(StartIndex, EndIndex: Integer): String;
begin
  Result := System.Copy(Self, StartIndex, (EndIndex - StartIndex) + 1);
end;

procedure TStringHelper.Delete(StartIndex, Count: Integer);
begin
  System.Delete(Self, StartIndex, Count);
end;

procedure TStringHelper.DeleteRange(StartIndex, EndIndex: Integer);
begin
  System.Delete(Self, StartIndex, (EndIndex - StartIndex) + 1);
end;

procedure TStringHelper.Insert(const Value: String; Index: Integer);
begin
  System.Insert(Value, Self, Index);
end;

function TStringHelper.PadLeft(Count: Integer; PaddingChar: Char): String;
begin
  Count := Count - Length(Self);
  if (Count > 0) then
    Result := StringOfChar(PaddingChar, Count) + Self
  else
    Result := Self;
end;

function TStringHelper.PadRight(Count: Integer; PaddingChar: Char): String;
begin
  Count := Count - Length(Self);
  if (Count > 0) then
    Result := Self + StringOfChar(PaddingChar, Count)
  else
    Result := Self;
end;
