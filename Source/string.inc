{%MAINUNIT simba.base}

{$IFDEF HEADER}
type
  PRegExprGroup = ^TRegExprGroup;
  TRegExprGroup = record
    Position: Integer;
    Length: Integer;
    Match: String;
  end;
  TRegExprGroups = array of TRegExprGroup;

  PRegExprMatch = ^TRegExprMatch;
  TRegExprMatch = record
    Position: Integer;
    Length: Integer;
    Match: String;

    Groups: TRegExprGroups;
  end;
  PRegExprMatchArray = ^TRegExprMatchArray;
  TRegExprMatchArray = array of TRegExprMatch;

  TSimbaCharHelper = type helper for Char
  public
    function IsUpper(): Boolean;
    function IsLower(): Boolean;
    function IsAlphaNum(): Boolean;
    function IsInteger(): Boolean;
    function IsFloat(): Boolean;

    function ToUpper(): Char;
    function ToLower(): Char;
    function Capitalize(): Char;

    function Join(const Values: TStringArray): String;
  end;

  TSimbaStringHelper = type helper for String
  public const
    NumberChars   = '0123456789';
    LowerChars    = 'abcdefghijklmnopqrstuvwxyz';
    UpperChars    = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
    AlphaChars    = LowerChars + UpperChars;
    AlphaNumChars = AlphaChars + NumberChars;
  public
    function Equals(Other: String): Boolean;
    function EqualsIgnoreCase(Other: String): Boolean;
    function Compare(Other: String): Integer;
    function Hash(Seed: UInt32 = 0): UInt32;

    function IsUpper(): Boolean;
    function IsLower(): Boolean;
    function IsAlphaNum(): Boolean;
    function IsInteger(): Boolean;
    function IsFloat(): Boolean;

    function ToUpper(): String;
    function ToLower(): String;
    function Capitalize(): String;
    function SwapCase(): String;

    function Before(const Value: String): String;
    function After(const Value: String): String;

    function Between(const S1, S2: String): String;
    function BetweenAll(const S1, S2: String): TStringArray;

    function RegExprSplit(const Pattern: String): TStringArray;
    function RegExprFindAll(const Pattern: String): TRegExprMatchArray;
    function RegExprFind(const Pattern: String): TRegExprMatch;
    function RegExprExists(const Pattern: String): Boolean;

    function IndexOfAny(const Values: TStringArray): Integer; overload;
    function IndexOfAny(const Values: TStringArray; Offset: Integer): Integer; overload;

    function IndexOf(const Value: String): Integer; overload;
    function IndexOf(const Value: String; Offset: Integer): Integer; overload;
    function LastIndexOf(const Value: String): Integer; overload;
    function LastIndexOf(const Value: String; Offset: Integer): Integer; overload;

    function IndicesOf(const Value: String): TIntegerArray; overload;
    function IndicesOf(const Value: String; Offset: Integer): TIntegerArray; overload;

    function Extract(const Chars: array of Char): String;
    function ExtractInteger(Default: Int64 = -1): Int64;
    function ExtractFloat(Default: Extended = -1): Extended;

    function Trim: String; overload;
    function Trim(const TrimChars: array of Char): String; overload;

    function TrimLeft: String; overload;
    function TrimLeft(const TrimChars: array of Char): String; overload;

    function TrimRight: String; overload;
    function TrimRight(const TrimChars: array of Char): String; overload;

    function StartsWith(const Value: String; CaseSenstive: Boolean = True): Boolean;
    function EndsWith(const Value: String; CaseSenstive: Boolean = True): Boolean;

    function Partition(const Value: String; CaseSenstive: Boolean = True): TStringArray;
    function Replace(const OldValue: String; const NewValue: String): String; overload;
    function Replace(const OldValue: String; const NewValue: String; ReplaceFlags: TReplaceFlags): String; overload;

    function Contains(const Value: String; CaseSenstive: Boolean = True): Boolean;
    function ContainsAny(const Values: TStringArray; CaseSenstive: Boolean = True): Boolean;

    function Count(const Value: String): Integer;
    function CountAll(const Values: TStringArray): TIntegerArray;

    function Join(const Values: TStringArray): String;
    function Split(const Seperators: TStringArray): TStringArray; overload;
    function Split(const Seperator: String): TStringArray; overload;

    function Copy: String;
    function CopyRange(StartIndex, EndIndex: Integer): String;
    procedure DeleteRange(StartIndex, EndIndex: Integer);
    procedure Insert(const Value: String; Index: Integer);

    procedure Extend(Value: String);
    procedure Append(Value: Char);

    function Remove(Value: String): Boolean;
    function RemoveAll(Value: String): Integer;

    function PadLeft(ACount: Integer; PaddingChar: Char = #32): String;
    function PadRight(ACount: Integer; PaddingChar: Char = #32): String;
    function PadCenter(ACount: Integer; PaddingChar: Char = #32): String;

    function Format(Args: array of const): String;

    function ToBoolean: Boolean; overload;
    function ToBoolean(Default: Boolean): Boolean; overload;
    function ToInteger: Integer; overload;
    function ToInteger(Default: Integer): Integer; overload;
    function ToInt64: Int64; overload;
    function ToInt64(Default: Int64): Int64; overload;
    function ToSingle: Single; overload;
    function ToSingle(Default: Single): Single; overload;
    function ToDouble: Double; overload;
    function ToDouble(Default: Double): Double; overload;
    function ToExtended: Extended; overload;
    function ToExtended(Default: Extended): Extended; overload;

    function ParseJSON: TJSONData;
  end;

  operator * (const Left: String; Right: Int32): String;
  operator in(const Left: String; const Right: String): Boolean;
  operator in(const Left: String; const Right: TStringArray): Boolean;
{$ENDIF}

{$IFDEF BODY}
function TSimbaCharHelper.IsUpper(): Boolean;
begin
  Result := (Byte(Self) >= Byte('A')) and (Byte(Self) <= Byte('Z'));
end;

function TSimbaCharHelper.IsLower(): Boolean;
begin
  Result := (Byte(Self) >= Byte('a')) and (Byte(Self) <= Byte('z'));
end;

function TSimbaCharHelper.IsAlphaNum(): Boolean;
begin
  Result := Self in ['0'..'9', 'a'..'z', 'A'..'Z'];
end;

function TSimbaCharHelper.IsInteger(): Boolean;
begin
  Result := Self in ['0'..'9'];
end;

// same as is integer for chars, here for convinience
function TSimbaCharHelper.IsFloat(): Boolean;
begin
  Result := Self in ['0'..'9'];
end;

function TSimbaCharHelper.ToUpper(): Char;
begin
  Result := UpCase(Self);
end;

function TSimbaCharHelper.ToLower(): Char;
begin
  Result := LowerCase(Self);
end;

function TSimbaCharHelper.Capitalize(): Char;
begin
  Result := UpCase(Self);
end;

function TSimbaCharHelper.Join(const Values: TStringArray): String;
var i: Integer;
begin
  Result := '';
  for i:=0 to High(values) do
  begin
    Result += Values[i];
    if i <> High(values) then
      Result += Self;
  end;
end;


(* ---- String helpers ---- *)

function TSimbaStringHelper.Before(const Value: String): String;
var
  P: Integer;
begin
  P := Pos(Value, Self);
  if (P = 0) then
    Result := ''
  else
    Result := System.Copy(Self, 1, P - 1);
end;

function TSimbaStringHelper.After(const Value: String): String;
var
  P: Integer;
begin
  P := Pos(Value, Self);
  if (P = 0) then
    Result := ''
  else
    Result := System.Copy(Self, P + System.Length(Value), (System.Length(Self) - System.Length(Value)) + 1);
end;

function TSimbaStringHelper.Between(const S1, S2: String): String;
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

function TSimbaStringHelper.BetweenAll(const S1, S2: String): TStringArray;

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

function TSimbaStringHelper.RegExprSplit(const Pattern: String): TStringArray;
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

function TSimbaStringHelper.RegExprFind(const Pattern: String): TRegExprMatch;
var
  RegExpr: TRegExpr;

  function Add: TRegExprMatch;
  var
    Group: TRegExprGroup;
    I: Integer;
  begin
    Result.Match    := RegExpr.Match[0];
    Result.Length   := RegExpr.MatchLen[0];
    Result.Position := RegExpr.MatchPos[0];

    SetLength(Result.Groups, RegExpr.SubExprMatchCount);

    for I := 0 to RegExpr.SubExprMatchCount - 1 do
    begin
      Group.Match    := RegExpr.Match[I+1];
      Group.Length   := RegExpr.MatchLen[I+1];
      Group.Position := RegExpr.MatchPos[I+1];

      Result.Groups[I] := Group;
    end;
  end;

begin
  Result := Default(TRegExprMatch);

  RegExpr := TRegExpr.Create(Pattern);
  try
    if RegExpr.Exec(Self) then
      Result := Add();
  finally
    RegExpr.Free();
  end;
end;

function TSimbaStringHelper.RegExprExists(const Pattern: String): Boolean;
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

function TSimbaStringHelper.RegExprFindAll(const Pattern: String): TRegExprMatchArray;
var
  RegExpr: TRegExpr;

  function Add: TRegExprMatch;
  var
    Group: TRegExprGroup;
    I: Integer;
  begin
    Result.Match    := RegExpr.Match[0];
    Result.Length   := RegExpr.MatchLen[0];
    Result.Position := RegExpr.MatchPos[0];

    SetLength(Result.Groups, RegExpr.SubExprMatchCount);

    for I := 0 to RegExpr.SubExprMatchCount - 1 do
    begin
      Group.Match    := RegExpr.Match[I+1];
      Group.Length   := RegExpr.MatchLen[I+1];
      Group.Position := RegExpr.MatchPos[I+1];

      Result.Groups[I] := Group;
    end;
  end;

begin
  RegExpr := TRegExpr.Create(Pattern);
  try
    if RegExpr.Exec(Self) then
    repeat
      Result := Result + [Add()];
    until not RegExpr.ExecNext();
  finally
    RegExpr.Free();
  end;
end;

function TSimbaStringHelper.IndexOfAny(const Values: TStringArray): Integer;
begin
  Result := IndexOfAny(Values, 1);
end;

function TSimbaStringHelper.IndexOfAny(const Values: TStringArray; Offset: Integer): Integer;
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

function TSimbaStringHelper.IndexOf(const Value: String): Integer;
begin
  Result := Pos(Value, Self);
end;

function TSimbaStringHelper.IndexOf(const Value: String; Offset: Integer): Integer;
begin
  Result := Pos(Value, Self, Offset);
end;

function TSimbaStringHelper.LastIndexOf(const Value: String): Integer;
begin
  Result := RPos(Value, Self);
end;

function TSimbaStringHelper.LastIndexOf(const Value: String; Offset: Integer): Integer;
begin
  Result := RPosEx(Value, Self, Offset);
end;

function TSimbaStringHelper.IndicesOf(const Value: String): TIntegerArray;
var
  Buffer: TSimbaIntegerBuffer;
  Matches: SizeIntArray;
  I: Integer;
begin
  Result := Default(TIntegerArray);
  if (Self = '') then
    Exit;

  if Length(Value) = 1 then
  begin
    Buffer.Init(32);

    for I := 1 to Length(Self) do
      if (Self[I] = Value) then
        Buffer.Add(I);

    Result := Buffer.ToArray(False);
    Exit;
  end;

  if FindMatchesBoyerMooreCaseSensitive(Self, Value, Matches, True) then
  begin
    SetLength(Result, System.Length(Matches));
    for I := 0 to High(Matches) do
      Result[I] := Matches[I];
  end;
end;

function TSimbaStringHelper.IndicesOf(const Value: String; Offset: Integer): TIntegerArray;
var
  Buffer: TSimbaIntegerBuffer;
  Matches: SizeIntArray;
  I: Integer;
begin
  Result := Default(TIntegerArray);
  if (Self = '') then
    Exit;

  if Length(Value) = 1 then
  begin
    Buffer.Init(32);

    for I := Offset to Length(Self) do
      if (Self[I] = Value) then
        Buffer.Add(I);

    Result := Buffer.ToArray(False);
    Exit;
  end;

  if FindMatchesBoyerMooreCaseSensitive(Self.CopyRange(Offset, Length(Self)), Value, Matches, True) then
  begin
    SetLength(Result, System.Length(Matches));
    for I := 0 to High(Matches) do
      Result[I] := Matches[I];
  end;
end;

function TSimbaStringHelper.Equals(Other: String): Boolean;
begin
  Result := Self = Other;
end;

function TSimbaStringHelper.EqualsIgnoreCase(Other: String): Boolean;
begin
  Result := SameText(Self, Other);
end;

function TSimbaStringHelper.Compare(Other: String): Integer;
begin
  Result := CompareText(Self, Other);
end;

//MurMurHas2 by Tommi Prami & optimizations by Patrick van Logchem
function TSimbaStringHelper.Hash(Seed: UInt32 = 0): UInt32;
const
  cM: UInt32 = $5BD1E995;
  cR: UInt32 = 24;
var
  Data: Pointer;
  Len: Integer;
  i, k: UInt32;
begin
  Data := PChar(Self);
  Len := Length(Self);

  {$UNDEF REDO_Q}{$IFOPT Q+}{$Q-}{$DEFINE REDO_Q}{$ENDIF}
  {$UNDEF REDO_R}{$IFOPT R+}{$R-}{$DEFINE REDO_R}{$ENDIF}
  Result := Seed xor Len;
  for i := 1 to Len div SizeOf(UInt32) do
  begin
    k := PUInt32(Data)^ * cM;
    Result := (Result * cM) xor ((k xor (k shr cR)) * cM);
    Inc(PtrUInt(Data), SizeOf(UInt32));
  end;

  Len := Len and 3;
  if (Len > 0) then
  begin
    if (Len >= SizeOf(UInt16)) then
    begin
      k := PUInt16(Data)^;
      if (Len > SizeOf(UInt16)) then
        k := k + (UInt32(PUInt8(PtrUInt(Data) + SizeOf(UInt16))^) shl 16);
    end
    else
      k := PUInt8(Data)^;
    Result := (Result xor k) * cM;
  end;

  Result := (Result xor (Result shr 13)) * cM;
  Result := (Result xor (Result shr 15));
  {$IFDEF REDO_Q}{$Q+}{$ENDIF}
  {$IFDEF REDO_R}{$R+}{$ENDIF}
end;

function TSimbaStringHelper.IsUpper(): Boolean;
var i: Int32;
begin
  for i:=1 to Length(self) do
     if not self[i].IsUpper() then
       Exit(False);
  Result := True;
end;

function TSimbaStringHelper.IsLower(): Boolean;
var i: Int32;
begin
  for i:=1 to Length(self) do
     if not self[i].IsLower() then
       Exit(False);
  Result := True;
end;

function TSimbaStringHelper.ToUpper(): String;
begin
  Result := UpperCase(Self);
end;

function TSimbaStringHelper.ToLower(): String;
begin
  Result := LowerCase(Self);
end;

function TSimbaStringHelper.Capitalize(): String;
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

function TSimbaStringHelper.SwapCase(): String;
var
  i: Integer;
begin
  SetLength(Result, Length(Self));

  for i:=1 to Length(self) do
    if Self[i].IsUpper() then
      Result[i] := Self[i].ToLower()
    else if Self[i].IsLower() then
      Result[i] := Self[i].ToUpper()
    else
      Result[i] := Self[i];
end;

function TSimbaStringHelper.Extract(const Chars: array of Char): String;
type
  TCharMap = array[Char] of Boolean;
var
  Hit: TCharMap;
  I, Hits: Integer;
begin
  Result := '';

  if (Length(Self) > 0) and (Length(Chars) > 0) then
  begin
    SetLength(Result, Length(Self));
    Hits := 1;

    Hit := Default(TCharMap);
    for I := 0 to High(Chars) Do
      Hit[Chars[I]] := True;

    for I := 1 to Length(Self) do
      if Hit[Self[I]] then
      begin
        Result[Hits] := Self[I];
        Inc(Hits);
      end;

    SetLength(Result, Hits - 1);
  end;
end;

function TSimbaStringHelper.ExtractInteger(Default: Int64): Int64;
begin
  Result := StrToInt64Def(Self.Extract(['-','0','1','2','3','4','5','6','7','8','9']), Default);
end;

function TSimbaStringHelper.ExtractFloat(Default: Extended): Extended;
begin
  Result := StrToFloatDef(Self.Extract(['.','-','0','1','2','3','4','5','6','7','8','9']), Default);
end;

function TSimbaStringHelper.IsAlphaNum: Boolean;
var
  I: Integer;
begin
  if (Self = '') then
    Exit(False);

  Result := True;

  for I := 1 to Length(Self) do
    if not (Self[I] in ['0'..'9','a'..'z','A'..'Z']) then
    begin
      Result := False;
      Exit;
    end;
end;

function TSimbaStringHelper.IsInteger: Boolean;
var
  I: Integer;
begin
  if (Self = '') then
    Exit(False);

  for I := 1 to Length(Self) do
    if (not (Self[I] in ['0'..'9'])) then
    begin
      if (I = 1) and (Self[I] = '-') then
        Continue;

      Result := False;
      Exit;
    end;

  Result := True;
end;

function TSimbaStringHelper.IsFloat: Boolean;
var
  I: Integer;
begin
  if (Self = '') then
    Exit(False);

  for I := 1 to Length(Self) do
    if (not (Self[I] in ['0'..'9', '.'])) then
    begin
      if (I = 1) and (Self[I] = '-') then
        Continue;

      Result := False;
      Exit;
    end;

  Result := True;
end;

function TSimbaStringHelper.Trim: String;
begin
  Result := SysUtils.Trim(Self);
end;

function TSimbaStringHelper.Trim(const TrimChars: array of Char): String;
begin
  Result := Self.TrimLeft(TrimChars).TrimRight(TrimChars);
end;

function TSimbaStringHelper.TrimLeft: String;
begin
  Result := SysUtils.TrimLeft(Self);
end;

function TSimbaStringHelper.TrimLeft(const TrimChars: array of Char): String;

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

function TSimbaStringHelper.TrimRight: String;
begin
  Result := SysUtils.TrimRight(Self);
end;

function TSimbaStringHelper.TrimRight(const TrimChars: array of Char): String;

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

function TSimbaStringHelper.StartsWith(const Value: String; CaseSenstive: Boolean): Boolean;
begin
  case CaseSenstive of
    False: Result := SameText(System.Copy(Self, 1, Length(Value)), Value);
    True:  Result := System.Copy(Self, 1, Length(Value)) = Value;
  end;
end;

function TSimbaStringHelper.EndsWith(const Value: String; CaseSenstive: Boolean): Boolean;
begin
  case CaseSenstive of
    False: Result := SameText(System.Copy(Self, Length(Self) - Length(Value) + 1), Value);
    True:  Result := System.Copy(Self, Length(Self) - Length(Value) + 1) = Value;
  end;
end;

function TSimbaStringHelper.Partition(const Value: String; CaseSenstive: Boolean): TStringArray;
var
  I: Integer;
begin
  Result := ['', '', ''];

  if CaseSenstive then
    I := Self.IndexOf(Value)
  else
    I := Self.ToUpper().IndexOf(Value.ToUpper());

  if (I > 0) then
  begin
    Result[0] := System.Copy(Self, 1, I-1);
    Result[1] := System.Copy(Self, I, Length(Value));
    Result[2] := System.Copy(Self, I+Length(Value));
  end;
end;

function TSimbaStringHelper.Replace(const OldValue: String; const NewValue: String): String;
begin
  Result := StringReplace(Self, OldValue, NewValue, [rfReplaceAll]);
end;

function TSimbaStringHelper.Replace(const OldValue: String; const NewValue: String; ReplaceFlags: TReplaceFlags): String;
begin
  Result := StringReplace(Self, OldValue, NewValue, ReplaceFlags);
end;

function TSimbaStringHelper.Contains(const Value: String; CaseSenstive: Boolean): Boolean;
begin
  Result := ContainsAny([Value], CaseSenstive);
end;

function TSimbaStringHelper.ContainsAny(const Values: TStringArray; CaseSenstive: Boolean): Boolean;
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

function TSimbaStringHelper.Count(const Value: String): Integer;
begin
  Result := Length(Self.IndicesOf(Value));
end;

function TSimbaStringHelper.CountAll(const Values: TStringArray): TIntegerArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Values));
  for I := 0 to High(Values) do
    Result[I] := Length(Self.IndicesOf(Values[I]));
end;

function TSimbaStringHelper.Join(const Values: TStringArray): String;
var i: Integer;
begin
  Result := '';
  for i:=0 to High(values) do
  begin
    Result += Values[i];
    if i <> High(values) then
      Result += Self;
  end;
end;

function TSimbaStringHelper.Split(const Seperators: TStringArray): TStringArray;
var
  Len: SizeInt;

  function NextSep(const StartIndex: SizeInt; out SepIndex: SizeInt): SizeInt;
  var
    I: Integer;
  begin
    Result := 0;

    for I := 0 to High(Seperators) do
    begin
      Result := Self.IndexOf(Seperators[I], StartIndex);
      if (Result > 0) then
      begin
        SepIndex := I;
        Exit;
      end;
    end;
  end;

  procedure Add(const S: String);
  begin
    if (Len >= Length(Result)) then
      SetLength(Result, Length(Result) * 2);
    Result[Len] := S;
    Inc(Len);
  end;

var
  Sep, SepIndex, LastSep: SizeInt;
begin
  SetLength(Result, 16);

  Len := 0;
  LastSep := 1;
  Sep := NextSep(1, SepIndex);
  while (Sep > 0) do
  begin
    Add(System.Copy(Self, LastSep, Sep - LastSep));
    LastSep := Sep + Length(Seperators[SepIndex]);
    Sep := NextSep(LastSep, SepIndex);
  end;

  if (LastSep <= Length(Self)) then
    Add(System.Copy(Self, LastSep, MaxInt));

  if (Len > 0) and (Result[Len - 1] = '') then
    Dec(Len);

  SetLength(Result, Len);
end;

function TSimbaStringHelper.Split(const Seperator: String): TStringArray;
begin
  Result := Split([Seperator]);
end;

function TSimbaStringHelper.Copy: String;
begin
  Result := System.Copy(Self, 1, Length(Self));
end;

function TSimbaStringHelper.CopyRange(StartIndex, EndIndex: Integer): String;
begin
  Result := System.Copy(Self, StartIndex, (EndIndex - StartIndex) + 1);
end;

procedure TSimbaStringHelper.DeleteRange(StartIndex, EndIndex: Integer);
begin
  System.Delete(Self, StartIndex, (EndIndex - StartIndex) + 1);
end;

procedure TSimbaStringHelper.Insert(const Value: String; Index: Integer);
begin
  System.Insert(Value, Self, Index);
end;

procedure TSimbaStringHelper.Extend(Value: String);
begin
  Self := Self + Value;
end;

procedure TSimbaStringHelper.Append(Value: Char);
begin
  Self := Self + Value;
end;

function TSimbaStringHelper.Remove(Value: String): Boolean;
var
  ReplaceCount: Integer;
begin
  Self := StringReplace(Self, Value, '', [], ReplaceCount);

  Result := ReplaceCount > 0;
end;

function TSimbaStringHelper.RemoveAll(Value: String): Integer;
begin
  Self := StringReplace(Self, Value, '', [rfReplaceAll], Result);
end;

function TSimbaStringHelper.PadLeft(ACount: Integer; PaddingChar: Char): String;
begin
  ACount := ACount - Length(Self);
  if (ACount > 0) then
    Result := StringOfChar(PaddingChar, ACount) + Self
  else
    Result := Self;
end;

function TSimbaStringHelper.PadRight(ACount: Integer; PaddingChar: Char): String;
begin
  ACount := ACount - Length(Self);
  if (ACount > 0) then
    Result := Self + StringOfChar(PaddingChar, ACount)
  else
    Result := Self;
end;

function TSimbaStringHelper.PadCenter(ACount: Integer; PaddingChar: Char): String;
begin
  if (Length(Self) < ACount) then
  begin
    Result := StringOfChar(' ', (ACount div 2) - (Length(Self) div 2)) + Self;
    Result := Result + StringOfChar(' ', ACount - Length(Result));
  end else
    Result := Self;
end;

function TSimbaStringHelper.Format(Args: array of const): String;
begin
  Result := SysUtils.Format(Self, Args);
end;

function TSimbaStringHelper.ToBoolean: Boolean;
begin
  Result := StrToBool(Self);
end;

function TSimbaStringHelper.ToBoolean(Default: Boolean): Boolean;
begin
  Result := StrToBoolDef(Self, Default);
end;

function TSimbaStringHelper.ToInteger: Integer;
begin
  Result := StrToInt(Self);
end;

function TSimbaStringHelper.ToInteger(Default: Integer): Integer;
begin
  Result := StrToIntDef(Self, Default);
end;

function TSimbaStringHelper.ToInt64: Int64;
begin
  Result := StrToInt64(Self);
end;

function TSimbaStringHelper.ToInt64(Default: Int64): Int64;
begin
  Result := StrToInt64Def(Self, Default);
end;

function TSimbaStringHelper.ToSingle: Single;
begin
  Result := StrToFloat(Self);
end;

function TSimbaStringHelper.ToSingle(Default: Single): Single;
begin
  Result := StrToFloatDef(Self, Default);
end;

function TSimbaStringHelper.ToDouble: Double;
begin
  Result := StrToFloat(Self);
end;

function TSimbaStringHelper.ToDouble(Default: Double): Double;
begin
  Result := StrToFloatDef(Self, Default);
end;

function TSimbaStringHelper.ToExtended: Extended;
begin
  Result := StrToFloat(Self);
end;

function TSimbaStringHelper.ToExtended(Default: Extended): Extended;
begin
  Result := StrToFloatDef(Self, Default);
end;

function TSimbaStringHelper.ParseJSON: TJSONData;
var
  Parser: TJSONParser;
begin
  Result := nil;

  if (Self <> '') then
  begin
    Parser := TJSONParser.Create(Self, [joUTF8, joComments, joIgnoreTrailingComma]);
    try
      Result := Parser.Parse();
    except
    end;
    Parser.Free();
  end;
end;

operator *(const Left: String; Right: Int32): String;
var
  I, Len: Integer;
begin
  Result := Left;

  Len := Length(Left);
  if (Len > 0) then
  begin
    SetLength(Result, Len * Right);
    for I := 1 to Right - 1 do
      Move(Left[1], Result[1+Len*I], Len);
  end;
end;

operator in(const Left: String; const Right: String): Boolean;
begin
  Result := Pos(Left, Right) > 0;
end;

operator in(const Left: String; const Right: TStringArray): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(Right) do
    if Pos(Left, Right[I]) > 0 then
    begin
      Result := True;
      Exit;
    end;

  Result := False;
end;

operator *(const Left: Char; Right: Int32): String;
var
  i: Integer;
begin
  for i:=0 to Right-1 do
    Result += Left;
end;

operator in(const Left: Char; const Right: String): Boolean;
begin
  Result := Pos(Left, Right) > 0;
end;

operator in(const Left: Char; const Right: TStringArray): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(Right) do
    if Pos(Left, Right[I]) > 0 then
    begin
      Result := True;
      Exit;
    end;

  Result := False;
end;
{$ENDIF}

