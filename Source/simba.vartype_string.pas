{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}

{
 avk959 - https://github.com/avk959/LGenerics

  - SimRatio (used in String.Similarity)
}

unit simba.vartype_string;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  fpjson, jsonparser, jsonscanner,
  simba.base;

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
    function Similarity(Other: String): Double;
    function Hash(Seed: UInt32 = 0): UInt32;

    function IsUpper(): Boolean;
    function IsLower(): Boolean;
    function IsAlphaNum(): Boolean;
    function IsInteger(): Boolean;
    function IsFloat(): Boolean;

    function ToUpper(): String;
    function ToLower(): String;
    function Capitalize(): String;
    function CapitalizeWords(): String;
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
    function Split(const Seperator: String; ExcludeEmpty: Boolean = True): TStringArray;
    function SplitLines: TStringArray;

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

    function ToBytes: TByteArray;
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

    function ToDateTime(Fmt: String; Def: TDateTime): TDateTime;

    function ParseJSON: TJSONData;
  end;

  operator * (const Left: String; Right: Int32): String;
  operator in(const Left: String; const Right: String): Boolean;
  operator in(const Left: String; const Right: TStringArray): Boolean;

implementation

uses
  RegExpr, StrUtils, DateUtils,
  simba.containers, simba.hash;

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
begin
  Result := String(Self).Join(Values);
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
  Result := [];

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

function TSimbaStringHelper.Similarity(Other: String): Double;

  function LevDistanceImpl(pL, pR: PByte; aLenL, aLenR: SizeInt): SizeInt;

    function SkipPrefix(var pL, pR: PByte; var aLenL, aLenR: SizeInt): SizeInt; inline;
    begin
      //implied aLenL <= aLenR
      Result := 0;
      while (Result < aLenL) and (pL[Result] = pR[Result]) do
        Inc(Result);

      pL += Result;
      pR += Result;
      aLenL -= Result;
      aLenR -= Result;
    end;

    function SkipSuffix(pL, pR: PByte; var aLenL, aLenR: SizeInt): SizeInt; inline;
    begin
      //implied aLenL <= aLenR
      Result := 0;
      while (aLenL > 0) and (pL[Pred(aLenL)] = pR[Pred(aLenR)]) do
      begin
        Dec(aLenL);
        Dec(aLenR);
        Inc(Result);
      end;
    end;

  const
    MAX_STATIC = 512;
  var
    StBuf: array[0..Pred(MAX_STATIC)] of SizeInt;
    Buf: array of SizeInt = nil;
    I, J, Prev, Next: SizeInt;
    Dist: PSizeInt;
    b: Byte;
  begin
    //here aLenL <= aLenR
    if pL = pR then
      Exit(aLenR - aLenL);

    SkipSuffix(pL, pR, aLenL, aLenR);
    SkipPrefix(pL, pR, aLenL, aLenR);

    if aLenL = 0 then
      Exit(aLenR);

    if aLenR < MAX_STATIC then
      Dist := @StBuf[0]
    else begin
      System.SetLength(Buf, Succ(aLenR));
      Dist := Pointer(Buf);
    end;
    for I := 0 to aLenR do
      Dist[I] := I;

    for I := 1 to aLenL do
    begin
      Prev := I;
      b := pL[I-1];
      for J := 1 to aLenR do
      begin
        if pR[J-1] = b then
          Next := Dist[J-1]
        else
          Next := Succ(Min(Min(Dist[J-1], Prev), Dist[J]));
        Dist[J-1] := Prev;
        Prev := Next;
      end;
      Dist[aLenR] := Prev;
    end;

    Result := Dist[aLenR];
  end;

  function LevDistance(const L, R: ansistring): SizeInt;
  begin
    if System.Length(L) = 0 then
      Exit(System.Length(R))
    else if System.Length(R) = 0 then
      Exit(System.Length(L));

    if System.Length(L) <= System.Length(R) then
      Result := LevDistanceImpl(Pointer(L), Pointer(R), System.Length(L), System.Length(R))
    else
      Result := LevDistanceImpl(Pointer(R), Pointer(L), System.Length(R), System.Length(L));
  end;

var
  MaxLen: SizeInt;
begin
  if (Self = '') and (Other = '') then
    Exit(Double(1.0));

  MaxLen := Max(System.Length(Self), System.Length(Other));
  Result := Double(MaxLen - LevDistance(Self, Other)) / Double(MaxLen);
end;

function TSimbaStringHelper.Hash(Seed: UInt32 = 0): UInt32;
begin
  Result := Hash32(Self, Seed);
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

function TSimbaStringHelper.CapitalizeWords(): String;
var
  Temp: TStringArray;
  I: Integer;
begin
  Temp := Self.Split(' ', False);
  for I := 0 to High(Temp) do
    Temp[I] := Temp[I].Capitalize();
  Result := ' '.Join(Temp);
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

  function IsTrimChar(const C: Char): Boolean; inline;
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
    False: Result := (Length(Value) > 0) and SameText(System.Copy(Self, 1, Length(Value)), Value);
    True:  Result := (Length(Value) > 0) and (System.Copy(Self, 1, Length(Value)) = Value);
  end;
end;

function TSimbaStringHelper.EndsWith(const Value: String; CaseSenstive: Boolean): Boolean;
begin
  case CaseSenstive of
    False: Result := (Length(Value) > 0) and SameText(System.Copy(Self, Length(Self) - Length(Value) + 1), Value);
    True:  Result := (Length(Value) > 0) and (System.Copy(Self, Length(Self) - Length(Value) + 1) = Value);
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
var
  Builder: TSimbaStringBuilder;
  I, Current, Total: Integer;
begin
  if (Length(Values) = 0) then Exit('');
  if (Length(Values) = 1) then Exit(Values[0]);

  Total := High(Values) * Length(Self);
  for I := 0 to High(Values) do
    Total += Length(Values[I]);
  SetLength(Result, Total);

  Current := 1;
  for I := 0 to High(Values) do
  begin
    if (Values[I] <> '') then
    begin
      Move(Values[I][1], Result[Current], Length(Values[I]));
      Inc(Current, Length(Values[I]));
    end;

    if (I < High(Values)) then
    begin
      Move(Self[1], Result[Current], Length(Self));
      Inc(Current, Length(Self));
    end;
  end;
end;

function TSimbaStringHelper.Split(const Seperator: String; ExcludeEmpty: Boolean): TStringArray;

  function DoSplit(const Str, Seperator: String): TStringArray;
  var
    StrLen, SepLen, MaxLen, ResultCount: SizeInt;

    procedure Add(const Index, Count: Integer);
    begin
      if ExcludeEmpty and (Count = 0) then
        Exit;

      if (ResultCount >= Length(Result)) then
        SetLength(Result, 4 + (Length(Result) * 2));
      Result[ResultCount] := System.Copy(Str, Index, Count);
      Inc(ResultCount);
    end;

    function NextSep(const StartIndex: SizeInt): SizeInt;
    var
      I: SizeInt;
      Ptr: PChar;
    begin
      if (StartIndex <= StrLen) then
      begin
        Ptr := @Str[StartIndex];
        I := StartIndex - 1;
        while (I <= MaxLen) do
        begin
          Inc(I);
          if (PChar(Seperator)^ = Ptr^) and (CompareByte(PChar(Seperator)^, Ptr^, SepLen) = 0) then
            Exit(I);
          Inc(Ptr);
        end;
      end;

      Result := -1;
    end;

  var
    FoundSep, LastSep: SizeInt;
  begin
    if (Length(Seperator) = 0) then
      Result := [Str]
    else
    begin
      Result := [];
      ResultCount := 0;

      StrLen := Length(Str);
      SepLen := Length(Seperator);
      MaxLen := StrLen - SepLen;

      LastSep := 1;
      FoundSep := NextSep(1);
      while (FoundSep > 0) do
      begin
        Add(LastSep, FoundSep - LastSep);
        LastSep := FoundSep + SepLen;
        FoundSep := NextSep(LastSep);
      end;
      if (LastSep <= StrLen) then
        Add(LastSep, MaxInt);

      SetLength(Result, ResultCount);
    end;
  end;

begin
  Result := DoSplit(Self, Seperator);
end;

function TSimbaStringHelper.SplitLines: TStringArray;

  function DoSplit(const Str: String): TStringArray;
  var
    StrLen, MaxLen, ResultCount: SizeInt;

    procedure Add(const Index, Count: Integer);
    begin
      if (ResultCount >= Length(Result)) then
        SetLength(Result, 4 + (Length(Result) * 2));
      Result[ResultCount] := System.Copy(Str, Index, Count);
      Inc(ResultCount);
    end;

    function NextSep(const StartIndex: SizeInt; out HasReturn: Boolean): SizeInt;
    var
      I: SizeInt;
      Ptr: PChar;
    begin
      if (StartIndex <= StrLen) then
      begin
        HasReturn := False;
        Ptr := @Str[StartIndex];
        I := StartIndex - 1;
        while (I <= MaxLen) do
        begin
          Inc(I);
          if (Ptr^ = #10) then
            Exit(I);
          HasReturn := Ptr^ = #13;
          Inc(Ptr);
        end;
      end;

      Result := -1;
    end;

  var
    FoundSep, LastSep: SizeInt;
    HasReturn: Boolean;
  begin
    Result := [];
    ResultCount := 0;

    StrLen := Length(Str);
    MaxLen := StrLen - 1;

    LastSep := 1;
    FoundSep := NextSep(1, HasReturn);
    while (FoundSep > 0) do
    begin
      if HasReturn then
      begin
        Dec(FoundSep);
        Add(LastSep, FoundSep - LastSep);
        LastSep := FoundSep + 2;
      end else
      begin
        Add(LastSep, FoundSep - LastSep);
        LastSep := FoundSep + 1;
      end;
      FoundSep := NextSep(LastSep, HasReturn);
    end;
    if (LastSep <= StrLen) then
      Add(LastSep, MaxInt);

    SetLength(Result, ResultCount);
  end;

begin
  Result := DoSplit(Self);
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

function TSimbaStringHelper.ToBytes: TByteArray;
var
  Len: Integer;
begin
  Len := Length(Self);
  SetLength(Result, Len);
  if (Len > 0) then
    Move(Self[1], Result[0], Len);
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

function TSimbaStringHelper.ToDateTime(Fmt: String; Def: TDateTime): TDateTime;
begin
  Result := Def;

  case Fmt.ToLower() of
    '':
      TryStrToDateTime(Self, Result);
    'iso':
      TryISOStrToDateTime(Self, Result);
    'iso8601':
      TryISO8601ToDate(Self, Result);
    'unix':
      if Self.IsInteger() then
        Result := UnixToDateTime(Self.ToInt64());
    else
      SimbaException('String.ToDateTime: Fmt "%s" not recognized', [Fmt]);
  end;
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
  I: Integer;
begin
  Result := '';
  for I:=0 to Right-1 do
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

end.

