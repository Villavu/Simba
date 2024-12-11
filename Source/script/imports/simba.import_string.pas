unit simba.import_string;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lpvartypes,
  simba.base, simba.script, simba.vartype_string;

procedure ImportString(Script: TSimbaScript);

implementation

uses
  lpmessages;

(*
String
======
String methods
*)

(*
String.SetLength
----------------
```
procedure String.SetLength(NewLength: Integer);
```
*)
procedure _LapeString_SetLength(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SetLength(PString(Params^[0])^, PInteger(Params^[1])^);
end;

(*
String.Length
-------------
```
property String.Length: Integer;
```
*)
procedure _LapeString_Length(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := Length(PString(Params^[0])^);
end;

(*
String.High
-----------
```
property String.Low: Integer;
```
*)
procedure _LapeString_Low(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := Low(PString(Params^[0])^);
end;

(*
String.High
-----------
```
property String.High: Integer;
```
*)
procedure _LapeString_High(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := High(PString(Params^[0])^);
end;

(*
String.Pop
----------
```
property String.Pop: Char;
```
*)
procedure _LapeString_Pop(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV

  function _Pop(var Str: lpString): Char;
  begin
    if Length(Str) < 1 then
      LapeExceptionFmt(lpeIndexOutOfRange, [Length(Str), Low(Str), Length(Str)]);
    Result := Str[Length(Str)];
    SetLength(Str, Length(Str) - 1);
  end;

begin
  PChar(Result)^ := _Pop(PString(Params^[0])^);
end;

(*
String.First
------------
```
property String.First: Char;
```
*)
procedure _LapeString_First(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV

  function _First(const Str: lpString): Char;
  begin
    if Length(Str) < 1 then
      LapeExceptionFmt(lpeIndexOutOfRange, [Low(Str), Low(Str), Length(Str)]);
    Result := Str[Low(Str)];
  end;

begin
  PChar(Result)^ := _First(PString(Params^[0])^);
end;

(*
String.Last
-----------
```
property String.Last: Char;
```
*)
procedure _LapeString_Last(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV

  function _Last(const Str: lpString): Char;
  begin
    if Length(Str) < 1 then
      LapeExceptionFmt(lpeIndexOutOfRange, [High(Str), Low(Str), Length(Str)]);
    Result := Str[High(Str)];
  end;

begin
  PChar(Result)^ := _Last(PString(Params^[0])^);
end;

(*
String.Before
-------------
```
function String.Before(Value: String): String;
```
*)
procedure _LapeString_Before(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PString(Params^[0])^.Before(PString(Params^[1])^);
end;

(*
String.After
------------
```
function String.After(Value: String): String;
```
*)
procedure _LapeString_After(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PString(Params^[0])^.After(PString(Params^[1])^);
end;

(*
String.StartsWith
-----------------
```
function String.StartsWith(Value: String; CaseSensitive: Boolean = True): Boolean;
```
*)
procedure _LapeString_StartsWith(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PString(Params^[0])^.StartsWith(PString(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
String.Equals
-------------
```
function String.EqualsIgnoreCase(Other: String; CaseSensitive: Boolean = True): Boolean;
```
*)
procedure _LapeString_Equals(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PString(Params^[0])^.Equals(PString(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
String.Compare
--------------
```
function String.Compare(Other: String): Integer;
```
*)
procedure _LapeString_Compare(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PString(Params^[0])^.Compare(PString(Params^[1])^);
end;

(*
String.Similarity
-----------------
```
function String.Similarity(Other: String): Double;
```
*)
procedure _LapeString_Similarity(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := PString(Params^[0])^.Similarity(PString(Params^[1])^);
end;

(*
String.Hash
-----------
```
function String.Hash(Seed: UInt32 = 0): UInt32;
```
*)
procedure _LapeString_Hash(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PUInt32(Result)^ := PString(Params^[0])^.Hash(PUInt32(Params^[1])^);
end;

(*
String.EndsWith
---------------
```
function String.EndsWith(Value: String; CaseSensitive: Boolean = True): Boolean;
```
*)
procedure _LapeString_EndsWith(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PString(Params^[0])^.EndsWith(PString(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
String.IsUpper
--------------
```
property String.IsUpper(): Boolean;
```
*)
procedure _LapeString_IsUpper(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PString(Params^[0])^.IsUpper;
end;

(*
String.IsLower
--------------
```
property String.IsLower: Boolean;
```
*)
procedure _LapeString_IsLower(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PString(Params^[0])^.IsLower;
end;

(*
String.ToUpper
--------------
```
function String.ToUpper: String;
```
*)
procedure _LapeString_ToUpper(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PString(Params^[0])^.ToUpper();
end;

(*
String.ToLower
--------------
```
function String.ToLower: String;
```
*)
procedure _LapeString_ToLower(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PString(Params^[0])^.ToLower();
end;

(*
String.Capitalize
-----------------
```
function String.Capitalize(): String;
```
*)
procedure _LapeString_Capitalize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PString(Params^[0])^.Capitalize();
end;

(*
String.CapitalizeWords
----------------------
```
function String.CapitalizeWords: String;
```
*)
procedure _LapeString_CapitalizeWords(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PString(Params^[0])^.CapitalizeWords();
end;

(*
String.SwapCase
---------------
```
function String.SwapCase(): String;
```
*)
procedure _LapeString_SwapCase(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PString(Params^[0])^.SwapCase();
end;

(*
String.Join
-----------
```
function String.Join(Values: TStringArray): String;
```
*)
procedure _LapeString_Join(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PString(Params^[0])^.Join(PStringArray(Params^[1])^);
end;

(*
String.Split
------------
```
function String.Split(Seperator: String; ExcludeEmpty: Boolean = True): TStringArray;
```
*)
procedure _LapeString_Split(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := PString(Params^[0])^.Split(PString(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
String.SplitLines
-----------------
```
function String.SplitLines: TStringArray;
```
*)
procedure _LapeString_SplitLines(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := PString(Params^[0])^.SplitLines();
end;

(*
String.PadLeft
--------------
```
function String.PadLeft(Count: Integer; PaddingChar: Char = #32): String;
```
*)
procedure _LapeString_PadLeft(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PString(Params^[0])^.PadLeft(PInteger(Params^[1])^, PChar(Params^[2])^);
end;

(*
String.PadRight
---------------
```
function String.PadRight(Count: Integer; PaddingChar: Char = #32): String;
```
*)
procedure _LapeString_PadRight(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PString(Params^[0])^.PadRight(PInteger(Params^[1])^, PChar(Params^[2])^);
end;

(*
String.PadCenter
----------------
```
function String.PadCenter(Count: Integer; PaddingChar: Char = #32): String;
```
*)
procedure _LapeString_PadCenter(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PString(Params^[0])^.PadCenter(PInteger(Params^[1])^, PChar(Params^[2])^);
end;

(*
String.Partition
----------------
```
function String.Partition(Value: String): TStringArray;
```
*)
procedure _LapeString_Partition(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := PString(Params^[0])^.Partition(PString(Params^[1])^);
end;

(*
String.Replace
--------------
```
function String.Replace(OldValue: String; NewValue: String; CaseSensitive: Boolean = True): String;
```
*)
procedure _LapeString_Replace(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PString(Params^[0])^.Replace(PString(Params^[1])^, PString(Params^[2])^, PBoolean(Params^[3])^);
end;

(*
String.Trim
-----------
```
function String.Trim: String;
```
*)
procedure _LapeString_Trim(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PString(Params^[0])^.Trim();
end;

(*
String.TrimLeft
---------------
```
function String.TrimLeft: String;
```
*)
procedure _LapeString_TrimLeft(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PString(Params^[0])^.TrimLeft();
end;

(*
String.TrimRight
----------------
```
function String.TrimRight: String;
```
*)
procedure _LapeString_TrimRight(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PString(Params^[0])^.TrimRight();
end;

(*
String.Trim
-----------
```
function String.Trim(TrimChars: array of Char): String;
```
*)
procedure _LapeString_TrimEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
type
  PCharArray = ^TCharArray;
begin
  PString(Result)^ := PString(Params^[0])^.Trim(PCharArray(Params^[1])^);
end;

(*
String.TrimLeft
---------------
```
function String.TrimLeft(TrimChars: array of Char): String;
```
*)
procedure _LapeString_TrimLeftEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
type
  PCharArray = ^TCharArray;
begin
  PString(Result)^ := PString(Params^[0])^.TrimLeft(PCharArray(Params^[1])^);
end;

(*
String.TrimRight
----------------
```
function String.TrimRight(TrimChars: array of Char): String;
```
*)
procedure _LapeString_TrimRightEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
type
  PCharArray = ^TCharArray;
begin
  PString(Result)^ := PString(Params^[0])^.TrimRight(PCharArray(Params^[1])^);
end;

(*
String.RegExprSplit
-------------------
```
function String.RegExprSplit(Pattern: String): TStringArray;
```
*)
procedure _LapeString_RegExprSplit(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := PString(Params^[0])^.RegExprSplit(PString(Params^[1])^);
end;

(*
String.RegExprFind
------------------
```
function String.RegExprFind(Pattern: String): TRegExprMatch;
```
*)
procedure _LapeString_RegExprFind(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PRegExprMatch(Result)^ := PString(Params^[0])^.RegExprFind(PString(Params^[1])^);
end;

(*
String.RegExprFindAll
---------------------
```
function String.RegExprFindAll(Pattern: String): TRegExprMatchArray;
```
*)
procedure _LapeString_RegExprFindAll(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PRegExprMatchArray(Result)^ := PString(Params^[0])^.RegExprFindAll(PString(Params^[1])^);
end;

(*
String.RegExprExists
--------------------
```
function String.RegExprExists(Pattern: String): Boolean;
```
*)
procedure _LapeString_RegExprExists(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PString(Params^[0])^.RegExprExists(PString(Params^[1])^);
end;

(*
String.IndexOf
--------------
```
function String.IndexOf(Value: String): Integer;
```
*)
procedure _LapeString_IndexOf(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PString(Params^[0])^.IndexOf(PString(Params^[1])^);
end;

(*
String.IndexOf
--------------
```
function String.IndexOf(Value: String; Offset: Integer): Integer;
```
*)
procedure _LapeString_IndexOfEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PString(Params^[0])^.IndexOf(PString(Params^[1])^, PInteger(Params^[2])^);
end;

(*
String.LastIndexOf
------------------
```
function String.LastIndexOf(Value: String): Integer;
```
*)
procedure _LapeString_LastIndexOf(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PString(Params^[0])^.LastIndexOf(PString(Params^[1])^);
end;

(*
String.LastIndexOf
------------------
```
function String.LastIndexOf(Value: String; Offset: Integer): Integer;
```
*)
procedure _LapeString_LastIndexOfEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PString(Params^[0])^.LastIndexOf(PString(Params^[1])^, PInteger(Params^[2])^);
end;

(*
String.IndicesOf
----------------
```
function String.IndicesOf(Value: String): TIntegerArray;
```
*)
procedure _LapeString_IndicesOf(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerArray(Result)^ := PString(Params^[0])^.IndicesOf(PString(Params^[1])^);
end;

(*
String.IndicesOf
----------------
```
function String.IndicesOf(Value: String; Offset: Integer): TIntegerArray;
```
*)
procedure _LapeString_IndicesOfEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerArray(Result)^ := PString(Params^[0])^.IndicesOf(PString(Params^[1])^, PInteger(Params^[2])^);
end;

(*
String.Between
--------------
```
function String.Between(S1, S2: String): String;
```
*)
procedure _LapeString_Between(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PString(Params^[0])^.Between(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
String.BetweenAll
-----------------
```
function String.BetweenAll(S1, S2: String): TStringArray;
```
*)
procedure _LapeString_BetweenAll(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := PString(Params^[0])^.BetweenAll(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
String.Extract
--------------
```
function String.Extract(Chars: array of Char): String;
```
*)
procedure _LapeString_Extract(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
type
  PCharArray = ^TCharArray;
begin
  PString(Result)^ := PString(Params^[0])^.Extract(PCharArray(Params^[1])^);
end;

(*
String.ExtractInteger
---------------------
```
function String.ExtractInteger(Default: Int64 = -1): Int64;
```
*)
procedure _LapeString_ExtractInteger(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := PString(Params^[0])^.ExtractInteger(PInt64(Params^[1])^);
end;

(*
String.ExtractFloat
-------------------
```
function String.ExtractFloat(Default: Double = -1): Double;
```
*)
procedure _LapeString_ExtractFloat(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := PString(Params^[0])^.ExtractFloat(PInt64(Params^[1])^);
end;

(*
String.IsAlpha
--------------
```
property String.IsAlpha: Boolean;
```
*)
procedure _LapeString_IsAlpha(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PString(Params^[0])^.IsAlpha;
end;

(*
String.IsAlphaNum
-----------------
```
property String.IsAlphaNum: Boolean;
```
*)
procedure _LapeString_IsAlphaNum(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PString(Params^[0])^.IsAlphaNum;
end;

(*
String.IsNumeric
----------------
```
property String.IsNumeric: Boolean;
```
*)
procedure _LapeString_IsNumeric(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PString(Params^[0])^.IsNumeric;
end;

(*
String.IsInteger
----------------
```
property String.IsInteger: Boolean;
```
*)
procedure _LapeString_IsInteger(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PString(Params^[0])^.IsInteger;
end;

(*
String.IsFloat
--------------
```
property String.IsFloat: Boolean;
```
*)
procedure _LapeString_IsFloat(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PString(Params^[0])^.IsFloat;
end;

(*
String.Insert
-------------
```
procedure String.Insert(Value: String; Index: Integer);
```
*)
procedure _LapeString_Insert(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  Insert(PString(Params^[1])^, PString(Params^[0])^, PInteger(Params^[2])^);
end;

(*
String.DeleteIndex
------------------
```
function String.DeleteIndex(Index: Integer): Char;
```
*)
procedure _LapeString_DeleteIndex(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  Delete(PString(Params^[0])^, PInteger(Params^[1])^, 1);
end;

(*
String.DeleteRange
------------------
```
procedure String.DeleteRange(StartIndex, EndIndex: Integer);
```
*)
procedure _LapeString_DeleteRange(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  Delete(PString(Params^[0])^, PInteger(Params^[1])^, (PInteger(Params^[2])^ - PInteger(Params^[1])^) + 1);
end;

(*
String.Copy
-----------
```
function String.Copy: String;
```
*)
procedure _LapeString_Copy(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := Copy(PString(Params^[0])^, 1, Length(PString(Params^[0])^));
end;

(*
String.CopyRange
----------------
```
function String.CopyRange(StartIndex, EndIndex: Integer): String;
```
*)
procedure _LapeString_CopyRange(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := Copy(PString(Params^[0])^, PInteger(Params^[1])^, (PInteger(Params^[2])^ - PInteger(Params^[1])^) + 1);
end;

(*
String.Count
------------
```
function String.Count(Value: String): Integer;
```
*)
procedure _LapeString_Count(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PString(Params^[0])^.Count(PString(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
String.Contains
---------------
```
function String.Contains(Value: String; CaseSensitive: Boolean): Boolean;
```
*)
procedure _LapeString_Contains(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PString(Params^[0])^.Contains(PString(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
String.ContainsAny
------------------
```
function String.ContainsAny(Values: TStringArray; CaseSensitive: Boolean = True): Boolean;
```
*)
procedure _LapeString_ContainsAny(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PString(Params^[0])^.ContainsAny(PStringArray(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
String.Format
-------------
```
function String.Format(Args: TVariantArray): String;
```
*)
procedure _LapeString_Format(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
type
  TVariantArray = array of Variant;
  PVariantArray = ^TVariantArray;
begin
  with VariantArrToConstArr(PVariantArray(Params^[1])^) do
    PString(Result)^ := PString(Params^[0])^.Format(VarRecs);
end;

(*
String.ToBytes
--------------
```
function String.ToBytes: TByteArray;
```
*)
procedure _LapeString_ToBytes(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PByteArray(Result)^ := PString(Params^[0])^.ToBytes();
end;

(*
String.ToBoolean
----------------
```
function String.ToBoolean: Boolean;
```
*)
procedure _LapeString_ToBoolean(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PString(Params^[0])^.ToBoolean();
end;

(*
String.ToBoolean
----------------
```
function String.ToBoolean(Default: Boolean): Boolean;
```
*)
procedure _LapeString_ToBooleanDef(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PString(Params^[0])^.ToBoolean(PBoolean(Params^[1])^);
end;

(*
String.ToInteger
----------------
```
function String.ToInteger: Integer;
```
*)
procedure _LapeString_ToInteger(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PString(Params^[0])^.ToInteger();
end;

(*
String.ToInteger
----------------
```
function String.ToInteger(Default: Integer): Integer;
```
*)
procedure _LapeString_ToIntegerDef(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PString(Params^[0])^.ToInteger(PInteger(Params^[1])^);
end;

(*
String.ToInt64
--------------
```
function String.ToInt64: Int64;
```
*)
procedure _LapeString_ToInt64(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := PString(Params^[0])^.ToInt64();
end;

(*
String.ToInt64
--------------
```
function String.ToInt64(Default: Int64): Int64;
```
*)
procedure _LapeString_ToInt64Def(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := PString(Params^[0])^.ToInt64(PInt64(Params^[1])^);
end;

(*
String.ToSingle
---------------
```
function String.ToSingle: Single;
```
*)
procedure _LapeString_ToSingle(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PString(Params^[0])^.ToSingle();
end;

(*
String.ToSingle
---------------
```
function String.ToSingle(Default: Single): Single;
```
*)
procedure _LapeString_ToSingleDef(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PString(Params^[0])^.ToSingle(PSingle(Params^[1])^);
end;

(*
String.ToDouble
---------------
```
function String.ToDouble: Double;
```
*)
procedure _LapeString_ToDouble(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := PString(Params^[0])^.ToDouble();
end;

(*
String.ToDouble
---------------
```
function String.ToDouble(Default: Double): Double;
```
*)
procedure _LapeString_ToDoubleDef(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := PString(Params^[0])^.ToDouble(PDouble(Params^[1])^);
end;

(*
String.ToDateTime
-----------------
```
function String.ToDateTime(Fmt: String; Def: TDateTime): TDateTime;
```
*)
procedure _LapeString_ToDateTime(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Result)^ := PString(Params^[0])^.ToDateTime(PString(Params^[1])^, PDateTime(Params^[2])^);
end;

(*
*
-
```
operator *(Left: String; Right: Integer): String;
```
*)
procedure _LapeString_MUL_Integer(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PString(Params^[0])^ * PInteger(Params^[1])^;
end;

(*
in
--
```
operator in(Left: String; Right: String): Boolean;
```
*)
procedure _LapeString_IN_String(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PString(Params^[0])^ in PString(Params^[1])^;
end;

(*
in
--
```
operator in(Left: String; Right: TStringArray): Boolean;
```
*)
procedure _LapeString_IN_StringArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PString(Params^[0])^ in PStringArray(Params^[1])^;
end;

(*
Char.IsAlpha
------------
```
property Char.IsAlpha: Boolean;
```
*)
procedure _LapeChar_IsAlpha(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PChar(Params^[0])^.IsAlpha;
end;

(*
Char.IsAlphaNum
---------------
```
property Char.IsAlphaNum: Boolean;
```
*)
procedure _LapeChar_IsAlphaNum(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PChar(Params^[0])^.IsAlphaNum;
end;

(*
Char.IsNumeric
--------------
```
property Char.IsNumeric: Boolean;
```
*)
procedure _LapeChar_IsNumeric(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PChar(Params^[0])^.IsNumeric;
end;

(*
Char.IsUpper
------------
```
property Char.IsUpper: Boolean;
```
*)
procedure _LapeChar_IsUpper(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PChar(Params^[0])^.IsUpper;
end;

(*
Char.IsLower
------------
```
property Char.IsLower: Boolean;
```
*)
procedure _LapeChar_IsLower(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PChar(Params^[0])^.IsLower;
end;

(*
Char.ToUpper
------------
```
function Char.ToUpper: String;
```
*)
procedure _LapeChar_ToUpper(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PChar(Params^[0])^.ToUpper();
end;

(*
Char.ToLower
------------
```
function Char.ToLower: String;
```
*)
procedure _LapeChar_ToLower(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PChar(Params^[0])^.ToLower();
end;

(*
Char.Join
---------
```
function Char.Join(Values: TStringArray): String;
```
*)
procedure _LapeChar_Join(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PChar(Params^[0])^.Join(PStringArray(Params^[1])^);
end;

(*
*
-
```
operator *(Left: Char; Right: Integer): String;
```
*)
procedure _LapeChar_MUL_Integer(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PChar(Params^[0])^ * PInteger(Params^[1])^;
end;

(*
in
--
```
operator in(Left: Char; Right: String): Boolean;
```
*)
procedure _LapeChar_IN_String(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PChar(Params^[0])^ in PString(Params^[1])^;
end;

(*
in
--
```
operator in(Left: Char; Right: TStringArray): Boolean;
```
*)
procedure _LapeChar_IN_StringArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PChar(Params^[0])^ in PStringArray(Params^[1])^;
end;

(*
TStringArray.Join
-----------------
```
function TStringArray.Join(Glue: String): String;
```
*)
procedure _LapeStringArray_Join(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PStringArray(Params^[0])^.Join(PString(Params^[1])^);
end;

procedure ImportString(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    DumpSection := 'String';

    addGlobalType(
      'record'               + LineEnding +
      '  Position: Integer;' + LineEnding +
      '  Length: Integer;'   + LineEnding +
      '  Match: String;'     + LineEnding +
      'end;',
      'TRegExprGroup'
    );
    addGlobalType('array of TRegExprGroup', 'TRegExprGroupArray');
    addGlobalType(
      'record'                          + LineEnding +
      '  Position: Integer;'            + LineEnding +
      '  Length: Integer;'              + LineEnding +
      '  Match: String;'                + LineEnding +
      ''                                + LineEnding +
      '  Groups: TRegExprGroupArray;'   + LineEnding +
      'end;',
      'TRegExprMatch'
    );
    addGlobalType('array of TRegExprMatch', 'TRegExprMatchArray');

    addGlobalFunc('procedure String.SetLength(NewLen: Int32);', @_LapeString_SetLength);
    addGlobalFunc('property String.Length: Int32;', @_LapeString_Length);
    addGlobalFunc('property String.Low: Int32;', @_LapeString_Low);
    addGlobalFunc('property String.High: Int32;', @_LapeString_High);
    addGlobalFunc('property String.Pop: Char;', @_LapeString_Pop);
    addGlobalFunc('property String.First: Char;', @_LapeString_First);
    addGlobalFunc('property String.Last: Char;', @_LapeString_Last);

    addGlobalFunc('property String.IsUpper: Boolean;', @_LapeString_IsUpper);
    addGlobalFunc('property String.IsLower: Boolean;', @_LapeString_IsLower);
    addGlobalFunc('property String.IsAlpha: Boolean;', @_LapeString_IsAlpha);
    addGlobalFunc('property String.IsAlphaNum: Boolean;', @_LapeString_IsAlphaNum);
    addGlobalFunc('property String.IsNumeric: Boolean;', @_LapeString_IsNumeric);
    addGlobalFunc('property String.IsInteger: Boolean;', @_LapeString_IsInteger);
    addGlobalFunc('property String.IsFloat: Boolean;', @_LapeString_IsFloat);

    addGlobalFunc('procedure String.Insert(Value: String; Index: Int32);', @_LapeString_Insert);
    addGlobalFunc('procedure String.DeleteIndex(Index: Integer);', @_LapeString_DeleteIndex);
    addGlobalFunc('procedure String.DeleteRange(StartIndex, EndIndex: Int32);', @_LapeString_DeleteRange);
    addGlobalFunc('function String.Copy: String', @_LapeString_Copy);
    addGlobalFunc('function String.CopyRange(StartIndex, EndIndex: Int32): String', @_LapeString_CopyRange);

    addGlobalFunc('function String.Equals(Other: String; CaseSensitive: Boolean = True): Boolean;', @_LapeString_Equals);
    addGlobalFunc('function String.Compare(Other: String): Integer;', @_LapeString_Compare);
    addGlobalFunc('function String.Similarity(Other: String): Double;', @_LapeString_Similarity);
    addGlobalFunc('function String.Hash(Seed: UInt32 = 0): UInt32;', @_LapeString_Hash);

    addGlobalFunc('function String.ToUpper: String;', @_LapeString_ToUpper);
    addGlobalFunc('function String.ToLower: String;', @_LapeString_ToLower);
    addGlobalFunc('function String.Capitalize: String;', @_LapeString_Capitalize);
    addGlobalFunc('function String.CapitalizeWords: String;', @_LapeString_CapitalizeWords);
    addGlobalFunc('function String.SwapCase: String;', @_LapeString_SwapCase);

    addGlobalFunc('function String.Before(Value: String): String;', @_LapeString_Before);
    addGlobalFunc('function String.After(Value: String): String;', @_LapeString_After);

    addGlobalFunc('function String.Between(S1, S2: String): String;', @_LapeString_Between);
    addGlobalFunc('function String.BetweenAll(S1, S2: String): TStringArray;', @_LapeString_BetweenAll);

    addGlobalFunc('function String.RegExprSplit(Pattern: String): TStringArray;', @_LapeString_RegExprSplit);
    addGlobalFunc('function String.RegExprFindAll(Pattern: String): TRegExprMatchArray;', @_LapeString_RegExprFindAll);
    addGlobalFunc('function String.RegExprFind(Pattern: String): TRegExprMatch;', @_LapeString_RegExprFind);
    addGlobalFunc('function String.RegExprExists(Pattern: String): Boolean;', @_LapeString_RegExprExists);

    addGlobalFunc('function String.IndexOf(Value: String): Integer; overload;', @_LapeString_IndexOf);
    addGlobalFunc('function String.IndexOf(Value: String; Offset: Integer): Integer; overload;', @_LapeString_IndexOfEx);
    addGlobalFunc('function String.LastIndexOf(Value: String): Integer; overload;', @_LapeString_LastIndexOf);
    addGlobalFunc('function String.LastIndexOf(Value: String; Offset: Integer): Integer; overload;', @_LapeString_LastIndexOfEx);
    addGlobalFunc('function String.IndicesOf(Value: String): TIntegerArray; overload;', @_LapeString_IndicesOf);
    addGlobalFunc('function String.IndicesOf(Value: String; Offset: Integer): TIntegerArray; overload;', @_LapeString_IndicesOfEx);

    addGlobalFunc('function String.Extract(Chars: array of Char): String;', @_LapeString_Extract);
    addGlobalFunc('function String.ExtractInteger(Default: Int64 = -1): Int64;', @_LapeString_ExtractInteger);
    addGlobalFunc('function String.ExtractFloat(Default: Double = -1): Double;', @_LapeString_ExtractFloat);

    addGlobalFunc('function String.Trim: String; overload;', @_LapeString_Trim);
    addGlobalFunc('function String.Trim(TrimChars: array of Char): String; overload;', @_LapeString_TrimEx);

    addGlobalFunc('function String.TrimLeft: String; overload;', @_LapeString_TrimLeft);
    addGlobalFunc('function String.TrimLeft(TrimChars: array of Char): String; overload;', @_LapeString_TrimLeftEx);

    addGlobalFunc('function String.TrimRight: String; overload;', @_LapeString_TrimRight);
    addGlobalFunc('function String.TrimRight(TrimChars: array of Char): String; overload;', @_LapeString_TrimRightEx);

    addGlobalFunc('function String.StartsWith(Value: String; CaseSensitive: Boolean = True): Boolean;', @_LapeString_StartsWith);
    addGlobalFunc('function String.EndsWith(Value: String; CaseSensitive: Boolean = True): Boolean;', @_LapeString_EndsWith);

    addGlobalFunc('function String.Partition(Value: String): TStringArray;', @_LapeString_Partition);
    addGlobalFunc('function String.Replace(OldValue, NewValue: String; CaseSensitive: Boolean = True): String;', @_LapeString_Replace);

    addGlobalFunc('function String.Count(Value: String; CaseSensitive: Boolean = True): Integer;', @_LapeString_Count);
    addGlobalFunc('function String.Contains(Value: String; CaseSensitive: Boolean = True): Boolean; overload', @_LapeString_Contains);
    addGlobalFunc('function String.ContainsAny(Values: TStringArray; CaseSensitive: Boolean = True): Boolean;', @_LapeString_ContainsAny);

    addGlobalFunc('function String.Join(Values: TStringArray): String;', @_LapeString_Join);
    addGlobalFunc('function String.Split(Seperator: String; ExcludeEmpty: Boolean = True): TStringArray;', @_LapeString_Split);
    addGlobalFunc('function String.SplitLines: TStringArray;', @_LapeString_SplitLines);

    addGlobalFunc('function String.PadLeft(Count: Integer; PaddingChar: Char = #32): String', @_LapeString_PadLeft);
    addGlobalFunc('function String.PadRight(Count: Integer; PaddingChar: Char = #32): String', @_LapeString_PadRight);
    addGlobalFunc('function String.PadCenter(ACount: Integer; PaddingChar: Char = #32): String', @_LapeString_PadCenter);

    addGlobalFunc('function String.Format(Args: TVariantArray): String;', @_LapeString_Format);

    addGlobalFunc('function String.ToBytes: TByteArray; overload;', @_LapeString_ToBytes);
    addGlobalFunc('function String.ToBoolean: Boolean; overload;', @_LapeString_ToBoolean);
    addGlobalFunc('function String.ToBoolean(Default: Boolean): Boolean; overload;', @_LapeString_ToBooleanDef);
    addGlobalFunc('function String.ToInteger: Integer; overload;', @_LapeString_ToInteger);
    addGlobalFunc('function String.ToInteger(Default: Integer): Integer; overload;', @_LapeString_ToIntegerDef);
    addGlobalFunc('function String.ToInt64: Int64; overload;', @_LapeString_ToInt64);
    addGlobalFunc('function String.ToInt64(Default: Int64): Int64; overload;', @_LapeString_ToInt64Def);
    addGlobalFunc('function String.ToSingle: Single; overload;', @_LapeString_ToSingle);
    addGlobalFunc('function String.ToSingle(Default: Single): Single; overload;', @_LapeString_ToSingleDef);
    addGlobalFunc('function String.ToDouble: Double; overload;', @_LapeString_ToDouble);
    addGlobalFunc('function String.ToDouble(Default: Double): Double; overload;', @_LapeString_ToDoubleDef);
    addGlobalFunc('function String.ToDateTime(Fmt: String; Def: TDateTime): TDateTime;', @_LapeString_ToDateTime);

    addGlobalFunc('function TStringArray.Join(Glue: String): String;', @_LapeStringArray_Join);

    addGlobalFunc('operator *(Left: String; Right: Integer): String', @_LapeString_MUL_Integer);
    addGlobalFunc('operator in(Left: String; Right: String): Boolean', @_LapeString_IN_String);
    addGlobalFunc('operator in(Left: String; Right: TStringArray): Boolean', @_LapeString_IN_StringArray);

    addGlobalFunc('property Char.IsUpper: Boolean;', @_LapeChar_IsUpper);
    addGlobalFunc('property Char.IsLower: Boolean;', @_LapeChar_IsLower);
    addGlobalFunc('property Char.IsAlpha: Boolean;', @_LapeChar_IsAlpha);
    addGlobalFunc('property Char.IsAlphaNum: Boolean;', @_LapeChar_IsAlphaNum);
    addGlobalFunc('property Char.IsNumeric: Boolean;', @_LapeChar_IsNumeric);
    addGlobalFunc('function Char.Join(Values: TStringArray): String;', @_LapeChar_Join);

    addGlobalFunc('operator *(Left: Char; Right: Integer): String', @_LapeChar_MUL_Integer);
    addGlobalFunc('operator in(Left: Char; Right: String): Boolean', @_LapeChar_IN_String);
    addGlobalFunc('operator in(Left: Char; Right: TStringArray): Boolean', @_LapeChar_IN_StringArray);

    DumpSection := '';
  end;
end;

end.
