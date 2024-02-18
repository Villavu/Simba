{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.algo_unique;

{$i simba.inc}

interface

uses
  Classes, SysUtils, TypInfo,
  simba.base, simba.math;

type
  generic THashFunc<_T> = function(const Value: _T): UInt32;
  generic TEqualsFunc<_T> = function(const L,R: _T): Boolean;

generic procedure _Unique_HashFunc<_T>(var P: Pointer; HashFunc: specialize THashFunc<_T>);
generic procedure _Unique_EqualsFunc<_T>(var P: Pointer; EqualsFunc: specialize TEqualsFunc<_T>);
generic procedure _Unique<_T>(var Arr: specialize TArray<_T>); // Just uses = operator

generic function Unique<_T>(const Arr: specialize TArray<_T>): specialize TArray<_T>;

// Sadly need to on global symtable for generics
function _HashAStr(const Value: String): UInt32;
function _HashInt(const Value: Integer): UInt32;
function _HashInt64(const Value: Int64): UInt32;

function _SameSingle(const A,B: Single): Boolean;
function _SameDouble(const A,B: Double): Boolean;

implementation

const
  EZeroResolution = 1E-16;
  DZeroResolution = 1E-12;
  SZeroResolution = 1E-4;

// FNV
function _HashAStr(const Value: String): UInt32;
var
  I: Int32;
begin
  Result := 2166136261;
  for I := 1 to Length(Value) do
  begin
    Result := Result xor Byte(Value[I]);
    Result := Result * 16777619;
  end;
end;

function _HashInt(const Value: Integer): UInt32;
begin
  Result := Value;
end;

function _HashInt64(const Value: Int64): UInt32;
begin
  Result := Value;
end;

function _SameSingle(const A, B: Single): Boolean;
begin
  if (A > B) then
    Result:=((A-B) <= SZeroResolution)
  else
    Result:=((B-A) <= SZeroResolution);
end;

function _SameDouble(const A, B: Double): Boolean;
begin
  if (A > B) then
    Result:=((A-B) <= DZeroResolution)
  else
    Result:=((B-A) <= DZeroResolution);
end;

generic procedure _Unique_EqualsFunc<_T>(var P: Pointer; EqualsFunc: specialize TEqualsFunc<_T>);
type
  TArr = specialize TArray<_T>;
var
  Arr: TArr absolute P;
  I, J, Len, NewLen: Integer;
begin
  Len := Length(Arr);
  NewLen := 0;
  for I := 0 to Len - 1 do
  begin
    J := 0;
    while (J < NewLen) do
    begin
      if EqualsFunc(Arr[I], Arr[J]) then
        Break;
      Inc(J);
    end;

    if (J = NewLen) then
    begin
      Arr[NewLen] := Arr[I];
      Inc(NewLen);
    end;
  end;

  SetLength(Arr, NewLen);
end;

generic procedure _Unique_HashFunc<_T>(var P: Pointer; HashFunc: specialize THashFunc<_T>);
type
  TArr = specialize TArray<_T>;
var
  Arr: TArr absolute P;
  I, J, Total: Integer;
  Value: _T;
  Table: array of record
    Bucket: array of _T;
    Count: Integer;
  end;
label
  Next;
begin
  Total := 0;
  SetLength(Table, NextPower2(Length(Arr)));

  for I := 0 to High(Arr) do
  begin
    Value := Arr[I];

    with Table[HashFunc(Value) and High(Table)] do
    begin
      // check if seen before
      for J := 0 to Count - 1 do
        if (Value = Bucket[J]) then
          goto Next;

      // not seen before: Add to bucket and result.
      if (Count >= Length(Bucket)) then
        SetLength(Bucket, 4 + (Length(Bucket) * 2));
      Bucket[Count] := Value;
      Inc(Count);

      Arr[Total] := Value;
      Inc(Total);
    end;

    Next:
  end;

  SetLength(Arr, Total);
end;

generic procedure _Unique<_T>(var Arr: specialize TArray<_T>);
var
  I, J, NewLen: Integer;
begin
  NewLen := 0;

  for I := 0 to High(Arr) do
  begin
    J := 0;
    while (J < NewLen) do
    begin
      if (Arr[I] = Arr[J]) then
        Break;
      Inc(J);
    end;

    if (J = NewLen) then
    begin
      Arr[NewLen] := Arr[I];
      Inc(NewLen);
    end;
  end;

  SetLength(Arr, NewLen);
end;

generic function Unique<_T>(const Arr: specialize TArray<_T>): specialize TArray<_T>;
type
  {$scopedenums on}
  EVarType = (OTHER, SINGLE, DOUBLE, INT, INT64, ASTRING);
  {$scopedenums off}

  function GetVarType: EVarType;
  begin
    Result := EVarType.OTHER;
    case GetTypeKind(_T) of
      tkFloat:
        case GetTypeData(TypeInfo(_T))^.FloatType of
          ftSingle: Result := EVarType.SINGLE;
          ftDouble: Result := EVarType.DOUBLE;
        end;
      tkInteger: Result := EVarType.INT;
      tkInt64:   Result := EVarType.INT64;
      tkAString: Result := EVarType.ASTRING;
    end;
  end;

begin
  Result := Copy(Arr);

  case GetVarType() of
    EVarType.SINGLE:  specialize _Unique_EqualsFunc<Single>(Pointer(Result), @_SameSingle);
    EVarType.DOUBLE:  specialize _Unique_EqualsFunc<Double>(Pointer(Result), @_SameDouble);
    EVarType.INT:     specialize _Unique_HashFunc<Integer>(Pointer(Result), @_HashInt);
    EVarType.INT64:   specialize _Unique_HashFunc<Int64>(Pointer(Result), @_HashInt64);
    EVarType.ASTRING: specialize _Unique_HashFunc<String>(Pointer(Result), @_HashAStr);
    else              specialize _Unique<_T>(Result);
  end;
end;

end.

