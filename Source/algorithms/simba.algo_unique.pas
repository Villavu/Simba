{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.algo_unique;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Math,
  simba.mufasatypes;

generic function Unique<_T>(const Arr: specialize TArray<_T>): specialize TArray<_T>;

function Algo_Unique_Points(const Arr: TPointArray): TPointArray;
function Algo_Unique_Integer(const Arr: TIntegerArray): TIntegerArray;
function Algo_Unique_String(const Arr: TStringArray): TStringArray;

implementation

uses
  TypInfo,
  simba.tpa, simba.arraybuffer, simba.math;

generic function Unique<_T>(const Arr: specialize TArray<_T>): specialize TArray<_T>;
var
  VarType: (OTHER, SINGLE, DOUBLE);

  function IsEquals(constref A, B: _T): Boolean;
  begin
    case VarType of
      SINGLE: Result := SameValue(PSingle(@A)^, PSingle(@B)^);
      DOUBLE: Result := SameValue(PDouble(@A)^, PDouble(@B)^);
      OTHER:  Result := (A = B);
    end;
  end;

var
  I, J, Last: Integer;
begin
  case GetTypeData(TypeInfo(_T))^.FloatType of
    ftSingle: VarType := SINGLE;
    ftDouble: VarType := DOUBLE;
    else
      VarType := OTHER;
  end;

  Result := Copy(Arr);

  Last := Length(Result);
  I := 0;
  while (I < Last) do
  begin
    J := I + 1;
    while (J < Last) do
    begin
      if IsEquals(Result[I], Result[J]) then
      begin
        Result[J] := Result[Last - 1];

        Dec(Last);
        Dec(J);
      end;

      Inc(J);
    end;
    Inc(I);
  end;

  SetLength(Result, Last);
end;

function Algo_Unique_Points(const Arr: TPointArray): TPointArray;
var
  Matrix: TBooleanMatrix;
  I, Count: Integer;
begin
  Result := Default(TPointArray);

  if (Length(Arr) > 0) then
  begin
    SetLength(Result, Length(Arr));

    Count := 0;
    with Arr.Bounds() do
    begin
      Matrix.SetSize(Width, Height);

      for I := 0 to High(Arr) do
        if not Matrix[Arr[I].Y - Y1, Arr[I].X - X1] then
        begin
          Matrix[Arr[I].Y - Y1, Arr[I].X - X1] := True;
          Result[Count] := Arr[I];
          Inc(Count);
        end;
    end;

    SetLength(Result, Count);
  end;
end;

function Algo_Unique_Integer(const Arr: TIntegerArray): TIntegerArray;
var
  I, J, Size: Integer;
  Value: Integer;
  Table: array of record
    Bucket: TIntegerArray;
    Count: Integer;
  end;
  Buffer: TSimbaIntegerBuffer;
label
  Next;
begin
  Buffer.Init();

  SetLength(Table, NextPower2(Length(Arr)));
  Size := High(Table);

  for i := 0 to High(Arr) do
  begin
    Value := Arr[i];

    with Table[Value and Size] do
    begin
      for J := 0 to Count - 1 do
        if (Value = Bucket[J]) then
          goto Next;

      if (Count >= Length(Bucket)) then
        SetLength(Bucket, 4 + (Length(Bucket) * 2));

      Bucket[Count] := Value;
      Inc(Count);

      Buffer.Add(Value);
    end;

    Next:
  end;

  Result := Buffer.ToArray(False);
end;

function Algo_Unique_String(const Arr: TStringArray): TStringArray;
var
  I, J, Size: Integer;
  Value: String;
  Table: array of record
    Bucket: TStringArray;
    Count: Integer;
  end;
  Buffer: TSimbaStringBuffer;
label
  Next;
begin
  Buffer.Init();

  SetLength(Table, NextPower2(Length(Arr)));
  Size := High(Table);

  for i := 0 to High(Arr) do
  begin
    Value := Arr[i];

    with Table[Value.Hash() and Size] do
    begin
      for J := 0 to Count - 1 do
        if (Value = Bucket[J]) then
          goto Next;

      if (Count >= Length(Bucket)) then
        SetLength(Bucket, 4 + (Length(Bucket) * 2));

      Bucket[Count] := Value;
      Inc(Count);

      Buffer.Add(Value);
    end;

    Next:
  end;

  Result := Buffer.ToArray(False);
end;

end.

