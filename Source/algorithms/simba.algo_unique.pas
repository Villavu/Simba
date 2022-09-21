unit simba.algo_unique;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Math,
  simba.mufasatypes;

generic function Unique<T>(const Arr: specialize TArray<T>): specialize TArray<T>;
generic function Unique_SameValue<T>(const Arr: specialize TArray<T>): specialize TArray<T>;

function Algo_Unique_Double(Arr: TDoubleArray): TDoubleArray;

implementation

generic function Unique<T>(const Arr: specialize TArray<T>): specialize TArray<T>;
var
  I, J, Last: Integer;
begin
  Result := Copy(Arr);

  Last := Length(Result);
  I := 0;
  while (I < Last) do
  begin
    J := I + 1;
    while (J < last) do
    begin
      if (Result[I] = Result[J]) then
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

generic function Unique_SameValue<T>(const Arr: specialize TArray<T>): specialize TArray<T>;
var
  I, J, Last: Integer;
begin
  Result := Copy(Arr);

  Last := Length(Result);
  I := 0;
  while (I < last) do
  begin
    J := I + 1;
    while (J < Last) do
    begin
      if SameValue(Result[I], Result[J]) then
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

function Algo_Unique_Double(Arr: TDoubleArray): TDoubleArray;
begin
  Result := specialize Unique_SameValue<Double>(Arr);
end;

end.

