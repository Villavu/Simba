{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.array_ord;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.colormath;

type
  TIntegerArrayHelper = type helper for TIntegerArray
    function Equals(Other: TIntegerArray): Boolean;
    function IndexOf(Value: Integer): Integer;
    function IndicesOf(Value: Integer): TIntegerArray;
    function Min: Integer;
    function Max: Integer;
    function Sum: Int64;
    function Unique: TIntegerArray;
    procedure Sort;
  end;

  TColorArrayHelper = type helper for TColorArray
    function Unique: TColorArray;
  end;

  TSingleArrayHelper = type helper for TSingleArray
    function Equals(Other: TSingleArray): Boolean;
    function IndexOf(Value: Single): Integer;
    function IndicesOf(Value: Single): TIntegerArray;
    function Min: Single;
    function Max: Single;
    function Sum: Double;
    function Unique: TSingleArray;
    procedure Sort;
  end;

  TDoubleArrayHelper = type helper for TDoubleArray
    function Equals(Other: TDoubleArray): Boolean;
    function IndexOf(Value: Double): Integer;
    function IndicesOf(Value: Double): TIntegerArray;
    function Min: Double;
    function Max: Double;
    function Sum: Double;
    function Unique: TDoubleArray;
    procedure Sort;
  end;

implementation

uses
  Math,
  simba.algo_unique, simba.algo_sort;

function TIntegerArrayHelper.Equals(Other: TIntegerArray): Boolean;
begin
  Result := specialize Equals<Integer>(Self, Other);
end;

function TIntegerArrayHelper.IndexOf(Value: Integer): Integer;
begin
  Result := specialize IndexOf<Integer>(Value, Self);
end;

function TIntegerArrayHelper.IndicesOf(Value: Integer): TIntegerArray;
begin
  Result := specialize IndicesOf<Integer>(Value, Self);
end;

function TIntegerArrayHelper.Min: Integer;
begin
  Result := specialize MinA<Integer>(Self);
end;

function TIntegerArrayHelper.Max: Integer;
begin
  Result := specialize MaxA<Integer>(Self);
end;

function TIntegerArrayHelper.Sum: Int64;
begin
  Result := specialize Sum<Integer, Int64>(Self);
end;

function TIntegerArrayHelper.Unique: TIntegerArray;
begin
  Result := specialize Unique<Integer>(Self);
end;

procedure TIntegerArrayHelper.Sort;
begin
  specialize QuickSort<Integer>(Self, Low(Self), High(Self));
end;

function TColorArrayHelper.Unique: TColorArray;
begin
  Result := specialize Unique<TColor>(Self);
end;

function TSingleArrayHelper.Equals(Other: TSingleArray): Boolean;
begin
  Result := specialize Equals<Single>(Self, Other);
end;

function TSingleArrayHelper.IndexOf(Value: Single): Integer;
begin
  Result := specialize IndexOf<Single>(Value, Self);
end;

function TSingleArrayHelper.IndicesOf(Value: Single): TIntegerArray;
begin
  Result := specialize IndicesOf<Single>(Value, Self);
end;

function TSingleArrayHelper.Min: Single;
begin
  Result := specialize MinA<Single>(Self);
end;

function TSingleArrayHelper.Max: Single;
begin
  Result := specialize MaxA<Single>(Self);
end;

function TSingleArrayHelper.Sum: Double;
begin
  Result := specialize Sum<Single, Double>(Self);
end;

function TSingleArrayHelper.Unique: TSingleArray;
begin
  Result := specialize Unique<Single>(Self);
end;

procedure TSingleArrayHelper.Sort;
begin
  specialize QuickSort<Single>(Self, Low(Self), High(Self));
end;

function TDoubleArrayHelper.Equals(Other: TDoubleArray): Boolean;
begin
  Result := specialize Equals<Double>(Self, Other);
end;

function TDoubleArrayHelper.IndexOf(Value: Double): Integer;
begin
  Result := specialize IndexOf<Double>(Value, Self);
end;

function TDoubleArrayHelper.IndicesOf(Value: Double): TIntegerArray;
begin
  Result := specialize IndicesOf<Double>(Value, Self);
end;

function TDoubleArrayHelper.Min: Double;
begin
  Result := specialize MinA<Double>(Self);
end;

function TDoubleArrayHelper.Max: Double;
begin
  Result := specialize MaxA<Double>(Self);
end;

function TDoubleArrayHelper.Sum: Double;
begin
  Result := specialize Sum<Double, Double>(Self);
end;

function TDoubleArrayHelper.Unique: TDoubleArray;
begin
  Result := specialize Unique<Double>(Self);
end;

procedure TDoubleArrayHelper.Sort;
begin
  specialize QuickSort<Double>(Self, Low(Self), High(Self));
end;

end.

