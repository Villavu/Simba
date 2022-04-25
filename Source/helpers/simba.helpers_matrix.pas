{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.helpers_matrix;

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.generics_matrix, simba.mufasatypes;

type
  TSingleMatrixHelper = type helper for TSingleMatrix
    function Width: Integer;
    function Height: Integer;
    function Area: Integer;
    function Size(out AWidth, AHeight: Integer): Boolean;
    procedure SetSize(const AWidth, AHeight: Integer);
    function GetValues(const Indices: TPointArray): TSingleArray;
    procedure SetValues(const Indices: TPointArray; const Values: TSingleArray); overload;
    procedure SetValues(const Indices: TPointArray; const Value: Single); overload;
    procedure Fill(const Box: TBox; const Value: Single); overload;
    procedure Fill(const Value: Single); overload;
    function Flatten: TSingleArray;
    function ToIntegerMatrix: TIntegerMatrix;
    function Mean: Double;
    procedure MeanStdev(out MeanValue, Stdev: Double);
    procedure MinMax(out MinValue, MaxValue: Single);
    function Min: Single;
    function Max: Single;
    function ArgMax: TPoint;
    function ArgMin: TPoint;
    function NormMinMax(const Alpha, Beta: Single): TSingleMatrix;
    function Indices(const Value: Single; const Comparator: EComparator): TPointArray;
    function ArgMulti(const Count: Integer; const HiLo: Boolean): TPointArray;
  end;

  TIntegerMatrixHelper = type helper for TIntegerMatrix
    function Width: Integer;
    function Height: Integer;
    function Area: Integer;
    function Size(out AWidth, AHeight: Integer): Boolean;
    procedure SetSize(const AWidth, AHeight: Integer);
    function GetValues(const Indices: TPointArray): TIntegerArray;
    procedure SetValues(const Indices: TPointArray; const Values: TIntegerArray); overload;
    procedure SetValues(const Indices: TPointArray; Value: Integer); overload;
    procedure Fill(const Box: TBox; const Value: Integer); overload;
    procedure Fill(const Value: Integer); overload;
    function Flatten: TIntegerArray;
    procedure MinMax(out MinValue, MaxValue: Integer);
    function Min: Single;
    function Max: Single;
    function ArgMax: TPoint;
    function ArgMin: TPoint;
    function Indices(const Value: Integer; const Comparator: EComparator): TPointArray;
    function ArgMulti(const Count: Integer; const HiLo: Boolean): TPointArray;
  end;

  TDoubleMatrixHelper = type helper for TDoubleMatrix
    function Width: Integer;
    function Height: Integer;
    function Area: Integer;
    function Size(out AWidth, AHeight: Integer): Boolean;
    procedure SetSize(const AWidth, AHeight: Integer);
    function Mean: Double;
    procedure MeanStdev(out MeanValue, Stdev: Double);
  end;

  TComplexMatrixHelper = type helper for TComplexMatrix
    function Width: Integer;
    function Height: Integer;
    function Area: Integer;
    function Size(out AWidth, AHeight: Integer): Boolean;
    procedure SetSize(const AWidth, AHeight: Integer);
  end;

  TByteMatrixHelper = type helper for TByteMatrix
    function Width: Integer;
    function Height: Integer;
    function Area: Integer;
    function Size(out AWidth, AHeight: Integer): Boolean;
    procedure SetSize(const AWidth, AHeight: Integer);
  end;

  TBooleanMatrixHelper = type helper for TBooleanMatrix
    function Width: Integer;
    function Height: Integer;
    function Area: Integer;
    function Size(out AWidth, AHeight: Integer): Boolean;
    procedure SetSize(const AWidth, AHeight: Integer);
  end;

implementation

uses
  simba.heaparray, simba.math;

function TBooleanMatrixHelper.Width: Integer;
begin
  Result := specialize MatrixWidth<Boolean>(Self);
end;

function TBooleanMatrixHelper.Height: Integer;
begin
  Result := specialize MatrixHeight<Boolean>(Self);
end;

function TBooleanMatrixHelper.Area: Integer;
begin
  Result := specialize MatrixArea<Boolean>(Self);
end;

function TBooleanMatrixHelper.Size(out AWidth, AHeight: Integer): Boolean;
begin
  Result := specialize MatrixSize<Boolean>(Self, AWidth, AHeight);
end;

procedure TBooleanMatrixHelper.SetSize(const AWidth, AHeight: Integer);
begin
  specialize MatrixSetSize<Boolean>(Self, AWidth, AHeight);
end;

function TByteMatrixHelper.Width: Integer;
begin
  Result := specialize MatrixWidth<Byte>(Self);
end;

function TByteMatrixHelper.Height: Integer;
begin
  Result := specialize MatrixHeight<Byte>(Self);
end;

function TByteMatrixHelper.Area: Integer;
begin
  Result := specialize MatrixArea<Byte>(Self);
end;

function TByteMatrixHelper.Size(out AWidth, AHeight: Integer): Boolean;
begin
  Result := specialize MatrixSize<Byte>(Self, AWidth, AHeight);
end;

procedure TByteMatrixHelper.SetSize(const AWidth, AHeight: Integer);
begin
  specialize MatrixSetSize<Byte>(Self, AWidth, AHeight);
end;

function TDoubleMatrixHelper.Width: Integer;
begin
  Result := specialize MatrixWidth<Double>(Self);
end;

function TDoubleMatrixHelper.Height: Integer;
begin
  Result := specialize MatrixHeight<Double>(Self);
end;

function TDoubleMatrixHelper.Area: Integer;
begin
  Result := specialize MatrixArea<Double>(Self);
end;

function TDoubleMatrixHelper.Size(out AWidth, AHeight: Integer): Boolean;
begin
  Result := specialize MatrixSize<Double>(Self, AWidth, AHeight);
end;

procedure TDoubleMatrixHelper.SetSize(const AWidth, AHeight: Integer);
begin
  specialize MatrixSetSize<Double>(Self, AWidth, AHeight);
end;

function TDoubleMatrixHelper.Mean: Double;
begin
  Result := specialize MatrixMean<Double>(Self);
end;

procedure TDoubleMatrixHelper.MeanStdev(out MeanValue, Stdev: Double);
begin
  specialize MatrixMeanStdev<Double>(Self, MeanValue, Stdev);
end;

function TComplexMatrixHelper.Width: Integer;
begin
  Result := specialize MatrixWidth<TComplex>(Self);
end;

function TComplexMatrixHelper.Height: Integer;
begin
  Result := specialize MatrixHeight<TComplex>(Self);
end;

function TComplexMatrixHelper.Area: Integer;
begin
  Result := specialize MatrixArea<TComplex>(Self);
end;

function TComplexMatrixHelper.Size(out AWidth, AHeight: Integer): Boolean;
begin
  Result := specialize MatrixSize<TComplex>(Self, AWidth, AHeight);
end;

procedure TComplexMatrixHelper.SetSize(const AWidth, AHeight: Integer);
begin
  specialize MatrixSetSize<TComplex>(Self, AWidth, AHeight);
end;

function TIntegerMatrixHelper.Width: Integer;
begin
  Result := specialize MatrixWidth<Integer>(Self);
end;

function TIntegerMatrixHelper.Height: Integer;
begin
  Result := specialize MatrixHeight<Integer>(Self);
end;

function TIntegerMatrixHelper.Area: Integer;
begin
  Result := specialize MatrixArea<Integer>(Self);
end;

function TIntegerMatrixHelper.Size(out AWidth, AHeight: Integer): Boolean;
begin
  Result := specialize MatrixSize<Integer>(Self, AWidth, AHeight);
end;

procedure TIntegerMatrixHelper.SetSize(const AWidth, AHeight: Integer);
begin
  specialize MatrixSetSize<Integer>(Self, AWidth, AHeight);
end;

function TIntegerMatrixHelper.GetValues(const Indices: TPointArray): TIntegerArray;
begin
  Result := specialize MatrixGetValues<Integer>(Self, Indices);
end;

procedure TIntegerMatrixHelper.SetValues(const Indices: TPointArray; const Values: TIntegerArray);
begin
  specialize MatrixSetValues<Integer>(Self, Indices, Values);
end;

procedure TIntegerMatrixHelper.SetValues(const Indices: TPointArray; Value: Integer);
begin
  specialize MatrixSetValues<Integer>(Self, Indices, Value);
end;

procedure TIntegerMatrixHelper.Fill(const Box: TBox; const Value: Integer);
begin
  specialize MatrixFill<Integer>(Self, Box, Value);
end;

procedure TIntegerMatrixHelper.Fill(const Value: Integer);
begin
  specialize MatrixFill<Integer>(Self, Value);
end;

function TIntegerMatrixHelper.Flatten: TIntegerArray;
begin
  Result := specialize MatrixFlatten<Integer>(Self);
end;

procedure TIntegerMatrixHelper.MinMax(out MinValue, MaxValue: Integer);
begin
  specialize MatrixMinMax<Integer>(Self, MinValue, MaxValue);
end;

function TIntegerMatrixHelper.Min: Single;
begin
  Result := specialize MatrixMax<Integer>(Self);
end;

function TIntegerMatrixHelper.Max: Single;
begin
  Result := specialize MatrixMax<Integer>(Self);
end;

function TIntegerMatrixHelper.ArgMax: TPoint;
begin
  Result := specialize MatrixArgMax<Integer>(Self);
end;

function TIntegerMatrixHelper.ArgMin: TPoint;
begin
  Result := specialize MatrixArgMin<Integer>(Self);
end;

function TIntegerMatrixHelper.Indices(const Value: Integer; const Comparator: EComparator): TPointArray;
begin
  Result := specialize MatrixIndices<Integer>(Self, Value, Comparator);
end;

function TIntegerMatrixHelper.ArgMulti(const Count: Integer; const HiLo: Boolean): TPointArray;
begin
  Result := specialize MatrixArgMulti<Integer>(Self, Count, HiLo);
end;

function TSingleMatrixHelper.Width: Integer;
begin
  Result := specialize MatrixWidth<Single>(Self);
end;

function TSingleMatrixHelper.Height: Integer;
begin
  Result := specialize MatrixHeight<Single>(Self);
end;

function TSingleMatrixHelper.Area: Integer;
begin
  Result := specialize MatrixArea<Single>(Self);
end;

function TSingleMatrixHelper.Size(out AWidth, AHeight: Integer): Boolean;
begin
  Result := specialize MatrixSize<Single>(Self, AWidth, AHeight);
end;

procedure TSingleMatrixHelper.SetSize(const AWidth, AHeight: Integer);
begin
  specialize MatrixSetSize<Single>(Self, AWidth, AHeight);
end;

function TSingleMatrixHelper.GetValues(const Indices: TPointArray): TSingleArray;
begin
  Result := specialize MatrixGetValues<Single>(Self, Indices);
end;

procedure TSingleMatrixHelper.SetValues(const Indices: TPointArray; const Values: TSingleArray);
begin
  specialize MatrixSetValues<Single>(Self, Indices, Values);
end;

procedure TSingleMatrixHelper.SetValues(const Indices: TPointArray; const Value: Single);
begin
  specialize MatrixSetValues<Single>(Self, Indices, Value);
end;

procedure TSingleMatrixHelper.Fill(const Box: TBox; const Value: Single);
begin
  specialize MatrixFill<Single>(Self, Box, Value);
end;

procedure TSingleMatrixHelper.Fill(const Value: Single);
begin
  specialize MatrixFill<Single>(Self, Value);
end;

function TSingleMatrixHelper.Flatten: TSingleArray;
begin
  Result := specialize MatrixFlatten<Single>(Self);
end;

function TSingleMatrixHelper.ToIntegerMatrix: TIntegerMatrix;
var
  X, Y, W, H: Integer;
begin
  if Self.Size(W, H) then
  begin
    SetLength(Result, H, W);

    W -= 1;
    H -= 1;
    for Y := 0 to H do
      for X := 0 to W do
        Result[Y, X] := Trunc(Self[Y, X]);
  end;
end;

function TSingleMatrixHelper.Mean: Double;
begin
  Result := specialize MatrixMean<Single>(Self);
end;

procedure TSingleMatrixHelper.MeanStdev(out MeanValue, Stdev: Double);
begin
  specialize MatrixMeanStdev<Single>(Self, MeanValue, Stdev);
end;

procedure TSingleMatrixHelper.MinMax(out MinValue, MaxValue: Single);
begin
  specialize MatrixMinMax<Single>(Self, MinValue, MaxValue);
end;

function TSingleMatrixHelper.Min: Single;
begin
  Result := specialize MatrixMin<Single>(Self);
end;

function TSingleMatrixHelper.Max: Single;
begin
  Result := specialize MatrixMax<Single>(Self);
end;

function TSingleMatrixHelper.ArgMax: TPoint;
begin
  Result := specialize MatrixArgMax<Single>(Self);
end;

function TSingleMatrixHelper.ArgMin: TPoint;
begin
  Result := specialize MatrixArgMin<Single>(Self);
end;

function TSingleMatrixHelper.NormMinMax(const Alpha, Beta: Single): TSingleMatrix;
begin
  Result := specialize MatrixNormMinMax<Single>(Self, Alpha, Beta);
end;

function TSingleMatrixHelper.Indices(const Value: Single; const Comparator: EComparator): TPointArray;
begin
  Result := specialize MatrixIndices<Single>(Self, Value, Comparator);
end;

function TSingleMatrixHelper.ArgMulti(const Count: Integer; const HiLo: Boolean): TPointArray;
begin
  Result := specialize MatrixArgMulti<Single>(Self, Count, HiLo);
end;

end.

