{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.vartype_stringarray;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base;

type
  TStringArrayHelper = type helper for TStringArray
    function IndexOf(Value: String): Integer;
    function IndicesOf(Value: String): TIntegerArray;
    function Unique: TStringArray;
    procedure Sort;
  end;

implementation

uses
  StrUtils,
  simba.array_algorithm;

function TStringArrayHelper.IndexOf(Value: String): Integer;
begin
  Result := specialize IndexOf<String>(Value, Self);
end;

function TStringArrayHelper.IndicesOf(Value: String): TIntegerArray;
begin
  Result := specialize IndicesOf<String>(Value, Self);
end;

function TStringArrayHelper.Unique: TStringArray;
begin
  Result := specialize TArrayUnique<String>.Unique(Self);
end;

procedure TStringArrayHelper.Sort;

  function Compare(const L, R: String): Integer;
  begin
    Result := NaturalCompareText(L, R);
  end;

begin
  specialize TArraySortFunc<String>.QuickSort(Self, Low(Self), High(Self), @Compare);
end;

end.

