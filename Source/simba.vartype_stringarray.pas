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
    function Equals(Other: TStringArray): Boolean;
    function IndexOf(Value: String): Integer;
    function IndicesOf(Value: String): TIntegerArray;
    function Unique: TStringArray;
    procedure Sort;
  end;

implementation

uses
  StrUtils,
  simba.array_algorithm;

function TStringArrayHelper.Equals(Other: TStringArray): Boolean;
begin
  Result := specialize TArrayEquals<String>.Equals(Self, Other);
end;

function TStringArrayHelper.IndexOf(Value: String): Integer;
begin
  Result := specialize TArrayIndexOf<String>.IndexOf(Value, Self);
end;

function TStringArrayHelper.IndicesOf(Value: String): TIntegerArray;
begin
  Result := specialize TArrayIndicesOf<String>.IndicesOf(Value, Self);
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

