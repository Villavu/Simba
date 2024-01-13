{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.array_string;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes;

type
  TStringArrayHelper = type helper for TStringArray
    function IndexOf(Value: String): Integer;
    function IndicesOf(Value: String): TIntegerArray;
    procedure Sort;
  end;

implementation

uses
  StrUtils,
  simba.algo_sort;

function TStringArrayHelper.IndexOf(Value: String): Integer;
begin
  Result := specialize IndexOf<String>(Value, Self);
end;

function TStringArrayHelper.IndicesOf(Value: String): TIntegerArray;
begin
  Result := specialize IndicesOf<String>(Value, Self);
end;

procedure TStringArrayHelper.Sort;
begin
  specialize QuickSort<String>(Self, Low(Self), High(Self), @NaturalCompareText);
end;

end.

