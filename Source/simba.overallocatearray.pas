{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.overallocatearray;

{$i simba.inc}

interface

uses
  Classes, SysUtils;

type
  generic TSimbaOverAllocateArray<_T> = record
  private
  type
    TArray = array of _T;
  private
    FLength: Integer;
    FCount: Integer;
    FArr: TArray;
  public
    property Count: Integer read FCount;

    procedure Init(InitialSize: Integer = 1024);
    procedure Add(const Value: _T); inline;
    function Trim: TArray;
  end;

implementation

procedure TSimbaOverAllocateArray.Init(InitialSize: Integer);
begin
  FLength := InitialSize;
  FCount := 0;

  SetLength(FArr, FLength);
end;

procedure TSimbaOverAllocateArray.Add(const Value: _T);
begin
  if (FCount = FLength) then
  begin
    FLength := FLength + (FLength div 2);

    SetLength(FArr, FLength);
  end;

  FArr[FCount] := Value;

  Inc(FCount);
end;

function TSimbaOverAllocateArray.Trim: TArray;
begin
  FLength := FCount;
  SetLength(FArr, FLength);

  Result := FArr;
end;

end.

