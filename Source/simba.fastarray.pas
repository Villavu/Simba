{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.fastarray;

{$i simba.inc}

interface

uses
  Classes, SysUtils;

type
  generic TSimbaFastArray<_T> = record
  private
  type
    TArr = array of _T;
    TSelf = specialize TSimbaFastArray<_T>;
  private
    FLength: Int32;
    FArr: TArr;
    FCount: Int32;
  public
    class operator Initialize(var Self: TSelf); inline;
    class operator :=(var Self: TSelf): TArr; inline;

    procedure Add(constref Value: _T); inline;
  end;

implementation

class operator TSimbaFastArray.Initialize(var Self: TSelf);
begin
  with Self do
  begin
    FCount := 0;
    FLength := 768;

    SetLength(FArr, FLength);
  end;
end;

class operator TSimbaFastArray.:=(var Self: TSelf): TArr;
begin
  with Self do
  begin
    FLength := FCount;
    SetLength(FArr, FLength);

    Result := FArr;
  end;
end;

procedure TSimbaFastArray.Add(constref Value: _T);
begin
  FArr[FCount] := Value;
  FCount += 1;

  if (FCount = FLength) then
  begin
    FLength := FLength + (FLength div 2);
    SetLength(FArr, FLength);
  end;
end;

end.

