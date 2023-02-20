{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Simple generic list class.
}
unit simba.list;

{$i simba.inc}

interface

uses
  Classes, SysUtils;

type
  generic TSimbaList<_T> = class(TObject)
  public type
    TArr = array of _T;
  protected
    FArr: TArr;
    FCount: Integer;

    function GetItem(Index: Integer): _T;
  public
    procedure Add(Item: _T);
    procedure Clear;
    function ToArray: TArr;

    property Count: Integer read FCount;
    property Items[Index: Integer]: _T read GetItem; default;
  end;

implementation

function TSimbaList.GetItem(Index: Integer): _T;
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt('TSimbaList.GetItem: Index %d out of bounds', [Index]);

  Result := FArr[Index];
end;

procedure TSimbaList.Add(Item: _T);
begin
  if (FCount >= Length(FArr)) then
    SetLength(FArr, 32 + (Length(FArr) * 2));

  FArr[FCount] := Item;
  Inc(FCount);
end;

procedure TSimbaList.Clear;
begin
  FCount := 0;
end;

function TSimbaList.ToArray: TArr;
begin
  SetLength(Result, FCount);
  if (FCount > 0) then
    Move(FArr[0], Result[0], FCount * SizeOf(_T));
end;

end.

