{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Simple generic stack class.
}
unit simba.stack;

{$i simba.inc}

interface

uses
  Classes, SysUtils;

type
  generic TSimbaStack<_T> = class(TObject)
  public type
    TArr = array of _T;
  protected
    FArr: TArr;
    FLen: Integer;
    FCount: Integer;

    function GetTop: _T;
  public
    property Top: _T read GetTop;
    property Count: Integer read FCount;
    procedure Clear;
    procedure Push(Item: _T);
    function Pop: _T;
  end;

implementation

function TSimbaStack.GetTop: _T;
begin
  if (FCount <= 0) then
    raise Exception.Create('TSimbaStack.GetTop: Stack is empty');

  Result := FArr[FCount - 1];
end;

procedure TSimbaStack.Clear;
begin
  FCount := 0;
end;

procedure TSimbaStack.Push(Item: _T);
begin
  if (FCount >= Length(FArr)) then
    SetLength(FArr, 32 + (Length(FArr) * 2));

  FArr[FCount] := Item;
  Inc(FCount);
end;

function TSimbaStack.Pop: _T;
begin
  if (FCount <= 0) then
    raise Exception.Create('TSimbaStack.GetTop: Stack is empty');

  Dec(FCount);

  Result := FArr[FCount];
end;

end.

