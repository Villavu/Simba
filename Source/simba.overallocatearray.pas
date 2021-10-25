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
    TArr = specialize TArray<_T>;
  private
    FLength: Integer;
    FCount: Integer;
    FArr: TArr;

    function GetItem(const Index: Integer): _T; inline;
  public
    property Count: Integer read FCount;
    property Item[Index: Integer]: _T read GetItem; default;

    procedure Clear;
    procedure Init(const InitialSize: Integer = 1024);
    procedure Add(const Value: _T); overload; inline;
    procedure Add(const Values: TArr); overload; inline;

    function First: _T; inline;
    function Last: _T; inline;
    function Pop: _T; inline;

    function Trim: TArr;
  end;

implementation

function TSimbaOverAllocateArray.GetItem(const Index: Integer): _T;
begin
  Result := FArr[Index];
end;

procedure TSimbaOverAllocateArray.Clear;
begin
  FCount := 0;
end;

procedure TSimbaOverAllocateArray.Init(const InitialSize: Integer);
begin
  FLength := InitialSize;
  FCount := 0;

  SetLength(FArr, FLength);
end;

procedure TSimbaOverAllocateArray.Add(const Value: _T);
begin
  FArr[FCount] := Value;
  Inc(FCount);

  if (FCount = FLength) then
  begin
    FLength := FLength + (FLength div 2);
    SetLength(FArr, FLength);
  end;
end;

procedure TSimbaOverAllocateArray.Add(const Values: TArr);
begin
  if (Length(Values) = 0) then
    Exit;

  if (FCount + Length(Values) >= FLength) then
  begin
    FLength := Length(Values) + FLength + (FLength div 2);

    SetLength(FArr, FLength);
  end;

  Move(Values[0], FArr[FCount], Length(Values) * SizeOf(_T));
  Inc(FCount, Length(Values));
end;

function TSimbaOverAllocateArray.First: _T;
begin
  Result := FArr[0];
end;

function TSimbaOverAllocateArray.Last: _T;
begin
  Result := FArr[FCount - 1];
end;

function TSimbaOverAllocateArray.Pop: _T;
begin
  Result := FArr[FCount - 1];
  Dec(FCount);
end;

function TSimbaOverAllocateArray.Trim: TArr;
begin
  FLength := FCount;
  SetLength(FArr, FLength);

  Result := FArr;
end;

end.

