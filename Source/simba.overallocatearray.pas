{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.overallocatearray;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes;

type
  generic TSimbaOverAllocateArray<_T> = record
  private
  type
    TSelf = specialize TSimbaOverAllocateArray<_T>;
    TArr = specialize TArray<_T>;
  private
    FLength: Integer;
    FCount: Integer;
    FArr: TArr;

    procedure EnsureGrowth(const Len: Integer = 1); inline;
    function GetItem(const Index: Integer): _T; inline;
  public
    property Count: Integer read FCount;
    property Item[Index: Integer]: _T read GetItem; default;

    procedure Clear;
    procedure Init(const InitialSize: Integer = 1024);
    procedure InitWith(const Values: TArr);

    procedure Add(const Value: _T); overload; inline;
    procedure Add(const Values: TArr); overload; inline;

    function First: _T; inline;
    function Last: _T; inline;
    function Pop: _T; inline;

    function Trim: TArr;
    function Copy: TArr;

    class operator Initialize(var Self: TSelf);
  end;

  TSimbaPointBuffer = specialize TSimbaOverAllocateArray<TPoint>;
  TSimbaPointArrayBuffer = specialize TSimbaOverAllocateArray<TPointArray>;

type
  TSimbaPointBufferHelper = record helper for TSimbaPointBuffer
  public
    procedure Add(const X, Y: Integer); overload; inline;
  end;

implementation

procedure TSimbaOverAllocateArray.EnsureGrowth(const Len: Integer);
begin
  if (FCount + Len >= FLength) then
  begin
    FLength := FLength + Len;
    if (FLength < 32) then
      FLength := 32;
    FLength := FLength * 2;

    SetLength(FArr, FLength);
  end;
end;

procedure TSimbaPointBufferHelper.Add(const X, Y: Integer);
begin
  EnsureGrowth();

  FArr[FCount].X := X;
  FArr[FCount].Y := Y;

  Inc(FCount);
end;

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

  if (FLength > 0) then
    SetLength(FArr, FLength);
end;

procedure TSimbaOverAllocateArray.InitWith(const Values: TArr);
begin
  FArr := Values;
  FLength := Length(FArr);
  FCount := FLength;
end;

procedure TSimbaOverAllocateArray.Add(const Value: _T);
begin
  EnsureGrowth();

  FArr[FCount] := Value;
  Inc(FCount);
end;

procedure TSimbaOverAllocateArray.Add(const Values: TArr);
var
  Len: Integer;
begin
  Len := Length(Values);

  if (Len > 0) then
  begin
    EnsureGrowth(Len);
    Move(Values[0], FArr[FCount], Len * SizeOf(_T));
    Inc(FCount, Len);
  end;
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

function TSimbaOverAllocateArray.Copy: TArr;
begin
  Result := System.Copy(FArr, 0, FCount);
end;

class operator TSimbaOverAllocateArray.Initialize(var Self: TSelf);
begin
  Self := Default(TSelf);
end;

end.
