{==============================================================================]
  Copyright (c) 2021, Jarl `slacky` Holta
[==============================================================================}
unit simba.heaparray;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  generic TSimbaHeapArray<_T> = class
  public
  type
    TElement = record Value: _T; Index: Int32; end;
    THeap = array of TElement;
  protected
    FHeap: THeap;

    procedure _MoveDownHI(startPos, Pos: Int32);
    procedure _MoveDownLO(startPos, Pos: Int32);
    procedure _MoveUpHI(Pos: Int32);
    procedure _MoveUpLO(Pos: Int32);
  public
    property Data: THeap read FHeap;

    function Peek: TElement;

    procedure Push(Item: _T; idx: Int32; HiLo: Boolean);
    procedure Pop(HiLo: Boolean);
  end;

  TSimbaHeapArrayF = specialize TSimbaHeapArray<Single>;

implementation

procedure TSimbaHeapArray._MoveDownHI(startPos, Pos: Int32); inline;
var
  parentPos: Int32;
  parent, newItem: TElement;
begin
  newItem := FHeap[Pos];

  while (Pos > startPos) do
  begin
    parentPos := (Pos - 1) shr 1;
    parent := FHeap[parentPos];
    if (newItem.Value < parent.Value) then
    begin
      FHeap[Pos] := parent;
      Pos := parentPos;
      Continue;
    end;
    break;
  end;

  FHeap[Pos] := newItem;
end;

procedure TSimbaHeapArray._MoveDownLO(startPos, Pos: Int32); inline;
var
  parentPos: Int32;
  parent, newItem: TElement;
begin
  newItem := FHeap[Pos];

  while (Pos > startPos) do
  begin
    parentPos := (Pos - 1) shr 1;
    parent := FHeap[parentPos];
    if (newItem.Value > parent.Value) then
    begin
      FHeap[Pos] := parent;
      Pos := parentPos;
      Continue;
    end;
    break;
  end;

  FHeap[Pos] := newItem;
end;

procedure TSimbaHeapArray._MoveUpHI(Pos: Int32); inline;
var
  endPos, startPos, childPos, rightPos: Int32;
  newItem: TElement;
begin
  endPos := Length(FHeap);
  startPos := Pos;
  newItem := FHeap[Pos];

  childPos := 2 * Pos + 1;
  while childPos < endPos do
  begin
    rightPos := childPos + 1;
    if (rightPos < endPos) and not(FHeap[childPos].Value < FHeap[rightPos].Value) then
      childPos := rightPos;
    FHeap[Pos] := FHeap[childPos];
    Pos := childPos;
    childPos := 2 * Pos + 1;
  end;
  FHeap[Pos] := newItem;

  _MoveDownHI(startPos, Pos);
end;

procedure TSimbaHeapArray._MoveUpLO(Pos: Int32); inline;
var
  endPos, startPos, childPos, rightPos: Int32;
  newItem: TElement;
begin
  endPos := Length(FHeap);
  startPos := Pos;
  newItem := FHeap[Pos];

  childPos := 2 * Pos + 1;
  while childPos < endPos do
  begin
    rightPos := childPos + 1;
    if (rightPos < endPos) and not(FHeap[childPos].Value > FHeap[rightPos].Value) then
      childPos := rightPos;
    FHeap[Pos] := FHeap[childPos];
    Pos := childPos;
    childPos := 2 * Pos + 1;
  end;
  FHeap[Pos] := newItem;

  _MoveDownLO(startPos, Pos);
end;

function TSimbaHeapArray.Peek: TElement; inline;
begin
  if Length(FHeap) = 0 then
    raise Exception.Create('TSimbaHeapArray.Peek: Empty!');

  Result := FHeap[0];
end;

procedure TSimbaHeapArray.Push(Item: _T; idx: Int32; HiLo: Boolean); inline;
var
  hi: Int32;
begin
  hi := Length(FHeap);
  SetLength(FHeap, hi+1);

  FHeap[hi].Value := item;
  FHeap[hi].Index := idx;

  case HiLo of
    True:  _MoveDownHI(0, hi);
    False: _MoveDownLO(0, hi);
  end;
end;

procedure TSimbaHeapArray.Pop(HiLo: Boolean); inline;
var
  m: TElement;
begin
  m := FHeap[High(FHeap)];
  SetLength(FHeap, High(FHeap));

  if (High(FHeap) >= 0) then
  begin
    FHeap[0] := m;

    case HiLo of
      True:  _MoveUpHI(0);
      False: _MoveUpLO(0);
    end;
  end;
end;

end.

