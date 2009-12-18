{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009 by Raymond van Venetië and Merlijn Wajer

    MML is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MML is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MML.  If not, see <http://www.gnu.org/licenses/>.

	See the file COPYING, included in this distribution,
	for details about the copyright.

    TPA functions for the Mufasa Macro Library
}
unit tpa;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mufasatypes;

function FastTPASort(TPA: TPointArray; Dists: TIntegerArray; maxDist: Integer; CloseFirst: Boolean): TPointArray;

//Start Wizzyplugin
procedure tSwap(var a, b: TPoint);
procedure tpaSwap(var a, b: TPointArray);
procedure SwapE(var a, b: Extended);
procedure RAaSTPAEx(var a: TPointArray; const w, h: Integer);
procedure RAaSTPA(var a: TPointArray; const Dist: Integer);
function NearbyPointInArrayEx(const P: TPoint; w, h:Integer; a: TPointArray): Boolean;
function NearbyPointInArray(const P: TPoint; Dist:Integer; a: TPointArray): Boolean;
function ReArrangeandShortenArrayEx(a: TPointArray; w, h: Integer): TPointArray;
function ReArrangeandShortenArray(a: TPointArray; Dist: Integer): TPointArray;
function TPAtoATPAEx(TPA: TPointArray; w, h: Integer): T2DPointArray;
function TPAtoATPA(TPA: TPointArray; Dist: Integer): T2DPointArray;
procedure QuickTPASort(var A: TIntegerArray; var B: TPointArray; iLo, iHi: Integer; SortUp: Boolean);
procedure QuickATPASort(var A: TIntegerArray; var B: T2DPointArray; iLo, iHi: Integer; SortUp: Boolean);
procedure SortTPAFrom(var a: TPointArray; const From: TPoint);
procedure SortATPAFrom(var a: T2DPointArray; const From: TPoint);
procedure SortATPAFromFirstPoint(var a: T2DPointArray; const From: TPoint);
procedure InvertTPA(var a: TPointArray);
procedure InvertATPA(var a: T2DPointArray);
function MiddleTPAEx(TPA: TPointArray; var x, y: Integer): Boolean;
function MiddleTPA(tpa: TPointArray): TPoint;
procedure SortATPASize(var a: T2DPointArray; const BigFirst: Boolean);
procedure SortATPAFromSize(var a: T2DPointArray; const Size: Integer; CloseFirst: Boolean);
function CombineTPA(Ar1, Ar2: TPointArray): TPointArray;
function CombineIntArray(Ar1, Ar2: TIntegerArray): TIntegerArray;
function InIntArrayEx(a: TIntegerArray; var Where: Integer; const Number: Integer): Boolean;
function InIntArray(a: TIntegerArray; Number: Integer): Boolean;
procedure ClearSameIntegers(var a: TIntegerArray);
procedure ClearSameIntegersAndTPA(var a: TIntegerArray; var p: TPointArray);
function SplitTPAEx(arr: TPointArray; w, h: Integer): T2DPointArray;
function SplitTPA(arr: TPointArray; Dist: Integer): T2DPointArray;
procedure FilterPointsPie(var Points: TPointArray; const SD, ED, MinR, MaxR: Extended; Mx, My: Integer);
function RemoveDistTPointArray(x, y, dist: Integer; ThePoints: TPointArray; RemoveHigher: Boolean): TPointArray;
function GetATPABounds(ATPA: T2DPointArray): TBox;
function GetTPABounds(TPA: TPointArray): TBox;
function FindTPAinTPA(SearchTPA, TotalTPA: TPointArray; var Matches: TPointArray): Boolean;
function FindTextTPAinTPA(Height : integer; SearchTPA, TotalTPA: TPointArray; var Matches: TPointArray): Boolean;
function FindGapsTPA(TPA: TPointArray; MinPixels: Integer): T2DPointArray;
procedure SortCircleWise(var tpa: TPointArray; const cx, cy, StartDegree: Integer; SortUp, ClockWise: Boolean);
procedure LinearSort(var tpa: TPointArray; cx, cy, sd: Integer; SortUp: Boolean);
Function MergeATPA(ATPA : T2DPointArray)  : TPointArray;
function TPAFromBox(const Box : TBox) : TPointArray;
Function RotatePoints(Const P: TPointArray; A, cx, cy: Extended): TPointArray ;
Function RotatePoint(Const p: TPoint; angle, mx, my: Extended): TPoint; inline;
function FindTPAEdges(p: TPointArray): TPointArray;
function PointInTPA(p: TPoint; arP: TPointArray): Boolean;
function ClearTPAFromTPA(arP, ClearPoints: TPointArray): TPointArray;
procedure ClearDoubleTPA(var TPA: TPointArray);
Function ReturnPointsNotInTPA(Const TotalTPA: TPointArray; Box: TBox): TPointArray;
Procedure TPACountSort(Var TPA: TPointArray;const max: TPoint;Const SortOnX : Boolean);
Procedure TPACountSortBase(Var TPA: TPointArray;const maxx, base: TPoint; const SortOnX : Boolean);

implementation

uses
  math;




{/\
   Very Fast TPA Sort, uses an adepted CountSort algorithm.
/\}

Function FastTPASort(TPA: TPointArray; Dists: TIntegerArray; maxDist: Integer; CloseFirst: Boolean): TPointArray;

{
 If you want to understand this algorithm, it might be helpful to read about
 CountSort. This algorithm is quite similar to CountSort.
}

Var
   Oc, D: TIntegerArray;
   I, H, J: Integer;
   ind: array of array of integer;

Begin
  SetLength(Oc, maxDist + 1); // Oc stores the occurances of each distance.
  H := High(Dists); // The amount of values in the given Array of Points...
                    // We store it instead of calling High(Dists) each time.

  // Get the occurance of each distance, and store it in Oc.
  // We need the occurances of each distance, so we can later on set the 2d Array
  // to it's proper length.
  For I := 0 To H Do
    Oc[Dists[I]] := Oc[Dists[I]] + 1;

  // What we are basically going to do now is the following:
  // Sort the points by their distances using countsort.
  // But because (-2, -2) and (2, 2) have the same distance from (0, 0), we also
  // store the index in the array 'TPA', which contains the actual points.
  // This way we can retreive the actuals points.

  // D nothing but a counter. It contains the amount of points (indices) stored
  // for EACH distance, so it's length is equal to the amount of Distances.
  // ind is the array that has as first index the Distance. The second depth of
  // the array is the index which is used to retreive the original point.

  SetLength(D, maxDist + 1);
  SetLength(ind, maxDist + 1);
  For I := 0 To maxDist Do  // set the ind length of each ind[distance] to Oc[distance]
    SetLength(ind[i], Oc[i] + 1);
  For I := 0 To H Do  // Performs the actual index sorting
  Begin
    // put an index (of Dists AND TPA) into a distance array.
    ind[Dists[I]][D[Dists[I]]] := I;
    // inc by one, so we won't override previous indexes.
    D[Dists[i]] := D[Dists[i]] + 1;
  End;

  // Here we are back to the CountSort style of writing back.
  // Now we are going to write back to the Result.
  // The Result's length is obviously of the same length as 'TPA'.
  SetLength(Result, H + 1);
  H := 0;

  // CloseFirst just means: Start writing from dist 0 to maxDist, or
  // start writing from maxDist to dist 0. The first one places the closest points
  // at the start of the result. The higher the index, the more far away the point.
  // Not Closefirst means put the most far away point in Result first.
  If CloseFirst Then
  Begin
    For I := 0 To maxDist Do  // Put back to result
      For J := 0 To oC[I] - 1 Do
      Begin
        Result[H] := TPA[ind[I][J]];
        H := H + 1;
      End;
  End
  Else
    For I := maxDist Downto 0 Do  // Put back to result
      For J := 0 To oC[I] - 1 Do
      Begin
        Result[H] := TPA[ind[I][J]];
        H := H + 1;
      End;
  // Voila!
End;

const
  flnC=545947;
  fsqrtA:single=0.5;

{$ASMMODE INTEL}
function fsqrt(x: Single): Single;
begin
  asm
    sub dword ptr x, ($3F800000-flnC)
    fild dword ptr x
    fmul fsqrtA
    fistp dword ptr x
    add dword ptr x,($3F800000-flnC)
    fld x
  end;
end;

procedure tSwap(var a, b: TPoint);
var
   c: TPoint;
begin
  c := a;
  a := b;
  b := c;
end;

procedure tpaSwap(var a, b: TPointArray);
var
   c: TPointArray;
begin
  c := a;
  a := b;
  b := c;
end;

procedure SwapE(var a, b: Extended);
var
   c: extended;
begin
  c := a;
  a := b;
  b := c;
end;

{/\
  Leaves one point per box with side lengths W and H to the TPA.
/\}

procedure RAaSTPAEx(var a: TPointArray; const w, h: Integer);
var
   i, c, NoTP, l: Integer;
   t: TPoint;
   b: boolean;

begin
  b:=false;
  NoTP := 0;
  l := High(a);
  for i := 0 to l do
  begin
    for c := 0 to NoTP - 1 do
      if (Abs(a[i].x - a[c].x) <= w) and (Abs(a[i].y - a[c].y) <= h) then
      begin
        b := true;
        Break;
      end;
    if not b then
    begin
      t := a[i];
      a[i] := a[NoTP];
      a[NoTP] := t;
      Inc(NoTP);
    end;
  end;
  SetLength(a, NoTP);
end;

{/\
  Leaves one point per box with the side length Dist.
/\}

procedure RAaSTPA(var a: TPointArray; const Dist: Integer);
var
   i, c, NoTP, l: Integer;
   t: TPoint;
   b: boolean;

begin
  b := false;
  NoTP := 0;
  l := High(a);
  for i := 0 to l do
  begin
    for c := 0 to NoTP - 1 do
      if (Round(fSqrt(Sqr(a[i].x - a[c].x) + Sqr(a[i].y - a[c].y))) <= Dist) then
      begin
        b:=true;
        Break;
      end;
    if (c >= NoTP) then
    begin
      t := a[i];
      a[i] := a[NoTP];
      a[NoTP] := t;
      Inc(NoTP);
    end;
  end;
  SetLength(a, NoTP);
end;

{/\
  Returns true if the point P is near a point in the TPA a with the max X and Y distances W and H.
/\}

function NearbyPointInArrayEx(const P: TPoint; w, h:Integer; a: TPointArray): Boolean;
var
  i, l: Integer;
begin
  Result := False;
  l := High(a);
  for i := 0 to l do
    if (Abs(P.x - a[i].x) <= w) and (Abs(P.y - a[i].y) <= h) then
    begin
      Result := True;
      Exit;
    end;
end;

{/\
  Returns true if the point P is near a point in the TPA a with the max distance Dist.
/\}

function NearbyPointInArray(const P: TPoint; Dist:Integer; a: TPointArray): Boolean;
var
  i, l: Integer;
begin
  Result := False;
  l := High(a);
  for i := 0 to l do
    if (Round(fSqrt(Sqr(P.x - a[i].x) + Sqr(P.y - a[i].y))) <= Dist) then
    begin
      Result := True;
      Exit;
    end;
end;

{/\
  Results the TPointArray a with one point per box with side lengths W and H left.
/\}

function ReArrangeandShortenArrayEx(a: TPointArray; w, h: Integer): TPointArray;
var
  i, t, c, l: Integer;
begin
  l := High(a);
  c := 0;
  SetLength(Result, l + 1);
  for i := 0 to l do
  begin
    for t := 0 to c -1 do
      if (Abs(Result[t].x - a[i].x) <= w) and (Abs(Result[t].y - a[i].y) <= h) then
        Break;
    if (t >= c) then
    begin
      Result[c] := a[i];
      Inc(c);
    end;
  end;
  SetLength(Result, c);
end;

{/\
  Results the TPointArray a with one point per box with side length Dist left.
/\}

function ReArrangeandShortenArray(a: TPointArray; Dist: Integer): TPointArray;
var
  i, t, c, l: Integer;
begin
  l := High(a);
  c := 0;
  SetLength(Result, l + 1);
  for i := 0 to l do
  begin
    for t := 0 to c -1 do
      if (Round(fSqrt(Sqr(Result[t].x - a[i].x) + Sqr(Result[t].y - a[i].y))) <= Dist) then
        Break;
    if (t >= c) then
    begin
      Result[c] := a[i];
      Inc(c);
    end;
  end;
  SetLength(Result, c);
end;

{/\
  Splits the TPA to boxes with sidelengths W and H and results them as a T2DPointArray.
/\}

function TPAtoATPAEx(TPA: TPointArray; w, h: Integer): T2DPointArray;
var
   a, b, c, l: LongInt;
begin
  SetLength(Result, 0);
  l := High(TPA);
  c := 0;
  for a := 0 to l do
  begin
    for b := 0 to c -1 do
      if (Abs(TPA[a].X - Result[b][0].X) <= w) and (Abs(TPA[a].Y - Result[b][0].Y) <= h) then
        Break;
    if (b < c) then
    begin
      SetLength(Result[b], Length(Result[b]) + 1);
      Result[b][High(Result[b])] := TPA[a];
    end else
    begin
      SetLength(Result, c + 1);
      SetLength(Result[c], 1);
      Result[c][0] := TPA[a];
      Inc(c);
    end;
  end;
end;

{/\
  Splits the TPA to boxes with sidelength Dist and results them as a T2DPointArray.
/\}

function TPAtoATPA(TPA: TPointArray; Dist: Integer): T2DPointArray;
var
   a, b, c, l: LongInt;
begin
  SetLength(Result, 0);
  l := High(tpa);
  c := 0;
  for a := 0 to l do
  begin
    for b := 0 to c -1 do
      if (Round(fSqrt(Sqr(TPA[a].X - Result[b][0].X) + Sqr(TPA[a].Y - Result[b][0].Y))) <= Dist) then
        Break;
    if (b < c) then
    begin
      SetLength(Result[b], Length(Result[b]) + 1);
      Result[b][High(Result[b])] := TPA[a];
    end else
    begin
      SetLength(Result, c + 1);
      SetLength(Result[c], 1);
      Result[c][0] := TPA[a];
      Inc(c);
    end;
  end;
end;

{/\
  Sorts the given TPointArray.
/\}

procedure QuickTPASort(var A: TIntegerArray; var B: TPointArray; iLo, iHi: Integer; SortUp: Boolean);
var
  Lo, Hi, Mid, T: Integer;
  TP: TPoint;
begin
  if (Length(A) <> Length(B)) then Exit;
  Lo := iLo;
  Hi := iHi;
  Mid := A[(Lo + Hi) shr 1];
  repeat
    if SortUp then
    begin
      while (A[Lo] < Mid) do Inc(Lo);
      while (A[Hi] > Mid) do Dec(Hi);
    end else
    begin
      while (A[Lo] > Mid) do Inc(Lo);
      while (A[Hi] < Mid) do Dec(Hi);
    end;
    if (Lo <= Hi) then
    begin
      T := A[Lo];
      A[Lo] := A[Hi];
      A[Hi] := T;
      TP := B[Lo];
      B[Lo] := B[Hi];
      B[Hi] := TP;
      Inc(Lo);
      Dec(Hi);
    end;
  until Lo > Hi;
  if (Hi > iLo) then QuickTPASort(A, B, iLo, Hi, SortUp);
  if (Lo < iHi) then QuickTPASort(A, B, Lo, iHi, SortUp);
end;

{/\
  Sorts the given T2DPointArray.
/\}

procedure QuickATPASort(var A: TIntegerArray; var B: T2DPointArray; iLo, iHi: Integer; SortUp: Boolean);
var
  Lo, Hi, Mid, T: Integer;
  TP: TPointArray;
begin
  if (Length(A) <> Length(B)) then Exit;
  Lo := iLo;
  Hi := iHi;
  Mid := A[(Lo + Hi) shr 1];
  repeat
    if SortUp then
    begin
      while (A[Lo] < Mid) do Inc(Lo);
      while (A[Hi] > Mid) do Dec(Hi);
    end else
    begin
      while (A[Lo] > Mid) do Inc(Lo);
      while (A[Hi] < Mid) do Dec(Hi);
    end;
    if (Lo <= Hi) then
    begin
      T := A[Lo];
      A[Lo] := A[Hi];
      A[Hi] := T;
      TP := B[Lo];
      B[Lo] := B[Hi];
      B[Hi] := TP;
      Inc(Lo);
      Dec(Hi);
    end;
  until Lo > Hi;
  if (Hi > iLo) then QuickATPASort(A, B, iLo, Hi, SortUp);
  if (Lo < iHi) then QuickATPASort(A, B, Lo, iHi, SortUp);
end;

{/\
  Sorts the TPointArray a from the point From.
  Closest one to the point is [0], second closest is [1] etc.
/\}

procedure SortTPAFrom(var a: TPointArray; const From: TPoint);
var
   i, l: Integer;
   DistArr: TIntegerArray;
begin
  l := High(a);
  if (l < 0) then Exit;
  SetLength(DistArr, l + 1);
  for i := 0 to l do
    DistArr[i] := Round(Sqr(From.x - a[i].x) + Sqr(From.y - a[i].y));
  QuickTPASort(DistArr, a, 0, l, True);
end;

{/\
  Sorts the T2DPointArray a from the point From.
/\}

procedure SortATPAFrom(var a: T2DPointArray; const From: TPoint);
var
   i, l: Integer;
begin
  l := High(a);
  if (l < 0) then Exit;
  for i := 0 to l do
    SortTPAFrom(a[i], From);
end;

{/\
  Sorts the T2DPointArray a from the point From.
/\}

procedure SortATPAFromFirstPoint(var a: T2DPointArray; const From: TPoint);
var
   i, l: Integer;
   DistArr: TIntegerArray;
begin
  writeln('hoi0');
  l := High(a);
  if (l < 0) then Exit;
  writeln('hoi1');
  SetLength(DistArr, l + 1);
  for i := 0 to l do
    DistArr[i] := Round(Sqr(From.x - a[i][0].x) + Sqr(From.y - a[i][0].y));
  writeln('hoi2');
  QuickATPASort(DistArr, a, 0, l, True);
  writeln('hoi3');
end;

{/\
  Inverts / Reverses the TPointArray a.
/\}

procedure InvertTPA(var a: TPointArray);
var
   i, l: Integer;
   b: tpointarray;
begin
  l := High(a);
  if (l < 0) then Exit;
  b := Copy(a);
  for i := l downto 0 Do
    a[l - i] := b[i];
end;

{/\
  Inverts / Reverts the T2DPointArray a.
/\}

procedure InvertATPA(var a: T2DPointArray);
var
   i, l: Integer;
   b: T2DPointArray;
begin
  l := High(a);
  if (l < 0) then Exit;
  b := Copy(a);
  for i := l downto 0 do
    a[l - i] := b[i];
end;

{/\
  Stores the coordinates of the middle of the TPointArray a to X and Y.
/\}

function MiddleTPAEx(TPA: TPointArray; var x, y: Integer): Boolean;
var
   i, l: Integer;
begin
  Result := False;
  l := High(tpa);
  if (l < 0) then Exit;
  x := 0;
  y := 0;
  for i := 0 to l do
  begin
    x := x + tpa[i].x;
    y := y + tpa[i].y;
  end;
  x := x div (l + 1);
  y := y div (l + 1);
  Result := True;
end;

{/\
  Returns the middle of the TPointArray tpa.
/\}

function MiddleTPA(tpa: TPointArray): TPoint;
var
   i, l: Integer;
begin
  l := High(tpa);
  if (l < 0) then Exit;
  Result.x := 0;
  Result.y := 0;
  for i := 0 to l do
  begin
    Result.x := Result.x + tpa[i].x;
    Result.y := Result.y + tpa[i].y;
  end;
  Result.x := Result.x div (l + 1);
  Result.y := Result.y div (l + 1);
end;

{/\
  Sorts the T2DPointArray a from either largest or smallest, by the amount of points in the TPAs.
/\}

procedure SortATPASize(var a: T2DPointArray; const BigFirst: Boolean);
var
   i, l: Integer;
   SizeArr: TIntegerArray;
begin
  l := High(a);
  if (l < 0) then Exit;
  SetLength(SizeArr, l + 1);
  for i := 0 to l do
    SizeArr[i] := Length(a[i]);
  QuickATPASort(SizeArr, a, 0, l, not BigFirst);
end;

{/\
  Combines the TPointArrays Ar1 and Ar2, and results the combination.
/\}

procedure SortATPAFromSize(var a: T2DPointArray; const Size: Integer; CloseFirst: Boolean);
var
   i, l: Integer;
   SizeArr: TIntegerArray;
begin
  l := High(a);
  if (l < 0) then Exit;
  SetLength(SizeArr, l + 1);
  for i := 0 to l do
    SizeArr[i] := Abs(Length(a[i]) - Size);
  QuickATPASort(SizeArr, a, 0, l, CloseFirst);
end;

{/\
  Combines the TPointArrays Ar1 and Ar2, and results the combination.
/\}

function CombineTPA(Ar1, Ar2: TPointArray): TPointArray;
var
  i, l1, l2: Integer;
begin
  Result := Copy(Ar1);
  l1 := Length(Result);
  l2 := Length(Ar2);
  SetLength(Result, l1 + l2);
  for i := 0 to l2 -1 do
    Result[i + l1] := Ar2[i];
end;

{/\
  Combines the TIntegerArrays Ar1 and Ar2, and results the combination.
/\}

function CombineIntArray(Ar1, Ar2: TIntegerArray): TIntegerArray;
var
  i, l1, l2: Integer;
begin
  Result := Copy(Ar1);
  l1 := Length(Result);
  l2 := Length(Ar2);
  SetLength(Result, l1 + l2);
  for i := 0 to l2 -1 do
    Result[i + l1] := Ar2[i];
end;

{/\
  Returns true if the integer Number was found in the integer array a, and stores the index to Where.
/\}

function InIntArrayEx(a: TIntegerArray; var Where: Integer; const Number: Integer): Boolean;
var
  i, l: Integer;
begin
  Result := False;
  l := High(a);
  for i := 0 to l do
    if (a[i] = Number) then
    begin
      Where := i;
      Result := True;
      Exit;
    end;
end;

{/\
  Returns true if the integer Number was found in the integer array a.
/\}

function InIntArray(a: TIntegerArray; Number: Integer): Boolean;
var
  i, l: Integer;
begin
  Result := False;
  l := High(a);
  for i := 0 to l do
    if (a[i] = Number) then
    begin
      Result := True;
      Exit;
    end;
end;

{/\
  Clears the duplicates in the integer array a.
/\}

procedure ClearSameIntegers(var a: TIntegerArray);
var
  i, t, c, l: Integer;
  b: TIntegerArray;
begin
  b := Copy(a);
  l := High(b);
  c := 0;
  for i := 0 to l do
  begin
    for t := 0 to c -1 do
      if (b[i] = a[t]) then
        Break;
    if (t >= c) then
    begin
      a[c] := b[i];
      Inc(c);
    end;
  end;
  SetLength(a, c);
end;

{/\
  Clears the duplicates in the integer array a and the TPointArray p.
/\}

procedure ClearSameIntegersAndTPA(var a: TIntegerArray; var p: TPointArray);
var
  i, t, c, l: Integer;
  b: TIntegerArray;
begin
  b := Copy(a);
  l := High(b);
  c := 0;
  for i := 0 to l do
  begin
    for t := 0 to c -1 do
      if (b[i] = a[t]) then
        Break;
    if (t >= c) then
    begin
      SetLength(a, c +1);
      a[c] := b[i];
      p[c] := p[i];
      Inc(c);
    end;
  end;
  SetLength(p, c);
  SetLength(a, c);
end;

{/\
  Splits the points with max X and Y distances W and H to their own TPointArrays.
/\}

function SplitTPAEx(arr: TPointArray; w, h: Integer): T2DPointArray;
var
  t1, t2, c, ec, tc, l: Integer;
  tpa: TPointArray;
begin
  tpa := Copy(arr);
  l := High(tpa);
  if (l < 0) then Exit;
  SetLength(Result, l + 1);
  c := 0;
  ec := 0;
  while ((l - ec) >= 0) do
  begin
    SetLength(Result[c], 1);
    Result[c][0] := tpa[0];
    tpa[0] := tpa[l - ec];
    Inc(ec);
    tc := 1;
    t1 := 0;
    while (t1 < tc) do
    begin
      t2 := 0;
      while (t2 <= (l - ec)) do
      begin
        if (Abs(Result[c][t1].x - tpa[t2].x) <= w) and (Abs(Result[c][t1].y - tpa[t2].y) <= h) then
        begin
          SetLength(Result[c], tc +1);
          Result[c][tc] := tpa[t2];
          tpa[t2] := tpa[l - ec];
          Inc(ec);
          Inc(tc);
          Dec(t2);
        end;
        Inc(t2);
      end;
      Inc(t1);
    end;
    Inc(c);
  end;
  SetLength(Result, c);
end;

{/\
  Splits the points with max distance Dist to their own TPointArrays.
  Dist 1 puts the points that are next to eachother to their own arrays.
/\}

function SplitTPA(arr: TPointArray; Dist: Integer): T2DPointArray;
var
  t1, t2, c, ec, tc, l: Integer;
  tpa: TPointArray;
begin
  tpa := Copy(arr);
  l := High(tpa);
  if (l < 0) then Exit;
  SetLength(Result, l + 1);
  c := 0;
  ec := 0;
  while ((l - ec) >= 0) do
  begin
    SetLength(Result[c], 1);
    Result[c][0] := tpa[0];
    tpa[0] := tpa[l - ec];
    Inc(ec);
    tc := 1;
    t1 := 0;
    while (t1 < tc) do
    begin
      t2 := 0;
      while (t2 <= (l - ec)) do
      begin
        if (Round(fSqrt(Sqr(Result[c][t1].x - tpa[t2].x) + Sqr(Result[c][t1].y - tpa[t2].y))) <= Dist) then
        begin
          SetLength(Result[c], tc +1);
          Result[c][tc] := tpa[t2];
          tpa[t2] := tpa[l - ec];
          Inc(ec);
          Inc(tc);
          Dec(t2);
        end;
        Inc(t2);
      end;
      Inc(t1);
    end;
    Inc(c);
  end;
  SetLength(Result, c);
end;

{/\
  Removes the points in the TPointArray Points that are not within the degrees
  \\ SD (StartDegree) and ED (EndDegree) and the distances MinR (MinRadius) and
  \\ MaxR (MaxRadius) from the origin (Mx, My).
/\}

procedure FilterPointsPie(var Points: TPointArray; const SD, ED, MinR, MaxR: Extended; Mx, My: Integer);
const
  i180Pi = 57.29577951;
var
   G: TPointArray;
   I, L, T: Integer;
   D, StartD, EndD: Extended;
   cWise: Boolean;
begin
  T := High(Points);
  if (T < 0) then Exit;
  SetLength(G, T + 1);
  L := 0;
  StartD := SD;
  EndD := ED;
  cWise := StartD > EndD;
  while StartD > 360.0 do
    StartD := StartD - 360.0;
  while EndD > 360.0 do
    EndD := EndD - 360.0;
  while StartD < 0.0 do
    StartD := StartD + 360.0;
  while EndD < 0.0 do
    EndD := EndD + 360.0;
  if StartD > EndD then
    SwapE(StartD, EndD);
  for I := 0 to T do
  begin
    D := fSqrt(Sqr(Points[I].X - Mx) + Sqr(Points[I].Y - My));
    if( D <= MinR) or (D >= MaxR) then
      Continue;
    D := (ArcTan2(Points[I].Y - My, Points[I].X - Mx) * i180Pi) + 90;
    if D < 0.0 then
      D := D + 360.0;
    if (not ((StartD <= D) and (EndD >= D))) xor CWise then
      Continue;
    G[L] := Points[I];
    Inc(L);
  end;
  SetLength(G, L);
  Points := G;
end;

{/\
  Removes the points that are inside or outside the distance Dist from the point (x, y) from the TPointArray ThePoints.
/\}

function RemoveDistTPointArray(x, y, dist: Integer; ThePoints: TPointArray; RemoveHigher: Boolean): TPointArray;
var
  I, L, LL: integer;
begin;
  L := 0;
  LL := Length(ThePoints) -1;
  SetLength(Result, LL + 1);
  if RemoveHigher then
  begin;
    for I := 0 to LL do
      if not (Round(fSqrt(sqr(ThePoints[i].x - x)+sqr(ThePoints[i].y - y))) > Dist) then
      begin;
        Result[L] := ThePoints[i];
        L := L + 1;
      end;
  end else
  begin;
    for I := 0 to LL do
      if not (Round(fSqrt(sqr(ThePoints[i].x - x)+sqr(ThePoints[i].y - y))) < Dist) then
      begin;
        Result[L] := ThePoints[i];
        L := L + 1;
      end;
  end;
  SetLength(Result,L);
end;

{/\
  Returns the boundaries of the ATPA as a TBox.
/\}

function GetATPABounds(ATPA: T2DPointArray): TBox;
var
  I,II,L2,L : Integer;
begin;
  L := High(ATPA);
  if (l < 0) then Exit;
  For I := 0 to L do
    if Length(ATPA[I]) > 0 then
    begin;
      Result.x1 := ATPA[I][0].x;
      Result.y1 := ATPA[I][0].y;
      Result.x2 := ATPA[I][0].x;
      Result.y2 := ATPA[I][0].y;
    end;
  for I := 0 to L do
  begin;
    L2 := High(ATPA[I]);
    for II := 0 to L2 do
    begin;
      if ATPA[i][II].x > Result.x2 then
        Result.x2 := ATPA[i][II].x
      else if ATPA[i][II].x < Result.x1 then
        Result.x1 := ATPA[i][II].x;
      if ATPA[i][II].y > Result.y2 then
        Result.y2 := ATPA[i][II].y
      else if ATPA[i][II].y < Result.y1 then
        Result.y1 := ATPA[i][II].y;
    end;
  end;
end;

{/\
  Returns the boundaries of the TPA as a TBox.
/\}

function GetTPABounds(TPA: TPointArray): TBox;
var
  I,L : Integer;
begin;
  L := High(TPA);
  if (l < 0) then Exit;
  Result.x1 := TPA[0].x;
  Result.y1 := TPA[0].y;
  Result.x2 := TPA[0].x;
  Result.y2 := TPA[0].y;
  for I:= 1 to L do
  begin;
    if TPA[i].x > Result.x2 then
      Result.x2 := TPA[i].x
    else if TPA[i].x < Result.x1 then
      Result.x1 := TPA[i].x;
    if TPA[i].y > Result.y2 then
      Result.y2 := TPA[i].y
    else if TPA[i].y < Result.y1 then
      Result.y1 := TPA[i].y;
  end;
end;

{/\
  Looks for the TPA SearchTPA in the TPA TotalTPA and returns the matched points
  \\ to the TPA Matches. Returns true if there were atleast one match(es).
/\}

function FindTPAinTPA(SearchTPA, TotalTPA: TPointArray; var Matches: TPointArray): Boolean;
var
  Len, I,II,LenSearch,xOff,yOff : integer;
  tx,ty,MatchCount : integer;
  Screen : Array of Array of Boolean;
  ScreenBox,SearchBox : TBox;
begin;
  Result := False;
  Len := High(TotalTPA);
  LenSearch := High(SearchTPA);
  if LenSearch < 0 then Exit;
  if Len < LenSearch then Exit;
  MatchCount := 0;
  ScreenBox := GetTPABounds(TotalTPA);
  SearchBox := GetTPABounds(SearchTPA);
  try
    SetLength(Screen,ScreenBox.x2 + 1,ScreenBox.y2 + 1);
  except
    Exit;
  end;
  if (SearchBox.x1 > 0) or (SearchBox.y1 > 0) then
  begin;
    for I := 0 to LenSearch do
    begin;
      SearchTPA[I].x := SearchTPA[I].x - SearchBox.x1;
      SearchTPA[I].y := SearchTPA[I].y - SearchBox.y1;
    end;
    SearchBox.x2 := SearchBox.x2 - SearchBox.x1;
    SearchBox.y2 := SearchBox.y2 - SearchBox.y1;
    SearchBox.x1 := 0;
    SearchBox.y1 := 0;
  end;
  xOff := SearchBox.x2;
  yOff := SearchBox.y2;
  for I := 0 to LenSearch do
  begin;
    if (SearchTPA[I].x = 0) and (SearchTPA[I].y < yOff) then
        yOff := SearchTPA[I].y;
    if (SearchTPA[I].y = 0) and (SearchTPA[I].x < xOff) then
        xOff := SearchTPA[I].x;
  end;
  for I := 0 to Len do
    Screen[TotalTPA[I].x][TotalTPA[I].y] := True;
  for I := 0 to Len do
  begin;
    tx := TotalTPA[I].x - xOff;
    ty := TotalTPA[I].y;// - yOff;
    if tx > 0 then
      if ty > 0 then
        if ((SearchBox.x2 + tx) <= ScreenBox.x2) and ((SearchBox.y2 + ty) <= ScreenBox.y2) then
          begin;
            For II := 0 to LenSearch do
              if Screen[tx + SearchTPA[II].x ][ty + SearchTPA[II].y] = False then
                Break;
            if II > LenSearch then
            begin;
              MatchCount := MatchCount + 1;
              SetLength(Matches,MatchCount);
              Matches[MatchCount - 1].x := TotalTPA[I].x;
              Matches[MatchCount - 1].y := TotalTPA[I].y;
            end;
          end;
  end;
  if (MatchCount > 0) then
    Result := True;
end;

{/\
  Read the description of FindTPAinTPA. Additional Height parameter.
/\}

function FindTextTPAinTPA(Height : integer; SearchTPA, TotalTPA: TPointArray; var Matches: TPointArray): Boolean;
var
  Len, I,II,LenSearch,LenTPA,xOff,yOff,x,y: integer;
  tx,ty,MatchCount : integer;
  Screen : Array of Array of Boolean;
  ScreenBox,SearchBox : TBox;
  InversedTPA : TPointArray;
begin;
  Result := False;
  Len := High(TotalTPA);
  LenSearch := High(SearchTPA);
  if LenSearch < 0 then Exit;
  if Len < LenSearch then Exit;
  MatchCount := 0;
  xOff := -5;
  yOff := 0;
  ScreenBox := GetTPABounds(TotalTPA);
  SearchBox := GetTPABounds(SearchTPA);
  if height > SearchBox.y2 then
    Screenbox.y2 := Screenbox.y2 + (height - SearchBox.y2);
  SearchBox.y2 := Height;
  SetLength(Screen, SearchBox.x2 + 1,Searchbox.y2 + 1);
  SetLength(InversedTPA,(SearchBox.x2 + 1) * (Searchbox.y2 + 1));
  for I := 0 to LenSearch do
    Screen[ SearchTPA[I].x,SearchTPA[I].y] := True;
  LenTPA := -1;
  for y := 0 to SearchBox.y2 do
    for x := 0 to SearchBox.x2 do
      if Screen[X][Y] = False then
      begin;
        LenTPA := LenTPA + 1;
        InversedTPA[LenTPA].x := x;
        InversedTPA[LenTPA].y := y;
      end;
  for x := 0 to SearchBox.x2 do
  begin;
    for y := 0 to SearchBox.y2 do
      if Screen[x][y] = True then
      begin;
        xOff := x;
        yOff := y;
        Break;
      end;
    if xOff >= 0 then
      Break;
  end;
  try
    SetLength(Screen,0);
    SetLength(Screen,ScreenBox.x2 + 1,ScreenBox.y2 + 1);
  except
    Exit;
  end;
  for I := 0 to Len do
    Screen[TotalTPA[I].x][TotalTPA[I].y] := True;
  for I := 0 to Len do
  begin;
    tx := TotalTPA[I].x - xOff;
    ty := TotalTPA[I].y - yOff;
    if tx > 0 then
      if ty > 0 then
        if ((SearchBox.x2 + tx) <= ScreenBox.x2) and ((SearchBox.y2 + ty) <= ScreenBox.y2) then
          begin;
            For II := 0 to LenSearch do
              if Screen[tx + SearchTPA[II].x ][ty + SearchTPA[II].y] = False then
                Break;
            if (II > LenSearch) then
            begin;
              For II := 0 to LenTPA do
                if Screen[tx + InversedTPA[II].x ][ty + InversedTPA[II].y] = True then
                  Break;
              if (II > LenTPA) then
              begin;
                MatchCount := MatchCount + 1;
                SetLength(Matches,MatchCount);
                Matches[MatchCount - 1].x := TotalTPA[I].x;
                Matches[MatchCount - 1].y := TotalTPA[I].y;
              end;
            end;
          end;
  end;
  if (MatchCount > 0) then
    Result := True;
end;

{/\
  Finds the possible gaps in the TPointArray TPA and results the gaps as a T2DPointArray.
  \\ Considers as a gap if the gap length is >= MinPixels.
  \\ Only horizontal, sorry folks.
/\}

function FindGapsTPA(TPA: TPointArray; MinPixels: Integer): T2DPointArray;
var
  Len,TotalLen,LenRes,I,II,III : integer;
  Screen : Array of Array of Boolean;
  Height,Width : Integer;
  Box : TBox;
begin;
  Len := High(TPA);
  if Len < 0 then exit;
  Box := GetTPABounds(TPA);
  Height := Box.y2 - Box.y1;
  Width := Box.x2 - Box.x1;
  LenRes := 0;
  III := 0;
  try
    SetLength(Screen,Width + 1,Height + 1);
  except
    Exit;
  end;
  For I := 0 to Len do
    Screen[TPA[I].x - Box.x1][TPA[I].y - Box.y1] := True;
  SetLength(result,1);
  SetLength(Result[0],Len+1);
  TotalLen := 0;
  for I := 0 to Width do
  begin;
    for II := 0 to Height do
      if Screen[I][II]=True then
      begin;
        Result[TotalLen][LenRes].x := I + Box.x1;
        Result[TotalLen][LenRes].y := II + Box.y1;
        LenRes := LenRes + 1;
        III := I;
      end;
    if LenRes = 0 then
      III := I
    else
    if (I - III) > MinPixels then
    begin;
      III := I;
      SetLength(Result[TotalLen],LenRes);
      LenRes := 0;
      TotalLen := TotalLen + 1;
      SetLength(Result,TotalLen + 1);
      SetLength(Result[TotalLen],Len + 1);
    end;
  end;
  SetLength(Result[TotalLen],LenRes);
end;



{/\
  Sorts all points in tpa by distance from degree (Deg) and distance from mx and my.
  \\ Sortup will return closest distance to mx and my first.
  \\ Distance will be sorted first (RadialWalk style).
/\}

procedure SortCircleWise(var tpa: TPointArray; const cx, cy, StartDegree: Integer; SortUp, ClockWise: Boolean);
const
  i180Pi = 57.29577951;
var
  i, l, td, sd: Integer;
  Dist, Deg: TIntegerArray;
begin
  l := Length(tpa);
  if (l = 0) then Exit;
  sd := StartDegree;
  while (sd > 360) do
    sd := (sd - 360);
  sd := (360 - sd);
  SetLength(Dist, l);
  SetLength(Deg, l);
  for i := 0 to l -1 do
    Dist[i] := Round(Hypot(tpa[i].x - cx, tpa[i].y - cy));
  QuickTPASort(Dist, tpa, 0, l -1, SortUp);
  if (l = 1) then Exit;
  for i := 0 to l -1 do
  begin
    td := Round(ArcTan2(tpa[i].y - cy, tpa[i].x - cx) * i180Pi) + 90;
    if (td < 0) then
      td := td + 360;
    if ClockWise then
      Deg[i] := Round(sd + td) mod 360
    else
      Deg[i] := 360 - Round(sd + td) mod 360;
  end;
  i := 1;
  td := 0;
  Dec(l);
  while (i < l) do
  begin
    while (i < l) and (Abs(Dist[td] - Dist[i]) <= 1) do Inc(i);
    QuickTPASort(Deg, tpa, td, i, False);
    Inc(i);
    td := i;
  end;
  if (td < l) then
    QuickTPASort(Deg, tpa, td, l, False);
end;

{/\
  Sorts all points in tpa by distance from degree (Deg) and distance from mx and my.
  \\ Sortup will return closest distance to mx and my first.
  \\ Degree will be sorted first (LinearWalk style).
/\}

procedure LinearSort(var tpa: TPointArray; cx, cy, sd: Integer; SortUp: Boolean);
const
  i180Pi = 57.29577951;
var
  i, l, td: Integer;
  Dist, Deg: TIntegerArray;
begin
  l := Length(tpa);
  if (l = 0) then Exit;
  while (sd > 360) do
    sd := (sd - 360);
  SetLength(Dist, l);
  SetLength(Deg, l);
  for i := 0 to l -1 do
  begin
    td := Round(ArcTan2(tpa[i].y - cy, tpa[i].x - cx) * i180Pi) + 90;
    if (td < 0) then
      td := td + 360;
    Deg[i] := Min(Abs(sd - td), Min(Abs(sd - (td + 360)), Abs((sd + 360) - td)))
  end;
  QuickTPASort(Deg, tpa, 0, l -1, True);
  if (l = 1) then Exit;
  for i := 0 to l -1 do
    Dist[i] := Round(Hypot(tpa[i].x - cx, tpa[i].y - cy));
  i := 1;
  td := 0;
  Dec(l);
  while (i < l) do
  begin
    while (i < l) and (Abs(Deg[td] - Deg[i]) <= 3) do Inc(i);
    QuickTPASort(Dist, tpa, td, i, SortUp);
    Inc(i);
    td := i;
  end;
  if (td < l) then
    QuickTPASort(Dist, tpa, td, l, SortUp);
end;

{/\
  Merges the TPointArrays of the T2DPointArray ATPA in to one TPA.
/\}

Function MergeATPA(ATPA : T2DPointArray)  : TPointArray;
var
  I,II, Len, TempL,CurrentL : integer;
begin;
  Len := High(ATPA);
  CurrentL := 0;
  For I:= 0 to Len do
  begin;
    TempL := High(ATPA[I]);
    if TempL < 0 then
      Continue;
    TempL := Templ + CurrentL + 1;
    Setlength(Result, TempL+1);
    For II := CurrentL to TempL do
      Result[II] := ATPA[I][II - CurrentL];
    CurrentL := TempL;
  end;
  SetLength(Result,CurrentL);
end;


{/\
  Returns a TPointArray of a the full given Box.
/\}

function TPAFromBox(const Box : TBox) : TPointArray;
var
  x, y: integer;
  l : integer;
begin;
  SetLength(Result, (Box.x2 - Box.x1 + 1) * (Box.y2 - Box.y1 + 1));
  l := 0;
  For x := box.x1 to Box.x2 do
    for y := box.y1 to box.y2 do
    begin;
      Result[l].x := x;
      Result[l].y := y;
      inc(l);
    end;
end;

{/\
  Rotate the given TPA with A radians.
/\}

Function RotatePoints(Const P: TPointArray; A, cx, cy: Extended): TPointArray ;

Var
   I, L: Integer;

Begin
  L := High(P);
  SetLength(Result, L + 1);
  For I := 0 To L Do
  Begin
    Result[I].X := Trunc(cx + cos(A) * (p[i].x - cx) - sin(A) * (p[i].y - cy));
    Result[I].Y := Trunc(cy + sin(A) * (p[i].x - cx) + cos(A) * (p[i].y - cy));
  End;
  // I recon its faster than Point().
End;

{/\
  Rotate the given Point with A radians.
/\}

Function RotatePoint(Const p: TPoint; angle, mx, my: Extended): TPoint; inline;

Begin
  Result.X := Trunc(mx + cos(angle) * (p.x - mx) - sin(angle) * (p.y - my));
  Result.Y := Trunc(my + sin(angle) * (p.x - mx) + cos(angle) * (p.y- my));
End;

{/\
  Returns the edges of the given TPA.
/\}

function FindTPAEdges(p: TPointArray): TPointArray;
var
  b: array of array of Boolean;
  i, x, y, l, c: Integer;
  Box: TBox;
begin
  SetLength(Result, 0);
  l := Length(p);
  if (l = 0) then Exit;
  Box := GetTPABounds(p);
  x := (Box.x2 - Box.x1) + 3;
  y := (Box.y2 - Box.y1) + 3;
  SetLength(b, x);
  for i := 0 to x -1 do
    SetLength(b[i], y);
  for i := 0 to l -1 do
    b[p[i].x +1 - Box.x1][p[i].y +1 - Box.y1] := True;
  SetLength(Result, l);
  c := 0;
  for i := 0 to l -1 do
  begin
    x := -1;
    while (x <= 1) do
    begin
      for y := -1 to 1 do
        try
          if not b[p[i].x + 1 + x - Box.x1][p[i].y + 1 + y - Box.y1] then
          begin
            Result[c] := p[i];
            Inc(c);
            x := 2;
            Break;
          end;
        except end;
      Inc(x);
    end;
  end;
  SetLength(Result, c);
end;

{/\
  Results true if a point is in a TPointArray.
  Notes: In actuallys means IN the array, not in the box shaped by the array.
/\}

function PointInTPA(p: TPoint; arP: TPointArray): Boolean;
var
  i, l: Integer;
begin
  l := High(arP);
  for i := 0 to l do
    if (arP[i].x = p.x) and (arP[i].y = p.y) then
      Break;
  Result := i <> Length(arP);
end;

{/\
  Removes the given ClearPoints from arP.
/\}

function ClearTPAFromTPA(arP, ClearPoints: TPointArray): TPointArray;
var
  i, j, l, l2: Integer;
begin
  l := High(arP);
  l2 := High(ClearPoints);
  for i := 0 to l do
  begin
    for j := 0 to l2 do
      if (arP[i].x = ClearPoints[j].x) and (arP[i].y = ClearPoints[j].y) then
        Break;

    if (j = l2 + 1) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := arP[i];
    end;
  end;
end;

{/\
  Removes all the doubles point from a TPA.
/\}

procedure ClearDoubleTPA(var TPA: TPointArray);
var
   I, II, L: Integer;
   Swappie: TPoint;
begin
  L := High(TPA);
  for I := 0 To L Do
    for II := I + 1 To L Do
      if ((TPA[I].X = TPA[II].X) And (TPA[I].Y = TPA[II].Y)) then
      begin
        Swappie := TPA[L];
        TPA[L] := TPA[II];
        TPA[II] := Swappie;
        L := L - 1;
      end;
  SetLength(TPA, L + 1);
end;

{/\
  Uses Box to define an area around TotalTPA.
  Every point that is not in TotalTPA, but is in Box, is added to the Result.
  \\ This can be very handy if you want for example, can get all the colors of the background, but not of the actual object.
  \\ If you pass this all the colors of the background, it will returns the points of the object.
/\}

Function ReturnPointsNotInTPA(Const TotalTPA: TPointArray; Box: TBox): TPointArray;

Var
   X, Y, I: Integer;
   B: Array Of Array Of Boolean;

Begin
  SetLength(Result, ((Box.X2 - Box.X1 + 1) * (Box.Y2 - Box.Y1 + 1)) - Length(TotalTPA));

  SetLength(B, Box.X2 - Box.X1 + 1, Box.Y2 - Box.Y1 + 1);

  For I := 0 To High(TotalTPA) Do
    B[TotalTPA[I].X - Box.X1][TotalTPA[I].Y - Box.Y1] := True;

  I := 0;
  For X := 0 To Box.X2 - Box.X1 Do
    For Y := 0 To Box.Y2 - Box.Y1 Do
      If Not B[X][Y] Then
      Begin
          Result[I].X := X + Box.X1;
          Result[I].Y := Y + Box.Y1;
        I := I + 1;
      End;
  SetLength(B, 0);
End;

{/\
  Sorts a TPointArray by either X or Y. You have to define the max Point as well.
/\}

Procedure TPACountSort(Var TPA: TPointArray;const max: TPoint;Const SortOnX : Boolean);
Var
   c: Array Of Array Of Integer;
   I, II, III, hTPA, cc: Integer;
Begin
  hTPA := High(TPA);
  if hTPA < 1 then
    Exit;
  SetLength(c, max.X + 1,max.Y + 1);
  For I := 0 To hTPA Do
    c[TPA[I].x][TPA[I].y] := c[TPA[i].x][TPA[i].y] + 1;

  cc := 0;
  if SortOnX then
  begin
    For I := 0 To max.X  Do
      For II := 0 To max.Y  Do
      Begin
        For III := 0 To c[I][II] - 1 Do
        Begin
          TPA[cc].x := I;
          TPA[cc].y := II;
          cc := cc + 1;
        End;
      End;
  end else
  begin;
    For II := 0 To max.Y  Do
      For I := 0 To max.X  Do
      Begin
        For III := 0 To c[I][II] - 1 Do
        Begin
          TPA[cc].x := I;
          TPA[cc].y := II;
          cc := cc + 1;
        End;
      End;
  end;
End;

{/\
  Sorts a TPointArray by either X or Y. Allows one to pass a Base.
/\}

Procedure TPACountSortBase(Var TPA: TPointArray;const maxx, base: TPoint; const SortOnX : Boolean);
Var
   c: Array Of Array Of Integer;
   I, II, III, hTPA, cc: Integer;
   Max : TPoint;
Begin
  hTPA := High(TPA);
  if hTPA < 1 then
    Exit;
  max.X := maxx.X - base.X;
  max.Y := maxx.Y - base.Y;
  SetLength(c, max.X + 1,max.Y + 1);
  hTPA := High(TPA);
  For I := 0 To hTPA Do
    c[TPA[I].x - base.X][TPA[I].y - base.Y] := c[TPA[i].x- base.X][TPA[i].y- base.Y] + 1;

  cc := 0;
  if SortOnX then
  begin
    For I := 0 To max.X  Do
      For II := 0 To max.Y  Do
      Begin
        For III := 0 To c[I][II] - 1 Do
        Begin
          TPA[cc].x := I + base.X;
          TPA[cc].y := II + base.Y;
          cc := cc + 1;
        End;
      End;
  end else
  begin;
    For II := 0 To max.Y  Do
      For I := 0 To max.X  Do
      Begin
        For III := 0 To c[I][II] - 1 Do
        Begin
          TPA[cc].x := I + base.X;
          TPA[cc].y := II + base.Y;
          cc := cc + 1;
        End;
      End;
  end;
End;

end.

