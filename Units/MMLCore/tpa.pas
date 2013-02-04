{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009-2012 by Raymond van Venetië and Merlijn Wajer

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

function FastTPASort(const TPA: TPointArray;const Dists: TIntegerArray; maxDist: Integer; CloseFirst: Boolean): TPointArray;
procedure QuickSort(var A: TIntegerArray; iLo, iHi: Integer);

//Start Wizzyplugin
procedure tSwap(var a, b: TPoint);
procedure tpaSwap(var a, b: TPointArray);
procedure SwapE(var a, b: Extended);
procedure RAaSTPAEx(var a: TPointArray; const w, h: Integer);
procedure RAaSTPA(var a: TPointArray; const Dist: Integer);
function NearbyPointInArrayEx(const P: TPoint; w, h:Integer; a: TPointArray): Boolean;
function NearbyPointInArray(const P: TPoint; Dist:Integer; a: TPointArray): Boolean;
function ReArrangeandShortenArrayEx(const a: TPointArray; w, h: Integer): TPointArray;
function ReArrangeandShortenArray(const a: TPointArray; Dist: Integer): TPointArray;
function TPAtoATPAEx(const TPA: TPointArray; w, h: Integer): T2DPointArray;
function TPAtoATPA(const TPA: TPointArray; Dist: Integer): T2DPointArray;
procedure QuickTPASort(var A: TIntegerArray; var B: TPointArray; iLo, iHi: Integer; SortUp: Boolean);
procedure QuickATPASort(var A: TIntegerArray; var B: T2DPointArray; iLo, iHi: Integer; SortUp: Boolean);
procedure SortTPAByX(var a: TPointArray; const LowToHi: Boolean);
procedure SortTPAByY(var a: TPointArray; const LowToHi: Boolean);
function FindTPARows(a: TPointArray): T2DPointArray;
function FindTPAColumns(a: TPointArray): T2DPointArray;
procedure SortTPAFrom(var a: TPointArray; const From: TPoint);
procedure SortATPAFrom(var a: T2DPointArray; const From: TPoint);
procedure SortATPAFromFirstPoint(var a: T2DPointArray; const From: TPoint);
procedure SortATPAFromMidPoint(var a: T2DPointArray; const From: TPoint);
procedure InvertTPA(var a: TPointArray);
procedure InvertATPA(var a: T2DPointArray);
function MiddleTPAEx(const TPA: TPointArray; var x, y: Integer): Boolean;
function MiddleTPA(const tpa: TPointArray): TPoint;
procedure SortATPASize(var a: T2DPointArray; const BigFirst: Boolean);
procedure SortATPAFromSize(var a: T2DPointArray; const Size: Integer; CloseFirst: Boolean);
function CombineTPA(const Ar1, Ar2: TPointArray): TPointArray;
function CombineIntArray(const Ar1, Ar2: TIntegerArray): TIntegerArray;
function InIntArrayEx(const a: TIntegerArray; var Where: Integer; const Number: Integer): Boolean;
function InIntArray(const a: TIntegerArray; Number: Integer): Boolean;
procedure ClearSameIntegers(var a: TIntegerArray);
procedure ClearSameIntegersAndTPA(var a: TIntegerArray; var p: TPointArray);
function SplitTPAEx(const arr: TPointArray; w, h: Integer): T2DPointArray;
function SplitTPA(const arr: TPointArray; Dist: Integer): T2DPointArray;
function  TPAPosNext(const Find: TPoint; const V: TPointArray; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer;
function GlueTPAs(const V1, V2: TPointArray; const IsSortedAscending,byDifference: Boolean):TPointArray;
function FloodFillTPA(const TPA : TPointArray) : T2DPointArray;
procedure FilterPointsPie(var Points: TPointArray; const SD, ED, MinR, MaxR: Extended; Mx, My: Integer);
procedure FilterPointsDist(var Points: TPointArray; const MinDist,MaxDist: Extended; Mx, My: Integer);
procedure FilterPointsLine(var Points: TPointArray; Radial: Extended; Radius, MX, MY: Integer);
procedure FilterTPADist(var TPA: TPointArray; maxDist: integer);
function RemoveDistTPointArray(x, y, dist: Integer;const ThePoints: TPointArray; RemoveHigher: Boolean): TPointArray;
function GetATPABounds(const ATPA: T2DPointArray): TBox;
function GetTPABounds(const TPA: TPointArray): TBox;
function FindTPAinTPA(SearchTPA: TPointArray; const TotalTPA: TPointArray; var Matches: TPointArray): Boolean;
function FindTextTPAinTPA(Height : integer;const SearchTPA, TotalTPA: TPointArray; var Matches: TPointArray): Boolean;
function GetSamePointsATPA(const ATPA : T2DPointArray; var Matches : TPointArray) : boolean;
function FindGapsTPA(const TPA: TPointArray; MinPixels: Integer): T2DPointArray;
procedure SortCircleWise(var tpa: TPointArray; const cx, cy, StartDegree: Integer; SortUp, ClockWise: Boolean);
procedure LinearSort(var tpa: TPointArray; cx, cy, sd: Integer; SortUp: Boolean);
function MergeATPA(const ATPA : T2DPointArray)  : TPointArray;
procedure AppendTPA(var TPA : TPointArray; const ToAppend : TPointArray);
function TPAFromLine(const x1, y1, x2, y2: Integer): TPointArray;
function EdgeFromBox(const Box: TBox): TPointArray;
function TPAFromBox(const Box : TBox) : TPointArray;
function TPAFromEllipse(const CX, CY, XRadius, YRadius : Integer): TPointArray;
function TPAFromCircle(const CX, CY, Radius: Integer): TPointArray;
procedure FillEllipse(var a: TPointArray);
function FindTPAEdges(const p: TPointArray): TPointArray;
function PointInTPA(const p: TPoint;const arP: TPointArray): Boolean;
function ClearTPAFromTPA(const arP, ClearPoints: TPointArray): TPointArray;
procedure ClearDoubleTPA(var TPA: TPointArray);
Function ReturnPointsNotInTPA(Const TotalTPA: TPointArray; const Box: TBox): TPointArray;
Procedure TPACountSort(Var TPA: TPointArray;const max: TPoint;Const SortOnX : Boolean);
Procedure TPACountSortBase(Var TPA: TPointArray;const maxx, base: TPoint; const SortOnX : Boolean);
procedure InvertTIA(var tI: TIntegerArray);
function SumIntegerArray(const Ints : TIntegerArray): Integer;
function AverageTIA(const tI: TIntegerArray): Integer;
function AverageExtended(const tE: TExtendedArray): Extended;
function SameTPA(const aTPA, bTPA: TPointArray): Boolean;
function TPAInATPA(const TPA: TPointArray;const InATPA: T2DPointArray; var Index: LongInt): Boolean;
procedure OffsetTPA(var TPA : TPointArray; const Offset : TPoint);
procedure OffsetATPA(var ATPA : T2DPointArray; const Offset : TPoint);
function MiddleBox(b : TBox) : TPoint;

implementation

uses
  math;




{/\
   Very Fast TPA Sort, uses an adepted CountSort algorithm.
/\}

Function FastTPASort(const TPA: TPointArray;const Dists: TIntegerArray; maxDist: Integer; CloseFirst: Boolean): TPointArray;

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

procedure QuickSort(var A: TIntegerArray; iLo, iHi: Integer) ;
var
  Lo, Hi, Pivot, T: Integer;
begin
  Lo := iLo;
  Hi := iHi;
  Pivot := A[(Lo + Hi) div 2];
  repeat
    while A[Lo] < Pivot do Inc(Lo) ;
    while A[Hi] > Pivot do Dec(Hi) ;
    if Lo <= Hi then
    begin
      T := A[Lo];
      A[Lo] := A[Hi];
      A[Hi] := T;
      Inc(Lo) ;
      Dec(Hi) ;
    end;
  until Lo > Hi;
  if Hi > iLo then QuickSort(A, iLo, Hi) ;
  if Lo < iHi then QuickSort(A, Lo, iHi) ;
end;

{const
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
end;   }


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
   Found : boolean;
begin
  NoTP := 0;
  l := High(a);
  for i := 0 to l do
  begin
    Found := false;
    for c := 0 to NoTP - 1 do
      if (Abs(a[i].x - a[c].x) <= w) and (Abs(a[i].y - a[c].y) <= h) then
      begin
        Found := true;
        Break;
      end;
    if not Found then
//    if (c >= NoTP) then
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
   Found : boolean;
begin
  NoTP := 0;
  l := High(a);
  for i := 0 to l do
  begin
    Found := false;
    for c := 0 to NoTP - 1 do
      if (Round(sqrt(Sqr(a[i].x - a[c].x) + Sqr(a[i].y - a[c].y))) <= Dist) then
      begin
        Found := True;
        Break;
      end;
    if not Found then
//    if (c >= NoTP) then
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
    if (Round(sqrt(Sqr(P.x - a[i].x) + Sqr(P.y - a[i].y))) <= Dist) then
    begin
      Result := True;
      Exit;
    end;
end;

{/\
  Results the TPointArray a with one point per box with side lengths W and H left.
/\}

function ReArrangeandShortenArrayEx(const a: TPointArray; w, h: Integer): TPointArray;
var
  i, t, c, l: Integer;
  Found: Boolean;
begin
  l := High(a);
  c := 0;
  SetLength(Result, l + 1);
  for i := 0 to l do
  begin
    Found := False;
    for t := 0 to c -1 do
      if (Abs(Result[t].x - a[i].x) <= w) and (Abs(Result[t].y - a[i].y) <= h) then
      begin
        Found := True;
        Break;
      end;
    if not Found then
//    if (t >= c) then
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

function ReArrangeandShortenArray(const a: TPointArray; Dist: Integer): TPointArray;
var
  i, t, c, l: Integer;
  Found: Boolean;
begin
  l := High(a);
  c := 0;
  SetLength(Result, l + 1);
  for i := 0 to l do
  begin
    Found := False;
    for t := 0 to c -1 do
      if (Round(sqrt(Sqr(Result[t].x - a[i].x) + Sqr(Result[t].y - a[i].y))) <= Dist) then
      begin
        Found := True;
        Break;
      end;
    if not found then
//    if (t >= c) then
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

function TPAtoATPAEx(const TPA: TPointArray; w, h: Integer): T2DPointArray;
var
   a, b, c, l: LongInt;
   Found: Boolean;
begin
  SetLength(Result, 0);
  l := High(TPA);
  c := 0;
  for a := 0 to l do
  begin
    Found := false;
    for b := 0 to c -1 do
      if (Abs(TPA[a].X - Result[b][0].X) <= w) and (Abs(TPA[a].Y - Result[b][0].Y) <= h) then
      begin
        Found := True;
        Break;
      end;
    if Found then
//    if (b < c) then
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

function TPAtoATPA(const TPA: TPointArray; Dist: Integer): T2DPointArray;
var
   a, b, c, l: LongInt;
   Found: Boolean;
begin
  SetLength(Result, 0);
  l := High(tpa);
  c := 0;
  for a := 0 to l do
  begin
    Found := false;
    for b := 0 to c -1 do
      if (Round(sqrt(Sqr(TPA[a].X - Result[b][0].X) + Sqr(TPA[a].Y - Result[b][0].Y))) <= Dist) then
      begin
        Found := True;
        Break;
      end;
    if Found then
//    if (b < c) then
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

procedure SortTPAByX(var a: TPointArray; const LowToHi: Boolean);
var
   i, l: Integer;
   Arr: TIntegerArray;
begin
  l := High(a);
  if (l < 0) then Exit;
  SetLength(Arr, l + 1);
  for i := 0 to l do
    Arr[i] := a[i].x;
  QuickTPASort(arr, a, 0, l, LowToHi);
end;

procedure SortTPAByY(var a: TPointArray; const LowToHi: Boolean);
var
   i, l: Integer;
   Arr: TIntegerArray;
begin
  l := High(a);
  if (l < 0) then Exit;
  SetLength(Arr, l + 1);
  for i := 0 to l do
    Arr[i] := a[i].y;
  QuickTPASort(arr, a, 0, l, LowToHi);
end;

function FindTPARows(a: TPointArray): T2DPointArray;
var
  bounds: TBox;
  l: Integer;
begin
  l := High(a);
  if (l < 1) then
    if l = 0 then
    begin
      SetLength(Result, 1);
      Result[0] := a;
      Exit;
    end else
      Exit;
  bounds := GetTPABounds(a);
  SortTPAByX(a, True);
  Result := TPAtoATPAEx(a, bounds.x2 - bounds.x1, 0);
end;

function FindTPAColumns(a: TPointArray): T2DPointArray;
var
  bounds: TBox;
  l: Integer;
begin
  l := High(a);
  if (l < 1) then
    if l = 0 then
    begin
      SetLength(Result, 1);
      Result[0] := a;
      Exit;
    end else
      Exit;
  bounds := GetTPABounds(a);
  SortTPAByY(a, True);
  Result := TPAtoATPAEx(a, 0, bounds.y2 - bounds.y1);
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
  l := High(a);
  if (l < 0) then Exit;
  SetLength(DistArr, l + 1);
  for i := 0 to l do
  begin
    if length(a[i]) = 0 then continue;
    DistArr[i] := Round(Sqr(From.x - a[i][0].x) + Sqr(From.y - a[i][0].y));
  end;
  QuickATPASort(DistArr, a, 0, l, True);
end;

{/\
  Sorts the T2DPointArray a from the midpoint of each TPA by From.
/\}

procedure SortATPAFromMidPoint(var a: T2DPointArray; const From: TPoint);
var
   i, l: Integer;
   DistArr: TIntegerArray;
   MidPt: TPoint;
begin
  l := High(a);
  if (l < 0) then Exit;
  SetLength(DistArr, l + 1);
  for i := 0 to l do
  begin
    MidPt := MiddleTPA(a[i]);
    DistArr[i] := Round(Sqr(From.x - MidPt.x) + Sqr(From.y - MidPt.y));
  end;
  QuickATPASort(DistArr, a, 0, l, True);
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

function MiddleTPAEx(const TPA: TPointArray; var x, y: Integer): Boolean;
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

function MiddleTPA(const tpa: TPointArray): TPoint;
var
   i, l: Integer;
begin
  FillChar(result,sizeof(TPoint),0);
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

function CombineTPA(const Ar1, Ar2: TPointArray): TPointArray;
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

function CombineIntArray(const Ar1, Ar2: TIntegerArray): TIntegerArray;
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

function InIntArrayEx(const a: TIntegerArray; var Where: Integer; const Number: Integer): Boolean;
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

function InIntArray(const a: TIntegerArray; Number: Integer): Boolean;
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
  Found: Boolean;
begin
  b := Copy(a);
  l := High(b);
  c := 0;
  for i := 0 to l do
  begin
    Found := False;
    for t := 0 to c -1 do
      if (b[i] = a[t]) then
      begin
        Found := True;
        Break;
      end;
    if not Found then
//    if (t >= c) then
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
  Found: Boolean;
begin
  b := Copy(a);
  l := High(b);
  c := 0;
  for i := 0 to l do
  begin
    Found := false;
    for t := 0 to c -1 do
      if (b[i] = a[t]) then
      begin
        Found := true;
        Break;
      end;
    if not Found then
//    if (t >= c) then
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

function SplitTPAEx(const arr: TPointArray; w, h: Integer): T2DPointArray;
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

function SplitTPA(const arr: TPointArray; Dist: Integer): T2DPointArray;
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
        if (Round(sqrt(Sqr(Result[c][t1].x - tpa[t2].x) + Sqr(Result[c][t1].y - tpa[t2].y))) <= Dist) then
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

function TPAPosNext(const Find: TPoint; const V: TPointArray;
  const PrevPos: Integer; const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : Tpoint;
begin
  if IsSortedAscending then // binary search
    begin
      if Max(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          while L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  while (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      for I := Max(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;
procedure AppendToArray(var V: TPointArray; const R: TPoint);
var
  Len: integer;
begin
  Len := Length(V);
  SetLength(V, Len + 1);
  V[Len] := R;
end;

function GlueTPAs(const V1, V2: TPointArray; const IsSortedAscending,byDifference: Boolean): TPointArray;
var
  I, J, L, LV : Integer;
begin
  if not byDifference then
    begin
       SetLength(Result, 0);
    if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      while (I < L) and (J < LV) do
        begin
          while (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] = V2[J] then
                AppendToArray(Result,V1[I]);
              while (J < LV) and ((V2[J] < V1[I]) or (V2[J] = V1[I])) do
                Inc(J);
            end;
        end;
    end
  else
    for I := 0 to Length(V1) - 1 do
      if (TPAPosNext(V1[I], V2) >= 0) and (TPAPosNext(V1[I], Result) = -1) then
       AppendToArray(Result,V1[I]);
    end
  else
    begin
      SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      while (I < L) and (J < LV) do
        begin
          while (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] <> V2[J] then
                 AppendToArray(Result,V1[I]);
              while (J < LV) and ((V2[J] < V1[I]) or (V2[J] = V1[I])) do
                Inc(J);
            end;
        end;
    end
  else
    for I := 0 to Length(V1) - 1 do
      if (TPAPosNext(V1[I], V2) = -1) and (TPAPosNext(V1[I], Result) = -1) then
        AppendToArray(Result,V1[I]);
    end;
end;

function FloodFillTPA(const TPA : TPointArray) : T2DPointArray;
var
  x,y,i,CurrentArray, LengthTPA,CurrentStack : integer;
  TempBox : TBox;
  PointsToFill : T2DBoolArray;
  Lengths : TIntegerArray;
  TempTPA : TPointArray;
  Stack : TPointArray;
  fx,fy : integer;
begin;
  LengthTPA := High(TPA);
  if LengthTPA < 1 then
  begin;
    if LengthTPA = 0 then
    begin;
      SetLength(Result,1,1);
      Result[0][0] := TPA[0];
    end else
      SetLength(Result,0);
    exit;
  end;
  TempBox := GetTPABounds(TPA);
  SetLength(PointsToFill,TempBox.x2 - TempBox.x1+3,TempBox.y2 - TempBox.y1+3); //W + 2, H + 2 so that we can check the borders
  fy := TempBox.y2 - TempBox.y1+3;
  fx := TempBox.x2 - TempBox.x1+2;
  for i := 0 to fx do
    FillChar(PointsToFill[i][0],fy,0);
  x := TempBox.x1 - 1;
  y := TempBox.y1 - 1;
  CurrentArray := -1;
  SetLength(Stack   , LengthTPA + 1);
  SetLength(Lengths , LengthTPA + 1);
  SetLength(TempTPA , LengthTPA + 1);
  for I := 0 to LengthTPA do
  begin;
    TempTPA[I].x := TPA[I].x - x;
    TempTPA[I].y := TPA[I].y - y;
  end;
  for I := 0 to LengthTPA do
    PointsToFill[TempTPA[I].x][TempTPA[I].y] := True;
  for I := 0 to LengthTPA do
    if PointsToFill[TempTPA[I].x][TempTPA[I].y] then
    begin;
      PointsToFill[TempTPA[i].x][TempTPA[i].y] := false;
      inc(CurrentArray);
      SetLength(Result,CurrentArray + 1);
      SetLength(Result[CurrentArray],LengthTPA - I + 1);
      Lengths[CurrentArray] := 0;
      CurrentStack := 0;
      Stack[0].x := TempTPA[I].x;
      Stack[0].y := TempTPA[I].y;
      While CurrentStack > -1 do
      begin;
        fx := stack[CurrentStack].x;
        fy := stack[CurrentStack].y;
        dec(CurrentStack);
        Result[CurrentArray][Lengths[CurrentArray]].x := fx + x;
        Result[CurrentArray][Lengths[CurrentArray]].y := fy + y;
        inc(Lengths[CurrentArray]);
        if PointsToFill[fx+1][fy] then
        begin
          inc(CurrentStack);Stack[CurrentStack].x := fx+1;Stack[Currentstack].y := fy;PointsToFill[fx+1][fy] := false;
        end;
        if PointsToFill[fx][fy+1] then
        begin
          inc(CurrentStack);Stack[CurrentStack].x := fx;Stack[Currentstack].y := fy+1;PointsToFill[fx][fy+1] := false;
        end;
        if PointsToFill[fx-1][fy] then
        begin
          inc(CurrentStack);Stack[CurrentStack].x := fx-1;Stack[Currentstack].y := fy;PointsToFill[fx-1][fy] := false;
        end;
        if PointsToFill[fx][fy-1] then
        begin
          inc(CurrentStack);Stack[CurrentStack].x := fx;Stack[Currentstack].y := fy-1;PointsToFill[fx][fy-1] := false;
        end;
        if PointsToFill[fx+1][fy+1] then
        begin
          inc(CurrentStack);Stack[CurrentStack].x := fx+1;Stack[Currentstack].y := fy+1;PointsToFill[fx+1][fy+1] := false;
        end;
        if PointsToFill[fx-1][fy-1] then
        begin
          inc(CurrentStack);Stack[CurrentStack].x := fx-1;Stack[Currentstack].y := fy-1;PointsToFill[fx-1][fy-1] := false;
        end;
        if PointsToFill[fx-1][fy+1] then
        begin
          inc(CurrentStack);Stack[CurrentStack].x := fx-1;Stack[Currentstack].y := fy+1;PointsToFill[fx-1][fy+1] := false;
        end;
        if PointsToFill[fx+1][fy-1] then
        begin
          inc(CurrentStack);Stack[CurrentStack].x := fx+1;Stack[Currentstack].y := fy-1;PointsToFill[fx+1][fy-1] := false;
        end;
      end;
      SetLength(Result[CurrentArray],Lengths[CurrentArray]);
    end;
  SetLength(Stack,0);
  SetLength(TempTPA,0);
  SetLength(Lengths,0);
end;

{/\
  Removes the points in the TPointArray Points that are not within the degrees
  \\ SD (StartDegree) and ED (EndDegree) and the distances MinR (MinRadius) and
  \\ MaxR (MaxRadius) from the origin (Mx, My).
/\}
procedure FilterPointsPie(var Points: TPointArray; const SD, ED, MinR, MaxR: Extended; Mx, My: Integer);
var
  BminusAx, BminusAy, CminusAx, CminusAy: Extended; //don't let the type deceive you. They are vectors!
  G: TPointArray;
  I, L, T: Integer;
  StartD, EndD: Extended;
  Over180: Boolean;
begin
  T := High(Points);
  if (T < 0) then Exit;
  SetLength(G, T + 1);
  L := 0;

  StartD := SD;    //I still think this can be done more efficient, a while loop... Come on
  EndD := ED;
  while StartD > 360.0 do
    StartD := StartD - 360.0;
  while EndD > 360.0 do
    EndD := EndD - 360.0;
  while StartD < 0.0 do
    StartD := StartD + 360.0;
  while EndD < 0.0 do
    EndD := EndD + 360.0;

  if StartD > EndD then          //Calculate if the difference is more then 180 degrees
    Over180 := (EndD + 360 - StartD) > 180
  else
    Over180 := (EndD - StartD) > 180;

  if Over180 then
    SwapE(StartD, EndD);

  //a is the midPoint, B is the left limit line, C is the right Limit Line, X the point we are checking
  BminusAx := cos(degtorad(StartD - 90));      //creating the two unit vectors
  BminusAy := sin(degtorad(StartD - 90));      //I use -90 or else it will start at the right side instead of top

  CminusAx := cos(degtorad(EndD - 90));
  CminusAy := sin(degtorad(EndD - 90));

  for I := 0 to T do
  begin
    if (not(((BminusAx * (Points[i].y - MY)) - (BminusAy * (Points[i].x - MY)) > 0) and
    ((CminusAx * (Points[i].y - MY)) - (CminusAy * (Points[i].x - MY)) < 0)) xor Over180) then
      continue;
    G[L] := Points[I];
    Inc(L);
  end;

  SetLength(G, L);
  FilterPointsDist(G, MinR, MaxR, Mx, My);   //TODO: move this to the MMLAddon section, this doesn't belong in FilterPointsPie
  Points := G;
end;   
{/\
  Removes the points that don't have a dist between mindist/maxdist with (mx,my)
/\}

procedure FilterPointsDist(var Points: TPointArray; const MinDist,
  MaxDist: Extended; Mx, My: Integer);
var
  c,i,l : integer;
  d : extended;
  mind,maxd : extended;
begin
  l := high(points);
  c := 0;
  mind := sqr(mindist);
  maxd := sqr(maxdist);
  for i := 0 to l do
  begin
    d := sqr(Points[i].x - mx) + sqr(points[i].y - my);
    if (d >= mind) and (d <= maxd) then
    begin
      points[c] := points[i];
      inc(c);
    end;
  end;
  setlength(points,c);
end;

{/\
  Removes the points in the TPointArray Points that are not on the line defined by angle, radius and center.
/\}

procedure FilterPointsLine(var Points: TPointArray; Radial: Extended; Radius, MX, MY: Integer);
var
  I, Hi, Ind, y: Integer;
  P: TPointArray;
  Box: TBox;
  B: T2DBoolArray;
  SinAngle,CosAngle : Extended;
begin
  if Length(Points) < 1 then
    Exit;
  Ind := 0;
  Box:= GetTPABounds(Points);
  SinAngle := sin(Radial);
  CosAngle := cos(Radial);
  SetLength(B, max(Box.x2, Round(SinAngle * Radius + MX)) + 1);
  y:= max(Box.x2, -Round(CosAngle * Radius) + MY);
  for I:= 0 to High(B) do
  begin;
    SetLength(B[I], y + 1);
    FillChar(B[i][0],y+1,0);
  end;
  Hi:= High(Points);
  for I:= 0 to Hi do
    B[Points[I].x][Points[I].y]:= True;
  SetLength(P, Hi + 1);
  for I:= 0 to Radius do
  begin
    if(B[Round(SinAngle * I) + MX][-Round(CosAngle * I) + MY])then
    begin
      P[Ind].X := Round(SinAngle * I) + MX;
      P[Ind].Y := -Round(CosAngle * I) + MY;
      inc(Ind);
    end;
  end;
  SetLength(P, Ind);
  Points:= P;
end;

{/\
  Removes points in the TPA that are within maxDist of each other.
/\}
procedure FilterTPADist(var TPA: TPointArray; maxDist: integer);
var
  c, i, j, l, h, maxDistSq: integer;
  newTPA: TPointArray;
  inBadElements: TBooleanArray;
begin
  h := high(TPA);
  l := (h + 1);
  maxDistSq := (maxDist * maxDist);

  setLength(inBadElements, l);
  setLength(newTPA, l);

  for i := 0 to h do
    inBadElements[i] := false;

  for i := 0 to (h - 1) do
  begin
    if (inBadElements[i]) then
      continue;

    for j := (i + 1) to h do
    begin
      if (inBadElements[j]) then
        continue;

     // simplified -> a^2 + b^2 <= c^2
     if (((TPA[i].x - TPA[j].x) * (TPA[i].x - TPA[j].x)) + ((TPA[i].y - TPA[j].y) * (TPA[i].y - TPA[j].y)) <= maxDistSq) then
       inBadElements[j] := true;
    end;
  end;

  c := 0;

  // set the new TPA
  for i := 0 to h do
    if (not inBadElements[i]) then
    begin
      newTPA[c] := TPA[i];
      inc(c);
    end;

  setLength(newTPA, c);
  TPA := newTPA;
end;

{/\
  Removes the points that are inside or outside the distance Dist from the point (x, y) from the TPointArray ThePoints.
/\}

function RemoveDistTPointArray(x, y, dist: Integer;const ThePoints: TPointArray; RemoveHigher: Boolean): TPointArray;
var
  I, L, LL: integer;
begin;
  L := 0;
  LL := Length(ThePoints) -1;
  SetLength(Result, LL + 1);
  if RemoveHigher then
  begin;
    for I := 0 to LL do
      if not (Round(sqrt(sqr(ThePoints[i].x - x)+sqr(ThePoints[i].y - y))) > Dist) then
      begin;
        Result[L] := ThePoints[i];
        L := L + 1;
      end;
  end else
  begin;
    for I := 0 to LL do
      if not (Round(sqrt(sqr(ThePoints[i].x - x)+sqr(ThePoints[i].y - y))) < Dist) then
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

function GetATPABounds(const ATPA: T2DPointArray): TBox;
var
  I,II,L2,L : Integer;
begin;
  FillChar(result,sizeof(TBox),0);
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

function GetTPABounds(const TPA: TPointArray): TBox;
var
  I,L : Integer;
begin;
  FillChar(result,sizeof(TBox),0);
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

function FindTPAinTPA(SearchTPA : TPointArray; const TotalTPA: TPointArray; var Matches: TPointArray): Boolean;
var
  Len, I,II,LenSearch,xOff,yOff : integer;
  tx,ty,MatchCount : integer;
  Screen : T2DBoolArray;
  ScreenBox,SearchBox : TBox;
  Found: Boolean;
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
    for i := ScreenBox.x2 downto 0 do
      FillChar(Screen[i][0],screenbox.y2+1,0);
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
            Found := false;
            For II := 0 to LenSearch do
              if Screen[tx + SearchTPA[II].x ][ty + SearchTPA[II].y] = False then
              begin
                Found := True;
                Break;
              end;
            if not found then
//            if II > LenSearch then
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

function FindTextTPAinTPA(Height : integer;const SearchTPA, TotalTPA: TPointArray; var Matches: TPointArray): Boolean;
var
  Len, I,II,LenSearch,LenTPA,xOff,yOff,x,y: integer;
  tx,ty,MatchCount : integer;
  Found : boolean;
  Screen : T2DBoolArray;
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
  SetLength(Screen, SearchBox.x2 + 1,SearchBox.y2 + 1);
  for i := SearchBox.x2 downto 0 do
    FillChar(screen[i][0],SearchBox.y2+1,0);
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
    for i := ScreenBox.x2 downto 0 do
      FillChar(screen[i][0],screenbox.y2+1,0);
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
            Found := false;
            For II := 0 to LenSearch do
              if Screen[tx + SearchTPA[II].x ][ty + SearchTPA[II].y] = False then
              begin
                Found := true;
                Break;
              end;
            if (not Found) then
            begin;
              Found := false;
              For II := 0 to LenTPA do
                if Screen[tx + InversedTPA[II].x ][ty + InversedTPA[II].y] = True then
                begin
                  Found := true;
                  Break;
                end;
              if (not Found) then
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
  Finds the points that exist in all TPA's in the ATPA.
/\}

function GetSamePointsATPA(const ATPA : T2DPointArray; var Matches : TPointArray) : boolean;
var
  I,ii,Len,MatchesC : integer;
  MinBox,TempBox : TBox;
  Grid : Array of Array of LongWord;
  CompareValue : Longword;
  W,H,x,y: integer;
begin;
  len := high(ATPA);
  result := false;
  if len >= 31 then
  begin;
    Writeln('You cannot have more than 32 TPA''s in your ATPA for this function');
    exit;
  end;
  if len <= 0 then
  begin;
    Writeln('You''d need more than 1 TPA for this function');
    exit;
  end;
  MinBox.x1 := 0;
  MinBox.y1 := 0;
  MinBox.x2 := MaxInt;
  MinBox.y2 := MaxInt;
  for i := 0 to len do
    if Length(ATPA[i]) = 0 then
      Exit
    else
    begin
      TempBox := GetTPABounds(ATPA[i]);
      MinBox.x1 := Max(MinBox.x1,TempBox.x1);
      MinBox.y1 := Max(MinBox.y1,TempBox.y1);
      MinBox.x2 := Min(MinBox.x2,TempBox.x2);
      MinBox.y2 := Min(MinBox.y2,TempBox.y2);
    end;
  w := MinBox.x2-minbox.x1;
  h := minbox.y2 - minbox.y1;
  Writeln(format('(%d,%d,%d,%d)',[minbox.x1,minbox.y1,minbox.x2,minbox.y2]));
  SetLength(Grid,w + 1);
  for i := (W) downto 0 do
  begin;
    setlength(grid[i],H + 1);
    FillChar(grid[i][0],SizeOf(LongWord) * (H + 1),0);
  end;
  for i := 0 to len do
    for ii := high(ATPA[i]) downto 0 do
      if (ATPA[i][ii].x >= MinBox.x1) and (ATPA[i][ii].x <= MinBox.x2) and
         (ATPA[i][ii].y >= MinBox.y1) and (ATPA[i][ii].y <= MinBox.y2) then
        Grid[ATPA[i][ii].x-MinBox.x1][ATPA[i][ii].y-MinBox.y1] :=
             Grid[ATPA[i][ii].x-MinBox.x1][ATPA[i][ii].y-MinBox.y1] or (1 shl i);//Set that this TPA has this point..
  CompareValue := 0;
  for i := 0 to len do
    CompareValue := CompareValue or (1 shl i);
  SetLength(matches, (W+1) * (H+ 1));
  MatchesC := 0;
  for y := 0 to H do
    for x := 0 to W do
      if Grid[x][y] = CompareValue then
      begin;
        Matches[MatchesC].x := x + minbox.x1;
        Matches[MatchesC].y := y + minbox.y1;
        inc(MatchesC);
      end;
  result := (MatchesC <> 0);
  setlength(matches,MatchesC);
end;
{/\
  Finds the possible gaps in the TPointArray TPA and results the gaps as a T2DPointArray.
  \\ Considers as a gap if the gap length is >= MinPixels.
  \\ Only horizontal, sorry folks.
/\}

function FindGapsTPA(const TPA: TPointArray; MinPixels: Integer): T2DPointArray;
var
  Len,TotalLen,LenRes,I,II,III : integer;
  Screen : T2DBoolArray;
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
    for i := 0 to Width do
      FillChar(Screen[i][0],(Height+1),0);
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

Function MergeATPA(const ATPA: T2DPointArray): TPointArray;
var
  I, II, Len, TempL, CurrentL: integer;
begin;
  Len := High(ATPA);
  if Len < 0 then
  begin
    SetLength(Result, 0);
    Exit;
  end;
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
  Appends ToAppend array to the end of TPA.
/\}
procedure AppendTPA(var TPA: TPointArray; const ToAppend: TPointArray);
var
  l,lo,i : integer;
begin
  l := high(ToAppend);
  lo := length(TPA);
  setlength(TPA,lo + l  + 1);
  for i := 0 to l do
    TPA[i + lo] := ToAppend[i];
end;

{/\
  Returns a TPointArray of a line specified by the end points x1,y1 and x2,y2.
/\}
function TPAFromLine(const x1, y1, x2, y2: Integer): TPointArray;
var
  Dx, Dy, CurrentX, CurrentY, Len, TwoDx, TwoDy, Xinc, YInc: Integer;
  TwoDxAccumulatedError, TwoDyAccumulatedError: Integer;
begin
  Len := 0;
  Dx := (X2-X1);
  Dy := (Y2-Y1);
  TwoDx := Dx + Dx;
  TwoDy := Dy + Dy;
  CurrentX := X1;
  CurrentY := Y1;
  Xinc := 1;
  Yinc := 1;
  if (Dx < 0) then
  begin
    Xinc := -1;
    Dx := - Dx;
    TwoDx := - TwoDx;
  end;
  if (Dy < 0) then
  begin
    Yinc := -1;
    Dy := -Dy;
    TwoDy := - TwoDy;
  end;
  SetLength(Result, 1);
  Result[0] := Point(X1,Y1);
  if ((Dx <> 0) or (Dy <> 0)) then
  begin
    if (Dy <= Dx) then
    begin
      TwoDxAccumulatedError := 0;
      repeat
        Inc(CurrentX, Xinc);
        Inc(TwoDxAccumulatedError, TwoDy);
        if (TwoDxAccumulatedError > Dx) then
        begin
          Inc(CurrentY, Yinc);
          Dec(TwoDxAccumulatedError, TwoDx);
        end;
        Inc(Len);
        SetLength(Result, Len + 1);
        Result[Len] := Point(CurrentX,CurrentY);
      until (CurrentX = X2);
    end else
    begin
      TwoDyAccumulatedError := 0;
      repeat
        Inc(CurrentY, Yinc);
        Inc(TwoDyAccumulatedError, TwoDx);
        if (TwoDyAccumulatedError > Dy) then
        begin
          Inc(CurrentX, Xinc);
          Dec(TwoDyAccumulatedError, TwoDy);
        end;
        Inc(Len);
        SetLength(Result, Len + 1);
        Result[Len] := Point(CurrentX,CurrentY);
      until (CurrentY = Y2);
    end;
  end
end;

{/\
  Returns a TPointArray of the edge/border of the given Box.
/\}
function EdgeFromBox(const Box: TBox): TPointArray;
var
  Height, I, Len, WHM1, Width: Integer;
begin
  Width := (Box.x2 - Box.x1);
  Height := (Box.y2 - Box.y1);
  Len := ((Width * 2) + (Height * 2));
  SetLength(Result, Len);
  for I := 0 to Width do
  begin
    Result[i].x := Box.x1 + i;
    Result[i].y := Box.y1;
    Result[(Len - (Width - i)) - 1].x := Box.x1 + i;
    Result[(Len - (Width - i)) - 1].y := Box.y2;
  end;
  WHM1 := Width + (Height - 1);
  for I := 1 to (Height - 1) do
  begin
    Result[Width + I].x := Box.x1;
    Result[Width + I].y := Box.y1 + I;
    Result[WHM1 + I].x := Box.x2;
    Result[WHM1 + I].y := Box.y1 + I;
  end;
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
  Returns a TPointArray of the outline of a ellipse.
/\}
function TPAFromEllipse(const CX, CY, XRadius, YRadius : Integer): TPointArray;
var
  x, y, a2, b2, h,
  d1, d2, sn, sd,
  a2t8, b2t8, Len : Integer;
begin
  SetLength(Result, 9999);
  Len := 0;
  Result[Len] := Point(CX + XRadius, CY);
  Inc(Len);
  Result[Len] := Point(CX, CY - YRadius);
  Inc(Len);
  Result[Len] := Point(CX - XRadius, CY);
  Inc(Len);
  Result[Len] := Point(CX, CY + YRadius);
  Inc(Len);
  x := 0;
  y := YRadius;
  a2 := XRadius * XRadius;
  b2 := y * y;
  a2t8 := a2 * 8;
  b2t8 := b2 * 8;
  h := b2 * 4 + a2 * (1 - 4 * y);
  d1 := 12 * b2;
  d2 := -a2t8 * (y - 1);
  sn := b2;
  sd := (a2 * y) - (a2 shr 1);

  while (sn < sd) do
  begin
    if (h > 0) then
    begin
      Dec(y);
      h := h + d2;
      sd := sd - a2;
      d2 := d2 + a2t8;
    end;
    Inc(x);
    h := h + d1;
    sn := sn + b2;
    d1 := d1 + b2t8;
    Result[Len] := Point(CX + X, CY + Y);
    Inc(Len);
    Result[Len] := Point(CX + X, CY - Y);
    Inc(Len);
    Result[Len] := Point(CX - X, CY + Y);
    Inc(Len);
    Result[Len] := Point(CX - X, CY - Y);
    Inc(Len);
  end;

  h := b2 * (4 * x * x + 4 * x + 1) + 4 * a2 * (y - 1) * (y - 1) - 4 * a2 * b2;
  d1 := b2t8 * (x + 1);
  d2 := -4 * a2 * (2 * y - 3);
  while (y > 1) do
  begin
    if (h < 0) then
    begin
      Inc(x);
      h := h + d1;
      d1 := d1 + b2t8;
    end;
    Dec(y);
    h := h + d2;
    d2 := d2 + a2t8;
    Result[Len] := Point(CX + X, CY + Y);
    Inc(Len);
    Result[Len] := Point(CX + X, CY - Y);
    Inc(Len);
    Result[Len] := Point(CX - X, CY + Y);
    Inc(Len);
    Result[Len] := Point(CX - X, CY - Y);
    Inc(Len);
  end;

  SetLength(Result, Len);
end;

{/\
  Returns a TPointArray of the outline of a ellipse.
/\}
function TPAFromCircle(const CX, CY, Radius: Integer): TPointArray;
begin
  Result := TPAFromEllipse(CX, CY, Radius, Radius);
end;

{/\
  Fills a Ellipse generated by TPAFromEllipse or TPAFromCircle.
/\}
procedure FillEllipse(var a: TPointArray);
var
  i, h, Hi: Integer;
  b, c: T2DPointArray;
begin
  b := FindTPARows(a);
  Hi := High(b);
  SetLength(c, Hi + 1);
  h := 0;
  for i := 0 to Hi do
  begin
    c[h] := TPAFromBox(GetTPABounds(b[i]));
    inc(h);
  end;
  a := MergeATPA(c);
end;

{/\
  Rotate the given TPA with A radians.
/\}

Function RotatePoints(Const P: TPointArray; A, cx, cy: Extended): TPointArray ;

Var
   I, L: Integer;
   CosA,SinA : extended;

Begin
  L := High(P);
  SetLength(Result, L + 1);
  CosA := Cos(a);
  SinA := Sin(a);
  For I := 0 To L Do
  Begin
    Result[I].X := Trunc(cx + CosA * (p[i].x - cx) - SinA * (p[i].y - cy));
    Result[I].Y := Trunc(cy + SinA * (p[i].x - cx) + CosA * (p[i].y - cy));
  End;
  // I recon it's faster than Point().
End;

{/\
  Rotate the given Point with A radians.
/\}

Function RotatePoint(Const p: TPoint; angle, mx, my: Extended): TPoint;

Begin
  Result.X := Trunc(mx + cos(angle) * (p.x - mx) - sin(angle) * (p.y - my));
  Result.Y := Trunc(my + sin(angle) * (p.x - mx) + cos(angle) * (p.y- my));
End;

{/\
  Returns the edges of the given TPA.
/\}

function FindTPAEdges(const p: TPointArray): TPointArray;
var
  b: T2DBoolArray;
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
  begin
    SetLength(b[i], y);
    FillChar(b[i][0],y,0);
  end;
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

function PointInTPA(const p: TPoint;const arP: TPointArray): Boolean;
var
  i, l: Integer;
begin
  l := High(arP);
  if l < 0 then
    Exit(false);
  Result := True;
  for i := 0 to l do
    if (arP[i].x = p.x) and (arP[i].y = p.y) then
      Exit;
  Result := False;
end;

{/\
  Removes the given ClearPoints from arP.
/\}

function ClearTPAFromTPA(const arP, ClearPoints: TPointArray): TPointArray;
var
  i, j, l, l2: Integer;
  Found: Boolean;
begin
  Setlength(result,0);
  l := High(arP);
  l2 := High(ClearPoints);
  for i := 0 to l do
  begin
    Found := false;
    for j := 0 to l2 do
      if (arP[i].x = ClearPoints[j].x) and (arP[i].y = ClearPoints[j].y) then
      begin
        Found := True;
        Break;
      end;
    if not found then
//    if (j = l2 + 1) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := arP[i];
    end;
  end;
end;

{/\
  Removes all the doubles point from a TPA.
/\}
{Fixed by Cynic}

procedure ClearDoubleTPA(var TPA: TPointArray);
var
  i,j,k : integer;
  flag  : boolean;
  tmp     : TPointArray;
Begin
  i:=0;
  SetLength(tmp,0);
  while i < Length(TPA) do
  begin
    flag:=false;
    k:=0;
    while (k < Length(tmp)) and (not flag) do
    begin
      flag:=tmp[k]=TPA[i];
      Inc(k);
    end;
    if not flag then
    begin
      k:=Length(tmp);
      SetLength(tmp,k+1);
      tmp[k]:=TPA[i];
      Inc(i);
    end
    else
     begin
      j:=i;
      while j < length (TPA)-1 do
      begin
        TPA[j]:=TPA[j+1];
        inc(j);
      end;
      SetLength(TPA,Length(TPA)-1);
    end;
  end;
end;
{/\
  Uses Box to define an area around TotalTPA.
  Every point that is not in TotalTPA, but is in Box, is added to the Result.
  \\ This can be very handy if you want for example, can get all the colors of the background, but not of the actual object.
  \\ If you pass this all the colors of the background, it will returns the points of the object.
/\}

Function ReturnPointsNotInTPA(Const TotalTPA: TPointArray; const Box: TBox): TPointArray;
var
  x, y, w, h, i, l: integer;
  B: T2DBoolArray;
begin;
  w := Box.x2 - Box.x1;
  h := Box.y2 - Box.y1;
  if (w = 0) and (h = 0) then
    Exit;
  SetLength(b, w + 1, h + 1);
  for i := w downto 0 do
    FillChar(b[i][0],h+1,0);
  l := High(TotalTPA);
  x := 0;
  for i := 0 to l do
    if ((TotalTPA[i].x >= Box.x1) and (TotalTPA[i].x <= Box.x2) and
        (TotalTPA[i].y >= Box.y1) and (TotalTPA[i].y <= Box.y2)) then
    begin;
      Inc(x);
      B[TotalTPA[i].x-Box.x1][TotalTPA[i].y-Box.y1] := True;
    end;
  if x = 0 then
    Exit;
  SetLength(result,(w + 1) * (h + 1) - x);
  i := 0;
  for x := 0 to w do
    for y := 0 to h do
      if not B[x][y] then
      try
        Result[i].x := x + Box.x1;
        Result[i].y := y + Box.y1;
        Inc(i);
      except end;
  SetLength(b, 0);
  SetLength(Result, i);
end;

{/\
  Sorts a TPointArray by either X or Y. You have to define the max Point as well.
/\}

Procedure TPACountSort(Var TPA: TPointArray;const max: TPoint;Const SortOnX : Boolean);
Var
   c: T2DIntegerArray;
   I, II, III, hTPA, cc: Integer;
Begin
  hTPA := High(TPA);
  if hTPA < 1 then
    Exit;
  SetLength(c, max.X + 1,max.Y + 1);
  for i := max.x downto 0 do
    FillChar(c[i][0],(max.y+1)*sizeof(Integer),0);
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
   c: T2DIntegerArray;
   I, II, III, hTPA, cc: Integer;
   Max : TPoint;
Begin
  hTPA := High(TPA);
  if hTPA < 1 then
    Exit;
  max.X := maxx.X - base.X;
  max.Y := maxx.Y - base.Y;
  SetLength(c, max.X + 1,max.Y + 1);
  for i := max.x downto 0 do
    FillChar(c[i][0],(max.y+1)*sizeof(integer),0);
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

{/\
  Returns the sum of all integers in the array
/\}
function SumIntegerArray(const Ints : TIntegerArray): Integer;
var
  I, H: Integer;
begin
  Result := 0;
  H := High(Ints);
  for I := 0 to H do
    Result := Result + Ints[I];
end;

{/\
  Inverts the IntegerArray, last becomes first etc..
/\}
procedure InvertTIA(var tI: TIntegerArray);
var
  Temp: TIntegerArray;
  i, h: Integer;
begin
  h := High(tI);
  Temp := Copy(tI);
  for i := 0 to h do
    tI[i] := Temp[h - i];
end;

{/\
  Results the Average of an IntegerArray
/\}

function AverageTIA(const tI: TIntegerArray): Integer;
begin
  try Result := (SumIntegerArray(tI) div Length(tI)); except Result := 0; end;
end;

{/\
  Results the Average of an ExtendedArray
/\}
function AverageExtended(const tE: TExtendedArray): Extended;
var
  i, h: Integer;
begin
  Result := 1;
  try
    h := High(tE);
    for i := 0 to h do
      Result := (Result * tE[i]);
    Result := Power(Result, 1/(h + 1));
  except
    Result := 0.0;
  end;
end;

{/\
  Returns true if the two inputed TPA's are exactly the same (so the order matters)
/\}
function SameTPA(const aTPA, bTPA: TPointArray): Boolean;
var
  I: LongInt;
  h : integer;
begin
  Result := False;
  if (Length(aTPA) <> Length(bTPA)) then
    Exit;
  h := high(ATPA);
  for I := Low(aTPA) to h do
    if ((aTPA[I].X <> bTPA[I].X) or (aTPA[I].Y <> bTPA[I].Y)) then
      Exit;
  Result := True;
end;
{/\
  Returns true if the TPA is found as one of ATPA's sub-TPA's.. And again, order matters
/\}
function TPAInATPA(const TPA: TPointArray;const InATPA: T2DPointArray; var Index: LongInt): Boolean;
var
  I: LongInt;
  h : integer;
begin
  Result := True;
  h := high(inATPA);
    for I := Low(InATPA) to h do
      if (SameTPA(TPA, InATPA[I])) then
      begin
        Index := I;
        Exit;
      end;
  Result := False;
end;

procedure OffsetTPA(var TPA: TPointArray; const Offset: TPoint);
var
  i : integer;
begin
  for i := high(TPA) downto 0 do
  begin;
    inc(TPA[i].x,offset.x);
    inc(TPA[i].y,offset.y);
  end;
end;

procedure OffsetATPA(var ATPA: T2DPointArray; const Offset: TPoint);
var
  i : integer;
begin
  for i := high(ATPA) downto 0 do
    OffsetTPA(ATPA[i],Offset);
end;

function MiddleBox(b : TBox) : TPoint;
begin
  result := point((b.x2+b.x1) div 2,(b.y2+b.y1) div 2);
end;


end.

