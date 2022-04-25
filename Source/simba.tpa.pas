{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.tpa;

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.mufasatypes;

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
function FindTPARows(const TPA: TPointArray): T2DPointArray;
function FindTPAColumns(const TPA: TPointArray): T2DPointArray;
procedure SortTPAFrom(var a: TPointArray; const From: TPoint);
procedure SortATPAFrom(var a: T2DPointArray; const From: TPoint);
procedure SortATPAFromFirstPoint(var a: T2DPointArray; const From: TPoint);
procedure SortATPAFromMidPoint(var a: T2DPointArray; const From: TPoint);
procedure SortATPAFromFirstPointX(var a: T2DPointArray; const From: TPoint);
procedure SortATPAFromFirstPointY(var a: T2DPointArray; const From: TPoint);
function MiddleTPAEx(const TPA: TPointArray; var x, y: Integer): Boolean;
function MiddleTPA(const tpa: TPointArray): TPoint;
procedure MedianTPAEx(const TPA: TPointArray; out X, Y: Integer);
function MedianTPA(const TPA: TPointArray): TPoint;
procedure SortATPASize(var a: T2DPointArray; const BigFirst: Boolean);
procedure SortATPAFromSize(var a: T2DPointArray; const Size: Integer; CloseFirst: Boolean);
procedure FilterTPAsBetween(var atpa: T2DPointArray; const minLength, maxLength: integer);
function InIntArrayEx(const a: TIntegerArray; var Where: Integer; const Number: Integer): Boolean;
function InIntArray(const a: TIntegerArray; Number: Integer): Boolean;
function SplitTPAEx(const arr: TPointArray; w, h: Integer): T2DPointArray;
function SplitTPA(const arr: TPointArray; Dist: Integer): T2DPointArray;
function ClusterTPAEx(const TPA: TPointArray; width, height: Integer): T2DPointArray;
function ClusterTPA(const TPA: TPointArray; dist: Integer): T2DPointArray;
function FloodFillTPA(const TPA : TPointArray) : T2DPointArray;
procedure FilterPointsPie(var Points: TPointArray; const SD, ED, MinR, MaxR: Extended; Mx, My: Integer; Natural: Boolean);
procedure FilterPointsDist(var Points: TPointArray; const MinDist,MaxDist: Extended; Mx, My: Integer);
procedure FilterPointsLine(var Points: TPointArray; Radial: Extended; Radius, MX, MY: Integer);
procedure FilterPointsBox(var points: TPointArray; x1, y1, x2, y2: integer);
procedure FilterTPADist(var TPA: TPointArray; maxDist: integer);
function RemoveDistTPointArray(x, y, dist: Integer;const ThePoints: TPointArray; RemoveHigher: Boolean): TPointArray;
function GetATPABounds(const ATPA: T2DPointArray): TBox;
function GetTPABounds(const TPA: TPointArray): TBox;
function GetSamePointsATPA(const ATPA: T2DPointArray; var Matches: TPointArray): Boolean;
function FindGapsTPA(const TPA: TPointArray; MinPixels: Integer): T2DPointArray;
procedure SortCircleWise(var tpa: TPointArray; const cx, cy, StartDegree: Integer; SortUp, ClockWise: Boolean);
procedure LinearSort(var tpa: TPointArray; cx, cy, sd: Integer; SortUp: Boolean);
function MergeATPA(const ATPA : T2DPointArray) : TPointArray;
function TPAFromLine(const x1, y1, x2, y2: Integer): TPointArray;
function EdgeFromBox(const Box: TBox): TPointArray;
function TPAFromBox(const Box : TBox) : TPointArray;
function TPAFromEllipse(const CX, CY, XRadius, YRadius : Integer): TPointArray;
function TPAFromCircle(const CX, CY, Radius: Integer): TPointArray;
function TPAFromPolygon(const shape: TPointArray): TPointArray;
procedure FillEllipse(var TPA: TPointArray);
function FindTPAEdges(const TPA: TPointArray): TPointArray;
function TPAErode(const TPA: TPointArray; Amount: Int32): TPointArray;
function TPAGrow(const TPA: TPointArray; Amount: Int32): TPointArray;
function PointInTPA(const p: TPoint;const arP: TPointArray): Boolean;
function ClearTPAFromTPA(const arP, ClearPoints: TPointArray): TPointArray;
procedure ClearDoubleTPA(var TPA: TPointArray);
function ReturnPointsNotInTPA(const TPA: TPointArray; Area: TBox): TPointArray;
function SameTPA(const aTPA, bTPA: TPointArray): Boolean;
function TPAInATPA(const TPA: TPointArray;const InATPA: T2DPointArray; var Index: LongInt): Boolean;
procedure OffsetTPA(var TPA : TPointArray; const Offset : TPoint);
procedure OffsetATPA(var ATPA : T2DPointArray; const Offset : TPoint);
function PartitionTPA(const TPA:TPointArray; BoxWidth, BoxHeight:Integer): T2DPointArray;
function PointsInRangeOf(Points, Other: TPointArray; MinDist, MaxDist: Double): TPointArray; overload;
function PointsInRangeOf(Points, Other: TPointArray; MinDistX, MinDistY, MaxDistX, MaxDistY: Double): TPointArray; overload;

function Unique(const Arr: TIntegerArray): TIntegerArray;
function Unique(const Arr: TDoubleArray): TDoubleArray;
function Unique(const Points: TPointArray): TPointArray;

implementation

uses
  math,
  simba.math, simba.slacktree, simba.overallocatearray,
  simba.generics_array, simba.helpers_matrix;

function PointsInRangeOf(Points, Other: TPointArray; MinDist, MaxDist: Double): TPointArray; overload;
var
  Tree: TSlackTree;
  I: Int32;
  Matches: T2DPointArray;
begin
  Result := Default(TPointArray);

  if (Length(Points) > 0) and (Length(Other) > 0) then
  begin
    SetLength(Matches, Length(Other));

    Tree.Init(Copy(Points));
    for I := 0 to High(Other) do
      Matches[I] := Tree.RangeQueryEx(Other[I], MinDist, MinDist, MaxDist, MaxDist, True);

    Result := MergeATPA(Matches);
  end;
end;

function PointsInRangeOf(Points, Other: TPointArray; MinDistX, MinDistY, MaxDistX, MaxDistY: Double): TPointArray; overload;
var
  Tree: TSlackTree;
  I: Int32;
  Matches: T2DPointArray;
begin
  Result := Default(TPointArray);

  if (Length(Points) > 0) and (Length(Other) > 0) then
  begin
    SetLength(Matches, Length(Other));

    Tree.Init(Copy(Points));
    for I := 0 to High(Other) do
      Matches[I] := Tree.RangeQueryEx(Other[I], MinDistX, MinDistY, MaxDistX, MaxDistY, True);

    Result := MergeATPA(Matches);
  end;
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

function FindTPARows(const TPA: TPointArray): T2DPointArray;
var
  Points: TPointArray;
begin
  Result := Default(T2DPointArray);
  if Length(TPA) > 0 then
  begin
    if (Length(TPA) = 1) then
    begin
      Result := [TPA];
      Exit;
    end;

    Points := Copy(TPA);

    SortTPAByX(Points, True);
    with GetTPABounds(Points) do
      Result := TPAtoATPAEx(Points, X2 - X1, 0);
  end;
end;

function FindTPAColumns(const TPA: TPointArray): T2DPointArray;
var
  Points: TPointArray;
begin
  Result := Default(T2DPointArray);
  if Length(TPA) > 0 then
  begin
    if (Length(TPA) = 1) then
    begin
      Result := [TPA];
      Exit;
    end;

    Points := Copy(TPA);

    SortTPAByY(Points, True);
    with GetTPABounds(Points) do
      Result := TPAtoATPAEx(Points, Y2 - Y1, 0);
  end;
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
  Sorts the T2DPointArray a from the first X point of each TPA by from.
/\}
procedure SortATPAFromFirstPointX(var a: T2DPointArray; const From: TPoint);
var
  i, l: Integer;
  DistArr: TIntegerArray;
begin
  l := High(a);
  if (l < 0) then
    Exit;

  SetLength(DistArr, l + 1);
  for i := 0 to l do
  begin
    if (length(a[i]) <= 0) then
      continue;

    DistArr[i] := Round(Sqr(From.x - a[i][0].x));
  end;

  QuickATPASort(DistArr, a, 0, l, True);
end;

{/\
  Sorts the T2DPointArray a from the first Y point of each TPA by from.
/\}
procedure SortATPAFromFirstPointY(var a: T2DPointArray; const From: TPoint);
var
  i, l: Integer;
  DistArr: TIntegerArray;
begin
  l := high(a);
  if (l < 0) then
    Exit;

  setLength(DistArr, l + 1);
  for i := 0 to l do
  begin
    if (length(a[i]) <= 0) then
      continue;

    DistArr[i] := Round(Sqr(From.y - a[i][0].y));
  end;

  QuickATPASort(DistArr, a, 0, l, True);
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
  Returns the x and y coords of the point in the tpa which is closest to the middle of the tpa.
/\}
procedure MedianTPAEx(const TPA: TPointArray; out X, Y: Integer);
var
  I: Int32;
  Dist: Double;
  Best: record
    Index: Int32;
    Dist: Double;
  end;
begin
  X := 0;
  Y := 0;
  if Length(TPA) = 0 then
    Exit;

  Best.Index := 0;
  Best.Dist := $FFFFFF;

  MiddleTPAEx(TPA, X, Y);

  for I := 0 to High(TPA) do
  begin
    Dist := Hypot(TPA[I].X - X, TPA[I].Y - Y);
    if Dist < Best.Dist then
    begin
      Best.Dist := Dist;
      Best.Index := I;
    end;
  end;

  X := TPA[Best.Index].X;
  Y := TPA[Best.Index].Y;
end;

{/\
  Returns the *point in the tpa* closest to the middle of the tpa.
/\}
function MedianTPA(const TPA: TPointArray): TPoint;
begin
  MedianTPAEx(TPA, Result.X, Result.Y);
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

procedure FilterTPAsBetween(var atpa: T2DPointArray; const minLength, maxLength: integer);
var
  tmpATPA: T2DPointArray;
  l, i, x, a: integer;
begin
  if (minLength > maxLength) then
  begin
    writeln('FilterTPAsBetween: Tour min length is greater than your max length');
    exit;
  end;

  l := length(atpa);
  a := 0;

  if (l < 1) then
    exit;

  for i := 0 to (l - 1) do
  begin
    x := length(atpa[i]);

    if (not inRange(x, minLength, maxLength)) then
    begin
      setLength(tmpATPA, a + 1);
      tmpATPA[a] := atpa[i];
      inc(a);
    end;
  end;

  atpa := tmpATPA;
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
  Splits the points with max X and Y distances W and H to their own TPointArrays.
/\}
function SplitTPAEx(const arr: TPointArray; w, h: Integer): T2DPointArray;
var
  t1, t2, c, ec, tc, l: Integer;
  tpa: TPointArray;
begin
  Result := Default(T2DPointArray);

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
  Result := Default(T2DPointArray);

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

{/\
Splits TPA with dist (alternative method for SplitTPA)
/\}
function ClusterTPA(const TPA: TPointArray; dist: Integer): T2DPointArray;
type
  TPointScan = record
    skipRow: Boolean;
    count: Integer;
  end;
var
  h, i, l, c, s, x, y, o, r, d, m: Integer;
  p: array of array of TPointScan;
  q: TPointArray;
  a, b, t: TBox;
  e: Extended;
  z: TPoint;
  v: Boolean;
begin
  SetLength(Result, 0);
  h := High(TPA);
  if (h > -1) then
    if (h > 0) then
    begin
      b.X1 := TPA[0].X;
      b.Y1 := TPA[0].Y;
      b.X2 := TPA[0].X;
      b.Y2 := TPA[0].Y;
      r := 0;
      for i := 1 to h do
      begin
        if (TPA[i].X < b.X1) then
          b.X1 := TPA[i].X
        else
          if (TPA[i].X > b.X2) then
            b.X2 := TPA[i].X;
        if (TPA[i].Y < b.Y1) then
          b.Y1 := TPA[i].Y
        else
          if (TPA[i].Y > b.Y2) then
            b.Y2 := TPA[i].Y;
      end;
      SetLength(p, ((b.X2 - b.X1) + 1));
      for i := 0 to (b.X2 - b.X1) do
      begin
        SetLength(p[i], ((b.Y2 - b.Y1) + 1));
        for c := 0 to (b.Y2 - b.Y1) do
        begin
          p[i][c].count := 0;
          p[i][c].skipRow := False;
        end;
      end;
      e := Extended(dist);
      if (e < 0.0) then
        e := 0.0;
      d := Ceil(e);
      m := Max(((b.X2 - b.X1) + 1), ((b.Y2 - b.Y1) + 1));
      if (d > m) then
        d := m;
      for i := 0 to h do
        Inc(p[(TPA[i].X - b.X1)][(TPA[i].Y - b.Y1)].count);
      for i := 0 to h do
        if (p[(TPA[i].X - b.X1)][(TPA[i].Y - b.Y1)].count > 0) then
        begin
          c := Length(Result);
          SetLength(Result, (c + 1));
          SetLength(Result[c], p[(TPA[i].X - b.X1)][(TPA[i].Y - b.Y1)].count);
          for o := 0 to (p[(TPA[i].X - b.X1)][(TPA[i].Y - b.Y1)].count - 1) do
            Result[c][o] := TPA[i];
          r := (r + p[(TPA[i].X - b.X1)][(TPA[i].Y - b.Y1)].count);
          if (r > h) then
            Exit;
          SetLength(q, 1);
          q[0] := TPA[i];
          p[(TPA[i].X - b.X1)][(TPA[i].Y - b.Y1)].count := 0;
          s := 1;
          while (s > 0) do
          begin
            s := High(q);
            z := q[s];
            a.X1 := (z.X - d);
            a.Y1 := (z.Y - d);
            a.X2 := (z.X + d);
            a.Y2 := (z.Y + d);
            t := a;
            SetLength(q, s);
            if (a.X1 < b.X1) then
              a.X1 := b.X1
            else
              if (a.X1 > b.X2) then
                a.X1 := b.X2;
            if (a.Y1 < b.Y1) then
              a.Y1 := b.Y1
            else
              if (a.Y1 > b.Y2) then
                a.Y1 := b.Y2;
            if (a.X2 < b.X1) then
              a.X2 := b.X1
            else
              if (a.X2 > b.X2) then
                a.X2 := b.X2;
            if (a.Y2 < b.Y1) then
              a.Y2 := b.Y1
            else
              if (a.Y2 > b.Y2) then
                a.Y2 := b.Y2;
            case ((t.X1 <> a.X1) or (t.X2 <> a.X2)) of
              True:
              for y := a.Y1 to a.Y2 do
                if not p[(a.X2 - b.X1)][(y - b.Y1)].skipRow then
                for x := a.X1 to a.X2 do
                  if (p[(x - b.X1)][(y - b.Y1)].count > 0) then
                    if (Round(Sqrt(Sqr(z.X - x) + Sqr(z.Y - y))) <= dist) then
                    begin
                      l := Length(Result[c]);
                      SetLength(Result[c], (l + p[(x - b.X1)][(y - b.Y1)].count));
                      for o := 0 to (p[(x - b.X1)][(y - b.Y1)].count - 1) do
                      begin
                        Result[c][(l + o)].X := x;
                        Result[c][(l + o)].Y := y;
                      end;
                      r := (r + p[(x - b.X1)][(y - b.Y1)].count);
                      if (r > h) then
                        Exit;
                      p[(x - b.X1)][(y - b.Y1)].count := 0;
                      SetLength(q, (s + 1));
                      q[s] := Result[c][l];
                      Inc(s);
                    end;
              False:
              for y := a.Y1 to a.Y2 do
                if not p[(a.X2 - b.X1)][(y - b.Y1)].skipRow then
                begin
                  v := True;
                  for x := a.X1 to a.X2 do
                    if (p[(x - b.X1)][(y - b.Y1)].count > 0) then
                      if (Round(Sqrt(Sqr(z.X - x) + Sqr(z.Y - y))) <= dist) then
                      begin
                        l := Length(Result[c]);
                        SetLength(Result[c], (l + p[(x - b.X1)][(y - b.Y1)].count));
                        for o := 0 to (p[(x - b.X1)][(y - b.Y1)].count - 1) do
                        begin
                          Result[c][(l + o)].X := x;
                          Result[c][(l + o)].Y := y;
                        end;
                        r := (r + p[(x - b.X1)][(y - b.Y1)].count);
                        if (r > h) then
                          Exit;
                        p[(x - b.X1)][(y - b.Y1)].count := 0;
                        SetLength(q, (s + 1));
                        q[s] := Result[c][l];
                        Inc(s);
                      end else
                        v := False;
                  if v then
                    p[(a.X2 - b.X1)][(y - b.Y1)].skipRow := True;
                end;
            end;
          end;
        end;
    end else
    begin
      SetLength(Result, 1);
      SetLength(Result[0], 1);
      Result[0][0] := TPA[0];
    end;
end;

{/\
 Splits TPA with width, height (alternative method for SplitTPAEx).
 /\}
function ClusterTPAEx(const TPA: TPointArray; width, height: Integer): T2DPointArray;
type
  TPointScan = record
    skipRow: Boolean;
    count: Integer;
  end;
var
  h, i, l, c, s, x, y, o, r, dw, dh: Integer;
  p: array of array of TPointScan;
  q: TPointArray;
  a, b, t: TBox;
  z: TPoint;
begin
  SetLength(Result, 0);
  h := High(TPA);
  if (((width > 0) and (height > 0)) and (h > -1)) then
    if (h > 0) then
    begin
      dw := width;
      dh := height;
      b.X1 := TPA[0].X;
      b.Y1 := TPA[0].Y;
      b.X2 := TPA[0].X;
      b.Y2 := TPA[0].Y;
      r := 0;
      for i := 1 to h do
      begin
        if (TPA[i].X < b.X1) then
          b.X1 := TPA[i].X
        else
          if (TPA[i].X > b.X2) then
            b.X2 := TPA[i].X;
        if (TPA[i].Y < b.Y1) then
          b.Y1 := TPA[i].Y
        else
          if (TPA[i].Y > b.Y2) then
            b.Y2 := TPA[i].Y;
      end;
      SetLength(p, ((b.X2 - b.X1) + 1));
      for i := 0 to (b.X2 - b.X1) do
      begin
        SetLength(p[i], ((b.Y2 - b.Y1) + 1));
        for c := 0 to (b.Y2 - b.Y1) do
        begin
          p[i][c].count := 0;
          p[i][c].skipRow := False;
        end;
      end;
      if (dw > ((b.X2 - b.X1) + 1)) then
        dw := ((b.X2 - b.X1) + 1);
      if (dh > ((b.Y2 - b.Y1) + 1)) then
        dh := ((b.Y2 - b.Y1) + 1);
      for i := 0 to h do
        Inc(p[(TPA[i].X - b.X1)][(TPA[i].Y - b.Y1)].count);
      for i := 0 to h do
        if (p[(TPA[i].X - b.X1)][(TPA[i].Y - b.Y1)].count > 0) then
        begin
          c := Length(Result);
          SetLength(Result, (c + 1));
          SetLength(Result[c], p[(TPA[i].X - b.X1)][(TPA[i].Y - b.Y1)].count);
          for o := 0 to (p[(TPA[i].X - b.X1)][(TPA[i].Y - b.Y1)].count - 1) do
            Result[c][o] := TPA[i];
          r := (r + p[(TPA[i].X - b.X1)][(TPA[i].Y - b.Y1)].count);
          if (r > h) then
            Exit;
          SetLength(q, 1);
          q[0] := TPA[i];
          p[(TPA[i].X - b.X1)][(TPA[i].Y - b.Y1)].count := 0;
          s := 1;
          while (s > 0) do
          begin
            s := High(q);
            z := q[s];
            a.X1 := (z.X - dw);
            a.Y1 := (z.Y - dh);
            a.X2 := (z.X + dw);
            a.Y2 := (z.Y + dh);
            t := a;
            SetLength(q, s);
            if (a.X1 < b.X1) then
              a.X1 := b.X1
            else
              if (a.X1 > b.X2) then
                a.X1 := b.X2;
            if (a.Y1 < b.Y1) then
              a.Y1 := b.Y1
            else
              if (a.Y1 > b.Y2) then
                a.Y1 := b.Y2;
            if (a.X2 < b.X1) then
              a.X2 := b.X1
            else
              if (a.X2 > b.X2) then
                a.X2 := b.X2;
            if (a.Y2 < b.Y1) then
              a.Y2 := b.Y1
            else
              if (a.Y2 > b.Y2) then
                a.Y2 := b.Y2;
            case ((t.X1 <> a.X1) or (t.X2 <> a.X2)) of
              True:
              for y := a.Y1 to a.Y2 do
                if not p[(a.X2 - b.X1)][(y - b.Y1)].skipRow then
                for x := a.X1 to a.X2 do
                  if (p[(x - b.X1)][(y - b.Y1)].count > 0) then
                  begin
                    l := Length(Result[c]);
                    SetLength(Result[c], (l + p[(x - b.X1)][(y - b.Y1)].count));
                    for o := 0 to (p[(x - b.X1)][(y - b.Y1)].count - 1) do
                    begin
                      Result[c][(l + o)].X := x;
                      Result[c][(l + o)].Y := y;
                    end;
                    r := (r + p[(x - b.X1)][(y - b.Y1)].count);
                    if (r > h) then
                      Exit;
                    p[(x - b.X1)][(y - b.Y1)].count := 0;
                    SetLength(q, (s + 1));
                    q[s] := Result[c][l];
                    Inc(s);
                  end;
              False:
              for y := a.Y1 to a.Y2 do
                if not p[(a.X2 - b.X1)][(y - b.Y1)].skipRow then
                begin
                  for x := a.X1 to a.X2 do
                    if (p[(x - b.X1)][(y - b.Y1)].count > 0) then
                    begin
                      l := Length(Result[c]);
                      SetLength(Result[c], (l + p[(x - b.X1)][(y - b.Y1)].count));
                      for o := 0 to (p[(x - b.X1)][(y - b.Y1)].count - 1) do
                      begin
                        Result[c][(l + o)].X := x;
                        Result[c][(l + o)].Y := y;
                      end;
                      r := (r + p[(x - b.X1)][(y - b.Y1)].count);
                      if (r > h) then
                        Exit;
                      p[(x - b.X1)][(y - b.Y1)].count := 0;
                      SetLength(q, (s + 1));
                      q[s] := Result[c][l];
                      Inc(s);
                    end;
                  p[(a.X2 - b.X1)][(y - b.Y1)].skipRow := True;
                end;
            end;
          end;
        end;
    end else
    begin
      SetLength(Result, 1);
      SetLength(Result[0], 1);
      Result[0][0] := TPA[0];
    end;
end;

function FloodFillTPA(const TPA : TPointArray): T2DPointArray;
var
  x,y,i,CurrentArray, LengthTPA,CurrentStack : integer;
  TempBox : TBox;
  PointsToFill : TBooleanMatrix;
  Lengths : TIntegerArray;
  TempTPA : TPointArray;
  Stack : TPointArray;
  fx,fy : integer;
begin
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
  \\
  Setting Natural to true changes the behavior.
  \\It allows specification of both clockwise and counterclockwise radials.
  \\This means that -10, 10 will produce the same pie as 10, -10, -180, 90 will
  \\produce the same pie as 180, 450, and 180, 90 will produce the same
  \\pie as 90, 180, etc.
/\}
procedure FilterPointsPie(var Points: TPointArray; const SD, ED, MinR, MaxR: Extended; Mx, My: Integer; Natural: Boolean);
var
  BminusAx, BminusAy, CminusAx, CminusAy: Extended; //don't let the type deceive you. They are vectors!
  G: TPointArray;
  I, L, T: Integer;
  StartD, EndD: Extended;
  Over180, Ccw: Boolean;
begin
  T := High(Points);
  if (T < 0) then Exit;
  SetLength(G, T + 1);
  L := 0;

  Ccw := SD > ED; // See if we want to go counterclockwise

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


  if StartD <> EndD then // if StartD = EndD, then we have a circle...
  begin
    if natural then
    begin
      if Ccw then
        Swap(StartD, EndD);
      if StartD > EndD then EndD := EndD + 360;
    end;

    Over180 := (Max(StartD, EndD) - Min(StartD, EndD)) > 180;

    if Over180 then
    begin
      StartD := StartD + 180;
      EndD   := EndD   + 180;
    end;

    //a is the midPoint, B is the left limit line, C is the right Limit Line, X the point we are checking
    BminusAx := cos(degtorad(StartD - 90));      //creating the two unit vectors
    BminusAy := sin(degtorad(StartD - 90));      //I use -90 or else it will start at the right side instead of top

    CminusAx := cos(degtorad(EndD - 90));
    CminusAy := sin(degtorad(EndD - 90));

    for I := 0 to T do
    begin
      if (not(((BminusAx * (Points[i].y - MY)) - (BminusAy * (Points[i].x - MX)) > 0) and
         ((CminusAx * (Points[i].y - MY)) - (CminusAy * (Points[i].x - MX)) < 0)) xor Over180) then
        continue;
      G[L] := Points[I];
      Inc(L);
    end;
    SetLength(Points, L);
    Points := G;
  end;
  FilterPointsDist(Points, MinR, MaxR, Mx, My);
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
  B: TBooleanMatrix;
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
  Removes the points that are not within the box
/\}
procedure FilterPointsBox(var points: TPointArray; x1, y1, x2, y2: integer);
var
  h, c, i, tmp: integer;
begin
  h := high(points);
  c := 0;

  if (x1 > x2) then
  begin
    tmp := x1;

    x1 := x2;
    x2 := tmp;
  end;

  if (y1 > y2) then
  begin
    tmp := y1;

    y1 := y2;
    y2 := tmp;
  end;

  for i := 0 to h do
    if (points[i].x > x1) and (points[i].x < x2) and (points[i].y > y1) and (points[i].y < y2) then
    begin
      points[c] := points[i];
      inc(c);
    end;

  setlength(points,c);
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
  Finds the points that exist in all TPA's in the ATPA.
/\}
function GetSamePointsATPA(const ATPA: T2DPointArray; var Matches: TPointArray): Boolean;
var
  Matrix: TIntegerMatrix;
  X, Y, W, H: Integer;
  I, J: Integer;
  Hit: Integer;
  TPA: TPointArray;
  Arr: specialize TSimbaOverAllocateArray<TPoint>;
  P: TPoint;
begin
  Matches := Default(TPointArray);

  if (Length(ATPA) > 1) then
  begin
    with GetATPABounds(ATPA) do
    begin
      Matrix.SetSize(Width, Height);
      for I := 0 to High(ATPA) do
      begin
        TPA := ATPA[I];
        for J := 0 to High(TPA) do
          Matrix[TPA[J].Y - Y1, TPA[J].X - X1] += 1;
      end;

      Arr.Init();

      W := Matrix.Width - 1;
      H := Matrix.Height - 1;

      Hit := Length(ATPA);

      for Y := 0 to H do
        for X := 0 to W do
        begin
          if (Matrix[Y, X] = Hit) then
          begin
            P.X := X + X1;
            P.Y := Y + Y1;

            Arr.Add(P);
          end;
        end;

      Matches := Arr.Trim();
    end;
  end;

  Result := Length(Matches) > 0;
end;

{/\
  Finds the possible gaps in the TPointArray TPA and results the gaps as a T2DPointArray.
  \\ Considers as a gap if the gap length is >= MinPixels.
  \\ Only horizontal, sorry folks.
/\}
function FindGapsTPA(const TPA: TPointArray; MinPixels: Integer): T2DPointArray;
var
  Len,TotalLen,LenRes,I,II,III : integer;
  Screen : TBooleanMatrix;
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

function MergeATPA(const ATPA: T2DPointArray): TPointArray;
var
  i, L: Int32;
begin;
  SetLength(Result, 0);

  for i := 0 to High(ATPA) do
  begin
    if (Length(ATPA[i]) = 0) then
      Continue;

    L := Length(Result);
    SetLength(Result, L + Length(ATPA[i]));
    Move(ATPA[i][0], Result[L], Length(ATPA[i]) * SizeOf(TPoint));
  end;
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
  X, Y, Count: Int32;
begin
  Count := 0;

  if (Box.X1 > Box.X2) or (Box.Y1 > Box.Y2) then
  begin
    SetLength(Result, Count);
    Exit;
  end;

  if (Box.X1 = Box.X2) and (Box.Y1 = Box.Y2) then
  begin
    SetLength(Result, 1);

    Result[0].X := Box.X1;
    Result[0].Y := Box.Y1;
  end else
  begin
    SetLength(Result, (Max(2, (Box.X2 - Box.X1) * 2)) + (Max(2, (Box.Y2 - Box.Y1) * 2)));

    for X := Box.X1 to Box.X2 do
    begin
      Result[Count].X := X;
      Result[Count].Y := Box.Y1;
      Result[Count + 1].X := X;
      Result[Count + 1].Y := Box.Y2;

      Inc(Count, 2);
    end;

    for Y := Box.Y1+1 to Box.Y2-1 do
    begin
      Result[Count].X := Box.X1;
      Result[Count].Y := Y;
      Result[Count + 1].X := Box.X2;
      Result[Count + 1].Y := Y;

      Inc(Count, 2);
    end;
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
  Size, Count: Int32;

   procedure Add(const X, Y: Int32);
   begin
     if (Count + 4 >= Size) then
     begin
       Size := Size * 2;

       SetLength(Result, Size);
     end;

     Result[Count].X := CX + X;
     Result[Count].Y := CY + Y;

     Result[Count+1].X := CX + X;
     Result[Count+1].Y := CY - Y;

     Result[Count+2].X := CX - X;
     Result[Count+2].Y := CY + Y;

     Result[Count+3].X := CX - X;
     Result[Count+3].Y := CY - Y;

     Inc(Count, 4);
   end;

var
  x, y, a2, b2, h,
  d1, d2, sn, sd,
  a2t8, b2t8: Integer;
begin
  Count := 4;
  Size := 128;
  SetLength(Result, Size);

  Result[0].X := CX + XRadius;
  Result[0].Y := CY;

  Result[1].X := CX;
  Result[1].Y := CY - YRadius;

  Result[2].X := CX - XRadius;
  Result[2].Y := CY;

  Result[3].X := CX;
  Result[3].Y := CY + YRadius;

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

    Add(X, Y);
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

    Add(X, Y);
  end;

  SetLength(Result, Count);
end;

{/\
  Returns a TPointArray of the outline of a ellipse.
/\}
function TPAFromCircle(const CX, CY, Radius: Integer): TPointArray;
begin
  Result := TPAFromEllipse(CX, CY, Radius, Radius);
end;

{/\
  Returns polygon as a TPointArray from a shape, which can be working either as
  an array of main points OR border points. note: The order of the points are important.
/\}
function TPAFromPolygon(const shape: TPointArray): TPointArray;
var
  b: TBox;
  x, y, h, i, l, r, z: Integer;
  o: TPointArray;
  f: Boolean;
  t: TBooleanMatrix;
  e: Extended;
  q, p, d: TPoint;
begin
  Result := nil;
  h := High(shape);
  if (h > -1) then
  begin
    SetLength(o, 0);

    b.x1 := shape[0].x;
    b.y1 := shape[0].y;
    b.x2 := shape[0].x;
    b.y2 := shape[0].y;

    for i := 0 to h do
    begin
      q := shape[i];
      if (i < h) then
        p := shape[(i + 1)]
      else
        p := shape[0];
      r := Length(o);
      if ((q.X <> p.X) or (q.Y <> p.Y)) then
      begin
        l := Max(Round(Abs(q.X - p.X)), Round(Abs(q.Y - p.Y)));
        SetLength(o, ((r + l) + 1));
        for z := 0 to l do
          o[(r + z)] := Point((q.X + Round((p.X - q.X) * (z / Extended(l)))), (q.Y + Round((p.Y - q.Y) * (z / Extended(l)))));
      end else
      begin
        SetLength(o, (r + 1));
        o[r] := q;
      end;
      if (shape[i].X < b.X1) then
        b.X1 := shape[i].X
      else
        if (shape[i].X > b.X2) then
          b.X2 := shape[i].X;
      if (shape[i].Y < b.Y1) then
        b.Y1 := shape[i].Y
      else
        if (shape[i].Y > b.Y2) then
          b.Y2 := shape[i].Y;
    end;

    SetLength(t, ((b.X2 - b.X1) + 1), ((b.Y2 - b.Y1) + 1));
    l := Length(o);
    for i := 0 to (l - 1) do
      if not t[(o[i].X - b.X1)][(o[i].Y - b.Y1)] then
        t[(o[i].X - b.X1)][(o[i].Y - b.Y1)] := True;
    for y := 0 to (b.Y2 - b.Y1) do
      for x := 0 to (b.X2 - b.X1) do
      begin
        f := t[x][y];
        if not f then
        begin
          d := Point((x + b.X1), (y + b.Y1));
          q := shape[0];
          for i := 0 to (h + 1) do
          begin
            p := shape[(i mod (h + 1))];
            if (d.Y > Min(q.Y, p.Y)) then
              if (d.Y <= Max(q.Y, p.Y)) then
                if (d.X <= Max(q.X, p.X)) then
                begin
                  if (q.y <> p.y) then
                    e := ((d.Y - q.Y) * (p.X - q.X) / Extended((p.Y - q.Y)) + q.X);
                  if ((q.X = p.X) or (d.X < e)) then
                    f := not f;
                end;
            q := p;
          end;
        end;
        if f then
        begin
          l := Length(Result);
          SetLength(Result, (l + 1));
          Result[l] := Point((x + b.X1), (y + b.Y1));
        end;
      end;
  end;
end;

{/\
  Fills a Ellipse generated by TPAFromEllipse or TPAFromCircle.
/\}
procedure FillEllipse(var TPA: TPointArray);
var
  Rows: T2DPointArray;
  Result: TPointArray;
  I, X, Y, Count: Int32;
begin
  Count := 0;
  Rows := FindTPARows(TPA);
  with GetATPABounds(Rows) do
    SetLength(Result, Width * Height);

  for I := 0 to High(Rows) do
  begin
    Y := Rows[I][0].Y;

    for X := Rows[I][0].X to Rows[I][High(Rows[I])].X do
    begin
      Result[Count].X := X;
      Result[Count].Y := Y;

      Inc(Count);
    end;
  end;

  SetLength(Result, Count);

  TPA := Result;
end;

{/\
  Returns the edges of the given TPA.
  Edge-points are points that are not completely surrounded by other points (8 way).
/\}
function FindTPAEdges(const TPA: TPointArray): TPointArray;
var
  Matrix: TIntegerMatrix;
  B: TBox;
  I: Int32;
  P: TPoint;
  W, H, C: Int32;
begin
  SetLength(Result, Length(TPA));

  B := GetTPABounds(TPA);

  SetLength(Matrix, B.Y2 - B.Y1 + 1, B.X2 - B.X1 + 1);
  for I := 0 to High(TPA) do
    Matrix[TPA[I].Y - B.Y1, TPA[I].X - B.X1] := 1;

  W := High(Matrix[0]);
  H := High(Matrix);
  C := 0;

  for I := 0 to High(TPA) do
  begin
    P.X := TPA[I].X - B.X1;
    P.Y := TPA[I].Y - B.Y1;

    if (P.X = 0) or (P.Y = 0) or (P.X = W) or (P.Y = H) then
    begin
      Result[C] := TPA[I];
      Inc(C);
    end else
    begin
      if (Matrix[P.Y - 1, P.X - 1] = 0) or (Matrix[P.Y - 1, P.X] = 0) or (Matrix[P.Y - 1, P.X + 1] = 0) or
         (Matrix[P.Y, P.X - 1] = 0)     or (Matrix[P.Y, P.X + 1] = 0) or
         (Matrix[P.Y + 1, P.X - 1] = 0) or (Matrix[P.Y + 1, P.X] = 0) or (Matrix[P.Y + 1, P.X + 1] = 0) then
      begin
        Result[C] := TPA[I];
        Inc(C);
      end;
    end;
  end;

  SetLength(Result, C);
end;

{/\
  Removes the edges of the given TPA a specified `Amount` of times.
  Uses the FindTPAEdges method.
/\}
function TPAErode(const TPA: TPointArray; Amount: Int32): TPointArray;
var
  W, H: Int32;
  B: TBox;
  Matrix: T2DIntegerArray;
  Edges: TPointArray;
  Removed: TBooleanArray;

  function RemoveEdges: Boolean;
  var
    P: TPoint;
    I: Int32;
    Count: Int32;
  begin
    Result := False;

    Count := 0;

    for I := 0 to High(TPA) do
    begin
      if Removed[I] then
        Continue;

      P.X := TPA[I].X - B.X1;
      P.Y := TPA[I].Y - B.Y1;

      if (P.X > 0) and (P.Y > 0) and (P.X < W) and (P.Y < H) then
      begin
        if (Matrix[P.Y - 1, P.X - 1] = 1) and (Matrix[P.Y - 1, P.X] = 1) and (Matrix[P.Y - 1, P.X + 1] = 1) and
           (Matrix[P.Y, P.X - 1] = 1)     and (Matrix[P.Y, P.X + 1] = 1) and
           (Matrix[P.Y + 1, P.X - 1] = 1) and (Matrix[P.Y + 1, P.X] = 1) and (Matrix[P.Y + 1, P.X + 1] = 1) then
          Continue;
      end;

      Removed[I] := True;

      Edges[Count].X := P.X;
      Edges[Count].Y := P.Y;
      Inc(Count);

      Result := True; // Found a edge
    end;

    for I := 0 to Count - 1 do
      Matrix[Edges[I].Y, Edges[I].X] := 0;
  end;

var
  I: Int32;
  Count: Int32;
begin
  Result := Default(TPointArray);

  if (Amount > 0) and (Length(TPA) > 0) then
  begin
    B := GetTPABounds(TPA);

    SetLength(Matrix, B.Y2 - B.Y1 + 1, B.X2 - B.X1 + 1);
    for I := 0 to High(TPA) do
      Matrix[TPA[I].Y - B.Y1, TPA[I].X - B.X1] := 1;

    W := High(Matrix[0]);
    H := High(Matrix);

    SetLength(Result, Length(TPA));
    SetLength(Edges, Length(TPA));
    SetLength(Removed, Length(TPA));

    for I := 1 to Amount do
      if not RemoveEdges() then
        Break;

    Count := 0;

    for I := 0 to High(TPA) do
      if not Removed[I] then
      begin
        Result[Count] := TPA[I];
        Inc(Count);
      end;

    SetLength(Result, Count);
  end;
end;

{/\
  A simple and lazy but fast way to grow a TPA.

  For each point of the TPA, a filled circle at the X/Y with radius `Amount` is added to the TPA.
/\}
function TPAGrow(const TPA: TPointArray; Amount: Int32): TPointArray;
var
  Matrix: T2DIntegerArray;
  Template: TPointArray;
  I, J, H: Int32;
  X, Y: Int32;
  TemplateX, TemplateY: Int32;
  Size, Count: Int32;
  B: TBox;
  Hit: ^Int32;
begin
  Result := Default(TPointArray);

  if (Amount > 0) then
  begin
    Template := TPAFromCircle(0, 0, Amount);
    FillEllipse(Template);

    Count := 0;
    Size := Length(TPA) * 4;
    SetLength(Result, Size);

    H := High(Template);
    B := GetTPABounds(TPA);

    SetLength(Matrix, (B.Y2 - B.Y1 + 1) + (Amount * 2),
                      (B.X2 - B.X1 + 1) + (Amount * 2));

    for I := 0 to High(TPA) do
    begin
      X := (TPA[I].X - B.X1) + Amount;
      Y := (TPA[I].Y - B.Y1) + Amount;

      for J := 0 to H do
      begin
        TemplateX := X + Template[J].X;
        TemplateY := Y + Template[J].Y;

        Hit := @Matrix[TemplateY][TemplateX];

        if (Hit^ = 0) then
        begin
          Result[Count].X := (TemplateX + B.X1) - Amount;
          Result[Count].Y := (TemplateY + B.Y1) - Amount;

          Inc(Count);
          if (Count = Size) then
          begin
            Size *= 2;
            SetLength(Result, Size);
          end;

          Hit^ := 1;
        end;
      end;
    end;

    SetLength(Result, Count);
  end;
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
  v, h, r, l, x, y: Integer;
  B: array of TBooleanArray;
  bx, tmp: TBox;
begin;
  r := 0;
  l := Length(arP);

  if (l > 0) then
  begin
    SetLength(Result, l);
    bx := GetTPABounds(arP);
    h := High(ClearPoints);

    if (h > -1) then
    begin
      tmp := GetTPABounds(clearPoints);
      bx.X1 := Min(bx.X1, tmp.X1);
      bx.Y1 := Min(bx.Y1, tmp.Y1);
      bx.X2 := Max(bx.X2, tmp.X2);
      bx.Y2 := Max(bx.Y2, tmp.Y2);
    end;

    SetLength(B, ((bx.X2 - bx.X1) + 1));

    for v := 0 to (bx.X2 - bx.X1) do
    begin
      SetLength(B[v], ((bx.Y2 - bx.Y1) + 1));
      y := High(B[v]);
      for x := 0 to y do
        B[v][x] := False;
    end;

    for x := 0 to h do
      B[(ClearPoints[x].X - bx.X1)][(ClearPoints[x].Y - bx.Y1)] := True;

    for v := 0 to (l - 1) do
      if not B[(arP[v].X - bx.X1)][(arP[v].Y - bx.Y1)] then
      begin
        Result[r] := arP[v];
        Inc(r);
      end;

    SetLength(B, 0);
  end;

  SetLength(Result, r);
end;

{/\
  Removes all the doubles point from a TPA.
/\}
procedure ClearDoubleTPA(var TPA: TPointArray);
var
  v, h, r: Integer;
  B: array of TBooleanArray;
  bx: TBox;
begin;
  h := high(TPA);
  r := 0;

  if (h > 0) then
  begin
    bx.x1 := TPA[0].x;
    bx.y1 := TPA[0].Y;
    bx.x2 := TPA[0].X;
    bx.y2 := TPA[0].Y;

    for v := 1 to h do
    begin
      if (TPA[v].X < bx.X1) then
        bx.X1 := TPA[v].X
      else
        if (TPA[v].X > bx.X2) then
          bx.X2 := TPA[v].X;
      if (TPA[v].Y < bx.Y1) then
        bx.Y1 := TPA[v].Y
      else
        if (TPA[v].Y > bx.Y2) then
          bx.Y2 := TPA[v].Y;
    end;
    SetLength(B, ((bx.X2 - bx.X1) + 1));
    for v := 0 to (bx.X2 - bx.X1) do
      SetLength(B[v], ((bx.Y2 - bx.Y1) + 1));
    for v := 0 to h do
      if not B[(TPA[v].X - bx.X1)][(TPA[v].Y - bx.Y1)] then
      begin
        B[(TPA[v].X - bx.X1)][(TPA[v].Y - bx.Y1)] := True;
        TPA[r] := TPA[v];
        Inc(r);
      end;
    SetLength(TPA, r);
    SetLength(B, 0);
  end;
end;

{/\
  Uses Box to define an area around TotalTPA.
  Every point that is not in TotalTPA, but is in Box, is added to the Result.
  \\ This can be very handy if you want for example, can get all the colors of the background, but not of the actual object.
  \\ If you pass this all the colors of the background, it will returns the points of the object.
/\}

function ReturnPointsNotInTPA(const TPA: TPointArray; Area: TBox): TPointArray;
var
  Matrix: TBooleanMatrix;
  i, W, H, X, Y: Integer;
begin
  W := (Area.X2 - Area.X1) + 1;
  H := (Area.Y2 - Area.Y1) + 1;
  SetLength(Matrix, H, W);

  for i := 0 to High(TPA) do
    if (TPA[i].X >= Area.X1) and (TPA[i].Y >= Area.Y1) and (TPA[i].X <= Area.X2) and (TPA[i].Y <= Area.Y2) then
      Matrix[TPA[i].Y - Area.Y1][TPA[i].X - Area.X1] := True;

  SetLength(Result, W * H);

  i := 0;
  for Y := 0 to H-1 do
    for X := 0 to W-1 do
      if not Matrix[Y][X] then
      begin
        Result[i].X := X + Area.X1;
        Result[i].Y := Y + Area.Y1;
        Inc(i);
      end;

  SetLength(Result, i);
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
  for i := High(ATPA) downto 0 do
    OffsetTPA(ATPA[i],Offset);
end;

function PartitionTPA(const TPA: TPointArray; BoxWidth, BoxHeight:Integer): T2DPointArray;
var
  i,x,y,id,l,cols,rows,h:Integer;
  Area:TBox;
begin
  Result := Default(T2DPointArray);
  H := High(TPA);
  if (H < 0) then Exit;
  Area := GetTPABounds(TPA);
  Area.X2 := (Area.X2 - Area.X1) + 1;  //Width
  Area.Y2 := (Area.Y2 - Area.Y1) + 1;  //Height
  Cols := Ceil(Area.X2 / BoxWidth);
  Rows := Ceil(Area.Y2 / BoxHeight);
  SetLength(Result, (Cols+1)*(Rows+1));
  for i:=0 to H do
  begin
    X := (TPA[i].x-Area.x1) div BoxWidth;
    Y := (TPA[i].y-Area.y1) div BoxHeight;
    ID := (Y*Cols)+X;
    L := Length(Result[ID]);
    SetLength(Result[ID], L+1);
    Result[ID][L] := TPA[i];
  end;
end;

function Unique(const Points: TPointArray): TPointArray;
var
  Matrix: TBooleanMatrix;
  I, Count: Integer;
begin
  SetLength(Result, Length(Points));

  if (Length(Points) > 0) then
  begin
    Count := 0;

    with GetTPABounds(Points) do
    begin
      Matrix.SetSize(Width, Height);

      for I := 0 to High(Points) do
        if not Matrix[Points[I].Y - Y1, Points[I].X - X1] then
        begin
          Matrix[Points[I].Y - Y1, Points[I].X - X1] := True;
          Result[Count] := Points[I];
          Inc(Count);
        end;
    end;

    SetLength(Result, Count);
  end;
end;

function Unique(const Arr: TIntegerArray): TIntegerArray;
var
  I, J, Value, Size, Len: Int32;
  Table: T2DIntegerArray;
  Bucket: PIntegerArray;
  Buffer: specialize TSimbaOverAllocateArray<Integer>;
label
  Next;
begin
  Buffer.Init();

  SetLength(Table, NextPower2(Length(Arr)));
  Size := High(Table);

  for i := 0 to High(Arr) do
  begin
    Value := Arr[i];
    Bucket := @Table[Value and Size];
    Len := Length(Bucket^);

    for J := 0 to Len - 1 do
      if Bucket^[J] = Value then
        goto Next;

    SetLength(Bucket^, Len + 1);
    Bucket^[Len] := Value;

    Buffer.Add(Value);

    Next:
  end;

  Result := Buffer.Trim();
end;

function Unique(const Arr: TDoubleArray): TDoubleArray;
var
  i,j,last:Integer;
begin
  Result := Copy(Arr);

  last := Length(Result);

  i:=0;
  while (i < last) do
  begin
    j := i+1;
    while (j < last) do
    begin
      if SameValue(Result[i], Result[j]) then
      begin
        Result[j] := Result[last-1];
        dec(last);
        dec(j);
      end;

      Inc(j);
    end;
    Inc(i);
  end;

  SetLength(Result, last);
end;

end.

