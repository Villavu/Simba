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

function NearbyPointInArrayEx(const P: TPoint; w, h:Integer; a: TPointArray): Boolean;
function NearbyPointInArray(const P: TPoint; Dist:Integer; a: TPointArray): Boolean;
function ReArrangeandShortenArrayEx(const a: TPointArray; w, h: Integer): TPointArray;
function ReArrangeandShortenArray(const a: TPointArray; Dist: Integer): TPointArray;
function TPAtoATPAEx(const TPA: TPointArray; w, h: Integer): T2DPointArray;
function TPAtoATPA(const TPA: TPointArray; Dist: Integer): T2DPointArray;
procedure SortTPAByX(var Arr: TPointArray; const LowToHi: Boolean);
procedure SortTPAByY(var Arr: TPointArray; const LowToHi: Boolean);
function FindTPARows(const TPA: TPointArray): T2DPointArray;
function FindTPAColumns(const TPA: TPointArray): T2DPointArray;
procedure SortTPAFrom(var Arr: TPointArray; const From: TPoint);
procedure SortATPAFromFirstPoint(var Arr: T2DPointArray; const From: TPoint);
procedure SortATPAFromMidPoint(var Arr: T2DPointArray; const From: TPoint);
procedure SortATPAFromFirstPointX(var Arr: T2DPointArray; const From: TPoint);
procedure SortATPAFromFirstPointY(var Arr: T2DPointArray; const From: TPoint);
function MiddleTPAEx(const TPA: TPointArray; out X, Y: Integer): Boolean;
function MiddleTPA(const TPA: TPointArray): TPoint;
function MedianTPAEx(const TPA: TPointArray; out X, Y: Integer): Boolean;
function MedianTPA(const TPA: TPointArray): TPoint;
procedure SortATPASize(var Arr: T2DPointArray; const BigFirst: Boolean);
procedure SortATPAFromSize(var Arr: T2DPointArray; const Size: Integer; CloseFirst: Boolean);
procedure FilterTPAsBetween(var atpa: T2DPointArray; const minLength, maxLength: integer);
function SplitTPAEx(const arr: TPointArray; w, h: Integer): T2DPointArray;
function SplitTPA(const arr: TPointArray; Dist: Integer): T2DPointArray;
function ClusterTPAEx(const TPA: TPointArray; width, height: Integer): T2DPointArray;
function ClusterTPA(const TPA: TPointArray; dist: Integer): T2DPointArray;
procedure FilterPointsPie(var Points: TPointArray; const SD, ED, MinR, MaxR: Extended; Mx, My: Integer; Natural: Boolean);
procedure FilterPointsDist(var Points: TPointArray; const MinDist,MaxDist: Extended; Mx, My: Integer);
procedure FilterPointsLine(var Points: TPointArray; Radial: Extended; Radius, MX, MY: Integer);
procedure FilterPointsBox(var points: TPointArray; x1, y1, x2, y2: integer);
function GetATPABounds(const ATPA: T2DPointArray): TBox;
function GetTPABounds(const TPA: TPointArray): TBox;
function GetSamePointsATPA(const ATPA: T2DPointArray; var Matches: TPointArray): Boolean;
procedure SortCircleWise(var tpa: TPointArray; const cx, cy, StartDegree: Integer; SortUp, ClockWise: Boolean);
procedure LinearSort(var tpa: TPointArray; cx, cy, sd: Integer; SortUp: Boolean);
function MergeATPA(const ATPA : T2DPointArray): TPointArray;
function TPAFromLine(X1, Y1, X2, Y2: Integer; Thickness: Integer = 1): TPointArray; overload;
function TPAFromLine(P1, P2: TPoint; Thickness: Integer = 1): TPointArray; overload;
function EdgeFromBox(const Box: TBox): TPointArray;
function TPAFromBox(const Box : TBox) : TPointArray;
function TPAFromEllipse(const CX, CY, XRadius, YRadius : Integer): TPointArray;
function TPAFromCircle(const CX, CY, Radius: Integer): TPointArray;
function TPAFromPolygon(const Shape: TPointArray): TPointArray;
procedure FillEllipse(var TPA: TPointArray);
function FindTPAEdges(const TPA: TPointArray): TPointArray;
function TPAErode(const TPA: TPointArray; Amount: Int32): TPointArray;
function TPAGrow(const TPA: TPointArray; Amount: Int32): TPointArray;
function ClearTPAFromTPA(const Points, ClearPoints: TPointArray): TPointArray;
procedure ClearSamePoints(var TPA: TPointArray);
function ReturnPointsNotInTPA(const TPA: TPointArray; Area: TBox): TPointArray;
function SameTPA(const aTPA, bTPA: TPointArray): Boolean;
procedure OffsetTPA(var TPA : TPointArray; const Offset: TPoint);
procedure OffsetATPA(var ATPA : T2DPointArray; const Offset: TPoint);
function PartitionTPA(const TPA:TPointArray; BoxWidth, BoxHeight: Integer): T2DPointArray;
function PointsInRangeOf(const Points, Other: TPointArray; MinDist, MaxDist: Double): TPointArray; overload;
function PointsInRangeOf(const Points, Other: TPointArray; MinDistX, MinDistY, MaxDistX, MaxDistY: Double): TPointArray; overload;

implementation

uses
  math,
  simba.math, simba.slacktree, simba.overallocatearray,
  simba.helpers_matrix, simba.generics_array;

function PointsInRangeOf(const Points, Other: TPointArray; MinDist, MaxDist: Double): TPointArray; overload;
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

function PointsInRangeOf(const Points, Other: TPointArray; MinDistX, MinDistY, MaxDistX, MaxDistY: Double): TPointArray; overload;
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

procedure SortTPAByX(var Arr: TPointArray; const LowToHi: Boolean);
var
  I: Integer;
  Weights: TIntegerArray;
begin
  if (Length(Arr) = 0) then
    Exit;

  SetLength(Weights, Length(Arr));
  for I := 0 to High(Arr) do
    Weights[I] := Arr[I].X;

  Sort(Arr, Weights, LowToHi);
end;

procedure SortTPAByY(var Arr: TPointArray; const LowToHi: Boolean);
var
  I: Integer;
  Weights: TIntegerArray;
begin
  if (Length(Arr) = 0) then
    Exit;

  SetLength(Weights, Length(Arr));
  for I := 0 to High(Arr) do
    Weights[I] := Arr[I].Y;

  Sort(Arr, Weights, LowToHi);
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
procedure SortTPAFrom(var Arr: TPointArray; const From: TPoint);
var
  I: Integer;
  Weights: TIntegerArray;
begin
  if Length(Arr) = 0 then
    Exit;

  SetLength(Weights, Length(Arr));
  for i := 0 to High(Arr) do
    Weights[i] := Round(Sqr(From.X - Arr[i].X) + Sqr(From.Y - Arr[i].Y));
  Sort(Arr, Weights, True);
end;

{/\
  Sorts the T2DPointArray a from the point From.
/\}
procedure SortATPAFromFirstPoint(var Arr: T2DPointArray; const From: TPoint);
var
  I: Integer;
  Weights: TIntegerArray;
begin
  if (Length(Arr) = 0) then
    Exit;

  SetLength(Weights, Length(Arr));
  for i := 0 to High(Arr) do
    if (Length(Arr[I]) > 0) then
      Weights[i] := Round(Sqr(From.X - Arr[i][0].X) + Sqr(From.Y - Arr[i][0].Y));

  Sort(Arr, Weights, True);
end;

{/\
  Sorts the T2DPointArray a from the midpoint of each TPA by From.
/\}
procedure SortATPAFromMidPoint(var Arr: T2DPointArray; const From: TPoint);
var
  I: Integer;
  Weights: TIntegerArray;
begin
  if (Length(Arr) = 0) then
    Exit;

  SetLength(Weights, Length(Arr));
  for I := 0 to High(Arr) do
    with MiddleTPA(Arr[I]) do
      Weights[I] := Round(Sqr(From.X - X) + Sqr(From.Y - Y));

  Sort(Arr, Weights, True);
end;

{/\
  Sorts the T2DPointArray a from the first X point of each TPA by from.
/\}
procedure SortATPAFromFirstPointX(var Arr: T2DPointArray; const From: TPoint);
var
  I: Integer;
  Weights: TIntegerArray;
begin
  if (Length(Arr) = 0) then
    Exit;

  SetLength(Weights, Length(Arr));
  for I := 0 to High(Arr) do
    if (Length(Arr[i]) > 0) then
      Weights[I] := Round(Sqr(From.X - Arr[i][0].X));

  Sort(Arr, Weights, True);
end;

{/\
  Sorts the T2DPointArray a from the first Y point of each TPA by from.
/\}
procedure SortATPAFromFirstPointY(var Arr: T2DPointArray; const From: TPoint);
var
  I: Integer;
  Weights: TIntegerArray;
begin
  if (Length(Arr) = 0) then
    Exit;

  SetLength(Weights, Length(Arr));
  for I := 0 to High(Arr) do
    if (Length(Arr[i]) > 0) then
      Weights[I] := Round(Sqr(From.Y - Arr[i][0].Y));

  Sort(Arr, Weights, True);
end;

{/\
  Stores the coordinates of the middle of the TPointArray a to X and Y.
/\}
function MiddleTPAEx(const TPA: TPointArray; out X, Y: Integer): Boolean;
var
  P: TPoint;
begin
  Result := Length(TPA) > 0;

  if Result then
  begin
    P := MiddleTPA(TPA);

    X := P.X;
    Y := P.Y;
  end;
end;

{/\
  Returns the middle of the TPointArray TPA.
/\}
function MiddleTPA(const TPA: TPointArray): TPoint;
var
  I, Len: Integer;
begin
  Result := Default(TPoint);

  Len := Length(TPA);
  if (Len > 0) then
  begin
    for I := 0 to Len - 1 do
    begin
      Result.X := Result.X + TPA[I].X;
      Result.Y := Result.Y + TPA[I].Y;
    end;

    Result.X := Result.X div Len;
    Result.Y := Result.Y div Len;
  end;
end;

{/\
  Returns the x and y coords of the point in the TPA which is closest to the middle of the TPA.
/\}
function MedianTPAEx(const TPA: TPointArray; out X, Y: Integer): Boolean;
var
  P: TPoint;
begin
  Result := Length(TPA) > 0;

  if Result then
  begin
    P := MedianTPA(TPA);

    X := P.X;
    Y := P.Y;
  end;
end;

{/\
  Returns the *point in the TPA* closest to the middle of the TPA.
/\}
function MedianTPA(const TPA: TPointArray): TPoint;
begin
  MedianTPAEx(TPA, Result.X, Result.Y);
end;

{/\
  Sorts the T2DPointArray a from either largest or smallest, by the amount of points in the TPAs.
/\}
procedure SortATPASize(var Arr: T2DPointArray; const BigFirst: Boolean);
var
  I: Integer;
  Weights: TIntegerArray;
begin
  if (Length(Arr) = 0) then
    Exit;

  SetLength(Weights, Length(Arr));
  for I := 0 to High(Arr) do
    Weights[I] := Length(Arr[I]);

  Sort(Arr, Weights, not BigFirst);
end;

procedure SortATPAFromSize(var Arr: T2DPointArray; const Size: Integer; CloseFirst: Boolean);
var
  I: Integer;
  Weights: TIntegerArray;
begin
  if (Length(Arr) = 0) then
    Exit;

  SetLength(Weights, Length(Arr));
  for I := 0 to High(Arr) do
    Weights[I] :=  Abs(Length(Arr[I]) - Size);

  Sort(Arr, Weights, CloseFirst);
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

procedure FilterPointsDist(var Points: TPointArray; const MinDist, MaxDist: Extended; Mx, My: Integer);
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
  begin
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
  Returns the boundaries of the ATPA as a TBox.
/\}
function GetATPABounds(const ATPA: T2DPointArray): TBox;

  function HasPoints(var B: TBox): Boolean;
  var
    I: Integer;
  begin
    Result := False;

    for I := 0 to High(ATPA) do
      if Length(ATPA[I]) > 0 then
      begin
        B.X1 := ATPA[I][0].X;
        B.Y1 := ATPA[I][0].Y;
        B.X2 := ATPA[I][0].X;
        B.Y2 := ATPA[I][0].Y;

        Exit(True);
      end;
  end;

var
  I: Integer;
  TPA: TPointArray;
begin
  Result := Default(TBox);

  if HasPoints(Result) then
    for TPA in ATPA do
    begin
      for I := 0 to High(TPA) do
      begin
        if (TPA[I].X > Result.X2) then Result.X2 := TPA[I].X else
        if (TPA[I].X < Result.X1) then Result.X1 := TPA[I].X;

        if (TPA[I].Y > Result.Y2) then Result.Y2 := TPA[I].Y else
        if (TPA[I].Y < Result.Y1) then Result.Y1 := TPA[I].Y;
      end;
    end;
end;

{/\
  Returns the boundaries of the TPA as a TBox.
/\}
function GetTPABounds(const TPA: TPointArray): TBox;
begin
  Result := GetATPABounds([TPA]);
end;

{/\
  Finds the points that exist in all TPA's in the ATPA.
/\}
function GetSamePointsATPA(const ATPA: T2DPointArray; var Matches: TPointArray): Boolean;
var
  Matrix: TIntegerMatrix;
  I, J: Integer;
  TPA: TPointArray;
  Arr: specialize TSimbaOverAllocateArray<TPoint>;
begin
  Matches := Default(TPointArray);

  if (Length(ATPA) > 1) then
  begin
    with GetATPABounds(ATPA) do
    begin
      Arr.Init();

      Matrix.SetSize(Width, Height);
      for I := 0 to High(ATPA) - 1 do
      begin
        TPA := ATPA[I];
        for J := 0 to High(TPA) do
          Matrix[TPA[J].Y - Y1, TPA[J].X - X1] += 1;
      end;

      J := High(ATPA);

      TPA := ATPA[J];
      for I := 0 to High(TPA) do
        if Matrix[TPA[I].Y - Y1, TPA[I].X - X1] = J then
          Arr.Add(TPA[I]);

      Matches := Arr.Trim();
    end;
  end;

  Result := Length(Matches) > 0;
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
  Sort(TPA, Dist, SortUp);
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
    Sort(TPA, Deg, td, i, False);
    Inc(i);
    td := i;
  end;
  if (td < l) then
    Sort(TPA, Deg, td, l, False);
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
  Sort(TPA, Deg, 0, l -1, True);
  if (l = 1) then Exit;
  for i := 0 to l -1 do
    Dist[i] := Round(Hypot(tpa[i].x - cx, tpa[i].y - cy));
  i := 1;
  td := 0;
  Dec(l);
  while (i < l) do
  begin
    while (i < l) and (Abs(Deg[td] - Deg[i]) <= 3) do Inc(i);
    Sort(TPA, Dist, td, i, SortUp);
    Inc(i);
    td := i;
  end;
  if (td < l) then
    Sort(TPA, Dist, td, l, SortUp);
end;

{/\
  Merges the TPointArrays of the T2DPointArray ATPA in to one TPA.
/\}
function MergeATPA(const ATPA: T2DPointArray): TPointArray;
var
  I, Len: Integer;
  Arr: specialize TSimbaOverAllocateArray<TPoint>;
begin
  Result := Default(TPointArray);

  Len := 0;
  for I := 0 to High(ATPA) do
    Len += Length(ATPA[I]);

  if (Len > 0) then
  begin
    Arr.Init(Len);
    for I := 0 to High(ATPA) do
      Arr.Add(ATPA[I]);

    Result := Arr.Trim();
  end;
end;

function TPAFromLine(X1, Y1, X2, Y2: Integer; Thickness: Integer): TPointArray;
var
  dx,dy,step,I: Integer;
  rx,ry,x,y: Single;
begin
  dx := (x2 - x1);
  dy := (y2 - y1);
  if (Abs(dx) > Abs(dy)) then
    step := Abs(dx)
  else
    step := Abs(dy);

  SetLength(Result, step + 1);

  rx := dx / step;
  ry := dy / step;
  x := x1;
  y := y1;

  Result[0] := Point(x1, y1);
  for I := 1 to step do
  begin
    x := x + rx;
    y := y + ry;

    Result[i] := Point(Round(x), Round(y));
  end;

  if (Thickness - 1 > 0) then
    Result := TPAGrow(Result, Thickness - 1);
end;

function TPAFromLine(P1, P2: TPoint; Thickness: Integer): TPointArray;
begin
  Result := TPAFromLine(P1.X, P1.Y, P2.X, P2.Y, Thickness);
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
begin
  SetLength(Result, (Box.x2 - Box.x1 + 1) * (Box.y2 - Box.y1 + 1));
  l := 0;
  For x := box.x1 to Box.x2 do
    for y := box.y1 to box.y2 do
    begin
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
  Removes the given ClearPoints from arP.
/\}
function ClearTPAFromTPA(const Points, ClearPoints: TPointArray): TPointArray;
var
  Matrix: TBooleanMatrix;
  B: TBox;
  I: Integer;
  Res: specialize TSimbaOverAllocateArray<TPoint>;
begin
  Result := Default(TPointArray);
  if (Length(Points) = 0) then
    Exit;
  if (Length(ClearPoints) = 0) then
    Exit(Copy(Points));

  B := GetATPABounds([Points, ClearPoints]);

  Matrix.SetSize(B.Width, B.Height);
  for I := 0 to High(ClearPoints) do
    Matrix[ClearPoints[I].Y - B.Y1, ClearPoints[I].X - B.X1] := True;

  Res.Init(Length(Points));
  for I := 0 to High(Points) do
    if not Matrix[Points[I].Y - B.Y1, Points[I].X - B.X1] then
      Res.Add(Points[I]);

  Result := Res.Trim();
end;

{/\
  Removes all the doubles point from a TPA.
/\}
procedure ClearSamePoints(var TPA: TPointArray);
begin
  TPA := Unique(TPA);
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

procedure OffsetTPA(var TPA: TPointArray; const Offset: TPoint);
var
  I: Integer;
begin
  for I := High(TPA) downto 0 do
  begin
    Inc(TPA[I].X, Offset.X);
    Inc(TPA[I].Y, Offset.Y);
  end;
end;

procedure OffsetATPA(var ATPA: T2DPointArray; const Offset: TPoint);
var
  I: Integer;
begin
  for I := High(ATPA) downto 0 do
    OffsetTPA(ATPA[I], Offset);
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

end.

