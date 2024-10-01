{==============================================================================]
  Copyright (c) 2021, Jarl `slacky` Holta
  Project: SlackTree
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
[==============================================================================}
unit simba.container_slacktree;

{$DEFINE SIMBA_MAX_OPTIMIZATION}
{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.vartype_box;

type
  (*
    SlackTree is a static tree for quick "spatial lookups".
    Allows quick nearest neighbour lookup, and range-query.

    Note: Might still exists some bugs here.
  *)
  PNode = ^TNode;
  TNode = record
    split: TPoint;
    l,r: Int32;
    hidden: Boolean;
  end;

  TNodeArray = array of TNode;
  TNodeRefArray = array of PNode;

  TSlackTree = record
  public
    data: TNodeArray;
    size: Int32;
  private
    function GetItem(i:Int32): PNode; inline;
  public
    hidden: Integer;

    property items[i:Int32]: PNode read GetItem; default;

    function InitBranch: Int32; inline; //dummy
    function Copy: TSlackTree;

    procedure Init(TPA:TPointArray);

    function IndexOf(p:TPoint): Int32;
    function Find(p:TPoint): PNode;

    procedure HideNode(idx:Int32);
    function HideNode(pt:TPoint): Boolean; overload;

    function RawNearest(pt:TPoint; notEqual:Boolean=False): PNode;
    function Nearest(pt:TPoint; notEqual:Boolean=False): TPoint;

    function RawKNearest(pt:TPoint; k:Int32; notEqual:Boolean=False): TNodeRefArray;
    function KNearest(pt:TPoint; k:Int32; notEqual:Boolean=False): TPointArray;

    function RawRangeQuery(B:TBox): TNodeRefArray;
    function RangeQuery(B:TBox; hide:Boolean=False): TPointArray;
    function RangeQueryEx(query:TPoint; xRad,yRad:Double; hide:Boolean=False): TPointArray;
    function RangeQueryEx(query:TPoint; xmin,ymin,xmax,ymax:Double; hide:Boolean=False): TPointArray; overload;

    function RefArray: TNodeRefArray;
  end;

  PSlackTree = ^TSlackTree;

implementation

uses
  Math;

const
  NONE = -1;

function TPASelectNth_Axis(var arr:TPointArray; k, start, stop:Int32; axis:Byte=0): TPoint;
var
  l,r:Int32;
  tmp,mid:TPoint;
begin
  if stop-start < 0 then Exit(TPoint.ZERO);

  while (start < stop) do
  begin
    l := start;
    r := stop;
    mid := arr[(l + r) div 2];
    if axis = 0 then
    begin
      while (l < r) do
        if (arr[l].x >= mid.x) then
        begin
          tmp := arr[r];
          arr[r] := arr[l];
          arr[l] := tmp;
          Dec(r);
        end else
          Inc(l);
    end else
      while (l < r) do
        if (arr[l].y >= mid.y) then
        begin
          tmp := arr[r];
          arr[r] := arr[l];
          arr[l] := tmp;
          Dec(r);
        end else
          Inc(l);

    if (axis = 0) and (arr[l].x > mid.x) then Dec(l);
    if (axis = 1) and (arr[l].y > mid.y) then Dec(l);
    if (k <= l) then stop := l else start := l + 1;
  end;

  Result := arr[k];
end;


function TSlackTree.RefArray: TNodeRefArray;
var
  i:Int32;
begin
  SetLength(Result, Length(self.data));
  for i:=0 to High(self.data) do Result[i] := @self.data[i];
end;


function TSlackTree.GetItem(i:Int32): PNode;
begin
  Result := @self.data[i];
end;


function TSlackTree.InitBranch: Int32;
begin
  Result := self.size;
  with self.data[result] do
  begin
    L := -1;
    R := -1;
    hidden := False;
  end;
  Inc(self.size);
end;

function TSlackTree.Copy: TSlackTree;
begin
  Result.Data := System.Copy(Data);
  Result.Size := Size;
end;

procedure TSlackTree.Init(TPA:TPointArray);
  procedure __build(var node:TNode; left, right:Int32; depth:Int32=0);
  var mid: Int32;
  begin
    if (right-left < 0) then Exit(); // just nil back up..

    mid := (right+left) shr 1;
    node.split := TPASelectNth_Axis(TPA, mid, left, right, depth and 1);

    if mid-left > 0 then begin           //lower half
      node.L := self.InitBranch();
      __build(self.data[node.L], left,mid-1, depth+1);
    end;

    if right-mid > 0 then begin          //upper half
      node.R := self.InitBranch();
      __build(self.data[node.R], mid+1,right, depth+1);
    end;
  end;
begin
  Self.Size := 0;
  SetLength(self.data, Length(TPA));
  __build(self.data[InitBranch()], 0, High(TPA));
end;


function TSlackTree.IndexOf(p:TPoint): Int32;
  function __find(idx:Int32; depth:Int32=0): Int32;
  var
    s:Int32;
    this:PNode;
  begin
    this := Self[idx];
    if (this^.split = p) then
      Exit(idx);

    Result := NONE;
    if (depth and 1 = 0) then
      s := Sign(this^.split.x - p.x)
    else
      s := Sign(this^.split.y - p.y);

    case s of
       1: if this^.L <> NONE then Exit(__find(this^.L, depth+1));
      -1: if this^.R <> NONE then Exit(__find(this^.R, depth+1));
       0: begin
            if this^.R <> NONE then Result := __find(this^.R, depth+1);
            if (Result = NONE) and (this^.L <> NONE) then Result := __find(this^.L, depth+1);
            Exit();
          end;
    end;
  end;
begin
  Result := __find(0,0);
end;

function TSlackTree.Find(p:TPoint): PNode;
var i:Int32;
begin
  Result := nil;
  i := Self.IndexOf(p);
  if i <> NONE then Result := @self.data[i];
end;

procedure TSlackTree.HideNode(idx:Int32);
begin
  Self[idx]^.hidden := True;
end;

function TSlackTree.HideNode(pt:TPoint): Boolean; overload;
var idx:Int32;
begin
  idx := self.IndexOf(pt);
  Result := idx <> NONE;
  if result then
    self.data[idx].hidden := True;
end;

function TSlackTree.RawNearest(pt:TPoint; notEqual:Boolean=False): PNode;
var
  resDist:Int32;
  resNode:PNode;
  procedure __nearest(node:Int32; depth:UInt8=0);
  var
    test,dist,delta:Int32;
    this:PNode;
  begin
    this := @self.data[node];

    if depth and 1 = 0 then
      delta := this^.split.x - pt.x
    else
      delta := this^.split.y - pt.y;

    if not this^.hidden then
    begin
      dist := Sqr(this^.split.x - pt.x) + Sqr(this^.split.y - pt.y);
      if (dist < resDist) and not((dist = 0) and notEqual) then
      begin
        resDist := dist;
        resNode := this;
      end;
      if resDist = 0 then Exit();
    end;

    if delta > 0 then test := this^.l else test := this^.r;
    if (test <> NONE) then
      __nearest(test, depth+1);

    if (Sqr(delta) >= resDist) then Exit();

    if delta > 0 then test := this^.r else test := this^.l;
    if (test <> NONE) then
      __nearest(test, depth+1);
  end;

begin
  resDist := High(Int32);
  resNode := nil;
  __nearest(0);
  Result := resNode;
end;


function TSlackTree.Nearest(pt:TPoint; notEqual:Boolean=False): TPoint;
var tmp:PNode;
begin
  tmp := self.RawNearest(pt, notEqual);
  if tmp <> nil then
    Result := tmp^.split
  else
    Result := Point(-1,-1);
end;


function TSlackTree.RawKNearest(pt:TPoint; k:Int32; notEqual:Boolean=False): TNodeRefArray;
var i,c:Int32;
begin
  SetLength(Result, k);
  c := 0;
  while c < k do
  begin
     Result[c] := self.RawNearest(pt, notEqual);
     if Result[c] = nil then break;
     Result[c]^.hidden := True;
     inc(c);
  end;

  for i:=0 to c-1 do Result[i]^.hidden := False;
  SetLength(Result, c);
end;


function TSlackTree.KNearest(pt:TPoint; k:Int32; notEqual:Boolean=False): TPointArray;
var
  arr:TNodeRefArray;
  i:Int32;
begin
  arr := RawKNearest(pt,k,notEqual);
  SetLength(result, Length(arr));
  for i:=0 to High(arr) do result[i] := arr[i]^.split;
end;


function TSlackTree.RawRangeQuery(B:TBox): TNodeRefArray;
var
  res_len, res_count: Int32;
  procedure __query(node:Int32; var res:TNodeRefArray; depth:Int32=0);
  var
    goright, goleft:Boolean;
    this: PNode;
  begin
    goleft  := False;
    goright := False;
    this := @self.data[node];
    if depth and 1 = 0 then begin
      goleft  := B.x1 <= this^.split.x;
      goright := B.x2 >= this^.split.x;
    end else begin
      goleft  := B.y1 <= this^.split.y;
      goright := B.y2 >= this^.split.y;
    end;

    if (not this^.hidden) and
       (B.x1 <= this^.split.x) and (B.x2 >= this^.split.x) and
       (B.y1 <= this^.split.y) and (B.y2 >= this^.split.y) then
    begin
      res[res_count] := this;
      inc(res_count);

      if res_count = res_len then
      begin
        res_len := res_len*2;
        Setlength(res, res_len);
      end;
    end;

    if goleft and (this^.l <> NONE) then
      __query(this^.l, res, depth+1);

    if goright and (this^.r <> NONE) then
      __query(this^.r, res, depth+1);
  end;
begin
  res_len := 1024;
  res_count := 0;
  SetLength(result, res_len);
  __query(0, result);
  SetLength(result, res_count);
end;


function TSlackTree.RangeQuery(B:TBox; hide:Boolean=False): TPointArray;
var
  nodes:TNodeRefArray;
  i:Int32;
begin
  nodes := self.RawRangeQuery(B);
  SetLength(Result, length(nodes));
  for i:=0 to High(nodes) do
  begin
    Result[i] := Nodes[i]^.split;
    if hide then
    begin
      Nodes[i]^.hidden := True;
      inc(hidden);
    end;
  end;
end;


function TSlackTree.RangeQueryEx(query: TPoint; xRad, yRad: Double; hide: Boolean): TPointArray;
var
  i,c:Int32;
  nodes:TNodeRefArray;
  sqx,sqy,xxyy:Single;
  pt:TPoint;
begin
  nodes := self.RawRangeQuery(
    TBox.Create(query.x-Trunc(xrad), query.y-Trunc(yrad), query.x+Ceil(xrad), query.y+Ceil(yrad))
  );
  sqx := Sqr(xrad);
  sqy := Sqr(yrad);
  SetLength(Result, length(nodes));
  c := 0;
  if xrad <> yrad then
  begin
    xxyy := SqX*SqY;
    for i:=0 to High(nodes) do
    begin
      pt := Nodes[i]^.split;
      if Sqr(pt.X-query.x) * SqY + Sqr(pt.Y-query.y) * SqX <= xxyy then
      begin
        Result[c] := pt;
        Inc(c);
        if hide then Nodes[i]^.hidden := True;
      end;
    end;
  end else
    for i:=0 to High(nodes) do
    begin
      pt := Nodes[i]^.split;
      if Sqr(pt.x-query.x) + Sqr(pt.y-query.y) <= SqX then
      begin
        Result[c] := pt;
        Inc(c);
        if hide then Nodes[i]^.hidden := True;
      end;
    end;
  SetLength(Result, c);
end;


function TSlackTree.RangeQueryEx(query:TPoint; xmin,ymin,xmax,ymax:Double; hide:Boolean=False): TPointArray; overload;
var
  i,c:Int32;
  nodes:TNodeRefArray;
  hisqx,hisqy,hixxyy,losqx,losqy,loxxyy:Single;
  pt:TPoint;
begin
  nodes := self.RawRangeQuery(
    TBox.Create(query.x-Trunc(xmax), query.y-Trunc(ymax), query.x+Ceil(xmax), query.y+Ceil(ymax))
  );

  SetLength(Result, length(nodes));
  c := 0;

  hisqx := Sqr(xmax);
  hisqy := Sqr(ymax);
  hixxyy := hisqx*hisqy;

  losqx := Sqr(xmin);
  losqy := Sqr(ymin);
  loxxyy := losqx*losqy;

  for i:=0 to High(nodes) do
  begin
    pt := Nodes[i]^.split;
    if (Sqr(pt.X-query.x) * hisqy + Sqr(pt.Y-query.y) * hisqx <= hixxyy) and
       (Sqr(pt.X-query.x) * losqy + Sqr(pt.Y-query.y) * losqx >= loxxyy) then
    begin
      Result[c] := pt;
      Inc(c);
      if hide then Nodes[i]^.hidden := True;
    end;
  end;
  SetLength(Result, c);
end;

end.

