{==============================================================================]
  Copyright (c) 2024, Jarl `slacky` Holta
  Project: SlackTree
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
[==============================================================================}
unit simba.container_kdtree;

{$DEFINE SIMBA_MAX_OPTIMIZATION}
{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base;

const
  NONE = -1;

type
  (*
    KDTree is a static tree for quick "spatial lookups" that also contains `ref` per item added 
    which means it can be used for simple classifcation for example.
  *)
  ETreeSetting = (tsNotEqual, tsHideResult, tsRefSensitive, tsIgnoreHidden);
  TTreeSettings = set of ETreeSetting;

  TKDItem = record
    Ref: Integer;
    Vector: TSingleArray;
  end;

  TKDItems = array of TKDItem;
  T2DKDItems = array of TKDItems;

  PKDNode = ^TKDNode;
  TKDNode = record
    Split: TKDItem;
    L, R: Integer;
    Hidden: Boolean;
  end;

  TKDNodeArray = array of TKDNode;
  TKDNodeRefArray = array of PKDNode;

  TKDTree = record
    Dimensions: Integer;
    Data: TKDNodeArray;
    Size: Integer;

    class function Create(const AData: TKDItems): TKDTree; static; overload;
    class function Create(const FileName: string): TKDTree; static; overload;
    function SaveToFile(FileName: string): Boolean;

    function RefArray(): TKDNodeRefArray;
    function GetItem(i:Integer): PKDNode;
    function InitBranch(): Integer; inline;
    function Copy(): TKDTree;
    function SqDistance(A, B: TSingleArray; Limit: Single = High(UInt32)): Single; inline;
    function IndexOf(const Value: TSingleArray): Integer;
    function KNearest(Vector: TSingleArray; K: Integer; Setting: TTreeSettings=[]; WorkingRef:Int32=NONE): TKDItems;
    function KNearestIndex(Vector: TSingleArray; K: Integer; Setting: TTreeSettings=[]; WorkingRef:Int32=NONE): TIntegerArray;
    function RangeQuery(Low, High: TSingleArray; Setting: TTreeSettings = []; WorkingRef: Integer = NONE): TKDItems;
    function RangeQueryEx(Center: TSingleArray; Radii: TSingleArray; Setting: TTreeSettings = []; WorkingRef: Integer = NONE): TKDItems;
    function KNearestClassify(Vector: TSingleArray; K: Integer): Integer;
    function WeightedKNearestClassify(Vector: TSingleArray; K: Integer): Integer;
    function Clusters(Radii: TSingleArray): T2DKDItems;
  end;

  PKDTree = ^TKDTree; 
  

implementation

uses
  simba.fs;

(*
  Quick select for the KDTree build process
*)
function QuickSelectOnAxis(var Arr:TKDItems; k, Start, Stop: Integer; Axis: Integer): TKDItem;
var
  l, r: Integer;
  tmp: TKDItem;
  pivot: Single;
begin
  while Start < Stop do
  begin
    l := Start;
    r := Stop;
    pivot := Arr[(l + r) shr 1].Vector[axis];

    while l <= r do
    begin
      while Arr[l].Vector[Axis] < pivot do Inc(l);
      while Arr[r].Vector[Axis] > pivot do Dec(r);
      if l <= r then
      begin
        tmp    := Arr[l];
        Arr[l] := Arr[r];
        Arr[r] := tmp;
        Inc(l);
        Dec(r);
      end;
    end;

    if      k <= r then Stop  := r
    else if k >= l then Start := l
    else                break;
  end;

  Result := Arr[k];
end;


function TKDTree.RefArray(): TKDNodeRefArray;
var
  i:Integer;
begin
  SetLength(Result, Length(self.data));
  for i:=0 to High(self.data) do Result[i] := @self.data[i];
end;


function TKDTree.GetItem(i:Integer): PKDNode;
begin
  Result := @self.data[i];
end;


function TKDTree.InitBranch: Integer;
begin
  Result := self.size;
  with self.data[result] do
  begin
    L := -1;
    R := -1;
    Hidden := False;
  end;
  Inc(self.size);
end;


function TKDTree.Copy: TKDTree;
begin
  Result.Data       := System.Copy(Self.Data);
  Result.Dimensions := Self.Dimensions;
  Result.Size       := Self.Size;
end;


function TKDTree.SqDistance(A, B: TSingleArray; Limit: Single = High(UInt32)): Single;
var i: Integer;
begin
  Result := 0;
  for i:=0 to High(a) do
  begin
    Result += Sqr(a[i]-b[i]);
    if Result > Limit then Exit;
  end;
end;

class function TKDTree.Create(const AData: TKDItems): TKDTree;
var
  Sortable: TKDItems;

  procedure BuildTree(var Node: TKDNode; Left, Right:Integer; Depth:Integer=0);
  var
    Mid: Integer;
  begin
    if (Right-Left < 0) then Exit();     // just nil back up..

    Mid := (Right + left) shr 1;
    Node.Split := QuickSelectOnAxis(Sortable, Mid, Left, Right, depth mod Result.Dimensions);

    if Mid-left > 0 then begin           //lower half
      Node.L := Result.InitBranch();
      BuildTree(Result.Data[Node.L], left,mid-1, depth+1);
    end;

    if Right-Mid > 0 then begin          //upper half
      Node.R := Result.InitBranch();
      BuildTree(Result.Data[Node.R], mid+1,right, depth+1);
    end;
  end;
begin
  Result := Default(TKDTree);
  if Length(AData) = 0 then
    Exit;

  Sortable := System.Copy(AData);

  Result.Dimensions := Length(AData[0].Vector);
  Result.Size := 0;

  SetLength(Result.Data, Length(Sortable));
  BuildTree(Result.Data[Result.InitBranch()], 0, High(Sortable));
end;

class function TKDTree.Create(const FileName: string): TKDTree;
var
  bytes: TByteArray;
  i,j,c: Integer;
begin
  Result := Default(TKDTree);

  bytes := TSimbaFile.FileReadBytes(FileName);
  Move(bytes[0], Result.Dimensions, 4);
  Move(bytes[4], Result.Size,       4);
  c := 8;

  SetLength(Result.Data, Result.Size);
  for i:=0 to Result.Size-1 do
  begin
    Result.Data[i].Hidden := Boolean(bytes[c]);
    Inc(c);
    Move(bytes[c], Result.Data[i].L, 4);
    Inc(c, 4);
    Move(bytes[c], Result.Data[i].R, 4);
    Inc(c, 4);
    Move(bytes[c], Result.Data[i].Split.Ref, 4);
    Inc(c, 4);

    SetLength(Result.Data[i].Split.Vector, Result.Dimensions);
    for j:=0 to Result.Dimensions-1 do
    begin
      Move(bytes[c], Result.Data[i].Split.Vector[j], 4);
      Inc(c, 4);
    end;
  end;
end;



function TKDTree.SaveToFile(FileName: string): Boolean;
var
  bytes: TByteArray;
  i,j,c: Integer;
begin
  SetLength(bytes,
    4 +
    4 +
    Length(Self.Data) * (1+4+4+4+Self.Dimensions*SizeOf(Single)) +
    1
  );                   //h,l,r,^,vectors

  Move(self.Dimensions, bytes[0], 4);
  Move(self.Size,       bytes[4], 4);
  c := 8;

  for i:=0 to High(self.Data) do
  begin
    bytes[c] := Byte(self.Data[i].Hidden);
    Inc(c, 1);
    Move(self.Data[i].L, bytes[c], 4);
    Inc(c, 4);
    Move(self.Data[i].R, bytes[c], 4);
    Inc(c, 4);
    Move(self.Data[i].Split.Ref, bytes[c], 4);
    Inc(c, 4);

    for j:=0 to self.Dimensions-1 do
    begin
      Move(self.Data[i].Split.Vector[j], bytes[c], 4);
      Inc(c, 4);
    end;
  end;

  Result := TSimbaFile.FileWriteBytes(FileName, bytes);
end;




(*
  Returns an Index that can be used to access TKDTree.Data directly.

  Average Time Complexity: O(log n), worst case O(n)
*)
function TKDTree.IndexOf(const Value: TSingleArray): Integer;
const
  EPS = 0.000001;

  function FloatEqual(const A, B: Single): Boolean;
  begin
    Result := Abs(A - B) <= EPS;
  end;

  function FindNode(Node: Integer; Depth: Integer): Integer;
  var
    Axis, i: Integer;
    SplitVal, Val: Single;
    Equal: Boolean;
  begin
    if Node = NONE then
      Exit(NONE);

    Equal := True;
    for i := 0 to Self.Dimensions - 1 do
      if not FloatEqual(Self.Data[Node].Split.Vector[i], Value[i]) then
      begin
        Equal := False;
        Break;
      end;

    if Equal then
      Exit(Node);

    Axis := Depth mod Self.Dimensions;
    SplitVal := Self.Data[Node].Split.Vector[Axis];
    Val := Value[Axis];

    if FloatEqual(Val, SplitVal) then
    begin
      Result := FindNode(Self.Data[Node].L, Depth + 1);
      if Result = NONE then
        Result := FindNode(Self.Data[Node].R, Depth + 1);
    end
    else if Val < SplitVal then
      Result := FindNode(Self.Data[Node].L, Depth + 1)
    else
      Result := FindNode(Self.Data[Node].R, Depth + 1);
  end;
begin
  Result := FindNode(0, 0);
end;


// same as kNearest, but returns an array of indices.
function TKDTree.KNearestIndex(Vector: TSingleArray; K: Integer; Setting: TTreeSettings=[]; WorkingRef:Integer=NONE): TIntegerArray;
type
  TNearestItem = record
    Node: Integer;
    DistSq: Single;
  end;
  TNearestHeap = array of TNearestItem;

var
  Heap: TNearestHeap;

  procedure Heapify(Index: Integer);
  var
    Largest: Integer;
    Left, Right: Integer;
    Temp: TNearestItem;
  begin
    Largest := Index;
    Left    := 2 * Index + 1;
    Right   := 2 * Index + 2;

    if (Left < Length(Heap)) and (Heap[Left].DistSq > Heap[Largest].DistSq) then
      Largest := Left;

    if (Right < Length(Heap)) and (Heap[Right].DistSq > Heap[Largest].DistSq) then
      Largest := Right;

    if Largest <> Index then
    begin
      Temp := Heap[Index];
      Heap[Index] := Heap[Largest];
      Heap[Largest] := Temp;
      Heapify(Largest);
    end;
  end;

  procedure FindKNearest(Node: Integer; Depth: UInt8);
  var
    Delta, DistSq: Single;
    Test: Integer;
    This: PKDNode;
    Axis: Integer;
    Temp: TNearestItem;
    I: Integer;
  label
      NextNode;
  begin
    if Node = NONE then Exit;

    This := @Self.Data[Node];
    Axis := Depth mod Self.Dimensions;

    Delta := This^.Split.Vector[Axis] - Vector[Axis];

    if Length(Heap) < K then
      DistSq := Self.SqDistance(This^.Split.Vector, Vector, High(UInt32)) // No limit if heap is not full
    else
      DistSq := Self.SqDistance(This^.Split.Vector, Vector, Heap[0].DistSq); // Limit is the furthest distance in the heap

    if (tsRefSensitive in Setting) and (This^.Split.Ref <> WorkingRef) then
      goto NextNode;

    if (tsIgnoreHidden in Setting) and This^.Hidden then
      goto NextNode;

    if not((DistSq = 0) and (tsNotEqual in Setting)) then
    begin
      if Length(Heap) < K then
      begin
        // heap not full, add current node
        SetLength(Heap, Length(Heap) + 1);
        Heap[High(Heap)].Node := Node;
        Heap[High(Heap)].DistSq := DistSq;

        // heapify upwards
        i := High(Heap);
        while (i > 0) and (Heap[(i - 1) div 2].DistSq < Heap[i].DistSq) do
        begin
          Temp := Heap[i];
          Heap[i] := Heap[(i - 1) div 2];
          Heap[(i - 1) div 2] := Temp;
          i := (i - 1) div 2;
        end;
      end
      else
      begin
        if DistSq < Heap[0].DistSq then
        begin
          // replace the furthest node with the current node
          Heap[0].Node := Node;
          Heap[0].DistSq := DistSq;
          // heapify downwards
          Heapify(0);
        end;
      end;
    end;

  NextNode:

    if Delta > 0 then Test := This^.L else Test := This^.R;
    FindKNearest(Test, Depth + 1);

    if (Length(Heap) < K) or (Sqr(Delta) < Heap[0].DistSq) then
    begin
      if Delta > 0 then Test := This^.R else Test := This^.L;
      FindKNearest(Test, Depth + 1);
    end;
  end;

var
  i,j: Integer;
begin
  SetLength(Heap, 0);
  FindKNearest(0, 0);

  SetLength(Result, Length(Heap));
  j := 0;
  for i := High(Heap) downto 0 do
  begin
    Result[j] := Heap[i].Node;

    if (tsHideResult in Setting) then
      Self.Data[Heap[i].Node].Hidden := True;

    Inc(j);
  end;
end;

(*
  Returns the K nearest vectors to the input vector

  Average Time Complexity:    O(log n * log k)
  Worst Case Time Complexity: O(n)

  Note:
    The time complexity is typically closer to O(log n) when K is significantly smaller than n.
*)
function TKDTree.KNearest(Vector: TSingleArray; K: Integer; Setting: TTreeSettings; WorkingRef:Integer=NONE): TKDItems;
var
  arr: TIntegerArray;
  i: Int32;
begin
  arr := Self.KNearestIndex(Vector, k, setting, workingref);

  SetLength(Result, Length(arr));
  for i:=0 to High(arr) do
  begin
    Result[i] := Self.Data[arr[i]].Split;
  end;
end;


(*
  Returns the vectors that within the hyperrectangle defined by low and high vectors.

  Average Time Complexity:    O(log n + k)
  Worst Case Time Complexity: O(n^((d-1)/d) + k)

  Where k is the number of output points (not known ahead of time)
*)
function TKDTree.RangeQuery(Low, High: TSingleArray; Setting: TTreeSettings = []; WorkingRef: Integer = NONE): TKDItems;
var
  ResultSize: Integer;

  procedure Query(Node: Integer; Depth: UInt8);
  var
    This: PKDNode;
    Axis: Integer;
    i: Integer;
  begin
    if Node = NONE then Exit;

    This := @Self.Data[Node];
    Axis := Depth mod Self.Dimensions;

    for i := 0 to Self.Dimensions - 1 do
    begin
      if (This^.Split.Vector[i] < Low[i]) or (This^.Split.Vector[i] > High[i]) then
        Break; // break early to reduce dimensionality bottlenecking

      if i = Self.Dimensions - 1 then // completed = within range
      begin
        if (tsIgnoreHidden in Setting) and This^.Hidden then
          Exit;

        if (tsRefSensitive in Setting) and (This^.Split.Ref <> WorkingRef) then
          Exit;

        if ResultSize = Length(Result) then
          SetLength(Result, Length(Result) * 2);

        Result[ResultSize] := This^.Split;
        Inc(ResultSize);

        if tsHideResult in Setting then
          This^.Hidden := True;
      end;
    end;

    if (Low[Axis] <= This^.Split.Vector[Axis]) then
    begin
      if (High[Axis] < This^.Split.Vector[Axis]) then
      begin
        Query(This^.L, Depth + 1);
        Exit;
      end else
        Query(This^.L, Depth + 1);
    end;

    if (High[Axis] >= This^.Split.Vector[Axis]) then
    begin
      if (Low[Axis] > This^.Split.Vector[Axis]) then
      begin
        Query(This^.R, Depth + 1);
        Exit;
      end else
        Query(This^.R, Depth + 1);
    end;
  end;

begin
  SetLength(Result, 1024);
  ResultSize := 0;

  Query(0, 0);

  SetLength(Result, ResultSize);
end;


(*
  Returns the vectors thats within the query center and radii bounds defining the hypersphere.

  Average Time Complexity:    O(log n + k)
  Worst Case Time Complexity: O(n^((d-1)/d) + k)

  Where k is the number of output points (not known ahead of time)
*)
function TKDTree.RangeQueryEx(Center: TSingleArray; Radii: TSingleArray; Setting: TTreeSettings = []; WorkingRef: Integer = NONE): TKDItems;
var
  i, ResultSize: Integer;
  sqrProduct: Single;
  sqRadii: array of single;

  function Fits(const p,c: TSingleArray): Boolean; {inline; produces worse machinecode}
  var
    dsqr: Single;
    i: Integer;
  begin
    dsqr := 0;
    for i := 0 to High(p) do
    begin
      dsqr += (Sqr(p[i] - c[i]) * sqRadii[i]);
      if dsqr > sqrProduct then Exit(False);
    end;

    Result := dsqr <= sqrProduct;
  end;

  procedure Query(Node: Integer; Depth: UInt8);
  var
    This: PKDNode;
    Axis: Integer;
  begin
    if Node = NONE then Exit;

    This := @Self.Data[Node];
    Axis := Depth mod Self.Dimensions;

    if Fits(This^.Split.Vector, Center) then
    begin
      if (tsIgnoreHidden in Setting) and This^.Hidden then
        Exit;

      if (tsRefSensitive in Setting) and (This^.Split.Ref <> WorkingRef) then
        Exit;

      if ResultSize = Length(Result) then
        SetLength(Result, Length(Result) * 2);

      Result[ResultSize] := This^.Split;
      Inc(ResultSize);

      if tsHideResult in Setting then
        This^.Hidden := True;
    end;

    if (Center[Axis] - Radii[Axis] <= This^.Split.Vector[Axis]) then
        Query(This^.L, Depth + 1);

    if (Center[Axis] + Radii[Axis] >= This^.Split.Vector[Axis]) then
        Query(This^.R, Depth + 1);
  end;

begin
  SetLength(sqRadii, Self.Dimensions);
  for i:=0 to High(radii) do
    sqRadii[i] := Sqr(Radii[i]);

  sqrProduct := 1.0;
  for i:=0 to High(radii) do
    sqrProduct *= sqradii[i];

  SetLength(Result, 1024);
  ResultSize := 0;

  Query(0, 0);

  SetLength(Result, ResultSize);
end;


(*
  Finds the most frequent Ref (classification label) among the K-nearest neighbors of a given vector.

  This function uses a KD-Tree to efficiently find the K-nearest neighbors and then determines the
  most common Ref value among those neighbors. It assumes that Ref values are integers within a
  contiguous range [0..x]. This allows for efficient counting of Ref occurrences using a dynamic array.

  Parameters:
    Vector: The point (TSingleArray) for which to find the K-nearest neighbors and classify.
    K: The number of nearest neighbors to consider.

  Returns:
    The most frequent Ref value (an integer) among the K-nearest neighbors. Returns -1 if no
    neighbors are found (e.g., an empty tree or K=0).

  Average Time Complexity:    O(log n * log k)
  Worst Case Time Complexity: O(n)

  Where:
    n is the number of nodes in the KD-Tree.
    k is the number of nearest neighbors to consider (K).

  Note:
    The time complexity is typically closer to O(log n) when K is significantly smaller than n.
*)
function TKDTree.KNearestClassify(Vector: TSingleArray; K: Integer): Integer;
var
  NearestNeighbors: TKDItems;
  CategoryCounts: TIntegerArray;
  i: Integer;
  MostCommonCategory: Integer;
  MaxCount: Integer;
  Category: Integer;
  LowCategory: Integer;
begin
  NearestNeighbors := Self.KNearest(Vector, K);

  SetLength(CategoryCounts, NearestNeighbors[0].Ref + 1);
  LowCategory := High(Integer);

  for i := 0 to High(NearestNeighbors) do
  begin
    Category := NearestNeighbors[i].Ref;

    if Category < LowCategory then
      LowCategory := Category;

    if Category >= Length(CategoryCounts) then
      SetLength(CategoryCounts, Category + 1);

    Inc(CategoryCounts[Category]);
  end;

  MostCommonCategory := -1;
  MaxCount := 0;
  for i := LowCategory to High(CategoryCounts) do
  begin
    if CategoryCounts[i] > MaxCount then
    begin
      MaxCount := CategoryCounts[i];
      MostCommonCategory := i;
    end;
  end;

  Result := MostCommonCategory;
end;

(*
  Same as KNearestClassify except we weight the output towards our Vector so
  that closer vectors in the tree is slightly higher valued.
*)
function TKDTree.WeightedKNearestClassify(Vector: TSingleArray; K: Integer): Integer;
var
  NearestNeighbors: TKDItems;
  CategoryCounts: TSingleArray; // Dynamic array for weighted counts
  i: Integer;
  MostCommonCategory: Integer;
  MaxWeight: Single;
  Weight: Single;
  Dist: Single;
  LowCategory: Integer;
begin
  NearestNeighbors := Self.KNearest(Vector, K);

  SetLength(CategoryCounts, 0);
  LowCategory := High(Integer);

  for i := 0 to High(NearestNeighbors) do
  begin
    if NearestNeighbors[i].Ref >= Length(CategoryCounts) then
      SetLength(CategoryCounts, NearestNeighbors[i].Ref + 1);

    if NearestNeighbors[i].Ref < LowCategory then
      LowCategory := NearestNeighbors[i].Ref;

    Dist := Sqrt(Self.SqDistance(Vector, NearestNeighbors[i].Vector));
    Weight := 1.0 / Dist;

    CategoryCounts[NearestNeighbors[i].Ref] := CategoryCounts[NearestNeighbors[i].Ref] + Weight;
  end;

  MostCommonCategory := -1;
  MaxWeight := -1;

  for i := LowCategory to High(CategoryCounts) do
  begin
    if CategoryCounts[i] > MaxWeight then
    begin
      MaxWeight := CategoryCounts[i];
      MostCommonCategory := i;
    end;
  end;

  Result := MostCommonCategory;
end;

(*
  Implements n-dimensional spatial clustering algorithm using a KD-Tree.
  It supports label-based filtering to cluster points within specific categories.

  Best case:                  O(n)
  Average Time Complexity:    O(n log n)
  Worst Case Time Complexity: O(n^2) in theory .. Maybe still just O(n log n) or thereabout.

  Best case of O(n) can happen when we prune all branches in the first pass,
  this requires a radii that covers all points from anywhere or luck point selection.

  TODO: Add a version that returns T2DIntegerArray where each version represents
        an index in KDTree.Data.

  TODO2: Use a visited set to not mutate the tree's hidden states.
*)
function TKDTree.Clusters(Radii: TSingleArray): T2DKDItems;
var
  resCount, qCount: Integer;
  sqRadii: TSingleArray;
  sqrProduct: Double;
  queue: array of PKDNode;

  function Fits(const p,c: TKDItem): Boolean; {inline; produces worse machinecode}
  var
    dsqr: Single;
    i: Integer;
  begin
    if p.ref <> c.ref then Exit(False); //mismatch category
    dsqr := 0;
    for i := 0 to High(p.vector) do
    begin
      dsqr += (Sqr(p.vector[i] - c.vector[i]) * sqRadii[i]);
      if dsqr > sqrProduct then Exit(False);
    end;

    Result := dsqr <= sqrProduct;
  end;

  procedure Cluster(const test: TKDItem; var result: TKDItems; this: PKDNode; depth:Integer=0);
  var
    goright:Boolean = False;
    goleft: Boolean = False;
    splitDim: Integer;
  begin
    // Early exit if this node and its subtree are fully explored
    if Byte(this^.hidden) = 2 then Exit;

    splitDim := depth mod Self.Dimensions;

    goleft  := test.Vector[splitDim] - radii[splitDim] <= this^.Split.Vector[splitDim];
    goright := test.Vector[splitDim] + radii[splitDim] >= this^.Split.Vector[splitDim];

    if (not this^.hidden) and ((goleft=goright)=True) and Fits(test, this^.Split) then
    begin
      if rescount = Length(result) then Setlength(result, rescount*2);
      result[rescount] := this^.split;
      Inc(rescount);

      Byte(this^.Hidden) := 1;

      Inc(qcount);
      queue[qcount] := this;
    end;

    if goleft  and (this^.l <> -1) then Cluster(test, result, @self.data[this^.l], depth+1);
    if goright and (this^.r <> -1) then Cluster(test, result, @self.data[this^.r], depth+1);

    // Chop off branches completely visted! Avoids O(n^2) worst case
    if (this^.Hidden) and
       (((this^.l = -1) and (this^.r = -1)) or
        ((this^.l = -1) and (Byte(self.data[this^.r].hidden) = 2)) or
        ((this^.r = -1) and (Byte(self.data[this^.l].hidden) = 2)) or
        ((Byte(self.data[this^.l].hidden) = 2) and (Byte(self.data[this^.r].hidden) = 2)))
    then
      Byte(this^.Hidden) := 2;
  end;

var
  i,j:Integer;
begin
  if Length(Radii) <> self.Dimensions then
    raise Exception.Create('TKDTree.Clusters: Input dimensions does not match the tree');

  SetLength(sqRadii, Self.Dimensions);
  for i:=0 to High(radii) do
    sqRadii[i] := Sqr(Radii[i]);

  sqrProduct := 1.0;
  for i:=0 to High(radii) do
    sqrProduct *= sqradii[i];

  SetLength(queue, Length(self.Data));

  j := 0;
  for i:=0 to High(self.Data) do
  begin
    if self.Data[i].Hidden then
      continue;

    SetLength(Result, j+1);
    rescount := 0;
    SetLength(Result[j], 8);

    qcount := 0;
    queue[0] := @Self.Data[i];
    while qcount >= 0 do
    begin
      Dec(qcount);
      Cluster(queue[qcount+1]^.Split, Result[j], @Self.Data[0]);
    end;
    SetLength(Result[j], resCount);
    Inc(j);
  end;

  for i:=0 to High(Self.Data) do
    self.data[i].Hidden := False;
end;

end.

