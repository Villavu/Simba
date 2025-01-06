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

type
  (*
    KDTree is a static tree for quick "spatial lookups" that also contains `ref` per item added 
    which means it can be used for simple classifcation for example.

    ToDo: Load data from CSV for storing of training data to avoid rebuilding? - building can be slow
  *)
  
  TKDItem = record
    Ref: Int32;
    Vector: TSingleArray;
  end;

  TKDItems = array of TKDItem;
  T2DKDItems = array of TKDItems;

  PKDNode = ^TKDNode;
  TKDNode = record
    Split: TKDItem;
    L, R: Int32;
    Hidden: Boolean;
  end;

  TKDNodeArray = array of TKDNode;
  TKDNodeRefArray = array of PKDNode;

  TKDTree = record
    Dimensions: Int32;
    Data: TKDNodeArray;
    Size: Int32;
    
    function RefArray(): TKDNodeRefArray;
    function GetItem(i:Int32): PKDNode;
    function InitBranch(): Int32; inline;
    function Copy(): TKDTree;
    function SqDistance(A, B: TSingleArray; Limit: Single = High(UInt32)): Single; inline;
    procedure Init(const AData: TKDItems);
    function IndexOf(const Value: TSingleArray): Int32;
    function KNearest(Vector: TSingleArray; K: Int32; NotEqual: Boolean = False): TKDItems;
    function RangeQuery(Low, High: TSingleArray): TKDItems;
    function RangeQueryEx(Center: TSingleArray; Radii: TSingleArray; Hide: Boolean): TKDItems;
    function KNearestClassify(Vector: TSingleArray; K: Int32): Int32;
    function WeightedKNearestClassify(Vector: TSingleArray; K: Int32): Int32;
    function Clusters(Radii: TSingleArray): T2DKDItems;
  end;

  PKDTree = ^TKDTree; 
  

implementation

const
  NONE = -1;


(*
  Quick select for the KDTree build process
*)
function QuickSelectOnAxis(var Arr:TKDItems; k, Start, Stop: Int32; Axis: Int32): TKDItem;
var
  l, r: Int32;
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
  i:Int32;
begin
  SetLength(Result, Length(self.data));
  for i:=0 to High(self.data) do Result[i] := @self.data[i];
end;


function TKDTree.GetItem(i:Int32): PKDNode;
begin
  Result := @self.data[i];
end;


function TKDTree.InitBranch: Int32;
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
var i: Int32;
begin
  Result := 0;
  for i:=0 to High(a) do
  begin
    Result += Sqr(a[i]-b[i]);
    if Result > Limit then Exit;
  end;
end;


procedure TKDTree.Init(const AData: TKDItems);
var
  Sortable: TKDItems;

  procedure BuildTree(var Node: TKDNode; Left, Right:Int32; Depth:Int32=0);
  var
    Mid: Int32;
  begin
    if (Right-Left < 0) then Exit();     // just nil back up..

    Mid := (Right + left) shr 1;
    Node.Split := QuickSelectOnAxis(Sortable, Mid, Left, Right, depth mod Self.Dimensions);

    if Mid-left > 0 then begin           //lower half
      Node.L := self.InitBranch();
      BuildTree(self.Data[Node.L], left,mid-1, depth+1);
    end;

    if Right-Mid > 0 then begin          //upper half
      Node.R := self.InitBranch();
      BuildTree(self.Data[Node.R], mid+1,right, depth+1);
    end;
  end;
begin
  if Length(AData) = 0 then Exit;

  Sortable := System.Copy(AData);

  Self.Dimensions := Length(AData[0].Vector);
  Self.Size := 0;

  SetLength(self.Data, Length(Sortable));
  BuildTree(self.Data[self.InitBranch()], 0, High(Sortable));
end;


(*
  Returns an Index that can be used to access TKDTree.Data directly.

  Average Time Complexity: O(log n)

  Note:
    The time complexity is typically closer to O(log n) when K is significantly smaller than n.
*)
function TKDTree.IndexOf(const Value: TSingleArray): Int32;
var
  Node: Int32;
  Depth: UInt8;
  Axis: Integer;
  i: Integer;
  Found: Boolean;
begin
  Node := 0;
  Depth := 0;
  Result := NONE;

  while Node <> NONE do
  begin
    Found := True;
    for i := 0 to Self.Dimensions - 1 do
    begin
      if Self.Data[Node].Split.Vector[i] <> Value[i] then
      begin
        Found := False;
        Break;
      end;
    end;

    if Found then
      Exit(Node);

    Axis := Depth mod Self.Dimensions;
    if Value[Axis] < Self.Data[Node].Split.Vector[Axis] then
      Node := Self.Data[Node].L
    else
      Node := Self.Data[Node].R;

    Inc(Depth);
  end;
end;


(*
  Returns the K nearest vectors to the input vector

  Average Time Complexity:    O(log n * log k)
  Worst Case Time Complexity: O(n)

  Note:
    The time complexity is typically closer to O(log n) when K is significantly smaller than n.
    
  XXX:
    Can maybe use simba's heap strucutre, I elected not to as this was designed within Simba/Lape.
*)
function TKDTree.KNearest(Vector: TSingleArray; K: Int32; NotEqual: Boolean = False): TKDItems;
type
  TNearestItem = record
    Node: PKDNode;
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

  procedure FindKNearest(Node: Int32; Depth: UInt8);
  var
    Delta, DistSq: Single;
    Test: Int32;
    This: PKDNode;
    Axis: Integer;
    Temp: TNearestItem;
    I: Int32;
  begin
    if Node = NONE then Exit;

    This := @Self.Data[Node];
    Axis := Depth mod Self.Dimensions;

    Delta := This^.Split.Vector[Axis] - Vector[Axis];

    if Length(Heap) < K then
      DistSq := Self.SqDistance(This^.Split.Vector, Vector, High(UInt32)) // No limit if heap is not full
    else
      DistSq := Self.SqDistance(This^.Split.Vector, Vector, Heap[0].DistSq); // Limit is the furthest distance in the heap

    if not((DistSq = 0) and NotEqual) then
    begin
      if Length(Heap) < K then
      begin
        // heap not full, add current node
        SetLength(Heap, Length(Heap) + 1);
        Heap[High(Heap)].Node := This;
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
          Heap[0].Node := This;
          Heap[0].DistSq := DistSq;
          // heapify downwards
          Heapify(0);
        end;
      end;
    end;

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
    Result[j] := Heap[i].Node^.Split;
    Inc(j);
  end;
end;


(*
  Returns the vectors that within the hyperrectangle defined by low and high vectors.

  Average Time Complexity:    O(log n + k)
  Worst Case Time Complexity: O(n^((d-1)/d) + k)

  Where k is the number of output points (not known ahead of time)
*)
function TKDTree.RangeQuery(Low, High: TSingleArray): TKDItems;
var
  ResultSize: Integer;

  procedure Query(Node: Int32; Depth: UInt8);
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
        if ResultSize = Length(Result) then
          SetLength(Result, Length(Result) * 2);

        Result[ResultSize] := This^.Split;
        Inc(ResultSize);
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
  Returns the vectors that within the query center and radii bounds defining the hypersphere.

  Average Time Complexity:    O(log n + k)
  Worst Case Time Complexity: O(n^((d-1)/d) + k)

  Where k is the number of output points (not known ahead of time)
*)
function TKDTree.RangeQueryEx(Center: TSingleArray; Radii: TSingleArray; Hide: Boolean): TKDItems;
var
  i, ResultSize: Int32;
  SumSqRadii: Single;

  procedure Query(Node: Int32; Depth: UInt8);
  var
    This: PKDNode;
    Axis: Integer;
    i: Integer;
    DistSq: Single;
    WithinRange: Boolean;
  begin
    if Node = NONE then Exit;

    This := @Self.Data[Node];
    Axis := Depth mod Self.Dimensions;

    DistSq := 0;
    for i := 0 to Self.Dimensions - 1 do
    begin
      DistSq += Sqr(This^.Split.Vector[i] - Center[i]);
      if DistSq > SumSqRadii then
        break;
    end;

    WithinRange := DistSq <= SumSqRadii;

    if WithinRange then
    begin
      if ResultSize = Length(Result) then
        SetLength(Result, Length(Result) * 2);

      Result[ResultSize] := This^.Split;
      Inc(ResultSize);

      if Hide then
        This^.Hidden := True;
    end;

    if (Center[Axis] - Radii[Axis] <= This^.Split.Vector[Axis]) then
        Query(This^.L, Depth + 1);

    if (Center[Axis] + Radii[Axis] >= This^.Split.Vector[Axis]) then
        Query(This^.R, Depth + 1);
  end;

begin
  SumSqRadii := 0;
  for i := 0 to Self.Dimensions - 1 do
    SumSqRadii := SumSqRadii + Sqr(Radii[i]);

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
function TKDTree.KNearestClassify(Vector: TSingleArray; K: Int32): Int32;
var
  NearestNeighbors: TKDItems;
  CategoryCounts: TIntegerArray;
  i: Integer;
  MostCommonCategory: Int32;
  MaxCount: Integer;
  Category: Int32;
  LowCategory: Integer;
begin
  NearestNeighbors := Self.KNearest(Vector, K, False);

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
function TKDTree.WeightedKNearestClassify(Vector: TSingleArray; K: Int32): Int32;
var
  NearestNeighbors: TKDItems;
  CategoryCounts: TSingleArray; // Dynamic array for weighted counts
  i: Integer;
  MostCommonCategory: Int32;
  MaxWeight: Single;
  Weight: Single;
  Dist: Single;
  LowCategory: Int32;
begin
  NearestNeighbors := Self.KNearest(Vector, K, False);

  SetLength(CategoryCounts, 0);
  LowCategory := High(Int32);

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
  Implements an efficient n-dimensional spatial clustering algorithm using a KD-Tree.
  It supports label-based filtering to cluster points within specific categories.

  Best case:                  O(n)
  Average Time Complexity:    O(n log n)
  Worst Case Time Complexity: O(n^2) in theory .. Maybe still just O(n log n) or thereabout.

  Best case of O(n) can happen when we prune all branches in the first pass,
  this requires a radii that covers all points from anywhere or luck point selection.

  TODO: Add a version that returns T2DIntegerArray where each version represents
        an index in KDTree.Data.
*)
function TKDTree.Clusters(Radii: TSingleArray): T2DKDItems;
var
  resCount, qCount: Int32;
  sqRadii: TSingleArray;
  sqrProduct: Double;
  queue: array of PKDNode;

  function Fits(const p,c: TKDItem): Boolean; {inline; produces worse machinecode}
  var
    dsqr: Single;
    i: Int32;
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

  procedure Cluster(const test: TKDItem; var result: TKDItems; this: PKDNode; depth:Int32=0);
  var
    goright:Boolean = False;
    goleft: Boolean = False;
    splitDim: Int32;
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
  i,j:Int32;
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

