unit simba.import_kdtree;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script;

procedure ImportKDTree(Script: TSimbaScript);

implementation

uses
  lptypes,
  simba.container_kdtree;

(*
KDTree
======
A version of a KDTree for n dimensional vectors (each a float).

An Item is described as

```pascal
TKDItem = record
  Ref: Integer;
  Vector: TSingleArray;
end;
```

Where you can use Ref for example as a reference to the initial array before the
tree itself was built.

Ref can also be used as a label / category to differentiate between various
types of vectors in the tree, this can be further used alongside the methods:

```pascal
function TKDTree.KNearestClassify(Vector: TSingleArray; K: Integer): Integer;
function TKDTree.WeightedKNearestClassify(Vector: TSingleArray; K: Integer): Integer;
```

Which means you can build a simple kNN system to classify objects.

Note:
  For a simpeler tree structure for 2D points, look into TSlackTree


**Exposed methods so far**
```
function TKDTree.RefArray(): TKDNodeRefArray;
function TKDTree.GetItem(i:Integer): PKDNode;
function TKDTree.InitBranch(): Integer;
function TKDTree.Copy(): TKDTree;
procedure TKDTree.Init(const AData: TKDItems);
function TKDTree.IndexOf(const Value: TSingleArray): Integer;
function TKDTree.KNearest(Vector: TSingleArray; K: Integer; NotEqual: Boolean = False): TKDItems;
function TKDTree.KNearestIndex(Vector: TSingleArray; K: Integer; NotEqual: Boolean = False): TIntegerArray;
function TKDTree.RangeQuery(Low, High: TSingleArray): TKDItems;
function TKDTree.RangeQueryEx(Center: TSingleArray; Radii: TSingleArray; Hide: Boolean): TKDItems;
function TKDTree.KNearestClassify(Vector: TSingleArray; K: Integer): Integer;
function TKDTree.WeightedKNearestClassify(Vector: TSingleArray; K: Integer): Integer;
function TKDTree.Clusters(Radii: TSingleArray): T2DKDItems;
```
*)

(*
TKDTree.Create
--------------
```
function TKDTree.Create(const AData: TKDItems): TKDTree; static;
```

Same as Init, just as a constructor to allow simplified usage as seen in the example:

**Example:**
```
  TKDTree.Create(data).SaveToFile('file.kd');
```
*)
procedure _LapeKDTreeCreate1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TKDTree(Result^) := TKDTree.Create(TKDItems(Params^[0]^));
end;

(*
TKDTree.Create
--------------
```
function TKDTree.Create(const FileName: string): TKDTree; static;
```

Constructs the KDTree from a compatible KDTree file.
*)
procedure _LapeKDTreeCreate2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TKDTree(Result^) := TKDTree.Create(String(Params^[0]^));
end;

(*
TKDTree.SaveToFile
------------------
```
function TKDTree.SaveToFile(const FileName: string): Boolean; static;
```

Writes the KDTree to a binary file, so that you dont have to rebuild it. 
Should return True on success
*)
procedure _LapeKDTreeSaveToFile(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Boolean(Result^) := TKDTree(Params^[0]^).SaveToFile(String(Params^[1]^));
end;

procedure _LapeKDTreeGetItem(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PKDNode(Result^) := TKDTree(Params^[0]^).GetItem(Integer(Params^[1]^));
end;

procedure _LapeKDTreeCopy(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TKDTree(Result^) := TKDTree(Params^[0]^).Copy();
end;

procedure _LapeKDTreeIndexOf(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := TKDTree(Params^[0]^).IndexOf(TSingleArray(Params^[1]^));
end;

procedure _LapeKDTreeKNearest(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TKDItems(Result^) := TKDTree(Params^[0]^).KNearest(TSingleArray(Params^[1]^), Integer(Params^[2]^), Boolean(Params^[3]^));
end;

(*
TKDTree.KNearestIndex
---------------------
```
function TKDTree.KNearestIndex(Vector: TSingleArray; K: Integer; NotEqual: Boolean = False): TIntegerArray;
```

Returns an array that with indices that can be used to access the kdtree data directly if needed. As an alterantive to getting the vector itself.
*)
procedure _LapeKDTreeKNearestIndex(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TIntegerArray(Result^) := TKDTree(Params^[0]^).KNearestIndex(TSingleArray(Params^[1]^), Integer(Params^[2]^), Boolean(Params^[3]^));
end;

procedure _LapeKDTreeRangeQuery(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TKDItems(Result^) := TKDTree(Params^[0]^).RangeQuery(TSingleArray(Params^[1]^), TSingleArray(Params^[2]^));
end;

procedure _LapeKDTreeRangeQueryEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TKDItems(Result^) := TKDTree(Params^[0]^).RangeQueryEx(TSingleArray(Params^[1]^), TSingleArray(Params^[2]^), Boolean(Params^[3]^));
end;

(*
TKDTree.KNearestClassify
------------------------
```
function TKDTree.KNearestClassify(Vector: TSingleArray; K: Integer): Integer;
function TKDTree.WeightedKNearestClassify(Vector: TSingleArray; K: Integer): Integer;
```

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
  * The time complexity is typically closer to O(log n) when K is significantly smaller than n.
  * WeightedKNearestClassify is the same as KNearestClassify except we weight the output towards our Vector so that closer vectors in the tree is slightly higher valued.
*)
procedure _LapeKDTreeKNearestClassify(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Integer(Result^) := TKDTree(Params^[0]^).KNearestClassify(TSingleArray(Params^[1]^), Integer(Params^[2]^));
end;

procedure _LapeKDTreeWeightedKNearestClassify(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Integer(Result^) := TKDTree(Params^[0]^).WeightedKNearestClassify(TSingleArray(Params^[1]^), Integer(Params^[2]^));
end;

(*
TKDTree.Clusters
----------------
```
function TKDTree.Clusters(Radii: TSingleArray): T2DKDItems;
```

Like TPA.Cluster, a spatial clustering algorithm that clusters vectors based on
proximity to each neigbor, but acts on the vectors in the tree, works in n dimensions.

Note that Radii must match the dimensions of the kd-tree, which was decided by the input.

Time complexity average is between O(n) and O(n log n)
*)
procedure _LapeKDTreeClusters(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  T2DKDItems(Result^) := TKDTree(Params^[0]^).Clusters(TSingleArray(Params^[1]^));
end;

procedure ImportKDTree(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    //DumpSection := 'KDTree';

    addGlobalType('record Ref: Integer; Vector: TSingleArray; end;', 'TKDItem');
    addGlobalType('record Split: TKDItem; L, R: Integer; Hidden: Boolean; end;', 'TKDNode');
    addGlobalType('record Dimensions: Integer; Data: array of TKDNode; Size: Integer; end;', 'TKDTree');
    addGlobalType('array of TKDItem;', 'TKDItems');
    addGlobalType('array of TKDItems;', 'T2DKDItems');

    addGlobalFunc('function TKDTree.Create(const AData: TKDItems): TKDTree; static; overload;', @_LapeKDTreeCreate1);
    addGlobalFunc('function TKDTree.Create(const FileName: string): TKDTree; static; overload;', @_LapeKDTreeCreate2);
    addGlobalFunc('function TKDTree.SaveToFile(const FileName: string): Boolean;', @_LapeKDTreeSaveToFile); 

    addGlobalFunc('function TKDTree.Copy(): TKDTree;', @_LapeKDTreeCopy);
    addGlobalFunc('function TKDTree.IndexOf(const Value: TSingleArray): Integer;', @_LapeKDTreeIndexOf);
    addGlobalFunc('function TKDTree.KNearest(Vector: TSingleArray; K: Integer; NotEqual: Boolean = False): TKDItems;', @_LapeKDTreeKNearest);
    addGlobalFunc('function TKDTree.KNearestIndex(Vector: TSingleArray; K: Integer; NotEqual: Boolean = False): TIntegerArray;', @_LapeKDTreeKNearestIndex);
    addGlobalFunc('function TKDTree.RangeQuery(Low, High: TSingleArray): TKDItems;', @_LapeKDTreeRangeQuery);
    addGlobalFunc('function TKDTree.RangeQueryEx(Center: TSingleArray; Radii: TSingleArray; Hide: Boolean): TKDItems;', @_LapeKDTreeRangeQueryEx);
    addGlobalFunc('function TKDTree.KNearestClassify(Vector: TSingleArray; K: Integer): Integer;', @_LapeKDTreeKNearestClassify);
    addGlobalFunc('function TKDTree.WeightedKNearestClassify(Vector: TSingleArray; K: Integer): Integer;', @_LapeKDTreeWeightedKNearestClassify);
    addGlobalFunc('function TKDTree.Clusters(Radii: TSingleArray): T2DKDItems;', @_LapeKDTreeClusters);
  end;
end;

end.

