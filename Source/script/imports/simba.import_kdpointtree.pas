unit simba.import_kdpointtree;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script;

procedure ImportKDPointTree(Script: TSimbaScript);

implementation

uses
  lptypes,
  simba.container_kdpointtree;

type
  PKDPointTree = ^TKDPointTree;

(*
KDPointTree
===========
A 2D version of a KDTree for TPoint datatype

Note:
  For more dimensions, and/or floating point values, along with category or
  index reference see TKDTree.
*)

(*
TKDPointTree.Create
-------------------
```
function TKDPointTree.Create(TPA: TPointArray): TKDPointTree; static;
```
Builds the KDTree.
Time complexity average is O(n log n)
*)
procedure _LapeKDPointTreeCreate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PKDPointTree(Result)^ := TKDPointTree.Create(PPointArray(Params^[0])^);
end;

(*
TKDPointTree.IndexOf
--------------------
```
function TKDPointTree.IndexOf(P: TPoint): Integer;
```

Search and find the index for use in TKDPointTree.Data for a given point `p`

Time complexity average is O(log n)
*)
procedure _LapeKDPointTreeIndexOf(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PKDPointTree(Params^[0])^.IndexOf(PPoint(Params^[1])^);
end;

(*
TKDPointTree.Find
-----------------
```
function TKDPointTree.Find(P: TPoint): PSlackNode;
```

Search and find the given point `p`, returns a node, a pointer to the tree node.

Time complexity average is O(log n)
*)
procedure _LapeKDPointTreeFind(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointer(Result)^ := PKDPointTree(Params^[0])^.Find(PPoint(Params^[1])^);
end;

(*
TKDPointTree.Hide
-----------------
```
procedure TKDPointTree.Hide(idx:Integer);
```

Hide the node (by index) so that queries will not return it.

Time complexity average is O(1)
*)
procedure _LapeKDPointTreeHideNode(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PKDPointTree(Params^[0])^.HideNode(PInteger(Params^[1])^);
end;

(*
TKDPointTree.Hide
-----------------
```
function TKDPointTree.Hide(P: TPoint): Boolean; overload;
```

Hide the node (by TPoint) so that queries will not return it.

Time complexity average is O(log n)
*)
procedure _LapeKDPointTreeHideNode2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PKDPointTree(Params^[0])^.HideNode(PPoint(Params^[1])^);
end;


(*
TKDPointTree.RawNearest
-----------------------
```
function TKDPointTree.RawNearest(P: TPoint; NotEqual: Boolean = False): PSlackNode;
```

Returns the closest node to the point given.

Time complexity average is O(log n)
*)
procedure _LapeKDPointTreeRawNearest(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointer(Result)^ := PKDPointTree(Params^[0])^.RawNearest(PPoint(Params^[1])^, PBoolean(Params^[2])^);
end;


(*
TKDPointTree.Nearest
--------------------
```
function TKDPointTree.Nearest(P: TPoint; NotEqual :Boolean = False): TPoint;
```

Returns the closest point to the point given.

Time complexity average is O(log n)
*)
procedure _LapeKDPointTreeNearest(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PKDPointTree(Params^[0])^.Nearest(PPoint(Params^[1])^, PBoolean(Params^[2])^);
end;


(*
TKDPointTree.RawKNearest
------------------------
```
function TKDPointTree.RawKNearest(P: TPoint; k:Integer; NotEqual: Boolean = False): TSlackRefArray;
```

Returns the **k** closest node to the point given.

Time complexity average is O(k * log n)
*)
procedure _LapeKDPointTreeRawKNearest(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TNodeRefArray(Result^) := PKDPointTree(Params^[0])^.RawKNearest(PPoint(Params^[1])^, PInteger(Params^[2])^, PBoolean(Params^[3])^);
end;


(*
TKDPointTree.KNearest
---------------------
```
function TKDPointTree.KNearest(P: TPoint; k:Integer; NotEqual: Boolean = False): TPointArray;
```

Returns the **k** closest points to the point given.

Time complexity average is O(k * log n)
*)
procedure _LapeKDPointTreeKNearest(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PKDPointTree(Params^[0])^.KNearest(PPoint(Params^[1])^, PInteger(Params^[2])^, PBoolean(Params^[3])^);
end;

(*
TKDPointTree.RangeQuery
-----------------------
```
function TKDPointTree.RangeQuery(B:TBox; hide:Boolean = False): TPointArray;
```

Returns all the points that are within the given box.

Time complexity average is O(k * log n)
where **k** is the number of points returned
*)
procedure _LapeKDPointTreeRangeQuery(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PKDPointTree(Params^[0])^.RangeQuery(PBox(Params^[1])^, PBoolean(Params^[2])^);
end;


(*
TKDPointTree.RangeQueryEx
-------------------------
```
function TKDPointTree.RangeQueryEx(query:TPoint; xRad,yRad:Double; hide: Boolean = False): TPointArray;
```

Returns all the points that are within the given range **xRad** and **yRad**.

Time complexity average is O(k * log n)
where **k** is the number of points returned
*)
procedure _LapeKDPointTreeRangeQueryEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PKDPointTree(Params^[0])^.RangeQueryEx(PPoint(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^, PBoolean(Params^[4])^);
end;


(*
TKDPointTree.RangeQueryEx
-------------------------
```
function TKDPointTree.RangeQueryEx(query:TPoint; xmin,ymin,xmax,ymax: Double; hide: Boolean = False): TPointArray;
```

Returns all the points that are further away than xmin, and ymin, but closer than xmax, and ymax from the query.

Time complexity average is O(k * log n)
where **k** is the number of points returned
*)
procedure _LapeKDPointTreeRangeQueryEx2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PKDPointTree(Params^[0])^.RangeQueryEx(PPoint(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^, PDouble(Params^[4])^, PDouble(Params^[5])^, PBoolean(Params^[6])^);
end;

(*
TKDPointTree.Clusters
---------------------
```
function TKDPointTree.Clusters(xRad,yRad: Single): T2DPointArray;
```

Like TPA.Cluster, but acts on the points in the tree, and allows you to use floats for xRad, and yRad.
This methods also exists in TKDTree and works in n dimensions.

Time complexity average is between O(n) and O(n log n)

Speedwise, once the tree is built this method is on pair with ClusterTPA.
*)
procedure _LapeKDPointTreeClusters(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  T2DPointArray(Result^) := PKDPointTree(Params^[0])^.Clusters(Single(Params^[1]^), Single(Params^[2]^));
end;

procedure ImportKDPointTree(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    //DumpSection := 'KDPointTree';

    addGlobalType('record Split: TPoint; L, R: Integer; idden: Boolean; end;', 'TKDPointNode');
    addGlobalType('record Data: array of TKDPointNode; Size: Integer; end;', 'TKDPointTree');

    addGlobalFunc('function TKDPointTree.Create(TPA: TPointArray): TKDPointTree; static;', @_LapeKDPointTreeCreate);
    addGlobalFunc('function TKDPointTree.IndexOf(P: TPoint): Integer;', @_LapeKDPointTreeIndexOf);
    addGlobalFunc('function TKDPointTree.Find(P: TPoint): ^TKDPointNode;', @_LapeKDPointTreeFind);
    addGlobalFunc('procedure TKDPointTree.Hide(idx:Integer); overload;', @_LapeKDPointTreeHideNode);
    addGlobalFunc('function TKDPointTree.Hide(P: TPoint): Boolean; overload;', @_LapeKDPointTreeHideNode2);
    addGlobalFunc('function TKDPointTree.Nearest(P: TPoint; NotEqual:Boolean = False): TPoint;', @_LapeKDPointTreeNearest);
    addGlobalFunc('function TKDPointTree.KNearest(P: TPoint; k:Integer; NotEqual: Boolean = False): TPointArray;', @_LapeKDPointTreeKNearest);
    addGlobalFunc('function TKDPointTree.RangeQuery(B:TBox; hide:Boolean = False): TPointArray;', @_LapeKDPointTreeRangeQuery);
    addGlobalFunc('function TKDPointTree.RangeQueryEx(query:TPoint; xRad,yRad:Double; hide: Boolean = False): TPointArray; overload;', @_LapeKDPointTreeRangeQueryEx);
    addGlobalFunc('function TKDPointTree.RangeQueryEx(query:TPoint; xmin,ymin,xmax,ymax: Double; hide: Boolean = False): TPointArray; overload;', @_LapeKDPointTreeRangeQueryEx2);
    addGlobalFunc('function TKDPointTree.Clusters(xRad,yRad: Single): T2DPointArray;', @_LapeKDPointTreeClusters);
  end;
end;

end.

