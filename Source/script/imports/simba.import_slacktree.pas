unit simba.import_slacktree;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script;

procedure ImportSlackTree(Script: TSimbaScript);

implementation

uses
  lptypes,
  simba.container_slacktree;


(*
SlackTree
=========
A 2D version of a KDTree for TPoint datatype

Note:
  For more dimensions, and/or floating point values, along with category or
  index reference see TKDTree.
*)

(*
TSlackTree.Init
---------------
```
procedure TSlackTree.Init(TPA: TPointArray);
```

Builds the KDTree.

Time complexity average is O(n log n)
*)
procedure _LapeSlackTreeInit(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSlackTree(Params^[0])^.Init(PPointArray(Params^[1])^);
end;

(*
TSlackTree.Create
-----------------
```
function TSlackTree.Create(TPA: TPointArray): TSlackTree; static;
```

Same as Init, just as a constructor to allow simplified usage as seen in the example:

**Example:**
```
  Groups := TSlackTree.Create(TPA).Clusters(5,5);
```
*)
procedure _LapeSlackTreeCreate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSlackTree(Result^) := TSlackTree.Create(TPointArray(Params^[0]^));
end;


(*
TSlackTree.IndexOf
------------------
```
function TSlackTree.IndexOf(P: TPoint): Integer;
```

Search and find the index for use in TSlackTree.Data for a given point `p`

Time complexity average is O(log n)
*)
procedure _LapeSlackTreeIndexOf(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSlackTree(Params^[0])^.IndexOf(PPoint(Params^[1])^);
end;

(*
TSlackTree.Find
---------------
```
function TSlackTree.Find(P: TPoint): PSlackNode;
```

Search and find the given point `p`, returns a node, a pointer to the tree node.

Time complexity average is O(log n)
*)
procedure _LapeSlackTreeFind(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointer(Result)^ := PSlackTree(Params^[0])^.Find(PPoint(Params^[1])^);
end;

(*
TSlackTree.Hide
---------------
```
procedure TSlackTree.Hide(idx:Integer);
```

Hide the node (by index) so that queries will not return it.

Time complexity average is O(1)
*)
procedure _LapeSlackTreeHideNode(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSlackTree(Params^[0])^.HideNode(PInteger(Params^[1])^);
end;

(*
TSlackTree.Hide
---------------
```
function TSlackTree.Hide(P: TPoint): Boolean; overload;
```

Hide the node (by TPoint) so that queries will not return it.

Time complexity average is O(log n)
*)
procedure _LapeSlackTreeHideNode2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSlackTree(Params^[0])^.HideNode(PPoint(Params^[1])^);
end;


(*
TSlackTree.RawNearest
---------------------
```
function TSlackTree.RawNearest(P: TPoint; NotEqual: Boolean = False): PSlackNode;
```

Returns the closest node to the point given.

Time complexity average is O(log n)
*)
procedure _LapeSlackTreeRawNearest(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointer(Result)^ := PSlackTree(Params^[0])^.RawNearest(PPoint(Params^[1])^, PBoolean(Params^[2])^);
end;


(*
TSlackTree.Nearest
------------------
```
function TSlackTree.Nearest(P: TPoint; NotEqual :Boolean = False): TPoint;
```

Returns the closest point to the point given.

Time complexity average is O(log n)
*)
procedure _LapeSlackTreeNearest(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSlackTree(Params^[0])^.Nearest(PPoint(Params^[1])^, PBoolean(Params^[2])^);
end;


(*
TSlackTree.RawKNearest
----------------------
```
function TSlackTree.RawKNearest(P: TPoint; k:Integer; NotEqual: Boolean = False): TSlackRefArray;
```

Returns the **k** closest node to the point given.

Time complexity average is O(k * log n)
*)
procedure _LapeSlackTreeRawKNearest(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TNodeRefArray(Result^) := PSlackTree(Params^[0])^.RawKNearest(PPoint(Params^[1])^, PInteger(Params^[2])^, PBoolean(Params^[3])^);
end;


(*
TSlackTree.KNearest
-------------------
```
function TSlackTree.KNearest(P: TPoint; k:Integer; NotEqual: Boolean = False): TPointArray;
```

Returns the **k** closest points to the point given.

Time complexity average is O(k * log n)
*)
procedure _LapeSlackTreeKNearest(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSlackTree(Params^[0])^.KNearest(PPoint(Params^[1])^, PInteger(Params^[2])^, PBoolean(Params^[3])^);
end;


(*
TSlackTree.RawRangeQuery
------------------------
```
function TSlackTree.RawRangeQuery(B:TBox): TSlackRefArray;
```
*)
procedure _LapeSlackTreeRawRangeQuery(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TNodeRefArray(Result^) := PSlackTree(Params^[0])^.RawRangeQuery(PBox(Params^[1])^);
end;


(*
TSlackTree.RangeQuery
---------------------
```
function TSlackTree.RangeQuery(B:TBox; hide:Boolean = False): TPointArray;
```

Returns all the points that are within the given box.

Time complexity average is O(k * log n)
where **k** is the number of points returned
*)
procedure _LapeSlackTreeRangeQuery(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSlackTree(Params^[0])^.RangeQuery(PBox(Params^[1])^, PBoolean(Params^[2])^);
end;


(*
TSlackTree.RangeQueryEx
-----------------------
```
function TSlackTree.RangeQueryEx(query:TPoint; xRad,yRad:Double; hide: Boolean = False): TPointArray;
```

Returns all the points that are within the given range **xRad** and **yRad**.

Time complexity average is O(k * log n)
where **k** is the number of points returned
*)
procedure _LapeSlackTreeRangeQueryEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSlackTree(Params^[0])^.RangeQueryEx(PPoint(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^, PBoolean(Params^[4])^);
end;


(*
TSlackTree.RangeQueryEx
-----------------------
```
function TSlackTree.RangeQueryEx(query:TPoint; xmin,ymin,xmax,ymax: Double; hide: Boolean = False): TPointArray;
```

Returns all the points that are further away than xmin, and ymin, but closer than xmax, and ymax from the query.

Time complexity average is O(k * log n)
where **k** is the number of points returned
*)
procedure _LapeSlackTreeRangeQueryEx2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSlackTree(Params^[0])^.RangeQueryEx(PPoint(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^, PDouble(Params^[4])^, PDouble(Params^[5])^, PBoolean(Params^[6])^);
end;

(*
TSlackTree.Clusters
-------------------
```
function TSlackTree.Clusters(xRad,yRad: Single): T2DPointArray;
```

Like TPA.Cluster, but acts on the points in the tree, and allows you to use floats for xRad, and yRad.
This methods also exists in TKDTree and works in n dimensions.

Time complexity average is between O(n) and O(n log n)

Speedwise, once the tree is built this method is on pair with ClusterTPA.
*)
procedure _LapeSlackTreeClusters(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  T2DPointArray(Result^) := PSlackTree(Params^[0])^.Clusters(Single(Params^[1]^), Single(Params^[2]^));
end;


(*
TSlackTree.RefArray
-------------------
```
function TSlackTree.RefArray: TSlackRefArray;
```
*)
procedure _LapeSlackTreeRefArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TNodeRefArray(Result^) := PSlackTree(Params^[0])^.RefArray;
end;



procedure ImportSlackTree(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    addGlobalType('record Split: TPoint; L, R: Integer; hidden: Boolean; end;', 'TSlackNode');
    addGlobalType('^TSlackNode', 'PSlackNode');
    addGlobalType('array of TSlackNode;', 'TSlackArray');
    addGlobalType('array of PSlackNode;', 'TSlackRefArray');
    addGlobalType('record Data: TSlackArray; Size: Integer; end;', 'TSlackTree');

    addGlobalFunc('procedure TSlackTree.Init(TPA: TPointArray);', @_LapeSlackTreeInit);
    addGlobalFunc('function TSlackTree.Create(TPA: TPointArray): TSlackTree; static;', @_LapeSlackTreeCreate); 
    
    addGlobalFunc('function TSlackTree.IndexOf(P: TPoint): Integer;', @_LapeSlackTreeIndexOf);
    addGlobalFunc('function TSlackTree.Find(P: TPoint): PSlackNode;', @_LapeSlackTreeFind);
    addGlobalFunc('procedure TSlackTree.Hide(idx:Integer);', @_LapeSlackTreeHideNode);
    addGlobalFunc('function TSlackTree.Hide(P: TPoint): Boolean; overload;', @_LapeSlackTreeHideNode2);
    addGlobalFunc('function TSlackTree.RawNearest(P: TPoint; NotEqual: Boolean = False): PSlackNode;', @_LapeSlackTreeRawNearest);
    addGlobalFunc('function TSlackTree.Nearest(P: TPoint; NotEqual :Boolean = False): TPoint;', @_LapeSlackTreeNearest);
    addGlobalFunc('function TSlackTree.RawKNearest(P: TPoint; k:Integer; NotEqual: Boolean = False): TSlackRefArray;', @_LapeSlackTreeRawKNearest);
    addGlobalFunc('function TSlackTree.KNearest(P: TPoint; k:Integer; NotEqual: Boolean = False): TPointArray;', @_LapeSlackTreeKNearest);
    addGlobalFunc('function TSlackTree.RawRangeQuery(B:TBox): TSlackRefArray;', @_LapeSlackTreeRawRangeQuery);
    addGlobalFunc('function TSlackTree.RangeQuery(B:TBox; hide:Boolean = False): TPointArray;', @_LapeSlackTreeRangeQuery);
    addGlobalFunc('function TSlackTree.RangeQueryEx(query:TPoint; xRad,yRad:Double; hide: Boolean = False): TPointArray; overload;', @_LapeSlackTreeRangeQueryEx);
    addGlobalFunc('function TSlackTree.RangeQueryEx(query:TPoint; xmin,ymin,xmax,ymax: Double; hide: Boolean = False): TPointArray; overload;', @_LapeSlackTreeRangeQueryEx2);
    addGlobalFunc('function TSlackTree.Clusters(xRad,yRad: Single): T2DPointArray;', @_LapeSlackTreeClusters);
    addGlobalFunc('function TSlackTree.RefArray: TSlackRefArray;', @_LapeSlackTreeRefArray);
  end;
end;

end.

