pushSection('SlackTree');

addGlobalType('record Split: TPoint; L, R: Integer; hidden: Boolean; end;', 'TSlackNode');
addGlobalType('^TSlackNode', 'PSlackNode');
addGlobalType('array of TSlackNode;', 'TSlackArray');
addGlobalType('array of PSlackNode;', 'TSlackRefArray');
addGlobalType('record Data: TSlackArray; Size: Integer; end;', 'TSlackTree');

addGlobalFunc('procedure TSlackTree.Init(TPA: TPointArray);', @_LapeSlackTreeInit);
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
addGlobalFunc('function TSlackTree.RangeQueryEx(query:TPoint; xmin,ymin,xmax,ymax:double; hide: Boolean = False): TPointArray; overload;', @_LapeSlackTreeRangeQueryEx2);
addGlobalFunc('function TSlackTree.RefArray: TSlackRefArray;', @_LapeSlackTreeRefArray);

popSection();


