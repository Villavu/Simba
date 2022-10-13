unit simba.import_slacktree;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.script_compiler, simba.mufasatypes, simba.slacktree;

procedure _LapeSlackTreeInit(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSlackTree(Params^[0])^.Init(PPointArray(Params^[1])^);
end;

procedure _LapeSlackTreeIndexOf(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PSlackTree(Params^[0])^.IndexOf(PPoint(Params^[1])^);
end;

procedure _LapeSlackTreeFind(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Result)^ := PSlackTree(Params^[0])^.Find(PPoint(Params^[1])^);
end;

procedure _LapeSlackTreeHideNode(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSlackTree(Params^[0])^.HideNode(PInteger(Params^[1])^);
end;

procedure _LapeSlackTreeHideNode2(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PSlackTree(Params^[0])^.HideNode(PPoint(Params^[1])^);
end;

procedure _LapeSlackTreeRawNearest(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Result)^ := PSlackTree(Params^[0])^.RawNearest(PPoint(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeSlackTreeNearest(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PSlackTree(Params^[0])^.Nearest(PPoint(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeSlackTreeRawKNearest(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TNodeRefArray(Result^) := PSlackTree(Params^[0])^.RawKNearest(PPoint(Params^[1])^, PInteger(Params^[2])^, PBoolean(Params^[3])^);
end;

procedure _LapeSlackTreeKNearest(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := PSlackTree(Params^[0])^.KNearest(PPoint(Params^[1])^, PInteger(Params^[2])^, PBoolean(Params^[3])^);
end;

procedure _LapeSlackTreeRawRangeQuery(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TNodeRefArray(Result^) := PSlackTree(Params^[0])^.RawRangeQuery(PBox(Params^[1])^);
end;

procedure _LapeSlackTreeRangeQuery(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := PSlackTree(Params^[0])^.RangeQuery(PBox(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeSlackTreeRangeQueryEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := PSlackTree(Params^[0])^.RangeQueryEx(PPoint(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^, PBoolean(Params^[4])^);
end;

procedure _LapeSlackTreeRangeQueryEx2(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := PSlackTree(Params^[0])^.RangeQueryEx(PPoint(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^, PDouble(Params^[4])^, PDouble(Params^[5])^, PBoolean(Params^[6])^);
end;

procedure _LapeSlackTreeRefArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TNodeRefArray(Result^) := PSlackTree(Params^[0])^.RefArray;
end;

procedure ImportSlackTree(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'SlackTree';

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

    ImportingSection := '';
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportSlackTree);

end.

