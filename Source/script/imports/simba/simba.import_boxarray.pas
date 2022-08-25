unit simba.import_boxarray;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.mufasatypes, simba.script_compiler;

(*
TBoxArray
=========
TBoxArray methods
*)

(*
TBoxArray.Create
~~~~~~~~~~~~~~~~
function TBoxArray.Create(Start: TPoint; Columns, Rows, Width, Height: Integer; Spacing: TPoint): TBoxArray; static;
*)
procedure _LapeBoxArray_Create(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Result)^ := TBoxArray.Create(PPoint(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PPoint(Params^[5])^);
end;

(*
TBoxArray.Pack
~~~~~~~~~~~~~~
function TBoxArray.Pack: TBoxArray;
*)
procedure _LapeBoxArray_Pack(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.Pack();
end;

(*
TBoxArray.SortFrom
~~~~~~~~~~~~~~~~~~
procedure TBoxArray.SortFrom(From: TPoint);
*)
procedure _LapeBoxArray_SortFrom(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Params^[0])^.SortFrom(PPoint(Params^[1])^);
end;

(*
TBoxArray.SortByX
~~~~~~~~~~~~~~~~~
procedure TBoxArray.SortByX(LowToHigh: Boolean);
*)
procedure _LapeBoxArray_SortByX(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Params^[0])^.SortByX(PBoolean(Params^[1])^);
end;

(*
TBoxArray.SortByY
~~~~~~~~~~~~~~~~~
procedure TBoxArray.SortByY(LowToHigh: Boolean);
*)
procedure _LapeBoxArray_SortByY(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Params^[0])^.SortByY(PBoolean(Params^[1])^);
end;

(*
TBoxArray.SortByWidth
~~~~~~~~~~~~~~~~~~~~~
procedure TBoxArray.SortByWidth(LowToHigh: Boolean);
*)
procedure _LapeBoxArray_SortByWidth(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Params^[0])^.SortByWidth(PBoolean(Params^[1])^);
end;

(*
TBoxArray.SortByHeight
~~~~~~~~~~~~~~~~~~~~~~
procedure TBoxArray.SortByHeight(LowToHigh: Boolean);
*)
procedure _LapeBoxArray_SortByHeight(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Params^[0])^.SortByHeight(PBoolean(Params^[1])^);
end;

(*
TBoxArray.SortByArea
~~~~~~~~~~~~~~~~~~~~
procedure TBoxArray.SortByArea(LowToHigh: Boolean);
*)
procedure _LapeBoxArray_SortByArea(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Params^[0])^.SortByArea(PBoolean(Params^[1])^);
end;

(*
TBoxArray.Merge
~~~~~~~~~~~~~~~
function TBoxArray.Merge: TBox;
*)
procedure _LapeBoxArray_Merge(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := PBoxArray(Params^[0])^.Merge();
end;

(*
TBoxArray.Centers
~~~~~~~~~~~~~~~~~
function TBoxArray.Centers: TPointArray;
*)
procedure _LapeBoxArray_Centers(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := PBoxArray(Params^[0])^.Centers();
end;

(*
TBoxArray.Offset
~~~~~~~~~~~~~~~~
function TBoxArray.Offset(P: TPoint): TBoxArray;
*)
procedure _LapeBoxArray_Offset1(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.Offset(PPoint(Params^[1])^);
end;

(*
TBoxArray.Offset
~~~~~~~~~~~~~~~~
function TBoxArray.Offset(X, Y: Integer): TBoxArray;
*)
procedure _LapeBoxArray_Offset2(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.Offset(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TBoxArray.Expand
~~~~~~~~~~~~~~~~
function TBoxArray.Expand(SizeMod: Integer): TBoxArray;
*)
procedure _LapeBoxArray_Expand1(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.Expand(PInteger(Params^[1])^);
end;

(*
TBoxArray.Expand
~~~~~~~~~~~~~~~~
function TBoxArray.Expand(WidMod, HeiMod: Integer): TBoxArray;
*)
procedure _LapeBoxArray_Expand2(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.Expand(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure ImportBoxArray(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('https://villavu.github.io/Simba/tboxarray.html');

    addGlobalFunc('function TBoxArray.Create(Start: TPoint; Columns, Rows, Width, Height: Integer; Spacing: TPoint): TBoxArray; static;', @_LapeBoxArray_Create);
    addGlobalFunc('function TBoxArray.Pack: TBoxArray;', @_LapeBoxArray_Pack);

    addGlobalFunc('procedure TBoxArray.SortFrom(From: TPoint);', @_LapeBoxArray_SortFrom);
    addGlobalFunc('procedure TBoxArray.SortByX(LowToHigh: Boolean = True);', @_LapeBoxArray_SortByX);
    addGlobalFunc('procedure TBoxArray.SortByY(LowToHigh: Boolean = True);', @_LapeBoxArray_SortByY);
    addGlobalFunc('procedure TBoxArray.SortByWidth(LowToHigh: Boolean = True);', @_LapeBoxArray_SortByWidth);
    addGlobalFunc('procedure TBoxArray.SortByHeight(LowToHigh: Boolean = True);', @_LapeBoxArray_SortByHeight);
    addGlobalFunc('procedure TBoxArray.SortByArea(LowToHigh: Boolean = True);', @_LapeBoxArray_SortByArea);

    addGlobalFunc('function TBoxArray.Merge: TBox;', @_LapeBoxArray_Merge);
    addGlobalFunc('function TBoxArray.Centers: TPointArray;', @_LapeBoxArray_Centers);
    addGlobalFunc('function TBoxArray.Offset(P: TPoint): TBoxArray; overload;', @_LapeBoxArray_Offset1);
    addGlobalFunc('function TBoxArray.Offset(X, Y: Integer): TBoxArray; overload;', @_LapeBoxArray_Offset2);

    addGlobalFunc('function TBoxArray.Expand(SizeMod: Integer): TBoxArray; overload;', @_LapeBoxArray_Expand1);
    addGlobalFunc('function TBoxArray.Expand(WidMod, HeiMod: Integer): TBoxArray; overload;', @_LapeBoxArray_Expand2);

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportBoxArray);

end.

