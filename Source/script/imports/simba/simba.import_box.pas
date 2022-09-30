unit simba.import_box;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.mufasatypes, simba.script_compiler;

(*
TBox
====
The TBox type is a record which defines a box.
*)

(*
Box
~~~
function Box(X1, Y1, X2, Y2: Integer): TBox;
*)
procedure _LapeBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := Box(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

(*
Box
~~~
function Box(Mid: TPoint; XRad, YRad: Integer): TBox;
*)
procedure _LapeBoxEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := Box(PPoint(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TBox.Create
~~~~~~~~~~~
function TBox.Create(X1, Y1, X2, Y2: Integer): TBox; static;
*)
procedure _LapeBox_Create1(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := TBox.Create(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TBox.Create
~~~~~~~~~~~
function TBox.Create(Center: TPoint; XRad, YRad: Integer): TBox; static;
*)
procedure _LapeBox_Create2(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := TBox.Create(PPoint(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TBox.EqualDimensions
~~~~~~~~~~~~~~~~~~~~
function TBox.EqualDimensions(Other: TBox): Boolean;
*)
procedure _LapeBox_EqualDimensions(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PBox(Params^[0])^.EqualDimensions(PBox(Params^[1])^);
end;

(*
TBox.Area
~~~~~~~~~
function TBox.Area: Integer;
*)
procedure _LapeBox_Area(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PBox(Params^[0])^.Area();
end;

(*
TBox.Expand
~~~~~~~~~~~
function TBox.Expand(SizeMod: Integer): TBox;
*)
procedure _LapeBox_Expand1(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := PBox(Params^[0])^.Expand(PInteger(Params^[1])^);
end;

(*
TBox.Expand
~~~~~~~~~~~
function TBox.Expand(WidMod, HeiMod: Integer): TBox;
*)
procedure _LapeBox_Expand2(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := PBox(Params^[0])^.Expand(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TBox.Expand
~~~~~~~~~~~
function TBox.Expand(SizeMod: Integer; MaxBounds: TBox): TBox;
*)
procedure _LapeBox_Expand3(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := PBox(Params^[0])^.Expand(PInteger(Params^[1])^, PBox(Params^[2])^);
end;

(*
TBox.Expand
~~~~~~~~~~~
function TBox.Expand(WidMod, HeiMod: Integer; MaxBounds: TBox): TBox;
*)
procedure _LapeBox_Expand4(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := PBox(Params^[0])^.Expand(PInteger(Params^[1])^, PInteger(Params^[2])^, PBox(Params^[3])^);
end;

(*
TBox.Filter
~~~~~~~~~~~
function TBox.Filter(Points: TPointArray): TPointArray;

Returns all points that are in the box.
*)
procedure _LapeBox_Filter(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := PBox(Params^[0])^.Filter(PPointArray(Params^[1])^);
end;

(*
TBox.Contains
~~~~~~~~~~~~~
function TBox.Contains(Other: TBox): Boolean;
*)
procedure _LapeBox_Contains1(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PBox(Params^[0])^.Contains(PBox(Params^[1])^);
end;

(*
TBox.Contains
~~~~~~~~~~~~~
function TBox.Contains(Other: TPoint): Boolean;
*)
procedure _LapeBox_Contains2(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PBox(Params^[0])^.Contains(PPoint(Params^[1])^);
end;

(*
TBox.Contains
~~~~~~~~~~~~~
function TBox.Contains(X, Y: Integer): Boolean;
*)
procedure _LapeBox_Contains3(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PBox(Params^[0])^.Contains(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TBox.Contains
~~~~~~~~~~~~~
function TBox.Contains(Other: TQuad): Boolean;
*)
procedure _LapeBox_Contains4(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PBox(Params^[0])^.Contains(PQuad(Params^[1])^);
end;

(*
TBox.Partition
~~~~~~~~~~~~~~
function TBox.Partition(Rows, Cols: Integer): TBoxArray;
*)
procedure _LapeBox_Partition(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Result)^ := PBox(Params^[0])^.Partition(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TBox.Offset
~~~~~~~~~~~
function TBox.Offset(X, Y: Integer): TBox;
*)
procedure _LapeBox_Offset1(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := PBox(Params^[0])^.Offset(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TBox.Offset
~~~~~~~~~~~
function TBox.Offset(P: TPoint): TBox;
*)
procedure _LapeBox_Offset2(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := PBox(Params^[0])^.Offset(PPoint(Params^[1])^);
end;

(*
TBox.Combine
~~~~~~~~~~~~
function TBox.Combine(Other: TBox): TBox;
*)
procedure _LapeBox_Combine(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := PBox(Params^[0])^.Combine(PBox(Params^[1])^);
end;

(*
TBox.Invert
~~~~~~~~~~~
function TBox.Invert(Area: TBox): TBoxArray;
*)
procedure _LapeBox_Invert(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Result)^ := PBox(Params^[0])^.Invert(PBox(Params^[1])^);
end;

(*
TBox.NearestEdge
~~~~~~~~~~~~~~~~
function TBox.NearestEdge(P: TPoint): TPoint;
*)
procedure _LapeBox_NearestEdge(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PBox(Params^[0])^.NearestEdge(PPoint(Params^[1])^);
end;

(*
TBox.Intersect
~~~~~~~~~~~~~~
function TBox.Intersect(P: TPoint): TPoint;
*)
procedure _LapeBox_Intersect(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PBox(Params^[0])^.Intersect(PPoint(Params^[1])^);
end;

(*
TBox.Clip
~~~~~~~~~
procedure TBox.Clip(Other: TBox);
*)
procedure _LapeBox_Clip(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Params^[0])^.Clip(PBox(Params^[1])^);
end;

(*
TBox.Normalize
~~~~~~~~~~~~~~
procedure TBox.Normalize;
*)
procedure _LapeBox_Normalize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Params^[0])^.Normalize();
end;

(*
TBox.Width
~~~~~~~~~~
function TBox.Width: Integer;
*)
procedure _LapeBox_Width(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PBox(Params^[0])^.Width;
end;

(*
TBox.Height
~~~~~~~~~~~
function TBox.Height: Integer;
*)
procedure _LapeBox_Height(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PBox(Params^[0])^.Height;
end;

(*
TBox.Center
~~~~~~~~~~~
function TBox.Center: TPoint;
*)
procedure _LapeBox_Center(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PBox(Params^[0])^.Center;
end;

(*
TBox.Corners
~~~~~~~~~~~~
function TBox.Corners: TPointArray;
*)
procedure _LapeBox_Corners(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := PBox(Params^[0])^.Corners();
end;

procedure _LapeBox_ToQuad(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PQuad(Result)^ := PBox(Params^[0])^.ToQuad();
end;

procedure ImportBox(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('https://villavu.github.io/Simba/Box.html');

    addGlobalFunc('function Box(X1, Y1, X2, Y2: Integer): TBox; overload', @_LapeBox);
    addGlobalFunc('function Box(Mid: TPoint; XRad, YRad: Integer): TBox; overload', @_LapeBoxEx);

    addGlobalFunc('function TBox.Create(X1, Y1, X2, Y2: Integer): TBox; static; overload;', @_LapeBox_Create1);
    addGlobalFunc('function TBox.Create(Center: TPoint; XRad, YRad: Integer): TBox; static; overload;', @_LapeBox_Create2);

    addGlobalFunc('function TBox.EqualDimensions(Other: TBox): Boolean;', @_LapeBox_EqualDimensions);
    addGlobalFunc('function TBox.Area: Integer;', @_LapeBox_Area);
    addGlobalFunc('function TBox.Expand(SizeMod: Integer): TBox; overload;', @_LapeBox_Expand1);
    addGlobalFunc('function TBox.Expand(SizeMod: Integer; MaxBounds: TBox): TBox; overload;', @_LapeBox_Expand3);
    addGlobalFunc('function TBox.Expand(WidMod, HeiMod: Integer): TBox; overload;', @_LapeBox_Expand2);
    addGlobalFunc('function TBox.Expand(WidMod, HeiMod: Integer; MaxBounds: TBox): TBox; overload;', @_LapeBox_Expand4);
    addGlobalFunc('function TBox.Filter(Points: TPointArray): TPointArray', @_LapeBox_Filter);
    addGlobalFunc('function TBox.Contains(Other: TBox): Boolean; overload;', @_LapeBox_Contains1);
    addGlobalFunc('function TBox.Contains(Other: TPoint): Boolean; overload;', @_LapeBox_Contains2);
    addGlobalFunc('function TBox.Contains(X, Y: Integer): Boolean; overload;', @_LapeBox_Contains3);
    addGlobalFunc('function TBox.Contains(Other: TQuad): Boolean; overload;', @_LapeBox_Contains4);
    addGlobalFunc('function TBox.Partition(Rows, Cols: Integer): TBoxArray;', @_LapeBox_Partition);
    addGlobalFunc('function TBox.Offset(X, Y: Integer): TBox; overload;', @_LapeBox_Offset1);
    addGlobalFunc('function TBox.Offset(P: TPoint): TBox; overload;', @_LapeBox_Offset2);
    addGlobalFunc('function TBox.Combine(Other: TBox): TBox;', @_LapeBox_Combine);
    addGlobalFunc('function TBox.Invert(Area: TBox): TBoxArray;', @_LapeBox_Invert);
    addGlobalFunc('function TBox.ToQuad: TQuad;', @_LapeBox_ToQuad);

    addGlobalFunc('function TBox.NearestEdge(P: TPoint): TPoint;', @_LapeBox_NearestEdge);
    addGlobalFunc('function TBox.Intersect(P: TPoint): TPoint;', @_LapeBox_Intersect);

    addGlobalFunc('procedure TBox.Clip(Other: TBox);', @_LapeBox_Clip);
    addGlobalFunc('procedure TBox.Normalize;', @_LapeBox_Normalize);
    addGlobalFunc('function TBox.Corners: TPointArray;', @_LapeBox_Corners);

    addGlobalFunc('function TBox.Width: Integer;', @_LapeBox_Width);
    addGlobalFunc('function TBox.Height: Integer;', @_LapeBox_Height);
    addGlobalFunc('function TBox.Center: TPoint;', @_LapeBox_Center);

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportBox);

end.

