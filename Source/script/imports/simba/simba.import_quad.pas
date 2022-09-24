unit simba.import_quad;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.mufasatypes, simba.script_compiler;

(*
TQuad
=====
The TQuad type is a record which stores four TPoints (Top, Right, Bottom, Left)

See: <https://en.wikipedia.org/wiki/Quadrilateral>

 - `Top` is the "most" top point (lowest Y)
 - `Right` is the "most" right point (highest X)
 - `Bottom` is the "most" bottom point (highest Y)
 - `Left` is the "most" left point (lowest X)
*)

(*
TQuad.Create
~~~~~~~~~~~~
function TQuad.Create(ATop, ARight, ABottom, ALeft: TPoint): TQuad; static;
*)
procedure _LapeQuad_Create1(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PQuad(Result)^ := TQuad.Create(PPoint(Params^[0])^, PPoint(Params^[1])^, PPoint(Params^[2])^, PPoint(Params^[3])^);
end;

(*
TQuad.Create
~~~~~~~~~~~~
function TQuad.Create(Points: TPointArray): TQuad; static;
*)
procedure _LapeQuad_Create2(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PQuad(Result)^ := TQuad.Create(PPointArray(Params^[0])^);
end;

(*
TQuad.ToTPA
~~~~~~~~~~~
function TQuad.ToTPA: TPointArray;
*)
procedure _LapeQuad_ToTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := PQuad(Params^[0])^.ToTPA();
end;

(*
TQuad.Bounds
~~~~~~~~~~~~
function TQuad.Bounds: TBox;
*)
procedure _LapeQuad_Bounds(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := PQuad(Params^[0])^.Bounds();
end;

(*
TQuad.ShortSideLen
~~~~~~~~~~~~~~~~~~
function TQuad.ShortSideLen: Integer;
*)
procedure _LapeQuad_ShortSideLen(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PQuad(Params^[0])^.ShortSideLen();
end;

(*
TQuad.LongSideLen
~~~~~~~~~~~~~~~~~
function TQuad.LongSideLen: Integer;
*)
procedure _LapeQuad_LongSideLen(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PQuad(Params^[0])^.LongSideLen();
end;

(*
TQuad.Mean
~~~~~~~~~~
function TQuad.Mean: TPoint;
*)
procedure _LapeQuad_Mean(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PQuad(Params^[0])^.Mean();
end;

(*
TQuad.Rotate
~~~~~~~~~~~~
function TQuad.Rotate(Angle: Double): TQuad;
*)
procedure _LapeQuad_Rotate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PQuad(Result)^ := PQuad(Params^[0])^.Rotate(PDouble(Params^[1])^);
end;

(*
TQuad.Contains
~~~~~~~~~~~~~~
function TQuad.Contains(P: TPoint): Boolean;
*)
procedure _LapeQuad_Contains1(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PQuad(Params^[0])^.Contains(PPoint(Params^[1])^);
end;

(*
TQuad.Contains
~~~~~~~~~~~~~~
function TQuad.Contains(X, Y: Integer): Boolean;
*)
procedure _LapeQuad_Contains2(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PQuad(Params^[0])^.Contains(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TQuad.Offset
~~~~~~~~~~~~
function TQuad.Offset(P: TPoint): TQuad;
*)
procedure _LapeQuad_Offset1(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PQuad(Result)^ := PQuad(Params^[0])^.Offset(PPoint(Params^[1])^);
end;

(*
TQuad.Offset
~~~~~~~~~~~~
function TQuad.Offset(X, Y: Integer): TQuad;
*)
procedure _LapeQuad_Offset2(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PQuad(Result)^ := PQuad(Params^[0])^.Offset(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TQuad.Filter
~~~~~~~~~~~~
function TQuad.Filter(Points: TPointArray): TPointArray;
*)
procedure _LapeQuad_Filter(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := PQuad(Params^[0])^.Filter(PPointArray(Params^[1])^);
end;

(*
TQuad.Expand
~~~~~~~~~~~~
function TQuad.Expand(Amount: Double): TQuad;
*)
procedure _LapeQuad_Expand(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PQuad(Result)^ := PQuad(Params^[0])^.Expand(PDouble(Params^[1])^);
end;

(*
TQuad.NearestEdge
~~~~~~~~~~~~~~~~~
function TQuad.NearestEdge(P: TPoint): TPoint;
*)
procedure _LapeQuad_NearestEdge(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PQuad(Params^[0])^.NearestEdge(PPoint(Params^[1])^);
end;

(*
TQuad.Area
~~~~~~~~~~
function TQuad.Area: Integer;
*)
procedure _LapeQuad_Area(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PQuad(Params^[0])^.Area();
end;

(*
TQuad.Normalize
~~~~~~~~~~~~~~~
function TQuad.Normalize: TQuad;
*)
procedure _LapeQuad_Normalize(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PQuad(Result)^ := PQuad(Params^[0])^.Normalize();
end;

(*
in
~~
operator in(Left: TPoint; Right: TQuad): Boolean;
*)
procedure _LapeQuad_IN_Quad(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PPoint(Params^[0])^ in PQuad(Params^[1])^;
end;

procedure ImportQuad(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('https://villavu.github.io/Simba/Quad.html');

    addGlobalType(
      'record            ' + LineEnding +
      '  Top: TPoint;    ' + LineEnding +
      '  Right: TPoint;  ' + LineEnding +
      '  Bottom: TPoint; ' + LineEnding +
      '  Left: TPoint;   ' + LineEnding +
      'end;', 'TQuad');

    addGlobalFunc('function TQuad.Create(ATop, ARight, ABottom, ALeft: TPoint): TQuad; static; overload', @_LapeQuad_Create1);
    addGlobalFunc('function TQuad.Create(Points: TPointArray): TQuad; static; overload', @_LapeQuad_Create2);
    addGlobalFunc('function TQuad.ToTPA: TPointArray', @_LapeQuad_ToTPA);
    addGlobalFunc('function TQuad.Bounds: TBox', @_LapeQuad_Bounds);
    addGlobalFunc('function TQuad.ShortSideLen: Integer', @_LapeQuad_ShortSideLen);
    addGlobalFunc('function TQuad.LongSideLen: Integer', @_LapeQuad_LongSideLen);
    addGlobalFunc('function TQuad.Mean: TPoint', @_LapeQuad_Mean);
    addGlobalFunc('function TQuad.Rotate(Angle: Double): TQuad', @_LapeQuad_Rotate);
    addGlobalFunc('function TQuad.Contains(P: TPoint): Boolean; overload', @_LapeQuad_Contains1);
    addGlobalFunc('function TQuad.Contains(X, Y: Integer): Boolean; overload', @_LapeQuad_Contains2);
    addGlobalFunc('function TQuad.Offset(P: TPoint): TQuad; overload', @_LapeQuad_Offset1);
    addGlobalFunc('function TQuad.Offset(X, Y: Integer): TQuad; overload', @_LapeQuad_Offset2);
    addGlobalFunc('function TQuad.Filter(Points: TPointArray): TPointArray', @_LapeQuad_Filter);
    addGlobalFunc('function TQuad.Expand(Amount: Double): TQuad', @_LapeQuad_Expand);
    addGlobalFunc('function TQuad.NearestEdge(P: TPoint): TPoint', @_LapeQuad_NearestEdge);
    addGlobalFunc('function TQuad.Area: Integer', @_LapeQuad_Area);
    addGlobalFunc('function TQuad.Normalize: TQuad', @_LapeQuad_Normalize);

    addGlobalFunc('operator in(Left: TPoint; Right: TQuad): Boolean;', @_LapeQuad_IN_Quad);

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportQuad);

end.

