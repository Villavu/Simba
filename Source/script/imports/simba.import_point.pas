unit simba.import_point;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.script_compiler;

procedure ImportPoint(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes;

(*
TPoint
======
The TPoint type is a record which defines a X,Y coordinate.
*)

(*
Point
~~~~~
> function Point(X, Y: Integer): TPoint;
*)
procedure _LapePoint(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := Point(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

(*
TPoint.Create
~~~~~~~~~~~~~
> function TPoint.Create(X, Y: Integer): TPoint; static;
*)
procedure _LapePoint_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := TPoint.Create(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

(*
TPoint.InPolygon
~~~~~~~~~~~~~~~~
> function TPoint.InPolygon(Poly: TPointArray): Boolean;
*)
procedure _LapePoint_InPolygon(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PPoint(Params^[0])^.InPolygon(PPointArray(Params^[1])^);
end;

(*
TPoint.InCircle
~~~~~~~~~~~~~~~
> function TPoint.InCircle(Center: TPoint; Radius: Double): Boolean;
*)
procedure _LapePoint_InCircle(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PPoint(Params^[0])^.InCircle(PPoint(Params^[1])^, PDouble(Params^[2])^);
end;

(*
TPoint.InTriangle
~~~~~~~~~~~~~~~~~
> function TPoint.InTriangle(A, B, C: TPoint): Boolean;
*)
procedure _LapePoint_InTriangle(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PPoint(Params^[0])^.InTriangle(PPoint(Params^[1])^, PPoint(Params^[2])^, PPoint(Params^[3])^);
end;

(*
TPoint.InBox
~~~~~~~~~~~~
> function TPoint.InBox(Box: TBox): Boolean;
*)
procedure _LapePoint_InBox(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PPoint(Params^[0])^.InBox(PBox(Params^[1])^);
end;

(*
TPoint.DistanceTo
~~~~~~~~~~~~~~~~~
> function TPoint.DistanceTo(Other: TPoint): Double;
*)
procedure _LapePoint_DistanceTo(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := PPoint(Params^[0])^.DistanceTo(PPoint(Params^[1])^);
end;

(*
TPoint.Rotate
~~~~~~~~~~~~~
> function TPoint.Rotate(Radians: Double; Center: TPoint): TPoint;
*)
procedure _LapePoint_Rotate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PPoint(Params^[0])^.Rotate(PDouble(Params^[1])^, PPoint(Params^[2])^);
end;

(*
TPoint.RotateFast
~~~~~~~~~~~~~~~~~
> function TPoint.RotateFast(Degrees: Integer; Center: TPoint): TPoint;
*)
procedure _LapePoint_RotateFast(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PPoint(Params^[0])^.RotateFast(PInteger(Params^[1])^, PPoint(Params^[2])^);
end;

(*
TPoint.Magnitude
~~~~~~~~~~~~~~~~
> function TPoint.Magnitude: Double;
*)
procedure _LapePoint_Magnitude(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := PPoint(Params^[0])^.Magnitude();
end;

(*
TPoint.AngleBetween
~~~~~~~~~~~~~~~~~~~
> function TPoint.AngleBetween(Other: TPoint): Double;
*)
procedure _LapePoint_AngleBetween(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := PPoint(Params^[0])^.AngleBetween(PPoint(Params^[1])^);
end;

(*
TPoint.Offset
~~~~~~~~~~~~~
> function TPoint.Offset(X, Y: Integer): TPoint;
*)
procedure _LapePoint_Offset1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PPoint(Params^[0])^.Offset(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TPoint.Offset
~~~~~~~~~~~~~
> function TPoint.Offset(P: TPoint): TPoint;
*)
procedure _LapePoint_Offset2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PPoint(Params^[0])^.Offset(PPoint(Params^[1])^);
end;

(*
TPoint.Random
~~~~~~~~~~~~~
> function TPoint.Random(Min, Max: Integer): TPoint;
*)
procedure _LapePoint_Random1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PPoint(Params^[0])^.Random(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TPoint.Random
~~~~~~~~~~~~~
> function TPoint.Random(Value: Integer): TPoint;
*)
procedure _LapePoint_Random2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PPoint(Params^[0])^.Random(PInteger(Params^[1])^);
end;

(*
TPoint +
~~~~~~~~
> operator + (L, R: TPoint): TPoint;
*)
procedure _LapePoint_Plus_Point(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PPoint(Params^[0])^ + PPoint(Params^[1])^;
end;

(*
TPoint +=
~~~~~~~~~
> operator += (var L: TPoint; R: TPoint): TPoint;
*)
procedure _LapePoint_PlusAssign_Point(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Params^[0])^ += PPoint(Params^[1])^;
  PPoint(Result)^ := PPoint(Params^[0])^;
end;

(*
TPoint *
~~~~~~~~
> operator * (L: TPoint; R: Double): TPoint;
*)
procedure _LapePoint_Multiply_Double(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PPoint(Params^[0])^ * PDouble(Params^[1])^;
end;


(*
TPoint \*=
~~~~~~~~~~
> operator *= (var L: TPoint; R: Double): TPoint;
*)
procedure _LapePoint_MultiplyAssign_Double(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Params^[0])^ *= PDouble(Params^[1])^;
  PPoint(Result)^ := PPoint(Params^[0])^;
end;

(*
TPoint -
~~~~~~~~
> operator - (L, R: TPoint): TPoint;
*)
procedure _LapePoint_Minus_Point(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PPoint(Params^[0])^ - PPoint(Params^[1])^;
end;

(*
TPoint -=
~~~~~~~~~
> operator -= (var L: TPoint; R: TPoint): TPoint;
*)
procedure _LapePoint_MinusAssign_Point(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Params^[0])^ -= PPoint(Params^[1])^;
  PPoint(Result)^ := PPoint(Params^[0])^;
end;

(*
TPoint in
~~~~~~~~~
> operator in(Left: TPoint; Right: TBox): Boolean;
*)
procedure _LapePoint_IN_Box(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PPoint(Params^[0])^ in PBox(Params^[1])^;
end;

procedure ImportPoint(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'TPoint';

    addGlobalFunc('function Point(X, Y: Integer): TPoint', @_LapePoint);

    addGlobalFunc('function TPoint.Create(X, Y: Integer): TPoint; static;', @_LapePoint_Create);
    addGlobalFunc('function TPoint.InTriangle(A, B, C: TPoint): Boolean;', @_LapePoint_InTriangle);
    addGlobalFunc('function TPoint.InPolygon(Poly: TPointArray): Boolean;', @_LapePoint_InPolygon);
    addGlobalFunc('function TPoint.InCircle(Center: TPoint; Radius: Double): Boolean;', @_LapePoint_InCircle);
    addGlobalFunc('function TPoint.InBox(Box: TBox): Boolean;', @_LapePoint_InBox);
    addGlobalFunc('function TPoint.DistanceTo(Other: TPoint): Double;', @_LapePoint_DistanceTo);
    addGlobalFunc('function TPoint.Rotate(Radians: Double; Center: TPoint): TPoint;', @_LapePoint_Rotate);
    addGlobalFunc('function TPoint.RotateFast(Degrees: Integer; Center: TPoint): TPoint;', @_LapePoint_RotateFast);
    addGlobalFunc('function TPoint.Magnitude: Double;', @_LapePoint_Magnitude);
    addGlobalFunc('function TPoint.AngleBetween(Other: TPoint): Double;', @_LapePoint_AngleBetween);
    addGlobalFunc('function TPoint.Offset(X, Y: Integer): TPoint; overload;', @_LapePoint_Offset1);
    addGlobalFunc('function TPoint.Offset(P: TPoint): TPoint; overload;', @_LapePoint_Offset2);
    addGlobalFunc('function TPoint.Random(Min, Max: Integer): TPoint; overload;', @_LapePoint_Random1);
    addGlobalFunc('function TPoint.Random(Value: Integer): TPoint; overload;', @_LapePoint_Random2);

    addGlobalFunc('operator + (L, R: TPoint): TPoint;', @_LapePoint_Plus_Point);
    addGlobalFunc('operator += (var L: TPoint; R: TPoint): TPoint;', @_LapePoint_PlusAssign_Point);
    addGlobalFunc('operator - (L, R: TPoint): TPoint;', @_LapePoint_Minus_Point);
    addGlobalFunc('operator -= (var L: TPoint; R: TPoint): TPoint;', @_LapePoint_MinusAssign_Point);
    addGlobalFunc('operator * (L: TPoint; R: Double): TPoint;', @_LapePoint_Multiply_Double);
    addGlobalFunc('operator *= (var L: TPoint; R: Double): TPoint;', @_LapePoint_MultiplyAssign_Double);
    addGlobalFunc('operator in(Left: TPoint; Right: TBox): Boolean;', @_LapePoint_IN_Box);

    ImportingSection := '';
  end;
end;

end.