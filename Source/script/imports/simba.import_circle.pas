{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.import_circle;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

procedure ImportCircle(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes,
  simba.circle;

(*
TCircle
=======
Record that contains center point and radius.
*)

(*
TCircle.Create
~~~~~~~~~~~~~~
> function TCircle.Create(ACenter: TPoint; ARadius: Integer): TCircle; static;
*)
procedure _LapeCircle_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCircle(Result)^ := TCircle.Create(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TCircle.CreateFromPoints
~~~~~~~~~~~~~~~~~~~~~~~~
> function CreateFromPoints(Points: TPointArray): TCircle; static;
*)
procedure _LapeCircle_CreateFromPoints(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCircle(Result)^ := TCircle.CreateFromPoints(PPointArray(Params^[0])^);
end;

(*
TCircle.ToTPA
~~~~~~~~~~~~~
> function TCircle.ToTPA(Filled: Boolean): TPointArray;
*)
procedure _LapeCircle_ToTPA(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PCircle(Params^[0])^.ToTPA(PBoolean(Params^[1])^);
end;

(*
TCircle.Bounds
~~~~~~~~~~~~~~
> function TCircle.Bounds: TBox;
*)
procedure _LapeCircle_Bounds(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBox(Result)^ := PCircle(Params^[0])^.Bounds();
end;

(*
TCircle.Contains
~~~~~~~~~~~~~~~~
> function TCircle.Contains(P: TPoint): Boolean
*)
procedure _LapeCircle_Contains(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCircle(Params^[0])^.Contains(PPoint(Params^[1])^);
end;

(*
TCircle.PointAtDegrees
~~~~~~~~~~~~~~~~~~~~~~
> function TCircle.PointAtDegrees(Degrees: Double): TPoint;
*)
procedure _LapeCircle_PointAtDegrees(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PCircle(Params^[0])^.PointAtDegrees(PDouble(Params^[1])^);
end;

(*
TCircle.RandomPoint
~~~~~~~~~~~~~~~~~~~
> function TCircle.RandomPoint: TPoint;

Returns a completely random point in the circle.
*)
procedure _LapeCircle_RandomPoint(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PCircle(Params^[0])^.RandomPoint();
end;

(*
TCircle.RandomPointCenter
~~~~~~~~~~~~~~~~~~~~~~~~~
> function TCircle.RandomPointCenter: TPoint;

Returns a random point in the circle which is weighted torwards the circle's center.
*)
procedure _LapeCircle_RandomPointCenter(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PCircle(Params^[0])^.RandomPointCenter();
end;

(*
TCircle.Circumference
~~~~~~~~~~~~~~~~~~~~~
> function TCircle.Circumference: Double;

Returns the distance around the outside of a circle.
*)
procedure _LapeCircle_Circumference(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := PCircle(Params^[0])^.Circumference();
end;

(*
TCircle.Center
~~~~~~~~~~~~~~
> function TCircle.Center: TPoint;

Returns the center point of the circle.
*)
procedure _LapeCircle_Center(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PCircle(Params^[0])^.Center();
end;

(*
TCircle.Circularity
~~~~~~~~~~~~~~~~~~~
> function TCircle.Circularity(TPA: TPointArray): Double;
*)
procedure _LapeCircle_Circularity(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := PCircle(Params^[0])^.Circularity(PPointArray(Params^[1])^);
end;

(*
TCircle.Area
~~~~~~~~~~~~
> function TCircle.Area: Double;

Returns the area the circle covers.
*)
procedure _LapeCircle_Area(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := PCircle(Params^[0])^.Area();
end;

(*
TCircle.Area
~~~~~~~~~~~~
> function TCircle.Area: Double;

Returns the area the circle covers.
*)
procedure _LapeCircle_Expand(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCircle(Result)^ := PCircle(Params^[0])^.Expand(PInteger(Params^[1])^);
end;

(*
TCircle.Area
~~~~~~~~~~~~
> function TCircle.Area: Double;

Returns the area the circle covers.
*)
procedure _LapeCircle_Offset(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCircle(Result)^ := PCircle(Params^[0])^.Offset(PPoint(Params^[1])^);
end;

(*
TCircle.Extract
~~~~~~~~~~~~~~~
> function TCircle.Extract(Points: TPointArray): TPointArray;

Returns the points that **are** in the circle.
*)
procedure _LapeCircle_Extract(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PCircle(Params^[0])^.Extract(PPointArray(Params^[1])^);
end;

(*
TCircle.Exclude
~~~~~~~~~~~~~~~
> function TCircle.Exclude(Points: TPointArray): TPointArray;

Returns the points that are not inside the circle.
*)
procedure _LapeCircle_Exclude(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PCircle(Params^[0])^.Exclude(PPointArray(Params^[1])^);
end;

(*
in
~~
> operator in(Left: TPoint; Right: TCircle): Boolean;
*)
procedure _LapePoint_IN_Cicle(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PPoint(Params^[0])^ in PCircle(Params^[1])^;
end;

procedure ImportCircle(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'TCircle';

    addGlobalType('record X, Y, Radius: Integer; end;', 'TCircle');
    addGlobalType('array of TCircle;', 'TCircleArray');

    addGlobalFunc('function TCircle.Create(X, Y, Radius: Integer): TCircle; static; overload', @_LapeCircle_Create);
    addGlobalFunc('function TCircle.CreateFromPoints(Points: TPointArray): TCircle; static; overload', @_LapeCircle_CreateFromPoints);

    addGlobalFunc('function TCircle.ToTPA(Filled: Boolean): TPointArray', @_LapeCircle_ToTPA);
    addGlobalFunc('function TCircle.Bounds: TBox', @_LapeCircle_Bounds);
    addGlobalFunc('function TCircle.Contains(P: TPoint): Boolean', @_LapeCircle_Contains);
    addGlobalFunc('function TCircle.PointAtDegrees(Degrees: Double): TPoint;', @_LapeCircle_PointAtDegrees);

    addGlobalFunc('function TCircle.RandomPoint: TPoint', @_LapeCircle_RandomPoint);
    addGlobalFunc('function TCircle.RandomPointCenter: TPoint', @_LapeCircle_RandomPointCenter);

    addGlobalFunc('function TCircle.Center: TPoint', @_LapeCircle_Center);
    addGlobalFunc('function TCircle.Circularity(TPA: TPointArray): Double', @_LapeCircle_Circularity);
    addGlobalFunc('function TCircle.Circumference: Double', @_LapeCircle_Circumference);
    addGlobalFunc('function TCircle.Area: Double', @_LapeCircle_Area);
    addGlobalFunc('function TCircle.Expand(Amount: Integer): TCircle', @_LapeCircle_Expand);
    addGlobalFunc('function TCircle.Offset(P: TPoint): TCircle', @_LapeCircle_Offset);
    addGlobalFunc('function TCircle.Extract(Points: TPointArray): TPointArray', @_LapeCircle_Extract);
    addGlobalFunc('function TCircle.Exclude(Points: TPointArray): TPointArray', @_LapeCircle_Exclude);

    addGlobalFunc('operator in(Left: TPoint; Right: TCircle): Boolean;', @_LapePoint_IN_Cicle);

    ImportingSection := '';
  end;
end;

end.
