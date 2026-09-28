unit simba.import_matrix;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script;

procedure ImportMatrix(Script: TSimbaScript);

implementation

uses
  lptypes,
  simba.vartype_matrix;

(*
Matrix
======
Matrix related methods.
*)

(*
TIntegerMatrix.Width
--------------------
```
property TIntegerMatrix.Width: Integer;
```
*)
procedure _LapeIntegerMatrix_Width(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PIntegerMatrix(Params^[0])^.Width;
end;

(*
TIntegerMatrix.Height
---------------------
```
property TIntegerMatrix.Height: Integer;
```
*)
procedure _LapeIntegerMatrix_Height(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PIntegerMatrix(Params^[0])^.Height;
end;

(*
TIntegerMatrix.Area
-------------------
```
property TIntegerMatrix.Area: Integer;
```
*)
procedure _LapeIntegerMatrix_Area(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PIntegerMatrix(Params^[0])^.Area;
end;

(*
TIntegerMatrix.SetSize
----------------------
```
procedure TIntegerMatrix.SetSize(NewWidth, NewHeight: Integer);
```
*)
procedure _LapeIntegerMatrix_SetSize(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerMatrix(Params^[0])^.SetSize(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TIntegerMatrix.GetSize
----------------------
```
function TIntegerMatrix.GetSize(out Width, Height: Integer): Boolean;
```
*)
procedure _LapeIntegerMatrix_GetSize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PIntegerMatrix(Params^[0])^.GetSize(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TIntegerMatrix.GetValues
------------------------
```
function TIntegerMatrix.GetValues(Indices: TPointArray): TIntegerArray;
```
*)
procedure _LapeIntegerMatrix_GetValues(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerArray(Result)^ := PIntegerMatrix(Params^[0])^.GetValues(PPointArray(Params^[1])^);
end;

(*
TIntegerMatrix.SetValue
-----------------------
```
procedure TIntegerMatrix.SetValue(Indices: TPointArray; Value: Integer);
```
*)
procedure _LapeIntegerMatrix_SetValue(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerMatrix(Params^[0])^.SetValue(PPointArray(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TIntegerMatrix.SetValues
------------------------
```
procedure TIntegerMatrix.SetValues(Indices: TPointArray; Values: TIntegerArray);
```
*)
procedure _LapeIntegerMatrix_SetValues(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerMatrix(Params^[0])^.SetValues(PPointArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

(*
TIntegerMatrix.Fill
-------------------
```
procedure TIntegerMatrix.Fill(Area: TBox; Value: Integer);
```
*)
procedure _LapeIntegerMatrix_FillArea(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerMatrix(Params^[0])^.Fill(PBox(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TIntegerMatrix.Fill
-------------------
```
procedure TIntegerMatrix.Fill(Value: Integer);
```
*)
procedure _LapeIntegerMatrix_Fill(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerMatrix(Params^[0])^.Fill(PInteger(Params^[1])^);
end;

(*
TIntegerMatrix.Flatten
----------------------
```
function TIntegerMatrix.Flatten: TIntegerArray;
```
*)
procedure _LapeIntegerMatrix_Flatten(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerArray(Result)^ := PIntegerMatrix(Params^[0])^.Flatten;
end;

(*
TIntegerMatrix.Indices
----------------------
```
function TIntegerMatrix.Indices(Value: Integer; Comparator: EComparator): TPointArray;
```
*)
procedure _LapeIntegerMatrix_Indices(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PIntegerMatrix(Params^[0])^.Indices(PInteger(Params^[1])^, PComparator(Params^[2])^);
end;

(*
TIntegerMatrix.Copy
-------------------
```
function TIntegerMatrix.Copy: TIntegerMatrix;
```
*)
procedure _LapeIntegerMatrix_Copy1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerMatrix(Result)^ := PIntegerMatrix(Params^[0])^.Copy();
end;

(*
TIntegerMatrix.Copy
-------------------
```
function TIntegerMatrix.Copy(Y1, Y2: Integer): TIntegerMatrix;
```
*)
procedure _LapeIntegerMatrix_Copy2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerMatrix(Result)^ := PIntegerMatrix(Params^[0])^.Copy(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TBooleanMatrix.Width
--------------------
```
property TBooleanMatrix.Width: Integer;
```
*)
procedure _LapeBooleanMatrix_Width(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PBooleanMatrix(Params^[0])^.Width;
end;

(*
TBooleanMatrix.Height
---------------------
```
property TBooleanMatrix.Height: Integer;
```
*)
procedure _LapeBooleanMatrix_Height(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PBooleanMatrix(Params^[0])^.Height;
end;

(*
TBooleanMatrix.Area
-------------------
```
property TBooleanMatrix.Area: Integer;
```
*)
procedure _LapeBooleanMatrix_Area(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PBooleanMatrix(Params^[0])^.Area;
end;


(*
TBooleanMatrix.SetSize
----------------------
```
procedure TBooleanMatrix.SetSize(Width, Height: Integer);
```
*)
procedure _LapeBooleanMatrix_SetSize(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PBooleanMatrix(Params^[0])^.SetSize(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TBooleanMatrix.GetSize
----------------------
```
function TBooleanMatrix.GetSize(out Width, Height: Integer): Boolean;
```
*)
procedure _LapeBooleanMatrix_GetSize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PBooleanMatrix(Params^[0])^.GetSize(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TSingleMatrix.Width
-------------------
```
property TSingleMatrix.Width: Integer;
```
*)
procedure _LapeSingleMatrix_Width(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSingleMatrix(Params^[0])^.Width;
end;

(*
TSingleMatrix.Height
--------------------
```
property TSingleMatrix.Height: Integer;
```
*)
procedure _LapeSingleMatrix_Height(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSingleMatrix(Params^[0])^.Height;
end;

(*
TSingleMatrix.Min
-----------------
```
function TSingleMatrix.Min: Single;
```
*)
procedure _LapeSingleMatrix_Min(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PSingleMatrix(Params^[0])^.Min;
end;

(*
TSingleMatrix.Max
-----------------
```
function TSingleMatrix.Max: Single;
```
*)
procedure _LapeSingleMatrix_Max(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PSingleMatrix(Params^[0])^.Max;
end;

(*
TSingleMatrix.Mean
------------------
```
function TSingleMatrix.Mean: Single;
```
*)
procedure _LapeSingleMatrix_Mean(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PSingleMatrix(Params^[0])^.Mean;
end;

(*
TSingleMatrix.Area
------------------
```
property TSingleMatrix.Area: Integer;
```
*)
procedure _LapeSingleMatrix_Area(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSingleMatrix(Params^[0])^.Area;
end;

(*
TSingleMatrix.ArgMax
--------------------
```
function TSingleMatrix.ArgMax: TPoint;
```
*)
procedure _LapeSingleMatrix_ArgMax(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSingleMatrix(Params^[0])^.ArgMax;
end;

(*
TSingleMatrix.ArgMin
--------------------
```
function TSingleMatrix.ArgMin: TPoint;
```
*)
procedure _LapeSingleMatrix_ArgMin(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSingleMatrix(Params^[0])^.ArgMin;
end;

(*
TSingleMatrix.SetSize
---------------------
```
procedure TSingleMatrix.SetSize(NewWidth, NewHeight: Integer);
```
*)
procedure _LapeSingleMatrix_SetSize(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Params^[0])^.SetSize(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TSingleMatrix.GetSize
---------------------
```
function TSingleMatrix.GetSize(out Width, Height: Integer): Boolean;
```
*)
procedure _LapeSingleMatrix_GetSize(const Params: PParamArray ; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSingleMatrix(Params^[0])^.GetSize(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TSingleMatrix.GetValues
-----------------------
```
function TSingleMatrix.GetValues(Indices: TPointArray): TSingleArray;
```
*)
procedure _LapeSingleMatrix_GetValues(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleArray(Result)^ := PSingleMatrix(Params^[0])^.GetValues(PPointArray(Params^[1])^);
end;

(*
TSingleMatrix.SetValues
-----------------------
```
procedure TSingleMatrix.SetValues(Indices: TPointArray; Values: TSingleArray);
```
*)
procedure _LapeSingleMatrix_SetValues(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Params^[0])^.SetValues(PPointArray(Params^[1])^, PSingleArray(Params^[2])^);
end;

(*
TSingleMatrix.SetValue
----------------------
```
procedure TSingleMatrix.SetValue(Indices: TPointArray; Value: Single);
```
*)
procedure _LapeSingleMatrix_SetValue(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Params^[0])^.SetValue(PPointArray(Params^[1])^, PSingle(Params^[2])^);
end;

(*
TSingleMatrix.Fill
------------------
```
procedure TSingleMatrix.Fill(Area: TBox; Value: Single);
```
*)
procedure _LapeSingleMatrix_FillArea(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Params^[0])^.Fill(PBox(Params^[1])^, PSingle(Params^[2])^);
end;

(*
TSingleMatrix.Fill
------------------
```
procedure TSingleMatrix.Fill(Value: Single);
```
*)
procedure _LapeSingleMatrix_Fill(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Params^[0])^.Fill(PSingle(Params^[1])^);
end;

(*
TSingleMatrix.Flatten
---------------------
```
function TSingleMatrix.Flatten: TSingleArray;
```
*)
procedure _LapeSingleMatrix_Flatten(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleArray(Result)^ := PSingleMatrix(Params^[0])^.Flatten;
end;

(*
TSingleMatrix.ToIntegerMatrix
-----------------------------
```
function TSingleMatrix.ToIntegerMatrix: TIntegerMatrix;
```
*)
procedure _LapeSingleMatrix_ToIntegerMatrix(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerMatrix(Result)^ := PSingleMatrix(Params^[0])^.ToIntegerMatrix();
end;

(*
TSingleMatrix.MeanStdev
-----------------------
```
procedure TSingleMatrix.MeanStdev(out Mean, Stdev: Double);
```
*)
procedure _LapeSingleMatrix_MeanStdev(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Params^[0])^.MeanStdev(PDouble(Params^[1])^, PDouble(Params^[2])^);
end;

(*
TSingleMatrix.MinMax
--------------------
```
procedure TSingleMatrix.MinMax(out MinValue, MaxValue: Single);
```
*)
procedure _LapeSingleMatrix_MinMax(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Params^[0])^.MinMax(PSingle(Params^[1])^, PSingle(Params^[2])^);
end;

(*
TSingleMatrix.NormMinMax
------------------------
```
procedure TSingleMatrix.NormMinMax(Alpha, Beta: Single);
```
*)
procedure _LapeSingleMatrix_NormMinMax(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Params^[0])^.NormMinMax(PSingle(Params^[1])^, PSingle(Params^[2])^);
end;

(*
TSingleMatrix.Indices
---------------------
```
function TSingleMatrix.Indices(Value: Single; Comparator: EComparator): TPointArray;
```
*)
procedure _LapeSingleMatrix_Indices(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSingleMatrix(Params^[0])^.Indices(PSingle(Params^[1])^, PComparator(Params^[2])^);
end;

(*
TSingleMatrix.ArgMulti
----------------------
```
function TSingleMatrix.ArgMulti(Count: Integer; HiLo: Boolean): TPointArray;
```
*)
procedure _LapeSingleMatrix_ArgMulti(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSingleMatrix(Params^[0])^.ArgMulti(PInteger(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TSingleMatrix.Smoothen
----------------------
```
procedure TSingleMatrix.Smoothen(Block: Integer);
```
*)
procedure _LapeSingleMatrix_Smoothen(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Params^[0])^.Smoothen(PInteger(Params^[1])^);
end;

(*
TSingleMatrix.Equals
--------------------
```
function TSingleMatrix.Equals(Other: TSingleMatrix; Epsilon: Single = 0): Boolean;
```
*)
procedure _LapeSingleMatrix_Equals(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSingleMatrix(Params^[0])^.Equals(PSingleMatrix(Params^[1])^, PSingle(Params^[2])^);
end;

(*
TSingleMatrix.Copy
------------------
```
function TSingleMatrix.Copy: TSingleMatrix;
```
*)
procedure _LapeSingleMatrix_Copy1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Result)^ := PSingleMatrix(Params^[0])^.Copy();
end;

(*
TSingleMatrix.Copy
------------------
```
function TSingleMatrix.Copy(Y1, Y2: Integer): TSingleMatrix;
```
*)
procedure _LapeSingleMatrix_Copy2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Result)^ := PSingleMatrix(Params^[0])^.Copy(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TSingleMatrix.Rot90
-------------------
```
function TSingleMatrix.Rot90: TSingleMatrix;
```
*)
procedure _LapeSingleMatrix_Rot90(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Result)^ := PSingleMatrix(Params^[0])^.Rot90();
end;

(*
TSingleMatrix.ArgExtrema
------------------------
```
function TSingleMatrix.ArgExtrema(Count: Int32; HiLo: Boolean = True): TPointArray;
```
*)
procedure _LapeSingleMatrix_ArgExtrema(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSingleMatrix(Params^[0])^.ArgExtrema(PInteger(Params^[1])^, PBoolean(Params^[2])^, PBoolean(Params^[3])^);
end;

(*
TDoubleMatrix.Width
-------------------
```
property TDoubleMatrix.Width: Integer;
```
*)
procedure _LapeDoubleMatrix_Width(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PDoubleMatrix(Params^[0])^.Width;
end;

(*
TDoubleMatrix.Height
--------------------
```
property TDoubleMatrix.Height: Integer;
```
*)
procedure _LapeDoubleMatrix_Height(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PDoubleMatrix(Params^[0])^.Height;
end;

(*
TDoubleMatrix.Area
------------------
```
property TDoubleMatrix.Area: Integer;
```
*)
procedure _LapeDoubleMatrix_Area(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PDoubleMatrix(Params^[0])^.Area;
end;

(*
TDoubleMatrix.SetSize
---------------------
```
procedure TDoubleMatrix.SetSize(NewWidth, NewHeight: Integer);
```
*)
procedure _LapeDoubleMatrix_SetSize(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDoubleMatrix(Params^[0])^.SetSize(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TDoubleMatrix.GetSize
---------------------
```
function TDoubleMatrix.GetSize(out Width, Height: Integer): Boolean;
```
*)
procedure _LapeDoubleMatrix_GetSize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PDoubleMatrix(Params^[0])^.GetSize(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure ImportMatrix(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    DumpSection := 'Matrix';

    // single
    addGlobalFunc('property TSingleMatrix.Width: Integer;', @_LapeSingleMatrix_Width);
    addGlobalFunc('property TSingleMatrix.Height: Integer;', @_LapeSingleMatrix_Height);
    addGlobalFunc('property TSingleMatrix.Area: Integer;', @_LapeSingleMatrix_Area);
    addGlobalFunc('function TSingleMatrix.Min: Single;', @_LapeSingleMatrix_Min);
    addGlobalFunc('function TSingleMatrix.Max: Single;', @_LapeSingleMatrix_Max);
    addGlobalFunc('function TSingleMatrix.Mean: Single;', @_LapeSingleMatrix_Mean);
    addGlobalFunc('function TSingleMatrix.ArgMax: TPoint;', @_LapeSingleMatrix_ArgMax);
    addGlobalFunc('function TSingleMatrix.ArgMin: TPoint;', @_LapeSingleMatrix_ArgMin);
    addGlobalFunc('procedure TSingleMatrix.SetSize(NewWidth, NewHeight: Integer);', @_LapeSingleMatrix_SetSize);
    addGlobalFunc('function TSingleMatrix.GetSize(out Width, Height: Integer): Boolean;', @_LapeSingleMatrix_GetSize);
    addGlobalFunc('function TSingleMatrix.GetValues(Indices: TPointArray): TSingleArray;', @_LapeSingleMatrix_GetValues);
    addGlobalFunc('procedure TSingleMatrix.SetValues(Indices: TPointArray; Values: TSingleArray)', @_LapeSingleMatrix_SetValues);
    addGlobalFunc('procedure TSingleMatrix.SetValue(Indices: TPointArray; Value: Single)', @_LapeSingleMatrix_SetValue);
    addGlobalFunc('procedure TSingleMatrix.Fill(Area: TBox; Value: Single); overload', @_LapeSingleMatrix_FillArea);
    addGlobalFunc('procedure TSingleMatrix.Fill(Value: Single); overload', @_LapeSingleMatrix_Fill);
    addGlobalFunc('function TSingleMatrix.Flatten: TSingleArray', @_LapeSingleMatrix_Flatten);
    addGlobalFunc('function TSingleMatrix.ToIntegerMatrix: TIntegerMatrix;', @_LapeSingleMatrix_ToIntegerMatrix);
    addGlobalFunc('procedure TSingleMatrix.MeanStdev(out Mean, Stdev: Double);', @_LapeSingleMatrix_MeanStdev);
    addGlobalFunc('procedure TSingleMatrix.MinMax(out MinValue, MaxValue: Single);', @_LapeSingleMatrix_MinMax);
    addGlobalFunc('procedure TSingleMatrix.NormMinMax(Alpha, Beta: Single);', @_LapeSingleMatrix_NormMinMax);
    addGlobalFunc('function TSingleMatrix.Indices(Value: Single; Comparator: EComparator): TPointArray;', @_LapeSingleMatrix_Indices);
    addGlobalFunc('function TSingleMatrix.ArgMulti(Count: Integer; HiLo: Boolean): TPointArray;', @_LapeSingleMatrix_ArgMulti);
    addGlobalFunc('procedure TSingleMatrix.Smoothen(Block: Integer);', @_LapeSingleMatrix_Smoothen);
    addGlobalFunc('function TSingleMatrix.Equals(Other: TSingleMatrix; Epsilon: Single = 0): Boolean; overload;', @_LapeSingleMatrix_Equals);
    addGlobalFunc('function TSingleMatrix.Copy: TSingleMatrix; overload', @_LapeSingleMatrix_Copy1);
    addGlobalFunc('function TSingleMatrix.Copy(Y1, Y2: Integer): TSingleMatrix; overload', @_LapeSingleMatrix_Copy2);
    addGlobalFunc('function TSingleMatrix.Rot90: TSingleMatrix;', @_LapeSingleMatrix_Rot90);
    addGlobalFunc('function TSingleMatrix.ArgExtrema(Count: Int32; HiLo: Boolean = True; XYIntersection: Boolean = True): TPointArray;', @_LapeSingleMatrix_ArgExtrema);

    // double
    addGlobalFunc('property TDoubleMatrix.Width: Integer;', @_LapeDoubleMatrix_Width);
    addGlobalFunc('property TDoubleMatrix.Height: Integer;', @_LapeDoubleMatrix_Height);
    addGlobalFunc('property TDoubleMatrix.Area: Integer;', @_LapeDoubleMatrix_Area);
    addGlobalFunc('procedure TDoubleMatrix.SetSize(NewWidth, NewHeight: Integer);', @_LapeDoubleMatrix_SetSize);
    addGlobalFunc('function TDoubleMatrix.GetSize(out Width, Height: Integer): Boolean;', @_LapeDoubleMatrix_GetSize);

    // integer
    addGlobalFunc('property TIntegerMatrix.Width: Integer;', @_LapeIntegerMatrix_Width);
    addGlobalFunc('property TIntegerMatrix.Height: Integer;', @_LapeIntegerMatrix_Height);
    addGlobalFunc('property TIntegerMatrix.Area: Integer;', @_LapeIntegerMatrix_Area);
    addGlobalFunc('procedure TIntegerMatrix.SetSize(NewWidth, NewHeight: Integer);', @_LapeIntegerMatrix_SetSize);
    addGlobalFunc('function TIntegerMatrix.GetSize(out Width, Height: Integer): Boolean;', @_LapeIntegerMatrix_GetSize);
    addGlobalFunc('function TIntegerMatrix.GetValues(Indices: TPointArray): TIntegerArray;', @_LapeIntegerMatrix_GetValues);
    addGlobalFunc('procedure TIntegerMatrix.SetValues(Indices: TPointArray; Values: TIntegerArray);', @_LapeIntegerMatrix_SetValues);
    addGlobalFunc('procedure TIntegerMatrix.SetValue(Indices: TPointArray; Value: Integer);', @_LapeIntegerMatrix_SetValue);
    addGlobalFunc('procedure TIntegerMatrix.Fill(Area: TBox; Value: Integer); overload', @_LapeIntegerMatrix_FillArea);
    addGlobalFunc('procedure TIntegerMatrix.Fill(Value: Integer); overload', @_LapeIntegerMatrix_Fill);
    addGlobalFunc('function TIntegerMatrix.Flatten: TIntegerArray', @_LapeIntegerMatrix_Flatten);
    addGlobalFunc('function TIntegerMatrix.Indices(Value: Integer; Comparator: EComparator): TPointArray;', @_LapeIntegerMatrix_Indices);
    addGlobalFunc('function TIntegerMatrix.Copy: TIntegerMatrix; overload', @_LapeIntegerMatrix_Copy1);
    addGlobalFunc('function TIntegerMatrix.Copy(Y1, Y2: Integer): TIntegerMatrix; overload', @_LapeIntegerMatrix_Copy2);

    // boolean
    addGlobalFunc('property TBooleanMatrix.Width: Integer;', @_LapeBooleanMatrix_Width);
    addGlobalFunc('property TBooleanMatrix.Height: Integer;', @_LapeBooleanMatrix_Height);
    addGlobalFunc('property TBooleanMatrix.Area: Integer;', @_LapeBooleanMatrix_Area);
    addGlobalFunc('procedure TBooleanMatrix.SetSize(NewWidth, NewHeight: Integer);', @_LapeBooleanMatrix_SetSize);
    addGlobalFunc('function TBooleanMatrix.GetSize(out Width, Height: Integer): Boolean;', @_LapeBooleanMatrix_GetSize);

    DumpSection := '';
  end;
end;

end.
