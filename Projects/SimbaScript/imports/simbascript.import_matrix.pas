unit simbascript.import_matrix;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_Matrix(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  simba.matrix;

//procedure SetSize(a: TSingleMatrix; Width, Height: Int32);
procedure Lape_MatrixSetSize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  MatrixSetSize(TSingleMatrix(Params^[0]^), Int32(Params^[1]^), Int32(Params^[2]^));
end;

//procedure Size(a: TSingleMatrix; out Width, Height: Int32);
procedure Lape_MatrixSize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  MatrixSize(TSingleMatrix(Params^[0]^), Int32(Params^[1]^), Int32(Params^[2]^));
end;

//function Width(a: TSingleMatrix): Int32;
procedure Lape_MatrixWidth(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Int32(Result^) := MatrixWidth(TSingleMatrix(Params^[0]^));
end;

//function Height(a: TSingleMatrix): Int32;
procedure Lape_MatrixHeight(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Int32(Result^) := MatrixHeight(TSingleMatrix(Params^[0]^));
end;

//function ToInt(a: TSingleMatrix): T2DIntArray;
procedure Lape_MatrixToInt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  T2DIntArray(Result^) := MatrixToInt(TSingleMatrix(Params^[0]^));
end;

//function Mean(a: TSingleMatrix): Single;
procedure Lape_MatrixMean(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Single(Result^) := MatrixMean(TSingleMatrix(Params^[0]^));
end;

//procedure MeanStdev(a: TSingleMatrix; out Mean, Stdev: Double);
procedure Lape_MatrixMeanStdev(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  MatrixMeanStdev(TSingleMatrix(Params^[0]^), Single(Params^[1]^), Single(Params^[2]^));
end;

//procedure MinMax(a: TSingleMatrix; out vMin,vMax: Single);
procedure Lape_MatrixMinMax(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  MatrixMinMax(TSingleMatrix(Params^[0]^), Single(Params^[1]^), Single(Params^[2]^));
end;

procedure Lape_MatrixMin(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var _:Single;
begin
  MatrixMinMax(TSingleMatrix(Params^[0]^), Single(Result^), _);
end;

procedure Lape_MatrixMax(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var _:Single;
begin
  MatrixMinMax(TSingleMatrix(Params^[0]^), _, Single(Result^));
end;

//function ArgMax(a: TSingleMatrix): TPoint;
procedure Lape_MatrixArgMax(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TPoint(Result^) := MatrixArgMax(TSingleMatrix(Params^[0]^));
end;

//function ArgMin(a: TSingleMatrix): TPoint;
procedure Lape_MatrixArgMin(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TPoint(Result^) := MatrixArgMin(TSingleMatrix(Params^[0]^));
end;

//function NormMinMax(a: TSingleMatrix; Alpha, Beta: Single): TSingleMatrix;
procedure Lape_NormMinMax(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TSingleMatrix(Result^) := MatrixNormMinMax(TSingleMatrix(Params^[0]^), Single(Params^[1]^), Single(Params^[2]^));
end;

//function Indices(a: TSingleMatrix; Value: Single; Comparator: EComparator): TPointArray;  
procedure Lape_MatrixIndices(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TPointArray(Result^) := MatrixIndices(TSingleMatrix(Params^[0]^), Single(Params^[1]^), EComparator(Params^[2]^));
end;

//function Extract(a: TSingleMatrix; Indices: TPointArray): TSingleArray;
procedure Lape_MatrixExtract(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TSingleArray(Result^) := MatrixExtract(TSingleMatrix(Params^[0]^), TPointArray(Params^[1]^));
end;

//procedure Fill(a: TSingleMatrix; Indices: TPointArray; Values: TSingleArray);
procedure Lape_MatrixFill(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  MatrixFill(TSingleMatrix(Params^[0]^), TPointArray(Params^[1]^), TSingleArray(Params^[2]^));
end;

procedure Lape_Import_Matrix(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    Section := 'Matrix';

    addGlobalFunc('procedure TSingleMatrix.SetSize(Width, Height: Int32);', @Lape_MatrixSetSize);
    addGlobalFunc('procedure TSingleMatrix.Size(out Width, Height: Int32); constref;', @Lape_MatrixSize);
    addGlobalFunc('function TSingleMatrix.Width(): Int32; constref;', @Lape_MatrixWidth);
    addGlobalFunc('function TSingleMatrix.Height(): Int32; constref;', @Lape_MatrixHeight);
    addGlobalFunc('function TSingleMatrix.ToInt(): T2DIntArray; constref;', @Lape_MatrixToInt);
    addGlobalFunc('function TSingleMatrix.Mean(): Single; constref;', @Lape_MatrixMean);
    addGlobalFunc('procedure TSingleMatrix.MeanStdev(out Mean, Stdev: Double); constref;', @Lape_MatrixMeanStdev);
    addGlobalFunc('procedure TSingleMatrix.MinMax(out vMin,vMax: Single); constref;', @Lape_MatrixMinMax);
    addGlobalFunc('function TSingleMatrix.Min(): Single; constref;', @Lape_MatrixMin);
    addGlobalFunc('function TSingleMatrix.Max(): Single; constref;', @Lape_MatrixMax);
    addGlobalFunc('function TSingleMatrix.ArgMax(): TPoint; constref;', @Lape_MatrixArgMax);
    addGlobalFunc('function TSingleMatrix.ArgMin(): TPoint; constref;', @Lape_MatrixArgMin);
    addGlobalFunc('function TSingleMatrix.NormMinMax(Alpha, Beta: Single): TSingleMatrix; constref;', @Lape_NormMinMax);
    addGlobalFunc('function TSingleMatrix.Indices(Value: Single; Comparator: EComparator): TPointArray; constref;', @Lape_MatrixIndices);
    addGlobalFunc('function TSingleMatrix.Extract(Indices: TPointArray): TSingleArray; constref;', @Lape_MatrixExtract);
    addGlobalFunc('procedure TSingleMatrix.Fill(Indices: TPointArray; Values: TSingleArray); constref;', @Lape_MatrixFill);
  end;
end;

end.

