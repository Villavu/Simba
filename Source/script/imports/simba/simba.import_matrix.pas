unit simba.import_matrix;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.script_compiler, simba.mufasatypes, simba.helpers_matrix;

// Integer
procedure _LapeIntegerMatrix_Width(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PIntegerMatrix(Params^[0])^.Width;
end;

procedure _LapeIntegerMatrix_Height(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PIntegerMatrix(Params^[0])^.Height;
end;

procedure _LapeIntegerMatrix_SetSize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerMatrix(Params^[0])^.SetSize(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeIntegerMatrix_Area(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PIntegerMatrix(Params^[0])^.Area();
end;

procedure _LapeIntegerMatrix_Size(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PIntegerMatrix(Params^[0])^.Size(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeIntegerMatrix_GetValues(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := PIntegerMatrix(Params^[0])^.GetValues(PPointArray(Params^[1])^);
end;

procedure _LapeIntegerMatrix_SetValuesArray(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerMatrix(Params^[0])^.SetValues(PPointArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

procedure _LapeIntegerMatrix_SetValues(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerMatrix(Params^[0])^.SetValues(PPointArray(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeIntegerMatrix_FillArea(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerMatrix(Params^[0])^.Fill(PBox(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeIntegerMatrix_Fill(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerMatrix(Params^[0])^.Fill(PInteger(Params^[1])^);
end;

procedure _LapeIntegerMatrix_Flatten(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := PIntegerMatrix(Params^[0])^.Flatten;
end;

procedure _LapeIntegerMatrix_Indices(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := PIntegerMatrix(Params^[0])^.Indices(PInteger(Params^[1])^, PComparator(Params^[2])^);
end;

// Byte
procedure _LapeByteMatrix_Width(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PByteMatrix(Params^[0])^.Width;
end;

procedure _LapeByteMatrix_Height(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PByteMatrix(Params^[0])^.Height;
end;

procedure _LapeByteMatrix_SetSize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PByteMatrix(Params^[0])^.SetSize(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeByteMatrix_Area(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PByteMatrix(Params^[0])^.Area();
end;

procedure _LapeByteMatrix_Size(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PByteMatrix(Params^[0])^.Size(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

// Boolean
procedure _LapeBooleanMatrix_Width(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PBooleanMatrix(Params^[0])^.Width;
end;

procedure _LapeBooleanMatrix_Height(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PBooleanMatrix(Params^[0])^.Height;
end;

procedure _LapeBooleanMatrix_SetSize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBooleanMatrix(Params^[0])^.SetSize(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeBooleanMatrix_Area(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PBooleanMatrix(Params^[0])^.Area();
end;

procedure _LapeBooleanMatrix_Size(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PBooleanMatrix(Params^[0])^.Size(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

// Single
procedure _LapeSingleMatrix_Width(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PSingleMatrix(Params^[0])^.Width;
end;

procedure _LapeSingleMatrix_Height(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PSingleMatrix(Params^[0])^.Height;
end;

procedure _LapeSingleMatrix_SetSize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Params^[0])^.SetSize(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeSingleMatrix_Area(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PSingleMatrix(Params^[0])^.Area();
end;

procedure _LapeSingleMatrix_Size(const Params: PParamArray ; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PSingleMatrix(Params^[0])^.Size(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeSingleMatrix_GetValues(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleArray(Result)^ := PSingleMatrix(Params^[0])^.GetValues(PPointArray(Params^[1])^);
end;

procedure _LapeSingleMatrix_SetValuesArray(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Params^[0])^.SetValues(PPointArray(Params^[1])^, PSingleArray(Params^[2])^);
end;

procedure _LapeSingleMatrix_SetValues(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Params^[0])^.SetValues(PPointArray(Params^[1])^, PSingle(Params^[2])^);
end;

procedure _LapeSingleMatrix_FillArea(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Params^[0])^.Fill(PBox(Params^[1])^, PSingle(Params^[2])^);
end;

procedure _LapeSingleMatrix_Fill(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Params^[0])^.Fill(PSingle(Params^[1])^);
end;

procedure _LapeSingleMatrix_Flatten(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleArray(Result)^ := PSingleMatrix(Params^[0])^.Flatten;
end;

procedure _LapeSingleMatrix_ToIntegerMatrix(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerMatrix(Result)^ := PSingleMatrix(Params^[0])^.ToIntegerMatrix();
end;

procedure _LapeSingleMatrix_Mean(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingle(Result)^ := PSingleMatrix(Params^[0])^.Mean;
end;

procedure _LapeSingleMatrix_MeanStdev(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Params^[0])^.MeanStdev(PDouble(Params^[1])^, PDouble(Params^[2])^);
end;

procedure _LapeSingleMatrix_MinMax(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Params^[0])^.MinMax(PSingle(Params^[1])^, PSingle(Params^[2])^);
end;

procedure _LapeSingleMatrix_Min(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingle(Result)^ := PSingleMatrix(Params^[0])^.Min;
end;

procedure _LapeSingleMatrix_Max(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingle(Result)^ := PSingleMatrix(Params^[0])^.Max;
end;

procedure _LapeSingleMatrix_ArgMax(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PSingleMatrix(Params^[0])^.ArgMax;
end;

procedure _LapeSingleMatrix_ArgMin(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PSingleMatrix(Params^[0])^.ArgMin;
end;

procedure _LapeSingleMatrix_NormMinMax(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Result)^ := PSingleMatrix(Params^[0])^.NormMinMax(PSingle(Params^[1])^, PSingle(Params^[2])^);
end;

procedure _LapeSingleMatrix_Indices(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := PSingleMatrix(Params^[0])^.Indices(PSingle(Params^[1])^, PComparator(Params^[2])^);
end;

procedure _LapeSingleMatrix_ArgMulti(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := PSingleMatrix(Params^[0])^.ArgMulti(PInteger(Params^[1])^, PBoolean(Params^[2])^);
end;

// Double
procedure _LapeDoubleMatrix_Width(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PDoubleMatrix(Params^[0])^.Width;
end;

procedure _LapeDoubleMatrix_Height(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PDoubleMatrix(Params^[0])^.Height;
end;

procedure _LapeDoubleMatrix_SetSize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDoubleMatrix(Params^[0])^.SetSize(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeDoubleMatrix_Area(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PDoubleMatrix(Params^[0])^.Area();
end;

procedure _LapeDoubleMatrix_Size(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PDoubleMatrix(Params^[0])^.Size(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure ImportMatrix(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('Matrix');

    addGlobalType('array of TSingleArray', 'TSingleMatrix');
    addGlobalType('array of TIntegerArray', 'TIntegerMatrix');
    addGlobalType('array of TBooleanArray', 'TBooleanMatrix');
    addGlobalType('array of TByteArray', 'TByteMatrix');
    addGlobalType('array of TDoubleArray', 'TDoubleMatrix');
    addGlobalType('array of TExtendedArray', 'TExtendedMatrix');

    // single
    addGlobalFunc('function TSingleMatrix.Width: Integer;', @_LapeSingleMatrix_Width);
    addGlobalFunc('function TSingleMatrix.Height: Integer;', @_LapeSingleMatrix_Height);
    addGlobalFunc('procedure TSingleMatrix.SetSize(const Width, Height: Integer);', @_LapeSingleMatrix_SetSize);
    addGlobalFunc('function TSingleMatrix.Area: Integer;', @_LapeSingleMatrix_Area);
    addGlobalFunc('function TSingleMatrix.Size(out Width, Height: Integer): Boolean;', @_LapeSingleMatrix_Size);
    addGlobalFunc('function TSingleMatrix.GetValues(Indices: TPointArray): TSingleArray;', @_LapeSingleMatrix_GetValues);
    addGlobalFunc('procedure TSingleMatrix.SetValues(Indices: TPointArray; Values: TSingleArray); overload', @_LapeSingleMatrix_SetValuesArray);
    addGlobalFunc('procedure TSingleMatrix.SetValues(Indices: TPointArray; Value: Single); overload', @_LapeSingleMatrix_SetValues);
    addGlobalFunc('procedure TSingleMatrix.Fill(Area: TBox; Value: Single); overload', @_LapeSingleMatrix_FillArea);
    addGlobalFunc('procedure TSingleMatrix.Fill(Value: Single); overload', @_LapeSingleMatrix_Fill);
    addGlobalFunc('function TSingleMatrix.Flatten: TSingleArray; overload', @_LapeSingleMatrix_Flatten);
    addGlobalFunc('function TSingleMatrix.ToIntegerMatrix: TIntegerMatrix;', @_LapeSingleMatrix_ToIntegerMatrix);
    addGlobalFunc('function TSingleMatrix.Mean: Single; overload;', @_LapeSingleMatrix_Mean);
    addGlobalFunc('procedure TSingleMatrix.MeanStdev(out Mean, Stdev: Double);', @_LapeSingleMatrix_MeanStdev);
    addGlobalFunc('procedure TSingleMatrix.MinMax(out MinValue, MaxValue: Single);', @_LapeSingleMatrix_MinMax);
    addGlobalFunc('function TSingleMatrix.Min: Single; overload;', @_LapeSingleMatrix_Min);
    addGlobalFunc('function TSingleMatrix.Max: Single; overload;', @_LapeSingleMatrix_Max);
    addGlobalFunc('function TSingleMatrix.ArgMax: TPoint;', @_LapeSingleMatrix_ArgMax);
    addGlobalFunc('function TSingleMatrix.ArgMin: TPoint;', @_LapeSingleMatrix_ArgMin);
    addGlobalFunc('function TSingleMatrix.NormMinMax(Alpha, Beta: Single): TSingleMatrix;', @_LapeSingleMatrix_NormMinMax);
    addGlobalFunc('function TSingleMatrix.Indices(Value: Single; Comparator: EComparator): TPointArray;', @_LapeSingleMatrix_Indices);
    addGlobalFunc('function TSingleMatrix.ArgMulti(Count: Int32; HiLo: Boolean): TPointArray;', @_LapeSingleMatrix_ArgMulti);

    // integer
    addGlobalFunc('function TIntegerMatrix.Width: Integer;', @_LapeIntegerMatrix_Width);
    addGlobalFunc('function TIntegerMatrix.Height: Integer;', @_LapeIntegerMatrix_Height);
    addGlobalFunc('procedure TIntegerMatrix.SetSize(Width, Height: Integer);', @_LapeIntegerMatrix_SetSize);
    addGlobalFunc('function TIntegerMatrix.Area: Integer;', @_LapeIntegerMatrix_Area);
    addGlobalFunc('function TIntegerMatrix.Size(out Width, Height: Integer): Boolean;', @_LapeIntegerMatrix_Size);
    addGlobalFunc('function TIntegerMatrix.GetValues(Indices: TPointArray): TIntegerArray;', @_LapeIntegerMatrix_GetValues);
    addGlobalFunc('procedure TIntegerMatrix.SetValues(Indices: TPointArray; Values: TIntegerArray); overload', @_LapeIntegerMatrix_SetValuesArray);
    addGlobalFunc('procedure TIntegerMatrix.SetValues(Indices: TPointArray; Value: Integer); overload', @_LapeIntegerMatrix_SetValues);
    addGlobalFunc('procedure TIntegerMatrix.Fill(Area: TBox; Value: Integer); overload', @_LapeIntegerMatrix_FillArea);
    addGlobalFunc('procedure TIntegerMatrix.Fill(Value: Integer); overload', @_LapeIntegerMatrix_Fill);
    addGlobalFunc('function TIntegerMatrix.Flatten: TIntegerArray; overload', @_LapeIntegerMatrix_Flatten);
    addGlobalFunc('function TIntegerMatrix.Indices(Value: Integer; Comparator: EComparator): TPointArray;', @_LapeIntegerMatrix_Indices);

    // boolean
    addGlobalFunc('function TBooleanMatrix.Width: Integer;', @_LapeBooleanMatrix_Width);
    addGlobalFunc('function TBooleanMatrix.Height: Integer;', @_LapeBooleanMatrix_Height);
    addGlobalFunc('procedure TBooleanMatrix.SetSize(Width, Height: Integer);', @_LapeBooleanMatrix_SetSize);
    addGlobalFunc('function TBooleanMatrix.Area: Integer;', @_LapeBooleanMatrix_Area);
    addGlobalFunc('function TBooleanMatrix.Size(out Width, Height: Integer): Boolean;', @_LapeBooleanMatrix_Size);

    // byte
    addGlobalFunc('function TByteMatrix.Width: Integer;', @_LapeByteMatrix_Width);
    addGlobalFunc('function TByteMatrix.Height: Integer;', @_LapeByteMatrix_Height);
    addGlobalFunc('procedure TByteMatrix.SetSize(Width, Height: Integer);', @_LapeByteMatrix_SetSize);
    addGlobalFunc('function TByteMatrix.Area: Integer;', @_LapeByteMatrix_Area);
    addGlobalFunc('function TByteMatrix.Size(out Width, Height: Integer): Boolean;', @_LapeByteMatrix_Size);

    // double
    addGlobalFunc('function TDoubleMatrix.Width: Integer;', @_LapeDoubleMatrix_Width);
    addGlobalFunc('function TDoubleMatrix.Height: Integer;', @_LapeDoubleMatrix_Height);
    addGlobalFunc('procedure TDoubleMatrix.SetSize(Width, Height: Integer);', @_LapeDoubleMatrix_SetSize);
    addGlobalFunc('function TDoubleMatrix.Area: Integer;', @_LapeDoubleMatrix_Area);
    addGlobalFunc('function TDoubleMatrix.Size(out Width, Height: Integer): Boolean;', @_LapeDoubleMatrix_Size);

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportMatrix);

end.
