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

