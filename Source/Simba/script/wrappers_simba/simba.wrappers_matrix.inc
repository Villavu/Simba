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

procedure _LapeByteMatrix_GetValues(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PByteArray(Result)^ := PByteMatrix(Params^[0])^.GetValues(PPointArray(Params^[1])^);
end;

procedure _LapeByteMatrix_SetValuesArray(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PByteMatrix(Params^[0])^.SetValues(PPointArray(Params^[1])^, PByteArray(Params^[2])^);
end;

procedure _LapeByteMatrix_SetValues(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PByteMatrix(Params^[0])^.SetValues(PPointArray(Params^[1])^, PByte(Params^[2])^);
end;

procedure _LapeByteMatrix_FillArea(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PByteMatrix(Params^[0])^.Fill(PBox(Params^[1])^, PByte(Params^[2])^);
end;

procedure _LapeByteMatrix_Fill(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PByteMatrix(Params^[0])^.Fill(PByte(Params^[1])^);
end;

procedure _LapeByteMatrix_Flatten(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PByteArray(Result)^ := PByteMatrix(Params^[0])^.Flatten;
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

procedure _LapeBooleanMatrix_GetValues(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBooleanArray(Result)^ := PBooleanMatrix(Params^[0])^.GetValues(PPointArray(Params^[1])^);
end;

procedure _LapeBooleanMatrix_SetValuesArray(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBooleanMatrix(Params^[0])^.SetValues(PPointArray(Params^[1])^, PBooleanArray(Params^[2])^);
end;

procedure _LapeBooleanMatrix_SetValues(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBooleanMatrix(Params^[0])^.SetValues(PPointArray(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeBooleanMatrix_FillArea(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBooleanMatrix(Params^[0])^.Fill(PBox(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeBooleanMatrix_Fill(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBooleanMatrix(Params^[0])^.Fill(PBoolean(Params^[1])^);
end;

procedure _LapeBooleanMatrix_Flatten(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBooleanArray(Result)^ := PBooleanMatrix(Params^[0])^.Flatten;
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

// Single (Extra)
procedure _LapeSingleMatrix_ToIntegerMatrix(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerMatrix(Result)^ := PSingleMatrix(Params^[0])^.ToIntegerMatrix;
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

procedure _LapeDoubleMatrix_GetValues(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDoubleArray(Result)^ := PDoubleMatrix(Params^[0])^.GetValues(PPointArray(Params^[1])^);
end;

procedure _LapeDoubleMatrix_SetValuesArray(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDoubleMatrix(Params^[0])^.SetValues(PPointArray(Params^[1])^, PDoubleArray(Params^[2])^);
end;

procedure _LapeDoubleMatrix_SetValues(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDoubleMatrix(Params^[0])^.SetValues(PPointArray(Params^[1])^, PDouble(Params^[2])^);
end;

procedure _LapeDoubleMatrix_FillArea(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDoubleMatrix(Params^[0])^.Fill(PBox(Params^[1])^, PDouble(Params^[2])^);
end;

procedure _LapeDoubleMatrix_Fill(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDoubleMatrix(Params^[0])^.Fill(PDouble(Params^[1])^);
end;

procedure _LapeDoubleMatrix_Flatten(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDoubleArray(Result)^ := PDoubleMatrix(Params^[0])^.Flatten;
end;

