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

