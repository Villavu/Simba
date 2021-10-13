FSection := 'Matrix';

addGlobalType('array of TSingleArray', 'TSingleMatrix');
addGlobalType('array of TIntegerArray', 'TIntegerMatrix');
addGlobalType('array of TBooleanArray', 'TBooleanMatrix');
addGlobalType('array of TByteArray', 'TByteMatrix');
addGlobalType('array of TDoubleArray', 'TDoubleMatrix');

// single
addGlobalFunc('function TSingleMatrix.Width: Integer; constref', @_LapeSingleMatrix_Width);
addGlobalFunc('function TSingleMatrix.Height: Integer; constref', @_LapeSingleMatrix_Height);
addGlobalFunc('procedure TSingleMatrix.SetSize(const Width, Height: Integer); constref', @_LapeSingleMatrix_SetSize);
addGlobalFunc('function TSingleMatrix.Area: Integer;', @_LapeSingleMatrix_Area);
addGlobalFunc('function TSingleMatrix.Size(out Width, Height: Integer): Boolean;', @_LapeSingleMatrix_Size);
addGlobalFunc('function TSingleMatrix.GetValues(Indices: TPointArray): TSingleArray; constref', @_LapeSingleMatrix_GetValues);
addGlobalFunc('procedure TSingleMatrix.SetValues(Indices: TPointArray; Values: TSingleArray); constref; overload', @_LapeSingleMatrix_SetValuesArray);
addGlobalFunc('procedure TSingleMatrix.SetValues(Indices: TPointArray; Value: Single); constref; overload', @_LapeSingleMatrix_SetValues);
addGlobalFunc('procedure TSingleMatrix.Fill(Area: TBox; Value: Single); constref; overload', @_LapeSingleMatrix_FillArea);
addGlobalFunc('procedure TSingleMatrix.Fill(Value: Single); constref; overload', @_LapeSingleMatrix_Fill);
addGlobalFunc('function TSingleMatrix.Flatten: TSingleArray; constref; overload', @_LapeSingleMatrix_Flatten);
addGlobalFunc('function TSingleMatrix.ToIntegerMatrix: TIntegerMatrix; constref', @_LapeSingleMatrix_ToIntegerMatrix);
addGlobalFunc('function TSingleMatrix.Mean: Single; constref', @_LapeSingleMatrix_Mean);
addGlobalFunc('procedure TSingleMatrix.MeanStdev(out Mean, Stdev: Double); constref', @_LapeSingleMatrix_MeanStdev);
addGlobalFunc('procedure TSingleMatrix.MinMax(out MinValue, MaxValue: Single); constref', @_LapeSingleMatrix_MinMax);
addGlobalFunc('function TSingleMatrix.Min: Single; constref', @_LapeSingleMatrix_Min);
addGlobalFunc('function TSingleMatrix.Max: Single; constref', @_LapeSingleMatrix_Max);
addGlobalFunc('function TSingleMatrix.ArgMax: TPoint; constref', @_LapeSingleMatrix_ArgMax);
addGlobalFunc('function TSingleMatrix.ArgMin: TPoint; constref', @_LapeSingleMatrix_ArgMin);
addGlobalFunc('function TSingleMatrix.NormMinMax(Alpha, Beta: Single): TSingleMatrix; constref', @_LapeSingleMatrix_NormMinMax);
addGlobalFunc('function TSingleMatrix.Indices(Value: Single; Comparator: EComparator): TPointArray; constref', @_LapeSingleMatrix_Indices);
addGlobalFunc('function TSingleMatrix.ArgMulti(Count: Int32; HiLo: Boolean): TPointArray; constref', @_LapeSingleMatrix_ArgMulti);

// integer
addGlobalFunc('function TIntegerMatrix.Width: Integer; constref', @_LapeIntegerMatrix_Width);
addGlobalFunc('function TIntegerMatrix.Height: Integer; constref', @_LapeIntegerMatrix_Height);
addGlobalFunc('procedure TIntegerMatrix.SetSize(Width, Height: Integer); constref', @_LapeIntegerMatrix_SetSize);
addGlobalFunc('function TIntegerMatrix.Area: Integer;', @_LapeIntegerMatrix_Area);
addGlobalFunc('function TIntegerMatrix.Size(out Width, Height: Integer): Boolean;', @_LapeIntegerMatrix_Size);
addGlobalFunc('function TIntegerMatrix.GetValues(Indices: TPointArray): TIntegerArray; constref', @_LapeIntegerMatrix_GetValues);
addGlobalFunc('procedure TIntegerMatrix.SetValues(Indices: TPointArray; Values: TIntegerArray); constref; overload', @_LapeIntegerMatrix_SetValuesArray);
addGlobalFunc('procedure TIntegerMatrix.SetValues(Indices: TPointArray; Value: Integer); constref; overload', @_LapeIntegerMatrix_SetValues);
addGlobalFunc('procedure TIntegerMatrix.Fill(Area: TBox; Value: Integer); constref; overload', @_LapeIntegerMatrix_FillArea);
addGlobalFunc('procedure TIntegerMatrix.Fill(Value: Integer); constref; overload', @_LapeIntegerMatrix_Fill);
addGlobalFunc('function TIntegerMatrix.Flatten: TIntegerArray; constref; overload', @_LapeIntegerMatrix_Flatten);
addGlobalFunc('function TIntegerMatrix.Indices(Value: Integer; Comparator: EComparator): TPointArray; constref', @_LapeIntegerMatrix_Indices);

// boolean
addGlobalFunc('function TBooleanMatrix.Width: Integer; constref', @_LapeBooleanMatrix_Width);
addGlobalFunc('function TBooleanMatrix.Height: Integer; constref', @_LapeBooleanMatrix_Height);
addGlobalFunc('procedure TBooleanMatrix.SetSize(Width, Height: Integer); constref', @_LapeBooleanMatrix_SetSize);
addGlobalFunc('function TBooleanMatrix.Area: Integer;', @_LapeBooleanMatrix_Area);
addGlobalFunc('function TBooleanMatrix.Size(out Width, Height: Integer): Boolean;', @_LapeBooleanMatrix_Size);

// byte
addGlobalFunc('function TByteMatrix.Width: Integer; constref', @_LapeByteMatrix_Width);
addGlobalFunc('function TByteMatrix.Height: Integer; constref', @_LapeByteMatrix_Height);
addGlobalFunc('procedure TByteMatrix.SetSize(Width, Height: Integer); constref', @_LapeByteMatrix_SetSize);
addGlobalFunc('function TByteMatrix.Area: Integer;', @_LapeByteMatrix_Area);
addGlobalFunc('function TByteMatrix.Size(out Width, Height: Integer): Boolean;', @_LapeByteMatrix_Size);

// double
addGlobalFunc('function TDoubleMatrix.Width: Integer; constref', @_LapeDoubleMatrix_Width);
addGlobalFunc('function TDoubleMatrix.Height: Integer; constref', @_LapeDoubleMatrix_Height);
addGlobalFunc('procedure TDoubleMatrix.SetSize(Width, Height: Integer); constref', @_LapeDoubleMatrix_SetSize);
addGlobalFunc('function TDoubleMatrix.Area: Integer;', @_LapeDoubleMatrix_Area);
addGlobalFunc('function TDoubleMatrix.Size(out Width, Height: Integer): Boolean;', @_LapeDoubleMatrix_Size);

