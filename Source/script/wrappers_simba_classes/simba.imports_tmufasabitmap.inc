pushSection('TMufasaBitmap');

addClass('TMufasaBitmap');

addGlobalType('array of TMufasaBitmap', 'TMufasaBitmapArray');
addGlobalType('packed record B, G, R, A: Byte; end', 'TRGB32');
addGlobalType('^TRGB32', 'PRGB32');
addGlobalType('(MirrorWidth, MirrorHeight, MirrorLine)', 'TBmpMirrorStyle');
addGlobalType('(TM_Mean, TM_MinMax)', 'TBmpThreshMethod');

addClassVar('TMufasaBitmap', 'Data', 'PRGB32', @_LapeMufasaBitmap_Data_Read);
addClassVar('TMufasaBitmap', 'Name', 'String', @_LapeMufasaBitmap_Name_Read, @_LapeMufasaBitmap_Name_Write);
addClassVar('TMufasaBitmap', 'Width', 'Integer', @_LapeMufasaBitmap_Width_Read);
addClassVar('TMufasaBitmap', 'Height', 'Integer', @_LapeMufasaBitmap_Height_Read);
addClassVar('TMufasaBitmap', 'Center', 'TPoint', @_LapeMufasaBitmap_Center_Read);
addClassVar('TMufasaBitmap', 'TransparentColor', 'Integer', @_LapeMufasaBitmap_GetTransparentColor, @_LapeMufasaBitmap_SetTransparentColor);
addClassVar('TMufasaBitmap', 'TransparentColorActive', 'Boolean', @_LapeMufasaBitmap_GetTransparentColorActive, @_LapeMufasaBitmap_SetTransparentColorActive);
addClassVar('TMufasaBitmap', 'Client', 'TObject', @_LapeMufasaBitmap_Client_Read, @_LapeMufasaBitmap_Client_Write);

addClassVar('TMufasaBitmap', 'Fonts', 'TStringArray', @_LapeMufasaBitmap_Fonts_Read);
addClassVar('TMufasaBitmap', 'FontName', 'String', @_LapeMufasaBitmap_FontName_Read, @_LapeMufasaBitmap_FontName_Write);
addClassVar('TMufasaBitmap', 'FontSize', 'Single', @_LapeMufasaBitmap_FontSize_Read, @_LapeMufasaBitmap_FontSize_Write);
addClassVar('TMufasaBitmap', 'FontAntialiasing', 'Boolean', @_LapeMufasaBitmap_FontAntialiasing_Read, @_LapeMufasaBitmap_FontAntialiasing_Write);

addGlobalFunc('function TMufasaBitmap.Create: TMufasaBitmap; static; overload', @_LapeMufasaBitmap_Create);
addGlobalFunc('function TMufasaBitmap.Create(Width, Height: Integer): TMufasaBitmap; static; overload', @_LapeMufasaBitmap_CreateEx);

//addGlobalFunc('function TMufasaBitmap.CreateFromClient(Client: TObject): TMufasaBitmap; static; overload', @_LapeMufasaBitmap_CreateFromClient);
//addGlobalFunc('function TMufasaBitmap.CreateFromClient(Client: TObject; Area: TBox): TMufasaBitmap; static; overload', @_LapeMufasaBitmap_CreateFromClientEx);

addGlobalFunc('function TMufasaBitmap.CreateFromFile(FileName: String): TMufasaBitmap; static; overload', @_LapeMufasaBitmap_CreateFromFile);
addGlobalFunc('function TMufasaBitmap.CreateFromString(Width, Height: Integer; Str: String): TMufasaBitmap; static; overload', @_LapeMufasaBitmap_CreateFromString);

addGlobalFunc('function TMufasaBitmap.Equals(Other: TMufasaBitmap): Boolean;', @_LapeMufasaBitmap_Equals);

addGlobalFunc('procedure TMufasaBitmap.SetPixel(X, Y: Integer; Color: Integer);', @_LapeMufasaBitmap_SetPixel);
addGlobalFunc('function TMufasaBitmap.GetPixel(X, Y: Integer): Integer;', @_LapeMufasaBitmap_GetPixel);

addGlobalFunc('procedure TMufasaBitmap.SetPixels(Points: TPointArray; Colors: TIntegerArray);', @_LapeMufasaBitmap_SetPixels);
addGlobalFunc('function TMufasaBitmap.GetPixels(Points: TPointArray): TIntegerArray;', @_LapeMufasaBitmap_GetPixels);

addGlobalFunc('function TMufasaBitmap.PixelDifference(Other: TMufasaBitmap): Integer; overload', @_LapeMufasaBitmap_PixelDifference);
addGlobalFunc('function TMufasaBitmap.PixelDifference(Other: TMufasaBitmap; Tolerance: Integer): Integer; overload', @_LapeMufasaBitmap_PixelDifferenceTolerance);

addGlobalFunc('function TMufasaBitmap.PixelDifferenceTPA(Other: TMufasaBitmap): TPointArray; overload', @_LapeMufasaBitmap_PixelDifferenceTPA);
addGlobalFunc('function TMufasaBitmap.PixelDifferenceTPA(Other: TMufasaBitmap; Tolerance: Integer): TPointArray; overload', @_LapeMufasaBitmap_PixelDifferenceToleranceTPA);

addGlobalFunc('function TMufasaBitmap.PixelEdgesTPA(MinDiff: Integer): TPointArray;', @_LapeMufasaBitmap_PixelEdgesTPA);

addGlobalFunc('function TMufasaBitmap.TextWidth(Text: String): Integer;', @_LapeMufasaBitmap_TextWidth);
addGlobalFunc('function TMufasaBitmap.TextHeight(Text: String): Integer;', @_LapeMufasaBitmap_TextHeight);
addGlobalFunc('function TMufasaBitmap.TextSize(Text: String): TPoint;', @_LapeMufasaBitmap_TextSize);
addGlobalFunc('procedure TMufasaBitmap.DrawText(Text: String; Position: TPoint; Color: Integer); overload', @_LapeMufasaBitmap_DrawText);
addGlobalFunc('procedure TMufasaBitmap.DrawText(Text: String; Box: TBox; Center: Boolean; Color: Integer); overload', @_LapeMufasaBitmap_DrawTextEx);
addGlobalFunc('procedure TMufasaBitmap.DrawTextLines(Text: TStringArray; Position: TPoint; Color: Integer);', @_LapeMufasaBitmap_DrawTextLines);

addGlobalFunc('procedure TMufasaBitmap.DrawATPA(ATPA: T2DPointArray); overload', @_LapeMufasaBitmap_DrawATPA);
addGlobalFunc('procedure TMufasaBitmap.DrawATPA(ATPA: T2DPointArray; Color: Integer); overload', @_LapeMufasaBitmap_DrawATPAEx);
addGlobalFunc('procedure TMufasaBitmap.DrawTPA(TPA: TPointArray; Color: Integer);', @_LapeMufasaBitmap_DrawTPA);

addGlobalFunc('procedure TMufasaBitmap.DrawCrosshairs(ACenter: TPoint; Size: Integer; Thickness: Integer; Color: Integer);', @_LapeMufasaBitmap_DrawCrosshairs);
addGlobalFunc('procedure TMufasaBitmap.DrawCross(ACenter: TPoint; Radius: Integer; Thickness: Integer; Color: Integer);', @_LapeMufasaBitmap_DrawCross);

addGlobalFunc('procedure TMufasaBitmap.DrawLine(Start, Stop: TPoint; Color: Integer); overload', @_LapeMufasaBitmap_DrawLine);
addGlobalFunc('procedure TMufasaBitmap.DrawLine(Start, Stop: TPoint; Thickness: Integer; Color: Integer); overload', @_LapeMufasaBitmap_DrawLineEx);

addGlobalFunc('procedure TMufasaBitmap.DrawPolygon(Points: TPointArray; Color: Integer);', @_LapeMufasaBitmap_DrawPolygon);
addGlobalFunc('procedure TMufasaBitmap.DrawPolygonFilled(Points: TPointArray; Color: Integer);', @_LapeMufasaBitmap_DrawPolygonFilled);
addGlobalFunc('procedure TMufasaBitmap.DrawPolygonInverted(Points: TPointArray; Color: Integer);', @_LapeMufasaBitmap_DrawPolygonInverted);

addGlobalFunc('procedure TMufasaBitmap.DrawCircle(ACenter: TPoint; Radius: Integer; Color: Integer);', @_LapeMufasaBitmap_DrawCircle);
addGlobalFunc('procedure TMufasaBitmap.DrawCircleFilled(ACenter: TPoint; Radius: Integer; Color: Integer);', @_LapeMufasaBitmap_DrawCircleFilled);
addGlobalFunc('procedure TMufasaBitmap.DrawCircleInverted(ACenter: TPoint; Radius: Integer; Color: Integer);', @_LapeMufasaBitmap_DrawCircleInverted);

addGlobalFunc('procedure TMufasaBitmap.DrawBox(B: TBox; Color: Integer);', @_LapeMufasaBitmap_DrawBox);
addGlobalFunc('procedure TMufasaBitmap.DrawBoxFilled(B: TBox; Color: Integer);', @_LapeMufasaBitmap_DrawBoxFilled);
addGlobalFunc('procedure TMufasaBitmap.DrawBoxInverted(B: TBox; Color: Integer);', @_LapeMufasaBitmap_DrawBoxInverted);
addGlobalFunc('procedure TMufasaBitmap.DrawBoxArray(Boxes: TBoxArray; Filled: Boolean; Color: Integer = -1);', @_LapeMufasaBitmap_DrawBoxArray);
addGlobalFunc('procedure TMufasaBitmap.DrawPolygonArray(Polygons: T2DPointArray; Filled: Boolean; Color: Integer = -1);', @_LapeMufasaBitmap_DrawPolygonArray);
addGlobalFunc('procedure TMufasaBitmap.DrawCircleArray(Points: TPointArray; Radius: Integer; Filled: Boolean; Color: Integer = -1);', @_LapeMufasaBitmap_DrawCircleArray);
addGlobalFunc('procedure TMufasaBitmap.DrawCrossArray(Points: TPointArray; Radius: Integer; Thickness: Integer; Color: Integer = -1);', @_LapeMufasaBitmap_DrawCrossArray);

addGlobalFunc('function TMufasaBitmap.FindColors(out Points: TPointArray; Color: Integer): Boolean;', @_LapeMufasaBitmap_FindColors);
addGlobalFunc('function TMufasaBitmap.FindColorsTolerance(out Points: TPointArray; Color, Tolerance: Integer): Boolean; overload', @_LapeMufasaBitmap_FindColorsToleranceCTS1);
addGlobalFunc('function TMufasaBitmap.FindColorsTolerance(out Points: TPointArray; Color, Tolerance: Integer; HueMod, SatMod: Extended): Boolean; overload', @_LapeMufasaBitmap_FindColorsToleranceCTS2);

addGlobalFunc('function TMufasaBitmap.FindBitmap(Bitmap: TMufasaBitmap; out X, Y: Integer; Tolerance: Integer): Boolean;', @_LapeMufasaBitmap_FindBitmap);
addGlobalFunc('function TMufasaBitmap.FindBitmaps(Bitmap: TMufasaBitmap; out Points: TPointArray; Tolerance: Integer): Boolean;', @_LapeMufasaBitmap_FindBitmaps);

addGlobalFunc('procedure TMufasaBitmap.Clear; overload', @_LapeMufasaBitmap_Clear);
addGlobalFunc('procedure TMufasaBitmap.Clear(Area: TBox); overload', @_LapeMufasaBitmap_ClearEx);
addGlobalFunc('procedure TMufasaBitmap.ClearInverted(Area: TBox);', @_LapeMufasaBitmap_ClearInverted);

addGlobalFunc('procedure TMufasaBitmap.DrawBitmap(Bitmap: TMufasaBitmap; Position: TPoint);', @_LapeMufasaBitmap_DrawBitmap);
addGlobalFunc('procedure TMufasaBitmap.DrawClient(Area: TBox; Position: TPoint); overload', @_LapeMufasaBitmap_DrawClient);
addGlobalFunc('procedure TMufasaBitmap.DrawClient(Position: TPoint); overload', @_LapeMufasaBitmap_DrawClientEx);

addGlobalFunc('procedure TMufasaBitmap.DrawMatrix(Matrix: TIntegerMatrix); overload', @_LapeMufasaBitmap_DrawMatrix);
addGlobalFunc('procedure TMufasaBitmap.DrawMatrix(Matrix: TSingleMatrix; ColorMapID: Integer = 0); overload', @_LapeMufasaBitmap_DrawMatrixF);
addGlobalFunc('procedure TMufasaBitmap.DrawMatrix(Matrix: TByteMatrix); overload', @_LapeMufasaBitmap_DrawMatrixB);

addGlobalFunc('function TMufasaBitmap.AverageBrightness: Integer;', @_LapeMufasaBitmap_AverageBrightness);
addGlobalFunc('function TMufasaBitmap.PeakBrightness: Integer;', @_LapeMufasaBitmap_PeakBrightness);

addGlobalFunc('procedure TMufasaBitmap.SetSize(AWidth, AHeight: Integer);', @_LapeMufasaBitmap_SetSize);
addGlobalFunc('procedure TMufasaBitmap.Resize(AWidth, AHeight: Integer);', @_LapeMufasaBitmap_Resize);
addGlobalFunc('procedure TMufasaBitmap.ResizeBilinear(AWidth, AHeight: Integer);', @_LapeMufasaBitmap_ResizeBilinear);
addGlobalFunc('procedure TMufasaBitmap.SetPersistentMemory(Memory: PtrUInt; AWidth, AHeight: Integer);', @_LapeMufasaBitmap_SetPersistentMemory);
addGlobalFunc('procedure TMufasaBitmap.ResetPersistentMemory;', @_LapeMufasaBitmap_ResetPersistentMemory);

addGlobalFunc('procedure TMufasaBitmap.Fill(Color: Integer);', @_LapeMufasaBitmap_Fill);
addGlobalFunc('procedure TMufasaBitmap.ReplaceColor(OldColor, NewColor: Integer);', @_LapeMufasaBitmap_ReplaceColor);
addGlobalFunc('procedure TMufasaBitmap.ReplaceColors(OldColors, NewColors: TIntegerArray);', @_LapeMufasaBitmap_ReplaceColors);
addGlobalFunc('procedure TMufasaBitmap.Rotate(Radians: Single; Expand: Boolean; TargetBitmap: TMufasaBitmap);', @_LapeMufasaBitmap_Rotate);
addGlobalFunc('procedure TMufasaBitmap.RotateBilinear(Radians: Single; Expand: Boolean; TargetBitmap: TMufasaBitmap);', @_LapeMufasaBitmap_RotateBilinear);
addGlobalFunc('procedure TMufasaBitmap.Desaturate(TargetBitmap: TMufasaBitmap); overload', @_LapeMufasaBitmap_Desaturate);
addGlobalFunc('procedure TMufasaBitmap.Desaturate; overload', @_LapeMufasaBitmap_DesaturateEx);
addGlobalFunc('procedure TMufasaBitmap.GreyScale(TargetBitmap: TMufasaBitmap); overload', @_LapeMufasaBitmap_GreyScale);
addGlobalFunc('procedure TMufasaBitmap.GreyScale; overload', @_LapeMufasaBitmap_GreyScaleEx);
addGlobalFunc('procedure TMufasaBitmap.Brightness(TargetBitmap: TMufasaBitmap; br: Integer); overload', @_LapeMufasaBitmap_Brightness);
addGlobalFunc('procedure TMufasaBitmap.Brightness(br: Integer); overload', @_LapeMufasaBitmap_BrightnessEx);
addGlobalFunc('procedure TMufasaBitmap.Contrast(TargetBitmap: TMufasaBitmap; co: Extended); overload', @_LapeMufasaBitmap_Contrast);
addGlobalFunc('procedure TMufasaBitmap.Contrast(co: Extended); overload', @_LapeMufasaBitmap_ContrastEx);
addGlobalFunc('procedure TMufasaBitmap.Invert(TargetBitmap: TMufasaBitmap);', @_LapeMufasaBitmap_Invert);
addGlobalFunc('procedure TMufasaBitmap.Invert; overload', @_LapeMufasaBitmap_InvertEx);
addGlobalFunc('procedure TMufasaBitmap.Posterize(TargetBitmap: TMufasaBitmap; Po: Integer); overload', @_LapeMufasaBitmap_Posterize);
addGlobalFunc('procedure TMufasaBitmap.Posterize(Po: Integer); overload', @_LapeMufasaBitmap_PosterizeEx);
addGlobalFunc('procedure TMufasaBitmap.Blur(Block: Integer); overload', @_LapeMufasaBitmap_Blur);
addGlobalFunc('procedure TMufasaBitmap.Blur(TargetBitmap: TMufasaBitmap; Block: Integer); overload', @_LapeMufasaBitmap_BlurEx);
addGlobalFunc('procedure TMufasaBitmap.Mirror(MirrorStyle: TBmpMirrorStyle); overload', @_LapeMufasaBitmap_Mirror);
addGlobalFunc('procedure TMufasaBitmap.Mirror(TargetBitmap: TMufasaBitmap; MirrorStyle: TBmpMirrorStyle); overload', @_LapeMufasaBitmap_MirrorEx);
addGlobalFunc('procedure TMufasaBitmap.Downsample(Scale: Integer); overload', @_LapeMufasaBitmap_DownSample);
addGlobalFunc('procedure TMufasaBitmap.Downsample(TargetBitmap: TMufasaBitmap; Scale: Integer); overload', @_LapeMufasaBitmap_DownSampleEx);
addGlobalFunc('procedure TMufasaBitmap.Blend(Points: TPointArray; Size: Integer); overload', @_LapeMufasaBitmap_Blend);
addGlobalFunc('procedure TMufasaBitmap.Blend(TargetBitmap: TMufasaBitmap; Points: TPointArray; Size: Integer); overload', @_LapeMufasaBitmap_BlendEx);
addGlobalFunc('procedure TMufasaBitmap.Convolute(TargetBitmap: TMufasaBitmap; Matrix: T2DExtendedArray);', @_LapeMufasaBitmap_Convolute);
addGlobalFunc('function TMufasaBitmap.Copy(X1, Y1, X2, Y2: Integer): TMufasaBitmap; overload', @_LapeMufasaBitmap_Copy);
addGlobalFunc('function TMufasaBitmap.Copy: TMufasaBitmap; overload', @_LapeMufasaBitmap_CopyEx);
addGlobalFunc('procedure TMufasaBitmap.Crop(X1, Y1, X2, Y2: Integer);', @_LapeMufasaBitmap_Crop);
addGlobalFunc('function TMufasaBitmap.GetColors: TIntegerArray;', @_LapeMufasaBitmap_GetColors);
addGlobalFunc('function TMufasaBitmap.ToMatrix: TIntegerMatrix; overload', @_LapeMufasaBitmap_ToMatrix);
addGlobalFunc('function TMufasaBitmap.ToMatrix(X1, Y1, X2, Y2: Integer): TIntegerMatrix; overload', @_LapeMufasaBitmap_ToMatrixEx);
addGlobalFunc('function TMufasaBitmap.ToGreyMatrix: TByteMatrix;', @_LapeMufasaBitmap_ToGreyMatrix);
addGlobalFunc('procedure TMufasaBitmap.ThresholdAdaptive(Alpha, Beta: Byte; AInvert: Boolean; Method: TBmpThreshMethod; k: Integer);', @_LapeMufasaBitmap_ThresholdAdaptive);
addGlobalFunc('procedure TMufasaBitmap.ThresholdSauvola(Radius: Integer; AInvert: Boolean; k: Single);', @_LapeMufasaBitmap_ThresholdSauvola);
addGlobalFunc('procedure TMufasaBitmap.Pad(Amount: Integer)', @_LapeMufasaBitmap_Pad);

addGlobalFunc('procedure TMufasaBitmap.LoadFromClient; overload', @_LapeMufasaBitmap_LoadFromClient);
addGlobalFunc('procedure TMufasaBitmap.LoadFromClient(Area: TBox); overload', @_LapeMufasaBitmap_LoadFromClientEx);
addGlobalFunc('procedure TMufasaBitmap.LoadFromFile(FileName: String); overload', @_LapeMufasaBitmap_LoadFromFile);
addGlobalFunc('procedure TMufasaBitmap.LoadFromFile(FileName: String; Area: TBox); overload', @_LapeMufasaBitmap_LoadFromFileEx);
addGlobalFunc('procedure TMufasaBitmap.LoadFromString(AWidth, AHeight: Integer; Str: String)', @_LapeMufasaBitmap_LoadFromString);
addGlobalFunc('procedure TMufasaBitmap.LoadFromMemory(AWidth, AHeight: Integer; Memory: PRGB32)', @_LapeMufasaBitmap_LoadFromMemory);
addGlobalFunc('procedure TMufasaBitmap.LoadFromBitmap(Bitmap: TMufasaBitmap);', @_LapeMufasaBitmap_LoadFromBitmap);

addGlobalFunc('function TMufasaBitmap.SaveToFile(FileName: String): Boolean;', @_LapeMufasaBitmap_SaveToFile);
addGlobalFunc('function TMufasaBitmap.SaveToString: String;', @_LapeMufasaBitmap_SaveToString);

addGlobalFunc('function TMufasaBitmap.ToTBitmap: TBitmap;', @_LapeMufasaBitmap_ToTBitmap);
addGlobalFunc('procedure TMufasaBitmap.DrawToCanvas(x, y: Integer; Canvas: TCanvas);', @_LapeMufasaBitmap_DrawToCanvas);
addGlobalFunc('procedure TMufasaBitmap.LoadFromTBitmap(bmp: TBitmap);', @_LapeMufasaBitmap_LoadFromTBitmap);

//addGlobalFunc('procedure TMufasaBitmap.Free;', @_LapeMufasaBitmap_Free);

addDelayedCode('{$HINTS OFF} procedure ShowBitmap(Bitmap: TMufasaBitmap; EnsureVisible: Boolean = True); begin end;', '!ShowBitmap');

addDelayedCode([
  'procedure TMufasaBitmap.Init; deprecated ' + #39 + 'Use TMufasaBitmap.Create' + #39 + ';',
  'begin',
  '  Self := TMufasaBitmap.Create();',
  '  Self.SetClient(Client);',
  'end;',
  '',
  'function TMufasaBitmap.Create: TMufasaBitmap; static; override;',
  'begin',
  '  Result := inherited();',
  '  Result.SetClient(Client);',
  'end;',
  '',
  'function TMufasaBitmap.Create(Width, Height: Integer): TMufasaBitmap; static; override;',
  'begin',
  '  Result := inherited();',
  '  Result.SetClient(Client);',
  'end;',
  '',
  'function TMufasaBitmap.CreateFromClient: TMufasaBitmap; static; overload;',
  'begin',
  '  Result := TMufasaBitmap.Create();',
  '  Result.LoadFromClient();',
  'end;',
  '',
  'function TMufasaBitmap.CreateFromClient(Area: TBox): TMufasaBitmap; static; overload;',
  'begin',
  '  Result := TMufasaBitmap.Create();',
  '  Result.LoadFromClient(Area);',
  'end;',
  '',
  'procedure TMufasaBitmap.Show(EnsureVisible: Boolean = True);',
  'begin',
  '  ShowBitmap(Self, EnsureVisible);',
  'end;'
]);

popSection();

