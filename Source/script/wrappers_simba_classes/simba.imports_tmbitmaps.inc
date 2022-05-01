pushSection('Classes');

addClass('TMBitmaps');
addGlobalFunc('function TMBitmaps.GetBMP(Index : integer): TMufasaBitmap; constref', @_LapeMBitmaps_GetBMP);
addGlobalFunc('function TMBitmaps.CreateBMP(w, h: integer): Integer; constref', @_LapeMBitmaps_CreateBMP);
addGlobalFunc('function TMBitmaps.ExistsBMP(Index : integer): boolean; constref', @_LapeMBitmaps_ExistsBMP);
addGlobalFunc('function TMBitmaps.AddBMP(_bmp: TMufasaBitmap): Integer; constref', @_LapeMBitmaps_AddBMP);
addGlobalFunc('function TMBitmaps.CopyBMP(Bitmap : integer): Integer; constref', @_LapeMBitmaps_CopyBMP);
addGlobalFunc('function TMBitmaps.CreateMirroredBitmap(bitmap: Integer; MirrorStyle : TBmpMirrorStyle): Integer; constref', @_LapeMBitmaps_CreateMirroredBitmap);
addGlobalFunc('function TMBitmaps.CreateBMPFromFile(const Path : string): integer; constref', @_LapeMBitmaps_CreateBMPFromFile);
addGlobalFunc('function TMBitmaps.CreateBMPFromString(width,height : integer; Data : string): integer; constref', @_LapeMBitmaps_CreateBMPFromString);
addGlobalFunc('procedure TMBitmaps.RemoveBMP(Number: integer); constref', @_LapeMBitmaps_RemoveBMP);
addGlobalFunc('procedure TMBitmaps.Init(Owner : TObject)', @_LapeMBitmaps_Init);
//addGlobalFunc('procedure TMBitmaps.Free; constref', @_LapeMBitmaps_Free);

popSection();
