pushSection('Classes');

addClass('TMBitmaps');
addGlobalFunc('function TMBitmaps.GetBMP(Index : integer): TMufasaBitmap;', @_LapeMBitmaps_GetBMP);
addGlobalFunc('function TMBitmaps.CreateBMP(w, h: integer): Integer;', @_LapeMBitmaps_CreateBMP);
addGlobalFunc('function TMBitmaps.ExistsBMP(Index : integer): boolean;', @_LapeMBitmaps_ExistsBMP);
addGlobalFunc('function TMBitmaps.AddBMP(_bmp: TMufasaBitmap): Integer;', @_LapeMBitmaps_AddBMP);
addGlobalFunc('function TMBitmaps.CopyBMP(Bitmap : integer): Integer;', @_LapeMBitmaps_CopyBMP);
addGlobalFunc('function TMBitmaps.CreateMirroredBitmap(bitmap: Integer; MirrorStyle : TBmpMirrorStyle): Integer;', @_LapeMBitmaps_CreateMirroredBitmap);
addGlobalFunc('function TMBitmaps.CreateBMPFromFile(const Path : string): integer;', @_LapeMBitmaps_CreateBMPFromFile);
addGlobalFunc('function TMBitmaps.CreateBMPFromString(width,height : integer; Data : string): integer;', @_LapeMBitmaps_CreateBMPFromString);
addGlobalFunc('procedure TMBitmaps.RemoveBMP(Number: integer);', @_LapeMBitmaps_RemoveBMP);
addGlobalFunc('procedure TMBitmaps.Init(Owner : TObject)', @_LapeMBitmaps_Init);
//addGlobalFunc('procedure TMBitmaps.Free;', @_LapeMBitmaps_Free);

popSection();
