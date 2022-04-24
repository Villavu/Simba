pushSection('Other');

addGlobalFunc('procedure PlaySound(Sound: String)', @_LapePlaySound);
addGlobalFunc('procedure StopSound', @_LapeStopSound);
addGlobalFunc('procedure SetSupressExceptions(Supress: Boolean)', @_LapeSetSupressExceptions);
addGlobalFunc('procedure Simba', @_LapeSimba);
addGlobalFunc('procedure SetClipBoard(Data: string)', @_LapeSetClipBoard);
addGlobalFunc('function GetClipBoard: String', @_LapeGetClipBoard);

addGlobalType('(bfNone, bfInfo, bfWarning, bfError)', 'TBalloonFlags');
addGlobalFunc('procedure Status(const Status: String)', @_LapeStatus);
addGlobalFunc('procedure ShowBalloonHint(const Title, Hint: String; const Timeout: Integer; const Flags: TBalloonFlags)', @_LapeShowBalloonHint);
addGlobalFunc('procedure ClearDebug', @_LapeClearDebug);
addGlobalFunc('procedure Disguise(const Caption: String)', @_LapeDisguise);
addGlobalFunc('procedure ShowBitmap(Bitmap: TMufasaBitmap); overload', @_LapeShowBitmap);
addGlobalFunc('procedure ShowBitmap(Bitmap: Integer); overload', @_LapeShowBitmapEx);
addGlobalFunc('procedure DrawBitmapDebugImg(Bitmap: TMufasaBitmap); overload', @_LapeDrawBitmapDebugImg);
addGlobalFunc('procedure DrawBitmapDebugImg(Bitmap: Integer); overload', @_LapeDrawBitmapDebugImgEx);
addGlobalFunc('procedure DisplayDebugImgWindow(W, H: Integer)', @_LapeDisplayDebugImgWindow);
addGlobalFunc('procedure SetDebugImgMaxSize(MaxWidth, MaxHeight: Integer)', @_LapeSetDebugImgMaxSize);
addGlobalFunc('procedure HideDebugImg', @_LapeHideDebugImg);
addGlobalFunc('procedure ClearDebugImg', @_LapeClearDebugImg);
addGlobalFunc('function GetSimbaPID: PtrUInt', @_LapeGetSimbaPID);
addGlobalFunc('function GetSimbaTargetPID: PtrUInt', @_LapeGetSimbaTargetPID);
addGlobalFunc('function GetSimbaTargetWindow: TWindowHandle', @_LapeGetSimbaTargetWindow);

popSection();

