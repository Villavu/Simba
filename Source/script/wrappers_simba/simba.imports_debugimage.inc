pushSection('Debug Image');

addGlobalFunc('procedure ShowBitmap(Bitmap: TMufasaBitmap; EnsureVisible: Boolean = True); override', @_LapeShowBitmap);
addGlobalFunc('procedure DrawBitmapDebugImg(Bitmap: TMufasaBitmap); overload', @_LapeDrawBitmapDebugImg);
addGlobalFunc('procedure DisplayDebugImgWindow(Width, Height: Integer)', @_LapeDisplayDebugImgWindow);
addGlobalFunc('procedure SetDebugImgMaxSize(MaxWidth, MaxHeight: Integer)', @_LapeSetDebugImgMaxSize);
addGlobalFunc('procedure HideDebugImg', @_LapeHideDebugImg);
addGlobalFunc('procedure ClearDebugImg', @_LapeClearDebugImg);

popSection();

