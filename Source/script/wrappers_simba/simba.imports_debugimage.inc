pushSection('Debug Image');

addGlobalFunc('procedure ShowBitmap(Bitmap: TMufasaBitmap; EnsureVisible: Boolean = True); overload', @_LapeShowBitmap);
addGlobalFunc('procedure ShowBitmap(Bitmap: Integer; EnsureVisible: Boolean = True); overload', @_LapeShowBitmapEx);
addGlobalFunc('procedure DrawBitmapDebugImg(Bitmap: TMufasaBitmap); overload', @_LapeDrawBitmapDebugImg);
addGlobalFunc('procedure DrawBitmapDebugImg(Bitmap: Integer); overload', @_LapeDrawBitmapDebugImgEx);
addGlobalFunc('procedure DisplayDebugImgWindow(Width, Height: Integer)', @_LapeDisplayDebugImgWindow);
addGlobalFunc('procedure SetDebugImgMaxSize(MaxWidth, MaxHeight: Integer)', @_LapeSetDebugImgMaxSize);
addGlobalFunc('procedure HideDebugImg', @_LapeHideDebugImg);
addGlobalFunc('procedure ClearDebugImg', @_LapeClearDebugImg);

popSection();

