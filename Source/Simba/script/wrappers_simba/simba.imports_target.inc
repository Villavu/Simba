FSection := 'Target';

addGlobalFunc('procedure SetDesktopAsClient', @_LapeSetDesktopAsClient);
addGlobalFunc('function SetTargetArray(P: PtrUInt; W, H: Int32): Int32', @_LapeSetTargetArray);
addGlobalFunc('function SetTargetBitmap(bitmap: Int32): Int32', @_LapeSetTargetBitmap);
addGlobalFunc('function SetEIOSTarget(name, args: String): Int32', @_LapeSetEIOSTarget);
addGlobalFunc('function MouseSetClientArea(x1, y1, x2, y2: Int32): Boolean', @_LapeMouseSetClientArea);
addGlobalFunc('procedure MouseResetClientArea', @_LapeMouseResetClientArea);
addGlobalFunc('function ImageSetClientArea(x1, y1, x2, y2: Int32): Boolean', @_LapeImageSetClientArea);
addGlobalFunc('procedure ImageResetClientArea', @_LapeImageResetClientArea);
addGlobalFunc('procedure SetImageTarget(idx: Int32)', @_LapeSetImageTarget);
addGlobalFunc('procedure SetKeyMouseTarget(idx: Int32)', @_LapeSetKeyMouseTarget);
addGlobalFunc('function GetImageTarget: Int32', @_LapeGetImageTarget);
addGlobalFunc('function GetKeyMouseTarget: Int32', @_LapeGetKeyMouseTarget);
addGlobalFunc('function ExportImageTarget: TTarget_Exported', @_LapeExportImageTarget);
addGlobalFunc('function ExportKeyMouseTarget: TTarget_Exported', @_LapeExportKeyMouseTarget);
addGlobalFunc('procedure FreeTarget(idx: Int32)', @_LapeFreeTarget);
addGlobalFunc('procedure GetClientDimensions(var W, H: Int32)', @_LapeGetClientDimensions);
addGlobalFunc('procedure GetClientPosition(var Left, Top: Int32)', @_LapeGetClientPosition);
addGlobalFunc('procedure Freeze', @_LapeFreeze);
addGlobalFunc('procedure Unfreeze', @_LapeUnfreeze);
addGlobalFunc('procedure ActivateClient', @_LapeActivateClient);
addGlobalFunc('function IsTargetValid: Boolean', @_LapeIsTargetValid);
addGlobalFunc('function GetNativeWindow: PtrUInt', @_LapeGetNativeWindow);
addGlobalFunc('procedure SaveScreenshot(FileName: string)', @_LapeSaveScreenshot);
addGlobalFunc('function GetTargetWindow: TOSWindow', @_LapeGetTargetWindow);
addGlobalFunc('function GetTargetPID: UInt32', @_LapeGetTargetPID);

