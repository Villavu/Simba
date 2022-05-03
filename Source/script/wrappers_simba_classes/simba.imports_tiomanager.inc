pushSection('Classes');

addClass('TIOManager');
addGlobalType('record func1, func2, func3, func4, func5, func6, func7, func8, func9, func10, func11, func12, func13, func14, func15, func16: Pointer; end', 'TTarget_Exported');
addGlobalFunc('function TIOManager.SetTarget(WindowHandle: PtrUInt): Integer; overload', @_LapeIOManager_SetTargetHandle);
addGlobalFunc('function TIOManager.SetTarget(ArrPtr: PRGB32; Size: TPoint): Integer; overload', @_LapeIOManager_SetTarget);
addGlobalFunc('function TIOManager.SetTarget(bmp : TMufasaBitmap): Integer; overload', @_LapeIOManager_SetTargetEx);
addGlobalFunc('function TIOManager.SetTarget(name, initargs: string): Integer; overload', @_LapeIOManager_SetTargetExEx);
addGlobalFunc('function TIOManager.TargetValid: Boolean;', @_LapeIOManager_TargetValid);
addGlobalFunc('function TIOManager.GetColor(x,y : Integer): TColor;', @_LapeIOManager_GetColor);
addGlobalFunc('function TIOManager.CopyData(X, Y, Width, Height: Integer): PRGB32;', @_LapeIOManager_CopyData);
addGlobalFunc('function TIOManager.ReturnMatrix(X, Y, Width, Height: Integer): TIntegerMatrix;', @_LapeIOManager_ReturnMatrix);
addGlobalFunc('function TIOManager.ReturnData(X, Y, Width, Height: Integer): TRetData;', @_LapeIOManager_ReturnData);
addGlobalFunc('procedure TIOManager.FreeReturnData;', @_LapeIOManager_FreeReturnData);
addGlobalFunc('procedure TIOManager.GetDimensions(out W, H: Integer);', @_LapeIOManager_GetDimensions);
addGlobalFunc('procedure TIOManager.GetPosition(var Left, Top: Integer);', @_LapeIOManager_GetPosition);
addGlobalFunc('procedure TIOManager.ActivateClient;', @_LapeIOManager_ActivateClient);
addGlobalFunc('function TIOManager.IsFrozen: Boolean;', @_LapeIOManager_IsFrozen);
addGlobalFunc('procedure TIOManager.Freeze;', @_LapeIOManager_Freeze);
addGlobalFunc('procedure TIOManager.UnFreeze;', @_LapeIOManager_UnFreeze);
addGlobalFunc('function TIOManager.MouseSetClientArea(x1, y1, x2, y2: Integer): boolean;', @_LapeIOManager_MouseSetClientArea);
addGlobalFunc('procedure TIOManager.MouseResetClientArea;', @_LapeIOManager_MouseResetClientArea);
addGlobalFunc('function TIOManager.ImageSetClientArea(x1, y1, x2, y2: Integer): boolean;', @_LapeIOManager_ImageSetClientArea);
addGlobalFunc('procedure TIOManager.ImageResetClientArea;', @_LapeIOManager_ImageResetClientArea);
addGlobalFunc('procedure TIOManager.GetMousePos(var X, Y: Integer);', @_LapeIOManager_GetMousePos);
addGlobalFunc('procedure TIOManager.MoveMouse(X, Y: Integer);', @_LapeIOManager_MoveMouse);
addGlobalFunc('procedure TIOManager.ScrollMouse(X, Y: Integer; Lines : Integer);', @_LapeIOManager_ScrollMouse);
addGlobalFunc('procedure TIOManager.HoldMouse(X, Y: Integer; button: TClickType);', @_LapeIOManager_HoldMouse);
addGlobalFunc('procedure TIOManager.ReleaseMouse(X, Y: Integer; button: TClickType);', @_LapeIOManager_ReleaseMouse);
addGlobalFunc('procedure TIOManager.ClickMouse(X, Y: Integer; button: TClickType);', @_LapeIOManager_ClickMouse);
addGlobalFunc('function TIOManager.IsMouseButtonDown(button : TClickType): boolean;', @_LapeIOManager_IsMouseButtonDown);
addGlobalFunc('procedure TIOManager.KeyUp(key: Integer);', @_LapeIOManager_KeyUp);
addGlobalFunc('procedure TIOManager.KeyDown(key: Integer);', @_LapeIOManager_KeyDown);
addGlobalFunc('procedure TIOManager.PressKey(key: Integer);', @_LapeIOManager_PressKey);
addGlobalFunc('procedure TIOManager.SendText(Text: string; KeyWait, KeyModWait: Integer);', @_LapeIOManager_SendText);
addGlobalFunc('procedure TIOManager.SendTextEx(Text: string; MinKeyWait, MaxKeyWait: Integer);', @_LapeIOManager_SendTextEx);
addGlobalFunc('function TIOManager.IsKeyDown(key: Integer): Boolean;', @_LapeIOManager_isKeyDown);
addGlobalFunc('function TIOManager.GetKeyCode(c : char): Integer;', @_LapeIOManager_GetKeyCode);
addGlobalFunc('function TIOManager.GetImageTarget: TTarget;', @_LapeIOManager_GetImageTarget);
addGlobalFunc('function TIOManager.GetKeyMouseTarget: TTarget;', @_LapeIOManager_GetKeyMouseTarget);
addGlobalFunc('function TIOManager.ExportImageTarget: TTarget_Exported;', @_LapeIOManager_ExportImageTarget);
addGlobalFunc('function TIOManager.ExportKeyMouseTarget: TTarget_Exported;', @_LapeIOManager_ExportKeyMouseTarget);
addGlobalFunc('procedure TIOManager.GetImageTarget(var idx: Integer); overload', @_LapeIOManager_GetImageTargetEx);
addGlobalFunc('procedure TIOManager.GetKeyMouseTarget(var idx: Integer); overload', @_LapeIOManager_GetKeyMouseTargetEx);
addGlobalFunc('procedure TIOManager.SetImageTarget(idx: Integer);', @_LapeIOManager_SetImageTarget);
addGlobalFunc('procedure TIOManager.SetKeyMouseTarget(idx: Integer);', @_LapeIOManager_SetKeyMouseTarget);
addGlobalFunc('procedure TIOManager.FreeTarget(idx: Integer);', @_LapeIOManager_FreeTarget);
addGlobalFunc('function TIOManager.AddHandlerInvalidTarget(Handler: TNotifyEvent): TNotifyEvent;', @_LapeIOManager_AddHandlerInvalidTarget);
addGlobalFunc('procedure TIOManager.RemoveHandlerInvalidTarget(Handler: TNotifyEvent);', @_LapeIOManager_RemoveHandlerInvalidTarget);

addClassVar('TIOManager', 'AutoActivate', 'Boolean', @_LapeIOManager_AutoActivate_Read, @_LapeIOManager_AutoActivate_Write);

//addGlobalFunc('procedure TIOManager.Free;', @_LapeIOManager_Free);

popSection();

