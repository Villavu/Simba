pushSection('Classes');

addClass('TTarget');
addGlobalType('record Ptr: PRGB32; IncPtrWith: integer; RowLen: integer; end', 'TRetData');
addGlobalType('UInt32', 'TClickType');
addGlobalFunc('procedure TTarget.GetTargetDimensions(out w, h: integer);', @_LapeTarget_GetTargetDimensions);
addGlobalFunc('procedure TTarget.GetTargetPosition(out left, top: integer);', @_LapeTarget_GetTargetPosition);
addGlobalFunc('function TTarget.GetColor(x,y : integer): TColor;', @_LapeTarget_GetColor);
addGlobalFunc('function TTarget.CopyData(X, Y, Width, Height: Integer): PRGB32;', @_LapeTarget_CopyData);
addGlobalFunc('function TTarget.ReturnData(xs, ys, width, height: Integer): TRetData;', @_LapeTarget_ReturnData);
addGlobalFunc('procedure TTarget.FreeReturnData;', @_LapeTarget_FreeReturnData);
addGlobalFunc('procedure TTarget.ActivateClient;', @_LapeTarget_ActivateClient);
addGlobalFunc('function TTarget.TargetValid: boolean;', @_LapeTarget_TargetValid);
addGlobalFunc('function TTarget.MouseSetClientArea(x1, y1, x2, y2: integer): boolean;', @_LapeTarget_MouseSetClientArea);
addGlobalFunc('procedure TTarget.MouseResetClientArea;', @_LapeTarget_MouseResetClientArea);
addGlobalFunc('function TTarget.ImageSetClientArea(x1, y1, x2, y2: integer): boolean;', @_LapeTarget_ImageSetClientArea);
addGlobalFunc('procedure TTarget.ImageResetClientArea;', @_LapeTarget_ImageResetClientArea);
addGlobalFunc('procedure TTarget.GetMousePosition(out x,y: integer);', @_LapeTarget_GetMousePosition);
addGlobalFunc('procedure TTarget.MoveMouse(x,y: integer);', @_LapeTarget_MoveMouse);
addGlobalFunc('procedure TTarget.ScrollMouse(x,y : integer; Lines : integer);', @_LapeTarget_ScrollMouse);
addGlobalFunc('procedure TTarget.HoldMouse(x,y: integer; button: TClickType);', @_LapeTarget_HoldMouse);
addGlobalFunc('procedure TTarget.ReleaseMouse(x,y: integer; button: TClickType);', @_LapeTarget_ReleaseMouse);
addGlobalFunc('function TTarget.IsMouseButtonHeld(button : TClickType): boolean;', @_LapeTarget_IsMouseButtonHeld);
addGlobalFunc('procedure TTarget.SendString(str: string; keywait, keymodwait: integer);', @_LapeTarget_SendString);
addGlobalFunc('procedure TTarget.HoldKey(key: integer);', @_LapeTarget_HoldKey);
addGlobalFunc('procedure TTarget.ReleaseKey(key: integer);', @_LapeTarget_ReleaseKey);
addGlobalFunc('function TTarget.IsKeyHeld(key: integer): boolean;', @_LapeTarget_IsKeyHeld);
addGlobalFunc('function TTarget.GetKeyCode(C : char): integer;', @_LapeTarget_GetKeyCode);
addGlobalFunc('procedure TTarget.Init()', @_LapeTarget_Init);
// addGlobalFunc('procedure TTarget.Free;', @_LapeTarget_Free);

popSection();

