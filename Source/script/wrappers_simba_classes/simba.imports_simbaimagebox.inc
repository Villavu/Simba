pushSection('Classes');

addClass('TSimbaImageBox', 'TCustomControl');
addGlobalType('procedure(Sender: TObject; ACanvas: TCanvas; R: TRect) of object', 'TSimbaImageBox_PaintArea', FFI_DEFAULT_ABI);
addClassVar('TSimbaImageBox', 'OnPaintArea', 'TSimbaImageBox_PaintArea', @_LapeSimbaImageBox_OnPaintArea_Read, @_LapeSimbaImageBox_OnPaintArea_Write);
addClassVar('TSimbaImageBox', 'Zoom', 'Single', @_LapeSimbaImageBox_Zoom_Read, @_LapeSimbaImageBox_Zoom_Write);
addClassVar('TSimbaImageBox', 'StatusBar', 'TStatusBar', @_LapeSimbaImageBox_StatusBar_Read);
addClassVar('TSimbaImageBox', 'StatusPanel', 'TStatusPanel', @_LapeSimbaImageBox_StatusPanel_Read);
addClassVar('TSimbaImageBox', 'OnMouseMove', 'TMouseMoveEvent', @_LapeSimbaImageBox_OnMouseMove_Read, @_LapeSimbaImageBox_OnMouseMove_Write);
addClassVar('TSimbaImageBox', 'OnMouseDown', 'TMouseEvent', @_LapeSimbaImageBox_OnMouseDown_Read, @_LapeSimbaImageBox_OnMouseDown_Write);
addClassVar('TSimbaImageBox', 'OnMouseUp', 'TMouseEvent', @_LapeSimbaImageBox_OnMouseUp_Read, @_LapeSimbaImageBox_OnMouseUp_Write);
addClassVar('TSimbaImageBox', 'OnMouseLeave', 'TNotifyEvent', @_LapeSimbaImageBox_OnMouseLeave_Read, @_LapeSimbaImageBox_OnMouseLeave_Write);
addClassVar('TSimbaImageBox', 'OnMouseEnter', 'TNotifyEvent', @_LapeSimbaImageBox_OnMouseEnter_Read, @_LapeSimbaImageBox_OnMouseEnter_Write);
addClassVar('TSimbaImageBox', 'OnDblClick', 'TNotifyEvent', @_LapeSimbaImageBox_OnDblClick_Read, @_LapeSimbaImageBox_OnDblClick_Write);
//addClassVar('TSimbaImageBox', 'Cursor', 'TCursor', @_LapeSimbaImageBox_Cursor_Read, @_LapeSimbaImageBox_Cursor_Write);
addGlobalFunc('procedure TSimbaImageBox.MoveTo(X, Y: Int32);', @_LapeSimbaImageBox_MoveTo);
addGlobalFunc('function TSimbaImageBox.IsVisible(X, Y: Int32): Boolean; overload;', @_LapeSimbaImageBox_IsVisible);

addGlobalFunc('procedure TSimbaImageBox.DebugColor(CTS: Integer; Col, Tol: Integer; HueMod: Extended = 0.2; SatMod: Extended = 0.2);', @_LapeSimbaImageBox_DebugColor);
addGlobalFunc('procedure TSimbaImageBox.DebugDTM(DTM: TMDTM);', @_LapeSimbaImageBox_DebugDTM);
addGlobalFunc('procedure TSimbaImageBox.DebugTPA(TPA: TPointArray);', @_LapeSimbaImageBox_DebugTPA);
addGlobalFunc('procedure TSimbaImageBox.Clear; overload;', @_LapeSimbaImageBox_Clear);
addGlobalFunc('procedure TSimbaImageBox.Paint; overload;', @_LapeSimbaImageBox_Paint);

addGlobalFunc('procedure TSimbaImageBox.SetBackground(Data: PRGB32; AWidth, AHeight: Integer); overload;', @_LapeSimbaImageBox_SetBackground_Data);
addGlobalFunc('procedure TSimbaImageBox.SetBackground(FileName: String); overload;', @_LapeSimbaImageBox_SetBackground_FileName);
addGlobalFunc('procedure TSimbaImageBox.SetBackground(Bitmap: TMufasaBitmap); overload;', @_LapeSimbaImageBox_SetBackground_Bitmap);
addGlobalFunc('procedure TSimbaImageBox.SetBackground(IOManager: TIOManager; X1, Y1, X2, Y2: Integer); overload;', @_LapeSimbaImageBox_SetBackground_IOManagerArea);
addGlobalFunc('procedure TSimbaImageBox.SetBackground(IOManager: TIOManager); overload;', @_LapeSimbaImageBox_SetBackground_IOManager);

addGlobalFunc('procedure TSimbaImageBox.Init(Owner: TComponent); override', @_LapeSimbaImageBox_Init);
addGlobalFunc('procedure TSimbaImageBox.InitNoOverlay(Owner: TComponent); overload; override', @_LapeSimbaImageBox_InitNoOverlay);
//addGlobalFunc('procedure TSimbaImageBox.Free;', @_LapeSimbaImageBox_Free);

popSection();
