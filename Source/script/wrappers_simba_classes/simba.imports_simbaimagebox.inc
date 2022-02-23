pushSection('Classes');

addClass('TSimbaImageBox_Background', 'TBitmap');
addGlobalFunc('procedure TSimbaImageBox_Background.LoadFromMufasaBitmap(Bitmap: TMufasaBitmap); constref', @_LapeSimbaImageBox_Background_LoadFromMufasaBitmap);
addGlobalFunc('procedure TSimbaImageBox_Background.LoadFromPointer(Data: PRGB32; AWidth, AHeight: Int32); constref; ', @_LapeSimbaImageBox_Background_LoadFromPointer);

addClass('TSimbaImageBox_Overlay', 'TObject');
addClassVar('TSimbaImageBox_Overlay', 'Canvas', 'TCanvas', @_LapeSimbaImageBox_Overlay_Canvas_Read);
addGlobalFunc('procedure TSimbaImageBox_Overlay.Clear; constref', @_LapeSimbaImageBox_Overlay_Clear);
addGlobalFunc('procedure TSimbaImageBox_Overlay.DebugTPA(TPA: TPointArray); constref', @_LapeSimbaImageBox_Overlay_DebugTPA);
addGlobalFunc('function TSimbaImageBox_Overlay.DebugColorCTS0(Color, Tolerance: Int32): Int32; constref', @_LapeSimbaImageBox_Overlay_DebugColorCTS0);
addGlobalFunc('function TSimbaImageBox_Overlay.DebugColorCTS1(Color, Tolerance: Int32): Int32; constref', @_LapeSimbaImageBox_Overlay_DebugColorCTS1);
addGlobalFunc('function TSimbaImageBox_Overlay.DebugColorCTS2(Color, Tolerance: Int32; Hue, Sat: Extended): Int32; constref', @_LapeSimbaImageBox_Overlay_DebugColorCTS2);
addGlobalFunc('function TSimbaImageBox_Overlay.DebugDTM(DTM: TMDTM): Int32; constref', @_LapeSimbaImageBox_Overlay_DebugDTM);

addClass('TSimbaImageBox', 'TCustomControl');
addGlobalType('procedure(Sender: TObject; ACanvas: TCanvas; R: TRect) of object', 'TSimbaImageBox_PaintArea', FFI_DEFAULT_ABI);
addClassVar('TSimbaImageBox', 'OnPaintArea', 'TSimbaImageBox_PaintArea', @_LapeSimbaImageBox_OnPaintArea_Read, @_LapeSimbaImageBox_OnPaintArea_Write);
addClassVar('TSimbaImageBox', 'Zoom', 'Double', @_LapeSimbaImageBox_Zoom_Read, @_LapeSimbaImageBox_Zoom_Write);
addClassVar('TSimbaImageBox', 'StatusBar', 'TStatusBar', @_LapeSimbaImageBox_StatusBar_Read);
addClassVar('TSimbaImageBox', 'StatusPanel', 'TStatusPanel', @_LapeSimbaImageBox_StatusPanel_Read);
addClassVar('TSimbaImageBox', 'Background', 'TSimbaImageBox_Background', @_LapeSimbaImageBox_Background_Read);
addClassVar('TSimbaImageBox', 'Overlay', 'TSimbaImageBox_Overlay', @_LapeSimbaImageBox_Overlay_Read);
addClassVar('TSimbaImageBox', 'OnMouseMove', 'TMouseMoveEvent', @_LapeSimbaImageBox_OnMouseMove_Read, @_LapeSimbaImageBox_OnMouseMove_Write);
addClassVar('TSimbaImageBox', 'OnMouseDown', 'TMouseEvent', @_LapeSimbaImageBox_OnMouseDown_Read, @_LapeSimbaImageBox_OnMouseDown_Write);
addClassVar('TSimbaImageBox', 'OnMouseUp', 'TMouseEvent', @_LapeSimbaImageBox_OnMouseUp_Read, @_LapeSimbaImageBox_OnMouseUp_Write);
addClassVar('TSimbaImageBox', 'OnMouseLeave', 'TNotifyEvent', @_LapeSimbaImageBox_OnMouseLeave_Read, @_LapeSimbaImageBox_OnMouseLeave_Write);
addClassVar('TSimbaImageBox', 'OnMouseEnter', 'TNotifyEvent', @_LapeSimbaImageBox_OnMouseEnter_Read, @_LapeSimbaImageBox_OnMouseEnter_Write);
addClassVar('TSimbaImageBox', 'OnDblClick', 'TNotifyEvent', @_LapeSimbaImageBox_OnDblClick_Read, @_LapeSimbaImageBox_OnDblClick_Write);
//addClassVar('TSimbaImageBox', 'Cursor', 'TCursor', @_LapeSimbaImageBox_Cursor_Read, @_LapeSimbaImageBox_Cursor_Write);
addGlobalFunc('procedure TSimbaImageBox.BackgroundChanged(UpdateOverlay: Boolean = True); constref', @_LapeSimbaImageBox_BackgroundChanged);
addGlobalFunc('procedure TSimbaImageBox.MoveTo(X, Y: Int32); constref', @_LapeSimbaImageBox_MoveTo);
addGlobalFunc('function TSimbaImageBox.IsVisible(X, Y: Int32): Boolean; overload; constref', @_LapeSimbaImageBox_IsVisible);
addGlobalFunc('procedure TSimbaImageBox.Init(Owner: TComponent); override', @_LapeSimbaImageBox_Init);
//addGlobalFunc('procedure TSimbaImageBox.Free; constref', @_LapeSimbaImageBox_Free);

popSection();

