pushSection('Classes');

addGlobalType('(bvNone, bvLowered, bvRaised, bvSpace)', 'TGraphicsBevelCut');
addGlobalType('TGraphicsBevelCut', 'TBevelCut');
addGlobalType('TBevelCut', 'TPanelBevel');
addGlobalType('Integer', 'TBevelWidth');
addGlobalType('(stRectangle, stSquare, stRoundRect, stRoundSquare, stEllipse, stCircle, stSquaredDiamond, stDiamond, stTriangle)', 'TShapeType');

addClass('TCustomTimer', 'TComponent');
addGlobalFunc('procedure TCustomTimer.Init(AOwner: TComponent); override', @_LapeCustomTimer_Init);
addClassVar('TCustomTimer', 'Enabled', 'Boolean', @_LapeCustomTimer_Enabled_Read, @_LapeCustomTimer_Enabled_Write);
addClassVar('TCustomTimer', 'Interval', 'UInt32', @_LapeCustomTimer_Interval_Read, @_LapeCustomTimer_Interval_Write);
addClassVar('TCustomTimer', 'OnTimer', 'TNotifyEvent', @_LapeCustomTimer_OnTimer_Read, @_LapeCustomTimer_OnTimer_Write);
addClassVar('TCustomTimer', 'OnStartTimer', 'TNotifyEvent', @_LapeCustomTimer_OnStartTimer_Read, @_LapeCustomTimer_OnStartTimer_Write);
addClassVar('TCustomTimer', 'OnStopTimer', 'TNotifyEvent', @_LapeCustomTimer_OnStopTimer_Read, @_LapeCustomTimer_OnStopTimer_Write);
//addGlobalFunc('procedure TCustomTimer.Free; constref', @_LapeCustomTimer_Free);

addClass('TTimer', 'TCustomTimer');
addGlobalFunc('procedure TTimer.Init(AOwner: TComponent); override', @_LapeTimer_Init);
//addGlobalFunc('procedure TTimer.Free; constref', @_LapeTimer_Free);

addClass('TCustomImage', 'TGraphicControl');
addGlobalFunc('procedure TCustomImage.Init(AOwner: TComponent); override', @_LapeCustomImage_Init);
//addClassVar('TCustomImage', 'Canvas', 'TCanvas', @_LapeCustomImage_Canvas_Read);
addGlobalFunc('function TCustomImage.DestRect: TRect; constref', @_LapeCustomImage_DestRect);
addClassVar('TCustomImage', 'Center', 'Boolean', @_LapeCustomImage_Center_Read, @_LapeCustomImage_Center_Write);
addClassVar('TCustomImage', 'Picture', 'TPicture', @_LapeCustomImage_Picture_Read, @_LapeCustomImage_Picture_Write);
addClassVar('TCustomImage', 'Stretch', 'Boolean', @_LapeCustomImage_Stretch_Read, @_LapeCustomImage_Stretch_Write);
addClassVar('TCustomImage', 'Transparent', 'Boolean', @_LapeCustomImage_Transparent_Read, @_LapeCustomImage_Transparent_Write);
addClassVar('TCustomImage', 'Proportional', 'Boolean', @_LapeCustomImage_Proportional_Read, @_LapeCustomImage_Proportional_Write);
addClassVar('TCustomImage', 'OnPictureChanged', 'TNotifyEvent', @_LapeCustomImage_OnPictureChanged_Read, @_LapeCustomImage_OnPictureChanged_Write);
addClassVar('TCustomImage', 'OnMouseDown', 'TMouseEvent', nil, @_LapeCustomImage_OnMouseDown_Write);
addClassVar('TCustomImage', 'OnMouseUp', 'TMouseEvent', nil, @_LapeCustomImage_OnMouseUp_Write);
//addGlobalFunc('procedure TCustomImage.Free; constref', @_LapeCustomImage_Free);

addClass('TImage', 'TCustomImage');
addGlobalFunc('procedure TImage.Init(AOwner: TComponent); override', @_LapeImage_Init);
//addGlobalFunc('procedure TImage.Free; constref', @_LapeImage_Free);
//addClassVar('TImage', 'Hint', 'String', @_LapeImage_Hint_Read, @_LapeImage_Hint_Write);
//addClassVar('TImage', 'ShowHint', 'Boolean', @_LapeImage_ShowHint_Read, @_LapeImage_ShowHint_Write);
addClassVar('TImage', 'OnMouseEnter', 'TNotifyEvent', @_LapeImage_OnMouseEnter_Read, @_LapeImage_OnMouseEnter_Write);
addClassVar('TImage', 'OnMouseLeave', 'TNotifyEvent', @_LapeImage_OnMouseLeave_Read, @_LapeImage_OnMouseLeave_Write);
addClassVar('TImage', 'OnMouseMove', 'TMouseMoveEvent', @_LapeImage_OnMouseMove_Read, @_LapeImage_OnMouseMove_Write);

addClass('TCustomPanel', 'TCustomControl');
addClassVar('TCustomPanel', 'Alignment', 'TAlignment', @_LapeCustomPanel_Alignment_Read, @_LapeCustomPanel_Alignment_Write);
addClassVar('TCustomPanel', 'BevelInner', 'TPanelBevel', @_LapeCustomPanel_BevelInner_Read, @_LapeCustomPanel_BevelInner_Write);
addClassVar('TCustomPanel', 'BevelOuter', 'TPanelBevel', @_LapeCustomPanel_BevelOuter_Read, @_LapeCustomPanel_BevelOuter_Write);
addClassVar('TCustomPanel', 'BevelWidth', 'TBevelWidth', @_LapeCustomPanel_BevelWidth_Read, @_LapeCustomPanel_BevelWidth_Write);
addClassVar('TCustomPanel', 'FullRepaint', 'Boolean', @_LapeCustomPanel_FullRepaint_Read, @_LapeCustomPanel_FullRepaint_Write);
addGlobalFunc('procedure TCustomPanel.Init(TheOwner: TComponent); override', @_LapeCustomPanel_Init);
//addGlobalFunc('procedure TCustomPanel.Free; constref', @_LapeCustomPanel_Free);

addClass('TPanel', 'TCustomPanel');
addGlobalFunc('procedure TPanel.Init(TheOwner: TComponent); override', @_LapePanel_Init);
//addGlobalFunc('procedure TPanel.Free; constref', @_LapePanel_Free);

addClass('TShape', 'TGraphicControl');
addGlobalFunc('procedure TShape.StyleChanged(Sender: TObject); constref', @_LapeShape_StyleChanged);
addClassVar('TShape', 'Brush', 'TBrush', @_LapeShape_Brush_Read, @_LapeShape_Brush_Write);
addClassVar('TShape', 'Pen', 'TPen', @_LapeShape_Pen_Read, @_LapeShape_Pen_Write);
addClassVar('TShape', 'Shape', 'TShapeType', @_LapeShape_Shape_Read, @_LapeShape_Shape_Write);
addGlobalFunc('procedure TShape.Init(TheOwner: TComponent); override', @_LapeShape_Init);
// addGlobalFunc('procedure TShape.Free; constref', @_LapeShape_Free);

popSection();
