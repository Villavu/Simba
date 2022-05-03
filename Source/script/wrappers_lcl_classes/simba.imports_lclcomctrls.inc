pushSection('Classes');

addGlobalType('(pbHorizontal, pbVertical, pbRightToLeft, pbTopDown)', 'TProgressBarOrientation');
addGlobalType('(pbstNormal, pbstMarquee)', 'TProgressBarStyle');
addGlobalType('(trHorizontal, trVertical)', 'TTrackBarOrientation');
addGlobalType('(tmBottomRight, tmTopLeft, tmBoth)', 'TTickMark');
addGlobalType('(tsNone, tsAuto, tsManual)', 'TTickStyle');
addGlobalType('(trLeft, trRight, trTop, trBottom)', 'TTrackBarScalePos');
addGlobalType('procedure(Sender: TObject; Index: Integer) of object', 'TCheckListClicked', FFI_DEFAULT_ABI);
addGlobalType('procedure(Sender: TObject; var AllowChange: Boolean) of object', 'TTabChangingEvent', FFI_DEFAULT_ABI);
addGlobalType('(tsTabs, tsButtons, tsFlatButtons)', 'TTabStyle');
addGlobalType('(tpTop, tpBottom, tpLeft, tpRight)', 'TTabPosition');
addGlobalType('(nboShowCloseButtons, nboMultiLine, nboHidePageListPopup, nboKeyboardTabSwitch, nboShowAddTabButton)', 'TCTabControlOption');
addGlobalType('set of TCTabControlOption', 'TCTabControlOptions');
addGlobalType('(ppText, ppBorder, ppWidth)', 'TPanelPart');
addGlobalType('set of TPanelPart', 'TPanelParts');
addGlobalType('(psText, psOwnerDraw)', 'TStatusPanelStyle');
addGlobalType('(pbNone, pbLowered, pbRaised)', 'TStatusPanelBevel');

addClass('TCustomProgressBar', 'TWinControl');
addGlobalFunc('procedure TCustomProgressBar.Init(AOwner: TComponent); override', @_LapeCustomProgressBar_Init);
addGlobalFunc('procedure TCustomProgressBar.StepIt;', @_LapeCustomProgressBar_StepIt);
addGlobalFunc('procedure TCustomProgressBar.StepBy(Delta: Integer);', @_LapeCustomProgressBar_StepBy);
addClassVar('TCustomProgressBar', 'Max', 'Integer', @_LapeCustomProgressBar_Max_Read, @_LapeCustomProgressBar_Max_Write);
addClassVar('TCustomProgressBar', 'Min', 'Integer', @_LapeCustomProgressBar_Min_Read, @_LapeCustomProgressBar_Min_Write);
addClassVar('TCustomProgressBar', 'Orientation', 'TProgressBarOrientation', @_LapeCustomProgressBar_Orientation_Read, @_LapeCustomProgressBar_Orientation_Write);
addClassVar('TCustomProgressBar', 'Position', 'Integer', @_LapeCustomProgressBar_Position_Read, @_LapeCustomProgressBar_Position_Write);
addClassVar('TCustomProgressBar', 'Smooth', 'boolean', @_LapeCustomProgressBar_Smooth_Read, @_LapeCustomProgressBar_Smooth_Write);
addClassVar('TCustomProgressBar', 'Step', 'Integer', @_LapeCustomProgressBar_Step_Read, @_LapeCustomProgressBar_Step_Write);
addClassVar('TCustomProgressBar', 'Style', 'TProgressBarStyle', @_LapeCustomProgressBar_Style_Read, @_LapeCustomProgressBar_Style_Write);
addClassVar('TCustomProgressBar', 'BarShowText', 'boolean', @_LapeCustomProgressBar_BarShowText_Read, @_LapeCustomProgressBar_BarShowText_Write);
//addGlobalFunc('procedure TCustomProgressBar.Free;', @_LapeCustomProgressBar_Free);

addClass('TProgressBar', 'TCustomProgressBar');
addGlobalFunc('procedure TProgressBar.Init(AOwner: TComponent); override', @_LapeProgressBar_Init);
//addGlobalFunc('procedure TProgressBar.Free;', @_LapeProgressBar_Free);

addClass('TCustomTrackBar', 'TWinControl');
addGlobalFunc('procedure TCustomTrackBar.Init(AOwner: TComponent); override', @_LapeCustomTrackBar_Init);
addGlobalFunc('procedure TCustomTrackBar.SetTick(Value: Integer);', @_LapeCustomTrackBar_SetTick);
addClassVar('TCustomTrackBar', 'Frequency', 'Integer', @_LapeCustomTrackBar_Frequency_Read, @_LapeCustomTrackBar_Frequency_Write);
addClassVar('TCustomTrackBar', 'LineSize', 'Integer', @_LapeCustomTrackBar_LineSize_Read, @_LapeCustomTrackBar_LineSize_Write);
addClassVar('TCustomTrackBar', 'Max', 'Integer', @_LapeCustomTrackBar_Max_Read, @_LapeCustomTrackBar_Max_Write);
addClassVar('TCustomTrackBar', 'Min', 'Integer', @_LapeCustomTrackBar_Min_Read, @_LapeCustomTrackBar_Min_Write);
addClassVar('TCustomTrackBar', 'OnChange', 'TNotifyEvent', @_LapeCustomTrackBar_OnChange_Read, @_LapeCustomTrackBar_OnChange_Write);
addClassVar('TCustomTrackBar', 'Orientation', 'TTrackBarOrientation', @_LapeCustomTrackBar_Orientation_Read, @_LapeCustomTrackBar_Orientation_Write);
addClassVar('TCustomTrackBar', 'PageSize', 'Integer', @_LapeCustomTrackBar_PageSize_Read, @_LapeCustomTrackBar_PageSize_Write);
addClassVar('TCustomTrackBar', 'Position', 'Integer', @_LapeCustomTrackBar_Position_Read, @_LapeCustomTrackBar_Position_Write);
addClassVar('TCustomTrackBar', 'Reversed', 'Boolean', @_LapeCustomTrackBar_Reversed_Read, @_LapeCustomTrackBar_Reversed_Write);
addClassVar('TCustomTrackBar', 'ScalePos', 'TTrackBarScalePos', @_LapeCustomTrackBar_ScalePos_Read, @_LapeCustomTrackBar_ScalePos_Write);
addClassVar('TCustomTrackBar', 'SelEnd', 'Integer', @_LapeCustomTrackBar_SelEnd_Read, @_LapeCustomTrackBar_SelEnd_Write);
addClassVar('TCustomTrackBar', 'SelStart', 'Integer', @_LapeCustomTrackBar_SelStart_Read, @_LapeCustomTrackBar_SelStart_Write);
addClassVar('TCustomTrackBar', 'ShowSelRange', 'Boolean', @_LapeCustomTrackBar_ShowSelRange_Read, @_LapeCustomTrackBar_ShowSelRange_Write);
addClassVar('TCustomTrackBar', 'TickMarks', 'TTickMark', @_LapeCustomTrackBar_TickMarks_Read, @_LapeCustomTrackBar_TickMarks_Write);
addClassVar('TCustomTrackBar', 'TickStyle', 'TTickStyle', @_LapeCustomTrackBar_TickStyle_Read, @_LapeCustomTrackBar_TickStyle_Write);
//addGlobalFunc('procedure TCustomTrackBar.Free;', @_LapeCustomTrackBar_Free);

addClass('TTrackBar', 'TCustomTrackBar');
addGlobalFunc('procedure TTrackBar.Init(AOwner: TComponent); override', @_LapeTrackBar_Init);
//addGlobalFunc('procedure TTrackBar.Free;', @_LapeTrackBar_Free);

addClass('TCustomCheckListBox', 'TCustomListBox');
addGlobalFunc('procedure TCustomCheckListBox.Toggle(AIndex: Integer);', @_LapeCustomCheckListBox_Toggle);
addGlobalFunc('procedure TCustomCheckListBox.CheckAll(AState: TCheckBoxState; aAllowGrayed: Boolean = True; aAllowDisabled: Boolean = True);', @_LapeCustomCheckListBox_CheckAll);
addClassVar('TCustomCheckListBox', 'AllowGrayed', 'Boolean', @_LapeCustomCheckListBox_AllowGrayed_Read, @_LapeCustomCheckListBox_AllowGrayed_Write);
addClassVar('TCustomCheckListBox', 'Checked', 'Boolean', @_LapeCustomCheckListBox_Checked_Read, @_LapeCustomCheckListBox_Checked_Write, True);
addClassVar('TCustomCheckListBox', 'ItemEnabled', 'Boolean', @_LapeCustomCheckListBox_ItemEnabled_Read, @_LapeCustomCheckListBox_ItemEnabled_Write, True);
addClassVar('TCustomCheckListBox', 'State', 'TCheckBoxState', @_LapeCustomCheckListBox_State_Read, @_LapeCustomCheckListBox_State_Write, True);
//addClassVar('TCustomCheckListBox', 'Count', 'integer', @_LapeCustomCheckListBox_Count_Read, nil);
addClassVar('TCustomCheckListBox', 'OnClickCheck', 'TNotifyEvent', @_LapeCustomCheckListBox_OnClickCheck_Read, @_LapeCustomCheckListBox_OnClickCheck_Write);
addClassVar('TCustomCheckListBox', 'OnItemClick', 'TCheckListClicked', nil, @_LapeCustomCheckListBox_OnCheckListClicked_Write);
addGlobalFunc('procedure TCustomCheckListBox.Init(AOwner: TComponent)', @_LapeCustomCheckListBox_Init);
//addGlobalFunc('procedure TCustomCheckListBox.Free;', @_LapeCustomCheckListBox_Free);

addClass('TCheckListBox', 'TCustomCheckListBox');
addGlobalFunc('procedure TCheckListBox.Init(AOwner: TComponent); override', @_LapeCheckListBox_Init);
//addGlobalFunc('procedure TCheckListBox.Free;', @_LapeCheckListBox_Free);

addClass('TCustomPage', 'TWinControl');
addGlobalFunc('procedure TCustomPage.Init(TheOwner: TComponent); override', @_LapeCustomPage_Init);
addGlobalFunc('function TCustomPage.CanTab: boolean;', @_LapeCustomPage_CanTab);
//addGlobalFunc('function TCustomPage.IsControlVisible: Boolean;', @_LapeCustomPage_IsControlVisible);
//addGlobalFunc('function TCustomPage.HandleObjectShouldBeVisible: boolean;', @_LapeCustomPage_HandleObjectShouldBeVisible);
addGlobalFunc('function TCustomPage.VisibleIndex: integer;', @_LapeCustomPage_VisibleIndex);
addClassVar('TCustomPage', 'PageIndex', 'Integer', @_LapeCustomPage_PageIndex_Read, @_LapeCustomPage_PageIndex_Write);
addClassVar('TCustomPage', 'TabVisible', 'Boolean', @_LapeCustomPage_TabVisible_Read, @_LapeCustomPage_TabVisible_Write);
addClassVar('TCustomPage', 'OnHide', 'TNotifyEvent', @_LapeCustomPage_OnHide_Read, @_LapeCustomPage_OnHide_Write);
addClassVar('TCustomPage', 'OnShow', 'TNotifyEvent', @_LapeCustomPage_OnShow_Read, @_LapeCustomPage_OnShow_Write);
//addGlobalFunc('procedure TCustomPage.Free;', @_LapeCustomPage_Free);

addClass('TCustomTabControl', 'TWinControl');
addGlobalFunc('function TCustomTabControl.TabRect(AIndex: Integer): TRect;', @_LapeCustomTabControl_TabRect);
addGlobalFunc('function TCustomTabControl.GetImageIndex(ThePageIndex: Integer): Integer;', @_LapeCustomTabControl_GetImageIndex);
addGlobalFunc('function TCustomTabControl.IndexOf(APage: TPersistent): integer;', @_LapeCustomTabControl_IndexOf);
addGlobalFunc('function TCustomTabControl.CustomPage(Index: integer): TCustomPage;', @_LapeCustomTabControl_CustomPage);
addGlobalFunc('function TCustomTabControl.CanChangePageIndex: boolean;', @_LapeCustomTabControl_CanChangePageIndex);
addGlobalFunc('function TCustomTabControl.GetMinimumTabWidth: integer;', @_LapeCustomTabControl_GetMinimumTabWidth);
addGlobalFunc('function TCustomTabControl.GetMinimumTabHeight: integer;', @_LapeCustomTabControl_GetMinimumTabHeight);
addGlobalFunc('function TCustomTabControl.TabToPageIndex(AIndex: integer): integer;', @_LapeCustomTabControl_TabToPageIndex);
addGlobalFunc('function TCustomTabControl.PageToTabIndex(AIndex: integer): integer;', @_LapeCustomTabControl_PageToTabIndex);
addGlobalFunc('function TCustomTabControl.IndexOfTabAt(X, Y: Integer): Integer;', @_LapeCustomTabControl_IndexOfTabAt);
addGlobalFunc('procedure TCustomTabControl.DoCloseTabClicked(APage: TCustomPage);', @_LapeCustomTabControl_DoCloseTabClicked);
addClassVar('TCustomTabControl', 'MultiLine', 'Boolean', @_LapeCustomTabControl_MultiLine_Read, @_LapeCustomTabControl_MultiLine_Write);
addClassVar('TCustomTabControl', 'OnChanging', 'TTabChangingEvent', @_LapeCustomTabControl_OnChanging_Read, @_LapeCustomTabControl_OnChanging_Write);
addClassVar('TCustomTabControl', 'Options', 'TCTabControlOptions', @_LapeCustomTabControl_Options_Read, @_LapeCustomTabControl_Options_Write);
addClassVar('TCustomTabControl', 'Page', 'TCustomPage', @_LapeCustomTabControl_Page_Index_Read, nil);
addClassVar('TCustomTabControl', 'PageCount', 'integer', @_LapeCustomTabControl_PageCount_Read, nil);
addClassVar('TCustomTabControl', 'PageIndex', 'Integer', @_LapeCustomTabControl_PageIndex_Read, @_LapeCustomTabControl_PageIndex_Write);
addClassVar('TCustomTabControl', 'Pages', 'TStrings', @_LapeCustomTabControl_Pages_Read, @_LapeCustomTabControl_Pages_Write);
addClassVar('TCustomTabControl', 'ShowTabs', 'Boolean', @_LapeCustomTabControl_ShowTabs_Read, @_LapeCustomTabControl_ShowTabs_Write);
addClassVar('TCustomTabControl', 'TabPosition', 'TTabPosition', @_LapeCustomTabControl_TabPosition_Read, @_LapeCustomTabControl_TabPosition_Write);
addClassVar('TCustomTabControl', 'IsUnpaged', 'boolean', @_LapeCustomTabControl_IsUnpaged_Read, nil);
addGlobalFunc('procedure TCustomTabControl.Init(TheOwner: TComponent); override', @_LapeCustomTabControl_Init);
//addGlobalFunc('procedure TCustomTabControl.Free;', @_LapeCustomTabControl_Free);

addClass('TTabSheet', 'TCustomPage');
addClassVar('TTabSheet', 'TabIndex', 'Integer', @_LapeTabSheet_TabIndex_Read, nil);
addGlobalFunc('procedure TTabSheet.Init(TheOwner: TComponent); override', @_LapeTabSheet_Init);
//addGlobalFunc('procedure TTabSheet.Free;', @_LapeTabSheet_Free);

addClass('TPageControl', 'TCustomTabControl');
addGlobalFunc('procedure TPageControl.SelectNextPage(GoForward: Boolean; CheckTabVisible: Boolean);', @_LapePageControl_SelectNextPageEx);
addGlobalFunc('function TPageControl.AddTabSheet: TTabSheet;', @_LapePageControl_AddTabSheet);
addClassVar('TPageControl', 'Pages', 'Integer', @_LapePageControl_Pages_Read, nil);
addClassVar('TPageControl', 'ActivePage', 'TTabSheet', @_LapePageControl_ActivePage_Read, @_LapePageControl_ActivePage_Write);
addGlobalFunc('procedure TPageControl.Init(TheOwner: TComponent); override', @_LapePageControl_Init);
//addGlobalFunc('procedure TPageControl.Free;', @_LapePageControl_Free);

addClass('TStatusBar', 'TWinControl');
addClass('TStatusPanel', 'TCollectionItem');
addGlobalFunc('function TStatusPanel.StatusBar: TStatusBar;', @_LapeStatusPanel_StatusBar);
addClassVar('TStatusPanel', 'Alignment', 'TAlignment', @_LapeStatusPanel_Alignment_Read, @_LapeStatusPanel_Alignment_Write);
addClassVar('TStatusPanel', 'Bevel', 'TStatusPanelBevel', @_LapeStatusPanel_Bevel_Read, @_LapeStatusPanel_Bevel_Write);
addClassVar('TStatusPanel', 'Style', 'TStatusPanelStyle', @_LapeStatusPanel_Style_Read, @_LapeStatusPanel_Style_Write);
addClassVar('TStatusPanel', 'Text', 'TCaption', @_LapeStatusPanel_Text_Read, @_LapeStatusPanel_Text_Write);
addClassVar('TStatusPanel', 'Width', 'Integer', @_LapeStatusPanel_Width_Read, @_LapeStatusPanel_Width_Write);
addGlobalFunc('procedure TStatusPanel.Init(ACollection: TCollection); override', @_LapeStatusPanel_Init);
//addGlobalFunc('procedure TStatusPanel.Free;', @_LapeStatusPanel_Free);

addClass('TStatusPanels', 'TCollection');
addGlobalFunc('procedure TStatusPanels.Init(AStatusBar: TStatusBar)', @_LapeStatusPanels_Init);
addGlobalFunc('function TStatusPanels.Add: TStatusPanel;', @_LapeStatusPanels_Add);
addClassVar('TStatusPanels', 'Items', 'TStatusPanel', @_LapeStatusPanels_Items_Index_Read, @_LapeStatusPanels_Items_Index_Write, True); // array
addClassVar('TStatusPanels', 'StatusBar', 'TStatusBar', @_LapeStatusPanels_StatusBar_Read, nil);
//addGlobalFunc('procedure TStatusPanels.Free;', @_LapeStatusPanels_Free);

addGlobalFunc('procedure TStatusBar.InvalidatePanel(PanelIndex: integer; PanelParts: TPanelParts);', @_LapeStatusBar_InvalidatePanel);
addGlobalFunc('procedure TStatusBar.BeginUpdate;', @_LapeStatusBar_BeginUpdate);
addGlobalFunc('procedure TStatusBar.EndUpdate;', @_LapeStatusBar_EndUpdate);
addGlobalFunc('function TStatusBar.GetPanelIndexAt(X, Y: Integer): Integer;', @_LapeStatusBar_GetPanelIndexAt);
addGlobalFunc('function TStatusBar.SizeGripEnabled: Boolean;', @_LapeStatusBar_SizeGripEnabled);
addGlobalFunc('function TStatusBar.UpdatingStatusBar: boolean;', @_LapeStatusBar_UpdatingStatusBar);
addClassVar('TStatusBar', 'Canvas', 'TCanvas', @_LapeStatusBar_Canvas_Read, nil);
addClassVar('TStatusBar', 'AutoHint', 'Boolean', @_LapeStatusBar_AutoHint_Read, @_LapeStatusBar_AutoHint_Write);
addClassVar('TStatusBar', 'Panels', 'TStatusPanels', @_LapeStatusBar_Panels_Read, @_LapeStatusBar_Panels_Write);
addClassVar('TStatusBar', 'SimpleText', 'TCaption', @_LapeStatusBar_SimpleText_Read, @_LapeStatusBar_SimpleText_Write);
addClassVar('TStatusBar', 'SimplePanel', 'Boolean', @_LapeStatusBar_SimplePanel_Read, @_LapeStatusBar_SimplePanel_Write);
addClassVar('TStatusBar', 'SizeGrip', 'Boolean', @_LapeStatusBar_SizeGrip_Read, @_LapeStatusBar_SizeGrip_Write);
addClassVar('TStatusBar', 'OnHint', 'TNotifyEvent', @_LapeStatusBar_OnHint_Read, @_LapeStatusBar_OnHint_Write);
addGlobalFunc('procedure TStatusBar.Init(TheOwner: TComponent); override', @_LapeStatusBar_Init);
//addGlobalFunc('procedure TStatusBar.Free;', @_LapeStatusBar_Free);

popSection();
