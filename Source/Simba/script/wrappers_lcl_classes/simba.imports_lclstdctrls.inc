addGlobalType('(ssNone, ssHorizontal, ssVertical, ssBoth,ssAutoHorizontal, ssAutoVertical, ssAutoBoth)', 'TScrollStyle');
addGlobalType('(scLineUp,scLineDown, scPageUp,scPageDown,scPosition, scTrack,scTop,scBottom,scEndScroll)', 'TScrollCode');
addGlobalType('procedure(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer) of object', 'TScrollEvent', FFI_DEFAULT_ABI);
addGlobalType('(odSelected, odGrayed, odDisabled, odChecked, odFocused, odDefault, odHotLight, odInactive, odNoAccel, odNoFocusRect, odReserved1, odReserved2, odComboBoxEdit, odPainted)', 'TOwnerDrawStateType');
addGlobalType('set of TOwnerDrawStateType', 'TOwnerDrawState');
addGlobalType('procedure(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState) of object', 'TDrawItemEvent', FFI_DEFAULT_ABI);
addGlobalType('(csDropDown,csSimple,csDropDownList,csOwnerDrawFixed,csOwnerDrawVariable)', 'TComboBoxStyle');
addGlobalType('(lbStandard, lbOwnerDrawFixed, lbOwnerDrawVariable, lbVirtual)', 'TListBoxStyle');
addGlobalType('(sbsNone, sbsSingle, sbsSunken)', 'TStaticBorderStyle');
addGlobalType('(taLeftJustify, taRightJustify, taCenter)', 'TAlignment');
addGlobalType('(cbUnchecked, cbChecked, cbGrayed)', 'TCheckBoxState');
addGlobalType('(blGlyphLeft, blGlyphRight, blGlyphTop, blGlyphBottom)', 'TButtonLayout');

addClass('TCustomScrollBar', 'TWinControl');
addGlobalFunc('procedure TCustomScrollBar.Init(AOwner: TComponent); override', @_LapeCustomScrollBar_Init);
addGlobalFunc('procedure TCustomScrollBar.SetParams(APosition, AMin, AMax, APageSize: Integer); constref', @_LapeCustomScrollBar_SetParams);
addGlobalFunc('procedure TCustomScrollBar.SetParams(APosition, AMin, AMax: Integer); constref; overload', @_LapeCustomScrollBar_SetParamsEx);
addClassVar('TCustomScrollBar', 'Kind', 'TScrollBarKind', @_LapeCustomScrollBar_Kind_Read, @_LapeCustomScrollBar_Kind_Write);
addClassVar('TCustomScrollBar', 'LargeChange', 'integer', @_LapeCustomScrollBar_LargeChange_Read, @_LapeCustomScrollBar_LargeChange_Write);
addClassVar('TCustomScrollBar', 'Max', 'Integer', @_LapeCustomScrollBar_Max_Read, @_LapeCustomScrollBar_Max_Write);
addClassVar('TCustomScrollBar', 'Min', 'Integer', @_LapeCustomScrollBar_Min_Read, @_LapeCustomScrollBar_Min_Write);
addClassVar('TCustomScrollBar', 'PageSize', 'Integer', @_LapeCustomScrollBar_PageSize_Read, @_LapeCustomScrollBar_PageSize_Write);
addClassVar('TCustomScrollBar', 'Position', 'Integer', @_LapeCustomScrollBar_Position_Read, @_LapeCustomScrollBar_Position_Write);
addClassVar('TCustomScrollBar', 'SmallChange', 'integer', @_LapeCustomScrollBar_SmallChange_Read, @_LapeCustomScrollBar_SmallChange_Write);
addClassVar('TCustomScrollBar', 'OnChange', 'TNotifyEvent', @_LapeCustomScrollBar_OnChange_Read, @_LapeCustomScrollBar_OnChange_Write);
addClassVar('TCustomScrollBar', 'OnScroll', 'TScrollEvent', @_LapeCustomScrollBar_OnScroll_Read, @_LapeCustomScrollBar_OnScroll_Write);
//addGlobalFunc('procedure TCustomScrollBar.Free; constref', @_LapeCustomScrollBar_Free);

addClass('TScrollBar', 'TCustomScrollBar');
addGlobalFunc('procedure TScrollBar.Init(AOwner: TComponent); override', @_LapeScrollBar_Init);
//addGlobalFunc('procedure TScrollBar.Free; constref', @_LapeScrollBar_Free);

addClass('TCustomComboBox', 'TWinControl');
addGlobalFunc('procedure TCustomComboBox.Init(TheOwner: TComponent); override', @_LapeCustomComboBox_Init);
addGlobalFunc('procedure TCustomComboBox.IntfGetItems; constref', @_LapeCustomComboBox_IntfGetItems);
addGlobalFunc('procedure TCustomComboBox.AddItem(const Item: String; AnObject: TObject); constref', @_LapeCustomComboBox_AddItem);
addGlobalFunc('procedure TCustomComboBox.AddHistoryItem(const Item: string; MaxHistoryCount: integer;SetAsText, CaseSensitive: boolean); constref', @_LapeCustomComboBox_AddHistoryItem);
addGlobalFunc('procedure TCustomComboBox.AddHistoryItem(const Item: string; AnObject: TObject;MaxHistoryCount: integer; SetAsText, CaseSensitive: boolean); constref; overload', @_LapeCustomComboBox_AddHistoryItemEx);
addGlobalFunc('procedure TCustomComboBox.Clear; constref', @_LapeCustomComboBox_Clear);
addGlobalFunc('procedure TCustomComboBox.ClearSelection; constref', @_LapeCustomComboBox_ClearSelection);
addClassVar('TCustomComboBox', 'DroppedDown', 'Boolean', @_LapeCustomComboBox_DroppedDown_Read, @_LapeCustomComboBox_DroppedDown_Write);
addGlobalFunc('procedure TCustomComboBox.SelectAll; constref', @_LapeCustomComboBox_SelectAll);
addClassVar('TCustomComboBox', 'AutoComplete', 'boolean', @_LapeCustomComboBox_AutoComplete_Read, @_LapeCustomComboBox_AutoComplete_Write);
addClassVar('TCustomComboBox', 'AutoDropDown', 'Boolean', @_LapeCustomComboBox_AutoDropDown_Read, @_LapeCustomComboBox_AutoDropDown_Write);
addClassVar('TCustomComboBox', 'AutoSelect', 'Boolean', @_LapeCustomComboBox_AutoSelect_Read, @_LapeCustomComboBox_AutoSelect_Write);
addClassVar('TCustomComboBox', 'AutoSelected', 'Boolean', @_LapeCustomComboBox_AutoSelected_Read, @_LapeCustomComboBox_AutoSelected_Write);
addClassVar('TCustomComboBox', 'ArrowKeysTraverseList', 'Boolean', @_LapeCustomComboBox_ArrowKeysTraverseList_Read, @_LapeCustomComboBox_ArrowKeysTraverseList_Write);
addClassVar('TCustomComboBox', 'Canvas', 'TCanvas', @_LapeCustomComboBox_Canvas_Read);
addClassVar('TCustomComboBox', 'DropDownCount', 'Integer', @_LapeCustomComboBox_DropDownCount_Read, @_LapeCustomComboBox_DropDownCount_Write);
addClassVar('TCustomComboBox', 'Items', 'TStrings', @_LapeCustomComboBox_Items_Read, @_LapeCustomComboBox_Items_Write);
addClassVar('TCustomComboBox', 'ItemIndex', 'integer', @_LapeCustomComboBox_ItemIndex_Read, @_LapeCustomComboBox_ItemIndex_Write);
addClassVar('TCustomComboBox', 'ReadOnly', 'Boolean', @_LapeCustomComboBox_ReadOnly_Read, @_LapeCustomComboBox_ReadOnly_Write);
addClassVar('TCustomComboBox', 'SelLength', 'integer', @_LapeCustomComboBox_SelLength_Read, @_LapeCustomComboBox_SelLength_Write);
addClassVar('TCustomComboBox', 'SelStart', 'integer', @_LapeCustomComboBox_SelStart_Read, @_LapeCustomComboBox_SelStart_Write);
addClassVar('TCustomComboBox', 'SelText', 'String', @_LapeCustomComboBox_SelText_Read, @_LapeCustomComboBox_SelText_Write);
addClassVar('TCustomComboBox', 'Style', 'TComboBoxStyle', @_LapeCustomComboBox_Style_Read, @_LapeCustomComboBox_Style_Write);
addClassVar('TCustomComboBox', 'Text', 'string', @_LapeCustomComboBox_Text_Read, @_LapeCustomComboBox_Text_Write);
//addGlobalFunc('procedure TCustomComboBox.Free; constref', @_LapeCustomComboBox_Free);

addClass('TComboBox', 'TCustomComboBox');
addGlobalFunc('procedure TComboBox.Init(TheOwner: TComponent); override', @_LapeComboBox_Init);
addClassVar('TComboBox', 'OnChange', 'TNotifyEvent', @_LapeCustomComboBox_OnChange_Read, @_LapeCustomComboBox_OnChange_Write);
//addGlobalFunc('procedure TComboBox.Free; constref', @_LapeComboBox_Free);

addClass('TCustomListBox', 'TWinControl');
addGlobalFunc('procedure TCustomListBox.Init(TheOwner: TComponent); override', @_LapeCustomListBox_Init);
addGlobalFunc('procedure TCustomListBox.AddItem(const Item: String; AnObject: TObject); constref', @_LapeCustomListBox_AddItem);
addGlobalFunc('procedure TCustomListBox.Click; constref', @_LapeCustomListBox_Click);
addGlobalFunc('procedure TCustomListBox.Clear; constref', @_LapeCustomListBox_Clear);
addGlobalFunc('procedure TCustomListBox.ClearSelection; constref', @_LapeCustomListBox_ClearSelection);
addGlobalFunc('function TCustomListBox.GetIndexAtXY(X, Y: integer): integer; constref', @_LapeCustomListBox_GetIndexAtXY);
addGlobalFunc('function TCustomListBox.GetIndexAtY(Y: integer): integer; constref', @_LapeCustomListBox_GetIndexAtY);
addGlobalFunc('function TCustomListBox.GetSelectedText: string; constref', @_LapeCustomListBox_GetSelectedText);
addGlobalFunc('function TCustomListBox.ItemAtPos(const Pos: TPoint; Existing: Boolean): Integer; constref', @_LapeCustomListBox_ItemAtPos);
addGlobalFunc('function TCustomListBox.ItemRect(Index: Integer): TRect; constref', @_LapeCustomListBox_ItemRect);
addGlobalFunc('function TCustomListBox.ItemVisible(Index: Integer): boolean; constref', @_LapeCustomListBox_ItemVisible);
addGlobalFunc('function TCustomListBox.ItemFullyVisible(Index: Integer): boolean; constref', @_LapeCustomListBox_ItemFullyVisible);
addGlobalFunc('procedure TCustomListBox.LockSelectionChange; constref', @_LapeCustomListBox_LockSelectionChange);
addGlobalFunc('procedure TCustomListBox.MakeCurrentVisible; constref', @_LapeCustomListBox_MakeCurrentVisible);
addGlobalFunc('procedure TCustomListBox.MeasureItem(Index: Integer; var TheHeight: Integer); constref', @_LapeCustomListBox_MeasureItem);
addGlobalFunc('procedure TCustomListBox.SelectAll; constref', @_LapeCustomListBox_SelectAll);
addGlobalFunc('procedure TCustomListBox.UnlockSelectionChange; constref', @_LapeCustomListBox_UnlockSelectionChange);
addClassVar('TCustomListBox', 'Canvas', 'TCanvas', @_LapeCustomListBox_Canvas_Read);
addClassVar('TCustomListBox', 'ClickOnSelChange', 'boolean', @_LapeCustomListBox_ClickOnSelChange_Read, @_LapeCustomListBox_ClickOnSelChange_Write);
addClassVar('TCustomListBox', 'Columns', 'Integer', @_LapeCustomListBox_Columns_Read, @_LapeCustomListBox_Columns_Write);
addClassVar('TCustomListBox', 'Count', 'Integer', @_LapeCustomListBox_Count_Read);
addClassVar('TCustomListBox', 'ExtendedSelect', 'boolean', @_LapeCustomListBox_ExtendedSelect_Read, @_LapeCustomListBox_ExtendedSelect_Write);
addClassVar('TCustomListBox', 'ItemHeight', 'Integer', @_LapeCustomListBox_ItemHeight_Read, @_LapeCustomListBox_ItemHeight_Write);
addClassVar('TCustomListBox', 'ItemIndex', 'integer', @_LapeCustomListBox_ItemIndex_Read, @_LapeCustomListBox_ItemIndex_Write);
addClassVar('TCustomListBox', 'Items', 'TStrings', @_LapeCustomListBox_Items_Read, @_LapeCustomListBox_Items_Write);
addClassVar('TCustomListBox', 'MultiSelect', 'boolean', @_LapeCustomListBox_MultiSelect_Read, @_LapeCustomListBox_MultiSelect_Write);
addClassVar('TCustomListBox', 'ScrollWidth', 'Integer', @_LapeCustomListBox_ScrollWidth_Read, @_LapeCustomListBox_ScrollWidth_Write);
addClassVar('TCustomListBox', 'SelCount', 'integer', @_LapeCustomListBox_SelCount_Read);
addClassVar('TCustomListBox', 'Sorted', 'boolean', @_LapeCustomListBox_Sorted_Read, @_LapeCustomListBox_Sorted_Write);
addClassVar('TCustomListBox', 'TopIndex', 'Integer', @_LapeCustomListBox_TopIndex_Read, @_LapeCustomListBox_TopIndex_Write);
addClassVar('TCustomListBox', 'OnDrawItem', 'TDrawItemEvent', nil, @_LapeCustomListBox_OnDrawItem_Write);
addClassVar('TCustomListBox', 'Style', 'TListBoxStyle', @_LapeCustomListBox_Style_Read, @_LapeCustomListBox_Style_Write);
//addGlobalFunc('procedure TCustomListBox.Free; constref', @_LapeCustomListBox_Free);

addClass('TListBox', 'TCustomListBox');
addGlobalFunc('procedure TListBox.Init(TheOwner: TComponent); override', @_LapeListBox_Init);
// addGlobalFunc('procedure TListBox.Free; constref', @_LapeListBox_Free);

addClass('TCustomEdit', 'TWinControl');
addGlobalFunc('procedure TCustomEdit.Init(AOwner: TComponent); override', @_LapeCustomEdit_Init);
addGlobalFunc('procedure TCustomEdit.Clear; constref', @_LapeCustomEdit_Clear);
addGlobalFunc('procedure TCustomEdit.SelectAll; constref', @_LapeCustomEdit_SelectAll);
addGlobalFunc('procedure TCustomEdit.ClearSelection; constref', @_LapeCustomEdit_ClearSelection);
addGlobalFunc('procedure TCustomEdit.CopyToClipboard; constref', @_LapeCustomEdit_CopyToClipboard);
addGlobalFunc('procedure TCustomEdit.CutToClipboard; constref', @_LapeCustomEdit_CutToClipboard);
addGlobalFunc('procedure TCustomEdit.PasteFromClipboard; constref', @_LapeCustomEdit_PasteFromClipboard);
addGlobalFunc('procedure TCustomEdit.Undo; constref', @_LapeCustomEdit_Undo);
addClassVar('TCustomEdit', 'CanUndo', 'Boolean', @_LapeCustomEdit_CanUndo_Read);
addClassVar('TCustomEdit', 'CaretPos', 'TPoint', @_LapeCustomEdit_CaretPos_Read, @_LapeCustomEdit_CaretPos_Write);
addClassVar('TCustomEdit', 'HideSelection', 'Boolean', @_LapeCustomEdit_HideSelection_Read, @_LapeCustomEdit_HideSelection_Write);
addClassVar('TCustomEdit', 'MaxLength', 'Integer', @_LapeCustomEdit_MaxLength_Read, @_LapeCustomEdit_MaxLength_Write);
addClassVar('TCustomEdit', 'Modified', 'Boolean', @_LapeCustomEdit_Modified_Read, @_LapeCustomEdit_Modified_Write);
addClassVar('TCustomEdit', 'OnChange', 'TNotifyEvent', @_LapeCustomEdit_OnChange_Read, @_LapeCustomEdit_OnChange_Write);
addClassVar('TCustomEdit', 'PasswordChar', 'Char', @_LapeCustomEdit_PasswordChar_Read, @_LapeCustomEdit_PasswordChar_Write);
addClassVar('TCustomEdit', 'ReadOnly', 'Boolean', @_LapeCustomEdit_ReadOnly_Read, @_LapeCustomEdit_ReadOnly_Write);
addClassVar('TCustomEdit', 'SelLength', 'integer', @_LapeCustomEdit_SelLength_Read, @_LapeCustomEdit_SelLength_Write);
addClassVar('TCustomEdit', 'SelStart', 'integer', @_LapeCustomEdit_SelStart_Read, @_LapeCustomEdit_SelStart_Write);
addClassVar('TCustomEdit', 'SelText', 'String', @_LapeCustomEdit_SelText_Read, @_LapeCustomEdit_SelText_Write);
addClassVar('TCustomEdit', 'Text', 'string', @_LapeCustomEdit_Text_Read, @_LapeCustomEdit_Text_Write);
//addGlobalFunc('procedure TCustomEdit.Free; constref', @_LapeCustomEdit_Free);

addClass('TEdit', 'TCustomEdit');
addClassVar('TEdit', 'OnEditingDone', 'TNotifyEvent', @_LapeEdit_OnEditingDone_Read, @_LapeEdit_OnEditingDone_Write);
addGlobalFunc('procedure TEdit.Init(AOwner: TComponent); override', @_LapeEdit_Init);
//addGlobalFunc('procedure TEdit.Free; constref', @_LapeEdit_Free);

addClass('TCustomGroupBox', 'TWinControl');
addGlobalFunc('procedure TCustomGroupBox.Init(AOwner: TComponent); override', @_LapeCustomGroupBox_Init);
//addGlobalFunc('procedure TCustomGroupBox.Free; constref', @_LapeCustomGroupBox_Free);

addClass('TGroupBox', 'TCustomGroupBox');
addGlobalFunc('procedure TGroupBox.Init(AOwner: TComponent); override', @_LapeGroupBox_Init);
//addGlobalFunc('procedure TGroupBox.Free; constref', @_LapeGroupBox_Free);

addClass('TMemoScrollbar', 'TControlScrollBar');
addGlobalFunc('procedure TMemoScrollbar.Init(AControl: TWinControl; AKind: TScrollBarKind)', @_LapeMemoScrollbar_Init);
//addGlobalFunc('procedure TMemoScrollbar.Free; constref', @_LapeMemoScrollbar_Free);

addClass('TCustomMemo', 'TCustomEdit');
addGlobalFunc('procedure TCustomMemo.Init(AOwner: TComponent); override', @_LapeCustomMemo_Init);
addGlobalFunc('procedure TCustomMemo.Append(const Value: String); constref', @_LapeCustomMemo_Append);
addClassVar('TCustomMemo', 'Lines', 'TStrings', @_LapeCustomMemo_Lines_Read, @_LapeCustomMemo_Lines_Write);
addClassVar('TCustomMemo', 'HorzScrollBar', 'TMemoScrollBar', @_LapeCustomMemo_HorzScrollBar_Read, @_LapeCustomMemo_HorzScrollBar_Write);
addClassVar('TCustomMemo', 'VertScrollBar', 'TMemoScrollBar', @_LapeCustomMemo_VertScrollBar_Read, @_LapeCustomMemo_VertScrollBar_Write);
addClassVar('TCustomMemo', 'ScrollBars', 'TScrollStyle', @_LapeCustomMemo_ScrollBars_Read, @_LapeCustomMemo_ScrollBars_Write);
addClassVar('TCustomMemo', 'WantReturns', 'Boolean', @_LapeCustomMemo_WantReturns_Read, @_LapeCustomMemo_WantReturns_Write);
addClassVar('TCustomMemo', 'WantTabs', 'Boolean', @_LapeCustomMemo_WantTabs_Read, @_LapeCustomMemo_WantTabs_Write);
addClassVar('TCustomMemo', 'WordWrap', 'Boolean', @_LapeCustomMemo_WordWrap_Read, @_LapeCustomMemo_WordWrap_Write);
//addGlobalFunc('procedure TCustomMemo.Free; constref', @_LapeCustomMemo_Free);

addClass('TMemo', 'TCustomMemo');
addGlobalFunc('procedure TMemo.Init(AOwner: TComponent); override', @_LapeMemo_Init);
//addGlobalFunc('procedure TMemo.Free; constref', @_LapeMemo_Free);

addClass('TButtonControl', 'TWinControl');
addGlobalFunc('procedure TButtonControl.Init(TheOwner: TComponent); override', @_LapeButtonControl_Init);
//addGlobalFunc('procedure TButtonControl.Free; constref', @_LapeButtonControl_Free);

addClass('TCustomButton', 'TButtonControl');
addGlobalFunc('procedure TCustomButton.Init(TheOwner: TComponent); override', @_LapeCustomButton_Init);
//addGlobalFunc('procedure TCustomButton.ExecuteDefaultAction; constref', @_LapeCustomButton_ExecuteDefaultAction);
//addGlobalFunc('procedure TCustomButton.ExecuteCancelAction; constref', @_LapeCustomButton_ExecuteCancelAction);
//addGlobalFunc('procedure TCustomButton.ActiveDefaultControlChanged(NewControl: TControl); constref', @_LapeCustomButton_ActiveDefaultControlChanged);
//addGlobalFunc('procedure TCustomButton.UpdateRolesForForm; constref', @_LapeCustomButton_UpdateRolesForForm);
//addGlobalFunc('function TCustomButton.UseRightToLeftAlignment: Boolean; constref', @_LapeCustomButton_UseRightToLeftAlignment);
addClassVar('TCustomButton', 'Active', 'boolean', @_LapeCustomButton_Active_Read);
addClassVar('TCustomButton', 'Default', 'Boolean', @_LapeCustomButton_Default_Read, @_LapeCustomButton_Default_Write);
addClassVar('TCustomButton', 'Cancel', 'Boolean', @_LapeCustomButton_Cancel_Read, @_LapeCustomButton_Cancel_Write);
//addGlobalFunc('procedure TCustomButton.Free; constref', @_LapeCustomButton_Free);

addClass('TButton', 'TCustomButton');
addGlobalFunc('procedure TButton.Init(TheOwner: TComponent); override', @_LapeButton_Init);
//addGlobalFunc('procedure TButton.Free; constref', @_LapeButton_Free);

addClass('TCustomCheckBox', 'TButtonControl');
addGlobalFunc('procedure TCustomCheckBox.Init(TheOwner: TComponent); override', @_LapeCustomCheckBox_Init);
addClassVar('TCustomCheckBox', 'AllowGrayed', 'Boolean', @_LapeCustomCheckBox_AllowGrayed_Read, @_LapeCustomCheckBox_AllowGrayed_Write);
addClassVar('TCustomCheckBox', 'State', 'TCheckBoxState', @_LapeCustomCheckBox_State_Read, @_LapeCustomCheckBox_State_Write);
addClassVar('TCustomCheckBox', 'OnChange', 'TNotifyEvent', @_LapeCustomCheckBox_OnChange_Read, @_LapeCustomCheckBox_OnChange_Write);
// addGlobalFunc('procedure TCustomCheckBox.Free; constref', @_LapeCustomCheckBox_Free);

addClass('TCheckBox', 'TCustomCheckBox');
addGlobalFunc('procedure TCheckBox.Init(TheOwner: TComponent); override', @_LapeCheckBox_Init);
//addGlobalFunc('procedure TCheckBox.Free; constref', @_LapeCheckBox_Free);

addClass('TCustomLabel', 'TGraphicControl');
addGlobalFunc('procedure TCustomLabel.Init(TheOwner: TComponent); override', @_LapeCustomLabel_Init);
addGlobalFunc('function TCustomLabel.ColorIsStored: boolean; constref', @_LapeCustomLabel_ColorIsStored);
addGlobalFunc('function TCustomLabel.AdjustFontForOptimalFill: Boolean; constref', @_LapeCustomLabel_AdjustFontForOptimalFill);
addGlobalFunc('procedure TCustomLabel.Paint; constref', @_LapeCustomLabel_Paint);
//addGlobalFunc('procedure TCustomLabel.SetBounds(aLeft, aTop, aWidth, aHeight: integer); constref', @_LapeCustomLabel_SetBounds);
//addGlobalFunc('procedure TCustomLabel.Free; constref', @_LapeCustomLabel_Free);

addClass('TLabel', 'TCustomLabel');
addGlobalFunc('procedure TLabel.Init(TheOwner: TComponent); override', @_LapeLabel_Init);
//addGlobalFunc('procedure TLabel.Free; constref', @_LapeLabel_Free);

addClass('TCustomSpeedButton', 'TGraphicControl');
addGlobalFunc('function TCustomSpeedButton.FindDownButton: TCustomSpeedButton; constref', @_LapeCustomSpeedButton_FindDownButton);
addGlobalFunc('procedure TCustomSpeedButton.LoadGlyphFromLazarusResource(const AName: String); constref', @_LapeCustomSpeedButton_LoadGlyphFromLazarusResource);
addClassVar('TCustomSpeedButton', 'AllowAllUp', 'Boolean', @_LapeCustomSpeedButton_AllowAllUp_Read, @_LapeCustomSpeedButton_AllowAllUp_Write);
addClassVar('TCustomSpeedButton', 'Down', 'Boolean', @_LapeCustomSpeedButton_Down_Read, @_LapeCustomSpeedButton_Down_Write);
addClassVar('TCustomSpeedButton', 'Flat', 'Boolean', @_LapeCustomSpeedButton_Flat_Read, @_LapeCustomSpeedButton_Flat_Write);
addClassVar('TCustomSpeedButton', 'Glyph', 'TBitmap', @_LapeCustomSpeedButton_Glyph_Read, @_LapeCustomSpeedButton_Glyph_Write);
addClassVar('TCustomSpeedButton', 'GroupIndex', 'Integer', @_LapeCustomSpeedButton_GroupIndex_Read, @_LapeCustomSpeedButton_GroupIndex_Write);
addClassVar('TCustomSpeedButton', 'Layout', 'TButtonLayout', @_LapeCustomSpeedButton_Layout_Read, @_LapeCustomSpeedButton_Layout_Write);
addClassVar('TCustomSpeedButton', 'Margin', 'integer', @_LapeCustomSpeedButton_Margin_Read, @_LapeCustomSpeedButton_Margin_Write);
addClassVar('TCustomSpeedButton', 'NumGlyphs', 'Integer', @_LapeCustomSpeedButton_NumGlyphs_Read, @_LapeCustomSpeedButton_NumGlyphs_Write);
addClassVar('TCustomSpeedButton', 'ShowAccelChar', 'boolean', @_LapeCustomSpeedButton_ShowAccelChar_Read, @_LapeCustomSpeedButton_ShowAccelChar_Write);
addClassVar('TCustomSpeedButton', 'ShowCaption', 'boolean', @_LapeCustomSpeedButton_ShowCaption_Read, @_LapeCustomSpeedButton_ShowCaption_Write);
addClassVar('TCustomSpeedButton', 'Spacing', 'integer', @_LapeCustomSpeedButton_Spacing_Read, @_LapeCustomSpeedButton_Spacing_Write);
addClassVar('TCustomSpeedButton', 'Transparent', 'Boolean', @_LapeCustomSpeedButton_Transparent_Read, @_LapeCustomSpeedButton_Transparent_Write);
addGlobalFunc('procedure TCustomSpeedButton.Init(AOwner: TComponent); override', @_LapeCustomSpeedButton_Init);
//addGlobalFunc('procedure TCustomSpeedButton.Free; constref', @_LapeCustomSpeedButton_Free);

addClass('TSpeedButton', 'TCustomSpeedButton');
addGlobalFunc('procedure TSpeedButton.Init(AOwner: TComponent); override', @_LapeSpeedButton_Init);
//addGlobalFunc('procedure TSpeedButton.Free; constref', @_LapeSpeedButton_Free);

addClass('TRadioButton', 'TCustomCheckBox');
addGlobalFunc('procedure TRadioButton.Init(AOwner: TComponent); override', @_LapeRadioButton_Init);
// addGlobalFunc('procedure TRadioButton.Free; constref', @_LapeRadioButton_Free);

