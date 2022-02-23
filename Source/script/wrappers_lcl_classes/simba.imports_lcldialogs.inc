pushSection('Classes');

addGlobalType('(ofReadOnly, ofOverwritePrompt,ofHideReadOnly,ofNoChangeDir,ofShowHelp,ofNoValidate,ofAllowMultiSelect,ofExtensionDifferent,ofPathMustExist,ofFileMustExist,ofCreatePrompt,ofShareAware,ofNoReadOnlyReturn,ofNoTestFileCreate,ofNoNetworkButton,ofNoLongNames,ofOldStyleDialog,ofNoDereferenceLinks,ofEnableIncludeNotify,ofEnableSizing,ofDontAddToRecent,ofForceShowHidden,ofViewDetail,ofAutoPreview)', 'TOpenOption');
addGlobalType('set of TOpenOption', 'TOpenOptions');

addClass('TLCLComponent', 'TComponent');
addGlobalFunc('procedure TLCLComponent.RemoveAllHandlersOfObject(AnObject: TObject); constref', @_LapeLCLComponent_RemoveAllHandlersOfObject);
addGlobalFunc('procedure TLCLComponent.IncLCLRefCount; constref', @_LapeLCLComponent_IncLCLRefCount);
addGlobalFunc('procedure TLCLComponent.DecLCLRefCount; constref', @_LapeLCLComponent_DecLCLRefCount);
addClassVar('TLCLComponent', 'LCLRefCount', 'integer', @_LapeLCLComponent_LCLRefCount_Read, nil);
addGlobalFunc('procedure TLCLComponent.Init(TheOwner: TComponent); override', @_LapeLCLComponent_Init);
//addGlobalFunc('procedure TLCLComponent.Free; constref', @_LapeLCLComponent_Free);

addClass('TCommonDialog', 'TLCLComponent');
addClassVar('TCommonDialog', 'FCompStyle', 'Integer', @_LapeCommonDialog_FCompStyle_Read, @_LapeCommonDialog_FCompStyle_Write);
addGlobalFunc('function TCommonDialog.Execute: boolean; constref', @_LapeCommonDialog_Execute);
addClassVar('TCommonDialog', 'Handle', 'THandle', @_LapeCommonDialog_Handle_Read, @_LapeCommonDialog_Handle_Write);
addClassVar('TCommonDialog', 'UserChoice', 'integer', @_LapeCommonDialog_UserChoice_Read, @_LapeCommonDialog_UserChoice_Write);
addGlobalFunc('procedure TCommonDialog.Close; constref', @_LapeCommonDialog_Close);
addGlobalFunc('procedure TCommonDialog.DoShow; constref', @_LapeCommonDialog_DoShow);
addGlobalFunc('procedure TCommonDialog.DoCanClose(var CanClose: Boolean); constref', @_LapeCommonDialog_DoCanClose);
addGlobalFunc('procedure TCommonDialog.DoClose; constref', @_LapeCommonDialog_DoClose);
addGlobalFunc('function TCommonDialog.HandleAllocated: boolean; constref', @_LapeCommonDialog_HandleAllocated);
addClassVar('TCommonDialog', 'OnClose', 'TNotifyEvent', @_LapeCommonDialog_OnClose_Read, @_LapeCommonDialog_OnClose_Write);
addClassVar('TCommonDialog', 'OnCanClose', 'TCloseQueryEvent', @_LapeCommonDialog_OnCanClose_Read, @_LapeCommonDialog_OnCanClose_Write);
addClassVar('TCommonDialog', 'OnShow', 'TNotifyEvent', @_LapeCommonDialog_OnShow_Read, @_LapeCommonDialog_OnShow_Write);
addClassVar('TCommonDialog', 'Width', 'integer', @_LapeCommonDialog_Width_Read, @_LapeCommonDialog_Width_Write);
addClassVar('TCommonDialog', 'Height', 'integer', @_LapeCommonDialog_Height_Read, @_LapeCommonDialog_Height_Write);
addGlobalFunc('procedure TCommonDialog.Init(AOwner: TComponent)', @_LapeCommonDialog_Init);
//addGlobalFunc('procedure TCommonDialog.Free; constref', @_LapeCommonDialog_Free);

AddClass('TFileDialog', 'TCommonDialog');
addGlobalFunc('procedure TFileDialog.DoTypeChange; constref', @_LapeFileDialog_DoTypeChange);
//addGlobalFunc('function TFileDialog.Execute: boolean; constref', @_LapeFileDialog_Execute);
addClassVar('TFileDialog', 'Files', 'TStrings', @_LapeFileDialog_Files_Read, nil);
addClassVar('TFileDialog', 'HistoryList', 'TStrings', @_LapeFileDialog_HistoryList_Read, @_LapeFileDialog_HistoryList_Write);
addGlobalFunc('procedure TFileDialog.IntfFileTypeChanged(NewFilterIndex: Integer); constref', @_LapeFileDialog_IntfFileTypeChanged);
addClassVar('TFileDialog', 'DefaultExt', 'string', @_LapeFileDialog_DefaultExt_Read, @_LapeFileDialog_DefaultExt_Write);
addClassVar('TFileDialog', 'FileName', 'String', @_LapeFileDialog_FileName_Read, @_LapeFileDialog_FileName_Write);
addClassVar('TFileDialog', 'Filter', 'String', @_LapeFileDialog_Filter_Read, @_LapeFileDialog_Filter_Write);
addClassVar('TFileDialog', 'FilterIndex', 'Integer', @_LapeFileDialog_FilterIndex_Read, @_LapeFileDialog_FilterIndex_Write);
addClassVar('TFileDialog', 'InitialDir', 'string', @_LapeFileDialog_InitialDir_Read, @_LapeFileDialog_InitialDir_Write);
addClassVar('TFileDialog', 'OnHelpClicked', 'TNotifyEvent', @_LapeFileDialog_OnHelpClicked_Read, @_LapeFileDialog_OnHelpClicked_Write);
addClassVar('TFileDialog', 'OnTypeChange', 'TNotifyEvent', @_LapeFileDialog_OnTypeChange_Read, @_LapeFileDialog_OnTypeChange_Write);
addGlobalFunc('procedure TFileDialog.Init(AOwner: TComponent); override', @_LapeFileDialog_Init);
//addGlobalFunc('procedure TFileDialog.Free; constref', @_LapeFileDialog_Free);

addClass('TOpenDialog', 'TFileDialog');
addGlobalFunc('procedure TOpenDialog.DoFolderChange; constref', @_LapeOpenDialog_DoFolderChange);
addGlobalFunc('procedure TOpenDialog.DoSelectionChange; constref', @_LapeOpenDialog_DoSelectionChange);
addClassVar('TOpenDialog', 'Options', 'TOpenOptions', @_LapeOpenDialog_Options_Read, @_LapeOpenDialog_Options_Write);
addClassVar('TOpenDialog', 'OnFolderChange', 'TNotifyEvent', @_LapeOpenDialog_OnFolderChange_Read, @_LapeOpenDialog_OnFolderChange_Write);
addClassVar('TOpenDialog', 'OnSelectionChange', 'TNotifyEvent', @_LapeOpenDialog_OnSelectionChange_Read, @_LapeOpenDialog_OnSelectionChange_Write);
addGlobalFunc('procedure TOpenDialog.Init(TheOwner: TComponent); override', @_LapeOpenDialog_Init);
//addGlobalFunc('procedure TOpenDialog.Free; constref', @_LapeOpenDialog_Free);

addClass('TColorDialog', 'TCommonDialog');
addClassVar('TColorDialog', 'Color', 'TColor', @_LapeColorDialog_Color_Read, @_LapeColorDialog_Color_Write);
addClassVar('TColorDialog', 'CustomColors', 'TStrings', @_LapeColorDialog_CustomColors_Read, @_LapeColorDialog_CustomColors_Write);
addGlobalFunc('procedure TColorDialog.Init(TheOwner: TComponent); override', @_LapeColorDialog_Init);
//addGlobalFunc('procedure TColorDialog.Free; constref', @_LapeColorDialog_Free);

popSection();
