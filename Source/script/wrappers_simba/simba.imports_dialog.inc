pushSection('Dialogs');

addGlobalVar(mrNone, 'mrNone').isConstant := True;
addGlobalVar(mrOK, 'mrOK').isConstant := True;
addGlobalVar(mrCancel, 'mrCancel').isConstant := True;
addGlobalVar(mrAbort, 'mrAbort').isConstant := True;
addGlobalVar(mrRetry, 'mrRetry').isConstant := True;
addGlobalVar(mrIgnore,'mrIgnore').isConstant := True;
addGlobalVar(mrYes, 'mrYes').isConstant := True;
addGlobalVar(mrNo, 'mrNo').isConstant := True;
addGlobalVar(mrAll,'mrAll').isConstant := True;
addGlobalVar(mrNoToAll, 'mrNoToAll').isConstant := True;
addGlobalVar(mrYesToAll, 'mrYesToAll').isConstant := True;
addGlobalVar(mrClose, 'mrClose').isConstant := True;
addGlobalType('set of (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore, mbAll, mbNoToAll, mbYesToAll, mbHelp, mbClose)', 'TMsgDlgButtons');
addGlobalType('(mtWarning, mtError, mtInformation, mtConfirmation, mtCustom)', 'TMsgDlgType');
addGlobalFunc('function SelectDirectory(Caption, InitialDirectory: String; out Directory: String): Boolean;', @_LapeSelectDirectory);
addGlobalFunc('function InputQuery(Caption, Prompt: String; var Value: String): Boolean', @_LapeInputQuery);
addGlobalFunc('function InputCombo(Caption, Prompt: string; List: TStringArray): Integer', @_LapeInputCombo);
addGlobalFunc('function ShowDialog(Caption, Message: string; DialogType: TMsgDlgType; Buttons: TMsgDlgButtons): Integer', @_LapeShowDialog);
addGlobalFunc('procedure ShowMessage(Message: String)', @_LapeShowMessage);
addGlobalFunc('procedure ShowDTMEditor(Title: String; out DTM: String)', @_LapeShowDTMEditor);
addGlobalFunc('procedure ShowACA(Title: String; out CTS, Color, Tolerance: Integer; out Hue, Sat: Extended)', @_LapeShowACA);

popSection();
