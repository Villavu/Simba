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
addGlobalFunc('procedure ShowMessage(const Message: String)', @_LapeShowMessage);
addGlobalFunc('function InputQuery(const Caption, Prompt: String; var Value: String): Boolean', @_LapeInputQuery);
addGlobalFunc('function InputCombo(const Caption, Prompt: string; const List: TStringArray): Integer', @_LapeInputCombo);
addGlobalFunc('function MessageDlg(const Caption, Message: string; DialogType: TMsgDlgType; Buttons: TMsgDlgButtons): Integer', @_LapeMessageDlg);
addGlobalFunc('procedure DTMEditor(Title: String; out DTM: String)', @_LapeDTMEditor);
addGlobalFunc('procedure ACAGUI(Title: String; out CTS, Color, Tolerance: Integer; out Hue, Sat: Extended)', @_LapeACA);
addGlobalFunc('procedure ACA(Colors: TIntegerArray; CTS: Integer; out Color, Tolerance: Integer; out Hue, Sat: Extended)', @_LapeACAEx);

popSection();
