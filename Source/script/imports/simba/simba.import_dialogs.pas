unit simba.import_dialogs;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, dialogs, controls, lptypes,
  simba.script_compiler, simba.mufasatypes, simba.scriptthread, simba.aca, simba.dtmeditor;

procedure _LapeInputCombo(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  procedure Execute;
  begin
    PInteger(Result)^ := InputCombo(PString(Params^[0])^, PString(Params^[1])^, PStringArray(Params^[2])^);
  end;

begin
  Sync(@Execute);
end;

procedure _LapeInputQuery(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  procedure Execute;
  begin
    PBoolean(Result)^ := InputQuery(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
  end;

begin
  Sync(@Execute);
end;

procedure _LapeShowDialog(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  procedure Execute;
  begin
    PInteger(Result)^ := MessageDlg(PString(Params^[0])^, PString(Params^[1])^, TMsgDlgType(Params^[2]^), TMsgDlgButtons(Params^[3]^), '');
  end;

begin
  Sync(@Execute);
end;

procedure _LapeShowMessage(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  procedure Execute;
  begin
    ShowMessage(PString(Params^[0])^);
  end;

begin
  Sync(@Execute);
end;

procedure _LapeShowDTMEditor(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  procedure PrintDTM(DTM: String);
  begin
    PString(Params^[1])^ := DTM;
  end;

  procedure Execute;
  begin
    with TSimbaDTMEditorForm.Create(SimbaScriptThread.Script.Client, False) do
    begin
      OnPrintDTMEx := @PrintDTM;
      if (PString(Params^[0])^ <> '') then
        Caption := PString(Params^[0])^;

      ShowModal();
    end;
  end;

begin
  Sync(@Execute);
end;

procedure _LapeShowACA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  procedure CalculateBestColor(CTS, Color, Tolerance: Int32; Hue, Sat: Extended);
  begin
    PInteger(Params^[1])^ := CTS;
    PInteger(Params^[2])^ := Color;
    PInteger(Params^[3])^ := Tolerance;

    PExtended(Params^[4])^ := Hue;
    PExtended(Params^[5])^ := Sat;
  end;

  procedure Execute;
  begin
    with TSimbaACAForm.Create(SimbaScriptThread.Script.Client, False) do
    begin
      OnCalculateBestColorEx := @CalculateBestColor;
      if (PString(Params^[0])^ <> '') then
        Caption := PString(Params^[0])^;

      ShowModal();
    end;
  end;

begin
  Sync(@Execute);
end;

procedure _LapeSelectDirectory(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  procedure Execute;
  begin
    PBoolean(Result)^ := SelectDirectory(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
  end;

begin
  Sync(@Execute);
end;

procedure ImportDialogs(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
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
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportDialogs);

end.

