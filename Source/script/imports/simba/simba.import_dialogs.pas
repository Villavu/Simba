unit simba.import_dialogs;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, dialogs, controls, lptypes,
  simba.script_compiler, simba.mufasatypes, simba.scriptthread, simba.aca, simba.dtmeditor,
  simba.dialog;

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

procedure _LapeShowQuestionDialog(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  procedure Execute;
  begin
    PSimbaDialogResult(Result)^ := SimbaQuestionDlg(PString(Params^[0])^, PString(Params^[1])^);
  end;

begin
  Sync(@Execute);
end;

procedure _LapeShowTrayNotification(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('ShowTrayNotification requires Simba communication');

  SimbaScriptThread.Script.SimbaCommunication.ShowTrayNotification(PString(Params^[0])^, PString(Params^[1])^, PInteger(Params^[2])^);
end;

procedure ImportDialogs(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('Dialogs');

    addGlobalType('enum(CANCEL, YES, NO)', 'ESimbaDialogResult');

    addGlobalFunc('function ShowDirectoryDialog(Title, InitialDirectory: String; out Directory: String): Boolean;', @_LapeSelectDirectory);
    addGlobalFunc('function ShowQueryDialog(Caption, Prompt: String; var Value: String): Boolean', @_LapeInputQuery);
    addGlobalFunc('function ShowComboDialog(Caption, Prompt: string; List: TStringArray): Integer', @_LapeInputCombo);
    addGlobalFunc('procedure ShowMessage(Message: String)', @_LapeShowMessage);
    addGlobalFunc('procedure ShowDTMEditor(Title: String; out DTM: String)', @_LapeShowDTMEditor);
    addGlobalFunc('procedure ShowACA(Title: String; out CTS, Color, Tolerance: Integer; out Hue, Sat: Extended)', @_LapeShowACA);
    addGlobalFunc('procedure ShowTrayNotification(Title, Message: String; Timeout: Integer = 3000)', @_LapeShowTrayNotification);
    addGlobalFunc('function ShowQuestionDialog(Title, Question: String): ESimbaDialogResult', @_LapeShowQuestionDialog);

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportDialogs);

end.

