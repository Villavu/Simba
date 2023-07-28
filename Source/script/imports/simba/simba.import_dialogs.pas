unit simba.import_dialogs;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.script_compiler;

procedure ImportDialogs(Compiler: TSimbaScript_Compiler);

implementation

uses
  dialogs, controls, lptypes,
  simba.scriptthread, simba.aca, simba.dtmeditor,
  simba.dialog, simba.threading, simba.target, simba.finder;

procedure _LapeInputCombo(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV

  procedure Execute;
  begin
    PInteger(Result)^ := InputCombo(PString(Params^[0])^, PString(Params^[1])^, PStringArray(Params^[2])^);
  end;

begin
  ExecuteOnMainThread(@Execute);
end;

procedure _LapeInputQuery(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV

  procedure Execute;
  begin
    PBoolean(Result)^ := InputQuery(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
  end;

begin
  ExecuteOnMainThread(@Execute);
end;

procedure _LapeShowMessage(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV

  procedure Execute;
  begin
    ShowMessage(PString(Params^[0])^);
  end;

begin
  ExecuteOnMainThread(@Execute);
end;

procedure _LapeShowDTMEditor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV

  procedure Execute;
  var
    Window: TWindowHandle;
  begin
    with PSimbaTarget(Params^[0])^ do
      Window := GetWindowTarget();

    with TSimbaDTMEditorForm.Create(Window) do
    try
      FreeOnClose := False;
      Caption := PString(Params^[1])^;
      ShowModal();

      PString(Result)^ := DTMString;
    finally
      Free();
    end;
  end;

begin
  ExecuteOnMainThread(@Execute);
end;

procedure _LapeShowACA(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV

  procedure Execute;
  var
    Window: TWindowHandle;
  begin
    with PSimbaTarget(Params^[0])^ do
      Window := GetWindowTarget();

    with TSimbaACAForm.Create(Window) do
    try
      FreeOnClose := False;
      Caption := PString(Params^[1])^;
      ShowModal();

      PColorTolerance(Result)^ := BestColor;
    finally
      Free();
    end;
  end;

begin
  ExecuteOnMainThread(@Execute);
end;

procedure _LapeSelectDirectory(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV

  procedure Execute;
  begin
    PBoolean(Result)^ := SelectDirectory(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
  end;

begin
  ExecuteOnMainThread(@Execute);
end;

procedure _LapeShowQuestionDialog(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV

  procedure Execute;
  begin
    PSimbaDialogResult(Result)^ := SimbaQuestionDlg(PString(Params^[0])^, PString(Params^[1])^);
  end;

begin
  ExecuteOnMainThread(@Execute);
end;

procedure _LapeShowTrayNotification(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('ShowTrayNotification requires Simba communication');

  SimbaScriptThread.Script.SimbaCommunication.ShowTrayNotification(PString(Params^[0])^, PString(Params^[1])^, PInteger(Params^[2])^);
end;

procedure ImportDialogs(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Dialogs';

    addGlobalType('enum(CANCEL, YES, NO)', 'ESimbaDialogResult');

    addGlobalFunc('function ShowDirectoryDialog(Title, InitialDirectory: String; out Directory: String): Boolean;', @_LapeSelectDirectory);
    addGlobalFunc('function ShowQueryDialog(Caption, Prompt: String; var Value: String): Boolean', @_LapeInputQuery);
    addGlobalFunc('function ShowComboDialog(Caption, Prompt: string; List: TStringArray): Integer', @_LapeInputCombo);
    addGlobalFunc('procedure ShowMessage(Message: String)', @_LapeShowMessage);
    addGlobalFunc('procedure ShowTrayNotification(Title, Message: String; Timeout: Integer = 3000)', @_LapeShowTrayNotification);
    addGlobalFunc('function ShowQuestionDialog(Title, Question: String): ESimbaDialogResult', @_LapeShowQuestionDialog);
    addGlobalFunc('function ShowDTMEditor(Target: TSimbaTarget; Title: String): String', @_LapeShowDTMEditor);
    addGlobalFunc('function ShowACA(Target: TSimbaTarget; Title: String): TColorTolerance', @_LapeShowACA);

    ImportingSection := '';
  end;
end;

end.

