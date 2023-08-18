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
  simba.aca, simba.dtmeditor, simba.dialog, simba.threading, simba.target, simba.finder;

(*
Dialogs
=======
Simple dialog methods.
*)

(*
ShowComboDialog
~~~~~~~~~~~~~~~
> function ShowComboDialog(Caption, Prompt: string; List: TStringArray): Integer;
*)
procedure _LapeInputCombo(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV

  procedure Execute;
  begin
    PInteger(Result)^ := InputCombo(PString(Params^[0])^, PString(Params^[1])^, PStringArray(Params^[2])^);
  end;

begin
  ExecuteOnMainThread(@Execute);
end;

(*
ShowQueryDialog
~~~~~~~~~~~~~~~
> function ShowQueryDialog(Caption, Prompt: String; var Value: String): Boolean;
*)
procedure _LapeInputQuery(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV

  procedure Execute;
  begin
    PBoolean(Result)^ := InputQuery(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
  end;

begin
  ExecuteOnMainThread(@Execute);
end;

(*
ShowMessage
~~~~~~~~~~~
> procedure ShowMessage(Message: String);
*)
procedure _LapeShowMessage(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV

  procedure Execute;
  begin
    ShowMessage(PString(Params^[0])^);
  end;

begin
  ExecuteOnMainThread(@Execute);
end;

(*
ShowDTMEditor
~~~~~~~~~~~~~
> function ShowDTMEditor(Target: TTarget; Title: String): String;
> function ShowDTMEditor(Title: String): String;
*)
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

(*
ShowACA
~~~~~~~
> function ShowACA(Target: TTarget; Title: String): TColorTolerance;
> function ShowACA(Title: String): TColorTolerance;
*)
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

(*
ShowDirectoryDialog
~~~~~~~~~~~~~~~~~~~
> function ShowDirectoryDialog(Title, InitialDirectory: String; out Directory: String): Boolean;
*)
procedure _LapeSelectDirectory(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV

  procedure Execute;
  begin
    PBoolean(Result)^ := SelectDirectory(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
  end;

begin
  ExecuteOnMainThread(@Execute);
end;

(*
ShowQuestionDialog
~~~~~~~~~~~~~~~~~~
> function ShowQuestionDialog(Title, Question: String): Boolean;
*)
procedure _LapeShowQuestionDialog(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV

  procedure Execute;
  begin
    PBoolean(Result)^ := SimbaQuestionDlg(PString(Params^[0])^, PString(Params^[1])^) = ESimbaDialogResult.YES;
  end;

begin
  ExecuteOnMainThread(@Execute);
end;

(*
ShowTrayNotification
~~~~~~~~~~~~~~~~~~~~
> procedure ShowTrayNotification(Title, Message: String; Timeout: Integer = 3000);
*)
procedure ImportDialogs(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Dialogs';

    addGlobalFunc('function ShowDirectoryDialog(Title, InitialDirectory: String; out Directory: String): Boolean;', @_LapeSelectDirectory);
    addGlobalFunc('function ShowQueryDialog(Caption, Prompt: String; var Value: String): Boolean', @_LapeInputQuery);
    addGlobalFunc('function ShowComboDialog(Caption, Prompt: string; List: TStringArray): Integer', @_LapeInputCombo);
    addGlobalFunc('procedure ShowMessage(Message: String)', @_LapeShowMessage);
    addGlobalFunc('function ShowQuestionDialog(Title, Question: String): Boolean', @_LapeShowQuestionDialog);
    addGlobalFunc('function ShowDTMEditor(Target: TTarget; Title: String): String; overload', @_LapeShowDTMEditor);
    addGlobalFunc('function ShowACA(Target: TTarget; Title: String): TColorTolerance; overload', @_LapeShowACA);

    addGlobalFunc(
      'function ShowDTMEditor(Title: String): String; overload;', [
      'begin',
      '  Result := ShowDTMEditor(Target, Title);',
      'end;'
    ]);

    addGlobalFunc(
      'function ShowACA(Title: String): TColorTolerance; overload;', [
      'begin',
      '  Result := ShowACA(Target, Title);',
      'end;'
    ]);

    addGlobalFunc(
      'procedure ShowTrayNotification(Title, Message: String; Timeout: Integer = 3000);', [
      'begin',
      '  _SimbaScript.ShowTrayNotification(Title, Message, Timeout);',
      'end;'
    ]);

    ImportingSection := '';
  end;
end;

end.

