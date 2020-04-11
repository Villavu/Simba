unit simbascript.import_dialog;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_Dialogs(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  dialogs, controls,
  simba.aca, simba.aca_math, simba.dtmeditor;

type
  TInputQuery = class
    Params: PParamArray;
    Return: Pointer;

    procedure Execute;
  end;

procedure TInputQuery.Execute;
begin
  PBoolean(Return)^ := InputQuery(PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^);
end;

// function InputQuery(const ACaption, APrompt: String; var Value: String): Boolean
procedure Lape_InputQuery(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  InputQuery: TInputQuery;
begin
  InputQuery := TInputQuery.Create();
  InputQuery.Params := Params;
  InputQuery.Return := Result;

  TThread.Synchronize(nil, @InputQuery.Execute);

  InputQuery.Free();
end;

type
  TMessageDialog = class
    Params: PParamArray;
    Result: Pointer;

    procedure Execute;
  end;

procedure TMessageDialog.Execute;
type
  PMsgDlgType = ^TMsgDlgType;
  PMsgDlgButtons = ^TMsgDlgButtons;
begin
  PInt32(Result)^ := MessageDlg(PString(Params^[1])^, PString(Params^[2])^, PMsgDlgType(Params^[3])^, PMsgDlgButtons(Params^[4])^, '');
end;

// function MessageDlg(const Caption, Message: string; DialogType: TMsgDlgType; Buttons: TMsgDlgButtons): Int32
procedure Lape_MessageDlg(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  MessageDialog: TMessageDialog;
begin
  MessageDialog := TMessageDialog.Create();
  MessageDialog.Params := Params;
  MessageDialog.Result := Result;

  TThread.Synchronize(nil, @MessageDialog.Execute);

  MessageDialog.Free();
end;

type
  TShowMessage = class
    Params: PParamArray;

    procedure Execute;
  end;

procedure TShowMessage.Execute;
begin
  ShowMessage(PString(Params^[1])^);
end;

// procedure ShowMessage(const Message: String);
procedure Lape_ShowMessage(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  ShowMessage: TShowMessage;
begin
  ShowMessage := TShowMessage.Create();
  ShowMessage.Params := Params;

  TThread.Synchronize(nil, @ShowMessage.Execute);

  ShowMessage.Free();
end;

type
  TACA = class
    Params: PParamArray;

    procedure CalculateBestColor(CTS, Color, Tolerance: Int32; Hue, Sat: Extended);
    procedure Execute;
  end;

procedure TACA.CalculateBestColor(CTS, Color, Tolerance: Int32; Hue, Sat: Extended);
begin
  PInt32(Params^[2])^ := CTS;
  PInt32(Params^[3])^ := Color;
  PInt32(Params^[4])^ := Tolerance;

  PExtended(Params^[5])^ := Hue;
  PExtended(Params^[6])^ := Sat;
end;

procedure TACA.Execute;
begin
  with TSimbaACAForm.Create(TSimbaScript(Params^[0]).Client.IOManager.GetImageTarget().Handle) do
  begin
    OnCalculateBestColor := @CalculateBestColor;
    if (PString(Params^[1])^ <> '') then
      Caption := 'Auto Color Aid - ' + PString(Params^[1])^;
    ShowModal();
  end;
end;

procedure Lape_ACA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  ACA: TACA;
begin
  ACA := TACA.Create();
  ACA.Params := Params;

  TThread.Synchronize(nil, @ACA.Execute);

  ACA.Free();
end;

procedure Lape_ACAEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  case PInt32(Params^[2])^ of
    0: BestColor_CTS0(PIntegerArray(Params^[1])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
    1: BestColor_CTS1(PIntegerArray(Params^[1])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
    2: BestColor_CTS2(PIntegerArray(Params^[1])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PExtended(Params^[5])^, PExtended(Params^[6])^);
  end;
end;

type
  TDTMEditor = class
    Params: PParamArray;

    procedure GetResult(constref DTM: String);
    procedure Execute;
  end;

procedure TDTMEditor.GetResult(constref DTM: String);
begin
  PString(Params^[2])^ := DTM;
end;

procedure TDTMEditor.Execute;
begin
  with TSimbaDTMEditorForm.Create(TSimbaScript(Params^[0]).Client.IOManager.GetImageTarget().Handle) do
  begin
    OnPrintDTM := @GetResult;
    if (PString(Params^[1])^ <> '') then
      Caption := 'DTM Editor - ' + PString(Params^[1])^;
    ShowModal();
  end;
end;

procedure Lape_DTMEditor(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  DTMEditor: TDTMEditor;
begin
  DTMEditor := TDTMEditor.Create();
  DTMEditor.Params := Params;

  TThread.Synchronize(nil, @DTMEditor.Execute);

  DTMEditor.Free();
end;

procedure Lape_Import_Dialogs(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    Section := 'Dialogs';

    addGlobalConst(mrNone, 'mrNone');
    addGlobalConst(mrOK, 'mrOK');
    addGlobalConst(mrCancel, 'mrCancel');
    addGlobalConst(mrAbort, 'mrAbort');
    addGlobalConst(mrRetry, 'mrRetry');
    addGlobalConst(mrIgnore,'mrIgnore');
    addGlobalConst(mrYes, 'mrYes');
    addGlobalConst(mrNo, 'mrNo');
    addGlobalConst(mrAll,'mrAll');
    addGlobalConst(mrNoToAll, 'mrNoToAll');
    addGlobalConst(mrYesToAll, 'mrYesToAll');
    addGlobalConst(mrClose, 'mrClose');

    addGlobalType('set of (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore, mbAll, mbNoToAll, mbYesToAll, mbHelp, mbClose)', 'TMsgDlgButtons');
    addGlobalType('(mtWarning, mtError, mtInformation, mtConfirmation, mtCustom)', 'TMsgDlgType');

    addGlobalMethod('procedure ShowMessage(const Message: String);', @Lape_ShowMessage, Data);
    addGlobalMethod('function InputQuery(const Caption, Prompt: String; var Value: String): Boolean;', @Lape_InputQuery, Data);
    addGlobalMethod('function MessageDlg(const Caption, Message: string; DialogType: TMsgDlgType; Buttons: TMsgDlgButtons): Int32;', @Lape_MessageDlg, Data);

    addGlobalMethod('procedure DTMEditor(Title: String; out DTM: String);', @Lape_DTMEditor, Data);
    addGlobalMethod('procedure ACAGUI(Title: String; out CTS, Color, Tolerance: Int32; out Hue, Sat: Extended);', @Lape_ACA, Data);
    addGlobalMethod('procedure ACA(Colors: TIntegerArray; CTS: Int32; out Color, Tolerance: Int32; out Hue, Sat: Extended);', @Lape_ACAEx, Data);
  end;
end;

end.


