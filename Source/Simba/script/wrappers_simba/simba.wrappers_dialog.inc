type
  TInputCombo = object
    Params: PParamArray;
    Result: Pointer;

    procedure Execute;
  end;

procedure TInputCombo.Execute;
begin
  PInteger(Result)^ := InputCombo(PString(Params^[0])^, PString(Params^[1])^, PStringArray(Params^[2])^);
end;

procedure _LapeInputCombo(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  InputCombo: TInputCombo;
begin
  InputCombo.Params := Params;
  InputCombo.Result := Result;

  TThread.Synchronize(nil, @InputCombo.Execute);
end;

type
  TInputQuery = object
    Params: PParamArray;
    Result: Pointer;

    procedure Execute;
  end;

procedure TInputQuery.Execute;
begin
  PBoolean(Result)^ := InputQuery(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeInputQuery(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  InputQuery: TInputQuery;
begin
  InputQuery.Params := Params;
  InputQuery.Result := Result;

  TThread.Synchronize(nil, @InputQuery.Execute);
end;

type
  TMessageDialog = object
    Params: PParamArray;
    Result: Pointer;

    procedure Execute;
  end;

procedure TMessageDialog.Execute;
type
  PMsgDlgType = ^TMsgDlgType;
  PMsgDlgButtons = ^TMsgDlgButtons;
begin
  PInt32(Result)^ := MessageDlg(PString(Params^[0])^, PString(Params^[1])^, PMsgDlgType(Params^[2])^, PMsgDlgButtons(Params^[3])^, '');
end;

procedure _LapeMessageDlg(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  MessageDialog: TMessageDialog;
begin
  MessageDialog.Params := Params;
  MessageDialog.Result := Result;

  TThread.Synchronize(nil, @MessageDialog.Execute);
end;

type
  TShowMessage = object
    Params: PParamArray;

    procedure Execute;
  end;

procedure TShowMessage.Execute;
begin
  ShowMessage(PString(Params^[0])^);
end;

procedure _LapeShowMessage(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  ShowMessage: TShowMessage;
begin
  ShowMessage.Params := Params;

  TThread.Synchronize(nil, @ShowMessage.Execute);
end;

type
  TACA = object
    Params: PParamArray;

    procedure CalculateBestColor(CTS, Color, Tolerance: Int32; Hue, Sat: Extended);
    procedure Execute;
  end;

procedure TACA.CalculateBestColor(CTS, Color, Tolerance: Int32; Hue, Sat: Extended);
begin
  PInt32(Params^[1])^ := CTS;
  PInt32(Params^[2])^ := Color;
  PInt32(Params^[3])^ := Tolerance;

  PExtended(Params^[4])^ := Hue;
  PExtended(Params^[5])^ := Sat;
end;

procedure TACA.Execute;
begin
  with TSimbaACAForm.Create(SimbaScript.Client.IOManager.GetImageTarget().Handle) do
  begin
    OnCalculateBestColor := @CalculateBestColor;
    if (PString(Params^[0])^ <> '') then
      Caption := 'Auto Color Aid - ' + PString(Params^[0])^;
    ShowModal();
  end;
end;

procedure _LapeACA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  ACA: TACA;
begin
  ACA.Params := Params;

  TThread.Synchronize(nil, @ACA.Execute);
end;

procedure _LapeACAEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  case PInt32(Params^[1])^ of
    0: BestColor_CTS0(PIntegerArray(Params^[0])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
    1: BestColor_CTS1(PIntegerArray(Params^[0])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
    2: BestColor_CTS2(PIntegerArray(Params^[0])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
  end;
end;

type
  TDTMEditor = object
    Params: PParamArray;

    procedure GetResult(DTM: String);
    procedure Execute;
  end;

procedure TDTMEditor.GetResult(DTM: String);
begin
  PString(Params^[1])^ := DTM;
end;

procedure TDTMEditor.Execute;
begin
  with TSimbaDTMEditorForm.Create(SimbaScript.Client.IOManager.GetImageTarget().Handle) do
  begin
    OnPrintDTM := @GetResult;
    if (PString(Params^[0])^ <> '') then
      Caption := 'DTM Editor - ' + PString(Params^[0])^;
    ShowModal();
  end;
end;

procedure _LapeDTMEditor(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  DTMEditor: TDTMEditor;
begin
  DTMEditor.Params := Params;

  TThread.Synchronize(nil, @DTMEditor.Execute);
end;
