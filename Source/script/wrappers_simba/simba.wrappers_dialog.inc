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
