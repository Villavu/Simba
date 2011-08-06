procedure Lape_Writeln(const Params: PParamArray);
begin
  ps_Writeln(Pstring(Params^[0])^);
end;

procedure Lape_SetScriptProp(const Params: PParamArray; const Result: Pointer);
begin
  Pboolean(Result)^ := ps_SetScriptProp(PSP_Property(Params^[0])^, PVariantArray(Params^[1])^);
end;

procedure Lape_GetScriptProp(const Params: PParamArray; const Result: Pointer);
begin
  Pboolean(Result)^ := ps_GetScriptProp(PSP_Property(Params^[0])^, PVariantArray(Params^[1])^);
end;

procedure Lape_Wait(const Params: PParamArray);
begin
  ps_Wait(PDWord(Params^[0])^);
end;

procedure Lape_PlaySound(const Params: PParamArray);
begin
  ps_PlaySound(Pstring(Params^[0])^);
end;

procedure Lape_StopSound(const Params: PParamArray);
begin
  ps_StopSound();
end;

procedure Lape_ClearDebug(const Params: PParamArray);
begin
  ps_ClearDebug();
end;

procedure Lape_SetSupressExceptions(const Params: PParamArray);
begin
  ps_SetSupressExceptions(Pboolean(Params^[0])^);
end;

procedure Lape_SaveScreenshot(const Params: PParamArray);
begin
  ps_SaveScreenshot(Pstring(Params^[0])^);
end;

procedure Lape_DisplayDebugImgWindow(const Params: PParamArray);
begin
  ps_DisplayDebugImgWindow(Pinteger(Params^[0])^, Pinteger(Params^[1])^);
end;

procedure Lape_DrawBitmapDebugImg(const Params: PParamArray);
begin
  ps_DrawBitmapDebugImg(Pinteger(Params^[0])^);
end;

procedure Lape_GetDebugBitmap(const Params: PParamArray; const Result: Pointer);
begin
  Pinteger(Result)^ := ps_GetDebugBitmap();
end;

procedure Lape_ClearDebugImg(const Params: PParamArray);
begin
  ps_ClearDebugImg();
end;

procedure Lape_Status(const Params: PParamArray);
begin
  ps_Status(Pstring(Params^[0])^);
end;

procedure Lape_Disguise(const Params: PParamArray);
begin
  ps_Disguise(Pstring(Params^[0])^);
end;

procedure Lape_ShowMessage(const Params: PParamArray);
begin
  ps_ShowMessage(Pstring(Params^[0])^);
end;

procedure Lape_MessageBox(const Params: PParamArray; const Result: Pointer);
begin
  PInteger(Result)^ := ps_MessageBox(Pstring(Params^[0])^, Pstring(Params^[1])^, PLongInt(Params^[2])^);
end;

//procedure Lape_MessageDlg(const Params: PParamArray; const Result: Pointer);
//begin
//  Pinteger(Result)^ := ps_MessageDlg(Pstring(Params^[0])^, Pstring(Params^[1])^, PMsgDlgType(Params^[2])^, PMsgDlgButtons(Params^[3])^);
//end;

procedure Lape_InputQuery(const Params: PParamArray; const Result: Pointer);
begin
  PBoolean(Result)^ := ps_InputQuery(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_TerminateScript(const Params: PParamArray);
begin
  ps_TerminateScript();
end;

procedure Lape_GetTimeRunning(const Params: PParamArray; const Result: Pointer);
begin
  PLongWord(Result)^ := ps_GetTimeRunning();
end;

//procedure Lape_GetTClient(const Params: PParamArray; const Result: Pointer);
//begin
//  PClient(Result)^ := ps_GetTClient();
//end;

procedure Lape_ConvertTime(const Params: PParamArray);
begin
  ps_ConvertTime(Pinteger(Params^[0])^, Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^);
end;

procedure Lape_DecodeDate(const Params: PParamArray);
begin
  ps_DecodeDate(PDateTime(Params^[0])^, PWord(Params^[1])^, PWord(Params^[2])^, PWord(Params^[3])^);
end;

procedure Lape_DecodeTime(const Params: PParamArray);
begin
  ps_DecodeTime(PDateTime(Params^[0])^, Pword(Params^[1])^, Pword(Params^[2])^, Pword(Params^[3])^, Pword(Params^[4])^);
end;

procedure Lape_Now(const Params: PParamArray; const Result: Pointer);
begin
  PDateTime(Result)^ := ps_Now();
end;

procedure Lape_Date(const Params: PParamArray; const Result: Pointer);
begin
  PDateTime(Result)^ := ps_Date();
end;

procedure Lape_GetTickCount(const Params: PParamArray; const Result: Pointer);
begin
  PLongword(Result)^ := ps_GetTickCount();
end;

procedure Lape_HakunaMatata(const Params: PParamArray);
begin
  ps_HakunaMatata();
end;

procedure Lape_Simba(const Params: PParamArray);
begin
  ps_Simba();
end;

procedure Lape_SetClipBoard(const Params: PParamArray);
begin
  ps_SetClipBoard(Pstring(Params^[0])^);
end;

procedure Lape_GetClipBoard(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ps_GetClipBoard();
end;

//procedure Lape_GetProcesses(const Params: PParamArray; const Result: Pointer);
//begin
//  PSysProcArr(Result)^ := ps_GetProcesses();
//end;
//
//procedure Lape_SetTarget(const Params: PParamArray);
//begin
//  ps_SetTarget(PSysProc(Params^[0])^);
//end;
