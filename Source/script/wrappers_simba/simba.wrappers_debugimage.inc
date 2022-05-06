procedure _LapeShowBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('ShowBitmap requires Simba communication');

  SimbaScriptThread.Script.SimbaCommunication.DebugImage_Show(PMufasaBitmap(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure _LapeShowBitmapEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('ShowBitmap requires Simba communication');

  with SimbaScriptThread.Script.Client do
    SimbaScriptThread.Script.SimbaCommunication.DebugImage_Show(MBitmaps[PInteger(Params^[0])^], PBoolean(Params^[1])^);
end;

procedure _LapeDrawBitmapDebugImg(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('DrawBitmapDebugImg requires Simba communication');

  SimbaScriptThread.Script.SimbaCommunication.DebugImage_Draw(PMufasaBitmap(Params^[0])^);
end;

procedure _LapeDrawBitmapDebugImgEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('DrawBitmapDebugImg requires Simba communication');

  with SimbaScriptThread.Script.Client do
    SimbaScriptThread.Script.SimbaCommunication.DebugImage_Draw(MBitmaps[PInteger(Params^[0])^]);
end;

procedure _LapeDisplayDebugImgWindow(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('DisplayDebugImgWindow requires Simba communication');

  with SimbaScriptThread.Script.Client do
    SimbaScriptThread.Script.SimbaCommunication.DebugImage_Display(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeClearDebugImg(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('ClearDebugImage requires Simba communication');

  with SimbaScriptThread.Script.Client do
    SimbaScriptThread.Script.SimbaCommunication.DebugImage_Clear();
end;

procedure _LapeSetDebugImgMaxSize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('SetDebugImgMaxSize requires Simba communication');

  with SimbaScriptThread.Script.Client do
    SimbaScriptThread.Script.SimbaCommunication.DebugImage_SetMaxSize(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeHideDebugImg(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('HideDebugImg requires Simba communication');

  with SimbaScriptThread.Script.Client do
    SimbaScriptThread.Script.SimbaCommunication.DebugImage_Hide();
end;
