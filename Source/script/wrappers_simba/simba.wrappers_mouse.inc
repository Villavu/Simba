procedure _LapeMoveMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.MoveMouse(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeScrollMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.ScrollMouse(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeGetMousePos(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.GetMousePos(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeHoldMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.HoldMouse(PInteger(Params^[0])^, PInteger(Params^[1])^, PClickType(Params^[2])^);
end;

procedure _LapeReleaseMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.ReleaseMouse(PInteger(Params^[0])^, PInteger(Params^[1])^, PClickType(Params^[2])^);
end;

procedure _LapeClickMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.ClickMouse(PInteger(Params^[0])^, PInteger(Params^[1])^, PClickType(Params^[2])^);
end;

procedure _LapeIsMouseButtonDown(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := SimbaScriptThread.Script.Client.IOManager.IsMouseButtonDown(PClickType(Params^[0])^);
end;

