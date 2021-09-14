procedure _LapeMoveMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.MoveMouse(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeScrollMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.ScrollMouse(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeGetMousePos(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.GetMousePos(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeHoldMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.HoldMouse(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeReleaseMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.ReleaseMouse(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeClickMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.ClickMouse(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeIsMouseButtonDown(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := SimbaScript.Client.IOManager.IsMouseButtonDown(PInteger(Params^[0])^);
end;

procedure _LapeKeyDown(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.KeyDown(PUInt16(Params^[0])^);
end;

procedure _LapeKeyUp(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.KeyUp(PUInt16(Params^[0])^);
end;

procedure _LapeSendKeys(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.SendText(PString(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapePressKey(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.PressKey(PUInt16(Params^[0])^);
end;

procedure _LapeisKeyDown(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := SimbaScript.Client.IOManager.isKeyDown(PUInt16(Params^[0])^);
end;

procedure _LapeGetKeyCode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := SimbaScript.Client.IOManager.GetKeyCode(PChar(Params^[0])^);
end;

