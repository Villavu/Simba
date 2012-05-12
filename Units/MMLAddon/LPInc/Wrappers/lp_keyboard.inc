procedure Lape_KeyDown(const Params: PParamArray); lape_extdecl
begin
  ps_KeyDown(PWord(Params^[0])^);
end;

procedure Lape_KeyUp(const Params: PParamArray); lape_extdecl
begin
  ps_KeyUp(PWord(Params^[0])^);
end;

procedure Lape_SendKeys(const Params: PParamArray); lape_extdecl
begin
  ps_SendKeys(Pstring(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure Lape_PressKey(const Params: PParamArray); lape_extdecl
begin
  ps_PressKey(PWord(Params^[0])^);
end;

procedure Lape_isKeyDown(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := ps_isKeyDown(PWord(Params^[0])^);
end;

procedure Lape_GetKeyCode(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := ps_GetKeyCode(Pchar(Params^[0])^);
end;
