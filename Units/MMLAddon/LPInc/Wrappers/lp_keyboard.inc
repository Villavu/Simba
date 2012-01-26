procedure Lape_KeyDown(const Params: PParamArray);
begin
  ps_KeyDown(PWord(Params^[0])^);
end;

procedure Lape_KeyUp(const Params: PParamArray);
begin
  ps_KeyUp(PWord(Params^[0])^);
end;

procedure Lape_SendKeys(const Params: PParamArray);
begin
  ps_SendKeys(Pstring(Params^[0])^, PInteger(Params^[1])^);
end;

procedure Lape_PressKey(const Params: PParamArray);
begin
  ps_PressKey(PWord(Params^[0])^);
end;

procedure Lape_isKeyDown(const Params: PParamArray; const Result: Pointer);
begin
  Pboolean(Result)^ := ps_isKeyDown(PWord(Params^[0])^);
end;

procedure Lape_GetKeyCode(const Params: PParamArray; const Result: Pointer);
begin
  Pinteger(Result)^ := ps_GetKeyCode(Pchar(Params^[0])^);
end;
