procedure RUTIS_ps_KeyDown(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_KeyDown(PWord(Params^[0].Data)^);
end;

procedure RUTIS_ps_KeyUp(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_KeyUp(PWord(Params^[0].Data)^);
end;

procedure RUTIS_ps_SendKeys(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_SendKeys(Pstring(Params^[0].Data)^);
end;

procedure RUTIS_ps_PressKey(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_PressKey(PWord(Params^[0].Data)^);
end;

procedure RUTIS_ps_isKeyDown(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pboolean(Result^.Data)^ := ps_isKeyDown(PWord(Params^[0].Data)^);
end;

procedure RUTIS_ps_GetKeyCode(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pinteger(Result^.Data)^ := ps_GetKeyCode(Pchar(Params^[0].Data)^);
end;
