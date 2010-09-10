procedure RUTIS_ps_MoveMouse(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_MoveMouse(Pinteger(Params^[0].Data)^, Pinteger(Params^[1].Data)^);
end;

procedure RUTIS_ps_ScrollMouse(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_ScrollMouse(Pinteger(Params^[0].Data)^, Pinteger(Params^[1].Data)^, Pinteger(Params^[2].Data)^);
end;

procedure RUTIS_ps_GetMousePos(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_GetMousePos(Pinteger(Params^[0].Data)^, Pinteger(Params^[1].Data)^);
end;

procedure RUTIS_ConvIntClickType(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PClickType(Result^.Data)^ := ConvIntClickType(PInteger(Params^[0].Data)^);
end;

procedure RUTIS_ps_HoldMouse(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_HoldMouse(Pinteger(Params^[0].Data)^, Pinteger(Params^[1].Data)^, Pinteger(Params^[2].Data)^);
end;

procedure RUTIS_ps_ReleaseMouse(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_ReleaseMouse(Pinteger(Params^[0].Data)^, Pinteger(Params^[1].Data)^, Pinteger(Params^[2].Data)^);
end;

procedure RUTIS_ps_ClickMouse(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_ClickMouse(Pinteger(Params^[0].Data)^, Pinteger(Params^[1].Data)^, Pinteger(Params^[2].Data)^);
end;

procedure RUTIS_ps_IsMouseButtonDown(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pboolean(Result^.Data)^ := ps_IsMouseButtonDown(Pinteger(Params^[0].Data)^);
end;
