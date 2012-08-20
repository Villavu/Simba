procedure Lape_MoveMouse(const Params: PParamArray); lape_extdecl
begin
  ps_MoveMouse(Pinteger(Params^[0])^, Pinteger(Params^[1])^);
end;

procedure Lape_ScrollMouse(const Params: PParamArray); lape_extdecl
begin
  ps_ScrollMouse(Pinteger(Params^[0])^, Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure Lape_GetMousePos(const Params: PParamArray); lape_extdecl
begin
  ps_GetMousePos(Pinteger(Params^[0])^, Pinteger(Params^[1])^);
end;

procedure Lape_ConvIntClickType(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PClickType(Result)^ := ConvIntClickType(PInteger(Params^[0])^);
end;

procedure Lape_HoldMouse(const Params: PParamArray); lape_extdecl
begin
  ps_HoldMouse(Pinteger(Params^[0])^, Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure Lape_ReleaseMouse(const Params: PParamArray); lape_extdecl
begin
  ps_ReleaseMouse(Pinteger(Params^[0])^, Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure Lape_ClickMouse(const Params: PParamArray); lape_extdecl
begin
  ps_ClickMouse(Pinteger(Params^[0])^, Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure Lape_IsMouseButtonDown(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := ps_IsMouseButtonDown(Pinteger(Params^[0])^);
end;
