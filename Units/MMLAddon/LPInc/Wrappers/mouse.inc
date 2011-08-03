procedure Lape_MoveMouse(const Params: PParamArray);
begin
  ps_MoveMouse(Pinteger(Params^[0])^, Pinteger(Params^[1])^);
end;

procedure Lape_ScrollMouse(const Params: PParamArray);
begin
  ps_ScrollMouse(Pinteger(Params^[0])^, Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure Lape_GetMousePos(const Params: PParamArray);
begin
  ps_GetMousePos(Pinteger(Params^[0])^, Pinteger(Params^[1])^);
end;

procedure Lape_(const Params: PParamArray; const Result: Pointer);
begin
  PClickType(Result)^ := ConvIntClickType(PInteger(Params^[0])^);
end;

procedure Lape_HoldMouse(const Params: PParamArray);
begin
  ps_HoldMouse(Pinteger(Params^[0])^, Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure Lape_ReleaseMouse(const Params: PParamArray);
begin
  ps_ReleaseMouse(Pinteger(Params^[0])^, Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure Lape_ClickMouse(const Params: PParamArray);
begin
  ps_ClickMouse(Pinteger(Params^[0])^, Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure Lape_IsMouseButtonDown(const Params: PParamArray; const Result: Pointer);
begin
  Pboolean(Result)^ := ps_IsMouseButtonDown(Pinteger(Params^[0])^);
end;
