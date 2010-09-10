procedure RUTIS_ps_rs_GetUpText(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PString(Result^.Data)^ := ps_rs_GetUpText();
end;

procedure RUTIS_ps_rs_GetUpTextAt(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pstring(Result^.Data)^ := ps_rs_GetUpTextAt(Pinteger(Params^[0].Data)^, Pinteger(Params^[1].Data)^);
end;

procedure RUTIS_ps_BitmapFromText(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pinteger(Result^.Data)^ := ps_BitmapFromText(PString(Params^[0].Data)^, PString(Params^[1].Data)^);
end;
