procedure RUTIS_ps_ColorToRGB(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_ColorToRGB(Pinteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^);
end;
