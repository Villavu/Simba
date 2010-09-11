procedure RUTIS_ps_ColorToRGB(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_ColorToRGB(Pinteger(Params^[0].Data)^, PInteger(PPointer(Params^[1].Data)^)^, PInteger(PPointer(Params^[2].Data)^)^, PInteger(PPointer(Params^[3].Data)^)^);
end;
