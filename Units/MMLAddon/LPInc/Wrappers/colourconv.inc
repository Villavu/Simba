procedure Lape_ColorToRGB(const Params: PParamArray);
begin
  ps_ColorToRGB(Pinteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;
