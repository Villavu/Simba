procedure Lape_ColorToRGB(const Params: PParamArray);
begin
  ps_ColorToRGB(Pinteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure Lape_RGBtoColor(const Params: PParamArray; const Result: Pointer);
begin
  PColor(Result)^ := ps_RGBtoColor(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure Lape_ColorToHSL(const Params: PParamArray);
begin
  ps_ColorToHSL(PInteger(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^);
end;

procedure Lape_HSLToColor(const Params: PParamArray; const Result: Pointer);
begin
  PColor(Result)^ := ps_HSLToColor(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^);
end;

procedure Lape_ColorToXYZ(const Params: PParamArray);
begin
  ps_ColorToXYZ(PInteger(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^);
end;

procedure Lape_XYZToColor(const Params: PParamArray; const Result: Pointer);
begin
  PColor(Result)^ := ps_XYZToColor(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^);
end;

procedure Lape_RGBToHSL(const Params: PParamArray);
begin
  ps_RGBToHSL(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure Lape_HSLtoRGB(const Params: PParamArray);
begin
  ps_HSLtoRGB(Pextended(Params^[0])^, Pextended(Params^[1])^, Pextended(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^);
end;

procedure Lape_RGBToXYZ(const Params: PParamArray);
begin
  ps_RGBToXYZ(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure Lape_XYZToRGB(const Params: PParamArray);
begin
  ps_XYZToRGB(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^);
end;
