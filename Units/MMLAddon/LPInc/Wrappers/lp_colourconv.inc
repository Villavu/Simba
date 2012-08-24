procedure Lape_ColorToRGB(const Params: PParamArray); lape_extdecl
begin
  ps_ColorToRGB(Pinteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure Lape_RGBtoColor(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PColor(Result)^ := ps_RGBtoColor(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure Lape_ColorToHSL(const Params: PParamArray); lape_extdecl
begin
  ps_ColorToHSL(PInteger(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^);
end;

procedure Lape_HSLToColor(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PColor(Result)^ := ps_HSLToColor(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^);
end;

procedure Lape_ColorToXYZ(const Params: PParamArray); lape_extdecl
begin
  ps_ColorToXYZ(PInteger(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^);
end;

procedure Lape_XYZToColor(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PColor(Result)^ := ps_XYZToColor(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^);
end;

procedure Lape_RGBToHSL(const Params: PParamArray); lape_extdecl
begin
  ps_RGBToHSL(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure Lape_HSLtoRGB(const Params: PParamArray); lape_extdecl
begin
  ps_HSLtoRGB(Pextended(Params^[0])^, Pextended(Params^[1])^, Pextended(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^);
end;

procedure Lape_RGBToXYZ(const Params: PParamArray); lape_extdecl
begin
  ps_RGBToXYZ(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure Lape_XYZToRGB(const Params: PParamArray); lape_extdecl
begin
  ps_XYZToRGB(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^);
end;

procedure Lape_XYZToHSL(const Params: PParamArray); lape_extdecl
begin
  ps_HSLToXYZ(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure Lape_HSLToXYZ(const Params: PParamArray); lape_extdecl
begin
  ps_HSLToXYZ(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure Lape_XYZtoCIELab(const Params: PParamArray); lape_extdecl
begin
  ps_XYZtoCIELab(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure Lape_CIELabtoXYZ(const Params: PParamArray); lape_extdecl
begin
  ps_CIELabToXYZ(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure Lape_CIELabToRGB(const Params: PParamArray); lape_extdecl
begin
  ps_CIELabToRGB(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^);
end;

procedure Lape_RGBToCIELab(const Params: PParamArray); lape_extdecl
begin
  ps_RGBToCIELab(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure Lape_CIELabToColor(const Params: PParamArray; out Result : Pointer); lape_extdecl
begin
  PColor(Result)^ := ps_CIELabToColor(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^);
end;

procedure Lape_ColorToCIELab(const Params: PParamArray); lape_extdecl
begin
  ps_ColorToCIELab(PInteger(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^);
end;

procedure Lape_CIELabToHSL(const Params: PParamArray); lape_extdecl
begin
  ps_CIELabToHSL(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure Lape_HSLToCIELab(const Params: PParamArray); lape_extdecl
begin
  ps_HSLToCIELab(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;
