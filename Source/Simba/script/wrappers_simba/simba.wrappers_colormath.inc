procedure _LapeColorToRGB(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  ColorToRGB(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure _LapeRGBtoColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PColor(Result)^ := RGBtoColor(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure _LapeColorToHSL(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  ColorToHSL(PInt32(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^);
end;

procedure _LapeHSLToColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PColor(Result)^ := HSLToColor(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^);
end;

procedure _LapeColorToXYZ(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  ColorToXYZ(PInt32(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^);
end;

procedure _LapeXYZToColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PColor(Result)^ := XYZToColor(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^);
end;

procedure _LapeRGBToHSL(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  RGBToHSL(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure _LapeHSLtoRGB(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  HSLtoRGB(Pextended(Params^[0])^, Pextended(Params^[1])^, Pextended(Params^[2])^, PByte(Params^[3])^, PByte(Params^[4])^, PByte(Params^[5])^);
end;

procedure _LapeRGBToXYZ(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  RGBToXYZ(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure _LapeXYZToRGB(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  XYZToRGB(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^);
end;

procedure _LapeXYZToHSL(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  HSLToXYZ(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure _LapeHSLToXYZ(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  HSLToXYZ(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure _LapeXYZtoCIELab(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  XYZtoCIELab(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure _LapeCIELabtoXYZ(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  CIELabToXYZ(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure _LapeCIELabToRGB(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  CIELabToRGB(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^);
end;

procedure _LapeRGBToCIELab(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  RGBToCIELab(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure _LapeCIELabToColor(const Params: PParamArray; const Result : Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PColor(Result)^ := CIELabToColor(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^);
end;

procedure _LapeColorToCIELab(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  ColorToCIELab(PInt32(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^);
end;

procedure _LapeCIELabToHSL(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  CIELabToHSL(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure _LapeHSLToCIELab(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  HSLToCIELab(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure _LapeColorToGray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PColor(Result)^ := ColorToGray(PInt32(Params^[0])^);
end;

