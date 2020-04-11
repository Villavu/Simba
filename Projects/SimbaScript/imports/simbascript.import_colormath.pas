unit simbascript.import_colormath;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_ColorMath(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  graphics, simba.colormath;

procedure Lape_ColorToRGB(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  ColorToRGB(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
end;

procedure Lape_RGBtoColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PColor(Result)^ := RGBtoColor(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_ColorToHSL(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  ColorToHSL(PInt32(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^);
end;

procedure Lape_HSLToColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PColor(Result)^ := HSLToColor(PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^);
end;

procedure Lape_ColorToXYZ(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  ColorToXYZ(PInt32(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^);
end;

procedure Lape_XYZToColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PColor(Result)^ := XYZToColor(PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^);
end;

procedure Lape_RGBToHSL(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  RGBToHSL(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^, PExtended(Params^[6])^);
end;

procedure Lape_HSLtoRGB(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  HSLtoRGB(Pextended(Params^[1])^, Pextended(Params^[2])^, Pextended(Params^[3])^, PByte(Params^[4])^, PByte(Params^[5])^, PByte(Params^[6])^);
end;

procedure Lape_RGBToXYZ(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  RGBToXYZ(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^, PExtended(Params^[6])^);
end;

procedure Lape_XYZToRGB(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  XYZToRGB(PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^);
end;

procedure Lape_XYZToHSL(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  HSLToXYZ(PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^, PExtended(Params^[6])^);
end;

procedure Lape_HSLToXYZ(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  HSLToXYZ(PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^, PExtended(Params^[6])^);
end;

procedure Lape_XYZtoCIELab(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  XYZtoCIELab(PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^, PExtended(Params^[6])^);
end;

procedure Lape_CIELabtoXYZ(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  CIELabToXYZ(PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^, PExtended(Params^[6])^);
end;

procedure Lape_CIELabToRGB(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  CIELabToRGB(PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^);
end;

procedure Lape_RGBToCIELab(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  RGBToCIELab(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^, PExtended(Params^[6])^);
end;

procedure Lape_CIELabToColor(const Params: PParamArray; const Result : Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PColor(Result)^ := CIELabToColor(PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^);
end;

procedure Lape_ColorToCIELab(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  ColorToCIELab(PInt32(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^);
end;

procedure Lape_CIELabToHSL(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  CIELabToHSL(PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^, PExtended(Params^[6])^);
end;

procedure Lape_HSLToCIELab(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  HSLToCIELab(PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^, PExtended(Params^[6])^);
end;

procedure Lape_ColorToGray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PColor(Result)^ := ColorToGray(PInt32(Params^[1])^);
end;

procedure Lape_Import_ColorMath(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    Section := 'Color Math';

    addGlobalMethod('procedure ColorToRGB(Color: Int32; var r, g, b: Int32);', @Lape_ColorToRGB, Data);
    addGlobalMethod('function RGBtoColor(r, g, b: Int32): TColor', @Lape_RGBtoColor, Data);
    addGlobalMethod('procedure ColorToHSL(Color: Int32; var h, s, l: Extended);', @Lape_ColorToHSL, Data);
    addGlobalMethod('function HSLToColor(H, S, L: Extended): TColor', @Lape_HSLToColor, Data);
    addGlobalMethod('procedure ColorToXYZ(Color: Int32; var x, y, z: Extended);', @Lape_ColorToXYZ, Data);
    addGlobalMethod('function XYZToColor(X, Y, Z: Extended): TColor', @Lape_XYZToColor, Data);
    addGlobalMethod('procedure RGBToHSL(R, G, B: Int32; var h, s, l: Extended);', @Lape_RGBToHSL, Data);
    addGlobalMethod('procedure HSLtoRGB(H, S, L: extended; var R, G, B: Byte);', @Lape_HSLtoRGB, Data);
    addGlobalMethod('procedure RGBToXYZ(R, G, B: Int32; var x, y, z: Extended);', @Lape_RGBToXYZ, Data);
    addGlobalMethod('procedure XYZToRGB(X, Y, Z: Extended; var R, G, B: Int32);', @Lape_XYZToRGB, Data);
    addGlobalMethod('procedure XYZToHSL(X, Y, Z: Extended; out H, S, L: Extended);', @Lape_XYZToHSL, Data);
    addGlobalMethod('procedure HSLToXYZ(H, S, L: Extended; out X, Y, Z: Extended);', @Lape_HSLToXYZ, Data);
    addGlobalMethod('procedure XYZtoCIELab(X, Y, Z: Extended; out L, a, b: Extended);', @Lape_XYZtoCIELab, Data);
    addGlobalMethod('procedure CIELabtoXYZ(L, a, b: Extended; out X, Y, Z: Extended);', @Lape_CIELabtoXYZ, Data);
    addGlobalMethod('procedure CIELabToRGB(L, a, b: Extended; out rr, gg, bb: Int32);', @Lape_CIELabToRGB, Data);
    addGlobalMethod('procedure RGBToCIELab(rr, gg, bb: Int32; out L, a, b: Extended);', @Lape_RGBToCIELab, Data);
    addGlobalMethod('function CIELabToColor(L, a, b: Extended): TColor;', @Lape_CIELabToColor, Data);
    addGlobalMethod('procedure ColorToCIELab(Color: Int32; out L, a, b: Extended);', @Lape_ColorToCIELab, Data);
    addGlobalMethod('procedure CIELabToHSL(L, a, b: Extended; out HH, SS, LL: Extended);', @Lape_CIELabToHSL, Data);
    addGlobalMethod('procedure HSLToCIELab(HH, SS, LL: Extended; out L, a, b: Extended);', @Lape_HSLToCIELab, Data);
    addGlobalMethod('function ColorToGray(const Color: Int32): TColor', @Lape_ColorToGray, Data);
  end;
end;

end.

