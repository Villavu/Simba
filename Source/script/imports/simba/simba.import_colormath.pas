unit simba.import_colormath;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, graphics, lptypes,
  simba.script_compiler, simba.mufasatypes, simba.colormath, simba.colormath_distance;

procedure _LapeColorToRGB(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ColorToRGB(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure _LapeRGBtoColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := RGBtoColor(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure _LapeColorToHSL(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  //ColorToHSL(PInt32(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^);
end;

procedure _LapeHSLToColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  //PColor(Result)^ := HSLToColor(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^);
end;

procedure _LapeColorToXYZ(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
 // ColorToXYZ(PInt32(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^);
end;

procedure _LapeXYZToColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  //PColor(Result)^ := XYZToColor(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^);
end;

procedure _LapeRGBToHSL(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
 // RGBToHSL(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure _LapeHSLtoRGB(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  //HSLtoRGB(Pextended(Params^[0])^, Pextended(Params^[1])^, Pextended(Params^[2])^, PByte(Params^[3])^, PByte(Params^[4])^, PByte(Params^[5])^);
end;

procedure _LapeCalculateBestColor(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  //case PInteger(Params^[1])^ of
  //  0: BestColor_CTS0(PIntegerArray(Params^[0])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
  //  1: BestColor_CTS1(PIntegerArray(Params^[0])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
  //  2: BestColor_CTS2(PIntegerArray(Params^[0])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
  //end;
end;

procedure _LapeRGBToXYZ(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  //RGBToXYZ(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure _LapeXYZToRGB(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
 // XYZToRGB(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^);
end;

procedure _LapeXYZToHSL(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
 // HSLToXYZ(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure _LapeHSLToXYZ(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
 // HSLToXYZ(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure _LapeXYZtoCIELab(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
 // XYZtoCIELab(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure _LapeCIELabtoXYZ(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
//  CIELabToXYZ(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure _LapeCIELabToRGB(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
//  CIELabToRGB(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^);
end;

procedure _LapeRGBToCIELab(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
 // RGBToCIELab(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure _LapeCIELabToColor(const Params: PParamArray; const Result : Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  //PColor(Result)^ := CIELabToColor(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^);
end;

procedure _LapeColorToCIELab(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  //ColorToCIELab(PInt32(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^);
end;

procedure _LapeCIELabToHSL(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  //CIELabToHSL(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure _LapeHSLToCIELab(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  //HSLToCIELab(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure _LapeColorToGray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  //PColor(Result)^ := ColorToGray(PInt32(Params^[0])^);
end;

procedure _LapeDistanceRGB(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  //PInteger(Result)^ := DistanceRGB(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeDistanceHSL(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  //PSingle(Result)^ := DistanceHSL(PInteger(Params^[0])^, PInteger(Params^[1])^, PSingle(Params^[2])^, PSingle(Params^[3])^);
end;

procedure ImportColorMath(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Color Math';

    addGlobalFunc('procedure ColorToRGB(Color: Int32; var r, g, b: Int32)', @_LapeColorToRGB);
    addGlobalFunc('function RGBtoColor(r, g, b: Int32): TColor', @_LapeRGBtoColor);
    addGlobalFunc('procedure ColorToHSL(Color: Int32; var h, s, l: Extended)', @_LapeColorToHSL);
    addGlobalFunc('function HSLToColor(H, S, L: Extended): TColor', @_LapeHSLToColor);
    addGlobalFunc('procedure ColorToXYZ(Color: Int32; var x, y, z: Extended)', @_LapeColorToXYZ);
    addGlobalFunc('function XYZToColor(X, Y, Z: Extended): TColor', @_LapeXYZToColor);
    addGlobalFunc('procedure RGBToHSL(R, G, B: Int32; var h, s, l: Extended)', @_LapeRGBToHSL);
    addGlobalFunc('procedure HSLtoRGB(H, S, L: extended; var R, G, B: Byte)', @_LapeHSLtoRGB);
    addGlobalFunc('procedure RGBToXYZ(R, G, B: Int32; var x, y, z: Extended)', @_LapeRGBToXYZ);
    addGlobalFunc('procedure XYZToRGB(X, Y, Z: Extended; var R, G, B: Int32)', @_LapeXYZToRGB);
    addGlobalFunc('procedure XYZToHSL(X, Y, Z: Extended; out H, S, L: Extended)', @_LapeXYZToHSL);
    addGlobalFunc('procedure HSLToXYZ(H, S, L: Extended; out X, Y, Z: Extended)', @_LapeHSLToXYZ);
    addGlobalFunc('procedure XYZtoCIELab(X, Y, Z: Extended; out L, a, b: Extended)', @_LapeXYZtoCIELab);
    addGlobalFunc('procedure CIELabtoXYZ(L, a, b: Extended; out X, Y, Z: Extended)', @_LapeCIELabtoXYZ);
    addGlobalFunc('procedure CIELabToRGB(L, a, b: Extended; out rr, gg, bb: Int32)', @_LapeCIELabToRGB);
    addGlobalFunc('procedure RGBToCIELab(rr, gg, bb: Int32; out L, a, b: Extended)', @_LapeRGBToCIELab);
    addGlobalFunc('function CIELabToColor(L, a, b: Extended): TColor', @_LapeCIELabToColor);
    addGlobalFunc('procedure ColorToCIELab(Color: Int32; out L, a, b: Extended)', @_LapeColorToCIELab);
    addGlobalFunc('procedure CIELabToHSL(L, a, b: Extended; out HH, SS, LL: Extended)', @_LapeCIELabToHSL);
    addGlobalFunc('procedure HSLToCIELab(HH, SS, LL: Extended; out L, a, b: Extended)', @_LapeHSLToCIELab);
    addGlobalFunc('function ColorToGray(const Color: Int32): TColor', @_LapeColorToGray);
    addGlobalFunc('procedure CalculateBestColor(Colors: TIntegerArray; CTS: Integer; out Color, Tolerance: Integer; out Hue, Sat: Extended)', @_LapeCalculateBestColor);

    addGlobalFunc('function DistanceHSL(const Color1, Color2: Integer; const HueMod: Single = 0.2; const SatMod: Single = 0.2): Single', @_LapeDistanceHSL);
    addGlobalFunc('function DistanceRGB(const Color1, Color2: Integer): Integer', @_LapeDistanceRGB);

    ImportingSection := '';
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportColorMath);

end.

