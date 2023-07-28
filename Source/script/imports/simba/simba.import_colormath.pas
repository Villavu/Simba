unit simba.import_colormath;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.script_compiler;

procedure ImportColorMath(Compiler: TSimbaScript_Compiler);

implementation

uses
  Graphics, lptypes,
  simba.colormath_distance, simba.colormath;

procedure _LapeColorRGBToBGRA(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorBGRA(Result)^ := PColorRGB(Params^[0])^.ToBGRA();
end;

procedure _LapeColorRGBToXYZ(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorXYZ(Result)^ := PColorRGB(Params^[0])^.ToXYZ();
end;

procedure _LapeColorRGBToLAB(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorLAB(Result)^ := PColorRGB(Params^[0])^.ToLAB();
end;

procedure _LapeColorRGBToLCH(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorLCH(Result)^ := PColorRGB(Params^[0])^.ToLCH();
end;

procedure _LapeColorRGBToHSV(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorHSV(Result)^ := PColorRGB(Params^[0])^.ToHSV();
end;

procedure _LapeColorRGBToHSL(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorHSL(Result)^ := PColorRGB(Params^[0])^.ToHSL();
end;

procedure _LapeColorRGBToColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PColorRGB(Params^[0])^.ToColor();
end;

procedure _LapeTColorToBGRA(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorBGRA(Result)^ := PColor(Params^[0])^.ToBGRA();
end;

procedure _LapeTColorToRGB(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorRGB(Result)^ := PColor(Params^[0])^.ToRGB();
end;

procedure _LapeTColorToXYZ(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorXYZ(Result)^ := PColor(Params^[0])^.ToXYZ();
end;

procedure _LapeTColorToLAB(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorLAB(Result)^ := PColor(Params^[0])^.ToLAB();
end;

procedure _LapeTColorToLCH(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorLCH(Result)^ := PColor(Params^[0])^.ToLCH();
end;

procedure _LapeTColorToHSV(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorHSV(Result)^ := PColor(Params^[0])^.ToHSV();
end;

procedure _LapeTColorToHSL(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorHSL(Result)^ := PColor(Params^[0])^.ToHSL();
end;

procedure _LapeColorHSLToRGB(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorRGB(Result)^ := PColorHSL(Params^[0])^.ToRGB();
end;

procedure _LapeColorHSLToColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PColorHSL(Params^[0])^.ToColor();
end;

procedure _LapeColorHSVToRGB(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorRGB(Result)^ := PColorHSV(Params^[0])^.ToRGB();
end;

procedure _LapeColorHSVToColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PColorHSV(Params^[0])^.ToColor();
end;

procedure _LapeColorXYZToRGB(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorRGB(Result)^ := PColorXYZ(Params^[0])^.ToRGB();
end;

procedure _LapeColorXYZToColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PColorXYZ(Params^[0])^.ToColor();
end;

procedure _LapeColorLABToRGB(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorRGB(Result)^ := PColorLAB(Params^[0])^.ToRGB();
end;

procedure _LapeColorLABToColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PColorLAB(Params^[0])^.ToColor();
end;

procedure _LapeColorLCHToRGB(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorRGB(Result)^ := PColorLCH(Params^[0])^.ToRGB();
end;

procedure _LapeColorLCHToColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PColorLCH(Params^[0])^.ToColor();
end;

procedure _LapeColorIntensity(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PByte(Result)^ := ColorIntensity(PColor(Params^[0])^);
end;

procedure _LapeColorToGray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PByte(Result)^ := ColorToGray(PColor(Params^[0])^);
end;

procedure _LapeColorToRGB(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorRGB(Result)^ := ColorToRGB(PColor(Params^[0])^);
end;

procedure _LapeColorToBGRA(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorBGRA(Result)^ := ColorToBGRA(PColor(Params^[0])^);
end;

procedure _LapeColorToHSL(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorHSL(Result)^ := ColorToHSL(PColor(Params^[0])^);
end;

procedure _LapeColorToHSV(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorHSV(Result)^ := ColorToHSV(PColor(Params^[0])^);
end;

procedure _LapeColorToXYZ(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorXYZ(Result)^ := ColorToXYZ(PColor(Params^[0])^);
end;

procedure _LapeColorToLAB(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorLAB(Result)^ := ColorToLAB(PColor(Params^[0])^);
end;

procedure _LapeColorToLCH(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorLCH(Result)^ := ColorToLCH(PColor(Params^[0])^);
end;

procedure _LapeSimilarColors1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := SimilarColors(PColor(Params^[0])^, PColor(Params^[1])^, PSingle(Params^[2])^);
end;

procedure _LapeSimilarColors2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := SimilarColors(PColor(Params^[0])^, PColor(Params^[1])^, PSingle(Params^[2])^, PColorSpace(Params^[3])^, PChannelMultipliers(Params^[4])^);
end;

procedure ImportColorMath(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Color Math';

    addGlobalType('enum(RGB, HSV, HSL, XYZ, LAB, LCH, DELTAE)', 'EColorSpace');
    addGlobalType('array [0..2] of Single', 'TChannelMultipliers');

    addGlobalType('record B,G,R,A: Byte; end;', 'TColorBGRA');
    addGlobalType('record R,G,B: Byte; end;', 'TColorRGB');
    addGlobalType('record X,Y,Z: Single; end;', 'TColorXYZ');
    addGlobalType('record L,A,B: Single; end;', 'TColorLAB');
    addGlobalType('record L,C,H: Single; end;', 'TColorLCH');
    addGlobalType('record H,S,V: Single; end;', 'TColorHSV');
    addGlobalType('record H,S,L: Single; end;', 'TColorHSL');

    addGlobalType('^TColorBGRA', 'PColorBGRA');

    addGlobalFunc('function TColorRGB.ToBGRA: TColorBGRA', @_LapeColorRGBToBGRA);
    addGlobalFunc('function TColorRGB.ToXYZ: TColorXYZ', @_LapeColorRGBToXYZ);
    addGlobalFunc('function TColorRGB.ToLAB: TColorLAB', @_LapeColorRGBToLAB);
    addGlobalFunc('function TColorRGB.ToLCH: TColorLCH', @_LapeColorRGBToLCH);
    addGlobalFunc('function TColorRGB.ToHSV: TColorHSV', @_LapeColorRGBToHSV);
    addGlobalFunc('function TColorRGB.ToHSL: TColorHSL', @_LapeColorRGBToHSL);
    addGlobalFunc('function TColorRGB.ToColor: TColor', @_LapeColorRGBToColor);

    addGlobalFunc('function TColorHSL.ToRGB: TColorRGB', @_LapeColorHSLToRGB);
    addGlobalFunc('function TColorHSL.ToColor: TColor', @_LapeColorHSLToColor);

    addGlobalFunc('function TColorHSV.ToRGB: TColorRGB', @_LapeColorHSVToRGB);
    addGlobalFunc('function TColorHSV.ToColor: TColor', @_LapeColorHSVToColor);

    addGlobalFunc('function TColorXYZ.ToRGB: TColorRGB', @_LapeColorXYZToRGB);
    addGlobalFunc('function TColorXYZ.ToColor: TColor', @_LapeColorXYZToColor);

    addGlobalFunc('function TColorLAB.ToRGB: TColorRGB', @_LapeColorLABToRGB);
    addGlobalFunc('function TColorLAB.ToColor: TColor', @_LapeColorLABToColor);

    addGlobalFunc('function TColorLCH.ToRGB: TColorRGB', @_LapeColorLCHToRGB);
    addGlobalFunc('function TColorLCH.ToColor: TColor', @_LapeColorLCHToColor);

    addGlobalFunc('function TColor.ToBGRA: TColorBGRA', @_LapeTColorToBGRA);
    addGlobalFunc('function TColor.ToRGB: TColorRGB', @_LapeTColorToRGB);
    addGlobalFunc('function TColor.ToXYZ: TColorXYZ', @_LapeTColorToXYZ);
    addGlobalFunc('function TColor.ToLAB: TColorLAB', @_LapeTColorToLAB);
    addGlobalFunc('function TColor.ToLCH: TColorLCH', @_LapeTColorToLCH);
    addGlobalFunc('function TColor.ToHSV: TColorHSV', @_LapeTColorToHSV);
    addGlobalFunc('function TColor.ToHSL: TColorHSL', @_LapeTColorToHSL);

    addGlobalFunc('function ColorIntensity(Color: TColor): Byte', @_LapeColorIntensity);
    addGlobalFunc('function ColorToGray(Color: TColor): Byte', @_LapeColorToGray);
    addGlobalFunc('function ColorToRGB(Color: TColor): TColorRGB', @_LapeColorToRGB);
    addGlobalFunc('function ColorToBGRA(Color: TColor): TColorBGRA', @_LapeColorToBGRA);
    addGlobalFunc('function ColorToHSL(Color: TColor): TColorHSL', @_LapeColorToHSL);
    addGlobalFunc('function ColorToHSV(Color: TColor): TColorHSV', @_LapeColorToHSV);
    addGlobalFunc('function ColorToXYZ(Color: TColor): TColorXYZ', @_LapeColorToXYZ);
    addGlobalFunc('function ColorToLAB(Color: TColor): TColorLAB', @_LapeColorToLAB);
    addGlobalFunc('function ColorToLCH(Color: TColor): TColorLCH', @_LapeColorToLCH);

    addGlobalFunc(
      'function RGBToColor(R, G, B: Byte): TColor;', [
      'begin',
      '  Result := TColorRGB([R, G, B]).ToColor();',
      'end;'
    ]);
    addGlobalFunc(
      'function HSLToColor(H, S, L: Single): TColor;', [
      'begin',
      '  Result := TColorHSL([H, S, L]).ToColor();',
      'end;'
    ]);
     addGlobalFunc(
      'function HSVToColor(H, S, V: Single): TColor;', [
      'begin',
      '  Result := TColorHSV([H, S, V]).ToColor();',
      'end;'
    ]);
    addGlobalFunc(
       'function XYZToColor(X, Y, Z: Single): TColor;', [
       'begin',
       '  Result := TColorXYZ([X, Y, Z]).ToColor();',
       'end;'
     ]);
     addGlobalFunc(
       'function LABToColor(L, A, B: Single): TColor;', [
       'begin',
       '  Result := TColorLAB([L, A, B]).ToColor();',
       'end;'
     ]);
     addGlobalFunc(
       'function LCHToColor(L, C, H: Single): TColor;', [
       'begin',
       '  Result := TColorLCH([L, C, H]).ToColor();',
       'end;'
     ]);

    addGlobalFunc('function SimilarColors(Color1, Color2: TColor; Tolerance: Single): Boolean; overload', @_LapeSimilarColors1);
    addGlobalFunc('function SimilarColors(Color1, Color2: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers): Boolean; overload;', @_LapeSimilarColors2);

    ImportingSection := '';
  end;
end;

end.

