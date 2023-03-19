unit simba.import_colormath;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, graphics, lptypes,
  simba.script_compiler, simba.mufasatypes, simba.colormath_distance, simba.colormath_conversion;

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

procedure _LapeColorToBGRA(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorBGRA(Result)^ := PColor(Params^[0])^.ToBGRA();
end;

procedure _LapeColorToRGB(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorRGB(Result)^ := PColor(Params^[0])^.ToRGB();
end;

procedure _LapeColorToXYZ(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorXYZ(Result)^ := PColor(Params^[0])^.ToXYZ();
end;

procedure _LapeColorToLAB(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorLAB(Result)^ := PColor(Params^[0])^.ToLAB();
end;

procedure _LapeColorToLCH(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorLCH(Result)^ := PColor(Params^[0])^.ToLCH();
end;

procedure _LapeColorToHSV(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorHSV(Result)^ := PColor(Params^[0])^.ToHSV();
end;

procedure _LapeColorToHSL(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
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

procedure ImportColorMath(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Color Math';

    addGlobalType('packed record B,G,R,A: Byte; end;', 'TColorBGRA');
    addGlobalType('record R,G,B: Byte; end;', 'TColorRGB');
    addGlobalType('record X,Y,Z: Single; end;', 'TColorXYZ');
    addGlobalType('record L,A,B: Single; end;', 'TColorLAB');
    addGlobalType('record L,C,H: Single; end;', 'TColorLCH');
    addGlobalType('record H,S,V: Single; end;', 'TColorHSV');
    addGlobalType('record H,S,L: Single; end;', 'TColorHSL');

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

    addGlobalFunc('function TColor.ToBGRA: TColorBGRA', @_LapeColorToBGRA);
    addGlobalFunc('function TColor.ToRGB: TColorRGB', @_LapeColorToRGB);
    addGlobalFunc('function TColor.ToXYZ: TColorXYZ', @_LapeColorToXYZ);
    addGlobalFunc('function TColor.ToLAB: TColorLAB', @_LapeColorToLAB);
    addGlobalFunc('function TColor.ToLCH: TColorLCH', @_LapeColorToLCH);
    addGlobalFunc('function TColor.ToHSV: TColorHSV', @_LapeColorToHSV);
    addGlobalFunc('function TColor.ToHSL: TColorHSL', @_LapeColorToHSL);

    ImportingSection := '';
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportColorMath);

end.

