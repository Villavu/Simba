unit simba.import_colormath;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

procedure ImportColorMath(Compiler: TSimbaScript_Compiler);

implementation

uses
  Graphics, lptypes,
  simba.colormath_distance, simba.colormath, simba.colormath_aca;

(*
Color Math
==========
Colors and conversion into different formats.
*)

(*
EColorSpace
-----------
> EColorSpace = enum(RGB, HSV, HSL, XYZ, LAB, LCH, DELTAE)

Enum of color spaces Simba supports.

Note:: This enum is scoped, which means it must be used like: `EColorSpace.HSL`, or `EColorSpace.RGB` etc.
*)

(*
TColorRGB.ToBGRA
----------------
> function TColorRGB.ToBGRA: TColorBGRA;
*)
procedure _LapeColorRGBToBGRA(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorBGRA(Result)^ := PColorRGB(Params^[0])^.ToBGRA();
end;

(*
TColorRGB.ToXYZ
---------------
> function TColorRGB.ToXYZ: TColorXYZ;
*)
procedure _LapeColorRGBToXYZ(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorXYZ(Result)^ := PColorRGB(Params^[0])^.ToXYZ();
end;

(*
TColorRGB.ToLAB
---------------
> function TColorRGB.ToLAB: TColorLAB;
*)
procedure _LapeColorRGBToLAB(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorLAB(Result)^ := PColorRGB(Params^[0])^.ToLAB();
end;

(*
TColorRGB.ToLCH
---------------
> function TColorRGB.ToLCH: TColorLCH;
*)
procedure _LapeColorRGBToLCH(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorLCH(Result)^ := PColorRGB(Params^[0])^.ToLCH();
end;

(*
TColorRGB.ToHSV
---------------
> function TColorRGB.ToHSV: TColorHSV;
*)
procedure _LapeColorRGBToHSV(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorHSV(Result)^ := PColorRGB(Params^[0])^.ToHSV();
end;

(*
TColorRGB.ToHSL
---------------
> function TColorRGB.ToHSL: TColorHSL;
*)
procedure _LapeColorRGBToHSL(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorHSL(Result)^ := PColorRGB(Params^[0])^.ToHSL();
end;

(*
TColorRGB.ToColor
-----------------
> function TColorRGB.ToColor: TColor;
*)
procedure _LapeColorRGBToColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PColorRGB(Params^[0])^.ToColor();
end;

(*
TColor.ToBGRA
-------------
> function TColor.ToBGRA: TColorBGRA;
*)
procedure _LapeTColorToBGRA(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorBGRA(Result)^ := PColor(Params^[0])^.ToBGRA();
end;

(*
TColor.ToRGB
------------
> function TColor.ToRGB: TColorRGB;
*)
procedure _LapeTColorToRGB(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorRGB(Result)^ := PColor(Params^[0])^.ToRGB();
end;

(*
TColor.ToXYZ
------------
> function TColor.ToXYZ: TColorXYZ;
*)
procedure _LapeTColorToXYZ(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorXYZ(Result)^ := PColor(Params^[0])^.ToXYZ();
end;

(*
TColor.ToLAB
------------
> function TColor.ToLAB: TColorLAB;
*)
procedure _LapeTColorToLAB(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorLAB(Result)^ := PColor(Params^[0])^.ToLAB();
end;

(*
TColor.ToLCH
------------
> function TColor.ToLCH: TColorLCH;
*)
procedure _LapeTColorToLCH(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorLCH(Result)^ := PColor(Params^[0])^.ToLCH();
end;

(*
TColor.ToHSV
------------
> function TColor.ToHSV: TColorHSV;
*)
procedure _LapeTColorToHSV(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorHSV(Result)^ := PColor(Params^[0])^.ToHSV();
end;

(*
TColor.ToHSL
------------
> function TColor.ToHSL: TColorHSL;
*)
procedure _LapeTColorToHSL(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorHSL(Result)^ := PColor(Params^[0])^.ToHSL();
end;

(*
TColorHSL.ToRGB
---------------
> function TColorHSL.ToRGB: TColorRGB;
*)
procedure _LapeColorHSLToRGB(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorRGB(Result)^ := PColorHSL(Params^[0])^.ToRGB();
end;

(*
TColorHSL.ToColor
-----------------
> function TColorHSL.ToColor: TColor;
*)
procedure _LapeColorHSLToColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PColorHSL(Params^[0])^.ToColor();
end;

(*
TColorHSV.ToRGB
---------------
> function TColorHSV.ToRGB: TColorRGB;
*)
procedure _LapeColorHSVToRGB(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorRGB(Result)^ := PColorHSV(Params^[0])^.ToRGB();
end;

(*
TColorHSV.ToColor
-----------------
> function TColorHSV.ToColor: TColor;
*)
procedure _LapeColorHSVToColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PColorHSV(Params^[0])^.ToColor();
end;

(*
TColorXYZ.ToRGB
---------------
> function TColorXYZ.ToRGB: TColorRGB;
*)
procedure _LapeColorXYZToRGB(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorRGB(Result)^ := PColorXYZ(Params^[0])^.ToRGB();
end;

(*
TColorXYZ.ToColor
-----------------
> function TColorXYZ.ToColor: TColor;
*)
procedure _LapeColorXYZToColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PColorXYZ(Params^[0])^.ToColor();
end;

(*
TColorLAB.ToRGB
---------------
> function TColorLAB.ToRGB: TColorRGB;
*)
procedure _LapeColorLABToRGB(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorRGB(Result)^ := PColorLAB(Params^[0])^.ToRGB();
end;

(*
TColorLAB.ToColor
-----------------
> function TColorLAB.ToColor: TColor;
*)
procedure _LapeColorLABToColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PColorLAB(Params^[0])^.ToColor();
end;

(*
TColorLCH.ToRGB
---------------
> function TColorLCH.ToRGB: TColorRGB;
*)
procedure _LapeColorLCHToRGB(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorRGB(Result)^ := PColorLCH(Params^[0])^.ToRGB();
end;

(*
TColorLCH.ToColor
-----------------
> function TColorLCH.ToColor: TColor;
*)
procedure _LapeColorLCHToColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PColorLCH(Params^[0])^.ToColor();
end;

(*
ColorIntensity
--------------
> function ColorIntensity(Color: TColor): Byte;
*)
procedure _LapeColorIntensity(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PByte(Result)^ := ColorIntensity(PColor(Params^[0])^);
end;

(*
ColorToGray
-----------
> function ColorToGray(Color: TColor): Byte;
*)
procedure _LapeColorToGray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PByte(Result)^ := ColorToGray(PColor(Params^[0])^);
end;

(*
ColorToRGB
----------
> function ColorToRGB(Color: TColor): TColorRGB;
*)
procedure _LapeColorToRGB(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorRGB(Result)^ := ColorToRGB(PColor(Params^[0])^);
end;

(*
ColorToBGRA
-----------
> function ColorToBGRA(Color: TColor): TColorBGRA;
*)
procedure _LapeColorToBGRA(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorBGRA(Result)^ := ColorToBGRA(PColor(Params^[0])^);
end;

(*
ColorToHSL
----------
> function ColorToHSL(Color: TColor): TColorHSL;
*)
procedure _LapeColorToHSL(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorHSL(Result)^ := ColorToHSL(PColor(Params^[0])^);
end;

(*
ColorToHSV
----------
> function ColorToHSV(Color: TColor): TColorHSV;
*)
procedure _LapeColorToHSV(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorHSV(Result)^ := ColorToHSV(PColor(Params^[0])^);
end;

(*
ColorToXYZ
----------
> function ColorToXYZ(Color: TColor): TColorXYZ;
*)
procedure _LapeColorToXYZ(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorXYZ(Result)^ := ColorToXYZ(PColor(Params^[0])^);
end;

(*
ColorToLAB
----------
> function ColorToLAB(Color: TColor): TColorLAB;
*)
procedure _LapeColorToLAB(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorLAB(Result)^ := ColorToLAB(PColor(Params^[0])^);
end;

(*
ColorToLCH
----------
> function ColorToLCH(Color: TColor): TColorLCH;
*)
procedure _LapeColorToLCH(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorLCH(Result)^ := ColorToLCH(PColor(Params^[0])^);
end;

(*
SimilarColors
-------------
> function SimilarColors(Color1, Color2: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers): Boolean;
*)
procedure _LapeSimilarColors1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := SimilarColors(PColor(Params^[0])^, PColor(Params^[1])^, PSingle(Params^[2])^, PColorSpace(Params^[3])^, PChannelMultipliers(Params^[4])^);
end;

(*
SimilarColors
-------------
> function SimilarColors(Color1, Color2: TColor; Tolerance: Single): Boolean;
*)
procedure _LapeSimilarColors2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := SimilarColors(PColor(Params^[0])^, PColor(Params^[1])^, PSingle(Params^[2])^);
end;

(*
ColorDistance
-------------
> function ColorDistance(const Color1, Color2: TColor; const ColorSpace: EColorSpace; const Multipliers: TChannelMultipliers): Single;
*)
procedure _LapeColorDistance1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := ColorDistance(PColor(Params^[0])^, PColor(Params^[1])^, PColorSpace(Params^[2])^, PChannelMultipliers(Params^[3])^);
end;

(*
ColorDistance
-------------
> function ColorDistance(const Color1, Color2: TColor): Single;
*)
procedure _LapeColorDistance2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := ColorDistance(PColor(Params^[0])^, PColor(Params^[1])^);
end;

(*
GetBestColor
------------
> function GetBestColor(ColorSpace: EColorSpace; Colors: TColorArray): TBestColor;
*)
procedure _LapeGetBestColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TBestColor(Result^) := GetBestColor(PColorSpace(Params^[0])^, PColorArray(Params^[1])^);
end;

(*
TColor.R
--------
> function TColor.R: Byte;

Return the red channel value of a TColor
*)

(*
TColor.G
--------
> function TColor.G: Byte;

Return the green channel value of a TColor
*)

(*
TColor.B
--------
> function TColor.B: Byte;

Return the blue channel value of a TColor
*)

(*
Colors
------
This record constants of -140 HTML colors.

```
Colors.ALICEBLUE
Colors.ANTIQUEWHITE
Colors.AQUA
Colors.AQUAMARINE
Colors.AZURE
Colors.BEIGE
Colors.BISQUE
Colors.BLACK
Colors.BLANCHEDALMOND
Colors.BLUE
Colors.BLUEVIOLET
Colors.BROWN
Colors.BURLYWOOD
Colors.CADET_BLUE
Colors.CHARTREUSE
Colors.CHOCOLATE
Colors.CORAL
Colors.CORNFLOWER_BLUE
Colors.CORNSILK
Colors.CRIMSON
Colors.CYAN
Colors.DARK_BLUE
Colors.DARK_CYAN
Colors.DARK_GOLDENROD
Colors.DARK_GRAY
Colors.DARK_GREEN
Colors.DARK_GREY
Colors.DARK_KHAKI
Colors.DARK_MAGENTA
Colors.DARK_OLIVEGREEN
Colors.DARK_ORANGE
Colors.DARK_ORCHID
Colors.DARK_RED
Colors.DARK_SALMON
Colors.DARK_SEAGREEN
Colors.DARK_SLATEBLUE
Colors.DARK_SLATEGRAY
Colors.DARK_SLATEGREY
Colors.DARK_TURQUOISE
Colors.DARK_VIOLET
Colors.DEEPPINK
Colors.DEEPSKYBLUE
Colors.DIMGRAY
Colors.DIMGREY
Colors.DODGERBLUE
Colors.FIREBRICK
Colors.FLORALWHITE
Colors.FORESTGREEN
Colors.FUCHSIA
Colors.GAINSBORO
Colors.GHOSTWHITE
Colors.GOLD
Colors.GOLDENROD
Colors.GRAY
Colors.GREEN
Colors.GREENYELLOW
Colors.GREY
Colors.HONEYDEW
Colors.HOTPINK
Colors.INDIANRED
Colors.INDIGO
Colors.IVORY
Colors.KHAKI
Colors.LAVENDER
Colors.LAVENDERBLUSH
Colors.LAWNGREEN
Colors.LEMONCHIFFON
Colors.LIGHT_BLUE
Colors.LIGHT_CORAL
Colors.LIGHT_CYAN
Colors.LIGHT_GOLDENRODYELLOW
Colors.LIGHT_GRAY
Colors.LIGHT_GREEN
Colors.LIGHT_GREY
Colors.LIGHT_PINK
Colors.LIGHT_SALMON
Colors.LIGHT_SEAGREEN
Colors.LIGHT_SKYBLUE
Colors.LIGHT_SLATEGRAY
Colors.LIGHT_SLATEGREY
Colors.LIGHT_STEELBLUE
Colors.LIGHT_YELLOW
Colors.LIME
Colors.LIMEGREEN
Colors.LINEN
Colors.MAGENTA
Colors.MAROON
Colors.MEDIUM_AQUAMARINE
Colors.MEDIUM_BLUE
Colors.MEDIUM_ORCHID
Colors.MEDIUM_PURPLE
Colors.MEDIUM_SEAGREEN
Colors.MEDIUM_SLATEBLUE
Colors.MEDIUM_SPRINGGREEN
Colors.MEDIUM_TURQUOISE
Colors.MEDIUM_VIOLETRED
Colors.MIDNIGHTBLUE
Colors.MINTCREAM
Colors.MISTYROSE
Colors.MOCCASIN
Colors.NAVAJOWHITE
Colors.NAVY
Colors.OLDLACE
Colors.OLIVE
Colors.OLIVEDRAB
Colors.ORANGE
Colors.ORANGERED
Colors.ORCHID
Colors.PALE_GOLDENROD
Colors.PALE_GREEN
Colors.PALE_TURQUOISE
Colors.PALE_VIOLETRED
Colors.PAPAYAWHIP
Colors.PEACHPUFF
Colors.PERU
Colors.PINK
Colors.PLUM
Colors.POWDERBLUE
Colors.PURPLE
Colors.REBECCAPURPLE
Colors.RED
Colors.ROSYBROWN
Colors.ROYALBLUE
Colors.SADDLEBROWN
Colors.SALMON
Colors.SANDYBROWN
Colors.SEAGREEN
Colors.SEASHELL
Colors.SIENNA
Colors.SILVER
Colors.SKYBLUE
Colors.SLATEBLUE
Colors.SLATEGRAY
Colors.SLATEGREY
Colors.SNOW
Colors.SPRINGGREEN
Colors.STEELBLUE
Colors.TAN
Colors.TEAL
Colors.THISTLE
Colors.TOMATO
Colors.TURQUOISE
Colors.VIOLET
Colors.WHEAT
Colors.WHITE
Colors.WHITESMOKE
Colors.YELLOW
Colors.YELLOWGREEN
```
*)

procedure ImportColorMath(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Color Math';

    addGlobalType('enum(RGB, HSV, HSL, XYZ, LAB, LCH, DELTAE)', 'EColorSpace');
    addGlobalType('array [0..2] of Single', 'TChannelMultipliers');

    addGlobalType('record B,G,R,A: Byte; end;', 'TColorBGRA');
    addGlobalType('record R,G,B: Byte;   end;', 'TColorRGB');
    addGlobalType('record X,Y,Z: Single; end;', 'TColorXYZ');
    addGlobalType('record L,A,B: Single; end;', 'TColorLAB');
    addGlobalType('record L,C,H: Single; end;', 'TColorLCH');
    addGlobalType('record H,S,V: Single; end;', 'TColorHSV');
    addGlobalType('record H,S,L: Single; end;', 'TColorHSL');

    addGlobalType('^TColorBGRA', 'PColorBGRA');

    addGlobalFunc('function TColor.ToBGRA: TColorBGRA', @_LapeTColorToBGRA);
    addGlobalFunc('function TColor.ToRGB: TColorRGB', @_LapeTColorToRGB);
    addGlobalFunc('function TColor.ToXYZ: TColorXYZ', @_LapeTColorToXYZ);
    addGlobalFunc('function TColor.ToLAB: TColorLAB', @_LapeTColorToLAB);
    addGlobalFunc('function TColor.ToLCH: TColorLCH', @_LapeTColorToLCH);
    addGlobalFunc('function TColor.ToHSV: TColorHSV', @_LapeTColorToHSV);
    addGlobalFunc('function TColor.ToHSL: TColorHSL', @_LapeTColorToHSL);

    addGlobalFunc(
      'function TColor.R: Byte;', [
      'type TRGB = packed record B,G,R,A: Byte; end;',
      'begin',
      '  Result := TRGB(Self).B;',
      'end;'
    ]);
    addGlobalFunc(
      'function TColor.G: Byte;', [
      'type TRGB = packed record B,G,R,A: Byte; end;',
      'begin',
      '  Result := TRGB(Self).G;',
      'end;'
    ]);
    addGlobalFunc(
      'function TColor.B: Byte;', [
      'type TRGB = packed record B,G,R,A: Byte; end;',
      'begin',
      '  Result := TRGB(Self).R;',
      'end;'
    ]);

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

    addGlobalFunc('function SimilarColors(Color1, Color2: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers): Boolean; overload', @_LapeSimilarColors1);
    addGlobalFunc('function SimilarColors(Color1, Color2: TColor; Tolerance: Single): Boolean; overload', @_LapeSimilarColors2);

    addGlobalFunc('function ColorDistance(Color1, Color2: TColor; const ColorSpace: EColorSpace; const Multipliers: TChannelMultipliers): Single; overload', @_LapeColorDistance1);
    addGlobalFunc('function ColorDistance(Color1, Color2: TColor): Single; overload', @_LapeColorDistance2);

    addGlobalType([
      'record',
      '  Color: TColor;',
      '  Mods: TChannelMultipliers;',
      '  Tolerance: Single;',
      'end;'],
      'TBestColor'
    );

    addGlobalFunc('function GetBestColor(Formula: EColorSpace; Colors: TColorArray): TBestColor;', @_LapeGetBestColor);

    addGlobalType([
      'record',
      '  const ALICEBLUE             = TColor($FFF8F0);',
      '  const ANTIQUEWHITE          = TColor($D7EBFA);',
      '  const AQUA                  = TColor($FFFF00);',
      '  const AQUAMARINE            = TColor($D4FF7F);',
      '  const AZURE                 = TColor($FFFFF0);',
      '  const BEIGE                 = TColor($DCF5F5);',
      '  const BISQUE                = TColor($C4E4FF);',
      '  const BLACK                 = TColor($000000);',
      '  const BLANCHEDALMOND        = TColor($CDEBFF);',
      '  const BLUE                  = TColor($FF0000);',
      '  const BLUEVIOLET            = TColor($E22B8A);',
      '  const BROWN                 = TColor($2A2AA5);',
      '  const BURLYWOOD             = TColor($87B8DE);',
      '  const CADET_BLUE            = TColor($A09E5F);',
      '  const CHARTREUSE            = TColor($00FF7F);',
      '  const CHOCOLATE             = TColor($1E69D2);',
      '  const CORAL                 = TColor($507FFF);',
      '  const CORNFLOWER_BLUE       = TColor($ED9564);',
      '  const CORNSILK              = TColor($DCF8FF);',
      '  const CRIMSON               = TColor($3C14DC);',
      '  const CYAN                  = TColor($FFFF00);',
      '  const DARK_BLUE             = TColor($8B0000);',
      '  const DARK_CYAN             = TColor($8B8B00);',
      '  const DARK_GOLDENROD        = TColor($0B86B8);',
      '  const DARK_GRAY             = TColor($A9A9A9);',
      '  const DARK_GREEN            = TColor($006400);',
      '  const DARK_GREY             = TColor($A9A9A9);',
      '  const DARK_KHAKI            = TColor($6BB7BD);',
      '  const DARK_MAGENTA          = TColor($8B008B);',
      '  const DARK_OLIVEGREEN       = TColor($2F6B55);',
      '  const DARK_ORANGE           = TColor($008CFF);',
      '  const DARK_ORCHID           = TColor($CC3299);',
      '  const DARK_RED              = TColor($00008B);',
      '  const DARK_SALMON           = TColor($7A96E9);',
      '  const DARK_SEAGREEN         = TColor($8FBC8F);',
      '  const DARK_SLATEBLUE        = TColor($8B3D48);',
      '  const DARK_SLATEGRAY        = TColor($4F4F2F);',
      '  const DARK_SLATEGREY        = TColor($4F4F2F);',
      '  const DARK_TURQUOISE        = TColor($D1CE00);',
      '  const DARK_VIOLET           = TColor($D30094);',
      '  const DEEPPINK              = TColor($9314FF);',
      '  const DEEPSKYBLUE           = TColor($FFBF00);',
      '  const DIMGRAY               = TColor($696969);',
      '  const DIMGREY               = TColor($696969);',
      '  const DODGERBLUE            = TColor($FF901E);',
      '  const FIREBRICK             = TColor($2222B2);',
      '  const FLORALWHITE           = TColor($F0FAFF);',
      '  const FORESTGREEN           = TColor($228B22);',
      '  const FUCHSIA               = TColor($FF00FF);',
      '  const GAINSBORO             = TColor($DCDCDC);',
      '  const GHOSTWHITE            = TColor($FFF8F8);',
      '  const GOLD                  = TColor($00D7FF);',
      '  const GOLDENROD             = TColor($20A5DA);',
      '  const GRAY                  = TColor($808080);',
      '  const GREEN                 = TColor($008000);',
      '  const GREENYELLOW           = TColor($2FFFAD);',
      '  const GREY                  = TColor($808080);',
      '  const HONEYDEW              = TColor($F0FFF0);',
      '  const HOTPINK               = TColor($B469FF);',
      '  const INDIANRED             = TColor($5C5CCD);',
      '  const INDIGO                = TColor($82004B);',
      '  const IVORY                 = TColor($F0FFFF);',
      '  const KHAKI                 = TColor($8CE6F0);',
      '  const LAVENDER              = TColor($FAE6E6);',
      '  const LAVENDERBLUSH         = TColor($F5F0FF);',
      '  const LAWNGREEN             = TColor($00FC7C);',
      '  const LEMONCHIFFON          = TColor($CDFAFF);',
      '  const LIGHT_BLUE            = TColor($E6D8AD);',
      '  const LIGHT_CORAL           = TColor($8080F0);',
      '  const LIGHT_CYAN            = TColor($FFFFE0);',
      '  const LIGHT_GOLDENRODYELLOW = TColor($D2FAFA);',
      '  const LIGHT_GRAY            = TColor($D3D3D3);',
      '  const LIGHT_GREEN           = TColor($90EE90);',
      '  const LIGHT_GREY            = TColor($D3D3D3);',
      '  const LIGHT_PINK            = TColor($C1B6FF);',
      '  const LIGHT_SALMON          = TColor($7AA0FF);',
      '  const LIGHT_SEAGREEN        = TColor($AAB220);',
      '  const LIGHT_SKYBLUE         = TColor($FACE87);',
      '  const LIGHT_SLATEGRAY       = TColor($998877);',
      '  const LIGHT_SLATEGREY       = TColor($998877);',
      '  const LIGHT_STEELBLUE       = TColor($DEC4B0);',
      '  const LIGHT_YELLOW          = TColor($E0FFFF);',
      '  const LIME                  = TColor($00FF00);',
      '  const LIMEGREEN             = TColor($32CD32);',
      '  const LINEN                 = TColor($E6F0FA);',
      '  const MAGENTA               = TColor($FF00FF);',
      '  const MAROON                = TColor($000080);',
      '  const MEDIUM_AQUAMARINE     = TColor($AACD66);',
      '  const MEDIUM_BLUE           = TColor($CD0000);',
      '  const MEDIUM_ORCHID         = TColor($D355BA);',
      '  const MEDIUM_PURPLE         = TColor($DB7093);',
      '  const MEDIUM_SEAGREEN       = TColor($71B33C);',
      '  const MEDIUM_SLATEBLUE      = TColor($EE687B);',
      '  const MEDIUM_SPRINGGREEN    = TColor($9AFA00);',
      '  const MEDIUM_TURQUOISE      = TColor($CCD148);',
      '  const MEDIUM_VIOLETRED      = TColor($8515C7);',
      '  const MIDNIGHTBLUE          = TColor($701919);',
      '  const MINTCREAM             = TColor($FAFFF5);',
      '  const MISTYROSE             = TColor($E1E4FF);',
      '  const MOCCASIN              = TColor($B5E4FF);',
      '  const NAVAJOWHITE           = TColor($ADDEFF);',
      '  const NAVY                  = TColor($800000);',
      '  const OLDLACE               = TColor($E6F5FD);',
      '  const OLIVE                 = TColor($008080);',
      '  const OLIVEDRAB             = TColor($238E6B);',
      '  const ORANGE                = TColor($00A5FF);',
      '  const ORANGERED             = TColor($0045FF);',
      '  const ORCHID                = TColor($D670DA);',
      '  const PALE_GOLDENROD        = TColor($AAE8EE);',
      '  const PALE_GREEN            = TColor($98FB98);',
      '  const PALE_TURQUOISE        = TColor($EEEEAF);',
      '  const PALE_VIOLETRED        = TColor($9370DB);',
      '  const PAPAYAWHIP            = TColor($D5EFFF);',
      '  const PEACHPUFF             = TColor($B9DAFF);',
      '  const PERU                  = TColor($3F85CD);',
      '  const PINK                  = TColor($CBC0FF);',
      '  const PLUM                  = TColor($DDA0DD);',
      '  const POWDERBLUE            = TColor($E6E0B0);',
      '  const PURPLE                = TColor($800080);',
      '  const REBECCAPURPLE         = TColor($993366);',
      '  const RED                   = TColor($0000FF);',
      '  const ROSYBROWN             = TColor($8F8FBC);',
      '  const ROYALBLUE             = TColor($E16941);',
      '  const SADDLEBROWN           = TColor($13458B);',
      '  const SALMON                = TColor($7280FA);',
      '  const SANDYBROWN            = TColor($60A4F4);',
      '  const SEAGREEN              = TColor($578B2E);',
      '  const SEASHELL              = TColor($EEF5FF);',
      '  const SIENNA                = TColor($2D52A0);',
      '  const SILVER                = TColor($C0C0C0);',
      '  const SKYBLUE               = TColor($EBCE87);',
      '  const SLATEBLUE             = TColor($CD5A6A);',
      '  const SLATEGRAY             = TColor($908070);',
      '  const SLATEGREY             = TColor($908070);',
      '  const SNOW                  = TColor($FAFAFF);',
      '  const SPRINGGREEN           = TColor($7FFF00);',
      '  const STEELBLUE             = TColor($B48246);',
      '  const TAN                   = TColor($8CB4D2);',
      '  const TEAL                  = TColor($808000);',
      '  const THISTLE               = TColor($D8BFD8);',
      '  const TOMATO                = TColor($4763FF);',
      '  const TURQUOISE             = TColor($D0E040);',
      '  const VIOLET                = TColor($EE82EE);',
      '  const WHEAT                 = TColor($B3DEF5);',
      '  const WHITE                 = TColor($FFFFFF);',
      '  const WHITESMOKE            = TColor($F5F5F5);',
      '  const YELLOW                = TColor($00FFFF);',
      '  const YELLOWGREEN           = TColor($32CD9A);',
      'end;'
    ], 'Colors');

    ImportingSection := '';
  end;
end;

end.

