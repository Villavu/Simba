addClass('TMOCR');
addGlobalFunc('procedure TMOCR.Init(Owner: TObject)', @_LapeMOCR_Init);
addGlobalFunc('function TMOCR.GetTextAt(atX, atY, minvspacing, maxvspacing, hspacing, color, tol, len: integer; font: string): string; constref', @_LapeMOCR_GetTextAt);
addGlobalFunc('function TMOCR.GetTextAt(xs, ys, xe, ye, minvspacing, maxvspacing, hspacing, color, tol: integer; font: string): string; constref; overload', @_LapeMOCR_GetTextAtEx);
addGlobalFunc('function TMOCR.GetTextATPA(const ATPA: T2DPointArray; const maxvspacing: integer; font: string): string; constref', @_LapeMOCR_GetTextATPA);
addGlobalFunc('function TMOCR.TextToFontTPA(Text, Font: String; out W, H: Int32): TPointArray; constref; overload', @_LapeMOCR_TextToFontTPA);
addGlobalFunc('function TMOCR.TextToFontTPA(Text: String; Font: TFont; out W, H: Int32): TPointArray; constref; overload', @_LapeMOCR_TextToFontTPAEx);
addGlobalFunc('function TMOCR.TextToFontBitmap(Text, font: String): TMufasaBitmap; constref', @_LapeMOCR_TextToFontBitmap);
addGlobalFunc('function TMOCR.TextToMask(Text, font: String): TMask; constref', @_LapeMOCR_TextToMask);
//addGlobalFunc('procedure TMOCR.Free; constref', @_LapeMOCR_Free);
addClassVar('TMOCR', 'Fonts', 'TMFonts', @_LapeMOCR_Fonts_Read);

