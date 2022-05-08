pushSection('Classes');

addClass('TMOCR');
addGlobalFunc('procedure TMOCR.Init(Owner: TObject)', @_LapeMOCR_Init);
addGlobalFunc('function TMOCR.GetTextAt(atX, atY, minvspacing, maxvspacing, hspacing, color, tol, len: integer; font: string): string;', @_LapeMOCR_GetTextAt);
addGlobalFunc('function TMOCR.GetTextAt(xs, ys, xe, ye, minvspacing, maxvspacing, hspacing, color, tol: integer; font: string): string; overload', @_LapeMOCR_GetTextAtEx);
addGlobalFunc('function TMOCR.GetTextATPA(const ATPA: T2DPointArray; const maxvspacing: integer; font: string): string;', @_LapeMOCR_GetTextATPA);
addGlobalFunc('function TMOCR.TextToFontTPA(Text, Font: String; out W, H: Int32): TPointArray; overload', @_LapeMOCR_TextToFontTPA);
addGlobalFunc('function TMOCR.TextToFontTPA(Text: String; Font: TFont; out W, H: Int32): TPointArray; overload', @_LapeMOCR_TextToFontTPAEx);
addGlobalFunc('function TMOCR.TextToFontBitmap(Text, font: String): TMufasaBitmap;', @_LapeMOCR_TextToFontBitmap);
//addGlobalFunc('procedure TMOCR.Free;', @_LapeMOCR_Free);
addClassVar('TMOCR', 'Fonts', 'TMFonts', @_LapeMOCR_Fonts_Read);

popSection();

