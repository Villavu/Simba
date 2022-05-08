pushSection('OCR');

addGlobalFunc('function TPAFromText(Text, Font: String; var W, H: Int32): TPointArray', @_LapeTPAFromText);
addGlobalFunc('function GetTextATPA(const ATPA: T2DPointArray; const maxvspacing: integer; const font: string): String', @_LapeGetTextATPA);
addGlobalFunc('function GetTextAt(const atX, atY, minvspacing, maxvspacing, hspacing, color, tol, len: integer; const font: string): String', @_LapeGetTextAt);
addGlobalFunc('function GetTextAtEx(const xs,ys,xe,ye, minvspacing, maxvspacing, hspacing, color, tol: integer; const font: string): String', @_LapeGetTextAtEx);
addGlobalFunc('function LoadSystemFont(Font: TFont; FontName: String): Boolean', @_LapeLoadSystemFont);
addGlobalFunc('function LoadFont(FontName: String; Shadow: Boolean): Boolean', @_LapeLoadFont);
addGlobalFunc('function FreeFont(FontName: String): Boolean', @_LapeFreeFont);
addGlobalFunc('function IsFontLoaded(FontName: String): Boolean', @_LapeIsFontLoaded);

popSection();
