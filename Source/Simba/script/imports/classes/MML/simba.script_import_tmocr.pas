unit simba.script_import_tmocr;
//Depends: TMOCR, TObject, string, Integer, boolean, T2DPointArray, integer, TOCRFilterDataArray, TMufasaBitmap, String, TMFonts

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_TMOCR(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  simba.ocr, simba.bitmap, simba.fontloader;

type
  PMOCR = ^TMOCR;
  PMFonts = ^TMFonts;
  PObject = ^TObject;

//constructor Create(Owner: TObject);
procedure TMOCR_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMOCR(Params^[0])^ := TMOCR.Create(PObject(Params^[1])^);
end;

//function getTextPointsIn(sx, sy, w, h: Integer; shadow: boolean; var _chars, _shadows: T2DPointArray): Boolean;
procedure TMOCR_getTextPointsIn(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMOCR(Params^[0])^.getTextPointsIn(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, Pboolean(Params^[5])^, P2DPointArray(Params^[6])^, P2DPointArray(Params^[7])^);
end;

//function GetUpTextAtEx(atX, atY: integer; shadow: boolean; fontname: string): string;
procedure TMOCR_GetUpTextAtEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PMOCR(Params^[0])^.GetUpTextAtEx(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pboolean(Params^[3])^, PlpString(Params^[4])^);
end;

//function GetUpTextAt(atX, atY: integer; shadow: boolean): string;
procedure TMOCR_GetUpTextAt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PMOCR(Params^[0])^.GetUpTextAt(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pboolean(Params^[3])^);
end;

//procedure CreateDefaultFilter;
procedure TMOCR_CreateDefaultFilter(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMOCR(Params^[0])^.CreateDefaultFilter();
end;

//procedure SetFilter(filter: TOCRFilterDataArray);
procedure TMOCR_SetFilter(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMOCR(Params^[0])^.SetFilter(POCRFilterDataArray(Params^[1])^);
end;

//procedure ResetFilter;
procedure TMOCR_ResetFilter(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMOCR(Params^[0])^.ResetFilter();
end;

//procedure FilterUpTextByColour(bmp: TMufasaBitmap);
procedure TMOCR_FilterUpTextByColour(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMOCR(Params^[0])^.FilterUpTextByColour(PMufasaBitmap(Params^[1])^);
end;

//procedure FilterUpTextByCharacteristics(bmp: TMufasaBitmap);
procedure TMOCR_FilterUpTextByCharacteristics(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMOCR(Params^[0])^.FilterUpTextByCharacteristics(PMufasaBitmap(Params^[1])^);
end;

//procedure FilterUpTextByColourMatches(bmp: TMufasaBitmap);
procedure TMOCR_FilterUpTextByColourMatches(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMOCR(Params^[0])^.FilterUpTextByColourMatches(PMufasaBitmap(Params^[1])^);
end;

//procedure FilterShadowBitmap(bmp: TMufasaBitmap);
procedure TMOCR_FilterShadowBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMOCR(Params^[0])^.FilterShadowBitmap(PMufasaBitmap(Params^[1])^);
end;

//procedure FilterCharsBitmap(bmp: TMufasaBitmap);
procedure TMOCR_FilterCharsBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMOCR(Params^[0])^.FilterCharsBitmap(PMufasaBitmap(Params^[1])^);
end;

//function GetTextAt(atX, atY, minvspacing, maxvspacing, hspacing, color, tol, len: integer; font: string): string;overload;
procedure TMOCR_GetTextAt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PMOCR(Params^[0])^.GetTextAt(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^, Pinteger(Params^[5])^, Pinteger(Params^[6])^, Pinteger(Params^[7])^, Pinteger(Params^[8])^, PlpString(Params^[9])^);
end;

//function GetTextAt(xs, ys, xe, ye, minvspacing, maxvspacing, hspacing, color, tol: integer; font: string): string;overload;
procedure TMOCR_GetTextAtEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PMOCR(Params^[0])^.GetTextAt(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^, Pinteger(Params^[5])^, Pinteger(Params^[6])^, Pinteger(Params^[7])^, Pinteger(Params^[8])^, Pinteger(Params^[9])^, PlpString(Params^[10])^);
end;

//function GetTextATPA(const ATPA: T2DPointArray; const maxvspacing: integer; font: string): string;
procedure TMOCR_GetTextATPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PMOCR(Params^[0])^.GetTextATPA(P2DPointArray(Params^[1])^, Pinteger(Params^[2])^, PlpString(Params^[3])^);
end;

//function TextToFontTPA(Text, font: String; out w, h: integer): TPointArray;
procedure TMOCR_TextToFontTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := PMOCR(Params^[0])^.TextToFontTPA(PlpString(Params^[1])^, PlpString(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

//function TextToFontBitmap(Text, font: String): TMufasaBitmap;
procedure TMOCR_TextToFontBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Result)^ := PMOCR(Params^[0])^.TextToFontBitmap(PlpString(Params^[1])^, PlpString(Params^[2])^);
end;

//function TextToMask(Text, font: String): TMask;
procedure TMOCR_TextToMask(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMask(Result)^ := PMOCR(Params^[0])^.TextToMask(PlpString(Params^[1])^, PlpString(Params^[2])^);
end;

//Read: property Fonts : TMFonts read GetFonts write SetFonts;
procedure TMOCR_Fonts_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFonts(Result)^ := PMOCR(Params^[0])^.Fonts;
end;

//procedure Free();
procedure TMOCR_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMOCR(Params^[0])^.Free();
end;

//  function TextToFontTPA(Text: String; Font: TFont; out W, H: Int32): TPointArray; overload;
procedure TMOCR_TextToFontTPAEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := PMOCR(Params^[0])^.TextToFontTPA(PlpString(Params^[1])^, PFont(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

procedure Lape_Import_TMOCR(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TMOCR');

    addGlobalFunc('procedure TMOCR.Init(Owner: TObject);', @TMOCR_Init);
    addGlobalFunc('function TMOCR.GetUpTextAtEx(atX, atY: integer; shadow: boolean; fontname: string): string; constref;', @TMOCR_GetUpTextAtEx);
    addGlobalFunc('function TMOCR.GetUpTextAt(atX, atY: integer; shadow: boolean): string; constref;', @TMOCR_GetUpTextAt);
    addGlobalFunc('function TMOCR.GetTextAt(atX, atY, minvspacing, maxvspacing, hspacing, color, tol, len: integer; font: string): string; constref;', @TMOCR_GetTextAt);
    addGlobalFunc('function TMOCR.GetTextAt(xs, ys, xe, ye, minvspacing, maxvspacing, hspacing, color, tol: integer; font: string): string; constref; overload;', @TMOCR_GetTextAtEx);
    addGlobalFunc('function TMOCR.GetTextATPA(const ATPA: T2DPointArray; const maxvspacing: integer; font: string): string; constref;', @TMOCR_GetTextATPA);
    addGlobalFunc('function TMOCR.TextToFontTPA(Text, Font: String; out W, H: Int32): TPointArray; constref; overload;', @TMOCR_TextToFontTPA);
    addGlobalFunc('function TMOCR.TextToFontTPA(Text: String; Font: TFont; out W, H: Int32): TPointArray; constref; overload;', @TMOCR_TextToFontTPAEx);
    addGlobalFunc('function TMOCR.TextToFontBitmap(Text, font: String): TMufasaBitmap; constref;', @TMOCR_TextToFontBitmap);
    addGlobalFunc('function TMOCR.TextToMask(Text, font: String): TMask; constref;', @TMOCR_TextToMask);
    //addGlobalFunc('procedure TMOCR.Free(); constref;', @TMOCR_Free);

    addClassVar('TMOCR', 'Fonts', 'TMFonts', @TMOCR_Fonts_Read);
  end;
end;

end.

