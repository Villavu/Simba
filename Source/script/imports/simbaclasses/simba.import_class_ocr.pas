unit simba.import_class_ocr;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes, graphics,
  simba.script_compiler, simba.mufasatypes, simba.ocr, simba.bitmap, simba.fontloader;

type
  PObject = ^TObject;

procedure _LapeMOCR_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMOCR(Params^[0])^ := TMOCR.Create(PObject(Params^[1])^);
end;

procedure _LapeMOCR_GetTextAt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PMOCR(Params^[0])^.GetTextAt(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^, Pinteger(Params^[5])^, Pinteger(Params^[6])^, Pinteger(Params^[7])^, Pinteger(Params^[8])^, PString(Params^[9])^);
end;

procedure _LapeMOCR_GetTextAtEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PMOCR(Params^[0])^.GetTextAt(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^, Pinteger(Params^[5])^, Pinteger(Params^[6])^, Pinteger(Params^[7])^, Pinteger(Params^[8])^, Pinteger(Params^[9])^, PString(Params^[10])^);
end;

procedure _LapeMOCR_GetTextATPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PMOCR(Params^[0])^.GetTextATPA(P2DPointArray(Params^[1])^, Pinteger(Params^[2])^, PString(Params^[3])^);
end;

procedure _LapeMOCR_TextToFontTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := PMOCR(Params^[0])^.TextToFontTPA(PString(Params^[1])^, PString(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

procedure _LapeMOCR_TextToFontBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Result)^ := PMOCR(Params^[0])^.TextToFontBitmap(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeMOCR_Fonts_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFonts(Result)^ := PMOCR(Params^[0])^.Fonts;
end;

procedure _LapeMOCR_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMOCR(Params^[0])^.Free();
end;

procedure _LapeMOCR_TextToFontTPAEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := PMOCR(Params^[0])^.TextToFontTPA(PString(Params^[1])^, TFont(Params^[2]^), Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

procedure ImportOCR(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('Classes');

    addClass('TMOCR');
    addGlobalFunc('procedure TMOCR.Init(Owner: TObject)', @_LapeMOCR_Init);
    addGlobalFunc('function TMOCR.GetTextAt(atX, atY, minvspacing, maxvspacing, hspacing, color, tol, len: integer; font: string): string; overload', @_LapeMOCR_GetTextAt);
    addGlobalFunc('function TMOCR.GetTextAt(xs, ys, xe, ye, minvspacing, maxvspacing, hspacing, color, tol: integer; font: string): string; overload', @_LapeMOCR_GetTextAtEx);
    addGlobalFunc('function TMOCR.GetTextATPA(const ATPA: T2DPointArray; const maxvspacing: integer; font: string): string;', @_LapeMOCR_GetTextATPA);
    addGlobalFunc('function TMOCR.TextToFontTPA(Text, Font: String; out W, H: Int32): TPointArray; overload', @_LapeMOCR_TextToFontTPA);
    addGlobalFunc('function TMOCR.TextToFontTPA(Text: String; Font: TFont; out W, H: Int32): TPointArray; overload', @_LapeMOCR_TextToFontTPAEx);
    addGlobalFunc('function TMOCR.TextToFontBitmap(Text, font: String): TMufasaBitmap;', @_LapeMOCR_TextToFontBitmap);
    //addGlobalFunc('procedure TMOCR.Free;', @_LapeMOCR_Free);
    addClassVar('TMOCR', 'Fonts', 'TMFonts', @_LapeMOCR_Fonts_Read);

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportOCR);

end.

