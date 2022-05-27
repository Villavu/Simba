unit simba.import_ocr;

{$i simba.inc}

interface

uses
  classes, sysutils,
  lptypes, ffi,
  simba.script_compiler;

implementation

uses
  graphics,
  simba.mufasatypes, simba.ocr, simba.fontloader, simba.scriptthread, simba.bitmap;

procedure _LapeTPAFromText(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PPointArray(Result)^ := MOCR.TextToFontTPA(PString(Params^[0])^, PString(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure _LapeBitmapFromText(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PMufasaBitmap(Result)^ := MOCR.TextToFontBitmap(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeGetTextATPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    Pstring(Result)^ := MOCR.GetTextATPA(P2DPointArray(Params^[0])^, PInt32(Params^[1])^, Pstring(Params^[2])^);
end;

procedure _LapeGetTextAt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    Pstring(Result)^ := MOCR.GetTextAt(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, Pstring(Params^[8])^);
end;

procedure _LapeGetTextAtEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    Pstring(Result)^ := MOCR.GetTextAt(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PInt32(Params^[8])^, Pstring(Params^[9])^);
end;

procedure _LapeLoadSystemFont(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    Pboolean(Result)^ := MOCR.Fonts.LoadSystemFont(TFont(Params^[0]^), Pstring(Params^[1])^);
end;

procedure _LapeLoadFont(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    Pboolean(Result)^ := MOCR.Fonts.LoadFont(Pstring(Params^[0])^, Pboolean(Params^[1])^);
end;

procedure _LapeFreeFont(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    Pboolean(Result)^ := MOCR.Fonts.FreeFont(Pstring(Params^[0])^);
end;

procedure _LapeIsFontLoaded(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MOCR.Fonts.IsFontLoaded(Pstring(Params^[0])^);
end;

procedure ImportOCR(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('OCR');

    addGlobalFunc('function TPAFromText(Text, Font: String; var W, H: Int32): TPointArray', @_LapeTPAFromText);
    addGlobalFunc('function BitmapFromText(Text, Font: String): TMufasaBitmap', @_LapeBitmapFromText);
    addGlobalFunc('function GetTextATPA(const ATPA: T2DPointArray; const maxvspacing: integer; const font: string): String', @_LapeGetTextATPA);
    addGlobalFunc('function GetTextAt(const atX, atY, minvspacing, maxvspacing, hspacing, color, tol, len: integer; const font: string): String', @_LapeGetTextAt);
    addGlobalFunc('function GetTextAtEx(const xs,ys,xe,ye, minvspacing, maxvspacing, hspacing, color, tol: integer; const font: string): String', @_LapeGetTextAtEx);
    addGlobalFunc('function LoadSystemFont(Font: TFont; FontName: String): Boolean', @_LapeLoadSystemFont);
    addGlobalFunc('function LoadFont(FontName: String; Shadow: Boolean): Boolean', @_LapeLoadFont);
    addGlobalFunc('function FreeFont(FontName: String): Boolean', @_LapeFreeFont);
    addGlobalFunc('function IsFontLoaded(FontName: String): Boolean', @_LapeIsFontLoaded);

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportOCR);

end.

