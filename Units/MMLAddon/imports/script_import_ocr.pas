unit script_import_ocr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  script_imports, script_thread, lpcompiler, lptypes, mufasatypes, bitmaps;

procedure Lape_MaskFromText(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PMask(Result)^ := MOCR.TextToMask(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_TPAFromText(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PPointArray(Result)^ := MOCR.TextToFontTPA(PString(Params^[1])^, PString(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
end;

procedure Lape_GetTextATPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    Pstring(Result)^ := MOCR.GetTextATPA(P2DPointArray(Params^[1])^, PInt32(Params^[2])^, Pstring(Params^[3])^);
end;

procedure Lape_GetTextAt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    Pstring(Result)^ := MOCR.GetTextAt(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PInt32(Params^[8])^, Pstring(Params^[9])^);
end;

procedure Lape_GetTextAtEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    Pstring(Result)^ := MOCR.GetTextAt(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PInt32(Params^[8])^, PInt32(Params^[9])^, Pstring(Params^[10])^);
end;

procedure Lape_LoadSystemFont(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    Pboolean(Result)^ := MOCR.Fonts.LoadSystemFont(PFont(Params^[1])^, Pstring(Params^[2])^);
end;

procedure Lape_LoadFont(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    Pboolean(Result)^ := MOCR.Fonts.LoadFont(Pstring(Params^[1])^, Pboolean(Params^[2])^);
end;

procedure Lape_FreeFont(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    Pboolean(Result)^ := MOCR.Fonts.FreeFont(Pstring(Params^[1])^);
end;

procedure Lape_IsFontLoaded(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PBoolean(Result)^ := MOCR.Fonts.IsFontLoaded(Pstring(Params^[1])^);
end;

procedure Lape_FilterUpTextByCharacteristics(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MOCR.FilterUpTextByCharacteristics(PMufasaBitmap(Params^[1])^);
end;

procedure Lape_Import_OCR(Compiler: TLapeCompiler; Data: Pointer);
begin
  with Compiler do
  begin
    addGlobalMethod('function MaskFromText(Text, Font: String): TMask', @Lape_MaskFromText, Data);
    addGlobalMethod('function TPAFromText(Text, Font: String; var W, H: Int32): TPointArray', @Lape_TPAFromText, Data);
    addGlobalMethod('function GetTextATPA(constref ATPA: T2DPointArray; maxvspacing: Int32; font: String): String', @Lape_GetTextATPA, Data);
    addGlobalMethod('function GetTextAt(X, Y, minvspacing, maxvspacing, hspacing, color, tol, len: Int32; font: String): String', @Lape_GetTextAt, Data);
    addGlobalMethod('function GetTextAtEx(xs, ys, xe, ye, minvspacing, maxvspacing, hspacing, color, tol: Int32; font: String): String', @Lape_GetTextAtEx, Data);
    addGlobalMethod('function LoadSystemFont(Font: TFont; FontName: String): Boolean', @Lape_LoadSystemFont, Data);
    addGlobalMethod('function LoadFont(FontName: String; Shadow: Boolean): Boolean', @Lape_LoadFont, Data);
    addGlobalMethod('function FreeFont(FontName: String): Boolean', @Lape_FreeFont, Data);
    addGlobalMethod('function IsFontLoaded(FontName: String): Boolean;', @Lape_IsFontLoaded, Data);
    addGlobalMethod('procedure ocr_FilterUpTextByCharacteristics(bmp: TMufasaBitmap);', @Lape_FilterUpTextByCharacteristics, Data);
  end;
end;

initialization
  ScriptImports.Add('OCR', @Lape_Import_OCR);

end.

