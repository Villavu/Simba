unit simba.script_import_ocr;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_OCR(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  simba.bitmap;

procedure Lape_MaskFromText(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PMask(Result)^ := MOCR.TextToMask(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure Lape_TPAFromText(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PPointArray(Result)^ := MOCR.TextToFontTPA(PString(Params^[0])^, PString(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_GetTextATPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    Pstring(Result)^ := MOCR.GetTextATPA(P2DPointArray(Params^[0])^, PInt32(Params^[1])^, Pstring(Params^[2])^);
end;

procedure Lape_GetTextAt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    Pstring(Result)^ := MOCR.GetTextAt(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, Pstring(Params^[8])^);
end;

procedure Lape_GetTextAtEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    Pstring(Result)^ := MOCR.GetTextAt(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PInt32(Params^[8])^, Pstring(Params^[9])^);
end;

procedure Lape_LoadSystemFont(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    Pboolean(Result)^ := MOCR.Fonts.LoadSystemFont(PFont(Params^[0])^, Pstring(Params^[1])^);
end;

procedure Lape_LoadFont(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    Pboolean(Result)^ := MOCR.Fonts.LoadFont(Pstring(Params^[0])^, Pboolean(Params^[1])^);
end;

procedure Lape_FreeFont(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    Pboolean(Result)^ := MOCR.Fonts.FreeFont(Pstring(Params^[0])^);
end;

procedure Lape_IsFontLoaded(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PBoolean(Result)^ := MOCR.Fonts.IsFontLoaded(Pstring(Params^[0])^);
end;

procedure Lape_FilterUpTextByCharacteristics(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MOCR.FilterUpTextByCharacteristics(PMufasaBitmap(Params^[0])^);
end;

procedure Lape_Import_OCR(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    Section := 'OCR';

    addGlobalFunc('function MaskFromText(Text, Font: String): TMask', @Lape_MaskFromText);
    addGlobalFunc('function TPAFromText(Text, Font: String; var W, H: Int32): TPointArray', @Lape_TPAFromText);
    addGlobalFunc('function GetTextATPA(const ATPA: T2DPointArray; const maxvspacing: integer; const font: string): String', @Lape_GetTextATPA);
    addGlobalFunc('function GetTextAt(const atX, atY, minvspacing, maxvspacing, hspacing, color, tol, len: integer; const font: string): String', @Lape_GetTextAt);
    addGlobalFunc('function GetTextAtEx(const xs,ys,xe,ye, minvspacing, maxvspacing, hspacing, color, tol: integer; const font: string): String', @Lape_GetTextAtEx);
    addGlobalFunc('function LoadSystemFont(Font: TFont; FontName: String): Boolean', @Lape_LoadSystemFont);
    addGlobalFunc('function LoadFont(FontName: String; Shadow: Boolean): Boolean', @Lape_LoadFont);
    addGlobalFunc('function FreeFont(FontName: String): Boolean', @Lape_FreeFont);
    addGlobalFunc('function IsFontLoaded(FontName: String): Boolean;', @Lape_IsFontLoaded);
    addGlobalFunc('procedure ocr_FilterUpTextByCharacteristics(bmp: TMufasaBitmap);', @Lape_FilterUpTextByCharacteristics);
  end;
end;

end.


