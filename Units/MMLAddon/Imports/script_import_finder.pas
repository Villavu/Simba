unit script_import_finder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  script_imports, script_thread, lpcompiler, lptypes, mufasatypes, bitmaps;

procedure Lape_FindDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PBoolean(Result)^ := MFinder.FindDTM(MDTMs[PInt32(Params^[1])^], PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^);
end;

procedure Lape_FindDTMs(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PBoolean(Result)^ := MFinder.FindDTMs(MDTMs[PInt32(Params^[1])^], PPointArray(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^);
end;

procedure Lape_FindDTMRotatedAlternating(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PBoolean(Result)^ := MFinder.FindDTMRotated(MDTMs[PInt32(Params^[1])^], PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PExtended(Params^[8])^, PExtended(Params^[9])^, PExtended(Params^[10])^, PExtended(Params^[11])^, True);
end;

procedure Lape_FindDTMRotatedSE(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PBoolean(Result)^ := MFinder.FindDTMRotated(MDTMs[PInt32(Params^[1])^], PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PExtended(Params^[8])^, PExtended(Params^[9])^, PExtended(Params^[10])^, PExtended(Params^[11])^, False);
end;

procedure Lape_FindDTMsRotatedAlternating(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PBoolean(Result)^ := MFinder.FindDTMsRotated(MDTMs[PInt32(Params^[1])^], PPointArray(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PExtended(Params^[7])^, PExtended(Params^[8])^, PExtended(Params^[8])^, P2DExtendedArray(Params^[9])^, True);
end;

procedure Lape_FindDTMsRotatedSE(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PBoolean(Result)^ := MFinder.FindDTMsRotated(MDTMs[PInt32(Params^[1])^], PPointArray(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PExtended(Params^[7])^, PExtended(Params^[8])^, PExtended(Params^[8])^, P2DExtendedArray(Params^[9])^, False);
end;

procedure Lape_FindBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PBoolean(Result)^ := MFinder.FindBitmap(MBitmaps[PInt32(Params^[1])^], PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_FindBitmapIn(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PBoolean(Result)^ := MFinder.FindBitmapIn(MBitmaps[PInt32(Params^[1])^], PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^);
end;

procedure Lape_FindBitmapToleranceIn(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PBoolean(Result)^ := MFinder.FindBitmapToleranceIn(MBitmaps[PInt32(Params^[1])^], PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PInt32(Params^[8])^);
end;

procedure Lape_FindBitmapSpiral(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PBoolean(Result)^ := MFinder.FindBitmapSpiral(MBitmaps[PInt32(Params^[1])^], PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^);
end;

procedure Lape_FindBitmapsSpiralTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PBoolean(Result)^ := MFinder.FindBitmapsSpiralTolerance(MBitmaps[PInt32(Params^[1])^], PInt32(Params^[2])^, PInt32(Params^[3])^, PPointArray(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PInt32(Params^[8])^, PInt32(Params^[9])^);
end;

procedure Lape_FindBitmapSpiralTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PBoolean(Result)^ := MFinder.FindBitmapSpiralTolerance(MBitmaps[PInt32(Params^[1])^], PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PInt32(Params^[8])^);
end;

procedure Lape_GetColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PInt32(Result)^ := MFinder.GetColor(PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_GetColors(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PIntegerArray(Result)^ := MFinder.GetColors(PPointArray(Params^[1])^);
end;

procedure Lape_FindColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    Pboolean(Result)^ := MFinder.FindColor(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^);
end;

procedure Lape_findcolortolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    Pboolean(Result)^ := MFinder.FindColorTolerance(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PInt32(Params^[8])^);
end;

procedure Lape_FindColors(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PBoolean(Result)^ := MFinder.FindColors(PPointArray(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^);
end;

procedure Lape_SetColorToleranceSpeed(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MFinder.SetToleranceSpeed(PInt32(Params^[1])^);
end;

procedure Lape_GetToleranceSpeed(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PInt32(Result)^ := MFinder.GetToleranceSpeed();
end;

procedure Lape_SetToleranceSpeed2Modifiers(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MFinder.SetToleranceSpeed2Modifiers(PExtended(Params^[1])^, PExtended(Params^[2])^);
end;

procedure Lape_GetToleranceSpeed2Modifiers(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MFinder.GetToleranceSpeed2Modifiers(PExtended(Params^[1])^, PExtended(Params^[2])^);
end;

procedure Lape_SetToleranceSpeed3Modifier(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MFinder.SetToleranceSpeed3Modifier(PExtended(Params^[1])^);
end;

procedure Lape_GetToleranceSpeed3Modifier(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PExtended(Result)^ := MFinder.GetToleranceSpeed3Modifier();
end;

procedure Lape_SimilarColors(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
  Pboolean(Result)^ := MFinder.SimilarColors(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_CountColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PInt32(Result)^ := MFinder.CountColor(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^);
end;

procedure Lape_CountColorTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PInt32(Result)^ := MFinder.CountColorTolerance(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^);
end;

procedure Lape_FindColorsTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PBoolean(Result)^ := MFinder.FindColorsTolerance(PPointArray(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^);
end;

procedure Lape_FindColorSpiral(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PBoolean(Result)^ := MFinder.FindColorSpiral(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^);
end;

procedure Lape_FindColorSpiralTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PBoolean(Result)^ := MFinder.FindColorSpiralTolerance(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PInt32(Params^[8])^);
end;

procedure Lape_FindColorsSpiralTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    Pboolean(Result)^ := MFinder.FindColorsSpiralTolerance(PInt32(Params^[1])^, PInt32(Params^[2])^, PPointArray(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PInt32(Params^[8])^, PInt32(Params^[9])^);
end;

procedure Lape_FindColoredArea(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PBoolean(Result)^ := MFinder.FindColoredArea(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PInt32(Params^[8])^);
end;

procedure Lape_FindColoredAreaTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PBoolean(Result)^ := MFinder.FindColoredAreaTolerance(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PInt32(Params^[8])^, PInt32(Params^[9])^);
end;

procedure Lape_FindMaskTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PBoolean(Result)^ := MFinder.FindMaskTolerance(PMask(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PInt32(Params^[8])^, PInt32(Params^[9])^);
end;

procedure Lape_FindBitmapMaskTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PBoolean(Result)^ := MFinder.FindMaskTolerance(MBitmaps[PInt32(Params^[1])^].CreateTMask(), PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PInt32(Params^[8])^, PInt32(Params^[9])^);
end;

procedure Lape_FindDeformedBitmapToleranceIn(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PBoolean(Result)^ := MFinder.FindDeformedBitmapToleranceIn(MBitmaps[PInt32(Params^[1])^], PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PInt32(Params^[8])^, PInt32(Params^[9])^, PBoolean(Params^[10])^, PExtended(Params^[11])^);
end;

//function TMFinder.FindTemplateEx(Templ: TMufasaBitmap; out TPA: TPointArray; Formula: ETMFormula; xs,ys,xe,ye: Integer; MinMatch: Extended; DynamicAdjust: Boolean): Boolean;
procedure Lape_FindTemplateEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do // bitmap                     out TPA                   formula                  xs                  ys                  xe                  ye                  min match              dynamic adjust
    PBoolean(Result)^ := MFinder.FindTemplateEx(TMufasaBitmap(Params^[1]^), TPointArray(Params^[2]^), ETMFormula(Params^[3]^), Int32(Params^[4]^), Int32(Params^[5]^), Int32(Params^[6]^), Int32(Params^[7]^), Extended(Params^[8]^), Boolean(Params^[9]^));
end;

//function TMFinder.FindTemplateEx(Templ: TMufasaBitmap; out X,Y: Int32; Formula: ETMFormula; xs,ys,xe,ye: Integer; MinMatch: Extended; DynamicAdjust: Boolean): Boolean;
procedure Lape_FindTemplate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do // bitmap                   out X               out Y               formula                  xs                  ys,                 xe                  ye                  min match              dynamic adjust
    PBoolean(Result)^ := MFinder.FindTemplate(TMufasaBitmap(Params^[1]^), Int32(Params^[2]^), Int32(Params^[3]^), ETMFormula(Params^[4]^), Int32(Params^[5]^), Int32(Params^[6]^), Int32(Params^[7]^), Int32(Params^[8]^), Extended(Params^[9]^), Boolean(Params^[10]^));
end;

procedure Lape_Import_Finder(Compiler: TLapeCompiler; Data: Pointer);
begin
  with Compiler do
  begin
    addGlobalMethod('function FindDTM(DTM: Int32; var x, y: Int32; xs, ys, xe, ye: Int32): Boolean', @Lape_FindDTM, Data);
    addGlobalMethod('function FindDTMs(DTM: Int32; var p: TPointArray; xs, ys, xe, ye: Int32): Boolean', @Lape_FindDTMs, Data);
    addGlobalMethod('function FindDTMRotatedAlternating(DTM: Int32; var x, y: Int32; xs, ys, xe, ye: Int32; sAngle, eAngle, aStep: Extended; var aFound: Extended): Boolean', @Lape_FindDTMRotatedAlternating, Data);
    addGlobalMethod('function FindDTMRotatedSE(DTM: Int32; var x, y: Int32; xs, ys, xe, ye: Int32; sAngle, eAngle, aStep: Extended; var aFound: Extended): Boolean', @Lape_FindDTMRotatedSE, Data);
    addGlobalMethod('function FindDTMsRotatedAlternating(DTM: Int32; var Points: TPointArray; xs, ys, xe, ye: Int32; sAngle, eAngle, aStep: Extended; var aFound: T2DExtendedArray): Boolean', @Lape_FindDTMsRotatedAlternating, Data);
    addGlobalMethod('function FindDTMsRotatedSE(DTM: Int32; var Points: TPointArray; xs, ys, xe, ye: Int32; sAngle, eAngle, aStep: Extended; var aFound: T2DExtendedArray): Boolean', @Lape_FindDTMsRotatedSE, Data);
    addGlobalMethod('function FindBitmap(Bitmap: Int32; var x, y: Int32): Boolean', @Lape_FindBitmap, Data);
    addGlobalMethod('function FindBitmapIn(bitmap: Int32; var x, y: Int32; xs, ys, xe, ye: Int32): Boolean', @Lape_FindBitmapIn, Data);
    addGlobalMethod('function FindBitmapToleranceIn(bitmap: Int32; var x, y: Int32; xs, ys, xe, ye: Int32; tolerance: Int32): Boolean', @Lape_FindBitmapToleranceIn, Data);
    addGlobalMethod('function FindBitmapSpiral(bitmap: Int32; var x, y: Int32; xs, ys, xe, ye: Int32): Boolean', @Lape_FindBitmapSpiral, Data);
    addGlobalMethod('function FindBitmapsSpiralTolerance(bitmap: Int32; x, y: Int32; var Points: TPointArray; xs, ys, xe, ye,tolerance: Int32): Boolean', @Lape_FindBitmapsSpiralTolerance, Data);
    addGlobalMethod('function FindBitmapSpiralTolerance(bitmap: Int32; var x, y: Int32; xs, ys, xe, ye,tolerance: Int32): Boolean', @Lape_FindBitmapSpiralTolerance, Data);
    addGlobalMethod('function GetColor(x, y: Int32): Integer;', @Lape_GetColor, Data);
    addGlobalMethod('function GetColors(const Coords: TPointArray): TIntegerArray', @Lape_GetColors, Data);
    addGlobalMethod('function FindColor(var x, y: Int32; color, x1, y1, x2, y2: Int32): boolean', @Lape_FindColor, Data);
    addGlobalMethod('function FindColorTolerance(var x, y: Int32; color, x1, y1, x2, y2, tol: Int32): boolean', @Lape_FindColorTolerance, Data);
    addGlobalMethod('function FindColors(var TPA: TPointArray; Color, x1, y1, x2, y2: Int32): Boolean', @Lape_FindColors, Data);
    addGlobalMethod('procedure SetColorToleranceSpeed(CTS: Int32);', @Lape_SetColorToleranceSpeed, Data);
    addGlobalMethod('function GetToleranceSpeed: Int32', @Lape_GetToleranceSpeed, Data);
    addGlobalMethod('procedure SetToleranceSpeed2Modifiers(Hue, Sat: Extended);', @Lape_SetToleranceSpeed2Modifiers, Data);
    addGlobalMethod('procedure GetToleranceSpeed2Modifiers(var Hue, Sat: Extended);', @Lape_GetToleranceSpeed2Modifiers, Data);
    addGlobalMethod('procedure SetToleranceSpeed3Modifier(Modifier: Extended);', @Lape_SetToleranceSpeed3Modifier, Data);
    addGlobalMethod('function GetToleranceSpeed3Modifier: Extended;', @Lape_GetToleranceSpeed3Modifier, Data);
    addGlobalMethod('function SimilarColors(Color1, Color2, Tol: Int32): boolean', @Lape_SimilarColors, Data);
    addGlobalMethod('function CountColor(Color, xs, ys, xe, ye: Int32): Int32', @Lape_CountColor, Data);
    addGlobalMethod('function CountColorTolerance(Color, xs, ys, xe, ye, Tolerance: Int32): Int32', @Lape_CountColorTolerance, Data);
    addGlobalMethod('function FindColorsTolerance(var Points: TPointArray; Color, xs, ys, xe, ye, Tolerance: Int32): Boolean', @Lape_FindColorsTolerance, Data);
    addGlobalMethod('function FindColorSpiral(var x, y: Int32; color, xs, ys, xe, ye: Int32): Boolean', @Lape_FindColorSpiral, Data);
    addGlobalMethod('function FindColorSpiralTolerance(var x, y: Int32; color, xs, ys, xe, ye, Tol: Int32): Boolean', @Lape_FindColorSpiralTolerance, Data);
    addGlobalMethod('function FindColorsSpiralTolerance(x, y: Int32; var Points: TPointArray; color, xs, ys, xe, ye: Int32; Tolerance: Int32): Boolean', @Lape_FindColorsSpiralTolerance, Data);
    addGlobalMethod('function FindColoredArea(var x, y: Int32; color, xs, ys, xe, ye: Int32; MinArea: Int32): Boolean', @Lape_FindColoredArea, Data);
    addGlobalMethod('function FindColoredAreaTolerance(var x, y: Int32; Color, xs, ys, xe, ye, MinArea, Tol: Int32): Boolean', @Lape_FindColoredAreaTolerance, Data);
    addGlobalMethod('function FindMaskTolerance(const mask: TMask; var x, y: Int32; xs,ys, xe, ye: Int32; Tolerance, ContourTolerance: Int32): Boolean;', @Lape_FindMaskTolerance, Data);
    addGlobalMethod('function FindBitmapMaskTolerance(mask: Int32; var x, y: Int32; xs, ys, xe, ye: Int32; Tolerance, ContourTolerance: Int32): Boolean', @Lape_FindBitmapMaskTolerance, Data);
    addGlobalMethod('function FindDeformedBitmapToleranceIn(bitmap: Int32; var x, y: Int32; xs, ys, xe, ye: Int32; tolerance: Int32; Range: Int32; AllowPartialAccuracy: Boolean; var accuracy: Extended): Boolean', @Lape_FindDeformedBitmapToleranceIn, Data);
    
    addGlobalMethod('function FindTemplate(Templ: TMufasaBitmap; out x, y: Int32; Formula: ETMFormula; xs,ys,xe,ye: Int32; MinMatch: Extended; DynamicAdjust: Boolean = True): Boolean', @Lape_FindTemplate, Data);
    addGlobalMethod('function FindTemplateEx(Templ: TMufasaBitmap; out TPA: TPointArray; Formula: ETMFormula; xs,ys,xe,ye: Int32; MinMatch: Extended; DynamicAdjust: Boolean = True): Boolean', @Lape_FindTemplateEx, Data);
  end;
end;

initialization
  ScriptImports.Add('Finder', @Lape_Import_Finder);

end.

