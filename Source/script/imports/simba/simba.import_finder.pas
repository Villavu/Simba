unit simba.import_finder;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.script_compiler, simba.mufasatypes, simba.scriptthread, simba.finder, simba.bitmap, simba.matchtemplate;

procedure _LapeFindDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindDTM(MDTMs[PInteger(Params^[0])^], PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^);
end;

procedure _LapeFindDTMs(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindDTMs(MDTMs[PInteger(Params^[0])^], PPointArray(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^);
end;

procedure _LapeFindDTMRotatedAlternating(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindDTMRotated(MDTMs[PInteger(Params^[0])^], PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PExtended(Params^[7])^, PExtended(Params^[8])^, PExtended(Params^[9])^, PExtended(Params^[10])^, True);
end;

procedure _LapeFindDTMRotatedSE(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindDTMRotated(MDTMs[PInteger(Params^[0])^], PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PExtended(Params^[7])^, PExtended(Params^[8])^, PExtended(Params^[9])^, PExtended(Params^[10])^, False);
end;

procedure _LapeFindDTMsRotatedAlternating(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindDTMsRotated(MDTMs[PInteger(Params^[0])^], PPointArray(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PExtended(Params^[6])^, PExtended(Params^[7])^, PExtended(Params^[7])^, PDoubleArray(Params^[8])^, True);
end;

procedure _LapeFindDTMsRotatedSE(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindDTMsRotated(MDTMs[PInteger(Params^[0])^], PPointArray(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PExtended(Params^[6])^, PExtended(Params^[7])^, PExtended(Params^[7])^, PDoubleArray(Params^[8])^, False);
end;

procedure _LapeFindBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindBitmap(PMufasaBitmap(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeFindBitmapIn(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindBitmapIn(PMufasaBitmap(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^);
end;

procedure _LapeFindBitmapToleranceIn(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindBitmapToleranceIn(PMufasaBitmap(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^);
end;

procedure _LapeFindBitmapSpiral(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindBitmapSpiral(PMufasaBitmap(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^);
end;

procedure _LapeFindBitmapsSpiralTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindBitmapsSpiralTolerance(PMufasaBitmap(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PPointArray(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PInteger(Params^[8])^);
end;

procedure _LapeFindBitmapSpiralTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindBitmapSpiralTolerance(PMufasaBitmap(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^);
end;

procedure _LapeGetColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PInteger(Result)^ := MFinder.GetColor(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeGetColors(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PIntegerArray(Result)^ := MFinder.GetColors(PPointArray(Params^[0])^);
end;

procedure _LapeFindColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    Pboolean(Result)^ := MFinder.FindColor(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^);
end;

procedure _Lapefindcolortolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    Pboolean(Result)^ := MFinder.FindColorTolerance(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^);
end;

procedure _LapeFindColors(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindColors(PPointArray(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^);
end;

procedure _LapeSetColorToleranceSpeed(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    MFinder.SetToleranceSpeed(PInteger(Params^[0])^);
end;

procedure _LapeGetToleranceSpeed(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PInteger(Result)^ := MFinder.GetToleranceSpeed();
end;

procedure _LapeSetToleranceSpeed2Modifiers(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    MFinder.SetToleranceSpeed2Modifiers(PExtended(Params^[0])^, PExtended(Params^[1])^);
end;

procedure _LapeGetToleranceSpeed2Modifiers(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    MFinder.GetToleranceSpeed2Modifiers(PExtended(Params^[0])^, PExtended(Params^[1])^);
end;

procedure _LapeSetToleranceSpeed3Modifier(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    MFinder.SetToleranceSpeed3Modifier(PExtended(Params^[0])^);
end;

procedure _LapeGetToleranceSpeed3Modifier(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PExtended(Result)^ := MFinder.GetToleranceSpeed3Modifier();
end;

procedure _LapeSimilarColors(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
  Pboolean(Result)^ := MFinder.SimilarColors(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeCountColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PInteger(Result)^ := MFinder.CountColor(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeCountColorTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PInteger(Result)^ := MFinder.CountColorTolerance(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^);
end;

procedure _LapeFindColorsTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindColorsTolerance(PPointArray(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^);
end;

procedure _LapeFindColorSpiral(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindColorSpiral(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^);
end;

procedure _LapeFindColorSpiralTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindColorSpiralTolerance(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^);
end;

procedure _LapeFindColorsSpiralTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindColorsSpiralTolerance(PInteger(Params^[0])^, PInteger(Params^[1])^, PPointArray(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PInteger(Params^[8])^);
end;

procedure _LapeFindColoredArea(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindColoredArea(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^);
end;

procedure _LapeFindColoredAreaTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindColoredAreaTolerance(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PInteger(Params^[8])^);
end;

procedure _LapeFindDeformedBitmapToleranceIn(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindDeformedBitmapToleranceIn(PMufasaBitmap(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PInteger(Params^[8])^, PBoolean(Params^[9])^, PExtended(Params^[10])^);
end;

procedure _LapeFindTemplateEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do // bitmap                     out TPA                   formula                  xs                  ys                  xe                  ye                  min match              dynamic adjust
    PBoolean(Result)^ := MFinder.FindTemplateEx(TMufasaBitmap(Params^[0]^), TPointArray(Params^[1]^), ETMFormula(Params^[2]^), Int32(Params^[3]^), Int32(Params^[4]^), Int32(Params^[5]^), Int32(Params^[6]^), Extended(Params^[7]^), Boolean(Params^[8]^));
end;

procedure _LapeFindTemplate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do // bitmap                   out X               out Y               formula                  xs                  ys,                 xe                  ye                  min match              dynamic adjust
    PBoolean(Result)^ := MFinder.FindTemplate(TMufasaBitmap(Params^[0]^), Int32(Params^[1]^), Int32(Params^[2]^), ETMFormula(Params^[3]^), Int32(Params^[4]^), Int32(Params^[5]^), Int32(Params^[6]^), Int32(Params^[7]^), Extended(Params^[8]^), Boolean(Params^[9]^));
end;

procedure _LapeGetColorsMatrix(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PIntegerMatrix(Result)^ := IOManager.ReturnMatrix(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^ - PInteger(Params^[0])^ + 1, PInteger(Params^[3])^ - PInteger(Params^[1])^ + 1);
end;

procedure ImportFinder(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Finder';

    addGlobalFunc('function FindDTM(DTM: Integer; var x, y: Integer; xs, ys, xe, ye: Integer): Boolean', @_LapeFindDTM);
    addGlobalFunc('function FindDTMs(DTM: Integer; var p: TPointArray; xs, ys, xe, ye: Integer): Boolean', @_LapeFindDTMs);
    addGlobalFunc('function FindDTMRotatedAlternating(DTM: Integer; var x, y: Integer; xs, ys, xe, ye: Integer; sAngle, eAngle, aStep: Extended; var aFound: Extended): Boolean', @_LapeFindDTMRotatedAlternating);
    addGlobalFunc('function FindDTMRotatedSE(DTM: Integer; var x, y: Integer; xs, ys, xe, ye: Integer; sAngle, eAngle, aStep: Extended; var aFound: Extended): Boolean', @_LapeFindDTMRotatedSE);
    addGlobalFunc('function FindDTMsRotatedAlternating(DTM: Integer; var Points: TPointArray; xs, ys, xe, ye: Integer; sAngle, eAngle, aStep: Extended; var aFound: TDoubleArray): Boolean', @_LapeFindDTMsRotatedAlternating);
    addGlobalFunc('function FindDTMsRotatedSE(DTM: Integer; var Points: TPointArray; xs, ys, xe, ye: Integer; sAngle, eAngle, aStep: Extended; var aFound: TDoubleArray): Boolean', @_LapeFindDTMsRotatedSE);
    addGlobalFunc('function FindBitmap(Bitmap: TMufasaBitmap; var x, y: Integer): Boolean', @_LapeFindBitmap);
    addGlobalFunc('function FindBitmapIn(bitmap: TMufasaBitmap; var x, y: Integer; xs, ys, xe, ye: Integer): Boolean', @_LapeFindBitmapIn);
    addGlobalFunc('function FindBitmapToleranceIn(bitmap: TMufasaBitmap; var x, y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer): Boolean', @_LapeFindBitmapToleranceIn);
    addGlobalFunc('function FindBitmapSpiral(bitmap: TMufasaBitmap; var x, y: Integer; xs, ys, xe, ye: Integer): Boolean', @_LapeFindBitmapSpiral);
    addGlobalFunc('function FindBitmapsSpiralTolerance(bitmap: TMufasaBitmap; x, y: Integer; var Points: TPointArray; xs, ys, xe, ye,tolerance: Integer): Boolean', @_LapeFindBitmapsSpiralTolerance);
    addGlobalFunc('function FindBitmapSpiralTolerance(bitmap: TMufasaBitmap; var x, y: Integer; xs, ys, xe, ye,tolerance: Integer): Boolean', @_LapeFindBitmapSpiralTolerance);
    addGlobalFunc('function GetColor(x, y: Integer): Integer', @_LapeGetColor);
    addGlobalFunc('function GetColors(const Coords: TPointArray): TIntegerArray', @_LapeGetColors);
    addGlobalFunc('function GetColorsMatrix(X1, Y1, X2, Y2: Integer): TIntegerMatrix', @_LapeGetColorsMatrix);
    addGlobalFunc('function FindColor(var x, y: Integer; color, x1, y1, x2, y2: Integer): boolean', @_LapeFindColor);
    addGlobalFunc('function FindColorTolerance(var x, y: Integer; color, x1, y1, x2, y2, tol: Integer): boolean', @_LapeFindColorTolerance);
    addGlobalFunc('function FindColors(var TPA: TPointArray; Color, x1, y1, x2, y2: Integer): Boolean', @_LapeFindColors);
    addGlobalFunc('procedure SetColorToleranceSpeed(CTS: Integer)', @_LapeSetColorToleranceSpeed);
    addGlobalFunc('function GetToleranceSpeed: Integer', @_LapeGetToleranceSpeed);
    addGlobalFunc('procedure SetToleranceSpeed2Modifiers(Hue, Sat: Extended)', @_LapeSetToleranceSpeed2Modifiers);
    addGlobalFunc('procedure GetToleranceSpeed2Modifiers(var Hue, Sat: Extended)', @_LapeGetToleranceSpeed2Modifiers);
    addGlobalFunc('procedure SetToleranceSpeed3Modifier(Modifier: Extended)', @_LapeSetToleranceSpeed3Modifier);
    addGlobalFunc('function GetToleranceSpeed3Modifier: Extended', @_LapeGetToleranceSpeed3Modifier);
    addGlobalFunc('function SimilarColors(Color1, Color2, Tol: Integer): boolean', @_LapeSimilarColors);
    addGlobalFunc('function CountColor(Color, xs, ys, xe, ye: Integer): Integer', @_LapeCountColor);
    addGlobalFunc('function CountColorTolerance(Color, xs, ys, xe, ye, Tolerance: Integer): Integer', @_LapeCountColorTolerance);
    addGlobalFunc('function FindColorsTolerance(var Points: TPointArray; Color, xs, ys, xe, ye, Tolerance: Integer): Boolean', @_LapeFindColorsTolerance);
    addGlobalFunc('function FindColorSpiral(var x, y: Integer; color, xs, ys, xe, ye: Integer): Boolean', @_LapeFindColorSpiral);
    addGlobalFunc('function FindColorSpiralTolerance(var x, y: Integer; color, xs, ys, xe, ye, Tol: Integer): Boolean', @_LapeFindColorSpiralTolerance);
    addGlobalFunc('function FindColorsSpiralTolerance(x, y: Integer; var Points: TPointArray; color, xs, ys, xe, ye: Integer; Tolerance: Integer): Boolean', @_LapeFindColorsSpiralTolerance);
    addGlobalFunc('function FindColoredArea(var x, y: Integer; color, xs, ys, xe, ye: Integer; MinArea: Integer): Boolean', @_LapeFindColoredArea);
    addGlobalFunc('function FindColoredAreaTolerance(var x, y: Integer; Color, xs, ys, xe, ye, MinArea, Tol: Integer): Boolean', @_LapeFindColoredAreaTolerance);
    addGlobalFunc('function FindDeformedBitmapToleranceIn(bitmap: TMufasaBitmap; var x, y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer; Range: Integer; AllowPartialAccuracy: Boolean; var accuracy: Extended): Boolean', @_LapeFindDeformedBitmapToleranceIn);
    addGlobalFunc('function FindTemplate(Templ: TMufasaBitmap; out x, y: Integer; Formula: ETMFormula; xs,ys,xe,ye: Integer; MinMatch: Extended; DynamicAdjust: Boolean = True): Boolean', @_LapeFindTemplate);
    addGlobalFunc('function FindTemplateEx(Templ: TMufasaBitmap; out TPA: TPointArray; Formula: ETMFormula; xs,ys,xe,ye: Integer; MinMatch: Extended; DynamicAdjust: Boolean = True): Boolean', @_LapeFindTemplateEx);

    ImportingSection := '';
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportFinder);

end.

