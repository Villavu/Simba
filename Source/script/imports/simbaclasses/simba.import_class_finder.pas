unit simba.import_class_finder;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.script_compiler, simba.mufasatypes, simba.finder, simba.bitmap, simba.dtm, simba.matchtemplate;

type
  PObject = ^TObject;

procedure _LapeMFinder_FindColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindColor(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PBox(Params^[4])^);
end;

procedure _LapeMFinder_FindColorTolerance(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindColorTolerance(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PBox(Params^[5])^);
end;

procedure _LapeMFinder_FindColors(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindColors(PPointArray(Params^[1])^, PInteger(Params^[2])^, PBox(Params^[3])^);
end;

procedure _LapeMFinder_FindColorsTolerance(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindColorsTolerance(PPointArray(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PBox(Params^[4])^, PInteger(Params^[5])^);
end;

procedure _LapeMFinder_CountColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PMFinder(Params^[0])^.CountColor(PInteger(Params^[1])^, PBox(Params^[2])^);
end;

procedure _LapeMFinder_CountColorTolerance(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PMFinder(Params^[0])^.CountColorTolerance(PInteger(Params^[1])^, PInteger(Params^[2])^, PBox(Params^[3])^);
end;

procedure _LapeMFinder_FindBitmaps(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindBitmaps(PMufasaBitmap(Params^[1])^, PPointArray(Params^[2])^, PBox(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeMFinder_FindBitmap(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindBitmap(PMufasaBitmap(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PBox(Params^[4])^);
end;

procedure _LapeMFinder_FindBitmapsTolerance(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindBitmapsTolerance(PMufasaBitmap(Params^[1])^, PPointArray(Params^[2])^, PBox(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^);
end;

procedure _LapeMFinder_FindBitmapTolerance(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindBitmapTolerance(PMufasaBitmap(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PBox(Params^[4])^, PInteger(Params^[5])^);
end;

procedure _LapeMFinder_FindDTMs(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindDTMs(PDTM(Params^[1])^, PPointArray(Params^[2])^, PBox(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeMFinder_FindDTM(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindDTM(PDTM(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PBox(Params^[4])^);
end;

procedure _LapeMFinder_FindDTMsRotated(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindDTMsRotated(PDTM(Params^[1])^, PPointArray(Params^[2])^, PBox(Params^[3])^, PDouble(Params^[4])^, PDouble(Params^[5])^, PDouble(Params^[6])^, PDoubleArray(Params^[7])^, PInteger(Params^[8])^);
end;

procedure _LapeMFinder_FindDTMRotated(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindDTMRotated(PDTM(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PBox(Params^[4])^, PDouble(Params^[5])^, PDouble(Params^[6])^, PDouble(Params^[7])^, PDouble(Params^[8])^);
end;

procedure _LapeMFinder_FindTemplate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindTemplate(PMufasaBitmap(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PTMFormula(Params^[4])^, PBox(Params^[5])^, PExtended(Params^[6])^, PBoolean(Params^[7])^);
end;

procedure _LapeMFinder_FindTemplateEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindTemplateEx(PMufasaBitmap(Params^[1])^, PPointArray(Params^[2])^, PTMFormula(Params^[3])^, PBox(Params^[4])^, PExtended(Params^[5])^, PBoolean(Params^[6])^);
end;

procedure _LapeMFinder_SimilarColors(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.SimilarColors(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeMFinder_GetColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PMFinder(Params^[0])^.GetColor(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeMFinder_GetColors(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerArray(Result)^ := PMFinder(Params^[0])^.GetColors(PPointArray(Params^[1])^);
end;

procedure _LapeMFinder_GetColorsMatrix(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerMatrix(Result)^ := PMFinder(Params^[0])^.GetColorsMatrix(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeMFinder_SetToleranceSpeed(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMFinder(Params^[0])^.SetToleranceSpeed(PInteger(Params^[1])^);
end;

procedure _LapeMFinder_GetToleranceSpeed(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PMFinder(Params^[0])^.GetToleranceSpeed();
end;

procedure _LapeMFinder_SetToleranceSpeed2Modifiers(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMFinder(Params^[0])^.SetToleranceSpeed2Modifiers(PExtended(Params^[1])^, PExtended(Params^[2])^);
end;

procedure _LapeMFinder_GetToleranceSpeed2Modifiers(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMFinder(Params^[0])^.GetToleranceSpeed2Modifiers(PExtended(Params^[1])^, PExtended(Params^[2])^);
end;

procedure _LapeMFinder_SetToleranceSpeed3Modifier(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMFinder(Params^[0])^.SetToleranceSpeed3Modifier(PExtended(Params^[1])^);
end;

procedure _LapeMFinder_GetToleranceSpeed3Modifier(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PExtended(Result)^ := PMFinder(Params^[0])^.GetToleranceSpeed3Modifier();
end;

procedure _LapeMFinder_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMFinder(Params^[0])^ := TMFinder.Create(PObject(Params^[1])^);
end;

procedure ImportFinder(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClass('TMFinder');

    addGlobalFunc('function TMFinder.SimilarColors(Color1, Color2, Tolerance: Integer): Boolean', @_LapeMFinder_SimilarColors);

    addGlobalFunc('function TMFinder.GetColor(X, Y: Integer): Integer', @_LapeMFinder_GetColor);
    addGlobalFunc('function TMFinder.GetColors(Points: TPointArray): TIntegerArray', @_LapeMFinder_GetColors);
    addGlobalFunc('function TMFinder.GetColorsMatrix(X1, Y1, X2, Y2: Integer): TIntegerMatrix', @_LapeMFinder_GetColorsMatrix);

    addGlobalFunc('function TMFinder.FindColor(out X, Y: Integer; Color: Integer; Area: TBox): Boolean', @_LapeMFinder_FindColor);
    addGlobalFunc('function TMFinder.FindColorTolerance(out X, Y: Integer; Color, Tolerance: Integer; Area: TBox): Boolean', @_LapeMFinder_FindColorTolerance);
    addGlobalFunc('function TMFinder.FindColors(out TPA: TPointArray; Color: Integer; Area: TBox): Boolean', @_LapeMFinder_FindColors);
    addGlobalFunc('function TMFinder.FindColorsTolerance(out Points: TPointArray; Color, Tolerance: Integer; Area: TBox; MaxToFind: Integer = 0): Boolean;', @_LapeMFinder_FindColorsTolerance);

    addGlobalFunc('function TMFinder.CountColorTolerance(Color, Tolerance: Integer; Area: TBox): Integer', @_LapeMFinder_CountColorTolerance);
    addGlobalFunc('function TMFinder.CountColor(Color: Integer; Area: TBox): Integer', @_LapeMFinder_CountColor);

    addGlobalFunc('function TMFinder.FindBitmaps(Bitmap: TMufasaBitmap; out Points: TPointArray; Area: TBox; MaxToFind: Integer = 0): Boolean', @_LapeMFinder_FindBitmaps);
    addGlobalFunc('function TMFinder.FindBitmap(Bitmap: TMufasaBitmap; out X, Y: Integer; Area: TBox): Boolean', @_LapeMFinder_FindBitmap);

    addGlobalFunc('function TMFinder.FindBitmapsTolerance(Bitmap: TMufasaBitmap; out Points: TPointArray; Area: TBox; Tolerance: Integer; MaxToFind: Integer = 0): Boolean', @_LapeMFinder_FindBitmapsTolerance);
    addGlobalFunc('function TMFinder.FindBitmapTolerance(Bitmap: TMufasaBitmap; out X, Y: Integer; Area: TBox; Tolerance: Integer): Boolean', @_LapeMFinder_FindBitmapTolerance);
    addGlobalFunc('function TMFinder.FindDTMs(DTM: TDTM; out Points: TPointArray; Area: TBox; MaxToFind: Integer = 0): Boolean', @_LapeMFinder_FindDTMs);
    addGlobalFunc('function TMFinder.FindDTM(DTM: TDTM; out x, y: Integer; Area: TBox): Boolean', @_LapeMFinder_FindDTM);
    addGlobalFunc('function TMFinder.FindDTMsRotated(DTM: TDTM; out Points: TPointArray; Area: TBox; sAngle, eAngle, aStep: Double; out aFound: TDoubleArray; MaxToFind: Integer = 0): Boolean', @_LapeMFinder_FindDTMsRotated);
    addGlobalFunc('function TMFinder.FindDTMRotated(DTM: TDTM; out x, y: Integer; Area: TBox; sAngle, eAngle, aStep: Double; out aFound: Double): Boolean', @_LapeMFinder_FindDTMRotated);

    addGlobalFunc('function TMFinder.FindTemplate(TemplImage: TMufasaBitmap; out X, Y: Integer; Formula: ETMFormula; Area: TBox; MinMatch: Extended; DynamicAdjust: Boolean = True): Boolean', @_LapeMFinder_FindTemplate);
    addGlobalFunc('function TMFinder.FindTemplateEx(TemplImage: TMufasaBitmap; out TPA: TPointArray; Formula: ETMFormula; Area: TBox; MinMatch: Extended; DynamicAdjust: Boolean = True): Boolean', @_LapeMFinder_FindTemplateEx);

    addGlobalFunc('procedure TMFinder.SetToleranceSpeed(CTS: Integer);', @_LapeMFinder_SetToleranceSpeed);
    addGlobalFunc('function TMFinder.GetToleranceSpeed: Integer;', @_LapeMFinder_GetToleranceSpeed);
    addGlobalFunc('procedure TMFinder.SetToleranceSpeed2Modifiers(HueMod, SatMod: Extended);', @_LapeMFinder_SetToleranceSpeed2Modifiers);
    addGlobalFunc('procedure TMFinder.GetToleranceSpeed2Modifiers(out HueMod, SatMod: Extended);', @_LapeMFinder_GetToleranceSpeed2Modifiers);
    addGlobalFunc('procedure TMFinder.SetToleranceSpeed3Modifier(modifier: Extended);', @_LapeMFinder_SetToleranceSpeed3Modifier);
    addGlobalFunc('function TMFinder.GetToleranceSpeed3Modifier: Extended;', @_LapeMFinder_GetToleranceSpeed3Modifier);
    addGlobalFunc('procedure TMFinder.Init(aClient: TObject)', @_LapeMFinder_Init);
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportFinder);

end.

