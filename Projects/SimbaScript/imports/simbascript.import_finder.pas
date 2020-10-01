unit simbascript.import_finder;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_Finder(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  simba.bitmap;

procedure Lape_FindDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PBoolean(Result)^ := MFinder.FindDTM(MDTMs[PInt32(Params^[0])^], PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^);
end;

procedure Lape_FindDTMs(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PBoolean(Result)^ := MFinder.FindDTMs(MDTMs[PInt32(Params^[0])^], PPointArray(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^);
end;

procedure Lape_FindDTMRotatedAlternating(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PBoolean(Result)^ := MFinder.FindDTMRotated(MDTMs[PInt32(Params^[0])^], PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PExtended(Params^[7])^, PExtended(Params^[8])^, PExtended(Params^[9])^, PExtended(Params^[10])^, True);
end;

procedure Lape_FindDTMRotatedSE(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PBoolean(Result)^ := MFinder.FindDTMRotated(MDTMs[PInt32(Params^[0])^], PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PExtended(Params^[7])^, PExtended(Params^[8])^, PExtended(Params^[9])^, PExtended(Params^[10])^, False);
end;

procedure Lape_FindDTMsRotatedAlternating(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PBoolean(Result)^ := MFinder.FindDTMsRotated(MDTMs[PInt32(Params^[0])^], PPointArray(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PExtended(Params^[6])^, PExtended(Params^[7])^, PExtended(Params^[7])^, P2DExtendedArray(Params^[8])^, True);
end;

procedure Lape_FindDTMsRotatedSE(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PBoolean(Result)^ := MFinder.FindDTMsRotated(MDTMs[PInt32(Params^[0])^], PPointArray(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PExtended(Params^[6])^, PExtended(Params^[7])^, PExtended(Params^[7])^, P2DExtendedArray(Params^[8])^, False);
end;

procedure Lape_FindBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PBoolean(Result)^ := MFinder.FindBitmap(MBitmaps[PInt32(Params^[0])^], PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_FindBitmapIn(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PBoolean(Result)^ := MFinder.FindBitmapIn(MBitmaps[PInt32(Params^[0])^], PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^);
end;

procedure Lape_FindBitmapToleranceIn(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PBoolean(Result)^ := MFinder.FindBitmapToleranceIn(MBitmaps[PInt32(Params^[0])^], PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^);
end;

procedure Lape_FindBitmapSpiral(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PBoolean(Result)^ := MFinder.FindBitmapSpiral(MBitmaps[PInt32(Params^[0])^], PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^);
end;

procedure Lape_FindBitmapsSpiralTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PBoolean(Result)^ := MFinder.FindBitmapsSpiralTolerance(MBitmaps[PInt32(Params^[0])^], PInt32(Params^[1])^, PInt32(Params^[2])^, PPointArray(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PInt32(Params^[8])^);
end;

procedure Lape_FindBitmapSpiralTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PBoolean(Result)^ := MFinder.FindBitmapSpiralTolerance(MBitmaps[PInt32(Params^[0])^], PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^);
end;

procedure Lape_GetColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PInt32(Result)^ := MFinder.GetColor(PInt32(Params^[0])^, PInt32(Params^[1])^);
end;

procedure Lape_GetColors(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PIntegerArray(Result)^ := MFinder.GetColors(PPointArray(Params^[0])^);
end;

procedure Lape_FindColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    Pboolean(Result)^ := MFinder.FindColor(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^);
end;

procedure Lape_findcolortolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    Pboolean(Result)^ := MFinder.FindColorTolerance(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^);
end;

procedure Lape_FindColors(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PBoolean(Result)^ := MFinder.FindColors(PPointArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^);
end;

procedure Lape_SetColorToleranceSpeed(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    MFinder.SetToleranceSpeed(PInt32(Params^[0])^);
end;

procedure Lape_GetToleranceSpeed(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PInt32(Result)^ := MFinder.GetToleranceSpeed();
end;

procedure Lape_SetToleranceSpeed2Modifiers(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    MFinder.SetToleranceSpeed2Modifiers(PExtended(Params^[0])^, PExtended(Params^[1])^);
end;

procedure Lape_GetToleranceSpeed2Modifiers(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    MFinder.GetToleranceSpeed2Modifiers(PExtended(Params^[0])^, PExtended(Params^[1])^);
end;

procedure Lape_SetToleranceSpeed3Modifier(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    MFinder.SetToleranceSpeed3Modifier(PExtended(Params^[0])^);
end;

procedure Lape_GetToleranceSpeed3Modifier(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PExtended(Result)^ := MFinder.GetToleranceSpeed3Modifier();
end;

procedure Lape_SimilarColors(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
  Pboolean(Result)^ := MFinder.SimilarColors(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_CountColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PInt32(Result)^ := MFinder.CountColor(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
end;

procedure Lape_CountColorTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PInt32(Result)^ := MFinder.CountColorTolerance(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^);
end;

procedure Lape_FindColorsTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PBoolean(Result)^ := MFinder.FindColorsTolerance(PPointArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^);
end;

procedure Lape_FindColorSpiral(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PBoolean(Result)^ := MFinder.FindColorSpiral(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^);
end;

procedure Lape_FindColorSpiralTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PBoolean(Result)^ := MFinder.FindColorSpiralTolerance(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^);
end;

procedure Lape_FindColorsSpiralTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    Pboolean(Result)^ := MFinder.FindColorsSpiralTolerance(PInt32(Params^[0])^, PInt32(Params^[1])^, PPointArray(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PInt32(Params^[8])^);
end;

procedure Lape_FindColoredArea(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PBoolean(Result)^ := MFinder.FindColoredArea(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^);
end;

procedure Lape_FindColoredAreaTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PBoolean(Result)^ := MFinder.FindColoredAreaTolerance(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PInt32(Params^[8])^);
end;

procedure Lape_FindMaskTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PBoolean(Result)^ := MFinder.FindMaskTolerance(PMask(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PInt32(Params^[8])^);
end;

procedure Lape_FindBitmapMaskTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PBoolean(Result)^ := MFinder.FindMaskTolerance(MBitmaps[PInt32(Params^[0])^].CreateTMask(), PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PInt32(Params^[8])^);
end;

procedure Lape_FindDeformedBitmapToleranceIn(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PBoolean(Result)^ := MFinder.FindDeformedBitmapToleranceIn(MBitmaps[PInt32(Params^[0])^], PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PInt32(Params^[8])^, PBoolean(Params^[9])^, PExtended(Params^[10])^);
end;

//function TMFinder.FindTemplateEx(Templ: TMufasaBitmap; out TPA: TPointArray; Formula: ETMFormula; xs,ys,xe,ye: Integer; MinMatch: Extended; DynamicAdjust: Boolean): Boolean;
procedure Lape_FindTemplateEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do // bitmap                     out TPA                   formula                  xs                  ys                  xe                  ye                  min match              dynamic adjust
    PBoolean(Result)^ := MFinder.FindTemplateEx(TMufasaBitmap(Params^[0]^), TPointArray(Params^[1]^), ETMFormula(Params^[2]^), Int32(Params^[3]^), Int32(Params^[4]^), Int32(Params^[5]^), Int32(Params^[6]^), Extended(Params^[7]^), Boolean(Params^[8]^));
end;

//function TMFinder.FindTemplateEx(Templ: TMufasaBitmap; out X,Y: Int32; Formula: ETMFormula; xs,ys,xe,ye: Integer; MinMatch: Extended; DynamicAdjust: Boolean): Boolean;
procedure Lape_FindTemplate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do // bitmap                   out X               out Y               formula                  xs                  ys,                 xe                  ye                  min match              dynamic adjust
    PBoolean(Result)^ := MFinder.FindTemplate(TMufasaBitmap(Params^[0]^), Int32(Params^[1]^), Int32(Params^[2]^), ETMFormula(Params^[3]^), Int32(Params^[4]^), Int32(Params^[5]^), Int32(Params^[6]^), Int32(Params^[7]^), Extended(Params^[8]^), Boolean(Params^[9]^));
end;

procedure Lape_FindTextMatrix(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PSingle(Result)^ := MFinder.FindTextMatrix(PString(Params^[0])^, PString(Params^[1])^, P2DIntegerArray(Params^[2])^, PBox(Params^[3])^);
end;

procedure Lape_FindTextColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PSingle(Result)^ := MFinder.FindTextColor(PString(Params^[0])^, PString(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PBox(Params^[8])^);
end;

procedure Lape_FindTextColorEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PBoolean(Result)^ := MFinder.FindTextColor(PString(Params^[0])^, PString(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PSingle(Params^[8])^);
end;

procedure Lape_FindText(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PSingle(Result)^ := MFinder.FindText(PString(Params^[0])^, PString(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PBox(Params^[6])^);
end;

procedure Lape_FindTextEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PBoolean(Result)^ := MFinder.FindText(PString(Params^[0])^, PString(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PSingle(Params^[6])^);
end;

procedure Lape_GetColorsMatrix(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with ScriptInstance.Client do
    PIntegerMatrix(Result)^ := IOManager.ReturnMatrix(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^ - PInt32(Params^[0])^ + 1, PInt32(Params^[3])^ - PInt32(Params^[1])^ + 1);
end;

procedure Lape_Import_Finder(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    Section := 'Finder';

    addGlobalFunc('function FindDTM(DTM: Int32; var x, y: Int32; xs, ys, xe, ye: Int32): Boolean', @Lape_FindDTM);
    addGlobalFunc('function FindDTMs(DTM: Int32; var p: TPointArray; xs, ys, xe, ye: Int32): Boolean', @Lape_FindDTMs);
    addGlobalFunc('function FindDTMRotatedAlternating(DTM: Int32; var x, y: Int32; xs, ys, xe, ye: Int32; sAngle, eAngle, aStep: Extended; var aFound: Extended): Boolean', @Lape_FindDTMRotatedAlternating);
    addGlobalFunc('function FindDTMRotatedSE(DTM: Int32; var x, y: Int32; xs, ys, xe, ye: Int32; sAngle, eAngle, aStep: Extended; var aFound: Extended): Boolean', @Lape_FindDTMRotatedSE);
    addGlobalFunc('function FindDTMsRotatedAlternating(DTM: Int32; var Points: TPointArray; xs, ys, xe, ye: Int32; sAngle, eAngle, aStep: Extended; var aFound: T2DExtendedArray): Boolean', @Lape_FindDTMsRotatedAlternating);
    addGlobalFunc('function FindDTMsRotatedSE(DTM: Int32; var Points: TPointArray; xs, ys, xe, ye: Int32; sAngle, eAngle, aStep: Extended; var aFound: T2DExtendedArray): Boolean', @Lape_FindDTMsRotatedSE);
    addGlobalFunc('function FindBitmap(Bitmap: Int32; var x, y: Int32): Boolean', @Lape_FindBitmap);
    addGlobalFunc('function FindBitmapIn(bitmap: Int32; var x, y: Int32; xs, ys, xe, ye: Int32): Boolean', @Lape_FindBitmapIn);
    addGlobalFunc('function FindBitmapToleranceIn(bitmap: Int32; var x, y: Int32; xs, ys, xe, ye: Int32; tolerance: Int32): Boolean', @Lape_FindBitmapToleranceIn);
    addGlobalFunc('function FindBitmapSpiral(bitmap: Int32; var x, y: Int32; xs, ys, xe, ye: Int32): Boolean', @Lape_FindBitmapSpiral);
    addGlobalFunc('function FindBitmapsSpiralTolerance(bitmap: Int32; x, y: Int32; var Points: TPointArray; xs, ys, xe, ye,tolerance: Int32): Boolean', @Lape_FindBitmapsSpiralTolerance);
    addGlobalFunc('function FindBitmapSpiralTolerance(bitmap: Int32; var x, y: Int32; xs, ys, xe, ye,tolerance: Int32): Boolean', @Lape_FindBitmapSpiralTolerance);
    addGlobalFunc('function GetColor(x, y: Int32): Integer;', @Lape_GetColor);
    addGlobalFunc('function GetColors(const Coords: TPointArray): TIntegerArray;', @Lape_GetColors);
    addGlobalFunc('function GetColorsMatrix(X1, Y1, X2, Y2: Int32): TIntegerMatrix;', @Lape_GetColorsMatrix);
    addGlobalFunc('function FindColor(var x, y: Int32; color, x1, y1, x2, y2: Int32): boolean', @Lape_FindColor);
    addGlobalFunc('function FindColorTolerance(var x, y: Int32; color, x1, y1, x2, y2, tol: Int32): boolean', @Lape_FindColorTolerance);
    addGlobalFunc('function FindColors(var TPA: TPointArray; Color, x1, y1, x2, y2: Int32): Boolean', @Lape_FindColors);
    addGlobalFunc('procedure SetColorToleranceSpeed(CTS: Int32);', @Lape_SetColorToleranceSpeed);
    addGlobalFunc('function GetToleranceSpeed: Int32', @Lape_GetToleranceSpeed);
    addGlobalFunc('procedure SetToleranceSpeed2Modifiers(Hue, Sat: Extended);', @Lape_SetToleranceSpeed2Modifiers);
    addGlobalFunc('procedure GetToleranceSpeed2Modifiers(var Hue, Sat: Extended);', @Lape_GetToleranceSpeed2Modifiers);
    addGlobalFunc('procedure SetToleranceSpeed3Modifier(Modifier: Extended);', @Lape_SetToleranceSpeed3Modifier);
    addGlobalFunc('function GetToleranceSpeed3Modifier: Extended;', @Lape_GetToleranceSpeed3Modifier);
    addGlobalFunc('function SimilarColors(Color1, Color2, Tol: Int32): boolean', @Lape_SimilarColors);
    addGlobalFunc('function CountColor(Color, xs, ys, xe, ye: Int32): Int32', @Lape_CountColor);
    addGlobalFunc('function CountColorTolerance(Color, xs, ys, xe, ye, Tolerance: Int32): Int32', @Lape_CountColorTolerance);
    addGlobalFunc('function FindColorsTolerance(var Points: TPointArray; Color, xs, ys, xe, ye, Tolerance: Int32): Boolean', @Lape_FindColorsTolerance);
    addGlobalFunc('function FindColorSpiral(var x, y: Int32; color, xs, ys, xe, ye: Int32): Boolean', @Lape_FindColorSpiral);
    addGlobalFunc('function FindColorSpiralTolerance(var x, y: Int32; color, xs, ys, xe, ye, Tol: Int32): Boolean', @Lape_FindColorSpiralTolerance);
    addGlobalFunc('function FindColorsSpiralTolerance(x, y: Int32; var Points: TPointArray; color, xs, ys, xe, ye: Int32; Tolerance: Int32): Boolean', @Lape_FindColorsSpiralTolerance);
    addGlobalFunc('function FindColoredArea(var x, y: Int32; color, xs, ys, xe, ye: Int32; MinArea: Int32): Boolean', @Lape_FindColoredArea);
    addGlobalFunc('function FindColoredAreaTolerance(var x, y: Int32; Color, xs, ys, xe, ye, MinArea, Tol: Int32): Boolean', @Lape_FindColoredAreaTolerance);
    addGlobalFunc('function FindMaskTolerance(const mask: TMask; var x, y: Int32; xs,ys, xe, ye: Int32; Tolerance, ContourTolerance: Int32): Boolean;', @Lape_FindMaskTolerance);
    addGlobalFunc('function FindBitmapMaskTolerance(mask: Int32; var x, y: Int32; xs, ys, xe, ye: Int32; Tolerance, ContourTolerance: Int32): Boolean', @Lape_FindBitmapMaskTolerance);
    addGlobalFunc('function FindDeformedBitmapToleranceIn(bitmap: Int32; var x, y: Int32; xs, ys, xe, ye: Int32; tolerance: Int32; Range: Int32; AllowPartialAccuracy: Boolean; var accuracy: Extended): Boolean', @Lape_FindDeformedBitmapToleranceIn);

    addGlobalFunc('function FindTemplate(Templ: TMufasaBitmap; out x, y: Int32; Formula: ETMFormula; xs,ys,xe,ye: Int32; MinMatch: Extended; DynamicAdjust: Boolean = True): Boolean', @Lape_FindTemplate);
    addGlobalFunc('function FindTemplateEx(Templ: TMufasaBitmap; out TPA: TPointArray; Formula: ETMFormula; xs,ys,xe,ye: Int32; MinMatch: Extended; DynamicAdjust: Boolean = True): Boolean', @Lape_FindTemplateEx);

    addGlobalFunc('function FindTextMatrix(Text, Font: String; constref Matrix: T2DIntegerArray; out Bounds: TBox): Single;', @Lape_FindTextMatrix);
    addGlobalFunc('function FindTextColor(Text, Font: String; Color, Tolerance: Int32; X1, Y1, X2, Y2: Int32; out Bounds: TBox): Single; overload;', @Lape_FindTextColor);
    addGlobalFunc('function FindTextColor(Text, Font: String; Color, Tolerance: Int32; X1, Y1, X2, Y2: Int32; MinMatch: Single = 1): Boolean; overload;', @Lape_FindTextColorEx);
    addGlobalFunc('function FindText(Text, Font: String; X1, Y1, X2, Y2: Int32; out Bounds: TBox): Single; overload;', @Lape_FindText);
    addGlobalFunc('function FindText(Text, Font: String; X1, Y1, X2, Y2: Int32; MinMatch: Single = 1): Boolean; overload;', @Lape_FindTextEx);
  end;
end;
end.


