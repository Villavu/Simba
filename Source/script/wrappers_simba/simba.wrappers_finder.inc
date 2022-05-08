procedure _LapeFindDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindDTM(MDTMs[PInt32(Params^[0])^], PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^);
end;

procedure _LapeFindDTMs(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindDTMs(MDTMs[PInt32(Params^[0])^], PPointArray(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^);
end;

procedure _LapeFindDTMRotatedAlternating(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindDTMRotated(MDTMs[PInt32(Params^[0])^], PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PExtended(Params^[7])^, PExtended(Params^[8])^, PExtended(Params^[9])^, PExtended(Params^[10])^, True);
end;

procedure _LapeFindDTMRotatedSE(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindDTMRotated(MDTMs[PInt32(Params^[0])^], PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PExtended(Params^[7])^, PExtended(Params^[8])^, PExtended(Params^[9])^, PExtended(Params^[10])^, False);
end;

procedure _LapeFindDTMsRotatedAlternating(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindDTMsRotated(MDTMs[PInt32(Params^[0])^], PPointArray(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PExtended(Params^[6])^, PExtended(Params^[7])^, PExtended(Params^[7])^, P2DExtendedArray(Params^[8])^, True);
end;

procedure _LapeFindDTMsRotatedSE(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindDTMsRotated(MDTMs[PInt32(Params^[0])^], PPointArray(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PExtended(Params^[6])^, PExtended(Params^[7])^, PExtended(Params^[7])^, P2DExtendedArray(Params^[8])^, False);
end;

procedure _LapeFindBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindBitmap(PMufasaBitmap(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure _LapeFindBitmapIn(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindBitmapIn(PMufasaBitmap(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^);
end;

procedure _LapeFindBitmapToleranceIn(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindBitmapToleranceIn(PMufasaBitmap(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^);
end;

procedure _LapeFindBitmapSpiral(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindBitmapSpiral(PMufasaBitmap(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^);
end;

procedure _LapeFindBitmapsSpiralTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindBitmapsSpiralTolerance(PMufasaBitmap(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PPointArray(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PInt32(Params^[8])^);
end;

procedure _LapeFindBitmapSpiralTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindBitmapSpiralTolerance(PMufasaBitmap(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^);
end;

procedure _LapeGetColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PInt32(Result)^ := MFinder.GetColor(PInt32(Params^[0])^, PInt32(Params^[1])^);
end;

procedure _LapeGetColors(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PIntegerArray(Result)^ := MFinder.GetColors(PPointArray(Params^[0])^);
end;

procedure _LapeFindColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    Pboolean(Result)^ := MFinder.FindColor(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^);
end;

procedure _Lapefindcolortolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    Pboolean(Result)^ := MFinder.FindColorTolerance(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^);
end;

procedure _LapeFindColors(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindColors(PPointArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^);
end;

procedure _LapeSetColorToleranceSpeed(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    MFinder.SetToleranceSpeed(PInt32(Params^[0])^);
end;

procedure _LapeGetToleranceSpeed(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PInt32(Result)^ := MFinder.GetToleranceSpeed();
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
  Pboolean(Result)^ := MFinder.SimilarColors(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure _LapeCountColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PInt32(Result)^ := MFinder.CountColor(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
end;

procedure _LapeCountColorTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PInt32(Result)^ := MFinder.CountColorTolerance(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^);
end;

procedure _LapeFindColorsTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindColorsTolerance(PPointArray(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^);
end;

procedure _LapeFindColorSpiral(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindColorSpiral(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^);
end;

procedure _LapeFindColorSpiralTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindColorSpiralTolerance(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^);
end;

procedure _LapeFindColorsSpiralTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindColorsSpiralTolerance(PInt32(Params^[0])^, PInt32(Params^[1])^, PPointArray(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PInt32(Params^[8])^);
end;

procedure _LapeFindColoredArea(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindColoredArea(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^);
end;

procedure _LapeFindColoredAreaTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindColoredAreaTolerance(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PInt32(Params^[8])^);
end;

procedure _LapeFindDeformedBitmapToleranceIn(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindDeformedBitmapToleranceIn(PMufasaBitmap(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PInt32(Params^[8])^, PBoolean(Params^[9])^, PExtended(Params^[10])^);
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

procedure _LapeFindTextMatrix(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PSingle(Result)^ := MFinder.FindTextMatrix(PString(Params^[0])^, PString(Params^[1])^, P2DIntegerArray(Params^[2])^, PBox(Params^[3])^);
end;

procedure _LapeFindTextColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PSingle(Result)^ := MFinder.FindTextColor(PString(Params^[0])^, PString(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PBox(Params^[8])^);
end;

procedure _LapeFindTextColorEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindTextColor(PString(Params^[0])^, PString(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PSingle(Params^[8])^);
end;

procedure _LapeFindText(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PSingle(Result)^ := MFinder.FindText(PString(Params^[0])^, PString(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PBox(Params^[6])^);
end;

procedure _LapeFindTextEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFinder.FindText(PString(Params^[0])^, PString(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PSingle(Params^[6])^);
end;

procedure _LapeGetColorsMatrix(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PIntegerMatrix(Result)^ := IOManager.ReturnMatrix(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^ - PInt32(Params^[0])^ + 1, PInt32(Params^[3])^ - PInt32(Params^[1])^ + 1);
end;

