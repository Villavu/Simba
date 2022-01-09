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

procedure _LapeMOCR_TextToMask(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMask(Result)^ := PMOCR(Params^[0])^.TextToMask(PString(Params^[1])^, PString(Params^[2])^);
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
  PPointArray(Result)^ := PMOCR(Params^[0])^.TextToFontTPA(PString(Params^[1])^, PFont(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

