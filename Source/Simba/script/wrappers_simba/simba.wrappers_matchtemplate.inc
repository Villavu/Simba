procedure _LapeMatchTemplateGreyCache_Create(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMatchTemplateGreyCache(Result)^ := TMatchTemplateGreyCache.Create(P2DByteArray(Params^[0])^, P2DByteArray(Params^[1])^);
end;

procedure _LapeMatchTemplateRGBCache_Create(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMatchTemplateRGBCache(Result)^ := TMatchTemplateRGBCache.Create(P2DIntegerArray(Params^[0])^, P2DIntegerArray(Params^[1])^);
end;

procedure _LapeMatchTemplateMaskGrey(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Result)^ := MatchTemplateMask(PMatchTemplateGreyCache(Params^[0])^, P2DByteArray(Params^[1])^, PTMFormula(Params^[2])^);
end;

procedure _LapeMatchTemplateMaskGreyEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Result)^ := MatchTemplateMask(P2DByteArray(Params^[0])^, P2DByteArray(Params^[1])^, P2DByteArray(Params^[2])^, PTMFormula(Params^[3])^);
end;

procedure _LapeMatchTemplateMaskRGB(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Result)^ := MatchTemplateMask(PMatchTemplateRGBCache(Params^[0])^, P2DIntegerArray(Params^[1])^, PTMFormula(Params^[2])^);
end;

procedure _LapeMatchTemplateMaskRGBEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Result)^ := MatchTemplateMask(P2DIntegerArray(Params^[0])^, P2DIntegerArray(Params^[1])^, P2DIntegerArray(Params^[2])^, PTMFormula(Params^[3])^);
end;

procedure _LapeMatchTemplate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Result)^ := MatchTemplate(P2DIntegerArray(Params^[0])^, P2DIntegerArray(Params^[1])^, PTMFormula(Params^[2])^);
end;

procedure _LapeMufasaBitmap_MatchTemplate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Result)^ := PMufasaBitmap(Params^[0])^.MatchTemplate(PMufasaBitmap(Params^[1])^, PTMFormula(Params^[2])^);
end;

procedure _LapeMufasaBitmap_MatchTemplateMask(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Result)^ := PMufasaBitmap(Params^[0])^.MatchTemplateMask(PMufasaBitmap(Params^[1])^, PTMFormula(Params^[2])^);
end;

