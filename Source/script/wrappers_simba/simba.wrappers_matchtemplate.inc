procedure _LapeMatchTemplateCache_Create(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMatchTemplateCache(Result)^ := SimbaMatchTemplate.CacheClass.Create(PIntegerMatrix(Params^[0])^, PIntegerMatrix(Params^[1])^);
end;

procedure _LapeMatchTemplateMaskCache(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Result)^ := SimbaMatchTemplate.MatchTemplateMask(PMatchTemplateCache(Params^[0])^, PIntegerMatrix(Params^[1])^, PTMFormula(Params^[2])^);
end;

procedure _LapeMatchTemplateMask(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Result)^ := SimbaMatchTemplate.MatchTemplateMask(PIntegerMatrix(Params^[0])^, PIntegerMatrix(Params^[1])^, PTMFormula(Params^[2])^);
end;

procedure _LapeMatchTemplate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Result)^ := SimbaMatchTemplate.MatchTemplate(PIntegerMatrix(Params^[0])^, PIntegerMatrix(Params^[1])^, PTMFormula(Params^[2])^);
end;

procedure _LapeMufasaBitmap_MatchTemplate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Result)^ := PMufasaBitmap(Params^[0])^.MatchTemplate(PMufasaBitmap(Params^[1])^, PTMFormula(Params^[2])^);
end;

procedure _LapeMufasaBitmap_MatchTemplateMask(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Result)^ := PMufasaBitmap(Params^[0])^.MatchTemplateMask(PMufasaBitmap(Params^[1])^, PTMFormula(Params^[2])^);
end;

