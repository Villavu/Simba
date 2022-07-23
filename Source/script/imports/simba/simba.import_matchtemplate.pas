{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.import_matchtemplate;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.script_compiler, simba.mufasatypes, simba.bitmap, simba.bitmap_helpers, simba.matchtemplate;

procedure _LapeMatchTemplateCache_Create(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMatchTemplateCacheBase(Result)^ := MatchTemplateCache(PIntegerMatrix(Params^[0])^, PIntegerMatrix(Params^[1])^, PTMFormula(Params^[2])^);
end;

procedure _LapeMatchTemplateCache_CreateEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMatchTemplateCacheBase(Result)^ := MatchTemplateCache(PMufasaBitmap(Params^[0])^, PMufasaBitmap(Params^[1])^, PTMFormula(Params^[2])^);
end;

procedure _LapeMatchTemplateMaskCache(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Result)^ := MatchTemplateMask(PMatchTemplateCacheBase(Params^[0])^, PIntegerMatrix(Params^[1])^, PTMFormula(Params^[2])^);
end;

procedure _LapeMatchTemplateMaskCacheEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Result)^ := MatchTemplateMask(PMatchTemplateCacheBase(Params^[0])^, PMufasaBitmap(Params^[1])^, PTMFormula(Params^[2])^);
end;

procedure _LapeMatchTemplateMask(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Result)^ := MatchTemplateMask(PIntegerMatrix(Params^[0])^, PIntegerMatrix(Params^[1])^, PTMFormula(Params^[2])^);
end;

procedure _LapeMatchTemplate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Result)^ := MatchTemplate(PIntegerMatrix(Params^[0])^, PIntegerMatrix(Params^[1])^, PTMFormula(Params^[2])^);
end;

procedure _LapeMufasaBitmap_MatchTemplate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Result)^ := PMufasaBitmap(Params^[0])^.MatchTemplate(PMufasaBitmap(Params^[1])^, PTMFormula(Params^[2])^);
end;

procedure _LapeMufasaBitmap_MatchTemplateMask(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Result)^ := PMufasaBitmap(Params^[0])^.MatchTemplateMask(PMufasaBitmap(Params^[1])^, PTMFormula(Params^[2])^);
end;

procedure ImportMatchTemplate(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('Match Template');

    addClass('TMatchTemplateCache');
    addGlobalType('(TM_CCORR, TM_CCORR_NORMED, TM_CCOEFF, TM_CCOEFF_NORMED, TM_SQDIFF, TM_SQDIFF_NORMED)', 'ETMFormula');

    addGlobalFunc('function TMatchTemplateCache.Create(Image, Template: TIntegerMatrix; Formula: ETMFormula): TMatchTemplateCache; static; overload', @_LapeMatchTemplateCache_Create);
    addGlobalFunc('function TMatchTemplateCache.Create(Image, Template: TMufasaBitmap; Formula: ETMFormula): TMatchTemplateCache; static; overload', @_LapeMatchTemplateCache_CreateEx);

    addGlobalFunc('function MatchTemplateMask(Cache: TMatchTemplateCache; Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix; overload', @_LapeMatchTemplateMaskCache);
    addGlobalFunc('function MatchTemplateMask(Cache: TMatchTemplateCache; Template: TMufasaBitmap; Formula: ETMFormula): TSingleMatrix; overload', @_LapeMatchTemplateMaskCacheEx);
    addGlobalFunc('function MatchTemplateMask(Image, Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix; overload', @_LapeMatchTemplateMask);

    addGlobalFunc('function MatchTemplate(Image, Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix', @_LapeMatchTemplate);

    addGlobalFunc('function TMufasaBitmap.MatchTemplate(Template: TMufasaBitmap; Formula: ETMFormula): TSingleMatrix;', @_LapeMufasaBitmap_MatchTemplate);
    addGlobalFunc('function TMufasaBitmap.MatchTemplateMask(Template: TMufasaBitmap; Formula: ETMFormula): TSingleMatrix;', @_LapeMufasaBitmap_MatchTemplateMask);

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportMatchTemplate);

end.

