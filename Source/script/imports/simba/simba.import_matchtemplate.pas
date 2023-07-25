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
  simba.script_compiler, simba.mufasatypes, simba.bitmap, simba.matchtemplate;

procedure _LapeMatchTemplateCache_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMatchTemplateCacheBase(Result)^ := MatchTemplateCache(PIntegerMatrix(Params^[0])^, PIntegerMatrix(Params^[1])^, PTMFormula(Params^[2])^);
end;

procedure _LapeMatchTemplateCache_CreateEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMatchTemplateCacheBase(Result)^ := MatchTemplateCache(PSimbaImage(Params^[0])^.ToMatrixBGR(), PSimbaImage(Params^[1])^.ToMatrixBGR(), PTMFormula(Params^[2])^);
end;

procedure _LapeMatchTemplateCache_FreeOnTerminate(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMatchTemplateCacheBase(Params^[0])^.FreeOnTerminate := PBoolean(Params^[1])^;
end;

procedure _LapeMatchTemplateMaskCache(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Result)^ := MatchTemplateMask(PMatchTemplateCacheBase(Params^[0])^, PIntegerMatrix(Params^[1])^, PTMFormula(Params^[2])^);
end;

procedure _LapeMatchTemplateMaskCacheEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Result)^ := MatchTemplateMask(PMatchTemplateCacheBase(Params^[0])^, PSimbaImage(Params^[1])^.ToMatrixBGR(), PTMFormula(Params^[2])^);
end;

procedure _LapeMatchTemplateMask(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Result)^ := MatchTemplateMask(PIntegerMatrix(Params^[0])^, PIntegerMatrix(Params^[1])^, PTMFormula(Params^[2])^);
end;

procedure _LapeMatchTemplate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Result)^ := MatchTemplate(PIntegerMatrix(Params^[0])^, PIntegerMatrix(Params^[1])^, PTMFormula(Params^[2])^);
end;

procedure _LapeMufasaBitmap_MatchTemplate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Result)^ := PSimbaImage(Params^[0])^.MatchTemplate(PSimbaImage(Params^[1])^, PTMFormula(Params^[2])^);
end;

procedure _LapeMufasaBitmap_MatchTemplateMask(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Result)^ := PSimbaImage(Params^[0])^.MatchTemplateMask(PSimbaImage(Params^[1])^, PTMFormula(Params^[2])^);
end;

procedure ImportMatchTemplate(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Match Template';

    addGlobalVar(ltBoolean, @MatchTemplateMT_Enabled, 'MatchTemplateMT_Enabled');
    addGlobalVar(ltInt32, @MatchTemplateMT_SliceHeight, 'MatchTemplateMT_SliceHeight');
    addGlobalVar(ltInt32, @MatchTemplateMT_SliceWidth, 'MatchTemplateMT_SliceWidth');

    addClass('TMatchTemplateCache');
    addGlobalType('(TM_CCORR, TM_CCORR_NORMED, TM_CCOEFF, TM_CCOEFF_NORMED, TM_SQDIFF, TM_SQDIFF_NORMED)', 'ETMFormula');

    addGlobalFunc('function TMatchTemplateCache.Create(Image, Template: TIntegerMatrix; Formula: ETMFormula): TMatchTemplateCache; static; overload', @_LapeMatchTemplateCache_Create);
    addGlobalFunc('function TMatchTemplateCache.Create(Image, Template: TSimbaImage; Formula: ETMFormula): TMatchTemplateCache; static; overload', @_LapeMatchTemplateCache_CreateEx);
    addGlobalFunc('procedure TMatchTemplateCache.FreeOnTerminate(Enable: Boolean);', @_LapeMatchTemplateCache_FreeOnTerminate);

    addGlobalFunc('function MatchTemplateMask(Cache: TMatchTemplateCache; Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix; overload', @_LapeMatchTemplateMaskCache);
    addGlobalFunc('function MatchTemplateMask(Cache: TMatchTemplateCache; Template: TSimbaImage; Formula: ETMFormula): TSingleMatrix; overload', @_LapeMatchTemplateMaskCacheEx);
    addGlobalFunc('function MatchTemplateMask(Image, Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix; overload', @_LapeMatchTemplateMask);

    addGlobalFunc('function MatchTemplate(Image, Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix', @_LapeMatchTemplate);

    addGlobalFunc('function TSimbaImage.MatchTemplate(Template: TSimbaImage; Formula: ETMFormula): TSingleMatrix;', @_LapeMufasaBitmap_MatchTemplate);
    addGlobalFunc('function TSimbaImage.MatchTemplateMask(Template: TSimbaImage; Formula: ETMFormula): TSingleMatrix;', @_LapeMufasaBitmap_MatchTemplateMask);

    ImportingSection := '';
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportMatchTemplate);

end.

