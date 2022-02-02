FSection := 'Match Template';

addClass('TMatchTemplateCache');

addGlobalType('(TM_CCORR, TM_CCORR_NORMED, TM_CCOEFF, TM_CCOEFF_NORMED, TM_SQDIFF, TM_SQDIFF_NORMED)', 'ETMFormula');

addGlobalFunc('function TMatchTemplateCache.Create(Image, Template: TIntegerMatrix): TMatchTemplateCache; static', @_LapeMatchTemplateCache_Create);

addGlobalFunc('function MatchTemplateMask(Image: TMatchTemplateCache; Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix; overload', @_LapeMatchTemplateMaskCache);
addGlobalFunc('function MatchTemplateMask(Image, Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix; overload', @_LapeMatchTemplateMask);

addGlobalFunc('function MatchTemplate(Image, Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix', @_LapeMatchTemplate);

addGlobalFunc('function TMufasaBitmap.MatchTemplate(Template: TMufasaBitmap; Formula: ETMFormula): TSingleMatrix; constref', @_LapeMufasaBitmap_MatchTemplate);
addGlobalFunc('function TMufasaBitmap.MatchTemplateMask(Template: TMufasaBitmap; Formula: ETMFormula): TSingleMatrix; constref', @_LapeMufasaBitmap_MatchTemplateMask);

