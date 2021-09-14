FSection := 'Match Template';

addGlobalType(
  '(TM_CCORR, TM_CCORR_NORMED, TM_CCOEFF, TM_CCOEFF_NORMED, TM_SQDIFF, TM_SQDIFF_NORMED)', 'ETMFormula'
);

addGlobalType(
  'packed record                                                            ' + LineEnding +
  '  Width: Int32;                                                          ' + LineEnding +
  '  Height: Int32;                                                         ' + LineEnding +
  '                                                                         ' + LineEnding +
  '  ImageSpec: array of array of packed record Re, Im: Single; end;        ' + LineEnding +
  '  ImageSquaredSpec: array of array of packed record Re, Im: Single; end; ' + LineEnding +
  '                                                                         ' + LineEnding +
  '  MaskChannel: TSingleMatrix;                                            ' + LineEnding +
  '  MaskSum: Double;                                                       ' + LineEnding +
  '  MaskSumSquared: Double;                                                ' + LineEnding +
  '                                                                         ' + LineEnding +
  '  ImgNormCorr: TSingleMatrix;                                            ' + LineEnding +
  '  ImgMaskCorr: TSingleMatrix;                                            ' + LineEnding +
  'end',
  'TMatchTemplateGreyCache'
);

addGlobalType(
  'packed record                                                            ' + LineEnding +
  '  Width: Int32;                                                          ' + LineEnding +
  '  Height: Int32;                                                         ' + LineEnding +
  '                                                                         ' + LineEnding +
  '  ImageSpec: record                                                      ' + LineEnding +
  '    R, G, B: array of array of packed record Re, Im: Single; end;        ' + LineEnding +
  '  end;                                                                   ' + LineEnding +
  '  ImageSpecSquared: record                                               ' + LineEnding +
  '    R, G, B: array of array of packed record Re, Im: Single; end;        ' + LineEnding +
  '  end;                                                                   ' + LineEnding +
  '                                                                         ' + LineEnding +
  '  MaskChannel: record R, G, B: TSingleMatrix; end;                       ' + LineEnding +
  '  MaskSum: array of Double;                                              ' + LineEnding +
  '  MaskSumSquared: array of Double;                                       ' + LineEnding +
  '                                                                         ' + LineEnding +
  '  ImgNormCorr: TSingleMatrix;                                            ' + LineEnding +
  '  ImgMaskCorr: record R, G, B: TSingleMatrix; end;                       ' + LineEnding +
  'end',
  'TMatchTemplateRGBCache'
);

addGlobalFunc('function TMatchTemplateGreyCache.Create(constref Image, Mask: TByteMatrix): TMatchTemplateGreyCache; static', @_LapeMatchTemplateGreyCache_Create);
addGlobalFunc('function TMatchTemplateRGBCache.Create(constref Image, Mask: TIntegerMatrix): TMatchTemplateRGBCache; static', @_LapeMatchTemplateRGBCache_Create);

addGlobalFunc('function MatchTemplateMask(constref Cache: TMatchTemplateGreyCache; constref Template: TByteMatrix; Formula: ETMFormula): TSingleMatrix; overload', @_LapeMatchTemplateMaskGrey);
addGlobalFunc('function MatchTemplateMask(constref Image, Template, Mask: TByteMatrix; Formula: ETMFormula): TSingleMatrix; overload', @_LapeMatchTemplateMaskGreyEx);

addGlobalFunc('function MatchTemplateMask(constref Cache: TMatchTemplateRGBCache; constref Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix; overload', @_LapeMatchTemplateMaskRGB);
addGlobalFunc('function MatchTemplateMask(constref Image, Template, Mask: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix; overload', @_LapeMatchTemplateMaskRGBEx);

addGlobalFunc('function MatchTemplate(constref Image, Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix', @_LapeMatchTemplate);

addGlobalFunc('function TMufasaBitmap.MatchTemplate(Template: TMufasaBitmap; Formula: ETMFormula): TSingleMatrix; constref', @_LapeMufasaBitmap_MatchTemplate);
addGlobalFunc('function TMufasaBitmap.MatchTemplateMask(Template: TMufasaBitmap; Formula: ETMFormula): TSingleMatrix; constref', @_LapeMufasaBitmap_MatchTemplateMask);

