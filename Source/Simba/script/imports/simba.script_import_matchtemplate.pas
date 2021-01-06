unit simba.script_import_matchtemplate;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_MatchTemplate(Compiler: TSimbaScript_Compiler);

implementation

uses
  simba.matchtemplate, simba.bitmap;

procedure Lape_MatchTemplateGreyCache_Create(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMatchTemplateGreyCache(Result)^ := TMatchTemplateGreyCache.Create(P2DByteArray(Params^[0])^, P2DByteArray(Params^[1])^);
end;

procedure Lape_MatchTemplateRGBCache_Create(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMatchTemplateRGBCache(Result)^ := TMatchTemplateRGBCache.Create(P2DIntArray(Params^[0])^, P2DIntArray(Params^[1])^);
end;

procedure Lape_MatchTemplateMaskGrey(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Result)^ := MatchTemplateMask(PMatchTemplateGreyCache(Params^[0])^, P2DByteArray(Params^[1])^, PTMFormula(Params^[2])^);
end;

procedure Lape_MatchTemplateMaskGreyEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Result)^ := MatchTemplateMask(P2DByteArray(Params^[0])^, P2DByteArray(Params^[1])^, P2DByteArray(Params^[2])^, PTMFormula(Params^[3])^);
end;

procedure Lape_MatchTemplateMaskRGB(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Result)^ := MatchTemplateMask(PMatchTemplateRGBCache(Params^[0])^, P2DIntArray(Params^[1])^, PTMFormula(Params^[2])^);
end;

procedure Lape_MatchTemplateMaskRGBEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Result)^ := MatchTemplateMask(P2DIntArray(Params^[0])^, P2DIntArray(Params^[1])^, P2DIntArray(Params^[2])^, PTMFormula(Params^[3])^);
end;

procedure Lape_MatchTemplate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Result)^ := MatchTemplate(P2DIntArray(Params^[0])^, P2DIntArray(Params^[1])^, PTMFormula(Params^[2])^);
end;

procedure Lape_TMufasaBitmap_MatchTemplate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Result)^ := PMufasaBitmap(Params^[0])^.MatchTemplate(PMufasaBitmap(Params^[1])^, PTMFormula(Params^[2])^);
end;

procedure Lape_TMufasaBitmap_MatchTemplateMask(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingleMatrix(Result)^ := PMufasaBitmap(Params^[0])^.MatchTemplateMask(PMufasaBitmap(Params^[1])^, PTMFormula(Params^[2])^);
end;

procedure Lape_Import_MatchTemplate(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    Section := 'Match Template';

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
      'end;',
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
      'end;',
      'TMatchTemplateRGBCache'
    );

    addGlobalFunc('function TMatchTemplateGreyCache.Create(constref Image, Mask: TByteMatrix): TMatchTemplateGreyCache; static;', @Lape_MatchTemplateGreyCache_Create);
    addGlobalFunc('function TMatchTemplateRGBCache.Create(constref Image, Mask: TIntegerMatrix): TMatchTemplateRGBCache; static;', @Lape_MatchTemplateRGBCache_Create);

    addGlobalFunc('function MatchTemplateMask(constref Cache: TMatchTemplateGreyCache; constref Template: TByteMatrix; Formula: ETMFormula): TSingleMatrix; overload;', @Lape_MatchTemplateMaskGrey);
    addGlobalFunc('function MatchTemplateMask(constref Image, Template, Mask: TByteMatrix; Formula: ETMFormula): TSingleMatrix; overload;', @Lape_MatchTemplateMaskGreyEx);

    addGlobalFunc('function MatchTemplateMask(constref Cache: TMatchTemplateRGBCache; constref Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix; overload;', @Lape_MatchTemplateMaskRGB);
    addGlobalFunc('function MatchTemplateMask(constref Image, Template, Mask: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix; overload;', @Lape_MatchTemplateMaskRGBEx);

    addGlobalFunc('function MatchTemplate(constref Image, Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix;', @Lape_MatchTemplate);

    addGlobalFunc('function TMufasaBitmap.MatchTemplate(Template: TMufasaBitmap; Formula: ETMFormula): TSingleMatrix; constref;', @Lape_TMufasaBitmap_MatchTemplate);
    addGlobalFunc('function TMufasaBitmap.MatchTemplateMask(Template: TMufasaBitmap; Formula: ETMFormula): TSingleMatrix; constref;', @Lape_TMufasaBitmap_MatchTemplateMask);
  end;
end;

end.

