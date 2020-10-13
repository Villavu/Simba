unit simbascript.import_matchtemplate;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_MatchTemplate(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  simba.matchtemplate, simba.bitmap;

procedure Lape_MatchTemplate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TSingleMatrix(Result^) := MatchTemplate(PMufasaBitmap(Params^[0])^, PMufasaBitmap(Params^[1])^, ETMFormula(Params^[2]^));
end;

procedure Lape_MatchTemplateEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TSingleMatrix(Result^) := MatchTemplate(PMufasaBitmap(Params^[0])^, PMufasaBitmap(Params^[1])^, ETMFormula(Params^[2]^), PMatchTemplateImageCache(Params^[3])^);
end;

procedure Lape_Import_MatchTemplate(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    Section := 'Match Template';

    addGlobalType(
      'packed record'                                                  + LineEnding +
      '  W, H: Int32;'                                                 + LineEnding +
      '  R, G, B: packed record'                                       + LineEnding +
      '    Spec: array of array of packed record Re, Im: Single; end;' + LineEnding +
      '    Sum: array of array of Double;'                             + LineEnding +
      '    SumSquared: array of array of Double;'                      + LineEnding +
      '  end;'                                                         + LineEnding +
      'end;',
      'TMatchTemplateImageCache'
    );

    addGlobalFunc('function MatchTemplate(Image, Templ: TMufasaBitmap; Formula: ETMFormula): TSingleMatrix; overload;', @Lape_MatchTemplate);
    addGlobalFunc('function MatchTemplate(Image, Templ: TMufasaBitmap; Formula: ETMFormula; var Cache: TMatchTemplateImageCache): TSingleMatrix; overload;', @Lape_MatchTemplateEx);
  end;
end;

end.

