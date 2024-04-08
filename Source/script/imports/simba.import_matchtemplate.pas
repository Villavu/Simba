{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.import_matchtemplate;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

procedure ImportMatchTemplate(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes,
  simba.image, simba.matchtemplate;

(*
Match Template
==============
Template matching.

Note:: These functions outputs are equal to OpenCV's matchTemplate.
*)

(*
TMatchTemplateCache.Create
--------------------------
> function TMatchTemplateCache.Create(Image, Template: TIntegerMatrix; Formula: ETMFormula): TMatchTemplateCache; static;
*)
procedure _LapeMatchTemplateCache_Create1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMatchTemplateCacheBase(Result)^ := MatchTemplateCache(PIntegerMatrix(Params^[0])^, PIntegerMatrix(Params^[1])^, PTMFormula(Params^[2])^);
end;

(*
TMatchTemplateCache.Create
--------------------------
> function TMatchTemplateCache.Create(Image, Template: TImage; Formula: ETMFormula): TMatchTemplateCache; static;
*)
procedure _LapeMatchTemplateCache_Create2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMatchTemplateCacheBase(Result)^ := MatchTemplateCache(PSimbaImage(Params^[0])^, PSimbaImage(Params^[1])^, PTMFormula(Params^[2])^);
end;

(*
TMatchTemplateCache.Free
------------------------
> procedure TMatchTemplateCache.Free;
*)
procedure _LapeMatchTemplateCache_Free(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMatchTemplateCacheBase(Params^[0])^.Free();
end;

(*
MatchTemplateMask
-----------------
> function MatchTemplateMask(Cache: TMatchTemplateCache; Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix;
*)
procedure _LapeMatchTemplateMaskCache1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Result)^ := MatchTemplateMask(PMatchTemplateCacheBase(Params^[0])^, PIntegerMatrix(Params^[1])^, PTMFormula(Params^[2])^);
end;

(*
MatchTemplateMask
-----------------
> function MatchTemplateMask(Image, Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix;
*)
procedure _LapeMatchTemplateMask1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Result)^ := MatchTemplateMask(PIntegerMatrix(Params^[0])^, PIntegerMatrix(Params^[1])^, PTMFormula(Params^[2])^);
end;

(*
MatchTemplate
-------------
> function MatchTemplate(Image, Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix;
*)
procedure _LapeMatchTemplate1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Result)^ := MatchTemplate(PIntegerMatrix(Params^[0])^, PIntegerMatrix(Params^[1])^, PTMFormula(Params^[2])^);
end;

(*
MatchTemplateMask
-----------------
> function MatchTemplateMask(Cache: TMatchTemplateCache; Template: TImage; Formula: ETMFormula): TSingleMatrix;
*)
procedure _LapeMatchTemplateMaskCache2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Result)^ := MatchTemplateMask(PMatchTemplateCacheBase(Params^[0])^, PSimbaImage(Params^[1])^, PTMFormula(Params^[2])^);
end;

(*
MatchTemplate
-------------
> function MatchTemplate(Image, Template: TImage; Formula: ETMFormula): TSingleMatrix;
*)
procedure _LapeMatchTemplate2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Result)^ := MatchTemplate(PSimbaImage(Params^[0])^, PSimbaImage(Params^[1])^, PTMFormula(Params^[2])^);
end;

(*
MatchTemplateMask
-----------------
> function MatchTemplateMask(Image, Template: TImage; Formula: ETMFormula): TSingleMatrix;
*)
procedure _LapeMatchTemplateMask2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Result)^ := MatchTemplateMask(PSimbaImage(Params^[0])^, PSimbaImage(Params^[1])^, PTMFormula(Params^[2])^);
end;

procedure ImportMatchTemplate(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Match Template';

    addGlobalVar('record Enabled: Boolean; SliceWidth, SliceHeight: Integer; end;', @MatchTemplateMultithreadOpts, 'MatchTemplateMultithreadOpts');

    addClass('TMatchTemplateCache', 'TBaseClass');
    addGlobalType('(TM_CCORR, TM_CCORR_NORMED, TM_CCOEFF, TM_CCOEFF_NORMED, TM_SQDIFF, TM_SQDIFF_NORMED)', 'ETMFormula');

    addGlobalFunc('function TMatchTemplateCache.Create(Image, Template: TIntegerMatrix; Formula: ETMFormula): TMatchTemplateCache; static; overload', @_LapeMatchTemplateCache_Create1);
    addGlobalFunc('function TMatchTemplateCache.Create(Image, Template: TImage; Formula: ETMFormula): TMatchTemplateCache; static; overload', @_LapeMatchTemplateCache_Create2);
    addGlobalFunc('procedure TMatchTemplateCache.Free;', @_LapeMatchTemplateCache_Free);

    // TIntegerMatrix
    addGlobalFunc('function MatchTemplateMask(Cache: TMatchTemplateCache; Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix; overload', @_LapeMatchTemplateMaskCache1);
    addGlobalFunc('function MatchTemplateMask(Image, Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix; overload', @_LapeMatchTemplateMask1);
    addGlobalFunc('function MatchTemplate(Image, Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix; overload', @_LapeMatchTemplate1);

    // TSimbaImage
    addGlobalFunc('function MatchTemplateMask(Cache: TMatchTemplateCache; Template: TImage; Formula: ETMFormula): TSingleMatrix; overload', @_LapeMatchTemplateMaskCache2);
    addGlobalFunc('function MatchTemplateMask(Image, Template: TImage; Formula: ETMFormula): TSingleMatrix; overload', @_LapeMatchTemplateMask2);
    addGlobalFunc('function MatchTemplate(Image, Template: TImage; Formula: ETMFormula): TSingleMatrix; overload', @_LapeMatchTemplate2);

    ImportingSection := '';
  end;
end;

end.
