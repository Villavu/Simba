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
  simba.base, simba.script;

procedure ImportMatchTemplate(Script: TSimbaScript);

implementation

uses
  lptypes, lpvartypes,
  simba.matchtemplate, simba.matchtemplate_core;

type
  PMatchTemplateCache = ^TMatchTemplateCache;

(*
Match Template
==============
Template matching.

Note:: These functions outputs are equal to OpenCV's matchTemplate.
*)

(*
MatchTemplateMask
-----------------
```
function MatchTemplateMask(Image, Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix;
```
*)
procedure _LapeMatchTemplateMask1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Result)^ := MatchTemplateMask(PIntegerMatrix(Params^[0])^, PIntegerMatrix(Params^[1])^, PTMFormula(Params^[2])^);
end;

(*
MatchTemplate
-------------
```
function MatchTemplate(Image, Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix;
```
*)
procedure _LapeMatchTemplate1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Result)^ := MatchTemplate(PIntegerMatrix(Params^[0])^, PIntegerMatrix(Params^[1])^, PTMFormula(Params^[2])^);
end;

(*
MatchTemplateMask
-----------------
```
function MatchTemplateMask(Image, Template: TIntegerMatrix; Formula: ETMFormula; var Cache: TMatchTemplateCache): TSingleMatrix;
```
MatchTemplateMask but uses cache to speed up processing assuming `Image` is completely static.
Will write to the `Cache` variable if empty, or use it.
*)
procedure _LapeMatchTemplateMask3(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Result)^ := MatchTemplateMask(PIntegerMatrix(Params^[0])^, PIntegerMatrix(Params^[1])^, PTMFormula(Params^[2])^, PMatchTemplateCache(Params^[3])^);
end;

(*
MatchTemplate (cached)
----------------------
```
function MatchTemplate(Image, Template: TIntegerMatrix; Formula: ETMFormula; var Cache: TMatchTemplateCache): TSingleMatrix;
```
MatchTemplate but uses cache to speed up processing assuming `Image` is completely static.
Will write to the `Cache` variable if empty, or use it.
*)
procedure _LapeMatchTemplate3(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Result)^ := MatchTemplate(PIntegerMatrix(Params^[0])^, PIntegerMatrix(Params^[1])^, PTMFormula(Params^[2])^, PMatchTemplateCache(Params^[3])^);
end;

procedure ImportMatchTemplate(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    DumpSection := 'Match Template';

    addGlobalType('(TM_CCORR, TM_CCORR_NORMED, TM_CCOEFF, TM_CCOEFF_NORMED, TM_SQDIFF, TM_SQDIFF_NORMED)', 'ETMFormula');

    addGlobalType([
      'record {%CODETOOLS OFF}',
      '  FImgR, FImgG, FImgB: TSingleMatrix;',
      {$IFDEF MT_PACKED}
      '  FSpectra, FSqSpectra: record',
      '    RG,B: record Data: array of record Re, Im: Single; end; FWidth, FHeight: Integer; end;',
      '  end;',
      {$ELSE}
      '  FSpectra, FSqSpectra: record',
      '    R,G,B: record Data: array of record Re, Im: Single; end; FWidth, FHeight: Integer; end;',
      '  end;',
      {$ENDIF}
      '  FSum, FSumSq: array[0..2] of TDoubleMatrix;',
      '  FMaskKey: TSingleMatrix;',
      '  FfMask, FfMask2: record Data: array of record Re, Im: Single; end; FWidth, FHeight: Integer; end;',
      '  FCorrIM, FCorrIMSq, FCorrISqMSq: array[0..2] of TSingleMatrix;',
      '  FWidth, FHeight: Integer;',
      '{%CODETOOLS ON} end;'],
      'TMatchTemplateCache'
    );

    if (getGlobalType('TMatchTemplateCache').Size <> SizeOf(TMatchTemplateCache)) then
      SimbaException('TMatchTemplateCache layout mismatch');

    addGlobalFunc('function MatchTemplateMask(Image, Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix; overload', @_LapeMatchTemplateMask1);
    addGlobalFunc('function MatchTemplate(Image, Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix; overload', @_LapeMatchTemplate1);
    addGlobalFunc('function MatchTemplateMask(Image, Template: TIntegerMatrix; Formula: ETMFormula; var Cache: TMatchTemplateCache): TSingleMatrix; overload', @_LapeMatchTemplateMask3);
    addGlobalFunc('function MatchTemplate(Image, Template: TIntegerMatrix; Formula: ETMFormula; var Cache: TMatchTemplateCache): TSingleMatrix; overload', @_LapeMatchTemplate3);

    DumpSection := '';
  end;
end;

end.
