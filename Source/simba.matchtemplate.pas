unit simba.matchtemplate;
{==============================================================================]
  Copyright © 2021, Jarl Krister Holta

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
[==============================================================================}
{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base,
  simba.matchtemplate_core;

type
  PTMFormula = ^ETMFormula;
  ETMFormula = (
    TM_CCORR,
    TM_CCORR_NORMED,
    TM_CCOEFF,
    TM_CCOEFF_NORMED,
    TM_SQDIFF,
    TM_SQDIFF_NORMED
  );

function MatchTemplateMask(Image, Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix;
function MatchTemplate(Image, Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix;

// Cache variants - if `Cache` var is empty build it else use it.
// Image cannot change for such cache, so pass a fresh var if different image.
function MatchTemplate(Image, Template: TIntegerMatrix; Formula: ETMFormula; var Cache: TMatchTemplateCache): TSingleMatrix;
function MatchTemplateMask(Image, Template: TIntegerMatrix; Formula: ETMFormula; var Cache: TMatchTemplateCache): TSingleMatrix;

implementation

uses
  simba.vartype_matrix,
  simba.matchtemplate_ccorr,
  simba.matchtemplate_sqdiff,
  simba.matchtemplate_ccoeff;

procedure Validate(ImageWidth, ImageHeight, TemplateWidth, TemplateHeight: Integer);
begin
  if (ImageWidth = 0) or (ImageHeight = 0) then
    raise Exception.Create('MatchTemplate: Image is empty');
  if (TemplateWidth = 0) or (TemplateHeight = 0) then
    raise Exception.Create('MatchTemplate: Template is empty');
  if (TemplateWidth > ImageWidth) or (TemplateHeight > ImageHeight) then
    raise Exception.Create('MatchTemplate: Template must be smaller than image');
end;

function MatchTemplate(Image, Template: TIntegerMatrix; Formula: ETMFormula; var Cache: TMatchTemplateCache): TSingleMatrix;
begin
  Validate(Image.Width, Image.Height, Template.Width, Template.Height);
  if (Cache.Width = 0) then
    Cache.Init(Image);

  case Formula of
    TM_CCOEFF:        Result := MatchTemplate_CCOEFF(Cache, Template, False);
    TM_CCOEFF_NORMED: Result := MatchTemplate_CCOEFF(Cache, Template, True);
    TM_SQDIFF:        Result := MatchTemplate_SQDIFF(Cache, Template, False);
    TM_SQDIFF_NORMED: Result := MatchTemplate_SQDIFF(Cache, Template, True);
    TM_CCORR:         Result := MatchTemplate_CCORR(Cache, Template, False);
    TM_CCORR_NORMED:  Result := MatchTemplate_CCORR(Cache, Template, True);
  end;
end;

function MatchTemplateMask(Image, Template: TIntegerMatrix; Formula: ETMFormula; var Cache: TMatchTemplateCache): TSingleMatrix;
begin
  Validate(Image.Width, Image.Height, Template.Width, Template.Height);
  if Cache.Width = 0 then
    Cache.Init(Image);

  case Formula of
    TM_CCOEFF:        Result := MatchTemplateMask_CCOEFF(Cache, Template, False);
    TM_CCOEFF_NORMED: Result := MatchTemplateMask_CCOEFF(Cache, Template, True);
    TM_SQDIFF:        Result := MatchTemplateMask_SQDIFF(Cache, Template, False);
    TM_SQDIFF_NORMED: Result := MatchTemplateMask_SQDIFF(Cache, Template, True);
    TM_CCORR:         Result := MatchTemplateMask_CCORR(Cache, Template, False);
    TM_CCORR_NORMED:  Result := MatchTemplateMask_CCORR(Cache, Template, True);
  end;
end;

function MatchTemplate(Image, Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix;
var
  Cache: TMatchTemplateCache;
begin
  Result := MatchTemplate(Image, Template, Formula, Cache);
end;

function MatchTemplateMask(Image, Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix;
var
  Cache: TMatchTemplateCache;
begin
  Result := MatchTemplateMask(Image, Template, Formula, Cache);
end;

end.
