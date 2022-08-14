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

  Mask template matching implemented from OpenCV's templmatch.cpp

[==============================================================================}
{$DEFINE SIMBA_O4}
{$i simba.inc}

{$MODESWITCH ARRAYOPERATORS OFF}

interface

uses
  classes, sysutils,
  simba.mufasatypes, simba.baseclass, simba.bitmap;

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

  PMatchTemplateCacheBase = ^TMatchTemplateCacheBase;
  TMatchTemplateCacheBase = class(TSimbaBaseClass)
  public
    Width: Integer;
    Height: Integer;
  end;

function MatchTemplateCache(Image, Template: TIntegerMatrix; Formula: ETMFormula): TMatchTemplateCacheBase; overload;
function MatchTemplateCache(Image, Template: TMufasaBitmap; Formula: ETMFormula): TMatchTemplateCacheBase; overload;
function MatchTemplateMask(Cache: TMatchTemplateCacheBase; Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix; overload;
function MatchTemplateMask(Cache: TMatchTemplateCacheBase; Template: TMufasaBitmap; Formula: ETMFormula): TSingleMatrix; overload;
function MatchTemplateMask(Image, Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix; overload;

function MatchTemplate(Image, Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix;

implementation

uses
  simba.matchtemplate_ccorr, simba.matchtemplate_sqdiff, simba.matchtemplate_ccoeff;

procedure Validate(ImageWidth, ImageHeight, TemplateWidth, TemplateHeight: Integer);
begin
  if (ImageWidth = 0) or (ImageHeight = 0) then
    raise Exception.Create('MatchTemplate: Image is empty');
  if (TemplateWidth = 0) or (TemplateHeight = 0) then
    raise Exception.Create('MatchTemplate: Template is empty');
  if (TemplateWidth > ImageWidth) or (TemplateHeight > ImageHeight) then
    raise Exception.Create('MatchTemplate: Template must be smaller than image');
end;

function MatchTemplateCache(Image, Template: TIntegerMatrix; Formula: ETMFormula): TMatchTemplateCacheBase;
begin
  Validate(Image.Width, Image.Height, Template.Width, Template.Height);

  case Formula of
    TM_CCOEFF, TM_CCOEFF_NORMED:
      Result := MatchTemplateMask_CCOEFF_CreateCache_MT(Image, Template);
    TM_CCORR, TM_CCORR_NORMED:
      Result := MatchTemplateMask_CCORR_CreateCache_MT(Image, Template);
    TM_SQDIFF, TM_SQDIFF_NORMED:
      Result := MatchTemplateMask_SQDIFF_CreateCache_MT(Image, Template);
  end;
end;

function MatchTemplateCache(Image, Template: TMufasaBitmap; Formula: ETMFormula): TMatchTemplateCacheBase;
begin
  Result := MatchTemplateCache(Image.ToMatrixBGR(), Template.ToMatrixBGR(), Formula);
end;

function MatchTemplateMask(Cache: TMatchTemplateCacheBase; Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix;
begin
  Validate(Cache.Width, Cache.Height, Template.Width, Template.Height);

  case Formula of
    TM_CCOEFF:        Result := MatchTemplateMask_CCOEFF_Cache(Cache, Template, False);
    TM_CCOEFF_NORMED: Result := MatchTemplateMask_CCOEFF_Cache(Cache, Template, True);
    TM_SQDIFF:        Result := MatchTemplateMask_SQDIFF_Cache(Cache, Template, False);
    TM_SQDIFF_NORMED: Result := MatchTemplateMask_SQDIFF_Cache(Cache, Template, True);
    TM_CCORR:         Result := MatchTemplateMask_CCORR_Cache(Cache, Template, False);
    TM_CCORR_NORMED:  Result := MatchTemplateMask_CCORR_Cache(Cache, Template, True);
  end;
end;

function MatchTemplateMask(Cache: TMatchTemplateCacheBase; Template: TMufasaBitmap; Formula: ETMFormula): TSingleMatrix;
begin
  Result := MatchTemplateMask(Cache, Template.ToMatrixBGR(), Formula);
end;

function MatchTemplateMask(Image, Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix;
begin
  Validate(Image.Width, Image.Height, Template.Width, Template.Height);

  case Formula of
    TM_CCOEFF:        Result := MatchTemplateMask_CCOEFF_MT(Image, Template, False);
    TM_CCOEFF_NORMED: Result := MatchTemplateMask_CCOEFF_MT(Image, Template, True);
    TM_SQDIFF:        Result := MatchTemplateMask_SQDIFF_MT(Image, Template, False);
    TM_SQDIFF_NORMED: Result := MatchTemplateMask_SQDIFF_MT(Image, Template, True);
    TM_CCORR:         Result := MatchTemplateMask_CCORR_MT(Image, Template, False);
    TM_CCORR_NORMED:  Result := MatchTemplateMask_CCORR_MT(Image, Template, True);
  end;
end;

function MatchTemplate(Image, Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix;
begin
  Validate(Image.Width, Image.Height, Template.Width, Template.Height);

  case Formula of
    TM_CCOEFF:        Result := MatchTemplate_CCOEFF_MT(Image, Template, False);
    TM_CCOEFF_NORMED: Result := MatchTemplate_CCOEFF_MT(Image, Template, True);
    TM_SQDIFF:        Result := MatchTemplate_SQDIFF_MT(Image, Template, False);
    TM_SQDIFF_NORMED: Result := MatchTemplate_SQDIFF_MT(Image, Template, True);
    TM_CCORR:         Result := MatchTemplate_CCORR_MT(Image, Template, False);
    TM_CCORR_NORMED:  Result := MatchTemplate_CCORR_MT(Image, Template, True);
  end;
end;

end.
