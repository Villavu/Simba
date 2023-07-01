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
{$DEFINE SIMBA_MAX_OPTIMIZATION}
{$i simba.inc}

{$MODESWITCH ARRAYOPERATORS OFF}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.baseclass;

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
function MatchTemplateMask(Cache: TMatchTemplateCacheBase; Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix; overload;
function MatchTemplateMask(Image, Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix; overload;
function MatchTemplate(Image, Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix;

function CalculateSlices(SearchWidth, SearchHeight: Integer): Integer;

type
  TMatchTemplate = function(Image, Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;

function Multithread(Image, Templ: TIntegerMatrix; MatchTemplate: TMatchTemplate; Normed: Boolean): TSingleMatrix;

var
  MatchTemplateMT_Enabled:     Boolean = True;
  MatchTemplateMT_SliceHeight: Integer = 125;
  MatchTemplateMT_SliceWidth:  Integer = 250;

implementation

uses
  simba.matchtemplate_ccorr, simba.matchtemplate_sqdiff, simba.matchtemplate_ccoeff,
  simba.threadpool,
  simba.singlematrix, simba.integermatrix;

// How much to "Slice" (vertically) the image up for multithreading.
function CalculateSlices(SearchWidth, SearchHeight: Integer): Integer;
var
  I: Integer;
begin
  Result := 1;

  if MatchTemplateMT_Enabled and (SearchWidth >= MatchTemplateMT_SliceWidth) and (SearchHeight >= (MatchTemplateMT_SliceHeight * 2)) then // not worth
  begin
    for I := SimbaThreadPool.ThreadCount - 1 downto 2 do
      if (SearchHeight div I) > MatchTemplateMT_SliceHeight then // Each slice is at least `MatchTemplateMT_SliceHeight` pixels
        Exit(I);
  end;

  // not possible to slice into at least `MatchTemplateMT_SliceHeight` pixels
end;

function Multithread(Image, Templ: TIntegerMatrix; MatchTemplate: TMatchTemplate; Normed: Boolean): TSingleMatrix;
var
  RowSize: Integer;

  procedure Execute(const Index, Lo, Hi: Integer);
  var
    Mat: TSingleMatrix;
    Y: Integer;
  begin
    Mat := MatchTemplate(Image.Copy(Lo, Min(Hi + Templ.Height, Image.Height)), Templ, Normed);
    for Y := 0 to Mat.Height - 1 do
      Move(Mat[Y, 0], Result[Lo+Y, 0], RowSize);
  end;

begin
  Result.SetSize(
    (Image.Width - Templ.Width) + 1,
    (Image.Height - Templ.Height) + 1
  );
  RowSize := Result.Width * SizeOf(Single);

  SimbaThreadPool.RunParallel(CalculateSlices(Image.Width, Image.Height), 0, Image.Height, @Execute);
end;

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
      Result := MatchTemplateMask_CCOEFF_CreateCache(Image, Template);
    TM_CCORR, TM_CCORR_NORMED:
      Result := MatchTemplateMask_CCORR_CreateCache(Image, Template);
    TM_SQDIFF, TM_SQDIFF_NORMED:
      Result := MatchTemplateMask_SQDIFF_CreateCache(Image, Template);
  end;
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

function MatchTemplateMask(Image, Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix;
begin
  Validate(Image.Width, Image.Height, Template.Width, Template.Height);

  case Formula of
    TM_CCOEFF:        Result := MatchTemplateMask_CCOEFF(Image, Template, False);
    TM_CCOEFF_NORMED: Result := MatchTemplateMask_CCOEFF(Image, Template, True);
    TM_SQDIFF:        Result := MatchTemplateMask_SQDIFF(Image, Template, False);
    TM_SQDIFF_NORMED: Result := MatchTemplateMask_SQDIFF(Image, Template, True);
    TM_CCORR:         Result := MatchTemplateMask_CCORR(Image, Template, False);
    TM_CCORR_NORMED:  Result := MatchTemplateMask_CCORR(Image, Template, True);
  end;
end;

function MatchTemplate(Image, Template: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix;
begin
  Validate(Image.Width, Image.Height, Template.Width, Template.Height);

  case Formula of
    TM_CCOEFF:        Result := MatchTemplate_CCOEFF(Image, Template, False);
    TM_CCOEFF_NORMED: Result := MatchTemplate_CCOEFF(Image, Template, True);
    TM_SQDIFF:        Result := MatchTemplate_SQDIFF(Image, Template, False);
    TM_SQDIFF_NORMED: Result := MatchTemplate_SQDIFF(Image, Template, True);
    TM_CCORR:         Result := MatchTemplate_CCORR(Image, Template, False);
    TM_CCORR_NORMED:  Result := MatchTemplate_CCORR(Image, Template, True);
  end;
end;

end.
