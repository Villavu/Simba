{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.matchtemplate_multithread;

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.mufasatypes, simba.matchtemplate;

type
  TSimbaMultithreadedMatchTemplate = class(TSimbaMatchTemplate)
  public
    class function CacheClass: TMatchTemplateCacheClass; override;

    class function MatchTemplateMask(const Cache: TMatchTemplateCache; const Templ: TIntegerMatrix; const Formula: ETMFormula): TSingleMatrix; override;
    class function MatchTemplate(const Image, Templ: TIntegerMatrix; const Formula: ETMFormula): TSingleMatrix; override;
  end;

implementation

uses
  math,
  simba.threadpool, simba.helpers_matrix;

type
  TMatchTemplateCacheSlices = class(TMatchTemplateCache)
  protected
    FWidth: Integer;
    FHeight: Integer;
    FCount: Integer;
    FSlices: array of TMatchTemplateCache;
  public
    constructor Create(const Image, Templ: TIntegerMatrix); override;
  end;

  TImageSlices = array of TIntegerMatrix;
  TOutputSlices = array of TSingleMatrix;

function CalculateSlices(Image, Template: TIntegerMatrix): Integer;
var
  I: Integer;
begin
  Result := 0;

  if (Image.Width - Template.Width > 200) and (Image.Height - Template.Height > 250) then // not worth
  begin
    for I := Min(TThread.ProcessorCount, 4) downto 2 do // more than 4 threads loses effectiveness very quickly
      if ((Image.Height div I) + Template.Height) > 200 then
        Exit(I);
  end;

  // not possible to slice into at least 200 pixels
end;

function SliceImage(const Image, Template: TIntegerMatrix; out Slices: TImageSlices): Boolean;
var
  I, Y, Offset, Count: Integer;
begin
  Count := CalculateSlices(Image, Template);
  if (Count = 0) then
  begin
    Result := False;
    Exit;
  end;

  SetLength(Slices, Count);

  for I := 0 to High(Slices) do
  begin
    if (I = High(Slices)) then
      Slices[I].SetSize(Image.Width, Image.Height div Count)
    else
      Slices[I].SetSize(Image.Width, (Image.Height div Count) + Template.Height);

    Offset := (Image.Height div Count) * I;
    for Y := 0 to Slices[I].Height - 1 do
      Move(Image[Y + Offset][0], Slices[I][Y][0], Image.Width * SizeOf(Integer));
  end;

  Result := True;
end;

procedure Parallel_MatchTemplateMask(const Params: PParamArray; const Result: Pointer);
begin
  PSingleMatrix(Result)^ := TSimbaMatchTemplate.MatchTemplateMask(PMatchTemplateCache(Params^[0])^, PIntegerMatrix(Params^[1])^, PTMFormula(Params^[2])^);
end;

procedure Parallel_MatchTemplate(const Params: PParamArray; const Result: Pointer);
begin
  PSingleMatrix(Result)^ := TSimbaMatchTemplate.MatchTemplate(PIntegerMatrix(Params^[0])^, PIntegerMatrix(Params^[1])^, PTMFormula(Params^[2])^);
end;

procedure Parallel_MatchTemplateCache(const Params: PParamArray; const Result: Pointer);
begin
  PMatchTemplateCache(Result)^ := TSimbaMatchTemplate.CacheClass.Create(PIntegerMatrix(Params^[0])^, PIntegerMatrix(Params^[1])^);
end;

constructor TMatchTemplateCacheSlices.Create(const Image, Templ: TIntegerMatrix);
var
  I: Integer;
  ImageSlices: TImageSlices;
  Tasks: TSimbaThreadPoolTasks;
begin
  FCount := 1;

  FWidth := Image.Width;
  FHeight := Image.Height;

  if SliceImage(Image, Templ, ImageSlices) then
  begin
    FCount := Length(ImageSlices);

    SetLength(FSlices, FCount);
    SetLength(Tasks, FCount);
    for I := 0 to High(Tasks) do
      Tasks[I] := TSimbaThreadPoolTask.Create(@Parallel_MatchTemplateCache, [@ImageSlices[I], @Templ], @FSlices[I]);

    SimbaThreadPool.RunParallel(Tasks);
  end else
    FSlices := [TSimbaMatchTemplate.CacheClass.Create(Image, Templ)];
end;

class function TSimbaMultithreadedMatchTemplate.MatchTemplateMask(const Cache: TMatchTemplateCache; const Templ: TIntegerMatrix; const Formula: ETMFormula): TSingleMatrix;
var
  OutputSlices: TOutputSlices;
  Tasks: TSimbaThreadPoolTasks;
  I, Y: Integer;
  Offset: Integer;
begin
  with Cache as TMatchTemplateCacheSlices do
  begin
    if (FCount > 1) then
    begin
      SetLength(OutputSlices, FCount);
      SetLength(Tasks, FCount);
      for I := 0 to High(Tasks) do
        Tasks[I] := TSimbaThreadPoolTask.Create(@Parallel_MatchTemplateMask, [@FSlices[I], @Templ, @Formula], @OutputSlices[I]);

      SimbaThreadPool.RunParallel(Tasks);

      Result.SetSize(
        (FWidth - Templ.Width) + 1,
        (FHeight - Templ.Height) + 1
      );

      for I := 0 to High(OutputSlices) do
      begin
        Offset := (FHeight div Length(OutputSlices)) * I;
        for Y := 0 to OutputSlices[I].Height - 1 do
          Move(OutputSlices[I][Y][0], Result[Offset + Y][0], Result.Width * SizeOf(Single));
      end;
    end else
      Result := TSimbaMatchTemplate.MatchTemplateMask(FSlices[0], Templ, Formula);
  end;
end;

class function TSimbaMultithreadedMatchTemplate.MatchTemplate(const Image, Templ: TIntegerMatrix; const Formula: ETMFormula): TSingleMatrix;
var
  ImageSlices: TImageSlices;
  OutputSlices: TOutputSlices;
  Tasks: TSimbaThreadPoolTasks;
  I, Offset, Y: Integer;
begin
  if SliceImage(Image, Templ, ImageSlices) then
  begin
    SetLength(OutputSlices, Length(ImageSlices));
    SetLength(Tasks, Length(ImageSlices));
    for I := 0 to High(Tasks) do
      Tasks[I] := TSimbaThreadPoolTask.Create(@Parallel_MatchTemplate, [@ImageSlices[I], @Templ, @Formula], @OutputSlices[I]);

    SimbaThreadPool.RunParallel(Tasks);

    Result.SetSize(
      (Image.Width - Templ.Width) + 1,
      (Image.Height - Templ.Height) + 1
    );

    for I := 0 to High(OutputSlices) do
    begin
      Offset := (Image.Height div Length(OutputSlices)) * I;
      for Y := 0 to OutputSlices[I].Height - 1 do
        Move(OutputSlices[I][Y][0], Result[Offset + Y][0], Result.Width * SizeOf(Single));
    end;
  end else
    Result := TSimbaMatchTemplate.MatchTemplate(Image, Templ, Formula);
end;

class function TSimbaMultithreadedMatchTemplate.CacheClass: TMatchTemplateCacheClass;
begin
  Result := TMatchTemplateCacheSlices;
end;

end.

