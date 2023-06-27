{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.matchtemplate_helpers;

{$i simba.inc}

{$IFOPT D-}
  {$OPTIMIZATION LEVEL4}
{$ENDIF}

{$MODESWITCH ARRAYOPERATORS OFF}

interface

uses
  classes, sysutils,
  simba.baseclass, simba.mufasatypes, simba.matchtemplate_matrix;

type
  TImageSlices = array of TIntegerMatrix;
  TMatchTemplate = function(Image, Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;

function SliceImage(Image, Template: TIntegerMatrix): TImageSlices;

function MaskFromTemplate(Templ: TIntegerMatrix): TRGBMatrix;
function Multithread(Image, Templ: TIntegerMatrix; MatchTemplate: TMatchTemplate; Normed: Boolean): TSingleMatrix;

function FFT2_RGB(Mat: TRGBMatrix): TRGBComplexMatrix;

function CrossCorr(Image, Templ: TSingleMatrix): TSingleMatrix;
function CrossCorrRGB(Image, Templ: TRGBMatrix): TRGBMatrix; overload;
function CrossCorrRGB(Image: TRGBComplexMatrix; Templ: TRGBMatrix): TRGBMatrix; overload;

implementation

uses
  simba.FFTPACK4, simba.threadpool;

function DoFFT2(const Matrix: TSingleMatrix; const outW, outH: Integer): TComplexMatrix;
var
  Spec: TComplexMatrix;
  X, Y, W, H: Integer;
begin
  Spec.SetSize(FFTPACK.OptimalDFTSize(OutW), FFTPACK.OptimalDFTSize(OutH));

  W := Matrix.Width - 1;
  H := Matrix.Height - 1;
  for Y := 0 to H do
    for X := 0 to W do
      Spec[Y, X].Re := Matrix[Y, X];

  Result := FFTPACK.FFT2(Spec);
end;

function DoIFFT2(Spec: TComplexMatrix; const outW, outH: Integer): TSingleMatrix;
var
  X, Y, W, H: Integer;
begin
  Spec := FFTPACK.IFFT2(Spec);

  Result.SetSize(OutW, OutH);

  W := OutW - 1;
  H := OutH - 1;
  for Y := 0 to H do
    for X := 0 to W do
      Result[Y, X] := Spec[Y, X].Re;
end;

function FFT2_RGB(Mat: TRGBMatrix): TRGBComplexMatrix;
begin
  Result.Width  := Mat.Width;
  Result.Height := Mat.Height;

  Result.R := DoFFT2(Mat.R, Result.Width, Result.Height);
  Result.G := DoFFT2(Mat.G, Result.Width, Result.Height);
  Result.B := DoFFT2(Mat.B, Result.Width, Result.Height);
end;

// -----------------------------------------------------------------------------
// a * conj(b)

function MulSpectrumConj(const A, B: TComplexMatrix): TComplexMatrix;
var
  X, Y, W, H: Integer;
begin
  Result.SetSize(A.Width, A.Height);

  W := A.Width - 1;
  H := A.Height - 1;

  for Y := 0 to H do
    for X := 0 to W do
    begin
      Result[y,x].re := (a[y,x].re *  b[y,x].re) - (a[y,x].im * -b[y,x].im);
      Result[y,x].im := (a[y,x].re * -b[y,x].im) + (a[y,x].im *  b[y,x].re);
    end;
end;

function MaskFromTemplate(Templ: TIntegerMatrix): TRGBMatrix;
var
  X, Y, W, H: Integer;
begin
  Result := Default(TRGBMatrix);
  Result.R.SetSize(Templ.Width, Templ.Height);
  Result.G := Result.R; // reference
  Result.B := Result.R; // ..

  W := Templ.Width - 1;
  H := Templ.Height - 1;
  for Y := 0 to H do
    for X := 0 to W do
      if (Templ[Y, X] and $FFFFFF) <> 0 then // Mask out alpha
        Result.R[Y, X] := 1;
end;

function CrossCorr(Image, Templ: TSingleMatrix): TSingleMatrix;
var
  Spec: TComplexMatrix;
begin
  Spec := MulSpectrumConj(
    DoFFT2(Image, Image.Width, Image.Height),
    DoFFT2(Templ, Image.Width, Image.Height)
  );

  Result := DoIFFT2(Spec, Image.Width - Templ.Width + 1, Image.Height - Templ.Height + 1);
end;

function CrossCorrRGB(Image, Templ: TRGBMatrix): TRGBMatrix;
begin
  Result.R := CrossCorr(Image.R, Templ.R);
  Result.G := CrossCorr(Image.G, Templ.G);
  Result.B := CrossCorr(Image.B, Templ.B);
end;

function CalculateSlices(ImageW, ImageH, TemplW, TemplH: Integer): Integer;
var
  I: Integer;
begin
  Result := 1;

  if (ImageW - TemplW > 200) and (ImageH - TemplH > 250) then // not worth
  begin
    for I := SimbaThreadPool.ThreadCount - 1 downto 2 do // more than 4 threads loses effectiveness very quickly
      if ((ImageH div I) + TemplH) > 200 then
        Exit(I);
  end;

  // not possible to slice into at least 200 pixels
end;

function SliceImage(Image, Template: TIntegerMatrix): TImageSlices;
var
  I, Y, Offset, RowSize, Count: Integer;
  Slice: TIntegerMatrix;
begin
  SetLength(Result, CalculateSlices(Image.Width, Image.Height, Template.Width, Template.Height));
  if (Length(Result) = 1) then
    Result[0] := Image
  else
  begin
    RowSize := Image.Width * SizeOf(Integer);
    Count := Length(Result);

    for I := 0 to High(Result) do
    begin
      Slice := nil;
      if (I = Count - 1) then
        Slice.SetSize(Image.Width, Image.Height div Count)
      else
        Slice.SetSize(Image.Width, (Image.Height div Count) + Template.Height);

      Offset := (Image.Height div Count) * I;
      for Y := 0 to Slice.Height - 1 do
        Move(Image[Y + Offset, 0], Slice[Y, 0], RowSize);

      Result[I] := Slice;
    end;
  end;
end;

function Multithread(Image, Templ: TIntegerMatrix; MatchTemplate: TMatchTemplate; Normed: Boolean): TSingleMatrix;
var
  ImageSlices: TImageSlices;
  RowSize: Integer;

  procedure DoMatchTemplate(const SliceIndex: Integer);
  var
    SliceOffset, Y: Integer;
    Mat: TSingleMatrix;
  begin
    SliceOffset := (Image.Height div Length(ImageSlices)) * SliceIndex;

    Mat := MatchTemplate(ImageSlices[SliceIndex], Templ, Normed);
    for Y := 0 to Mat.Height - 1 do
      Move(Mat[Y, 0], Result[SliceOffset + Y, 0], RowSize);
  end;

var
  //Tasks: TSimbaThreadPoolTaskArray;
  I: Integer;
begin
  if CalculateSlices(Image.Width, Image.Height, Templ.Width, Templ.Height) > 1 then
  begin
    Result.SetSize(
      (Image.Width - Templ.Width) + 1,
      (Image.Height - Templ.Height) + 1
    );
    RowSize := Result.Width * SizeOf(Single);
    ImageSlices := SliceImage(Image, Templ);

    {
    SetLength(Tasks, Length(ImageSlices));
    for I := 0 to High(Tasks) do
      Tasks[I] := TSimbaThreadPoolTask_NestedMethod.Create(@DoMatchTemplate);

    SimbaThreadPool.RunParallel(Tasks);
    }
  end else
    Result := MatchTemplate(Image, Templ, Normed);
end;

function CrossCorrRGB(Image: TRGBComplexMatrix; Templ: TRGBMatrix): TRGBMatrix;

  function CrossCorr(Channel: TComplexMatrix; Templ: TSingleMatrix): TSingleMatrix;
  begin
    Result := DoIFFT2(
      MulSpectrumConj(
        Channel,
        DoFFT2(Templ, Image.Width, Image.Height)
      ),
      Image.Width - Templ.Width + 1, Image.Height - Templ.Height + 1
    );
  end;

begin
  Result.R := CrossCorr(Image.R, Templ.R);
  Result.G := CrossCorr(Image.G, Templ.G);
  Result.B := CrossCorr(Image.B, Templ.B);
end;

end.

