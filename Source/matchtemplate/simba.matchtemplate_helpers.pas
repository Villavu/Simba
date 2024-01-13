{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.matchtemplate_helpers;

{$DEFINE SIMBA_MAX_OPTIMIZATION}
{$i simba.inc}

{$MODESWITCH ARRAYOPERATORS OFF}

interface

uses
  classes, sysutils,
  simba.baseclass, simba.mufasatypes, simba.matchtemplate_matrix;

function MaskFromTemplate(Templ: TIntegerMatrix): TRGBMatrix;

function FFT2_RGB(Mat: TRGBMatrix): TRGBComplexMatrix;

function CrossCorr(Image, Templ: TSingleMatrix): TSingleMatrix;
function CrossCorrRGB(Image, Templ: TRGBMatrix): TRGBMatrix; overload;
function CrossCorrRGB(Image: TRGBComplexMatrix; Templ: TRGBMatrix): TRGBMatrix; overload;

implementation

uses
  simba.FFTPACK4, simba.threadpool,
  simba.matrix_float, simba.matrix_int;

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

