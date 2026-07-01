{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.matchtemplate_core;

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.base,
  simba.fftpack4;

const
  // C math.h epsilons used by OpenCV
  FLT_EPSILON = 1.1920928955078125E-7;
  DBL_EPSILON = 2.2204460492503131E-16;

type
{$IFDEF MT_PACKED}
  TImgSpectra = record
    RG: TComplexMatrix;   // R packed into Re, G into Im
    B: TComplexMatrix;
  end;
{$ELSE}
  TImgSpectra = record
    R, G, B: TComplexMatrix;
  end;
{$ENDIF}
  TChannelCorrelations = array[0..2] of TSingleMatrix;   // per-channel correlation results (R, G, B)

  // Image-side cache for matching templates against the SAME image.
  // Template can change however will not be able to cache as much.
  TMatchTemplateCache = record
  private
    FImgR, FImgG, FImgB: TSingleMatrix;
    FSpectra: TImgSpectra;
    FSqSpectra: TImgSpectra;
    FSum, FSumSq: array[0..2] of TDoubleMatrix;
    FMaskKey: TSingleMatrix;                                 // identity of the cached mask
    FfMask, FfMask2: TComplexMatrix;                         // FFT(M), FFT(M^2)
    FCorrIM, FCorrIMSq, FCorrISqMSq: TChannelCorrelations;   // CCorr(I_c,M), CCorr(I_c,M^2), CCorr(I_c^2,M^2)
    procedure EnsureSums;
    procedure EnsureMaskFwd(const Mask, Mask2: TSingleMatrix);
    function GetSpectra: TImgSpectra;
    function GetSqSpectra: TImgSpectra;
    function GetSum(Channel: Integer): TDoubleMatrix;
    function GetSumSq(Channel: Integer): TDoubleMatrix;
  public
    Width, Height: Integer;
    class operator Initialize(var Self: TMatchTemplateCache);
    procedure Init(const Image: TIntegerMatrix);
    property Spectra: TImgSpectra read GetSpectra;                  // FFT of I_c        (masked + non-masked)
    property SqSpectra: TImgSpectra read GetSqSpectra;              // FFT of I_c^2      (masked normed / sqdiff)
    property Sum[Channel: Integer]: TDoubleMatrix read GetSum;      // SumsPd plain-sum integral image (non-masked CCOEFF)
    property SumSq[Channel: Integer]: TDoubleMatrix read GetSumSq;  // SumsPd sum-of-squares integral image (non-masked normed)
    function MaskCorrIM(const Mask, Mask2: TSingleMatrix; outW, outH: Integer): TChannelCorrelations;     // CCorr(I_c, M)
    function MaskCorrIMSq(const Mask, Mask2: TSingleMatrix; outW, outH: Integer): TChannelCorrelations;   // CCorr(I_c, M^2)
    function MaskCorrISqMSq(const Mask, Mask2: TSingleMatrix; outW, outH: Integer): TChannelCorrelations; // CCorr(I_c^2, M^2)
  end;

function MaskFromTemplate(Templ: TIntegerMatrix): TSingleMatrix;
procedure SplitChannels(const Image: TIntegerMatrix; out R, G, B: TSingleMatrix);

// pad real channel a to OptimalDFTSize(outW) x OptimalDFTSize(outH) and forward-transform.
function ForwardTransform(const a: TSingleMatrix; const outW, outH: Integer): TComplexMatrix;
// // Re(CorrSpectrum(fa, fb)), cropped to outW x outH.
function Correlate(const fa, fb: TComplexMatrix; const outW, outH: Integer): TSingleMatrix;
// Pack two real channels into one complex FFT2 (a -> Re, b -> Im). NO Hermitian unpack:
// the pair stays packed and is separated only at the correlation OUTPUT (Correlate/CorrelatePacked),
function ForwardTransformPacked(const a, b: TSingleMatrix; const outW, outH: Integer): TComplexMatrix;
// IFFT2(fImg * conj(fOp)) cropped; returns BOTH the real and imaginary outputs.
// fImg packs (a + i*b):
//   fOp a single real channel's spectrum  -> re = CCorr(a, op),  im = CCorr(b, op)
//   fOp a packed pair (c + i*d)            -> re = CCorr(a,c)+CCorr(b,d), im = CCorr(b,c)-CCorr(a,d)
procedure CorrelatePacked(const fImg, fOp: TComplexMatrix; const outW, outH: Integer; out re, im: TSingleMatrix);

// Correlate on all channels
function ForwardTransformChannels(const a, b, c: TSingleMatrix; const outW, outH: Integer): TImgSpectra;
function ForwardTransformChannelsSquared(const a, b, c: TSingleMatrix; const outW, outH: Integer): TImgSpectra;
function CorrelateChannelsCommon(const s: TImgSpectra; const fOp: TComplexMatrix; const outW, outH: Integer): TChannelCorrelations;
function CorrelateChannelsSum(const a, b: TImgSpectra; const outW, outH: Integer): TSingleMatrix;

// Basic matrix operations
function SumDouble(const m: TSingleMatrix): Double;
function SumOfSquaresChannels(const R, G, B: TSingleMatrix): Double;
function NormChannels(const R, G, B: TSingleMatrix): Double;
function SumChannelsMax(const R, G, B: TSingleMatrix; out maxVal: Double): TSingleMatrix;
function SubtractScalar(const m: TSingleMatrix; const s: Double): TSingleMatrix;
function MultiplyElements(const A, B: TSingleMatrix): TSingleMatrix;   // elementwise A.*B into a fresh result
function Sum(const Matrix: TSingleMatrix): Single;                     // sum of all elements
procedure NormalizeMasked(var Res: TSingleMatrix; const energy: TSingleMatrix; const maxEnergy, constFac: Double; const degenVal, loClamp, hiClamp: Single);

implementation

uses
  simba.vartype_matrix;

// Plain-sum + sum-of-squares integral images (summed-area tables), padded by one row/col. Used by the
// cache to build per-channel normalisers (EnsureSums).
function SumsPd(const Matrix: TSingleMatrix; out Square: TDoubleMatrix): TDoubleMatrix;
var
  x,y,W,H: Integer;
  sum,sqsum: Double;
begin
  H := Length(Matrix);
  W := Length(Matrix[0]);
  SetLength(Result, H+1,W+1);
  SetLength(Square, H+1,W+1);

  Result[1,1] := Matrix[0,0];
  Square[1,1] := Sqr(Matrix[0,0]);
  for y:=2 to H do
  begin
    Result[y,1] := Result[y-1,1] + Matrix[y-1,0];
    Square[y,1] := Square[y-1,1] + Sqr(Matrix[y-1,0]);
  end;

  for x:=2 to W do
  begin
    Result[1,x] := Result[1,x-1] + Matrix[0,x-1];
    Square[1,x] := Square[1,x-1] + Sqr(Matrix[0,x-1]);
  end;

  for y:=2 to H do
  begin
    sum   := Matrix[y-1,0];
    sqsum := Sqr(sum);
    for x:=2 to W do
    begin
      sum += Matrix[y-1,x-1];
      Result[y,x] := Result[y-1,x] + sum;
      sqsum += Sqr(Matrix[y-1,x-1]);
      Square[y,x] := Square[y-1,x] + sqsum;
    end;
  end;
end;

function Sum(const Matrix: TSingleMatrix): Single;
var
  W, H, X, Y: Integer;
begin
  Result := 0;
  W := Matrix.Width - 1;
  H := Matrix.Height - 1;
  for Y := 0 to H do
    for X := 0 to W do
      Result += Matrix[Y, X];
end;

function MultiplyElements(const A, B: TSingleMatrix): TSingleMatrix;
var
  W, H, X, Y: Integer;
begin
  W := A.Width - 1;
  H := A.Height - 1;
  SetLength(Result, H+1, W+1);
  for Y := 0 to H do
    for X := 0 to W do
      Result[Y, X] := A[Y, X] * B[Y, X];
end;

procedure SplitChannels(const Image: TIntegerMatrix; out R, G, B: TSingleMatrix);
var
  W, H, X, Y: Integer;
begin
  W := Image.Width;
  H := Image.Height;
  R.SetSize(W, H);
  G.SetSize(W, H);
  B.SetSize(W, H);
  Dec(W);
  Dec(H);
  for Y := 0 to H do
    for X := 0 to W do
    begin
      R[Y, X] := Image[Y, X]        and $FF;
      G[Y, X] := Image[Y, X] shr 08 and $FF;
      B[Y, X] := Image[Y, X] shr 16 and $FF;
    end;
end;

function ForwardTransform(const a: TSingleMatrix; const outW, outH: Integer): TComplexMatrix;
var
  Spectrum: TComplexMatrix;
  X, Y, W, H, sw: Integer;
begin
  Spectrum.SetSize(OptimalDFTSize(outW), OptimalDFTSize(outH));
  sw := Spectrum.Width;
  W := a.Width - 1;
  H := a.Height - 1;
  for Y := 0 to H do
    for X := 0 to W do
      Spectrum.Data[Y * sw + X].Re := a[Y, X];
  Result := FFT2(Spectrum);
end;

function CorrSpectrum(const fa, fb: TComplexMatrix): TComplexMatrix;
var
  Spectrum: TComplexMatrix;
  i, n: Integer;
begin
  Spectrum.SetSize(fa.Width, fa.Height);
  n := fa.Width * fa.Height;
  for i := 0 to n - 1 do
  begin
    Spectrum.Data[i].re := (fa.Data[i].re *  fb.Data[i].re) - (fa.Data[i].im * -fb.Data[i].im);
    Spectrum.Data[i].im := (fa.Data[i].re * -fb.Data[i].im) + (fa.Data[i].im *  fb.Data[i].re);
  end;
  Result := IFFT2(Spectrum);
end;

function Correlate(const fa, fb: TComplexMatrix; const outW, outH: Integer): TSingleMatrix;
var
  Spectrum: TComplexMatrix;
  X, Y, W, H, sw: Integer;
begin
  Spectrum := CorrSpectrum(fa, fb);
  Result.SetSize(outW, outH);
  sw := Spectrum.Width;
  W := outW - 1;
  H := outH - 1;
  for Y := 0 to H do
    for X := 0 to W do
      Result[Y, X] := Spectrum.Data[Y * sw + X].Re;
end;

procedure CorrelateInto(const fa, fb: TComplexMatrix; const outW, outH: Integer; var Acc: TSingleMatrix);
var
  Spectrum: TComplexMatrix;
  X, Y, W, H, sw: Integer;
begin
  Spectrum := CorrSpectrum(fa, fb);
  sw := Spectrum.Width;
  W := outW - 1;
  H := outH - 1;
  for Y := 0 to H do
    for X := 0 to W do
      Acc[Y, X] := Acc[Y, X] + Spectrum.Data[Y * sw + X].Re;
end;

function ForwardTransformPacked(const a, b: TSingleMatrix; const outW, outH: Integer): TComplexMatrix;
var
  Spectrum: TComplexMatrix;
  X, Y, W, H, sw: Integer;
begin
  Spectrum.SetSize(OptimalDFTSize(outW), OptimalDFTSize(outH));
  sw := Spectrum.Width;
  W := a.Width - 1;
  H := a.Height - 1;
  for Y := 0 to H do
    for X := 0 to W do
      Spectrum.Data[Y * sw + X].Re := a[Y, X];
  W := b.Width - 1;
  H := b.Height - 1;
  for Y := 0 to H do
    for X := 0 to W do
      Spectrum.Data[Y * sw + X].Im := b[Y, X];
  Result := FFT2(Spectrum);
end;

procedure CorrelatePacked(const fImg, fOp: TComplexMatrix; const outW, outH: Integer; out re, im: TSingleMatrix);
var
  Spectrum: TComplexMatrix;
  i, n, X, Y, W, H, sw: Integer;
begin
  Spectrum.SetSize(fImg.Width, fImg.Height);
  n := fImg.Width * fImg.Height;
  for i := 0 to n - 1 do          // fImg * conj(fOp)
  begin
    Spectrum.Data[i].re := (fImg.Data[i].re *  fOp.Data[i].re) - (fImg.Data[i].im * -fOp.Data[i].im);
    Spectrum.Data[i].im := (fImg.Data[i].re * -fOp.Data[i].im) + (fImg.Data[i].im *  fOp.Data[i].re);
  end;
  Spectrum := IFFT2(Spectrum);
  re.SetSize(outW, outH);
  im.SetSize(outW, outH);
  sw := Spectrum.Width;
  W := outW - 1;
  H := outH - 1;
  for Y := 0 to H do
    for X := 0 to W do
    begin
      re[Y, X] := Spectrum.Data[Y * sw + X].Re;
      im[Y, X] := Spectrum.Data[Y * sw + X].Im;
    end;
end;

function ForwardTransformChannels(const a, b, c: TSingleMatrix; const outW, outH: Integer): TImgSpectra;
begin
{$IFDEF MT_PACKED}
  Result.RG := ForwardTransformPacked(a, b, outW, outH);   // R,G packed into one complex FFT
  Result.B  := ForwardTransform(c, outW, outH);            // B alone
{$ELSE}
  Result.R := ForwardTransform(a, outW, outH);
  Result.G := ForwardTransform(b, outW, outH);
  Result.B := ForwardTransform(c, outW, outH);
{$ENDIF}
end;

function ForwardTransformSquared(const a: TSingleMatrix; const outW, outH: Integer): TComplexMatrix;
var
  Spectrum: TComplexMatrix;
  X, Y, W, H, sw: Integer;
begin
  Spectrum.SetSize(OptimalDFTSize(outW), OptimalDFTSize(outH));
  sw := Spectrum.Width;
  W := a.Width - 1;
  H := a.Height - 1;
  for Y := 0 to H do
    for X := 0 to W do
      Spectrum.Data[Y * sw + X].Re := a[Y, X] * a[Y, X];
  Result := FFT2(Spectrum);
end;

function ForwardTransformPackedSquared(const a, b: TSingleMatrix; const outW, outH: Integer): TComplexMatrix;
var
  Spectrum: TComplexMatrix;
  X, Y, W, H, sw: Integer;
begin
  Spectrum.SetSize(OptimalDFTSize(outW), OptimalDFTSize(outH));
  sw := Spectrum.Width;
  W := a.Width - 1;
  H := a.Height - 1;
  for Y := 0 to H do
    for X := 0 to W do
      Spectrum.Data[Y * sw + X].Re := a[Y, X] * a[Y, X];
  W := b.Width - 1;
  H := b.Height - 1;
  for Y := 0 to H do
    for X := 0 to W do
      Spectrum.Data[Y * sw + X].Im := b[Y, X] * b[Y, X];
  Result := FFT2(Spectrum);
end;

function ForwardTransformChannelsSquared(const a, b, c: TSingleMatrix; const outW, outH: Integer): TImgSpectra;
begin
{$IFDEF MT_PACKED}
  Result.RG := ForwardTransformPackedSquared(a, b, outW, outH);   // R^2,G^2 packed into one complex FFT
  Result.B  := ForwardTransformSquared(c, outW, outH);            // B^2 alone
{$ELSE}
  Result.R := ForwardTransformSquared(a, outW, outH);
  Result.G := ForwardTransformSquared(b, outW, outH);
  Result.B := ForwardTransformSquared(c, outW, outH);
{$ENDIF}
end;

function CorrelateChannelsCommon(const s: TImgSpectra; const fOp: TComplexMatrix; const outW, outH: Integer): TChannelCorrelations;
begin
{$IFDEF MT_PACKED}
  CorrelatePacked(s.RG, fOp, outW, outH, Result[0], Result[1]);   // R = Re, G = Im (one inverse FFT)
  Result[2] := Correlate(s.B, fOp, outW, outH);                   // B
{$ELSE}
  Result[0] := Correlate(s.R, fOp, outW, outH);
  Result[1] := Correlate(s.G, fOp, outW, outH);
  Result[2] := Correlate(s.B, fOp, outW, outH);
{$ENDIF}
end;

function CorrelateChannelsSum(const a, b: TImgSpectra; const outW, outH: Integer): TSingleMatrix;
begin
{$IFDEF MT_PACKED}
  Result := Correlate(a.RG, b.RG, outW, outH);
  CorrelateInto(a.B, b.B, outW, outH, Result);
{$ELSE}
  Result := Correlate(a.R, b.R, outW, outH);
  CorrelateInto(a.G, b.G, outW, outH, Result);
  CorrelateInto(a.B, b.B, outW, outH, Result);
{$ENDIF}
end;

function SumDouble(const m: TSingleMatrix): Double;
var
  X, Y, W, H: Integer;
begin
  Result := 0;
  W := m.Width - 1;
  H := m.Height - 1;
  for Y := 0 to H do
    for X := 0 to W do
      Result += m[Y, X];
end;

function SumOfSquaresChannels(const R, G, B: TSingleMatrix): Double;
var
  X, Y, W, H: Integer;
begin
  Result := 0;
  W := R.Width - 1;
  H := R.Height - 1;
  for Y := 0 to H do
    for X := 0 to W do
      Result += Sqr(R[Y, X]) + Sqr(G[Y, X]) + Sqr(B[Y, X]);
end;

function NormChannels(const R, G, B: TSingleMatrix): Double;
begin
  Result := Sqrt(SumOfSquaresChannels(R, G, B));
end;

function SumChannelsMax(const R, G, B: TSingleMatrix; out maxVal: Double): TSingleMatrix;
var
  X, Y, W, H: Integer;
  v: Single;
begin
  Result.SetSize(R.Width, R.Height);
  maxVal := 0;
  W := R.Width - 1;
  H := R.Height - 1;
  for Y := 0 to H do
    for X := 0 to W do
    begin
      v := R[Y, X] + G[Y, X] + B[Y, X];
      Result[Y, X] := v;
      if v > maxVal then maxVal := v;
    end;
end;

function SubtractScalar(const m: TSingleMatrix; const s: Double): TSingleMatrix;
var
  X, Y, W, H: Integer;
begin
  Result.SetSize(m.Width, m.Height);
  W := m.Width - 1;
  H := m.Height - 1;
  for Y := 0 to H do
    for X := 0 to W do
      Result[Y, X] := m[Y, X] - s;
end;

procedure NormalizeMasked(var Res: TSingleMatrix; const energy: TSingleMatrix; const maxEnergy, constFac: Double; const degenVal, loClamp, hiClamp: Single);
var
  x, y, w, h: Integer;
  thr: Double;
  v: Single;
begin
  w := Res.Width - 1;
  h := Res.Height - 1;
  thr := maxEnergy * 1e-4;
  for y := 0 to h do
    for x := 0 to w do
      if energy[y, x] <= thr then
        Res[y, x] := degenVal
      else
      begin
        v := Res[y, x] / Sqrt(constFac * energy[y, x]);
        if v < loClamp then
          v := loClamp
        else if v > hiClamp then
          v := hiClamp;
        Res[y, x] := v;
      end;
end;

function MaskFromTemplate(Templ: TIntegerMatrix): TSingleMatrix;
var
  X, Y, W, H: Integer;
begin
  Result.SetSize(Templ.Width, Templ.Height);

  W := Templ.Width - 1;
  H := Templ.Height - 1;
  for Y := 0 to H do
    for X := 0 to W do
      if (PByte(@Templ[Y, X])^ <> 0) then
        Result[Y, X] := 1;
end;

function SameMatrix(const a, b: TSingleMatrix): Boolean;
var
  X, Y: Integer;
begin
  if (Length(a) <> Length(b)) then
    Exit(False);
  if (Length(a) > 0) and (Length(a[0]) <> Length(b[0])) then
    Exit(False);
  for Y := 0 to High(a) do
    for X := 0 to High(a[Y]) do
      if (a[Y, X] <> b[Y, X]) then
        Exit(False);

  Result := True;
end;

class operator TMatchTemplateCache.Initialize(var Self: TMatchTemplateCache);
begin
  Self.Width := 0;
  Self.Height := 0;
end;

procedure TMatchTemplateCache.Init(const Image: TIntegerMatrix);
begin
  Self := Default(TMatchTemplateCache);
  Width := Image.Width;
  Height := Image.Height;
  SplitChannels(Image, FImgR, FImgG, FImgB);
end;

function TMatchTemplateCache.GetSpectra: TImgSpectra;
begin
  if (Length(FSpectra.B.Data) = 0) then
    FSpectra := ForwardTransformChannels(FImgR, FImgG, FImgB, Width, Height);
  Result := FSpectra;
end;

function TMatchTemplateCache.GetSqSpectra: TImgSpectra;
begin
  if (Length(FSqSpectra.B.Data) = 0) then
    FSqSpectra := ForwardTransformChannelsSquared(FImgR, FImgG, FImgB, Width, Height);
  Result := FSqSpectra;
end;

procedure TMatchTemplateCache.EnsureSums;
begin
  if (Length(FSum[0]) > 0) then
    Exit;

  FSum[0] := SumsPd(FImgR, FSumSq[0]);
  FSum[1] := SumsPd(FImgG, FSumSq[1]);
  FSum[2] := SumsPd(FImgB, FSumSq[2]);
end;

function TMatchTemplateCache.GetSum(Channel: Integer): TDoubleMatrix;
begin
  EnsureSums();
  Result := FSum[Channel];
end;

function TMatchTemplateCache.GetSumSq(Channel: Integer): TDoubleMatrix;
begin
  EnsureSums();
  Result := FSumSq[Channel];
end;

procedure TMatchTemplateCache.EnsureMaskFwd(const Mask, Mask2: TSingleMatrix);
begin
  if (Length(FfMask.Data) > 0) and SameMatrix(FMaskKey, Mask) then
    Exit; // static mask, no need to rebuild

  FMaskKey := Mask;
  FfMask   := ForwardTransform(Mask,  Width, Height);
  FfMask2  := ForwardTransform(Mask2, Width, Height);
  FCorrIM := Default(TChannelCorrelations);
  FCorrIMSq := Default(TChannelCorrelations);
  FCorrISqMSq := Default(TChannelCorrelations);
end;

function TMatchTemplateCache.MaskCorrIM(const Mask, Mask2: TSingleMatrix; outW, outH: Integer): TChannelCorrelations;
begin
  EnsureMaskFwd(Mask, Mask2);
  if (Length(FCorrIM[0]) = 0) then
    FCorrIM := CorrelateChannelsCommon(GetSpectra, FfMask, outW, outH);
  Result := FCorrIM;
end;

function TMatchTemplateCache.MaskCorrIMSq(const Mask, Mask2: TSingleMatrix; outW, outH: Integer): TChannelCorrelations;
begin
  EnsureMaskFwd(Mask, Mask2);
  if (Length(FCorrIMSq[0]) = 0) then
    FCorrIMSq := CorrelateChannelsCommon(GetSpectra, FfMask2, outW, outH);
  Result := FCorrIMSq;
end;

function TMatchTemplateCache.MaskCorrISqMSq(const Mask, Mask2: TSingleMatrix; outW, outH: Integer): TChannelCorrelations;
begin
  EnsureMaskFwd(Mask, Mask2);
  if (Length(FCorrISqMSq[0]) = 0) then
    FCorrISqMSq := CorrelateChannelsCommon(GetSqSpectra, FfMask2, outW, outH);
  Result := FCorrISqMSq;
end;

end.
