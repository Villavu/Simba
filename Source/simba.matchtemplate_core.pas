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
    RG: TComplexMatrix; // R packed into Re, G into Im
    B: TComplexMatrix;
  end;
{$ELSE}
  TImgSpectra = record
    R, G, B: TComplexMatrix;
  end;
{$ENDIF}
  TChannelCorrelations = array[0..2] of TSingleMatrix; // per-channel correlation results (R, G, B)

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

function MaskFromTemplate(const Templ: TIntegerMatrix): TSingleMatrix;
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

// Plain-sum + sum-of-squares integral images (summed-area tables), padded by one row/col.
function SumsPd(const Matrix: TSingleMatrix; out Square: TDoubleMatrix): TDoubleMatrix;
var
  x,y,W,H: Integer;
  sum,sqsum: Double;
  curMatrix: PSingle;
  prevResult, curResult, prevSquare, curSquare: PDouble;
  mv: Single;
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
    curMatrix := @Matrix[y-1][0];
    prevResult := @Result[y-1][0];
    curResult := @Result[y][0];
    prevSquare := @Square[y-1][0];
    curSquare := @Square[y][0];

    sum   := curMatrix[0];
    sqsum := Sqr(sum);
    for x:=2 to W do
    begin
      mv := curMatrix[x-1];
      sum += mv;
      curResult[x] := prevResult[x] + sum;
      sqsum += Sqr(mv);
      curSquare[x] := prevSquare[x] + sqsum;
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
  W, H, X, Y, pv: Integer;
  curImage: PInteger;
  curR, curG, curB: PSingle;
begin
  W := Image.Width;
  H := Image.Height;
  R.SetSize(W, H);
  G.SetSize(W, H);
  B.SetSize(W, H);
  Dec(W);
  Dec(H);
  for Y := 0 to H do
  begin
    curImage := @Image[Y][0];
    curR := @R[Y][0];
    curG := @G[Y][0];
    curB := @B[Y][0];
    for X := 0 to W do
    begin
      pv := curImage[X];
      curR[X] := pv        and $FF;
      curG[X] := pv shr 08 and $FF;
      curB[X] := pv shr 16 and $FF;
    end;
  end;
end;

function ForwardTransform(const a: TSingleMatrix; const outW, outH: Integer): TComplexMatrix;
var
  Spectrum: TComplexMatrix;
  X, Y, W, H, sw, base: Integer;
  curA: PSingle;
begin
  Spectrum.SetSize(OptimalDFTSize(outW), OptimalDFTSize(outH));
  sw := Spectrum.Width;
  W := a.Width - 1;
  H := a.Height - 1;
  for Y := 0 to H do
  begin
    curA := @a[Y][0];
    base := Y * sw;
    for X := 0 to W do
      Spectrum.Data[base + X].Re := curA[X];
  end;
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
  X, Y, W, H, sw, base: Integer;
  curResult: PSingle;
begin
  Spectrum := CorrSpectrum(fa, fb);
  Result.SetSize(outW, outH);
  sw := Spectrum.Width;
  W := outW - 1;
  H := outH - 1;
  for Y := 0 to H do
  begin
    curResult := @Result[Y][0]; base := Y * sw;
    for X := 0 to W do
      curResult[X] := Spectrum.Data[base + X].Re;
  end;
end;

procedure CorrelateInto(const fa, fb: TComplexMatrix; const outW, outH: Integer; var Acc: TSingleMatrix);
var
  Spectrum: TComplexMatrix;
  X, Y, W, H, sw, base: Integer;
  curAcc: PSingle;
begin
  Spectrum := CorrSpectrum(fa, fb);
  sw := Spectrum.Width;
  W := outW - 1;
  H := outH - 1;
  for Y := 0 to H do
  begin
    curAcc := @Acc[Y][0]; base := Y * sw;
    for X := 0 to W do
      curAcc[X] := curAcc[X] + Spectrum.Data[base + X].Re;
  end;
end;

function ForwardTransformPacked(const a, b: TSingleMatrix; const outW, outH: Integer): TComplexMatrix;
var
  Spectrum: TComplexMatrix;
  X, Y, W, H, sw, base: Integer;
  curA, curB: PSingle;
begin
  Spectrum.SetSize(OptimalDFTSize(outW), OptimalDFTSize(outH));
  sw := Spectrum.Width;
  W := a.Width - 1;
  H := a.Height - 1;
  for Y := 0 to H do
  begin
    curA := @a[Y][0]; base := Y * sw;
    for X := 0 to W do
      Spectrum.Data[base + X].Re := curA[X];
  end;
  W := b.Width - 1;
  H := b.Height - 1;
  for Y := 0 to H do
  begin
    curB := @b[Y][0]; base := Y * sw;
    for X := 0 to W do
      Spectrum.Data[base + X].Im := curB[X];
  end;
  Result := FFT2(Spectrum);
end;

procedure CorrelatePacked(const fImg, fOp: TComplexMatrix; const outW, outH: Integer; out re, im: TSingleMatrix);
var
  Spectrum: TComplexMatrix;
  i, n, X, Y, W, H, sw, base: Integer;
  curRe, curIm: PSingle;
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
  begin
    curRe := @re[Y][0]; curIm := @im[Y][0]; base := Y * sw;
    for X := 0 to W do
    begin
      curRe[X] := Spectrum.Data[base + X].Re;
      curIm[X] := Spectrum.Data[base + X].Im;
    end;
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
  X, Y, W, H, sw, base: Integer;
  curA: PSingle; av: Single;
begin
  Spectrum.SetSize(OptimalDFTSize(outW), OptimalDFTSize(outH));
  sw := Spectrum.Width;
  W := a.Width - 1;
  H := a.Height - 1;
  for Y := 0 to H do
  begin
    curA := @a[Y][0]; base := Y * sw;
    for X := 0 to W do
    begin
      av := curA[X];
      Spectrum.Data[base + X].Re := av * av;
    end;
  end;
  Result := FFT2(Spectrum);
end;

function ForwardTransformPackedSquared(const a, b: TSingleMatrix; const outW, outH: Integer): TComplexMatrix;
var
  Spectrum: TComplexMatrix;
  X, Y, W, H, sw, base: Integer;
  curA, curB: PSingle; v: Single;
begin
  Spectrum.SetSize(OptimalDFTSize(outW), OptimalDFTSize(outH));
  sw := Spectrum.Width;
  W := a.Width - 1;
  H := a.Height - 1;
  for Y := 0 to H do
  begin
    curA := @a[Y][0];
    base := Y * sw;
    for X := 0 to W do
    begin
      v := curA[X];
      Spectrum.Data[base + X].Re := v * v;
    end;
  end;
  W := b.Width - 1;
  H := b.Height - 1;
  for Y := 0 to H do
  begin
    curB := @b[Y][0]; base := Y * sw;
    for X := 0 to W do
    begin
      v := curB[X];
      Spectrum.Data[base + X].Im := v * v;
    end;
  end;
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
  v, mx: Single;
  curR, curG, curB, curResult: PSingle;
begin
  Result.SetSize(R.Width, R.Height);
  W := R.Width - 1;
  H := R.Height - 1;
  mx := 0;
  for Y := 0 to H do
  begin
    curR := @R[Y][0];
    curG := @G[Y][0];
    curB := @B[Y][0];
    curResult := @Result[Y][0];
    for X := 0 to W do
    begin
      v := curR[X] + curG[X] + curB[X];
      curResult[X] := v;
      if v > mx then
        mx := v;
    end;
  end;
  maxVal := mx;
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

{$IFDEF SIMBA_FFT_SIMD_X86_64}
{$asmmode intel}
procedure NormalizeMasked(var Res: TSingleMatrix; const energy: TSingleMatrix; const maxEnergy, constFac: Double; const degenVal, loClamp, hiClamp: Single);
var
  h, n, y: Integer;
  thr: Double;
  consts: array[0..7] of Single;
  constsPtr: PSingle;
  resRows, enRows: Pointer;
begin
  n := Res.Width;              // row width in elements
  h := Res.Height - 1;         // last row index
  thr := maxEnergy * 1e-4;
  consts[0] := constFac; consts[1] := thr;     consts[2] := degenVal;
  consts[3] := loClamp;  consts[4] := hiClamp; consts[7] := 1.0;
  constsPtr := @consts[0];
  resRows := Pointer(Res);     // &Res[0]    -> contiguous array of row pointers
  enRows  := Pointer(energy);  // &energy[0] -> contiguous array of row pointers
  asm
    mov      eax, h
    test     eax, eax
    js       @@done                          // h < 0 (empty) -> no rows
    xor      eax, eax
    mov      y, eax                           // y := 0
  @@row:
    mov      r11d, y                          // r11 = y (zero-extended; y >= 0)
    mov      r10, resRows
    mov      rax, [r10+r11*8]                 // rax = @Res[y][0]     (row to normalise)
    mov      r10, enRows
    mov      rdx, [r10+r11*8]                 // rdx = @energy[y][0]
    mov      r9,  constsPtr
    mov      r8d, n
    mov      ecx, r8d
    shr      ecx, 2                         // 4-lane vectors
    and      r8d, 3
    test     ecx, ecx
    jz       @@rem
  @@loop4:
    movups   xmm0, [rdx]                    // e
    movss    xmm1, [r9]
    shufps   xmm1, xmm1, 0                  // cf
    mulps    xmm1, xmm0                     // arg = cf*e
    movups   xmm2, [rax]                    // r
    movss    xmm5, [r9+4]
    shufps   xmm5, xmm5, 0                  // thr
    movaps   xmm4, xmm0
    cmpps    xmm4, xmm5, 2                  // e <= thr
    xorps    xmm3, xmm3
    movaps   xmm5, xmm1
    cmpps    xmm5, xmm3, 2                  // arg <= 0
    orps     xmm4, xmm5                     // degenerate mask (xmm4)
    movss    xmm5, [r9+28]
    shufps   xmm5, xmm5, 0                  // 1.0
    movaps   xmm3, xmm4
    andps    xmm5, xmm3                     // mask & 1.0
    andnps   xmm3, xmm1                     // ~mask & arg
    orps     xmm5, xmm3                     // argSafe (>0)
    sqrtps   xmm5, xmm5
    divps    xmm2, xmm5                     // v = r/sqrt(argSafe)
    movss    xmm5, [r9+12]
    shufps   xmm5, xmm5, 0                  // lo
    maxps    xmm2, xmm5
    movss    xmm5, [r9+16]
    shufps   xmm5, xmm5, 0                  // hi
    minps    xmm2, xmm5
    movss    xmm5, [r9+8]
    shufps   xmm5, xmm5, 0                  // degen
    andps    xmm5, xmm4                     // degen & mask
    andnps   xmm4, xmm2                     // ~mask & v
    orps     xmm5, xmm4                     // result
    movups   [rax], xmm5
    add      rdx, 16
    add      rax, 16
    dec      ecx
    jnz      @@loop4
  @@rem:
    test     r8d, r8d
    jz       @@rownext
  @@rloop:
    movss    xmm0, [rdx]
    movss    xmm1, [r9]
    mulss    xmm1, xmm0                     // arg
    movss    xmm2, [rax]
    movss    xmm4, [r9+4]
    ucomiss  xmm0, xmm4
    jbe      @@rdeg                         // e <= thr
    xorps    xmm4, xmm4
    ucomiss  xmm1, xmm4
    jbe      @@rdeg                         // arg <= 0
    sqrtss   xmm3, xmm1
    divss    xmm2, xmm3
    movss    xmm4, [r9+12]
    maxss    xmm2, xmm4
    movss    xmm4, [r9+16]
    minss    xmm2, xmm4
    movss    [rax], xmm2
    jmp      @@rnext
  @@rdeg:
    movss    xmm3, [r9+8]
    movss    [rax], xmm3
  @@rnext:
    add      rdx, 4
    add      rax, 4
    dec      r8d
    jnz      @@rloop
  @@rownext:
    mov      eax, y
    inc      eax
    mov      y, eax
    cmp      eax, h
    jle      @@row                          // for y := 0 to h
  @@done:
  end;
end;
{$ELSE}
procedure NormalizeMasked(var Res: TSingleMatrix; const energy: TSingleMatrix; const maxEnergy, constFac: Double; const degenVal, loClamp, hiClamp: Single);
var
  x, y, w, h: Integer;
  thr, arg: Double;
  v: Single;
begin
  w := Res.Width - 1;
  h := Res.Height - 1;
  thr := maxEnergy * 1e-4;
  for y := 0 to h do
    for x := 0 to w do
    begin
      arg := constFac * energy[y, x];
      if (energy[y, x] <= thr) or (arg <= 0) then
        Res[y, x] := degenVal
      else
      begin
        v := Res[y, x] / Sqrt(arg);
        if v < loClamp then
          v := loClamp
        else if v > hiClamp then
          v := hiClamp;
        Res[y, x] := v;
      end;
    end;
end;
{$ENDIF}

function MaskFromTemplate(const Templ: TIntegerMatrix): TSingleMatrix;
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
