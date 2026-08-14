{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Portions derived from the Python Imaging Library (Pillow),
  src/libImaging/resample.c, used under the HPND License:

  Copyright (c) 1997-2011 by Secret Labs AB
  Copyright (c) 1995-2011 by Fredrik Lundh and contributors
  Copyright (c) 2010 by Jeffrey A. Clark and contributors
}
unit simba.image_resample;

{$i simba.inc}

interface

uses
  simba.base, simba.image;

function SimbaImage_Resample(Image: TSimbaImage; NewWidth, NewHeight: Integer; Algo: EImageResizeAlgo): TSimbaImage;
function SimbaImage_ResampleMasked(Image: TSimbaImage; NewWidth, NewHeight: Integer; Algo: EImageResizeAlgo; const Ignore: TBooleanArray): TSimbaImage;

implementation

uses
  Math;

const
  RESAMPLE_BITS = 14; // Q14 weights: fit int16 (for SSE pmaddwd), ample for 8-bit output

type
  TFilterFunc = function(X: Double): Double;
  TFilter = record
    Func: TFilterFunc;
    Support: Double;
  end;
  TInt16Array = array of Int16;

function BoxFilter(X: Double): Double;
begin
  if (X > -0.5) and (X <= 0.5) then Result := 1.0 else Result := 0.0;
end;

function BilinearFilter(X: Double): Double;
begin
  if (X < 0.0) then X := -X;
  if (X < 1.0) then Result := 1.0 - X else Result := 0.0;
end;

function HammingFilter(X: Double): Double;
begin
  if (X < 0.0) then X := -X;
  if (X = 0.0) then Exit(1.0);
  if (X >= 1.0) then Exit(0.0);
  X := X * Pi;
  Result := Sin(X) / X * (0.54 + 0.46 * Cos(X));
end;

function BicubicFilter(X: Double): Double;
const
  A = -0.5; // Catmull-Rom-ish; Pillow's default
begin
  if (X < 0.0) then X := -X;
  if (X < 1.0) then Exit(((A + 2.0) * X - (A + 3.0)) * X * X + 1.0);
  if (X < 2.0) then Exit((((X - 5.0) * X + 8.0) * X - 4.0) * A);
  Result := 0.0;
end;

function SincFilter(X: Double): Double;
begin
  if (X = 0.0) then Exit(1.0);
  X := X * Pi;
  Result := Sin(X) / X;
end;

function LanczosFilter(X: Double): Double;
begin
  // truncated sinc, radius 3
  if (X >= -3.0) and (X < 3.0) then
    Result := SincFilter(X) * SincFilter(X / 3.0)
  else
    Result := 0.0;
end;

// Convolution kernels only; NEAREST_NEIGHBOUR is point-sampled and dispatched before this.
function GetFilter(Algo: EImageResizeAlgo): TFilter;
begin
  case Algo of
    EImageResizeAlgo.BOX:      begin Result.Func := @BoxFilter;      Result.Support := 0.5; end;
    EImageResizeAlgo.BILINEAR: begin Result.Func := @BilinearFilter; Result.Support := 1.0; end;
    EImageResizeAlgo.HAMMING:  begin Result.Func := @HammingFilter;  Result.Support := 1.0; end;
    EImageResizeAlgo.BICUBIC:  begin Result.Func := @BicubicFilter;  Result.Support := 2.0; end;
    EImageResizeAlgo.LANCZOS:  begin Result.Func := @LanczosFilter;  Result.Support := 3.0; end;
    else                       SimbaException('GetFilter: no kernel for this algo');
  end;
end;

function Clip8(const Value: Integer): Byte; inline;
begin
  Result := EnsureRange(SarLongint(Value, RESAMPLE_BITS), 0, 255);
end;

function PrecomputeCoeffs(InSize, OutSize: Integer; const Filter: TFilter; out Starts, Counts: TIntegerArray; out KK: TInt16Array): Integer;
var
  Support, Scale, FilterScale, Center, WeightSum, InvFilterScale, Weight: Double;
  OutIndex, Tap, KSize, Stride, WindowStart, WindowCount, RowBase, FixedWeight: Integer;
  Weights: TDoubleArray;
begin
  Scale := InSize / OutSize;
  FilterScale := Scale;
  if (FilterScale < 1.0) then
    FilterScale := 1.0; // shrink -> widen kernel (low-pass)

  Support := Filter.Support * FilterScale;
  KSize  := Ceil(Support) * 2 + 1;
  Stride := KSize + 1;                     // trailing zero for the SSE tail
  Result := Stride;
  InvFilterScale := 1.0 / FilterScale;     // filter-space scale (loop-invariant)

  SetLength(Starts, OutSize);
  SetLength(Counts, OutSize);
  SetLength(KK, OutSize * Stride);         // dynamic array -> zero-filled (the pad slots stay 0)
  SetLength(Weights, KSize);

  for OutIndex := 0 to OutSize - 1 do
  begin
    Center := (OutIndex + 0.5) * Scale;
    WeightSum := 0.0;

    WindowStart := Trunc(Center - Support + 0.5);
    if (WindowStart < 0) then
      WindowStart := 0;
    WindowCount := Trunc(Center + Support + 0.5);
    if (WindowCount > InSize) then
      WindowCount := InSize;
    WindowCount := WindowCount - WindowStart; // now the number of contributing source pixels

    for Tap := 0 to WindowCount - 1 do
    begin
      Weight := Filter.Func((Tap + WindowStart - Center + 0.5) * InvFilterScale);
      Weights[Tap] := Weight;
      WeightSum := WeightSum + Weight;
    end;
    if (WeightSum <> 0.0) then
      for Tap := 0 to WindowCount - 1 do
        Weights[Tap] := Weights[Tap] / WeightSum; // normalise so the window sums to 1

    RowBase := OutIndex * Stride;
    for Tap := 0 to WindowCount - 1 do
    begin
      if (Weights[Tap] < 0) then
        FixedWeight := Trunc(-0.5 + Weights[Tap] * (1 shl RESAMPLE_BITS))
      else
        FixedWeight := Trunc( 0.5 + Weights[Tap] * (1 shl RESAMPLE_BITS));

      KK[RowBase + Tap] := EnsureRange(FixedWeight, -32768, 32767); // fits int16 (weights <= ~1.0 * 2^14)
    end;

    Starts[OutIndex] := WindowStart;
    Counts[OutIndex] := WindowCount;
  end;
end;

procedure ResampleHorizontal(Src, Dst: PColorBGRA; Starts, Counts: PInteger; KK: PInt16; OutDim, RowCount, KSize, SrcStride: Integer);
{$IF DEFINED(IMAGE_ASM)}
  {$I asm/resamplehorizontal_x86_64.inc}
{$ELSE}
var
  OutX, Row, Tap, TapCount, Weight, SumB, SumG, SumR: Integer;
  SrcRow, DstRow, SrcPtr: PColorBGRA;
  WeightPtr: PInt16;
begin
  SrcRow := Src;
  DstRow := Dst;

  for Row := 0 to RowCount - 1 do
  begin
    for OutX := 0 to OutDim - 1 do
    begin
      TapCount  := Counts[OutX];
      SrcPtr    := SrcRow + Starts[OutX]; // window start
      WeightPtr := KK + OutX * KSize;     // this pixel's weights

      SumB := 1 shl (RESAMPLE_BITS - 1);
      SumG := SumB;
      SumR := SumB;

      for Tap := 1 to TapCount do
      begin
        Weight := WeightPtr^;
        SumB := SumB + SrcPtr^.B * Weight;
        SumG := SumG + SrcPtr^.G * Weight;
        SumR := SumR + SrcPtr^.R * Weight;
        Inc(SrcPtr);
        Inc(WeightPtr);
      end;

      with DstRow[OutX] do
      begin
        B := Clip8(SumB);
        G := Clip8(SumG);
        R := Clip8(SumR);
        A := 255;
      end;
    end;

    Inc(SrcRow, SrcStride);
    Inc(DstRow, OutDim);
  end;
end;
{$ENDIF}

procedure ResampleVertical(Src, Dst: PColorBGRA; Starts, Counts: PInteger; KK: PInt16; OutDim, RowCount, KSize: Integer);
{$IF DEFINED(IMAGE_ASM)}
  {$I asm/resamplevertical_x86_64.inc}
{$ELSE}
var
  OutX, Row, Tap, TapCount, Weight, SumB, SumG, SumR: Integer;
  SrcRowStart, DstRow, SrcPtr: PColorBGRA;
  WeightRow, WeightPtr: PInt16;
begin
  DstRow := Dst;

  for Row := 0 to RowCount - 1 do
  begin
    SrcRowStart := Src + Starts[Row] * OutDim; // row ymin (stride == width)
    WeightRow   := KK + Row * KSize;           // this row's weights
    TapCount    := Counts[Row];

    for OutX := 0 to OutDim - 1 do
    begin
      SrcPtr := SrcRowStart + OutX;
      WeightPtr := WeightRow;
      SumB := 1 shl (RESAMPLE_BITS - 1);
      SumG := SumB; SumR := SumB;

      for Tap := 1 to TapCount do
      begin
        Weight := WeightPtr^;
        SumB := SumB + SrcPtr^.B * Weight;
        SumG := SumG + SrcPtr^.G * Weight;
        SumR := SumR + SrcPtr^.R * Weight;
        Inc(SrcPtr, OutDim);
        Inc(WeightPtr);
      end;

      with DstRow[OutX] do
      begin
        B := Clip8(SumB);
        G := Clip8(SumG);
        R := Clip8(SumR);
        A := 255;
      end;
    end;

    Inc(DstRow, OutDim);
  end;
end;
{$ENDIF}

function SimbaImage_Resample(Image: TSimbaImage; NewWidth, NewHeight: Integer; Algo: EImageResizeAlgo): TSimbaImage;
var
  InputWidth, InputHeight, HorzStride, VertStride, X, Y: Integer;
  HorzStarts, HorzCounts, VertStarts, VertCounts: TIntegerArray;
  HorzWeights, VertWeights: TInt16Array;
  FilterKernel: TFilter;
  Intermediate: array of TColorBGRA;   // horizontal intermediate (NewWidth x InputHeight)
begin
  InputWidth := Image.Width;
  InputHeight := Image.Height;

  Result := TSimbaImage.Create(NewWidth, NewHeight);
  if (InputWidth = 0) or (InputHeight = 0) or (NewWidth = 0) or (NewHeight = 0) then
    Exit;

  // Nearest-neighbour: point-sampled, no filtering -- a buffer-free direct gather.
  if (Algo = EImageResizeAlgo.NEAREST_NEIGHBOUR) then
  begin
    for Y := 0 to NewHeight - 1 do
      for X := 0 to NewWidth - 1 do
        Result.Data[Y * NewWidth + X] := Image.Data[((Y * InputHeight) div NewHeight) * InputWidth + (X * InputWidth) div NewWidth];
    Exit;
  end;

  FilterKernel := GetFilter(Algo);
  HorzStride := PrecomputeCoeffs(InputWidth,  NewWidth,  FilterKernel, HorzStarts, HorzCounts, HorzWeights);
  VertStride := PrecomputeCoeffs(InputHeight, NewHeight, FilterKernel, VertStarts, VertCounts, VertWeights);

  // horizontal Image -> Intermediate (NewWidth x InputHeight), then vertical Intermediate -> Result
  SetLength(Intermediate, NewWidth * InputHeight);
  ResampleHorizontal(@Image.Data[0], @Intermediate[0], @HorzStarts[0], @HorzCounts[0], @HorzWeights[0], NewWidth, InputHeight, HorzStride, InputWidth);
  ResampleVertical  (@Intermediate[0], @Result.Data[0], @VertStarts[0], @VertCounts[0], @VertWeights[0], NewWidth, NewHeight, VertStride);
end;

type
  TMaskAccum = record B, G, R, Coverage: Int32; end;
  PMaskAccum = ^TMaskAccum;

procedure ResampleHorizontalMasked(Src: PColorBGRA; Mask: PByte; Dst: PMaskAccum; Starts, Counts: PInteger; KK: PInt16; OutDim, RowCount, KSize, SrcStride: Integer);
var
  OutX, Row, Tap, TapCount, Weight, AB, AG, AR, ACov: Integer;
  SrcRow, SrcPtr: PColorBGRA;
  MaskRow, MaskPtr: PByte;
  DstRow: PMaskAccum;
  WeightPtr: PInt16;
begin
  SrcRow := Src;
  MaskRow := Mask;
  DstRow := Dst;

  for Row := 0 to RowCount - 1 do
  begin
    for OutX := 0 to OutDim - 1 do
    begin
      TapCount  := Counts[OutX];
      SrcPtr    := SrcRow + Starts[OutX];
      MaskPtr   := MaskRow + Starts[OutX];
      WeightPtr := KK + OutX * KSize;
      AB := 0;
      AG := 0;
      AR := 0;
      ACov := 0;

      for Tap := 1 to TapCount do
      begin
        if (MaskPtr^ = 0) then // 0 = kept
        begin
          Weight := WeightPtr^;
          AB := AB + SrcPtr^.B * Weight;
          AG := AG + SrcPtr^.G * Weight;
          AR := AR + SrcPtr^.R * Weight;
          ACov := ACov + Weight;
        end;

        Inc(SrcPtr);
        Inc(MaskPtr);
        Inc(WeightPtr);
      end;

      with DstRow[OutX] do
      begin
        B := AB;
        G := AG;
        R := AR;
        Coverage := ACov;
      end;
    end;

    Inc(SrcRow, SrcStride);
    Inc(MaskRow, SrcStride);
    Inc(DstRow, OutDim);
  end;
end;

procedure ResampleVerticalMasked(Src: PMaskAccum; Dst: PColorBGRA; Starts, Counts: PInteger; KK: PInt16; OutDim, RowCount, KSize: Integer);
var
  OutX, Row, Tap, TapCount, Weight: Integer;
  SumB, SumG, SumR, SumCov, Bias: Int64;
  SrcRowStart, SrcPtr: PMaskAccum;
  DstRow: PColorBGRA;
  WeightRow, WeightPtr: PInt16;
begin
  DstRow := Dst;

  for Row := 0 to RowCount - 1 do
  begin
    SrcRowStart := Src + Starts[Row] * OutDim;
    WeightRow   := KK + Row * KSize;
    TapCount    := Counts[Row];

    for OutX := 0 to OutDim - 1 do
    begin
      SrcPtr := SrcRowStart + OutX;
      WeightPtr := WeightRow;
      SumB := 0;
      SumG := 0;
      SumR := 0;
      SumCov := 0;

      for Tap := 1 to TapCount do
      begin
        Weight := WeightPtr^;
        SumB   := SumB   + Int64(Weight) * SrcPtr^.B;
        SumG   := SumG   + Int64(Weight) * SrcPtr^.G;
        SumR   := SumR   + Int64(Weight) * SrcPtr^.R;
        SumCov := SumCov + Int64(Weight) * SrcPtr^.Coverage;

        Inc(SrcPtr, OutDim);
        Inc(WeightPtr);
      end;

      with DstRow[OutX] do
      begin
        if (SumCov <= 0) then
        begin
          B := 0;
          G := 0;
          R := 0;
          A := 255;
        end else
        begin
          Bias := SumCov div 2;
          B := EnsureRange((SumB + Bias) div SumCov, 0, 255);
          G := EnsureRange((SumG + Bias) div SumCov, 0, 255);
          R := EnsureRange((SumR + Bias) div SumCov, 0, 255);
          A := 255;
        end;
      end;
    end;

    Inc(DstRow, OutDim);
  end;
end;

function SimbaImage_ResampleMasked(Image: TSimbaImage; NewWidth, NewHeight: Integer; Algo: EImageResizeAlgo; const Ignore: TBooleanArray): TSimbaImage;
var
  InputWidth, InputHeight, HorzStride, VertStride, X, Y, SrcIdx: Integer;
  HorzStarts, HorzCounts, VertStarts, VertCounts: TIntegerArray;
  HorzWeights, VertWeights: TInt16Array;
  FilterKernel: TFilter;
  Intermediate: array of TMaskAccum; // horizontal intermediate
  SrcPtr, DstPtr: PColorBGRA;
  IgnorePtr: PByte;
begin
  InputWidth := Image.Width;
  InputHeight := Image.Height;

  Result := TSimbaImage.Create(NewWidth, NewHeight);
  if (InputWidth = 0) or (InputHeight = 0) or (NewWidth = 0) or (NewHeight = 0) then
    Exit;

  // Nearest-neighbour with ignore points: an output pixel whose nearest source
  // pixel is ignored expands to nothing -- it keeps the default (black) pixel.
  if (Algo = EImageResizeAlgo.NEAREST_NEIGHBOUR) then
  begin
    SrcPtr := Image.Data;
    DstPtr := Result.Data;
    IgnorePtr := PByte(@Ignore[0]);
    for Y := 0 to NewHeight - 1 do
      for X := 0 to NewWidth - 1 do
      begin
        SrcIdx := ((Y * InputHeight) div NewHeight) * InputWidth + (X * InputWidth) div NewWidth;
        if (IgnorePtr[SrcIdx] = 0) then // 0 = kept; ignored source pixels expand to nothing
          DstPtr[Y * NewWidth + X] := SrcPtr[SrcIdx];
      end;
    Exit;
  end;

  FilterKernel := GetFilter(Algo);
  HorzStride := PrecomputeCoeffs(InputWidth,  NewWidth,  FilterKernel, HorzStarts, HorzCounts, HorzWeights);
  VertStride := PrecomputeCoeffs(InputHeight, NewHeight, FilterKernel, VertStarts, VertCounts, VertWeights);

  SetLength(Intermediate, NewWidth * InputHeight);
  ResampleHorizontalMasked(@Image.Data[0], PByte(@Ignore[0]), @Intermediate[0], @HorzStarts[0], @HorzCounts[0], @HorzWeights[0], NewWidth, InputHeight, HorzStride, InputWidth);
  ResampleVerticalMasked  (@Intermediate[0], @Result.Data[0], @VertStarts[0], @VertCounts[0], @VertWeights[0], NewWidth, NewHeight, VertStride);
end;

end.
