{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.image_filters;

{$DEFINE SIMBA_MAX_OPTIMIZATION}
{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.image;

function SimbaImage_GreyScale(Image: TSimbaImage): TSimbaImage;
function SimbaImage_Brightness(Image: TSimbaImage; Value: Integer): TSimbaImage;
function SimbaImage_Invert(Image: TSimbaImage): TSimbaImage;
function SimbaImage_Posterize(Image: TSimbaImage; Value: Integer): TSimbaImage;
function SimbaImage_Sobel(Image: TSimbaImage): TSimbaImage;
function SimbaImage_Enhance(Image: TSimbaImage; Enchantment: Byte; C: Single): TSimbaImage;
function SimbaImage_BlurBox(Image: TSimbaImage; Radius: Integer): TSimbaImage;
function SimbaImage_BlurGauss(Image: TSimbaImage; Radius: Integer): TSimbaImage;
function SimbaImage_Threshold(Image: TSimbaImage; Invert: Boolean; C: Integer): TSimbaImage;
function SimbaImage_ThresholdAdaptive(Image: TSimbaImage; Invert: Boolean; Radius: Integer; C: Integer): TSimbaImage;
function SimbaImage_ThresholdAdaptiveSauvola(Image: TSimbaImage; Invert: Boolean; Radius: Integer; C: Single): TSimbaImage;

implementation

uses
  Math,
  simba.image_utils, simba.vartype_ordmatrix;

function SimbaImage_GreyScale(Image: TSimbaImage): TSimbaImage;
var
  I: Integer;
  Src, Dest: PColorBGRA;
  Lum: Byte;
begin
  Result := TSimbaImage.Create(Image.Width, Image.Height);

  Src := Image.Data;
  Dest := Result.Data;

  for I := (Image.Height * Image.Width - 1) downto 0 do
  begin
    Lum := Round(Src^.R * 0.299 + Src^.G * 0.587 + Src^.B * 0.114);

    Dest^.R := Lum;
    Dest^.G := Lum;
    Dest^.B := Lum;
    Dest^.A := ALPHA_OPAQUE;

    Inc(Src);
    Inc(Dest);
  end;
end;

function SimbaImage_Brightness(Image: TSimbaImage; Value: Integer): TSimbaImage;
var
  I: Integer;
  Src, Dest: PColorBGRA;
begin
  Result := TSimbaImage.Create(Image.Width, Image.Height);

  Src := Image.Data;
  Dest := Result.Data;

  for I := (Image.Height * Image.Width - 1) downto 0 do
  begin
    Dest^.R := EnsureRange(Src^.R + Value, 0, 255);
    Dest^.G := EnsureRange(Src^.G + Value, 0, 255);
    Dest^.B := EnsureRange(Src^.B + Value, 0, 255);
    Dest^.A := ALPHA_OPAQUE;

    Inc(Src);
    Inc(Dest);
  end;
end;

function SimbaImage_Invert(Image: TSimbaImage): TSimbaImage;
var
  I: Integer;
  Src, Dest: PColorBGRA;
begin
  Result := TSimbaImage.Create(Image.Width, Image.Height);

  Src := Image.Data;
  Dest := Result.Data;

  for I := (Image.Height * Image.Width - 1) downto 0 do
  begin
    Dest^.R := not Src^.R;
    Dest^.G := not Src^.G;
    Dest^.B := not Src^.B;
    Dest^.A := ALPHA_OPAQUE;

    Inc(Src);
    Inc(Dest);
  end;
end;

function SimbaImage_Posterize(Image: TSimbaImage; Value: Integer): TSimbaImage;
var
  I: Integer;
  Src, Dest: PColorBGRA;
begin
  if not InRange(Value, 1, 255) then
    SimbaException('TSimbaImage.Posterize: Value(%d) out of range[1..255]', [Value]);

  Result := TSimbaImage.Create(Image.Width, Image.Height);

  Src := Image.Data;
  Dest := Result.Data;

  for I := (Image.Height * Image.Width - 1) downto 0 do
  begin
    Dest^.A := ALPHA_OPAQUE;
    Dest^.R := Min(Round(Src^.R / Value) * Value, 255);
    Dest^.G := Min(Round(Src^.G / Value) * Value, 255);
    Dest^.B := Min(Round(Src^.B / Value) * Value, 255);

    Inc(Src);
    Inc(Dest);
  end;
end;

function SimbaImage_Sobel(Image: TSimbaImage): TSimbaImage;
var
  x,y,xx,yy,W,H,gx,gy:Int32;
  color: Byte;
  opx,opy: TIntegerMatrix;
  Grey:TByteMatrix;
begin
  Grey := Image.ToGreyMatrix;
  Result := TSimbaImage.Create(Image.Width, Image.Height);

  SetLength(opx, 3,3);
  opx[0][0] := -1; opx[0][1] := 0; opx[0][2] := 1;
  opx[1][0] := -2; opx[1][1] := 0; opx[1][2] := 2;
  opx[2][0] := -1; opx[2][1] := 0; opx[2][2] := 1;

  SetLength(opy, 3,3);
  opy[0][0] := -1; opy[0][1] := -2; opy[0][2] := -1;
  opy[1][0] :=  0; opy[1][1] :=  0; opy[1][2] := 0;
  opy[2][0] :=  1; opy[2][1] :=  2; opy[2][2] := 1;

  W := Image.Width-1;
  H := Image.Height-1;
  for y:=1 to H do
    for x:=1 to W do
    begin
      gx := 0;
      gy := 0;
      for yy:=0 to 2 do
        for xx:=0 to 2 do
        begin
          gx := gx + (opx[yy][xx] * Grey[y + yy - 1][x + xx - 1]);
          gy := gy + (opy[yy][xx] * Grey[y + yy - 1][x + xx - 1]);
        end;

      Color := EnsureRange(Trunc(Sqrt(gx*gx + gy*gy)), 0, 255);
      with Result.Data[Y * Result.Width + X] do
      begin
        R := Color;
        G := Color;
        B := Color;
      end;
    end;
end;

function SimbaImage_Enhance(Image: TSimbaImage; Enchantment: Byte; C: Single): TSimbaImage;
var
  W,H,x,y,R,G,B:Integer;
  mid: Single;
begin
  Result := TSimbaImage.Create(Image.Width, Image.Height);
  Mid := 127 * C;
  W := Image.Width - 1;
  H := Image.Height - 1;
  for y:=0 to H do
    for x:=0 to W do
    begin
      R := Image.Data[Y * Image.Width + X].R;
      G := Image.Data[Y * Image.Width + X].G;
      B := Image.Data[Y * Image.Width + X].B;

      if R > mid then
      begin
        R := R + Enchantment;
        if (R > 255) then R := 255;
      end else
      begin
        R := R - Enchantment;
        if (R < 0) then R := 0;
      end;

      if G > mid then
      begin
        G := G + Enchantment;
        if (G > 255) then G:=255;
      end else
      begin
        G := G - Enchantment;
        if (G < 0) then G:=0;
      end;

      if B > mid then
      begin
        B := B + Enchantment;
        if (B > 255) then B:=255;
      end else
      begin
        B := B - Enchantment;
        if (B < 0) then B:=0;
      end;

      Result.Data[Y * Result.Width + X].R := R;
      Result.Data[Y * Result.Width + X].G := G;
      Result.Data[Y * Result.Width + X].B := B;
    end;
end;

function SimbaImage_BlurBox(Image: TSimbaImage; Radius: Integer): TSimbaImage;
var
  X, Y, XX, YY, W, H: Integer;
  Size: Integer;
  B: TBox;
  Sum: record
    R,G,B: UInt64;
  end;
  UseIntergal: Boolean;
  IntegralImage: TSimbaIntegralImageRGB;
begin
  if (Radius <= 1) or (not Odd(Radius)) then
    SimbaException('Blur: Radius(%d) must be odd (1,3,5 etc).', [Radius]);

  UseIntergal := Radius >= 9;
  Radius := Radius div 2;

  Result := TSimbaImage.Create(Image.Width, Image.Height);
  if (Result.Width = 0) or (Result.Height = 0) then
    Exit;

  W := Image.Width - 1;
  H := Image.Height - 1;

  if UseIntergal then
  begin
    IntegralImage := TSimbaIntegralImageRGB.Create(Image);

    for Y := 0 to H do
      for X := 0 to W do
      begin
        B.X1 := Max(X-Radius, 0);
        B.Y1 := Max(Y-Radius, 0);
        B.X2 := Min(X+Radius, Image.Width - 1);
        B.Y2 := Min(Y+Radius, Image.Height - 1);
        Size := ((B.X2-B.X1) + 1) * ((B.Y2-B.Y1) + 1);

        IntegralImage.Query(B.X1, B.Y1, B.X2, B.Y2, Sum.R, Sum.G, Sum.B);
        with Result.Data[Y * Image.Width + X] do
        begin
          R := Sum.R div Size;
          G := Sum.G div Size;
          B := Sum.B div Size;
        end;
      end;
  end else
  begin
    for Y := 0 to H do
      for X := 0 to W do
      begin
        B.X1 := Max(X-Radius, 0);
        B.Y1 := Max(Y-Radius, 0);
        B.X2 := Min(X+Radius, Image.Width - 1);
        B.Y2 := Min(Y+Radius, Image.Height - 1);
        Size := ((B.X2-B.X1) + 1) * ((B.Y2-B.Y1) + 1);

        Sum.R := 0;
        Sum.G := 0;
        Sum.B := 0;

        for YY := B.Y1 to B.Y2 do
          for XX := B.X1 to B.X2 do
            with Image.Data[YY * Image.Width + XX] do
            begin
              Sum.R += R;
              Sum.G += G;
              Sum.B += B;
            end;

        with Result.Data[Y * Image.Width + X] do
        begin
          R := Sum.R div Size;
          G := Sum.G div Size;
          B := Sum.B div Size;
        end;
      end;
  end;
end;

// https://blog.ivank.net/fastest-gaussian-blur.html
procedure GaussBlurApprox(var Src, Dst: TByteArray; w, h, r: Integer);
var
  Hi: Integer;

  function OutOfRange(const Index: Integer): Boolean; inline;
  begin
    Result := (Index < 0) or (Index > Hi);
  end;

  procedure DoPass(var scl, tcl: TByteArray; w, h, r: Integer);

    procedure Horz(var scl, tcl: TByteArray; w, h, r: Integer);
    var
      i, j, ti, li, ri, fv, lv, val: Integer;
      iarr: Double;
    begin
      hi := High(scl);
      iarr := 1 / (r+r+1);

      for i := 0 to h - 1 do
      begin
        ti := i * w;
        li := ti;
        ri := ti + r;
        fv := scl[li];
        lv := scl[ti + (w - 1)];
        val := (r + 1) * fv;

        for j := 0 to r - 1 do
        begin
          if OutOfRange(ti + j) then
            Continue;

          val := val + scl[ti + j];
        end;

        for j := 0 to r do
        begin
          if OutOfRange(ri) or OutOfRange(ti) then
            Continue;

          val := val + scl[ri] - fv;
          tcl[ti] := Round(val * iarr);
          ri := ri + 1;
          ti := ti + 1;
        end;

        for j := r + 1 to w - r - 1 do
        begin
          if OutOfRange(ri) or OutOfRange(li) then
            Continue;

          val := val + scl[ri] - scl[li];
          tcl[ti] := Round(val * iarr);
          li := li + 1;
          ri := ri + 1;
          ti := ti + 1;
        end;

        for j := w - r to w - 1 do
        begin
          if OutOfRange(li) or OutOfRange(ti) then
            Continue;

          val := val + lv - scl[li];
          tcl[ti] := Round(val * iarr);
          li := li + 1;
          ti := ti + 1;
        end;
      end;
    end;

    procedure Vert(var scl, tcl: TByteArray; w, h, r: Integer);
    var
      i, j, ti, li, ri, fv, lv, val: Integer;
      iarr: Double;
    begin
      hi := High(scl);
      iarr := 1 / (r+r+1);

      for i := 0 to w - 1 do
      begin
        ti := i;
        li := ti;
        ri := ti + r * w;
        fv := scl[ti];
        lv := scl[ti + w * (h - 1)];
        val := (r + 1) * fv;

        for j := 0 to r - 1 do
        begin
          if OutOfRange(ti + j * w) then
            Continue;
          val := val + scl[ti + j * w];
        end;

        for j := 0 to r do
        begin
          if OutOfRange(ri) or OutOfRange(ti) then
            Continue;

          val := val + scl[ri] - fv;
          tcl[ti] := Round(val * iarr);
          ri := ri + w;
          ti := ti + w;
        end;

        for j := r + 1 to h - r - 1 do
        begin
          if OutOfRange(ri) or OutOfRange(li) or OutOfRange(ti) then
            Continue;

          val := val + scl[ri] - scl[li];
          tcl[ti] := Round(val * iarr);
          li := li + w;
          ri := ri + w;
          ti := ti + w;
        end;

        for j := h - r to h - 1 do
        begin
          if OutOfRange(li) or OutOfRange(ti) then
            Continue;

          val := val + lv - scl[li];
          tcl[ti] := Round(val * iarr);
          li := li + w;
          ti := ti + w;
        end;
      end;
    end;

  begin
    tcl := Copy(scl);

    Horz(tcl, scl, w, h, r);
    Vert(scl, tcl, w, h, r);
  end;

  function BoxesForGauss(sigma: Double): TIntegerArray;
  const
    N = 3;
  var
    wl, wu, m, i: Integer;
  begin
    wl := Floor(Sqrt((12 * sigma * sigma / n) + 1));
    if wl mod 2 = 0 then
      wl := wl - 1;
    wu := wl + 2;

    m := Round((12 * sigma * sigma - (wl * wl * n) - 4.0 * n * wl - 3.0 * n) / (-4.0 * wl - 4));

    SetLength(Result, N);
    for i := 0 to N - 1 do
      if (i < m) then
        Result[i] := wl
      else
        Result[i] := wu;
  end;

var
  Temp: TByteArray;
  Boxes: TIntegerArray;
begin
  Temp := Copy(Src);
  Hi := High(Src);
  Boxes := BoxesForGauss(r);

  DoPass(Temp, Dst, w, h, (Boxes[0] - 1) div 2);
  DoPass(Dst, Temp, w, h, (Boxes[1] - 1) div 2);
  DoPass(Temp, Dst, w, h, (Boxes[2] - 1) div 2);
end;

function SimbaImage_BlurGauss(Image: TSimbaImage; Radius: Integer): TSimbaImage;
var
  inR, inG, inB: TByteArray;
  outR, outG, outB: TByteArray;
begin
  Result := TSimbaImage.Create(Image.Width, Image.Height);
  if (Result.Width = 0) or (Result.Height = 0) then
    Exit;

  Image.SplitChannels(inB, inG, inR);

  SetLength(outR, Length(inR));
  SetLength(outG, Length(inG));
  SetLength(outB, Length(inB));

  GaussBlurApprox(inR, outR, Image.Width, Image.Height, radius);
  GaussBlurApprox(inG, outG, Image.Width, Image.Height, radius);
  GaussBlurApprox(inB, outB, Image.Width, Image.Height, radius);

  Result.FromChannels(outB, outG, outR, Result.Width, Result.Height);
end;

// https://github.com/galfar/imaginglib/blob/master/Extensions/ImagingBinary.pas#L79
function SimbaImage_Threshold(Image: TSimbaImage; Invert: Boolean; C: Integer): TSimbaImage;
var
  Histogram: array[Byte] of Single;
  Level, Max, Min, I, J, NumPixels: Integer;
  Mean, Variance: Single;
  Mu, Omega, LevelMean, LargestMu: Single;
  Upper: PtrUInt;
  Ptr: PColorBGRA;
begin
  Result := Image.GreyScale();

  FillByte(Histogram, SizeOf(Histogram), 0);
  Min := 255;
  Max := 0;
  Level := 0;
  NumPixels := Result.Width * Result.Height;

  // Compute histogram and determine min and max pixel values
  Upper := PtrUInt(Result.Data) + Result.DataSize;
  Ptr := Result.Data;
  while (PtrUInt(Ptr) < Upper) do
  begin
    Histogram[Ptr^.R] := Histogram[Ptr^.R] + 1.0;
    if (Ptr^.R < Min) then
      Min := Ptr^.R;
    if (Ptr^.R > Max) then
      Max := Ptr^.R;
    Inc(Ptr);
  end;

  // Normalize histogram
  for I := 0 to 255 do
    Histogram[I] := Histogram[I] / NumPixels;

  // Compute image mean and variance
  Mean := 0.0;
  Variance := 0.0;
  for I := 0 to 255 do
    Mean := Mean + (I + 1) * Histogram[I];
  for I := 0 to 255 do
    Variance := Variance + Sqr(I + 1 - Mean) * Histogram[I];

  // Now finally compute threshold level
  LargestMu := 0;

  for I := 0 to 255 do
  begin
    Omega := 0.0;
    LevelMean := 0.0;

    for J := 0 to I - 1 do
    begin
      Omega := Omega + Histogram[J];
      LevelMean := LevelMean + (J + 1) * Histogram[J];
    end;

    Mu := Sqr(Mean * Omega - LevelMean);
    Omega := Omega * (1.0 - Omega);

    if Omega > 0.0 then
      Mu := Mu / Omega
    else
      Mu := 0;

    if Mu > LargestMu then
    begin
      LargestMu := Mu;
      Level := I;
    end;
  end;

  Level := Level - C;

  // Do thresholding using computed level
  Ptr := Result.Data;
  while (PtrUInt(Ptr) < Upper) do
  begin
    if (Invert and (Ptr^.R <= Level)) or ((not Invert) and (Ptr^.R >= Level)) then
      Ptr^.AsInteger := $FFFFFFFF
    else
      Ptr^.AsInteger := $FF000000;

    Inc(Ptr);
  end;
end;

function SimbaImage_ThresholdAdaptive(Image: TSimbaImage; Invert: Boolean; Radius: Integer; C: Integer): TSimbaImage;
var
  Mat: TByteMatrix;
  Integral: TSimbaIntegralImageF;
  X, Y, W, H, Left, Right, Top, Bottom, Count: Integer;
  Threshold: Double;
begin
  if (Radius <= 1) or (not Odd(Radius)) then
    SimbaException('ThresholdAdaptive: Radius(%d) must be odd and not negative (1,3,5 etc).', [Radius]);
  Radius := Radius div 2;

  Result := TSimbaImage.Create(Image.Width, Image.Height);
  if (Result.Width = 0) or (Result.Height = 0) then
    Exit;

  Mat := Image.ToGreyMatrix();
  Integral := TSimbaIntegralImageF.Create(Mat);

  W := Image.Width - 1;
  H := Image.Height - 1;
  for Y := 0 to H do
    for X := 0 to W do
    begin
      Left   := Max(X-Radius, 0);
      Right  := Min(X+Radius, W);
      Top    := Max(Y-Radius, 0);
      Bottom := Min(Y+Radius, H);

      Count := (Bottom - Top + 1) * (Right - Left + 1);
      Threshold := (Integral.Query(Left, Top, Right, Bottom) / Count) - C;

      if (Invert and (Mat[Y, X] <= Threshold)) or ((not Invert) and (Mat[Y, X] >= Threshold)) then
        Result.Data[Y * Image.Width + X].AsInteger := $FFFFFFFF;
    end;
end;

{
  Sauvola binarization computes a local threshold based on
  the local average and square average.  It takes two constants:
  the window size for the measurment at each pixel and a
  parameter that determines the amount of normalized local
  standard deviation to subtract from the local average value.

  Invert = Invert output
  Radius = Window size (default = 25)
  C      = Constant value (default = 0.2). Typical values are between 0.2 and 0.5.
}
function SimbaImage_ThresholdAdaptiveSauvola(Image: TSimbaImage; Invert: Boolean; Radius: Integer; C: Single): TSimbaImage;
var
  Mat: TByteMatrix;
  Integral: TSimbaIntegralImageF;
  X, Y, W, H: Integer;
  Left, Right, Top, Bottom: Integer;
  Count: Integer;
  Sum, SumSquares: Double;
  Mean, Stdev, Threshold: Double;
begin
  if (Radius <= 1) or (not Odd(Radius)) then
    SimbaException('ThresholdAdaptive: Radius(%d) must be odd and not negative (1,3,5 etc).', [Radius]);
  Radius := Radius div 2;

  Result := TSimbaImage.Create(Image.Width, Image.Height);
  if (Result.Width = 0) or (Result.Height = 0) then
    Exit;

  Mat := Image.ToGreyMatrix();
  Mat.GetSize(W, H);

  Dec(W);
  Dec(H);

  Integral := TSimbaIntegralImageF.Create(Mat);

  for Y := 0 to H do
    for X := 0 to W do
    begin
      Left   := Max(X-Radius, 0);
      Right  := Min(X+Radius, W);
      Top    := Max(Y-Radius, 0);
      Bottom := Min(Y+Radius, H);
      Count := (Bottom - Top + 1) * (Right - Left + 1);

      //Sum := 0;
      //SumSquares := 0;
      //
      //for y := top to bottom do
      //  for x := left to right do
      //  begin
      //    Sum += mat[y,x];
      //    SumSquares += mat[y,x]*mat[y,x];
      //  end;

      Integral.Query(Left, Top, Right, Bottom, Sum, SumSquares);
      Mean := Sum / Count;
      Stdev := Sqrt((SumSquares / Count) - Sqr(Mean));
      Threshold := Mean * (1.0 + C * ((Stdev / 128.0) - 1.0));

      if (Invert and (Mat[Y, X] <= Threshold)) or ((not Invert) and (Mat[Y, X] >= Threshold)) then
        Result.Data[Y * Image.Width + X].AsInteger := $FFFFFFFF;
    end;
end;

{
function SimbaImage_ThresholdAdaptive_Mean(Image: TImage; Radius: Integer; Invert: Boolean; C: Integer): TImage;
var
  X, Y, XX, YY: Integer;
  W, H: Integer;
  Left, Right, Top, Bottom: Integer;
  Sum: UInt64;
  Count, Thresh: Integer;
  grey: TImage;
begin
  Result := TImage.Create(Image.Width, Image.Height);
  grey := Image.GreyScale();

  W := grey.Width-1;
  H := grey.Height-1;

  for Y := 0 to H do
    for X := 0 to W do
    begin
      Left   := Max(X-Radius, 0);
      Right  := Min(X+Radius, W);
      Top    := Max(Y-Radius, 0);
      Bottom := Min(Y+Radius, H);
      Count := (Bottom - Top + 1) * (Right - Left + 1);

      Sum := 0;
      for YY := Top to Bottom do
        for XX := Left to Right do
          Sum += grey.Pixel[XX, YY].R;
      Thresh := (Sum div Count) - C;

      if (grey.Pixel[X, Y].R >= thresh) then
        Result.Pixel[X, Y] := $FFFFFF;
    end;
end;
}

end.

