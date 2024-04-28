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
function SimbaImage_ThresholdAdaptive(Image: TSimbaImage; Inv: Boolean; Method: EImageThreshMethod; K: Integer): TSimbaImage;
function SimbaImage_ThresholdSauvola(Image: TSimbaImage; Radius: Integer; Inv: Boolean = False; R: Single = 128; K: Single = 0.5): TSimbaImage;
function SimbaImage_Sobel(Image: TSimbaImage): TSimbaImage;
function SimbaImage_Enhance(Image: TSimbaImage; Enchantment: Byte; C: Single): TSimbaImage;
function SimbaImage_BlurBox(Image: TSimbaImage; Radius: Single): TSimbaImage;
function SimbaImage_BlurGauss(Image: TSimbaImage; Radius: Single): TSimbaImage;

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

function SimbaImage_ThresholdAdaptive(Image: TSimbaImage; Inv: Boolean; Method: EImageThreshMethod; K: Integer): TSimbaImage;
var
  I: Integer;
  vMin, vMax, Threshold: UInt8;
  Counter: Int64;
  Tab: array[0..256] of TColorBGRA;
  Upper: PtrUInt;
  SrcPtr, DestPtr: PColorBGRA;
  HitColor, MissColor: TColorBGRA;
begin
  Result := TSimbaImage.Create(Image.Width, Image.Height);

  //Finding the threshold - While at it convert image to grayscale.
  Threshold := 0;
  case Method of
    //Find the Arithmetic Mean / Average.
    EImageThreshMethod.MEAN:
      begin
        SrcPtr := Image.Data;
        DestPtr := Result.Data;
        Upper := PtrUInt(@Image.Data[Image.Width * Image.Height]);

        Counter := 0;

        while (PtrUInt(SrcPtr) < Upper) do
        begin
          DestPtr^.R := (SrcPtr^.B + SrcPtr^.G + SrcPtr^.R) div 3;

          Counter += DestPtr^.R;

          Inc(SrcPtr);
          Inc(DestPtr);
        end;

        Threshold := (Counter div ((Image.Width * Image.Height) - 1)) + K;
      end;

    //Middle of Min- and Max-value
    EImageThreshMethod.MIN_MAX:
      begin
        vMin := 255;
        vMax := 0;

        SrcPtr := Image.Data;
        DestPtr := Result.Data;
        Upper := PtrUInt(@Image.Data[Image.Width * Image.Height]);

        while (PtrUInt(SrcPtr) < Upper) do
        begin
          DestPtr^.R := (SrcPtr^.B + SrcPtr^.G + SrcPtr^.R) div 3;

          if (DestPtr^.R < vMin) then vMin := DestPtr^.R else
          if (DestPtr^.R > vMax) then vMax := DestPtr^.R;

          Inc(SrcPtr);
          Inc(DestPtr)
        end;

        Threshold := ((vMax + Integer(vMin)) shr 1) + K;
      end;
  end;

  HitColor.AsInteger := 0;
  HitColor.A := ALPHA_OPAQUE;
  MissColor.AsInteger := $FFFFFF;
  MissColor.A := ALPHA_OPAQUE;

  if Inv then Swap(MissColor, HitColor);
  for I := 0 to Threshold - 1 do Tab[I] := MissColor;
  for I := Threshold to 255   do Tab[I] := HitColor;

  Upper := PtrUInt(@Result.Data[Result.Width * Result.Height]);
  DestPtr := Result.Data;
  while (PtrUInt(DestPtr) < Upper) do
  begin
    DestPtr^ := Tab[DestPtr^.R];

    Inc(DestPtr);
  end;
end;

{
  Radius = Window size
  Invert = Invert output
  R      = dynamic range of standard deviation (default = 128)
  K      = constant value in range 0.2..0.5 (default = 0.5)
}
function SimbaImage_ThresholdSauvola(Image: TSimbaImage; Radius: Integer; Inv: Boolean; R: Single; K: Single): TSimbaImage;
var
  Mat: TByteMatrix;
  Integral: TSimbaIntegralImageF;
  X,Y,W,H: Integer;
  Left, Right, Top, Bottom: Integer;
  Count: Integer;
  Sum, SumSquares: Double;
  Mean, Stdev, Threshold: Double;
  HitColor, MissColor: TColorBGRA;
begin
  Result := TSimbaImage.Create(Image.Width, Image.Height);

  Mat := Image.ToGreyMatrix();
  Mat.GetSize(W, H);

  Dec(W);
  Dec(H);

  Radius := (Radius - 1) div 2;
  Integral := TSimbaIntegralImageF.Create(Mat);

  HitColor.AsInteger := $FFFFFF;
  HitColor.A := ALPHA_OPAQUE;
  MissColor.AsInteger := 0;
  MissColor.A := ALPHA_OPAQUE;
  if Inv then
    Swap(HitColor, MissColor);

  for Y := 0 to H do
    for X := 0 to W do
    begin
      Left   := Max(X-Radius, 0);
      Right  := Min(X+Radius, W);
      Top    := Max(Y-Radius, 0);
      Bottom := Min(Y+Radius, H);

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

      Count := (Bottom - Top + 1) * (Right - Left + 1);
      Mean := Sum / Count;
      Stdev := Sqrt((SumSquares / Count) - Sqr(Mean));
      Threshold := Mean * (1.0 + K * ((Stdev / R) - 1.0));

      if Mat[Y, X] < Threshold then
        Result.Data[Y * Image.Width + X] := MissColor
      else
        Result.Data[Y * Image.Width + X] := HitColor;
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

function SimbaImage_BlurBox(Image: TSimbaImage; Radius: Single): TSimbaImage;
var
  X, Y, XX, YY, W, H: Integer;
  Size: Integer;
  B: TBox;
  Sum: record
    R,G,B: UInt64;
  end;
  Rad: Integer;
  UseIntergal: Boolean;
  IntegralImage: TSimbaIntegralImageRGB;
begin
  Result := TSimbaImage.Create(Image.Width, Image.Height);
  if (Result.Width = 0) or (Result.Height = 0) then
    Exit;

  //if (Radius <= 1) or (not Odd(Radius)) then
  //  SimbaException('TSimbaImage.BlockBlur: Radius(%d) must be odd (1,3,5 etc).', [Radius]);

  UseIntergal := Radius >= 9;

  Rad := Round(Radius);
  if Odd(Rad) then
    Rad := Rad div 2
  else
    Rad := (Rad + 1) div 2;

  W := Image.Width - 1;
  H := Image.Height - 1;

  if UseIntergal then
  begin
    IntegralImage := TSimbaIntegralImageRGB.Create(Image);

    for Y := 0 to H do
      for X := 0 to W do
      begin
        B.X1 := Max(X-Rad, 0);
        B.Y1 := Max(Y-Rad, 0);
        B.X2 := Min(X+Rad, Image.Width) - 1;
        B.Y2 := Min(Y+Rad, Image.Height) - 1;
        Size := ((B.X2-B.X1) + 1) * ((B.Y2-B.Y1) + 1);

        IntegralImage.Query(B.X1, B.Y1, B.X2, B.Y2, Sum.R, Sum.G, Sum.B);
        with Result.Data[Y * Image.Width + X] do
        begin
          R := Sum.R div Size;
          G := Sum.G div Size;
          B := Sum.B div Size;
          A := ALPHA_OPAQUE;
        end;
      end;
  end else
  begin
    for Y := 0 to H do
      for X := 0 to W do
      begin
        B.X1 := Max(X-Rad, 0);
        B.Y1 := Max(Y-Rad, 0);
        B.X2 := Min(X+Rad, Image.Width) - 1;
        B.Y2 := Min(Y+Rad, Image.Height) - 1;
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
          A := ALPHA_OPAQUE;
        end;
      end;
  end;
end;

// https://blog.ivank.net/fastest-gaussian-blur.html
function SimbaImage_BlurGauss(Image: TSimbaImage; Radius: Single): TSimbaImage;

  function boxesForGauss(sigma: Double; n: Integer): TIntegerArray;
  var
    wIdeal: Double;
    wl, wu: Integer;
    mIdeal, m: Double;
    i: Integer;
  begin
    wIdeal := Sqrt((12 * sigma * sigma / n) + 1); // Ideal averaging filter width
    wl := Floor(wIdeal);
    if (wl mod 2 = 0) then
      wl := wl - 1;
    wu := wl + 2;

    mIdeal := (12 * sigma * sigma - n * wl * wl - 4 * n * wl - 3 * n) / (-4 * wl - 4);
    m := Round(mIdeal);

    SetLength(Result, n);
    for i := 0 to n - 1 do
    begin
      if i < m then
        Result[i] := wl
      else
        Result[i] := wu;
    end;
  end;

  procedure boxBlurH_4(var scl, tcl: TByteArray; w, h: Integer; r: Integer);
  var
    iarr: Double;
    i,j: Integer;
    ti, li, ri, fv, lv: Integer;
    val: Double;
  begin
    iarr := 1.0 / (r + r + 1);

    for i := 0 to h-1 do
    begin
      ti := i * w;
      li := ti;
      ri := Round(ti + r);
      fv := scl[ti];
      lv := scl[ti + w - 1];
      val := (r + 1) * fv;

      for j := 0 to r-1 do
        val += scl[ti + j];

      for j := 0 to r do
      begin
        val += scl[ri] - fv;
        inc(ri);
        tcl[ti] := Round(val * iarr);
        Inc(ti);
      end;

      for j := r + 1 to (w - r) - 1  do
      begin
        val += scl[ri] - scl[li];
        inc(ri);
        inc(li);
        tcl[ti] := Round(val * iarr);
        inc(ti);
      end;

      for j := w - r to w-1 do
      begin
        val += lv - scl[li];
        inc(li);
        tcl[ti] := Round(val * iarr);
        inc(ti);
      end;
    end;
  end;

  procedure boxBlurT_4(var scl, tcl: TByteArray;  w, h: Integer; r: Integer);
  var
    iarr: Double;
    i,j: Integer;
    ti, li, ri, fv, lv: Integer;
    val: Double;
  begin
    iarr := 1.0 / (r + r + 1);

    for i := 0 to w-1 do
    begin
      ti := i;
      li := ti;
      ri := Round(ti + r * w);
      fv := scl[ti];
      lv := scl[ti + w * (h - 1)];
      val := (r + 1) * fv;

      for j := 0 to r-1 do
        val += scl[ti + j * w];

      for j := 0 to r do
      begin
        val += scl[ri] - fv;
        tcl[ti] := Round(val * iarr);
        ri += w;
        ti += w;
      end;

      for j := r + 1 to (h-r)-1 do
      begin
        val += scl[ri] - scl[li];
        tcl[ti] := Round(val * iarr);
        li += w;
        ri += w;
        ti += w;
      end;

      for j := h - r to h-1 do
      begin
        val += lv - scl[li];
        tcl[ti] := Round(val * iarr);
        li += w;
        ti += w;
      end;
    end;
  end;

  procedure boxBlur_4(var scl, tcl: TByteArray; w,h: Integer; r: Integer);
  begin
    Move(scl[0], tcl[0], Length(scl) * SizeOf(Byte));

    boxBlurH_4(tcl, scl, w, h, r);
    boxBlurT_4(scl, tcl, w, h, r);
  end;

  procedure gaussBlur_4(var scl, tcl: TByteArray; w,h: Integer; r: Double; bxs: TIntegerArray);
  begin
    boxBlur_4(scl, tcl, w, h, (bxs[0] - 1) div 2);
    boxBlur_4(tcl, scl, w, h, (bxs[1] - 1) div 2);
    boxBlur_4(scl, tcl, w, h, (bxs[2] - 1) div 2);
  end;

var
  inR, inG, inB: TByteArray;
  outR, outG, outB: TByteArray;
  boxes: TIntegerArray;
  Ptr: PColorBGRA;
  PtrR, PtrG, PtrB: PByte;
  Upper: PtrUInt;
begin
  Result := TSimbaImage.Create(Image.Width, Image.Height);
  if (Image.Width = 0) or (Image.Height = 0) then
    Exit;

  Image.SplitChannels(inB, inG, inR);

  SetLength(outR, Length(inR));
  SetLength(outG, Length(inG));
  SetLength(outB, Length(inB));

  boxes := boxesForGauss(radius, 3);

  gaussBlur_4(inR, outR, Image.Width, Image.Height, radius, boxes);
  gaussBlur_4(inG, outG, Image.Width, Image.Height, radius, boxes);
  gaussBlur_4(inB, outB, Image.Width, Image.Height, radius, boxes);

  Ptr := Result.Data;
  PtrB := @OutB[0];
  PtrG := @OutG[0];
  PtrR := @OutR[0];

  Upper := PtrUInt(Result.Data) + Result.DataSize;
  while (PtrUInt(Ptr) < Upper) do
  begin
    Ptr^.A := ALPHA_OPAQUE;
    Ptr^.B := PtrB^;
    Ptr^.G := PtrG^;
    Ptr^.R := PtrR^;

    Inc(Ptr);
    Inc(PtrB);
    Inc(PtrG);
    Inc(PtrR);
  end;
end;

end.

