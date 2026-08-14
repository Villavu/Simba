{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.image_filters;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.image, simba.colormath;

function SimbaImage_GreyScale(Image: TSimbaImage): TSimbaImage;
function SimbaImage_Brightness(Image: TSimbaImage; Value: Integer): TSimbaImage;
function SimbaImage_Invert(Image: TSimbaImage): TSimbaImage;
function SimbaImage_Posterize(Image: TSimbaImage; Value: Integer): TSimbaImage;
function SimbaImage_Sobel(Image: TSimbaImage): TSimbaImage;
function SimbaImage_Enhance(Image: TSimbaImage; Enchantment: Byte; C: Single): TSimbaImage;
function SimbaImage_BlurBox(Image: TSimbaImage; Radius: Single): TSimbaImage;
function SimbaImage_BlurGauss(Image: TSimbaImage; Radius: Single): TSimbaImage;
function SimbaImage_Threshold(Image: TSimbaImage; Invert: Boolean; C: Integer): TSimbaImage;
function SimbaImage_ThresholdAdaptive(Image: TSimbaImage; Invert: Boolean; Radius: Integer; C: Integer): TSimbaImage;
function SimbaImage_ThresholdAdaptiveSauvola(Image: TSimbaImage; Invert: Boolean; Radius: Integer; C: Single): TSimbaImage;

procedure SimbaImage_ReplaceColor(Image: TSimbaImage; OldColor, NewColor: TColor; Tol: Single = 0);
procedure SimbaImage_ReplaceColorBinary(Image: TSimbaImage; Invert: Boolean; Color: TColor; Tol: Single = 0);
procedure SimbaImage_ReplaceColorBinary(Image: TSimbaImage; Invert: Boolean; Colors: TColorArray; Tol: Single = 0);

implementation

uses
  Math,
  simba.image_utils, simba.vartype_matrix, simba.colormath_conversion, simba.colormath_distance;

function SimbaImage_GreyScale(Image: TSimbaImage): TSimbaImage;
var
  I: Integer;
  Src, Dst: PColorBGRA;
  Lum: Byte;
begin
  Result := TSimbaImage.Create(Image.Width, Image.Height);

  Src := Image.Data;
  Dst := Result.Data;

  for I := (Image.Height * Image.Width - 1) downto 0 do
  begin
    Lum := Round(Src^.R * 0.299 + Src^.G * 0.587 + Src^.B * 0.114);

    Dst^.R := Lum;
    Dst^.G := Lum;
    Dst^.B := Lum;
    Dst^.A := ALPHA_OPAQUE;

    Inc(Src);
    Inc(Dst);
  end;
end;

function SimbaImage_Brightness(Image: TSimbaImage; Value: Integer): TSimbaImage;
var
  I: Integer;
  Src, Dst: PColorBGRA;
begin
  Result := TSimbaImage.Create(Image.Width, Image.Height);

  Src := Image.Data;
  Dst := Result.Data;

  for I := (Image.Height * Image.Width - 1) downto 0 do
  begin
    Dst^.R := EnsureRange(Src^.R + Value, 0, 255);
    Dst^.G := EnsureRange(Src^.G + Value, 0, 255);
    Dst^.B := EnsureRange(Src^.B + Value, 0, 255);
    Dst^.A := ALPHA_OPAQUE;

    Inc(Src);
    Inc(Dst);
  end;
end;

function SimbaImage_Invert(Image: TSimbaImage): TSimbaImage;
var
  I: Integer;
  Src, Dst: PColorBGRA;
begin
  Result := TSimbaImage.Create(Image.Width, Image.Height);

  Src := Image.Data;
  Dst := Result.Data;

  for I := (Image.Height * Image.Width - 1) downto 0 do
  begin
    Dst^.R := not Src^.R;
    Dst^.G := not Src^.G;
    Dst^.B := not Src^.B;
    Dst^.A := ALPHA_OPAQUE;

    Inc(Src);
    Inc(Dst);
  end;
end;

function SimbaImage_Posterize(Image: TSimbaImage; Value: Integer): TSimbaImage;
var
  I: Integer;
  Src, Dst: PColorBGRA;
begin
  if not InRange(Value, 1, 255) then
    SimbaException('TSimbaImage.Posterize: Value(%d) out of range[1..255]', [Value]);

  Result := TSimbaImage.Create(Image.Width, Image.Height);

  Src := Image.Data;
  Dst := Result.Data;

  for I := (Image.Height * Image.Width - 1) downto 0 do
  begin
    Dst^.A := ALPHA_OPAQUE;
    Dst^.R := Min(Round(Src^.R / Value) * Value, 255);
    Dst^.G := Min(Round(Src^.G / Value) * Value, 255);
    Dst^.B := Min(Round(Src^.B / Value) * Value, 255);

    Inc(Src);
    Inc(Dst);
  end;
end;

function SimbaImage_Sobel(Image: TSimbaImage): TSimbaImage;
var
  x,y,xx,yy,W,H,gx,gy,SrcWidth: Integer;
  opx,opy: TIntegerMatrix;
  Grey: TByteMatrix;
  Ptr, DstPtr: PColorBGRA;
begin
  Grey := Image.ToGreyMatrix;
  Result := TSimbaImage.Create(Image.Width, Image.Height);
  DstPtr := Result.Data;
  SrcWidth := Image.Width;

  SetLength(opx, 3,3);
  opx[0][0] := -1; opx[0][1] := 0; opx[0][2] := 1;
  opx[1][0] := -2; opx[1][1] := 0; opx[1][2] := 2;
  opx[2][0] := -1; opx[2][1] := 0; opx[2][2] := 1;

  SetLength(opy, 3,3);
  opy[0][0] := -1; opy[0][1] := -2; opy[0][2] := -1;
  opy[1][0] :=  0; opy[1][1] :=  0; opy[1][2] := 0;
  opy[2][0] :=  1; opy[2][1] :=  2; opy[2][2] := 1;

  W := Image.Width - 2;
  H := Image.Height - 2;
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

      Ptr := @DstPtr[Y * SrcWidth + X];
      Ptr^.B := Byte(EnsureRange(Trunc(Sqrt(gx*gx + gy*gy)), 0, 255));
      Ptr^.G := Ptr^.B;
      Ptr^.R := Ptr^.B;
    end;
end;

function SimbaImage_Enhance(Image: TSimbaImage; Enchantment: Byte; C: Single): TSimbaImage;
var
  W,H,x,y,R,G,B,SrcWidth,Idx:Integer;
  mid: Single;
  SrcPtr, DstPtr: PColorBGRA;
begin
  Result := TSimbaImage.Create(Image.Width, Image.Height);
  SrcPtr := Image.Data;
  DstPtr := Result.Data;
  SrcWidth := Image.Width;
  Mid := 127 * C;
  W := Image.Width - 1;
  H := Image.Height - 1;
  for y:=0 to H do
    for x:=0 to W do
    begin
      Idx := Y * SrcWidth + X;
      R := SrcPtr[Idx].R;
      G := SrcPtr[Idx].G;
      B := SrcPtr[Idx].B;

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

      DstPtr[Idx].R := R;
      DstPtr[Idx].G := G;
      DstPtr[Idx].B := B;
    end;
end;

// Box blur Pillow style (BoxBlur.c)
function SimbaImage_BlurBox(Image: TSimbaImage; Radius: Single): TSimbaImage;

  procedure LineBoxBlur(OutP, InP: PByte; LastX, Rad, EdgeA, EdgeB: Integer; ww, fw: UInt32);
  var
    X: Integer;
    a0,a1,a2,a3, i0,i1,i2,i3, l0,l1,l2,l3, s0,s1,s2,s3: UInt32;
    pAdd, pSub, pFar, pOut, pLast: PByte;
  begin
    i0 := InP[0];
    i1 := InP[1];
    i2 := InP[2];
    i3 := InP[3];

    pLast := InP + LastX * 4;
    l0 := pLast[0];
    l1 := pLast[1];
    l2 := pLast[2];
    l3 := pLast[3];

    a0 := i0 * UInt32(Rad + 1);
    a1 := i1 * UInt32(Rad + 1);
    a2 := i2 * UInt32(Rad + 1);
    a3 := i3 * UInt32(Rad + 1);

    pAdd := InP;
    for X := 0 to EdgeA - 2 do
    begin
      a0 += pAdd[0];
      a1 += pAdd[1];
      a2 += pAdd[2];
      a3 += pAdd[3];
      Inc(pAdd, 4);
    end;

    a0 += l0 * UInt32(Rad - EdgeA + 1);
    a1 += l1 * UInt32(Rad - EdgeA + 1);
    a2 += l2 * UInt32(Rad - EdgeA + 1);
    a3 += l3 * UInt32(Rad - EdgeA + 1);

    if (EdgeA <= EdgeB) then
    begin
      pAdd := InP + Rad * 4;
      pFar := InP + (Rad + 1) * 4;
      pOut := OutP;

      for X := 0 to EdgeA - 1 do
      begin
        a0 := a0 + pAdd[0] - i0;
        a1 := a1 + pAdd[1] - i1;
        a2 := a2 + pAdd[2] - i2;
        a3 := a3 + pAdd[3] - i3;
        pOut[0] := UInt32(a0 * ww + (i0 + pFar[0]) * fw + (1 shl 23)) shr 24;
        pOut[1] := UInt32(a1 * ww + (i1 + pFar[1]) * fw + (1 shl 23)) shr 24;
        pOut[2] := UInt32(a2 * ww + (i2 + pFar[2]) * fw + (1 shl 23)) shr 24;
        pOut[3] := UInt32(a3 * ww + (i3 + pFar[3]) * fw + (1 shl 23)) shr 24;
        Inc(pAdd, 4);
        Inc(pFar, 4);
        Inc(pOut, 4);
      end;

      pAdd := InP + (EdgeA + Rad) * 4;
      pSub := InP + (EdgeA - Rad - 1) * 4;
      pFar := InP + (EdgeA + Rad + 1) * 4;
      pOut := OutP + EdgeA * 4;

      for X := EdgeA to EdgeB - 1 do
      begin
        s0 := pSub[0];
        s1 := pSub[1];
        s2 := pSub[2];
        s3 := pSub[3];
        a0 := a0 + pAdd[0] - s0;
        a1 := a1 + pAdd[1] - s1;
        a2 := a2 + pAdd[2] - s2;
        a3 := a3 + pAdd[3] - s3;
        pOut[0] := UInt32(a0 * ww + (s0 + pFar[0]) * fw + (1 shl 23)) shr 24;
        pOut[1] := UInt32(a1 * ww + (s1 + pFar[1]) * fw + (1 shl 23)) shr 24;
        pOut[2] := UInt32(a2 * ww + (s2 + pFar[2]) * fw + (1 shl 23)) shr 24;
        pOut[3] := UInt32(a3 * ww + (s3 + pFar[3]) * fw + (1 shl 23)) shr 24;
        Inc(pAdd, 4);
        Inc(pSub, 4);
        Inc(pFar, 4);
        Inc(pOut, 4);
      end;

      pSub := InP + (EdgeB - Rad - 1) * 4;
      pOut := OutP + EdgeB * 4;
      for X := EdgeB to LastX do
      begin
        s0 := pSub[0];
        s1 := pSub[1];
        s2 := pSub[2];
        s3 := pSub[3];
        a0 := a0 + l0 - s0;
        a1 := a1 + l1 - s1;
        a2 := a2 + l2 - s2;
        a3 := a3 + l3 - s3;
        pOut[0] := UInt32(a0 * ww + (s0 + l0) * fw + (1 shl 23)) shr 24;
        pOut[1] := UInt32(a1 * ww + (s1 + l1) * fw + (1 shl 23)) shr 24;
        pOut[2] := UInt32(a2 * ww + (s2 + l2) * fw + (1 shl 23)) shr 24;
        pOut[3] := UInt32(a3 * ww + (s3 + l3) * fw + (1 shl 23)) shr 24;
        Inc(pSub, 4);
        Inc(pOut, 4);
      end;
    end else
    begin
      pAdd := InP + Rad * 4;
      pFar := InP + (Rad + 1) * 4;
      pOut := OutP;
      for X := 0 to EdgeB - 1 do
      begin
        a0 := a0 + pAdd[0] - i0;
        a1 := a1 + pAdd[1] - i1;
        a2 := a2 + pAdd[2] - i2;
        a3 := a3 + pAdd[3] - i3;
        pOut[0] := UInt32(a0 * ww + (i0 + pFar[0]) * fw + (1 shl 23)) shr 24;
        pOut[1] := UInt32(a1 * ww + (i1 + pFar[1]) * fw + (1 shl 23)) shr 24;
        pOut[2] := UInt32(a2 * ww + (i2 + pFar[2]) * fw + (1 shl 23)) shr 24;
        pOut[3] := UInt32(a3 * ww + (i3 + pFar[3]) * fw + (1 shl 23)) shr 24;
        Inc(pAdd, 4);
        Inc(pFar, 4);
        Inc(pOut, 4);
      end;

      pOut := OutP + EdgeB * 4;
      for X := EdgeB to EdgeA - 1 do
      begin
        a0 := a0 + l0 - i0;
        a1 := a1 + l1 - i1;
        a2 := a2 + l2 - i2;
        a3 := a3 + l3 - i3;
        pOut[0] := UInt32(a0 * ww + (i0 + l0) * fw + (1 shl 23)) shr 24;
        pOut[1] := UInt32(a1 * ww + (i1 + l1) * fw + (1 shl 23)) shr 24;
        pOut[2] := UInt32(a2 * ww + (i2 + l2) * fw + (1 shl 23)) shr 24;
        pOut[3] := UInt32(a3 * ww + (i3 + l3) * fw + (1 shl 23)) shr 24;
        Inc(pOut, 4);
      end;

      pSub := InP + (EdgeA - Rad - 1) * 4;
      pOut := OutP + EdgeA * 4;
      for X := EdgeA to LastX do
      begin
        s0 := pSub[0];
        s1 := pSub[1];
        s2 := pSub[2];
        s3 := pSub[3];
        a0 := a0 + l0 - s0;
        a1 := a1 + l1 - s1;
        a2 := a2 + l2 - s2;
        a3 := a3 + l3 - s3;
        pOut[0] := UInt32(a0 * ww + (s0 + l0) * fw + (1 shl 23)) shr 24;
        pOut[1] := UInt32(a1 * ww + (s1 + l1) * fw + (1 shl 23)) shr 24;
        pOut[2] := UInt32(a2 * ww + (s2 + l2) * fw + (1 shl 23)) shr 24;
        pOut[3] := UInt32(a3 * ww + (s3 + l3) * fw + (1 shl 23)) shr 24;
        Inc(pSub, 4);
        Inc(pOut, 4);
      end;
    end;
  end;

  procedure HorizBoxBlur(Src, Dst: PColorBGRA; Width, Height, Rad: Integer; ww, fw: UInt32);
  var
    Y, EdgeA, EdgeB: Integer;
    InRow, OutRow: PColorBGRA;
  begin
    EdgeA := Min(Rad + 1, Width);
    EdgeB := Max(Width - Rad - 1, 0);
    for Y := 0 to Height - 1 do
    begin
      InRow := Src + Y * Width;
      OutRow := Dst + Y * Width;
      LineBoxBlur(PByte(OutRow), PByte(InRow), Width - 1, Rad, EdgeA, EdgeB, ww, fw);
    end;
  end;

  // Cache-blocked (tiled) transpose, after TransposeComplexBlocked in simba.fftpack4.
  procedure Transpose(Src, Dst: PColorBGRA; Width, Height: Integer);
  const
    B = 8;
  var
    X, Y, XEnd, YEnd: Integer;
    SrcRow, DstCol, Cur, CurDest, SrcRowEnd, CurEnd: PColorBGRA;
  begin
    Y := 0;
    while (Y < Height) do
    begin
      YEnd := Y + B;
      if (YEnd > Height) then
        YEnd := Height;

      X := 0;
      while (X < Width) do
      begin
        XEnd := X + B;
        if (XEnd > Width) then
          XEnd := Width;

        SrcRow := @Src[Y * Width + X];
        SrcRowEnd := @Src[YEnd * Width + X];
        DstCol := @Dst[X * Height + Y];
        while (PtrUInt(SrcRow) < PtrUInt(SrcRowEnd)) do
        begin
          Cur := SrcRow;
          CurDest := DstCol;
          CurEnd := @SrcRow[XEnd - X];
          while (PtrUInt(Cur) < PtrUInt(CurEnd)) do
          begin
            CurDest^ := Cur^;
            Inc(Cur);
            Inc(CurDest, Height);
          end;
          Inc(SrcRow, Width);
          Inc(DstCol);
        end;

        X := X + B;
      end;

      Y := Y + B;
    end;
  end;

var
  W, H, Rad: Integer;
  ww, fw: UInt32;
  Scratch: array of TColorBGRA;
begin
  if (Radius < 0) then
    SimbaException('Blur radius must be >= 0');

  Result := TSimbaImage.Create(Image.Width, Image.Height);
  if (Result.Width = 0) or (Result.Height = 0) then
    Exit;

  W := Image.Width;
  H := Image.Height;
  Rad := Trunc(Radius);                         // integer part = window half-width
  ww := Trunc((1 shl 24) / (2 * Radius + 1));   // float radius: fractional part handled by fw
  fw := ((1 shl 24) - (2 * Rad + 1) * ww) div 2;

  SetLength(Scratch, W * H);
  HorizBoxBlur(Image.Data,   @Scratch[0], W, H, Rad, ww, fw); // horizontal
  Transpose(@Scratch[0],     Result.Data, W, H);              // -> H x W
  HorizBoxBlur(Result.Data,  @Scratch[0], H, W, Rad, ww, fw); // horizontal on transposed (= vertical)
  Transpose(@Scratch[0],     Result.Data, H, W);              // -> W x H
end;

// 3-box approximation of a Gaussian blur (Ivan Kutski @ https://blog.ivank.net/fastest-gaussian-blur.html)
procedure GaussBlurApprox(var Src, Dst: TByteArray; Width, Height: Integer; Radius: Single);

  procedure BlurRows(const Source: TByteArray; var Target: TByteArray; Width, Height, Radius: Integer);
  var
    Row, k, FirstVal, LastVal, Sum: Integer;
    Recip: Integer;
    RowStart, SubPtr, AddPtr, OutPtr: PByte;
  begin
    if (Radius > (Width - 1) div 2) then
      Radius := (Width - 1) div 2;
    Recip := ((1 shl 22) + Radius) div (2 * Radius + 1); // Q22 reciprocal of the window size

    for Row := 0 to Height - 1 do
    begin
      RowStart := @Source[Row * Width];
      OutPtr   := @Target[Row * Width];
      FirstVal := RowStart^;
      LastVal  := (RowStart + Width - 1)^;
      SubPtr   := RowStart;
      AddPtr   := RowStart + Radius;

      Sum := (Radius + 1) * FirstVal;
      for k := 0 to Radius - 1 do
        Sum := Sum + (RowStart + k)^;

      for k := 0 to Radius do                         // left edge: window hangs off the start
      begin
        Sum := Sum + AddPtr^ - FirstVal;
        OutPtr^ := (Sum * Recip + (1 shl 21)) shr 22;
        Inc(AddPtr); Inc(OutPtr);
      end;
      for k := Radius + 1 to Width - Radius - 1 do     // window fully inside the row
      begin
        Sum := Sum + AddPtr^ - SubPtr^;
        OutPtr^ := (Sum * Recip + (1 shl 21)) shr 22;
        Inc(SubPtr); Inc(AddPtr); Inc(OutPtr);
      end;
      for k := Width - Radius to Width - 1 do          // right edge: window hangs off the end
      begin
        Sum := Sum + LastVal - SubPtr^;
        OutPtr^ := (Sum * Recip + (1 shl 21)) shr 22;
        Inc(SubPtr); Inc(OutPtr);
      end;
    end;
  end;

  procedure BlurCols(const Source: TByteArray; var Target: TByteArray; Width, Height, Radius: Integer);
  var
    Col, k, FirstVal, LastVal, Sum: Integer;
    Recip: Integer;
    ColStart, SubPtr, AddPtr, OutPtr: PByte;
  begin
    if (Radius > (Height - 1) div 2) then
      Radius := (Height - 1) div 2;
    Recip := ((1 shl 22) + Radius) div (2 * Radius + 1); // Q22 reciprocal of the window size

    for Col := 0 to Width - 1 do
    begin
      ColStart := @Source[Col];
      OutPtr   := @Target[Col];
      FirstVal := ColStart^;
      LastVal  := (ColStart + Width * (Height - 1))^;
      SubPtr   := ColStart;
      AddPtr   := ColStart + Radius * Width;

      Sum := (Radius + 1) * FirstVal;
      for k := 0 to Radius - 1 do
        Sum := Sum + (ColStart + k * Width)^;

      for k := 0 to Radius do
      begin
        Sum := Sum + AddPtr^ - FirstVal;
        OutPtr^ := (Sum * Recip + (1 shl 21)) shr 22;
        Inc(AddPtr, Width); Inc(OutPtr, Width);
      end;
      for k := Radius + 1 to Height - Radius - 1 do
      begin
        Sum := Sum + AddPtr^ - SubPtr^;
        OutPtr^ := (Sum * Recip + (1 shl 21)) shr 22;
        Inc(SubPtr, Width); Inc(AddPtr, Width); Inc(OutPtr, Width);
      end;
      for k := Height - Radius to Height - 1 do
      begin
        Sum := Sum + LastVal - SubPtr^;
        OutPtr^ := (Sum * Recip + (1 shl 21)) shr 22;
        Inc(SubPtr, Width); Inc(OutPtr, Width);
      end;
    end;
  end;

  procedure BoxBlur(var Buffer, Scratch: TByteArray; Width, Height, Radius: Integer);
  begin
    BlurRows(Buffer, Scratch, Width, Height, Radius);
    BlurCols(Scratch, Buffer, Width, Height, Radius);
  end;

  // The three box-blur widths (odd) whose repetition approximates a Gaussian of the given sigma:
  // the first LoCount boxes use width Lower, the rest use Lower + 2. Three boxes is the standard
  // sweet spot -- already Gaussian to the eye, more passes barely change it. (Ivan Kutskir)
  function BoxesForGauss(Sigma: Double): TIntegerArray;
  var
    Lower, LoCount, AllWide, PerBox: Integer;
    Target: Double;
  begin
    Lower := Floor(Sqrt(4 * Sigma * Sigma + 1)); // ideal box width for 3 boxes, forced odd
    if (Lower mod 2 = 0) then
      Dec(Lower);

    Target  := 12 * Sigma * Sigma;
    AllWide := 3 * (Lower + 1) * (Lower + 3);
    PerBox  := 4 * (Lower + 1);
    LoCount := Round((AllWide - Target) / PerBox);

    Result := [
      IfThen(LoCount > 0, Lower, Lower + 2),
      IfThen(LoCount > 1, Lower, Lower + 2),
      IfThen(LoCount > 2, Lower, Lower + 2)
    ];
  end;

var
  Scratch: TByteArray;
  Boxes: TIntegerArray;
begin
  if (Width <= 0) or (Height <= 0) then
    Exit;

  Move(Src[0], Dst[0], Width * Height); // work in Dst; Src stays as the untouched input
  SetLength(Scratch, Width * Height);
  Boxes := BoxesForGauss(Radius);

  BoxBlur(Dst, Scratch, Width, Height, (Boxes[0] - 1) div 2);
  BoxBlur(Dst, Scratch, Width, Height, (Boxes[1] - 1) div 2);
  BoxBlur(Dst, Scratch, Width, Height, (Boxes[2] - 1) div 2);
end;

function SimbaImage_BlurGauss(Image: TSimbaImage; Radius: Single): TSimbaImage;
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
  Ptr, Upper: PColorBGRA;
begin
  Result := Image.GreyScale();
  if not Result.DataRange(Ptr, Upper) then
    Exit;

  FillByte(Histogram, SizeOf(Histogram), 0);
  Min := 255;
  Max := 0;
  Level := 0;
  NumPixels := Result.Width * Result.Height;

  // Compute histogram and determine min and max pixel values
  while (Ptr <= Upper) do
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
  while (Ptr <= Upper) do
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

procedure SimbaImage_ReplaceColor(Image: TSimbaImage; OldColor, NewColor: TColor; Tol: Single);
var
  Old, New: TColorBGRA;
  Ptr, Upper: PColorBGRA;
begin
  if not Image.DataRange(Ptr, Upper) then
    Exit;

  Old := TSimbaColorConversion.ColorToBGRA(OldColor);
  New := TSimbaColorConversion.ColorToBGRA(NewColor, ALPHA_OPAQUE);
  while (Ptr <= Upper) do
  begin
    if SimilarRGB(Old, Ptr^, Tol) then
      Ptr^ := New;
    Inc(Ptr);
  end;
end;

procedure SimbaImage_ReplaceColorBinary(Image: TSimbaImage; Invert: Boolean; Color: TColor; Tol: Single);
const
  BLACK: TColorBGRA = (B:0; G:0; R:0; A: ALPHA_OPAQUE);
  WHITE: TColorBGRA = (B:255; G:255; R:255; A: ALPHA_OPAQUE);
var
  Col: TColorBGRA;
  Ptr, Upper: PColorBGRA;
  Hit, Miss: TColorBGRA;
begin
  if not Image.DataRange(Ptr, Upper) then
    Exit;

  Hit := WHITE;
  Miss := BLACK;
  if Invert then
    Swap(Hit, Miss);

  Col := TSimbaColorConversion.ColorToBGRA(Color);
  while (Ptr <= Upper) do
  begin
    if SimilarRGB(Col, Ptr^, Tol) then
      Ptr^ := Hit
    else
      Ptr^ := Miss;

    Inc(Ptr);
  end;
end;

procedure SimbaImage_ReplaceColorBinary(Image: TSimbaImage; Invert: Boolean; Colors: TColorArray; Tol: Single);
const
  BLACK: TColorBGRA = (B:0; G:0; R:0; A: ALPHA_OPAQUE);
  WHITE: TColorBGRA = (B:255; G:255; R:255; A: ALPHA_OPAQUE);
var
  Cols: array of TColorBGRA;
  I: Integer;
  Ptr, Upper: PColorBGRA;
  Hit, Miss: TColorBGRA;
label
  Next;
begin
  if not Image.DataRange(Ptr, Upper) then
    Exit;

  Hit := WHITE;
  Miss := BLACK;
  if Invert then
    Swap(Hit, Miss);

  SetLength(Cols, Length(Colors));
  for I := 0 to High(Colors) do
    Cols[I] := TSimbaColorConversion.ColorToBGRA(Colors[I]);

  while (Ptr <= Upper) do
  begin
    for I := 0 to High(Cols) do
      if SimilarRGB(Cols[I], Ptr^, Tol) then
      begin
        Ptr^ := Hit;
        goto Next;
      end;
    Ptr^ := Miss;
    Next:

    Inc(Ptr);
  end;
end;

end.

