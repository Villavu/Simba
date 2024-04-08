{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.image_blur;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.image, simba.image_utils;

function SimbaImage_BlurBox(Image: TSimbaImage; Radius: Single): TSimbaImage;
function SimbaImage_BlurGauss(Image: TSimbaImage; Radius: Single): TSimbaImage;

implementation

uses
  Math,
  simba.vartype_box;

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

