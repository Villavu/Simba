// https://blog.ivank.net/fastest-gaussian-blur.html
unit simba.image_gaussblur;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes;

procedure imgGaussBlur(radius: Double; var r,g,b: TByteArray; width, height: Integer);

implementation

uses
  Math;

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

procedure boxBlurT_4(var scl, tcl: TByteArray; w, h: Integer; r: Integer);
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
    ri := round(ti + r * w);
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

    for j := r + 1 to (h - r) - 1 do
    begin
      val += scl[ri] - scl[li];
      tcl[ti] := round(val * iarr);
      li += w;
      ri += w;
      ti += w;
    end;

    for j := h - r to h-1 do
    begin
      val += lv - scl[li];
      tcl[ti] := round(val * iarr);
      li += w;
      ti += w;
    end;
  end;
end;

procedure boxBlur_4(var scl, tcl: TByteArray; w,h: Integer; r: Integer);
var
  i: Integer;
begin
  for i:=0 to High(scl) do
    tcl[i] := scl[i];

  boxBlurH_4(tcl, scl, w, h, r);
  boxBlurT_4(scl, tcl, w, h, r);
end;

procedure gaussBlur_4(var scl, tcl: TByteArray; w,h: Integer; r: Double; bxs: TIntegerArray);
begin
  boxBlur_4(scl, tcl, w, h, (bxs[0] - 1) div 2);
  boxBlur_4(tcl, scl, w, h, (bxs[1] - 1) div 2);
  boxBlur_4(scl, tcl, w, h, (bxs[2] - 1) div 2);
end;

procedure imgGaussBlur(radius: Double; var r, g, b: TByteArray; width, height: Integer);
var
  outR, outG, outB: TByteArray;
  boxes: TIntegerArray;
begin
  SetLength(outR, Length(r));
  SetLength(outG, Length(g));
  SetLength(outB, Length(b));

  boxes := boxesForGauss(radius, 3);

  gaussBlur_4(r, outR, width, height-1, radius, boxes);
  gaussBlur_4(g, outG, width, height, radius, boxes);
  gaussBlur_4(b, outB, width, height, radius, boxes);

  r := outR;
  g := outG;
  b := outB;
end;

end.

