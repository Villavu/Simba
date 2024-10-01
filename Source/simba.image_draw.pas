{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.image_draw;

{$DEFINE SIMBA_MAX_OPTIMIZATION}
{$i simba.inc}

interface

uses
  Classes, SysUtils, Math,
  simba.base, simba.image;

procedure SimbaImage_DrawTPA(Image: TSimbaImage; TPA: TPointArray);
procedure SimbaImage_DrawTPAAlpha(Image: TSimbaImage; TPA: TPointArray);

procedure SimbaImage_DrawLine(Image: TSimbaImage; Start, Stop: TPoint);
procedure SimbaImage_DrawLineAlpha(Image: TSimbaImage; Start, Stop: TPoint);

procedure SimbaImage_DrawLineGap(Image: TSimbaImage; Start, Stop: TPoint; GapSize: Integer);
procedure SimbaImage_DrawLineGapAlpha(Image: TSimbaImage; Start, Stop: TPoint; GapSize: Integer);

procedure SimbaImage_DrawBoxFilled(Image: TSimbaImage; Box: TBox);
procedure SimbaImage_DrawBoxFilledAlpha(Image: TSimbaImage; Box: TBox);

procedure SimbaImage_DrawBoxEdge(Image: TSimbaImage; Box: TBox);
procedure SimbaImage_DrawBoxEdgeAlpha(Image: TSimbaImage; Box: TBox);

procedure SimbaImage_DrawCircleFilled(Image: TSimbaImage; ACenter: TPoint; Radius: Integer);
procedure SimbaImage_DrawCircleFilledAlpha(Image: TSimbaImage; ACenter: TPoint; Radius: Integer);
procedure SimbaImage_DrawCircleInverted(Image: TSimbaImage; ACenter: TPoint; Radius: Integer);
procedure SimbaImage_DrawCircleInvertedAlpha(Image: TSimbaImage; ACenter: TPoint; Radius: Integer);

procedure SimbaImage_DrawCircleEdge(Image: TSimbaImage; ACenter: TPoint; Radius: Integer);
procedure SimbaImage_DrawCircleEdgeAlpha(Image: TSimbaImage; ACenter: TPoint; Radius: Integer);

procedure SimbaImage_DrawPolygonFilled(Image: TSimbaImage; Points: TPointArray);
procedure SimbaImage_DrawPolygonFilledAlpha(Image: TSimbaImage; Points: TPointArray);

procedure SimbaImage_DrawPolygonInverted(Image: TSimbaImage; Points: TPointArray);
procedure SimbaImage_DrawPolygonInvertedAlpha(Image: TSimbaImage; Points: TPointArray);

procedure SimbaImage_DrawQuadInverted(Image: TSimbaImage; Quad: TQuad);
procedure SimbaImage_DrawQuadInvertedAlpha(Image: TSimbaImage; Quad: TQuad);

procedure SimbaImage_DrawLineAA(Image: TSimbaImage; Start, Stop: TPoint; Thickness: Single = 1.5);
procedure SimbaImage_DrawEllipseAA(Image: TSimbaImage; ACenter: TPoint; XRadius, YRadius: Integer; Thickness: Single = 1.5);

implementation

uses
  simba.image_utils, simba.array_algorithm, simba.geometry,
  simba.vartype_point, simba.vartype_box, simba.vartype_quad, simba.vartype_pointarray;

procedure SimbaImage_DrawTPA(Image: TSimbaImage; TPA: TPointArray);
var
  BGRA: TColorBGRA;
  Point: TPoint;
begin
  BGRA := Image.DrawColorAsBGRA;

  for Point in TPA do
    if (Point.X >= 0) and (Point.Y >= 0) and (Point.X < Image.Width) and (Point.Y < Image.Height) then
      Image.Data[Point.Y * Image.Width + Point.X] := BGRA;
end;

procedure SimbaImage_DrawTPAAlpha(Image: TSimbaImage; TPA: TPointArray);
var
  BGRA: TColorBGRA;
  Point: TPoint;
begin
  BGRA := Image.DrawColorAsBGRA;

  for Point in TPA do
    if (Point.X >= 0) and (Point.Y >= 0) and (Point.X < Image.Width) and (Point.Y < Image.Height) then
      BlendPixel(@Image.Data[Point.Y * Image.Width + Point.X], BGRA);
end;

procedure SimbaImage_DrawLine(Image: TSimbaImage; Start, Stop: TPoint);
var
  BGRA: TColorBGRA;

  procedure _Pixel(const X, Y: Integer); inline;
  begin
    if (X >= 0) and (Y >= 0) and (X < Image.Width) and (Y < Image.Height) then
      Image.Data[Y * Image.Width + X] := BGRA;
  end;

  {$i shapebuilder_line.inc}

begin
  BGRA := Image.DrawColorAsBGRA;

  _BuildLine(Start, Stop);
end;

procedure SimbaImage_DrawLineAlpha(Image: TSimbaImage; Start, Stop: TPoint);
var
  BGRA: TColorBGRA;

  procedure _Pixel(const X, Y: Integer);
  begin
    if (X >= 0) and (Y >= 0) and (X < Image.Width) and (Y < Image.Height) then
      BlendPixel(@Image.Data[Y * Image.Width + X], BGRA);
  end;

  {$i shapebuilder_line.inc}

begin
  BGRA := Image.DrawColorAsBGRA;

  _BuildLine(Start, Stop);
end;

procedure SimbaImage_DrawLineGap(Image: TSimbaImage; Start, Stop: TPoint; GapSize: Integer);
var
  BGRA: TColorBGRA;

  procedure _Pixel(const X, Y: Integer); inline;
  begin
    if (X >= 0) and (Y >= 0) and (X < Image.Width) and (Y < Image.Height) then
      Image.Data[Y * Image.Width + X] := BGRA;
  end;

  {$i shapebuilder_linegap.inc}

begin
  BGRA := Image.DrawColorAsBGRA;

  _BuildLineGap(Start, Stop, GapSize);
end;

procedure SimbaImage_DrawLineGapAlpha(Image: TSimbaImage; Start, Stop: TPoint; GapSize: Integer);
var
  BGRA: TColorBGRA;

  procedure _Pixel(const X, Y: Integer);
  begin
    if (X >= 0) and (Y >= 0) and (X < Image.Width) and (Y < Image.Height) then
      BlendPixel(@Image.Data[Y * Image.Width + X], BGRA);
  end;

  {$i shapebuilder_linegap.inc}

begin
  BGRA := Image.DrawColorAsBGRA;

  _BuildLineGap(Start, Stop, GapSize);
end;

procedure SimbaImage_DrawBoxFilled(Image: TSimbaImage; Box: TBox);
var
  BGRA: TColorBGRA;
  W: Integer;

  procedure _Row(const Y: Integer; const X1, X2: Integer);
  begin
    FillData(@Image.Data[Y * Image.Width + X1], W, BGRA);
  end;

  {$i shapebuilder_boxfilled.inc}

begin
  Box := Box.Clip(TBox.Create(0, 0, Image.Width-1, Image.Height - 1));

  if (Box.Width > 1) and (Box.Height > 1) then
  begin
    BGRA := Image.DrawColorAsBGRA;
    W := Box.Width;

    _BuildBoxFilled(Box);
  end;
end;

procedure SimbaImage_DrawBoxFilledAlpha(Image: TSimbaImage; Box: TBox);
var
  BGRA: TColorBGRA;

  procedure _Row(const Y: Integer; const X1, X2: Integer);
  var
    Ptr: PColorBGRA;
    Upper: PtrUInt;
  begin
    Ptr := @Image.Data[Y * Image.Width + X1];
    Upper := PtrUInt(Ptr) + ((X2 - X1) * SizeOf(TColorBGRA));

    while (PtrUInt(Ptr) <= Upper) do
    begin
      BlendPixel(Ptr, BGRA);

      Inc(Ptr);
    end;
  end;

  {$i shapebuilder_boxfilled.inc}

begin
  Box := Box.Clip(TBox.Create(0, 0, Image.Width - 1, Image.Height - 1));

  if (Box.Width > 1) and (Box.Height > 1) then
  begin
    BGRA := Image.DrawColorAsBGRA;

    _BuildBoxFilled(Box);
  end;
end;

procedure SimbaImage_DrawBoxEdge(Image: TSimbaImage; Box: TBox);
var
  BGRA: TColorBGRA;

  procedure _Pixel(const X, Y: Integer); inline;
  begin
    Image.Data[Y * Image.Width + X] := BGRA;
  end;

  procedure _Row(const Y: Integer; const X1, X2: Integer);
  begin
    FillData(@Image.Data[Y * Image.Width + X1], (X2 - X1) + 1, BGRA);
  end;

  {$i shapebuilder_boxedge.inc}

begin
  Box := Box.Clip(TBox.Create(0, 0, Image.Width - 1, Image.Height - 1));

  if (Box.Width > 1) and (Box.Height > 1) then
  begin
    BGRA := Image.DrawColorAsBGRA;

    _BuildBoxEdge(Box);
  end;
end;

procedure SimbaImage_DrawBoxEdgeAlpha(Image: TSimbaImage; Box: TBox);
var
  BGRA: TColorBGRA;

  procedure _Pixel(const X, Y: Integer); inline;
  begin
    BlendPixel(@Image.Data[Y * Image.Width + X], BGRA);
  end;

  procedure _Row(const Y: Integer; const X1, X2: Integer);
  var
    Ptr: PColorBGRA;
    Upper: PtrUInt;
  begin
    Ptr := @Image.Data[Y * Image.Width + X1];
    Upper := PtrUInt(Ptr) + ((X2 - X1) * SizeOf(TColorBGRA));

    while (PtrUInt(Ptr) <= Upper) do
    begin
      BlendPixel(Ptr, BGRA);

      Inc(Ptr);
    end;
  end;

  {$i shapebuilder_boxedge.inc}

begin
  Box := Box.Clip(TBox.Create(0, 0, Image.Width - 1, Image.Height - 1));

  if (Box.Width > 1) and (Box.Height > 1) then
  begin
    BGRA := Image.DrawColorAsBGRA;

    _BuildBoxEdge(Box);
  end;
end;

procedure SimbaImage_DrawCircleFilled(Image: TSimbaImage; ACenter: TPoint; Radius: Integer);
var
  BGRA: TColorBGRA;

  procedure _Row(const Y: Integer; X1, X2: Integer);
  begin
    if (Y >= 0) and (Y < Image.Height) then
    begin
      X1 := EnsureRange(X1, 0, Image.Width - 1);
      X2 := EnsureRange(X2, 0, Image.Width - 1);
      if ((X2 - X1) + 1 > 0) then
        FillData(@Image.Data[Y * Image.Width + X1], (X2 - X1) + 1, BGRA);
    end;
  end;

  {$i shapebuilder_circlefilled.inc}

begin
  BGRA := Image.DrawColorAsBGRA;

  if (Radius >= 1) then
    _BuildCircleFilled(ACenter.X, ACenter.Y, Radius);
end;

procedure SimbaImage_DrawCircleFilledAlpha(Image: TSimbaImage; ACenter: TPoint; Radius: Integer);
var
  BGRA: TColorBGRA;

  procedure _Row(const Y: Integer; X1, X2: Integer);
  var
    Ptr: PColorBGRA;
    Upper: PtrUInt;
  begin
    if (Y >= 0) and (Y < Image.Height) then
    begin
      X1 := EnsureRange(X1, 0, Image.Width - 1);
      X2 := EnsureRange(X2, 0, Image.Width - 1);

      if ((X2 - X1) + 1 > 0) then
      begin
        Ptr := @Image.Data[Y * Image.Width + X1];
        Upper := PtrUInt(Ptr) + ((X2 - X1) * SizeOf(TColorBGRA));

        while (PtrUInt(Ptr) <= Upper) do
        begin
          BlendPixel(Ptr, BGRA);

          Inc(Ptr);
        end;
      end;
    end;
  end;

  {$i shapebuilder_circlefilled.inc}

begin
  BGRA := Image.DrawColorAsBGRA;

  if (Radius >= 1) then
    _BuildCircleFilled(ACenter.X, ACenter.Y, Radius);
end;

procedure SimbaImage_DrawCircleInverted(Image: TSimbaImage; ACenter: TPoint; Radius: Integer);
var
  BGRA: TColorBGRA;
  B: TBox;
  X,Y: Integer;
begin
  BGRA := Image.DrawColorAsBGRA;

  B.X1 := Max(ACenter.X-Radius, 0);
  B.Y1 := Max(ACenter.Y-Radius, 0);
  B.X2 := Min(ACenter.X+Radius, Image.Width - 1);
  B.Y2 := Min(ACenter.Y+Radius, Image.Height - 1);

  for X := B.X1 to B.X2 do
    for Y := B.Y1 to B.Y2 do
      if Sqr(X - ACenter.X) + Sqr(Y - ACenter.Y) > Sqr(Radius) then
        Image.Data[Y * Image.Width + X] := BGRA;
end;

procedure SimbaImage_DrawCircleInvertedAlpha(Image: TSimbaImage; ACenter: TPoint; Radius: Integer);
var
  BGRA: TColorBGRA;
  B: TBox;
  X,Y: Integer;
begin
  BGRA := Image.DrawColorAsBGRA;

  B.X1 := Max(ACenter.X-Radius, 0);
  B.Y1 := Max(ACenter.Y-Radius, 0);
  B.X2 := Min(ACenter.X+Radius, Image.Width - 1);
  B.Y2 := Min(ACenter.Y+Radius, Image.Height - 1);

  for X := B.X1 to B.X2 do
    for Y := B.Y1 to B.Y2 do
      if Sqr(X - ACenter.X) + Sqr(Y - ACenter.Y) > Sqr(Radius) then
        BlendPixel(@Image.Data[Y * Image.Width + X], BGRA);
end;

procedure SimbaImage_DrawCircleEdge(Image: TSimbaImage; ACenter: TPoint; Radius: Integer);
var
  BGRA: TColorBGRA;

  procedure _Pixel(const X, Y: Integer); inline;
  begin
    if (X >= 0) and (Y >= 0) and (X < Image.Width) and (Y < Image.Height) then
      Image.Data[Y * Image.Width + X] := BGRA;
  end;

  {$i shapebuilder_circle.inc}

begin
  BGRA := Image.DrawColorAsBGRA;
  if (Radius >= 1) then
    _BuildCircle(ACenter.X, ACenter.Y, Radius);
end;

procedure SimbaImage_DrawCircleEdgeAlpha(Image: TSimbaImage; ACenter: TPoint; Radius: Integer);
var
  BGRA: TColorBGRA;

  procedure _Pixel(const X, Y: Integer); inline;
  begin
    if (X >= 0) and (Y >= 0) and (X < Image.Width) and (Y < Image.Height) then
      BlendPixel(@Image.Data[Y * Image.Width + X], BGRA);
  end;

  {$i shapebuilder_circle.inc}

begin
  BGRA := Image.DrawColorAsBGRA;
  if (Radius >= 1) then
    _BuildCircle(ACenter.X, ACenter.Y, Radius);
end;

procedure SimbaImage_DrawPolygonFilled(Image: TSimbaImage; Points: TPointArray);
var
  BGRA: TColorBGRA;

  procedure _Row(const Y: Integer; X1, X2: Integer);
  begin
    // Y is already clipped in _PolygonFilled
    X1 := EnsureRange(X1, 0, Image.Width - 1);
    X2 := EnsureRange(X2, 0, Image.Width - 1);

    if ((X2 - X1) + 1 > 0) then
      FillData(@Image.Data[Y * Image.Width + X1], (X2 - X1) + 1, BGRA);
  end;

  {$i shapebuilder_polygonfilled.inc}

begin
  BGRA := Image.DrawColorAsBGRA;

  _BuildPolygonFilled(Points, TRect.Create(0, 0, Image.Width-1, Image.Height-1), TPoint.ZERO);
end;

procedure SimbaImage_DrawPolygonFilledAlpha(Image: TSimbaImage; Points: TPointArray);
var
  BGRA: TColorBGRA;

  procedure _Row(const Y: Integer; X1, X2: Integer);
  var
    Ptr: PColorBGRA;
    Upper: PtrUInt;
  begin
    // Y is already clipped in _PolygonFilled
    X1 := EnsureRange(X1, 0, Image.Width - 1);
    X2 := EnsureRange(X2, 0, Image.Width - 1);

    if ((X2 - X1) + 1 > 0) then
    begin
      Ptr := @Image.Data[Y * Image.Width + X1];
      Upper := PtrUInt(Ptr) + ((X2 - X1) * SizeOf(TColorBGRA));

      while (PtrUInt(Ptr) <= Upper) do
      begin
        BlendPixel(Ptr, BGRA);

        Inc(Ptr);
      end;
    end;
  end;

  {$i shapebuilder_polygonfilled.inc}

begin
  BGRA := Image.DrawColorAsBGRA;

  _BuildPolygonFilled(Points, TRect.Create(0, 0, Image.Width-1, Image.Height-1), TPoint.ZERO);
end;

procedure SimbaImage_DrawPolygonInverted(Image: TSimbaImage; Points: TPointArray);
var
  BGRA: TColorBGRA;
  B: TBox;
  X,Y: Integer;
begin
  BGRA := Image.DrawColorAsBGRA;
  B := Points.Bounds().Clip(TBox.Create(0, 0, Image.Width-1, Image.Height-1));

  for X := B.X1 to B.X2 do
    for Y := B.Y1 to B.Y2 do
      if not TSimbaGeometry.PointInPolygon(TPoint.Create(X, Y), Points) then
        Image.Data[Y * Image.Width + X] := BGRA;
end;

procedure SimbaImage_DrawPolygonInvertedAlpha(Image: TSimbaImage; Points: TPointArray);
var
  BGRA: TColorBGRA;
  B: TBox;
  X,Y: Integer;
begin
  BGRA := Image.DrawColorAsBGRA;
  B := Points.Bounds().Clip(TBox.Create(0, 0, Image.Width-1, Image.Height-1));

  for X := B.X1 to B.X2 do
    for Y := B.Y1 to B.Y2 do
      if not TSimbaGeometry.PointInPolygon(TPoint.Create(X, Y), Points) then
        BlendPixel(@Image.Data[Y * Image.Width + X], BGRA);
end;

procedure SimbaImage_DrawQuadInverted(Image: TSimbaImage; Quad: TQuad);
var
  BGRA: TColorBGRA;
  B: TBox;
  X,Y: Integer;
begin
  BGRA := Image.DrawColorAsBGRA;
  B := Quad.Bounds.Clip(TBox.Create(0, 0, Image.Width-1, Image.Height-1));

  for X := B.X1 to B.X2 do
    for Y := B.Y1 to B.Y2 do
      if not TSimbaGeometry.PointInQuad(TPoint.Create(X, Y), Quad.Top, Quad.Right, Quad.Bottom, Quad.Left) then
        Image.Data[Y * Image.Width + X] := BGRA;
end;

procedure SimbaImage_DrawQuadInvertedAlpha(Image: TSimbaImage; Quad: TQuad);
var
  BGRA: TColorBGRA;
  B: TBox;
  X,Y: Integer;
begin
  BGRA := Image.DrawColorAsBGRA;
  B := Quad.Bounds.Clip(TBox.Create(0, 0, Image.Width-1, Image.Height-1));

  for X := B.X1 to B.X2 do
    for Y := B.Y1 to B.Y2 do
      if not TSimbaGeometry.PointInQuad(TPoint.Create(X, Y), Quad.Top, Quad.Right, Quad.Bottom, Quad.Left) then
        BlendPixel(@Image.Data[Y * Image.Width + X], BGRA);
end;

procedure SimbaImage_DrawLineAA(Image: TSimbaImage; Start, Stop: TPoint; Thickness: Single);
var
  BGRA: TColorBGRA;
  Alpha: Byte absolute BGRA.A;

  // https://zingl.github.io/bresenham.js
  procedure _LineAntialias(x0, y0, x1, y1: Integer; Thickness: Single);
  var
    dx, dy, err: Integer;
    e2, x2, y2: Integer;
    ed: Single;
    sx, sy: Integer;
  begin
    dx := Abs(x1 - x0);
    dy := Abs(y1 - y0);

    if (x0 < x1) then sx := 1 else sx := -1;
    if (y0 < y1) then sy := 1 else sy := -1;

    err := dx-dy;
    if (dx+dy = 0) then
      ed := 1
    else
      ed := Sqrt(Double(dx*dx) + Double(dy*dy));

    Thickness := (Thickness + 1) / 2;
    while True do
    begin
      Alpha := Round(255 - Max(0, 255 * (Abs(err-dx+dy)/ed-Thickness+1)));
      BlendPixel(Image.Data, Image.Width, Image.Height, x0, y0, BGRA);

      e2 := err;
      x2 := x0;
      if (2*e2 >= -dx) then
      begin
        e2 += dy;
        y2 := y0;
        while (e2 < ed*Thickness) and ((y1 <> y2) or (dx > dy)) do
        begin
          y2 += sy;

          Alpha := Round(255 - Max(0, 255 * (Abs(e2)/ed-Thickness+1)));
          BlendPixel(Image.Data, Image.Width, Image.Height, x0, y2, BGRA);

          e2 += dx;
        end;
        if (x0 = x1) then
          Break;

        e2 := err;
        err -= dy;
        x0 += sx;
      end;

      if (2*e2 <= dy) then
      begin
        e2 := dx-e2;
        while (e2 < ed*Thickness) and ((x1 <> x2) or (dx < dy)) do
        begin
          x2 += sx;

          Alpha := Round(255 - Max(0, 255 * (Abs(e2)/ed-Thickness+1)));
          BlendPixel(Image.Data, Image.Width, Image.Height, x2, y0, BGRA);

          e2 += dy;
        end;
        if (y0 = y1) then
          Break;

        err += dx;
        y0 += sy;
      end;
    end;
  end;

begin
  BGRA := Image.DrawColorAsBGRA;

  _LineAntialias(
    Start.X, Start.Y,
    Stop.X, Stop.Y,
    Thickness
  );
end;

procedure SimbaImage_DrawEllipseAA(Image: TSimbaImage; ACenter: TPoint; XRadius, YRadius: Integer; Thickness: Single);
var
  BGRA: TColorBGRA;
  Alpha: Byte absolute BGRA.A;

  // https://zingl.github.io/bresenham.js
  procedure _EllipseAntialias(x0, y0, x1, y1: Integer; Thickness: Single);
  var
    a,b,b1: Integer;
    a2,b2: Single;
    dx,dy: Single;
    err: Single;
    dx2,dy2,e2,ed: Single;
    i: Single;
  begin
    a := Abs(x1 - x0);
    b := Abs(y1 - y0);
    if (a = 0) or (b = 0) then
      Exit;

    b1 := b and 1;
    a2 := a-2*Thickness;
    b2 := b-2*Thickness;
    dx := 4*(a-1)*b*b;
    dy := 4*(b1-1)*a*a;

    i := a+b2;
    err := b1*a*a;

    if ((Thickness-1) * (2*b-Thickness) > a*a) then
      b2 := Sqrt(a*(b-a)*i*a2) / (a-Thickness);

    if ((Thickness-1) * (2*a-Thickness) > b*b) then
    begin
      a2 := Sqrt(b*(a-b)*i*b2) / (b-Thickness);
      Thickness := (a-a2) / 2;
    end;

    if (x0 > x1) then
    begin
      x0 := x1;
      x1 += a;
    end;

    if (y0 > y1) then
      y0 := y1;

    if (b2 <= 0) then
      Thickness := a;

    e2 := Thickness - Floor(Thickness);
    Thickness := x0+Thickness-e2;
    dx2 := 4*(a2+2*e2-1)*b2*b2;
    dy2 := 4*(b1-1)*a2*a2;
    e2 := dx2*e2;
    y0 += (b+1) shr 1;
    y1 := y0-b1;
    a := 8*a*a;
    b1 := 8*b*b;
    a2 := 8*a2*a2;
    b2 := 8*b2*b2;

    repeat
      while True do
      begin
        if (err < 0) or (x0 > x1) then
        begin
          i := x0;
          Break;
        end;

        i := Min(dx,dy);
        ed := Max(dx,dy);

        if ((y0 = y1+1) and (2*err > dx) and (a > b1)) then
          ed := a/4
        else
          ed += 2*ed*i*i/(4*ed*ed+i*i+1)+1;
        i := 255*err/ed;

        Alpha := 255-Byte(Round(i));

        BlendPixel(Image.Data, Image.Width, Image.Height, x0, y0, BGRA);
        BlendPixel(Image.Data, Image.Width, Image.Height, x0, y1, BGRA);
        BlendPixel(Image.Data, Image.Width, Image.Height, x1, y0, BGRA);
        BlendPixel(Image.Data, Image.Width, Image.Height, x1, y1, BGRA);

        if (err+dy+a < dx) then
        begin
          i := x0+1;
          Break;
        end;

        x0 += 1;
        x1 -= 1;
        err -= dx;
        dx -= b1;
      end;

      Alpha := 255;

      while (i < Thickness) and (2*i <= x0+x1) do
      begin
        BlendPixel(Image.Data, Image.Width, Image.Height, Round(i),       y0, BGRA);
        BlendPixel(Image.Data, Image.Width, Image.Height, Round(x0+x1-i), y0, BGRA);
        BlendPixel(Image.Data, Image.Width, Image.Height, Round(i),       y1, BGRA);
        BlendPixel(Image.Data, Image.Width, Image.Height, Round(x0+x1-i), y1, BGRA);

        i += 1.0;
      end;

      while ((e2 > 0) and (x0+x1 >= 2*Thickness)) do
      begin
         i := Min(dx2, dy2);
         ed := Max(dx2, dy2);

         if (y0 = y1+1) and (2*e2 > dx2) and (a2 > b2) then
           ed := a2/4
         else
           ed += 2*ed*i*i/(4*ed*ed+i*i);

         Alpha := 255-Byte(Round(255-255*e2/ed));

         BlendPixel(Image.Data, Image.Width, Image.Height, Round(Thickness),       y0, BGRA);
         BlendPixel(Image.Data, Image.Width, Image.Height, Round(x0+x1-Thickness), y0, BGRA);
         BlendPixel(Image.Data, Image.Width, Image.Height, Round(Thickness),       y1, BGRA);
         BlendPixel(Image.Data, Image.Width, Image.Height, Round(x0+x1-Thickness), y1, BGRA);

         if (e2+dy2+a2 < dx2) then
           Break;

         Thickness += 1;
         e2 -= dx2;
         dx2 -= b2;
      end;

      dy2 += a2;
      e2 += dy2;
      y0 += 1;
      y1 -= 1;
      dy += a;
      err += dy;
    until (x0 >= x1);

    while (y0-y1 <= b) do
    begin
      Alpha := 255 - Byte(Round(255*4*err/b1));

      BlendPixel(Image.Data, Image.Width, Image.Height, x0, y0, BGRA);
      BlendPixel(Image.Data, Image.Width, Image.Height, x1, y0, BGRA);
      BlendPixel(Image.Data, Image.Width, Image.Height, x0, y1, BGRA);
      BlendPixel(Image.Data, Image.Width, Image.Height, x1, y1, BGRA);

      y0 += 1;
      y1 -= 1;
      dy += a;
      err += dy;
    end;
  end;

begin
  BGRA := Image.DrawColorAsBGRA;

  _EllipseAntialias(
    ACenter.X - XRadius, ACenter.Y - YRadius,
    ACenter.X + XRadius, ACenter.Y + YRadius,
    Thickness
  );
end;

end.

