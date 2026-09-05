{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
}
unit simba.component_imageboxdrawers;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base,
  simba.colormath,
  simba.image,
  simba.image_drawmatrix,
  simba.image_utils,
  simba.math;

type
  TDrawInfo = record
    Color: TColor;
    Data: PByte;
    BytesPerLine: Integer;
    Width: Integer;
    Height: Integer;
    VisibleRect: TRect;
    Offset: TPoint;
  end;

generic procedure DoDrawPoints<_T>(TPA: TPointArray; DrawInfo: TDrawInfo);
generic procedure DoDrawLine<_T>(Start, Stop: TPoint; DrawInfo: TDrawInfo);
generic procedure DoDrawBoxEdge<_T>(Box: TBox; DrawInfo: TDrawInfo);
generic procedure DoDrawBoxFilled<_T>(Box: TBox; DrawInfo: TDrawInfo);
generic procedure DoDrawBoxFilledEx<_T>(Box: TBox; Transparency: Single; DrawInfo: TDrawInfo);
generic procedure DoDrawCircle<_T>(Center: TPoint; Radius: Integer; DrawInfo: TDrawInfo);
generic procedure DoDrawCircleFilled<_T>(Center: TPoint; Radius: Integer; DrawInfo: TDrawInfo);
generic procedure DoDrawPolygonFilled<_T>(Poly: TPointArray; DrawInfo: TDrawInfo);
generic procedure DoDrawHeatmap<_T>(Mat: TSingleMatrix; DrawInfo: TDrawInfo);
generic procedure DoDrawImage<_T>(Image: TSimbaImage; Point: TPoint; DrawInfo: TDrawInfo);

implementation

uses
  Math,
  simba.vartype_matrix, simba.array_algorithm, simba.vartype_box, simba.vartype_pointarray, simba.geometry;

generic procedure DoDrawPoints<_T>(TPA: TPointArray; DrawInfo: TDrawInfo);
type
  PType = ^_T;
var
  Color: _T;
  I, X, Y: Integer;
begin
  Color := Default(_T);
  Color.R := DrawInfo.Color.R;
  Color.G := DrawInfo.Color.G;
  Color.B := DrawInfo.Color.B;

  for I := 0 to High(TPA) do
  begin
    X := TPA[I].X + DrawInfo.Offset.X;
    Y := TPA[I].Y + DrawInfo.Offset.Y;
    if (UInt32(X) < UInt32(DrawInfo.Width)) and (UInt32(Y) < UInt32(DrawInfo.Height)) then
      PType(DrawInfo.Data + (Y * DrawInfo.BytesPerLine + X * SizeOf(_T)))^ := Color;
  end;
end;

generic procedure DoDrawLine<_T>(Start, Stop: TPoint; DrawInfo: TDrawInfo);
type
  PType = ^_T;
var
  Color: _T;

  procedure DoPixel(const X, Y: Integer); inline;
  begin
    if (UInt32(X) < UInt32(DrawInfo.Width)) and (UInt32(Y) < UInt32(DrawInfo.Height)) then
      PType(DrawInfo.Data + (Y * DrawInfo.BytesPerLine + X * SizeOf(_T)))^ := Color;
  end;

  procedure DoRow(Y: Integer; X1, X2: Integer);
  var
    Ptr: PByte;
    Upper: PtrUInt;
  begin
    if (UInt32(Y) >= UInt32(DrawInfo.Height)) then
      Exit;
    if (X1 < 0) then
      X1 := 0;
    if (X2 >= DrawInfo.Width) then
      X2 := DrawInfo.Width - 1;
    if (X1 > X2) then   // empty, or entirely off-screen (clamping both ends would leave an edge pixel)
      Exit;

    Ptr := DrawInfo.Data + (Y * DrawInfo.BytesPerLine + X1 * SizeOf(_T));
    Upper := PtrUInt(Ptr) + ((X2 - X1) * SizeOf(_T));
    while (PtrUInt(Ptr) <= Upper) do
    begin
      PType(Ptr)^ := Color;

      Inc(Ptr, SizeOf(_T));
    end;
  end;

  {$define _Pixel := DoPixel}
  {$define _Row := DoRow}
  {$i shapebuilders/shapebuilder_line.inc}

begin
  Color := Default(_T);
  Color.R := DrawInfo.Color.R;
  Color.G := DrawInfo.Color.G;
  Color.B := DrawInfo.Color.B;

  _BuildLine(Start, Stop, 1, TBox.Create(0, 0, DrawInfo.Width - 1, DrawInfo.Height - 1));
end;

generic procedure DoDrawBoxEdge<_T>(Box: TBox; DrawInfo: TDrawInfo);
type
  PType = ^_T;
var
  Color: _T;

  procedure DoPixel(const X, Y: Integer); inline;
  begin
    if (UInt32(X) < UInt32(DrawInfo.Width)) and (UInt32(Y) < UInt32(DrawInfo.Height)) then
      PType(DrawInfo.Data + (Y * DrawInfo.BytesPerLine + X * SizeOf(_T)))^ := Color;
  end;

  procedure DoRow(Y: Integer; X1, X2: Integer);
  var
    Ptr: PByte;
    Upper: PtrUInt;
  begin
    if (UInt32(Y) >= UInt32(DrawInfo.Height)) then
      Exit;
    if (X1 < 0) then
      X1 := 0;
    if (X2 >= DrawInfo.Width) then
      X2 := DrawInfo.Width - 1;
    if (X1 > X2) then   // empty, or entirely off-screen (clamping both ends would leave an edge pixel)
      Exit;

    Ptr := DrawInfo.Data + (Y * DrawInfo.BytesPerLine + X1 * SizeOf(_T));
    Upper := PtrUInt(Ptr) + ((X2 - X1) * SizeOf(_T));
    while (PtrUInt(Ptr) <= Upper) do
    begin
      PType(Ptr)^ := Color;

      Inc(Ptr, SizeOf(_T));
    end;
  end;

  {$define _Pixel := DoPixel}
  {$define _Row := DoRow}
  {$i shapebuilders/shapebuilder_boxedge.inc}

begin
  Color := Default(_T);
  Color.R := DrawInfo.Color.R;
  Color.G := DrawInfo.Color.G;
  Color.B := DrawInfo.Color.B;

  _BuildBoxEdge(Box, 1, TBox.Create(0, 0, DrawInfo.Width - 1, DrawInfo.Height - 1));
end;

generic procedure DoDrawBoxFilled<_T>(Box: TBox; DrawInfo: TDrawInfo);
type
  PType = ^_T;
var
  Color: _T;

  procedure DoRow(Y: Integer; X1, X2: Integer);
  var
    Ptr: PByte;
    Upper: PtrUInt;
  begin
    if (UInt32(Y) >= UInt32(DrawInfo.Height)) then
      Exit;
    if (X1 < 0) then
      X1 := 0;
    if (X2 >= DrawInfo.Width) then
      X2 := DrawInfo.Width - 1;
    if (X1 > X2) then   // empty, or entirely off-screen (clamping both ends would leave an edge pixel)
      Exit;

    Ptr := DrawInfo.Data + (Y * DrawInfo.BytesPerLine + X1 * SizeOf(_T));
    Upper := PtrUInt(Ptr) + ((X2 - X1) * SizeOf(_T));
    while (PtrUInt(Ptr) <= Upper) do
    begin
      PType(Ptr)^ := Color;

      Inc(Ptr, SizeOf(_T));
    end;
  end;

  {$define _Row := DoRow}
  {$i shapebuilders/shapebuilder_box.inc}

begin
  Color := Default(_T);
  Color.R := DrawInfo.Color.R;
  Color.G := DrawInfo.Color.G;
  Color.B := DrawInfo.Color.B;

  _BuildBox(Box, False, TBox.Create(0, 0, DrawInfo.Width - 1, DrawInfo.Height - 1));
end;

generic procedure DoDrawBoxFilledEx<_T>(Box: TBox; Transparency: Single; DrawInfo: TDrawInfo);
type
  PType = ^_T;
var
  RMod, GMod, BMod: Single;
  Size, Y: Integer;
  Ptr: PByte;
  Upper: PtrUInt;
begin
  RMod := DrawInfo.Color.R * Transparency;
  GMod := DrawInfo.Color.G * Transparency;
  BMod := DrawInfo.Color.B * Transparency;
  Transparency := 1.0 - Transparency;

  Size := Box.Width * SizeOf(_T);
  for Y := Box.Y1 to Box.Y2 do
  begin
    Ptr := DrawInfo.Data + (Y * DrawInfo.BytesPerLine + Box.X1 * SizeOf(_T));
    Upper := PtrUInt(Ptr + Size);
    while (PtrUInt(Ptr) < Upper) do
      with PType(Ptr)^ do
      begin
        R := Byte(Round((R * Transparency) + RMod));
        G := Byte(Round((G * Transparency) + GMod));
        B := Byte(Round((B * Transparency) + BMod));

        Inc(Ptr, SizeOf(_T));
      end;
  end;
end;

generic procedure DoDrawCircle<_T>(Center: TPoint; Radius: Integer; DrawInfo: TDrawInfo);
type
  PType = ^_T;
var
  Color: _T;

  procedure DoPixel(const X, Y: Integer); inline;
  begin
    if (UInt32(X) < UInt32(DrawInfo.Width)) and (UInt32(Y) < UInt32(DrawInfo.Height)) then
      PType(DrawInfo.Data + (Y * DrawInfo.BytesPerLine + X * SizeOf(_T)))^ := Color;
  end;

  procedure DoRow(Y: Integer; X1, X2: Integer);
  var
    Ptr: PByte;
    Upper: PtrUInt;
  begin
    if (UInt32(Y) >= UInt32(DrawInfo.Height)) then
      Exit;
    if (X1 < 0) then
      X1 := 0;
    if (X2 >= DrawInfo.Width) then
      X2 := DrawInfo.Width - 1;
    if (X1 > X2) then   // empty, or entirely off-screen (clamping both ends would leave an edge pixel)
      Exit;

    Ptr := DrawInfo.Data + (Y * DrawInfo.BytesPerLine + X1 * SizeOf(_T));
    Upper := PtrUInt(Ptr) + ((X2 - X1) * SizeOf(_T));
    while (PtrUInt(Ptr) <= Upper) do
    begin
      PType(Ptr)^ := Color;

      Inc(Ptr, SizeOf(_T));
    end;
  end;

  {$define _Pixel := DoPixel}
  {$define _Row := DoRow}
  {$i shapebuilders/shapebuilder_ellipseedge.inc}

begin
  Color := Default(_T);
  Color.R := DrawInfo.Color.R;
  Color.G := DrawInfo.Color.G;
  Color.B := DrawInfo.Color.B;

  _BuildEllipseEdge(Center.X, Center.Y, Radius, Radius, 1, TBox.Create(0, 0, DrawInfo.Width - 1, DrawInfo.Height - 1));
end;

generic procedure DoDrawCircleFilled<_T>(Center: TPoint; Radius: Integer; DrawInfo: TDrawInfo);
type
  PType = ^_T;
var
  Color: _T;

  procedure DoRow(Y: Integer; X1, X2: Integer);
  var
    Ptr: PByte;
    Upper: PtrUInt;
  begin
    if (UInt32(Y) >= UInt32(DrawInfo.Height)) then
      Exit;
    if (X1 < 0) then
      X1 := 0;
    if (X2 >= DrawInfo.Width) then
      X2 := DrawInfo.Width - 1;
    if (X1 > X2) then   // empty, or entirely off-screen (clamping both ends would leave an edge pixel)
      Exit;

    Ptr := DrawInfo.Data + (Y * DrawInfo.BytesPerLine + X1 * SizeOf(_T));
    Upper := PtrUInt(Ptr) + ((X2 - X1) * SizeOf(_T));
    while (PtrUInt(Ptr) <= Upper) do   // was "<": dropped the rightmost pixel of every row
    begin
      PType(Ptr)^ := Color;

      Inc(Ptr, SizeOf(_T));
    end;
  end;

  {$define _Row := DoRow}
  {$i shapebuilders/shapebuilder_ellipse.inc}

begin
  Color := Default(_T);
  Color.R := DrawInfo.Color.R;
  Color.G := DrawInfo.Color.G;
  Color.B := DrawInfo.Color.B;

  _BuildEllipse(Center.X, Center.Y, Radius, Radius, False, TBox.Create(0, 0, DrawInfo.Width - 1, DrawInfo.Height - 1));
end;

generic procedure DoDrawPolygonFilled<_T>(Poly: TPointArray; DrawInfo: TDrawInfo);
type
  PType = ^_T;
var
  Color: _T;

  procedure DoRow(Y: Integer; X1, X2: Integer);
  var
    Ptr: PByte;
    Upper: PtrUInt;
  begin
    if (UInt32(Y) >= UInt32(DrawInfo.Height)) then
      Exit;
    if (X1 < 0) then
      X1 := 0;
    if (X2 >= DrawInfo.Width) then
      X2 := DrawInfo.Width - 1;
    if (X1 > X2) then   // empty, or entirely off-screen (clamping both ends would leave an edge pixel)
      Exit;

    Ptr := DrawInfo.Data + (Y * DrawInfo.BytesPerLine + X1 * SizeOf(_T));
    Upper := PtrUInt(Ptr) + ((X2 - X1) * SizeOf(_T));
    while (PtrUInt(Ptr) <= Upper) do
    begin
      PType(Ptr)^ := Color;

      Inc(Ptr, SizeOf(_T));
    end;
  end;

  {$define _Row := DoRow}
  {$i shapebuilders/shapebuilder_polygon.inc}

begin
  Color := Default(_T);
  Color.R := DrawInfo.Color.R;
  Color.G := DrawInfo.Color.G;
  Color.B := DrawInfo.Color.B;

  _BuildPolygon(Poly.Offset(DrawInfo.Offset), False, TBox.Create(0, 0, DrawInfo.Width - 1, DrawInfo.Height - 1));
end;

generic procedure DoDrawHeatmap<_T>(Mat: TSingleMatrix; DrawInfo: TDrawInfo);
type
  PType = ^_T;

  procedure _Pixel(const X, Y: Integer; const Color: TColorBGR); inline;
  begin
    Assert((X >= 0) and (Y >= 0));
    Assert((X < DrawInfo.Width) and (Y < DrawInfo.Height));

    with PType(DrawInfo.Data + (Y * DrawInfo.BytesPerLine + X * SizeOf(_T)))^ do
    begin
      R := Color.R;
      G := Color.G;
      B := Color.B;
    end;
  end;

var
  X, Y: Integer;
  B: TBox;
begin
  B.X1 := DrawInfo.VisibleRect.Left;
  B.Y1 := DrawInfo.VisibleRect.Top;
  B.X2 := Min(Min(B.X1 + Mat.Width, Mat.Width) - 1, DrawInfo.VisibleRect.Right - 1);
  B.Y2 := Min(Min(B.Y1 + Mat.Height, Mat.Height) - 1, DrawInfo.VisibleRect.Bottom - 1);

  for Y := B.Y1 to B.Y2 do
    for X := B.X1 to B.X2 do
      _Pixel(X + DrawInfo.Offset.X, Y + DrawInfo.Offset.Y, GetHeapmapColor(Mat[Y, X]));
end;

generic procedure DoDrawImage<_T>(Image: TSimbaImage; Point: TPoint; DrawInfo: TDrawInfo);
type
  PType = ^_T;
var
  LoopX, LoopY: Integer;
  P: TPoint;
  Dst: PType;
  Src: PColorBGRA;
  Loop: TBox;
begin
  // we can shorten the loops by not drawing what we know isn't visible
  // and wont have to check ranges
  Loop := TBox.ZERO;
  if (Point.X < DrawInfo.VisibleRect.Left) then
    Loop.X1 := DrawInfo.VisibleRect.Left - Point.X;
  if (Point.Y < DrawInfo.VisibleRect.Top) then
    Loop.Y1 := DrawInfo.VisibleRect.Top - Point.Y;
  if (Point.X + Image.Width >= DrawInfo.VisibleRect.Right) then
    Loop.X2 := DrawInfo.VisibleRect.Right - (Point.X + Image.Width);
  if (Point.Y + Image.Height >= DrawInfo.VisibleRect.Bottom) then
    Loop.Y2 := DrawInfo.VisibleRect.Bottom - (Point.Y + Image.Height);
  Loop.X2 := (Image.Width + Loop.X2) - 1;
  Loop.Y2 := (Image.Height + Loop.Y2) - 1;

  for LoopY := Loop.Y1 to Loop.Y2 do
    for LoopX := Loop.X1 to Loop.X2 do
    begin
      Assert((LoopY >= 0) and (LoopY < Image.Height));
      Assert((LoopX >= 0) and (LoopX < Image.Width));
      if (Image.Data[LoopY * Image.Width + LoopX].A = ALPHA_TRANSPARENT) then
        Continue;

      P.X := Point.X + LoopX + DrawInfo.Offset.X;
      P.Y := Point.Y + LoopY + DrawInfo.Offset.Y;
      Assert((P.X >= 0) and (P.Y >= 0) and (P.X < DrawInfo.Width) and (P.Y < DrawInfo.Height));

      Dst := PType(DrawInfo.Data + (P.Y * DrawInfo.BytesPerLine + P.X * SizeOf(_T)));
      Src := @Image.Data[LoopY * Image.Width + LoopX];
      Dst^.R := Src^.R;
      Dst^.G := Src^.G;
      Dst^.B := Src^.B;
    end;
end;

end.

