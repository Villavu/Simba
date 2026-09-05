{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Aliased shape drawing for TSimbaImage
}
unit simba.image_draw;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Math,
  simba.base,
  simba.image,
  simba.vartype_polygon;

procedure SimbaImage_DrawTPA(Image: TSimbaImage; TPA: TPointArray; Thickness: Single);
procedure SimbaImage_DrawLine(Image: TSimbaImage; Start, Stop: TPoint; Thickness: Single);

procedure SimbaImage_DrawBox(Image: TSimbaImage; Box: TBox; Inverted: Boolean);
procedure SimbaImage_DrawBoxEdge(Image: TSimbaImage; Box: TBox; Thickness: Single);

procedure SimbaImage_DrawEllipse(Image: TSimbaImage; ACenter: TPoint; XRadius, YRadius: Integer; Inverted: Boolean);
procedure SimbaImage_DrawEllipseEdge(Image: TSimbaImage; ACenter: TPoint; XRadius, YRadius: Integer; Thickness: Single);

procedure SimbaImage_DrawPolygon(Image: TSimbaImage; Poly: TPolygon; Inverted: Boolean);
procedure SimbaImage_DrawPolygonEdge(Image: TSimbaImage; Poly: TPolygon; Thickness: Single);

implementation

uses
  simba.image_utils, simba.math,
  simba.vartype_point, simba.vartype_box;

type
  TDrawer = record
  private
    FData: PColorBGRA;
    FImgW, FImgH: Integer;
    FColor: TColorBGRA;
    FUseAlpha: Boolean;
    FBuffered: Boolean;        
    FRowHead: TIntegerArray;   
    FSpans: array of record   
      Next, X1, XEnd: Integer;
    end;
    FCount, FMinY, FMaxY: Integer;
    procedure Grow;
    function PushSpan(Y, X1, XEnd: Integer): Boolean; inline;
  public
    Clip: TBox;
    procedure Init(Image: TSimbaImage; Buffered: Boolean = False);
    procedure Row(Y, X1, X2: Integer); inline;
    procedure Pixel(const X, Y: Integer); inline;
    procedure Pixels(const Points: TPointArray);
    procedure Flush;
  end;

procedure TDrawer.Init(Image: TSimbaImage; Buffered: Boolean);
begin
  Clip      := TBox.Create(0, 0, Image.Width - 1, Image.Height - 1);
  FData     := Image.Data;
  FImgW     := Image.Width;
  FImgH     := Image.Height;
  FColor    := Image.DrawColorAsBGRA;
  FUseAlpha := (Image.DrawAlpha <> ALPHA_OPAQUE);
  FBuffered := FUseAlpha and Buffered;
  FCount    := 0;
  if FBuffered then
  begin
    FMinY    := FImgH;
    FMaxY    := -1;
    FRowHead := nil;
    SetLength(FRowHead, FImgH);
    SetLength(FSpans, 768);
  end;
end;

procedure TDrawer.Row(Y, X1, X2: Integer);
var
  Ptr, PEnd: PColorBGRA;
begin
  if (UInt32(Y) >= UInt32(FImgH)) then
    Exit;
  X1 := Max(X1, 0);
  X2 := Min(X2, FImgW - 1);
  if (X1 > X2) then
    Exit;

  if FBuffered then
  begin
    while not PushSpan(Y, X1, X2 + 1) do
      Grow;
  end
  else if FUseAlpha then
  begin
    Ptr  := @FData[Int64(Y) * FImgW + X1];
    PEnd := @Ptr[X2 - X1];
    while (PtrUInt(Ptr) <= PtrUInt(PEnd)) do
    begin
      BlendPixel(Ptr, @FColor);
      Inc(Ptr);
    end;
  end
  else
    FillData(@FData[Int64(Y) * FImgW + X1], (X2 - X1) + 1, FColor);
end;

procedure TDrawer.Pixel(const X, Y: Integer);
begin
  if (UInt32(X) >= UInt32(FImgW)) or (UInt32(Y) >= UInt32(FImgH)) then
    Exit;
  if FBuffered then
  begin
    while not PushSpan(Y, X, X + 1) do
      Grow;
  end
  else if FUseAlpha then
    BlendPixel(@FData[Int64(Y) * FImgW + X], @FColor)
  else
    FData[Int64(Y) * FImgW + X] := FColor;
end;

procedure TDrawer.Pixels(const Points: TPointArray);
var
  Point, PointEnd: PPoint;
  W, H: Integer;
  Data: PColorBGRA;
  Col: TColorBGRA;
begin
  if (Length(Points) = 0) then
    Exit;
  W := FImgW;
  H := FImgH;
  Data := FData;
  Col := FColor;
  Point    := @Points[0];
  PointEnd := @Points[High(Points)];

  if FUseAlpha then
    while (PtrUInt(Point) <= PtrUInt(PointEnd)) do
    begin
      if (UInt32(Point^.X) < UInt32(W)) and (UInt32(Point^.Y) < UInt32(H)) then
        BlendPixel(@Data[Int64(Point^.Y) * W + Point^.X], @Col);
      Inc(Point);
    end
  else
    while (PtrUInt(Point) <= PtrUInt(PointEnd)) do
    begin
      if (UInt32(Point^.X) < UInt32(W)) and (UInt32(Point^.Y) < UInt32(H)) then
        Data[Int64(Point^.Y) * W + Point^.X] := Col;
      Inc(Point);
    end;
end;

procedure TDrawer.Grow;
begin
  SetLength(FSpans, Length(FSpans) * 2);
end;

function TDrawer.PushSpan(Y, X1, XEnd: Integer): Boolean;
var
  Prev, Cur, N: Integer;
begin
  Result := True;
  Prev := -1;
  Cur  := FRowHead[Y] - 1;
  while (Cur >= 0) and (FSpans[Cur].XEnd < X1) do
  begin
    Prev := Cur;
    Cur  := FSpans[Cur].Next;
  end;

  if (Cur >= 0) and (FSpans[Cur].X1 <= XEnd) then
  begin
    if (FSpans[Cur].X1 < X1) then
      X1 := FSpans[Cur].X1;
    if (FSpans[Cur].XEnd > XEnd) then
      XEnd := FSpans[Cur].XEnd;
    N := FSpans[Cur].Next;
    while (N >= 0) and (FSpans[N].X1 <= XEnd) do
    begin
      if (FSpans[N].XEnd > XEnd) then
        XEnd := FSpans[N].XEnd;
      N := FSpans[N].Next;
    end;
    FSpans[Cur].X1   := X1;
    FSpans[Cur].XEnd := XEnd;
    FSpans[Cur].Next := N;
    Exit;
  end;

  if (FCount = Length(FSpans)) then
    Exit(False);
  FSpans[FCount].Next := Cur;                                
  FSpans[FCount].X1   := X1;
  FSpans[FCount].XEnd := XEnd;
  if (Prev < 0) then
    FRowHead[Y] := FCount + 1
  else
    FSpans[Prev].Next := FCount;
  Inc(FCount);
  if (Y < FMinY) then FMinY := Y;
  if (Y > FMaxY) then FMaxY := Y;
end;

procedure TDrawer.Flush;
var
  Y, Idx: Integer;
  RowPtr, Ptr, PEnd: PColorBGRA;
begin
  if (FCount = 0) then
    Exit;

  for Y := FMinY to FMaxY do
  begin
    RowPtr := @FData[Int64(Y) * FImgW];
    Idx := FRowHead[Y] - 1;
    while (Idx >= 0) do
      with FSpans[Idx] do
      begin
        Ptr  := @RowPtr[X1];
        PEnd := @RowPtr[XEnd];
        while (PtrUInt(Ptr) < PtrUInt(PEnd)) do
        begin
          BlendPixel(Ptr, @FColor);
          Inc(Ptr);
        end;
        Idx := Next;
      end;
  end;
end;

procedure SimbaImage_DrawTPA(Image: TSimbaImage; TPA: TPointArray; Thickness: Single);
var
  Drawer: TDrawer;
  I, Radius: Integer;

  {$define _Row := Drawer.Row}
  {$i shapebuilders/shapebuilder_ellipse.inc}

begin
  if (Round(Thickness) <= 1) then
  begin
    Drawer.Init(Image);
    Drawer.Pixels(TPA);
    Exit;
  end;

  Radius := Round(Thickness) div 2;
  Drawer.Init(Image, True);
  for I := 0 to High(TPA) do
    _BuildEllipse(TPA[I].X, TPA[I].Y, Radius, Radius, False, Drawer.Clip);
  Drawer.Flush;
end;

procedure SimbaImage_DrawLine(Image: TSimbaImage; Start, Stop: TPoint; Thickness: Single);
var
  Drawer: TDrawer;

  {$define _Row := Drawer.Row}
  {$define _Pixel := Drawer.Pixel}
  {$i shapebuilders/shapebuilder_line.inc}

begin
  Drawer.Init(Image);
  _BuildLine(Start, Stop, Round(Thickness), Drawer.Clip);
end;

procedure SimbaImage_DrawBox(Image: TSimbaImage; Box: TBox; Inverted: Boolean);
var
  Drawer: TDrawer;

  {$define _Row := Drawer.Row}
  {$i shapebuilders/shapebuilder_box.inc}

begin
  if (Image.Width < 1) or (Image.Height < 1) then
    Exit;
  Drawer.Init(Image);
  _BuildBox(Box, Inverted, Drawer.Clip);
end;

procedure SimbaImage_DrawBoxEdge(Image: TSimbaImage; Box: TBox; Thickness: Single);
var
  Drawer: TDrawer;

  {$define _Row := Drawer.Row}
  {$define _Pixel := Drawer.Pixel}
  {$i shapebuilders/shapebuilder_boxedge.inc}

begin
  if (Image.Width < 1) or (Image.Height < 1) then
    Exit;
  Drawer.Init(Image);
  _BuildBoxEdge(Box, Round(Thickness), Drawer.Clip);
end;

procedure SimbaImage_DrawEllipse(Image: TSimbaImage; ACenter: TPoint; XRadius, YRadius: Integer; Inverted: Boolean);
var
  Drawer: TDrawer;

  {$define _Row := Drawer.Row}
  {$i shapebuilders/shapebuilder_ellipse.inc}

begin
  Drawer.Init(Image);
  _BuildEllipse(ACenter.X, ACenter.Y, XRadius, YRadius, Inverted, Drawer.Clip);
end;

procedure SimbaImage_DrawEllipseEdge(Image: TSimbaImage; ACenter: TPoint; XRadius, YRadius: Integer; Thickness: Single);
var
  Drawer: TDrawer;

  {$define _Row := Drawer.Row}
  {$define _Pixel := Drawer.Pixel}
  {$i shapebuilders/shapebuilder_ellipseedge.inc}

begin
  if (XRadius < 1) or (YRadius < 1) then
    Exit;
  Drawer.Init(Image);
  _BuildEllipseEdge(ACenter.X, ACenter.Y, XRadius, YRadius, Round(Thickness), Drawer.Clip);
end;

procedure SimbaImage_DrawPolygon(Image: TSimbaImage; Poly: TPolygon; Inverted: Boolean);
var
  Drawer: TDrawer;

  {$define _Row := Drawer.Row}
  {$i shapebuilders/shapebuilder_polygon.inc}

begin
  Drawer.Init(Image);
  _BuildPolygon(Poly, Inverted, Drawer.Clip);
end;

procedure SimbaImage_DrawPolygonEdge(Image: TSimbaImage; Poly: TPolygon; Thickness: Single);
var
  Drawer: TDrawer;

  {$define _Row := Drawer.Row}
  {$define _Pixel := Drawer.Pixel}
  {$i shapebuilders/shapebuilder_polygonedge.inc}

begin
  if (Length(Poly) < 2) or (Image.Width < 1) or (Image.Height < 1) then
    Exit;
  Drawer.Init(Image, True);
  _BuildPolygonEdge(Poly, Round(Thickness), Drawer.Clip);
  Drawer.Flush;
end;

end.
