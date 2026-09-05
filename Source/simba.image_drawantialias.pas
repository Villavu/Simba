{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Anti-aliased shape drawing for TSimbaImage
}
unit simba.image_drawantialias;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Math,
  simba.base,
  simba.image,
  simba.vartype_polygon;

procedure SimbaImage_DrawTPAAA(Image: TSimbaImage; TPA: TPointArray; Thickness: Single; Feather: Single);
procedure SimbaImage_DrawLineAA(Image: TSimbaImage; Start, Stop: TPoint; Thickness: Single; Feather: Single);

procedure SimbaImage_DrawBoxAA(Image: TSimbaImage; Box: TBox; Feather: Single; Inverted: Boolean);
procedure SimbaImage_DrawBoxEdgeAA(Image: TSimbaImage; Box: TBox; Thickness: Single; Feather: Single);

procedure SimbaImage_DrawEllipseAA(Image: TSimbaImage; ACenter: TPoint; XRadius, YRadius: Integer; Feather: Single; Inverted: Boolean);
procedure SimbaImage_DrawEllipseEdgeAA(Image: TSimbaImage; ACenter: TPoint; XRadius, YRadius: Integer; Thickness: Single; Feather: Single);

procedure SimbaImage_DrawPolygonAA(Image: TSimbaImage; Poly: TPolygon; Feather: Single; Inverted: Boolean);
procedure SimbaImage_DrawPolygonEdgeAA(Image: TSimbaImage; Poly: TPolygon; Thickness: Single; Feather: Single);

implementation

uses
  simba.image_utils, simba.math,
  simba.vartype_box, simba.vartype_pointarray;

function StrokeMargin(Thickness, Feather: Single): Integer; inline;
begin
  Result := Ceil((Thickness + 2 * Max(Feather, 0) * (Thickness * 0.5 + 0.5)) / 2) + 1;
end;

type
  TDrawerAA = record
  private
    FData: PColorBGRA;
    FImgW, FImgH: Integer;
    FColor: TColorBGRA;
    FUseAlpha: Boolean;
    FBand: TBox;
  public
    procedure Init(Image: TSimbaImage);
    procedure Row(Y, X1, X2: Integer; Alpha: Byte); inline;
    procedure Pixel(const X, Y: Integer; Alpha: Byte); inline;
    property Band: TBox read FBand;   // the whole image
  end;

  TBufferedDrawerAA = record
  private
    FData: PColorBGRA;
    FImgW, FImgH: Integer;
    FColor: TColorBGRA;
    FRegion: TBox;
    FWindowWidth: Integer;
    FWindowHeight: Integer;
    FWindowCount: Integer;
    FStartY: Integer;
    FEndY: Integer;
    FAlphaWindow: array[0..65535] of Byte;
    function GetBand: TBox; inline;
  public
    procedure Init(Image: TSimbaImage; Bounds: TBox; Margin: Integer);
    procedure Pixel(const X, Y: Integer; Alpha: Byte); inline;
    procedure Flush;
    property WindowCount: Integer read FWindowCount;
    property Band: TBox read GetBand;
  end;

procedure TDrawerAA.Init(Image: TSimbaImage);
begin
  FData     := Image.Data;
  FImgW     := Image.Width;
  FImgH     := Image.Height;
  FColor    := Image.DrawColorAsBGRA;
  FUseAlpha := (FColor.A < 255);
  FBand     := TBox.Create(0, 0, FImgW - 1, FImgH - 1);
end;

procedure TDrawerAA.Row(Y, X1, X2: Integer; Alpha: Byte);
var
  Ptr, PEnd: PColorBGRA;
  Col: TColorBGRA;
begin
  if (UInt32(Y) >= UInt32(FImgH)) or (Alpha <= 0) then
    Exit;
  X1 := Max(X1, 0);
  X2 := Min(X2, FImgW - 1);
  if (X1 > X2) then
    Exit;

  Ptr := @FData[Int64(Y) * FImgW + X1];
  if (Alpha >= 255) and (not FUseAlpha) then
    FillData(Ptr, X2 - X1 + 1, FColor)
  else
  begin
    Col := FColor;
    if (Alpha < 255) then
      Col.A := Alpha * FColor.A div 255;   // fold the run alpha into the colour alpha (255 when opaque)
    PEnd := @Ptr[X2 - X1];
    while (PtrUInt(Ptr) <= PtrUInt(PEnd)) do
    begin
      BlendPixel(Ptr, @Col);
      Inc(Ptr);
    end;
  end;
end;

procedure TDrawerAA.Pixel(const X, Y: Integer; Alpha: Byte);
var
  A: Byte;
  Col: TColorBGRA;
begin
  Assert((UInt32(X) < UInt32(FImgW)) and (UInt32(Y) < UInt32(FImgH)));
  A := Alpha;
  if FUseAlpha then
    A := A * FColor.A div 255;
  if (A >= 255) then
    FData[Int64(Y) * FImgW + X] := FColor
  else if (A > 0) then
  begin
    Col := FColor;
    Col.A := A;
    BlendPixel(@FData[Int64(Y) * FImgW + X], @Col);
  end;
end;

procedure TBufferedDrawerAA.Init(Image: TSimbaImage; Bounds: TBox; Margin: Integer);
begin
  FData        := Image.Data;
  FImgW        := Image.Width;
  FImgH        := Image.Height;
  FColor       := Image.DrawColorAsBGRA;
  FWindowCount := 0;
  if (FImgW <= 0) or (FImgH <= 0) then
    Exit;

  // Bounds grown by Margin, within the image (Int64: bounds at the Integer limits must not wrap); empty = off-screen
  FRegion := TBox.Create(Max(0, Int64(Bounds.X1) - Margin), Max(0, Int64(Bounds.Y1) - Margin), Min(FImgW - 1, Int64(Bounds.X2) + Margin), Min(FImgH - 1, Int64(Bounds.Y2) + Margin));
  if (FRegion.X1 > FRegion.X2) or (FRegion.Y1 > FRegion.Y2) then
    Exit;
  FWindowWidth := FRegion.Width;
  if (FWindowWidth > Length(FAlphaWindow)) then
    Exit;

  FWindowHeight := Min(FRegion.Height, Max(1, Length(FAlphaWindow) div FWindowWidth));
  FWindowCount  := (FRegion.Height - 1) div FWindowHeight + 1;
  FStartY       := FRegion.Y1;
  FEndY         := Min(FStartY + FWindowHeight - 1, FRegion.Y2);
  FillChar(FAlphaWindow[0], FWindowWidth * FWindowHeight, 0);
end;

function TBufferedDrawerAA.GetBand: TBox;
begin
  Result := TBox.Create(FRegion.X1, FStartY, FRegion.X2, FEndY);
end;

procedure TBufferedDrawerAA.Pixel(const X, Y: Integer; Alpha: Byte);
var
  P: PByte;
begin
  Assert((UInt32(X - FRegion.X1) < UInt32(FWindowWidth)) and (UInt32(Y - FStartY) < UInt32(FEndY - FStartY + 1)));
  P := @FAlphaWindow[(Y - FStartY) * FWindowWidth + (X - FRegion.X1)];
  if (Alpha > P^) then
    P^ := Alpha;
end;

procedure TBufferedDrawerAA.Flush;
var
  Alpha: Byte;
  Y: Integer;
  RowBase: Int64;
  AlphaPtr, AlphaStart, AlphaEnd, BlockEnd: PByte;
  Col: TColorBGRA;
begin
  Col := FColor;
  AlphaPtr := @FAlphaWindow[0];
  for Y := FStartY to FEndY do
  begin
    RowBase    := Int64(Y) * FImgW + FRegion.X1;
    AlphaStart := AlphaPtr;
    AlphaEnd   := AlphaPtr + FWindowWidth;

    while (AlphaPtr < AlphaEnd) do
      if (AlphaPtr <= AlphaEnd - 8) and (PUInt64(AlphaPtr)^ = 0) then
        Inc(AlphaPtr, 8)
      else
      begin
        BlockEnd := AlphaPtr + 8;
        if (BlockEnd > AlphaEnd) then
          BlockEnd := AlphaEnd;

        repeat
          Alpha := AlphaPtr^;
          if (Alpha > 0) then
          begin
            AlphaPtr^ := 0;
            if (FColor.A < 255) then
              Alpha := Alpha * FColor.A div 255;
            Col.A := Alpha;
            BlendPixel(@FData[RowBase + (AlphaPtr - AlphaStart)], @Col);
          end;
          Inc(AlphaPtr);
        until (AlphaPtr >= BlockEnd);
      end;
  end;

  FStartY := FEndY + 1;
  FEndY   := Min(FStartY + FWindowHeight - 1, FRegion.Y2);
end;

procedure SimbaImage_DrawTPAAA(Image: TSimbaImage; TPA: TPointArray; Thickness: Single; Feather: Single);
var
  Drawer: TBufferedDrawerAA;
  Window: Integer;

  {$define _Pixel := Drawer.Pixel}
  {$i shapebuilders/shapebuilder_pointsaa.inc}

begin
  if (Length(TPA) = 0) or (not (Thickness > 0)) then
    Exit;
  Drawer.Init(Image, TPA.Bounds, StrokeMargin(Thickness, Feather));
  for Window := 0 to Drawer.WindowCount - 1 do
  begin
    _BuildPointsAA(TPA, Thickness, Feather, Drawer.Band);
    Drawer.Flush;
  end;
end;

procedure SimbaImage_DrawLineAA(Image: TSimbaImage; Start, Stop: TPoint; Thickness: Single; Feather: Single);
var
  Drawer: TDrawerAA;

  {$define _Pixel := Drawer.Pixel}
  {$i shapebuilders/shapebuilder_lineaa.inc}

begin
  if (not (Thickness > 0)) or (Image.Width < 1) or (Image.Height < 1) then
    Exit;
  Drawer.Init(Image);
  _BuildLineAA(Start.X, Start.Y, Stop.X, Stop.Y, Thickness, Feather, Drawer.Band);
end;

procedure SimbaImage_DrawBoxAA(Image: TSimbaImage; Box: TBox; Feather: Single; Inverted: Boolean);
var
  Drawer: TDrawerAA;

  {$define _Row := Drawer.Row}
  {$define _Pixel := Drawer.Pixel}
  {$i shapebuilders/shapebuilder_boxaa.inc}

begin
  if (Image.Width < 1) or (Image.Height < 1) then
    Exit;
  Drawer.Init(Image);
  _BuildBoxAA(Box, Feather, Inverted, Drawer.Band);
end;

procedure SimbaImage_DrawBoxEdgeAA(Image: TSimbaImage; Box: TBox; Thickness: Single; Feather: Single);
var
  Drawer: TDrawerAA;

  {$define _Pixel := Drawer.Pixel}
  {$i shapebuilders/shapebuilder_boxedgeaa.inc}

begin
  if (not (Thickness > 0)) or (Image.Width < 1) or (Image.Height < 1) then
    Exit;
  Drawer.Init(Image);
  _BuildBoxEdgeAA(Box, Thickness, Feather, Drawer.Band);
end;

procedure SimbaImage_DrawEllipseAA(Image: TSimbaImage; ACenter: TPoint; XRadius, YRadius: Integer; Feather: Single; Inverted: Boolean);
var
  Drawer: TDrawerAA;

  {$define _Row := Drawer.Row}
  {$define _Pixel := Drawer.Pixel}
  {$i shapebuilders/shapebuilder_ellipseaa.inc}

begin
  if (Image.Width < 1) or (Image.Height < 1) then
    Exit;
  Drawer.Init(Image);
  _BuildEllipseAA(ACenter.X, ACenter.Y, XRadius, YRadius, Feather, Inverted, Drawer.Band);
end;

procedure SimbaImage_DrawEllipseEdgeAA(Image: TSimbaImage; ACenter: TPoint; XRadius, YRadius: Integer; Thickness: Single; Feather: Single);
var
  Drawer: TDrawerAA;

  {$define _Pixel := Drawer.Pixel}
  {$i shapebuilders/shapebuilder_ellipseedgeaa.inc}

begin
  if (not (Thickness > 0)) or (Image.Width < 1) or (Image.Height < 1) then
    Exit;
  Drawer.Init(Image);
  _BuildEllipseEdgeAA(ACenter.X, ACenter.Y, XRadius, YRadius, Thickness, Feather, Drawer.Band);
end;

procedure SimbaImage_DrawPolygonAA(Image: TSimbaImage; Poly: TPolygon; Feather: Single; Inverted: Boolean);
var
  Drawer: TDrawerAA;

  {$define _Row := Drawer.Row}
  {$define _Pixel := Drawer.Pixel}
  {$i shapebuilders/shapebuilder_polygonaa.inc}

begin
  if (Length(Poly) < 3) or (Image.Width < 1) or (Image.Height < 1) then
    Exit;
  Drawer.Init(Image);
  _BuildPolygonAA(Poly, Feather, Inverted, Drawer.Band);
end;

procedure SimbaImage_DrawPolygonEdgeAA(Image: TSimbaImage; Poly: TPolygon; Thickness: Single; Feather: Single);
var
  Drawer: TDrawerAA;

  {$define _Pixel := Drawer.Pixel}
  {$i shapebuilders/shapebuilder_polygonedgeaa.inc}

begin
  if (Length(Poly) < 3) or (not (Thickness > 0)) or (Image.Width < 1) or (Image.Height < 1) then
    Exit;
  Drawer.Init(Image);
  _BuildPolygonEdgeAA(Poly, Thickness, Feather, Drawer.Band);
end;

end.
