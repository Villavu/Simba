{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.image_resizerotate;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.image;

function SimbaImage_RotateNN(Image: TSimbaImage; Radians: Single; Expand: Boolean): TSimbaImage;
function SimbaImage_RotateBilinear(Image: TSimbaImage; Radians: Single; Expand: Boolean): TSimbaImage;
function SimbaImage_Mirror(Image: TSimbaImage; Style: EImageMirrorStyle): TSimbaImage;

implementation

uses
  Math,
  simba.image_utils, simba.vartype_box, simba.vartype_pointarray, simba.geometry;

// The bounds of a W x H image rotated by Angle (radians) about its centre.
function GetRotatedSize(W, H: Integer; Angle: Single): TBox;
var
  B: TPointArray;
begin
  B := [
    TSimbaGeometry.RotatePoint(Point(0, H), Angle, W div 2, H div 2),
    TSimbaGeometry.RotatePoint(Point(W, H), Angle, W div 2, H div 2),
    TSimbaGeometry.RotatePoint(Point(W, 0), Angle, W div 2, H div 2),
    TSimbaGeometry.RotatePoint(Point(0, 0), Angle, W div 2, H div 2)
  ];

  Result := B.Bounds();
end;

function SimbaImage_RotateNN(Image: TSimbaImage; Radians: Single; Expand: Boolean): TSimbaImage;
var
  CosAngle, SinAngle: Single;
  SrcWidth, SrcHeight: Integer;
  NewBounds: TBox;

  // Rotate into an (OutW x OutH) result. (OffX,OffY) is the output canvas's top-left in source space -
  // (0,0) for no-expand, NewBounds.X1/Y1 for expand
  procedure Sample(OutW, OutH, OffX, OffY: Integer);
  var
    Y, OldX, OldY: Integer;
    MidX, MidY, sX, sY: Double; // source position; increments by (Cos,Sin) per output column
    SrcPtr, DstPtr, DstPixel, RowEnd: PColorBGRA;
  begin
    Result.SetSize(OutW, OutH);
    SrcPtr := Image.Data;
    DstPtr := Result.Data;

    MidX := (SrcWidth - 1) / 2;
    MidY := (SrcHeight - 1) / 2;

    for Y := 0 to OutH - 1 do
    begin
      sX := MidX + CosAngle * (OffX - MidX) - SinAngle * (OffY + Y - MidY); // source X,Y at output X=0
      sY := MidY + SinAngle * (OffX - MidX) + CosAngle * (OffY + Y - MidY);
      DstPixel := DstPtr + Y * OutW;
      RowEnd := DstPixel + OutW;
      while (DstPixel < RowEnd) do
      begin
        OldX := Round(sX);
        OldY := Round(sY);
        if (OldX >= 0) and (OldX < SrcWidth) and (OldY >= 0) and (OldY < SrcHeight) then
          DstPixel^.AsInteger := SrcPtr[OldY * SrcWidth + OldX].AsInteger or $FF000000; // opaque, like every rotate

        Inc(DstPixel);
        sX := sX + CosAngle;
        sY := sY + SinAngle;
      end;
    end;
  end;

begin
  Result := TSimbaImage.Create();

  SinCos(Radians, SinAngle, CosAngle);
  SrcWidth := Image.Width;
  SrcHeight := Image.Height;

  if Expand then
  begin
    NewBounds := GetRotatedSize(SrcWidth, SrcHeight, Radians);
    Sample(NewBounds.Width - 1, NewBounds.Height - 1, NewBounds.X1, NewBounds.Y1);
  end
  else
    Sample(SrcWidth, SrcHeight, 0, 0);
end;

function SimbaImage_RotateBilinear(Image: TSimbaImage; Radians: Single; Expand: Boolean): TSimbaImage;
var
  CosAngle, SinAngle: Single;
  SrcWidth, SrcHeight: Integer;
  NewBounds: TBox;

  // Rotate into an (OutW x OutH) result. (OffX,OffY) is the output canvas's top-left in source space -
  // (0,0) for no-expand, NewBounds.X1/Y1 for expand
  procedure Sample(OutW, OutH, OffX, OffY: Integer);
  var
    Y, fX, fY, cX, cY: Integer;
    MidX, MidY, sX, sY, OldX, OldY, dX, dY, dxMinus1, dyMinus1: Double;
    p0, p1, p2, p3: TColorBGRA;
    topR, topG, topB, BtmR, btmG, btmB: Double;
    SrcPtr, DstPtr, DstPixel, RowEnd: PColorBGRA;
  begin
    Result.SetSize(OutW, OutH);
    SrcPtr := Image.Data;
    DstPtr := Result.Data;

    MidX := (OutW - 1) / 2;
    MidY := (OutH - 1) / 2;

    for Y := 0 to OutH - 1 do
    begin
      sX := MidX - CosAngle * MidX - SinAngle * (Y - MidY);
      sY := MidY - SinAngle * MidX + CosAngle * (Y - MidY);
      DstPixel := DstPtr + Y * OutW;
      RowEnd := DstPixel + OutW;
      while (DstPixel < RowEnd) do
      begin
        OldX := sX;
        OldY := sY;

        fX := Trunc(OldX) + OffX;
        fY := Trunc(OldY) + OffY;
        cX := Ceil(OldX)  + OffX;
        cY := Ceil(OldY)  + OffY;

        if (fX >= 0) and (cX >= 0) and (fX < SrcWidth) and (cX < SrcWidth) and
           (fY >= 0) and (cY >= 0) and (fY < SrcHeight) and (cY < SrcHeight) then
        begin
          dx := OldX - (fX - OffX);
          dy := OldY - (fY - OffY);
          dxMinus1 := 1 - dx;
          dyMinus1 := 1 - dy;

          p0 := SrcPtr[fY * SrcWidth + fX];
          p1 := SrcPtr[fY * SrcWidth + cX];
          p2 := SrcPtr[cY * SrcWidth + fX];
          p3 := SrcPtr[cY * SrcWidth + cX];

          TopR := dxMinus1 * p0.R + dx * p1.R;
          TopG := dxMinus1 * p0.G + dx * p1.G;
          TopB := dxMinus1 * p0.B + dx * p1.B;
          BtmR := dxMinus1 * p2.R + dx * p3.R;
          BtmG := dxMinus1 * p2.G + dx * p3.G;
          BtmB := dxMinus1 * p2.B + dx * p3.B;

          with DstPixel^ do
          begin
            R := EnsureRange(Round(dyMinus1 * TopR + dy * BtmR), 0, 255);
            G := EnsureRange(Round(dyMinus1 * TopG + dy * BtmG), 0, 255);
            B := EnsureRange(Round(dyMinus1 * TopB + dy * BtmB), 0, 255);
            A := ALPHA_OPAQUE;
          end;
        end;

        Inc(DstPixel);
        sX := sX + CosAngle;
        sY := sY + SinAngle;
      end;
    end;
  end;

begin
  Result := TSimbaImage.Create();

  SinCos(Radians, SinAngle, CosAngle);
  SrcWidth := Image.Width;
  SrcHeight := Image.Height;

  if Expand then
  begin
    NewBounds := GetRotatedSize(SrcWidth, SrcHeight, Radians);
    Sample(NewBounds.Width - 1, NewBounds.Height - 1, NewBounds.X1, NewBounds.Y1);
  end
  else
    Sample(SrcWidth, SrcHeight, 0, 0);
end;

function SimbaImage_Mirror(Image: TSimbaImage; Style: EImageMirrorStyle): TSimbaImage;
var
  Y, W, H: Integer;
  Src, Dst, SrcPtr, DstPtr, RowEnd: PColorBGRA;
begin
  Result := nil;
  W := Image.Width;
  H := Image.Height;
  Src := Image.Data;

  case Style of
    EImageMirrorStyle.WIDTH:
      begin
        Result := TSimbaImage.Create(W, H);
        Dst := Result.Data;

        for Y := 0 to H - 1 do
        begin
          DstPtr := Dst + Y * W;
          RowEnd := DstPtr + W;
          SrcPtr := Src + Y * W + (W - 1);
          while (DstPtr < RowEnd) do
          begin
            DstPtr^ := SrcPtr^;

            Inc(DstPtr);
            Dec(SrcPtr);
          end;
        end;
      end;

    EImageMirrorStyle.HEIGHT:
      begin
        Result := TSimbaImage.Create(W, H);
        Dst := Result.Data;

        CopyRows(Dst, Result.BytesPerRow, Image.PixelPtr[0, H - 1], -Image.BytesPerRow, W, H);
      end;

    EImageMirrorStyle.LINE:
      begin
        Result := TSimbaImage.Create(H, W);
        Dst := Result.Data;

        for Y := 0 to H - 1 do
        begin
          SrcPtr := Src + Y * W;
          RowEnd := SrcPtr + W;
          DstPtr := Dst + Y;
          while (SrcPtr < RowEnd) do
          begin
            DstPtr^ := SrcPtr^;

            Inc(SrcPtr);
            Inc(DstPtr, H);
          end;
        end;
      end;

    else
      SimbaException('TImage.Mirror: Unknown style (%d)', [Ord(Style)]);
  end;
end;

end.

