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

// The size that holds every pixel of a W x H image rotated by Angle (radians) about its centre.
// Measured between pixel centres, so a quarter turn of W x H is exactly H x W.
function GetRotatedSize(W, H: Integer; Angle: Single): TPoint;
var
  CosAngle, SinAngle: Double;
begin
  SinCos(Double(Angle), SinAngle, CosAngle);

  Result.X := Round(Abs(CosAngle) * (W - 1) + Abs(SinAngle) * (H - 1)) + 1;
  Result.Y := Round(Abs(SinAngle) * (W - 1) + Abs(CosAngle) * (H - 1)) + 1;
end;

function SimbaImage_RotateNN(Image: TSimbaImage; Radians: Single; Expand: Boolean): TSimbaImage;
var
  CosAngle, SinAngle: Double;
  SrcWidth, SrcHeight: Integer;

  // Rotate into an (OutW x OutH) result: the centre of the result is the centre of the source
  procedure Sample(OutW, OutH: Integer);
  var
    Y, OldX, OldY: Integer;
    sX, sY: Double; // source position; increments by (Cos,Sin) per output column
    SrcPtr, DstPtr, DstPixel, RowEnd: PColorBGRA;
  begin
    Result.SetSize(OutW, OutH);
    SrcPtr := Image.Data;
    DstPtr := Result.Data;

    for Y := 0 to OutH - 1 do
    begin
      sX := (SrcWidth - 1) / 2 - CosAngle * ((OutW - 1) / 2) - SinAngle * (Y - (OutH - 1) / 2); // source X,Y at output X=0
      sY := (SrcHeight - 1) / 2 - SinAngle * ((OutW - 1) / 2) + CosAngle * (Y - (OutH - 1) / 2);
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

  SinCos(Double(Radians), SinAngle, CosAngle);
  SrcWidth := Image.Width;
  SrcHeight := Image.Height;

  if Expand then
    Sample(GetRotatedSize(SrcWidth, SrcHeight, Radians).X, GetRotatedSize(SrcWidth, SrcHeight, Radians).Y)
  else
    Sample(SrcWidth, SrcHeight);
end;

function SimbaImage_RotateBilinear(Image: TSimbaImage; Radians: Single; Expand: Boolean): TSimbaImage;
var
  CosAngle, SinAngle: Double;
  SrcWidth, SrcHeight: Integer;

  // Rotate into an (OutW x OutH) result: the centre of the result is the centre of the source
  procedure Sample(OutW, OutH: Integer);
  var
    Y, fX, fY, cX, cY: Integer;
    sX, sY, OldX, OldY, dX, dY, dxMinus1, dyMinus1: Double;
    p0, p1, p2, p3: TColorBGRA;
    topR, topG, topB, BtmR, btmG, btmB: Double;
    SrcPtr, DstPtr, DstPixel, RowEnd: PColorBGRA;
  begin
    Result.SetSize(OutW, OutH);
    SrcPtr := Image.Data;
    DstPtr := Result.Data;

    for Y := 0 to OutH - 1 do
    begin
      sX := (SrcWidth - 1) / 2 - CosAngle * ((OutW - 1) / 2) - SinAngle * (Y - (OutH - 1) / 2);
      sY := (SrcHeight - 1) / 2 - SinAngle * ((OutW - 1) / 2) + CosAngle * (Y - (OutH - 1) / 2);
      DstPixel := DstPtr + Y * OutW;
      RowEnd := DstPixel + OutW;
      while (DstPixel < RowEnd) do
      begin
        // within half a pixel of the image, like nearest neighbour: the edge pixels repeat
        if (sX >= -0.5) and (sX <= SrcWidth - 0.5) and (sY >= -0.5) and (sY <= SrcHeight - 0.5) then
        begin
          OldX := sX;
          if (OldX < 0) then
            OldX := 0
          else if (OldX > SrcWidth - 1) then
            OldX := SrcWidth - 1;
          OldY := sY;
          if (OldY < 0) then
            OldY := 0
          else if (OldY > SrcHeight - 1) then
            OldY := SrcHeight - 1;

          fX := Trunc(OldX);
          fY := Trunc(OldY);
          cX := Ceil(OldX);
          cY := Ceil(OldY);

          dx := OldX - fX;
          dy := OldY - fY;
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

  SinCos(Double(Radians), SinAngle, CosAngle);
  SrcWidth := Image.Width;
  SrcHeight := Image.Height;

  if Expand then
    Sample(GetRotatedSize(SrcWidth, SrcHeight, Radians).X, GetRotatedSize(SrcWidth, SrcHeight, Radians).Y)
  else
    Sample(SrcWidth, SrcHeight);
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

