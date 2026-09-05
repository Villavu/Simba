{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.image_resizerotate;

{$DEFINE SIMBA_MAX_OPTIMIZATION}
{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.image;

function SimbaImage_RotateNN(Image: TSimbaImage; Radians: Single; Expand: Boolean): TSimbaImage;
function SimbaImage_RotateBilinear(Image: TSimbaImage; Radians: Single; Expand: Boolean): TSimbaImage;

implementation

uses
  Math,
  simba.image_utils, simba.vartype_box;

function SimbaImage_RotateNN(Image: TSimbaImage; Radians: Single; Expand: Boolean): TSimbaImage;
var
  CosAngle, SinAngle: Single;
  SrcWidth, SrcHeight: Integer;
  NewBounds: TBox;

  // Rotate into an (OutW x OutH) result. (OffX,OffY) is the output canvas's top-left in source space -
  // (0,0) for no-expand, NewBounds.X1/Y1 for expand - folded straight into the per-row sample seed.
  // MidX/MidY are the SOURCE centre in both cases; the expand offset is the only difference.
  procedure Sample(OutW, OutH, OffX, OffY: Integer);
  var
    X, Y, OldX, OldY: Integer;
    MidX, MidY, sX, sY: Double; // source position; increments by (Cos,Sin) per output column
    SrcPtr, DstPtr: PColorBGRA;
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
      for X := 0 to OutW - 1 do
      begin
        OldX := Round(sX);
        OldY := Round(sY);
        if (OldX >= 0) and (OldX < SrcWidth) and (OldY >= 0) and (OldY < SrcHeight) then
          DstPtr[Y * OutW + X] := SrcPtr[OldY * SrcWidth + OldX];
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
  // (0,0) for no-expand, NewBounds.X1/Y1 for expand - added to the four sample indices (the fractional
  // weights dx/dy are unaffected). MidX/MidY are the OUTPUT centre in both cases.
  procedure Sample(OutW, OutH, OffX, OffY: Integer);
  var
    X, Y, fX, fY, cX, cY: Integer;
    MidX, MidY, sX, sY, OldX, OldY, dX, dY, dxMinus1, dyMinus1: Double;
    p0, p1, p2, p3: TColorBGRA;
    topR, topG, topB, BtmR, btmG, btmB: Double;
    SrcPtr, DstPtr: PColorBGRA;
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
      for X := 0 to OutW - 1 do
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

          with DstPtr[Y * OutW + X] do
          begin
            R := EnsureRange(Round(dyMinus1 * TopR + dy * BtmR), 0, 255);
            G := EnsureRange(Round(dyMinus1 * TopG + dy * BtmG), 0, 255);
            B := EnsureRange(Round(dyMinus1 * TopB + dy * BtmB), 0, 255);
            A := ALPHA_OPAQUE;
          end;
        end;

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

end.

