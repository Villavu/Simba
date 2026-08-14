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

  procedure RotateNoExpand;
  var
    X, Y, OldX, OldY, W, H, SrcWidth, SrcHeight: Integer;
    MidX, MidY, sX, sY: Double; // source position; increments by (Cos,Sin) per output column
    SrcPtr, DstPtr: PColorBGRA;
  begin
    SrcWidth := Image.Width;
    SrcHeight := Image.Height;
    Result.SetSize(SrcWidth, SrcHeight);
    SrcPtr := Image.Data;
    DstPtr := Result.Data;

    MidX := (SrcWidth - 1) / 2;
    MidY := (SrcHeight - 1) / 2;

    W := SrcWidth - 1;
    H := SrcHeight - 1;
    for Y := 0 to H do
    begin
      sX := MidX - CosAngle * MidX - SinAngle * (Y - MidY); // source X,Y at output X=0
      sY := MidY - SinAngle * MidX + CosAngle * (Y - MidY);
      for X := 0 to W do
      begin
        OldX := Round(sX);
        OldY := Round(sY);
        if (OldX >= 0) and (OldX < SrcWidth) and (OldY >= 0) and (OldY < SrcHeight) then
          DstPtr[Y * SrcWidth + X] := SrcPtr[OldY * SrcWidth + OldX];
        sX := sX + CosAngle;
        sY := sY + SinAngle;
      end;
    end;
  end;

  procedure RotateExpand;
  var
    X, Y, OldX, OldY, NewWidth, NewHeight, SrcWidth, SrcHeight, DstWidth: Integer;
    MidX, MidY, sX, sY: Double;
    NewBounds: TBox;
    SrcPtr, DstPtr: PColorBGRA;
  begin
    SrcWidth := Image.Width;
    SrcHeight := Image.Height;
    MidX := (SrcWidth - 1) / 2;
    MidY := (SrcHeight - 1) / 2;

    NewBounds := GetRotatedSize(SrcWidth, SrcHeight, Radians);

    NewWidth := NewBounds.Width - 1;
    NewHeight := NewBounds.Height - 1;

    Result.SetSize(NewWidth, NewHeight);
    SrcPtr := Image.Data;
    DstPtr := Result.Data;
    DstWidth := Result.Width;

    Dec(NewWidth);
    Dec(NewHeight);
    for Y := 0 to NewHeight do
    begin
      sX := MidX + CosAngle * (NewBounds.X1 - MidX) - SinAngle * (NewBounds.Y1 + Y - MidY);
      sY := MidY + SinAngle * (NewBounds.X1 - MidX) + CosAngle * (NewBounds.Y1 + Y - MidY);
      for X := 0 to NewWidth do
      begin
        OldX := Round(sX);
        OldY := Round(sY);
        if (OldX >= 0) and (OldX < SrcWidth) and (OldY >= 0) and (OldY < SrcHeight) then
          DstPtr[Y * DstWidth + X] := SrcPtr[OldY * SrcWidth + OldX];
        sX := sX + CosAngle;
        sY := sY + SinAngle;
      end;
    end;
  end;

begin
  Result := TSimbaImage.Create();

  SinCos(Radians, SinAngle, CosAngle);

  case Expand of
    True:  RotateExpand();
    False: RotateNoExpand();
  end;
end;

function SimbaImage_RotateBilinear(Image: TSimbaImage; Radians: Single; Expand: Boolean): TSimbaImage;
var
  CosAngle, SinAngle: Single;

  procedure RotateNoExpand;
  var
    X, Y, W, H, SrcWidth, SrcHeight, fX, fY, cX, cY: Integer;
    MidX, MidY, sX, sY, OldX, OldY, dX, dY, dxMinus1, dyMinus1: Double;
    p0, p1, p2, p3: TColorBGRA;
    topR, topG, topB, BtmR, btmG, btmB: Double;
    SrcPtr, DstPtr: PColorBGRA;
  begin
    SrcWidth := Image.Width;
    SrcHeight := Image.Height;
    Result.SetSize(SrcWidth, SrcHeight);
    SrcPtr := Image.Data;
    DstPtr := Result.Data;

    MidX := (SrcWidth - 1) / 2;
    MidY := (SrcHeight - 1) / 2;

    W := SrcWidth - 1;
    H := SrcHeight - 1;
    for Y := 0 to H do
    begin
      sX := MidX - CosAngle * MidX - SinAngle * (Y - MidY);
      sY := MidY - SinAngle * MidX + CosAngle * (Y - MidY);
      for X := 0 to W do
      begin
        OldX := sX;
        OldY := sY;

        fX := Trunc(OldX);
        fY := Trunc(OldY);
        cX := Ceil(OldX);
        cY := Ceil(OldY);

        if (fX >= 0) and (cX >= 0) and (fX < SrcWidth) and (cX < SrcWidth) and
           (fY >= 0) and (cY >= 0) and (fY < SrcHeight) and (cY < SrcHeight) then
        begin
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

          with DstPtr[Y * SrcWidth + X] do
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

  procedure RotateExpand;
  var
    NewWidth, NewHeight, X, Y, SrcWidth, SrcHeight, DstWidth, fX, fY, cX, cY: Integer;
    MidX, MidY, sX, sY, OldX, OldY, dX, dY, dxMinus1, dyMinus1: Double;
    NewBounds: TBox;
    p0, p1, p2, p3: TColorBGRA;
    topR, topG, topB, BtmR, btmG, btmB: Double;
    SrcPtr, DstPtr: PColorBGRA;
  begin
    SrcWidth := Image.Width;
    SrcHeight := Image.Height;
    NewBounds := GetRotatedSize(SrcWidth, SrcHeight, Radians);
    NewWidth := NewBounds.Width - 1;
    NewHeight := NewBounds.Height - 1;
    MidX := (NewWidth - 1) / 2;
    MidY := (NewHeight - 1) / 2;

    Result.SetSize(NewWidth, NewHeight);
    SrcPtr := Image.Data;
    DstPtr := Result.Data;
    DstWidth := Result.Width;

    Dec(NewWidth);
    Dec(NewHeight);
    for Y := 0 to NewHeight do
    begin
      sX := MidX - CosAngle * MidX - SinAngle * (Y - MidY);
      sY := MidY - SinAngle * MidX + CosAngle * (Y - MidY);
      for X := 0 to NewWidth do
      begin
        OldX := sX;
        OldY := sY;

        fX := Trunc(OldX) + NewBounds.X1;
        fY := Trunc(OldY) + NewBounds.Y1;
        cX := Ceil(OldX)  + NewBounds.X1;
        cY := Ceil(OldY)  + NewBounds.Y1;

        if (fX >= 0) and (cX >= 0) and (fX < SrcWidth) and (cX < SrcWidth) and
           (fY >= 0) and (cY >= 0) and (fY < SrcHeight) and (cY < SrcHeight) then
        begin
          dx := OldX - (fX - NewBounds.X1);
          dy := OldY - (fY - NewBounds.Y1);
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

          with DstPtr[Y * DstWidth + X] do
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

  case Expand of
    True:  RotateExpand();
    False: RotateNoExpand();
  end;
end;

end.

