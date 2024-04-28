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

function SimbaImage_Downsample(Image: TSimbaImage; Scale: Integer): TSimbaImage; overload;
function SimbaImage_Downsample(Image: TSimbaImage; Scale: Integer; IgnorePoints: TPointArray): TSimbaImage; overload;

function SimbaImage_ResizeNN(Image: TSimbaImage; NewWidth, NewHeight: Integer): TSimbaImage;
function SimbaImage_ResizeBilinear(Image: TSimbaImage; NewWidth, NewHeight: Integer): TSimbaImage;

function SimbaImage_RotateNN(Image: TSimbaImage; Radians: Single; Expand: Boolean): TSimbaImage;
function SimbaImage_RotateBilinear(Image: TSimbaImage; Radians: Single; Expand: Boolean): TSimbaImage;

implementation

uses
  Math,
  simba.image_utils, simba.vartype_box;

function SimbaImage_Downsample(Image: TSimbaImage; Scale: Integer): TSimbaImage;
var
  Area: Double;

  function BlendArea(const X1, Y1, X2, Y2: Integer): TColorBGRA;
  var
    R, G, B: Integer;
    Hit, Miss: Integer;
    X, Y: Integer;
    Color: TColorBGRA;
  begin
    Miss := 0;
    Hit := 0;

    R := 0;
    G := 0;
    B := 0;

    for X := X1 to X2 do
      for Y := Y1 to Y2 do
      begin
        Color := Image.Data[Y * Image.Width + X];

        Inc(Hit);

        Inc(R, Color.R);
        Inc(G, Color.G);
        Inc(B, Color.B);
      end;

    Result.R := Round((R + (R div Hit) * Miss) * Area);
    Result.G := Round((G + (G div Hit) * Miss) * Area);
    Result.B := Round((B + (B div Hit) * Miss) * Area);
    Result.A := ALPHA_OPAQUE;
  end;

var
  X, Y, W, H: Integer;
  OldX, OldY: Integer;
begin
  Result := TSimbaImage.Create(Image.Width div Scale, Image.Height div Scale);

  Area := Double(1.0) / Sqr(Scale);

  W := Result.Width - 1;
  H := Result.Height - 1;

  for Y := 0 to H do
    for X := 0 to W do
    begin
      OldX := X * Scale;
      OldY := Y * Scale;

      Result.Data[Y * Result.Width + X] := BlendArea(OldX, OldY, (OldX + Scale) - 1, (OldY + Scale) - 1);
    end;
end;

function SimbaImage_Downsample(Image: TSimbaImage; Scale: Integer; IgnorePoints: TPointArray): TSimbaImage;
var
  Skip: TBooleanArray;
  Area: Double;

  function BlendArea(const X1, Y1, X2, Y2: Integer): TColorBGRA;
  var
    SumR, SumG, SumB: UInt64;
    Hit, Miss: Integer;
    X, Y: Integer;
  begin
    Miss := 0;
    Hit := 0;

    SumR := 0;
    SumG := 0;
    SumB := 0;

    for X := X1 to X2 do
      for Y := Y1 to Y2 do
      begin
        if Skip[Y * Image.Width + X] then
          Inc(Miss)
        else
        begin
          with Image.Data[Y * Image.Width + X] do
          begin
            Inc(SumR, R);
            Inc(SumG, G);
            Inc(SumB, B);
            Inc(Hit);
          end;
        end;
      end;

    Result.R := Round((SumR + (SumR div Hit) * Miss) * Area);
    Result.G := Round((SumG + (SumG div Hit) * Miss) * Area);
    Result.B := Round((SumB + (SumB div Hit) * Miss) * Area);
    Result.A := ALPHA_OPAQUE;
  end;

var
  X, Y, W, H: Integer;
  OldX, OldY: Integer;
  P: TPoint;
begin
  Result := TSimbaImage.Create(Image.Width div Scale, Image.Height div Scale);

  SetLength(Skip, Image.Width*Image.Height);
  for P in IgnorePoints do
    Skip[P.Y * Image.Width + P.X] := True;

  Area := Double(1.0) / Sqr(Scale);

  W := Result.Width - 1;
  H := Result.Height - 1;

  for Y := 0 to H do
    for X := 0 to W do
    begin
      OldX := X * Scale;
      OldY := Y * Scale;

      // todo
      if Skip[OldY * Image.Width + OldX] then
        Result.Data[Y * Result.Width + X] := Image.Data[OldY * Image.Width + OldX]
      else
        Result.Data[Y * Result.Width + X] := BlendArea(OldX, OldY, (OldX + Scale) - 1, (OldY + Scale) - 1);
    end;
end;

function SimbaImage_ResizeNN(Image: TSimbaImage; NewWidth, NewHeight: Integer): TSimbaImage;
var
  X, Y, W, H: Integer;
begin
  Result := TSimbaImage.Create(NewWidth, NewHeight);

  W := NewWidth - 1;
  H := NewHeight - 1;

  for Y := 0 to H do
    for X := 0 to W do
      Result.Data[Y * NewWidth + X] := Image.Data[((Y * Image.Height) div NewHeight) * Image.Width + (X * Image.Width) div NewWidth];
end;

function SimbaImage_ResizeBilinear(Image: TSimbaImage; NewWidth, NewHeight: Integer): TSimbaImage;
var
  X, Y, OldX, OldY: Integer;
  p0, p1, p2, p3: TColorBGRA;
  RatioX, RatioY, dX, dY: Single;
  W,H: Integer;
begin
  Result := TSimbaImage.Create(NewWidth, NewHeight);

  RatioX := (Image.Width - 1) / NewWidth;
  RatioY := (Image.Height - 1) / NewHeight;

  W := NewWidth - 1;
  H := NewHeight - 1;
  for Y := 0 to H do
    for X := 0 to W do
    begin
      OldX := Trunc(RatioX * X);
      OldY := Trunc(RatioY * Y);

      p0 := Image.LineStarts[OldY,     OldX    ];
      p1 := Image.LineStarts[OldY,     OldX + 1];
      p2 := Image.LineStarts[OldY + 1, OldX    ];
      p3 := Image.LineStarts[OldY + 1, OldX + 1];

      dX := ratioX * X - OldX;
      dY := ratioY * Y - OldY;

      with Result.Data[Y * NewWidth + X] do
      begin
        R := Trunc(
          p0.R * (1-dX) * (1-dY) +
          p1.R * (dX * (1-dY)) +
          p2.R * (dY * (1-dX)) +
          p3.R * (dX * dY)
        );

        G := Trunc(
          p0.G * (1-dX) * (1-dY) +
          p1.G * (dX * (1-dY)) +
          p2.G * (dY * (1-dX)) +
          p3.G * (dX * dY)
        );

        B := Trunc(
          p0.B * (1-dX) * (1-dY) +
          p1.B * (dX * (1-dY)) +
          p2.B * (dY * (1-dX)) +
          p3.B * (dX * dY)
        );

        A := ALPHA_OPAQUE;
      end;
    end;
end;

function SimbaImage_RotateNN(Image: TSimbaImage; Radians: Single; Expand: Boolean): TSimbaImage;
var
  CosAngle, SinAngle: Single;

  procedure RotateNoExpand;
  var
    X, Y, OldX, OldY, W, H: Integer;
    MidX, MidY: Single;
  begin
    Result.SetSize(Image.Width, Image.Height);

    MidX := (Image.Width - 1) / 2;
    MidY := (Image.Height - 1) / 2;

    W := Image.Width - 1;
    H := Image.Height - 1;
    for Y := 0 to H do
      for X := 0 to W do
      begin
        OldX := Round(MidX + CosAngle * (X - MidX) - SinAngle * (Y - MidY));
        OldY := Round(MidY + SinAngle * (X - MidX) + CosAngle * (Y - MidY));
        if (OldX >= 0) and (OldX < Image.Width) and (OldY >= 0) and (OldY < Image.Height) then
          Result.Data[Y * Image.Width + X] := Image.Data[OldY * Image.Width + OldX];
      end;
  end;

  procedure RotateExpand;
  var
    X, Y, OldX, OldY, NewWidth, NewHeight: Integer;
    MidX, MidY: Single;
    NewBounds: TBox;
  begin
    MidX := (Image.Width - 1) / 2;
    MidY := (Image.Height - 1) / 2;

    NewBounds := GetRotatedSize(Image.Width, Image.Height, Radians);

    NewWidth := NewBounds.Width - 1;
    NewHeight := NewBounds.Height - 1;

    Result.SetSize(NewWidth, NewHeight);

    Dec(NewWidth);
    Dec(NewHeight);
    for Y := 0 to NewHeight do
      for X := 0 to NewWidth do
      begin
        OldX := Round(MidX + CosAngle * (NewBounds.X1+X - MidX) - SinAngle * (NewBounds.Y1+Y - MidY));
        OldY := Round(MidY + SinAngle * (NewBounds.X1+X - MidX) + CosAngle * (NewBounds.Y1+Y - MidY));
        if (OldX >= 0) and (OldX < Image.Width) and (OldY >= 0) and (OldY < Image.Height) then
          Result.Data[Y * Result.Width + X] := Image.Data[OldY * Image.Width + OldX];
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
    x, y, w, h: Integer;
    OldX, OldY: Single;
    dX, dY, dxMinus1, dyMinus1: Single;
    p0, p1, p2, p3: TColorBGRA;
    topR, topG, topB, BtmR, btmG, btmB: Single;
    fX, fY, cX, cY: Integer;
    MidX, MidY: Single;
  begin
    Result.SetSize(Image.Width, Image.Height);

    MidX := (Image.Width - 1) / 2;
    MidY := (Image.Height - 1) / 2;

    W := Image.Width - 1;
    H := Image.Height - 1;
    for Y := 0 to H do
      for X := 0 to W do
      begin
        OldX := (MidX + CosAngle * (X - MidX) - SinAngle * (Y - MidY));
        OldY := (MidY + SinAngle * (X - MidX) + CosAngle * (Y - MidY));

        fX := Trunc(OldX);
        fY := Trunc(OldY);
        cX := Ceil(OldX);
        cY := Ceil(OldY);

        if (fX >= 0) and (cX >= 0) and (fX < Image.Width) and (cX < Image.Width) and
           (fY >= 0) and (cY >= 0) and (fY < Image.Height) and (cY < Image.Height) then
        begin
          dx := OldX - fX;
          dy := OldY - fY;
          dxMinus1 := 1 - dx;
          dyMinus1 := 1 - dy;

          p0 := Image.Data[fY * Image.Width + fX];
          p1 := Image.Data[fY * Image.Width + cX];
          p2 := Image.Data[cY * Image.Width + fX];
          p3 := Image.Data[cY * Image.Width + cX];

          TopR := dxMinus1 * p0.R + dx * p1.R;
          TopG := dxMinus1 * p0.G + dx * p1.G;
          TopB := dxMinus1 * p0.B + dx * p1.B;
          BtmR := dxMinus1 * p2.R + dx * p3.R;
          BtmG := dxMinus1 * p2.G + dx * p3.G;
          BtmB := dxMinus1 * p2.B + dx * p3.B;

          with Result.Data[Y * Result.Width + X] do
          begin
            R := EnsureRange(Round(dyMinus1 * TopR + dy * BtmR), 0, 255);
            G := EnsureRange(Round(dyMinus1 * TopG + dy * BtmG), 0, 255);
            B := EnsureRange(Round(dyMinus1 * TopB + dy * BtmB), 0, 255);
            A := ALPHA_OPAQUE;
          end;
        end;
      end;
  end;

  procedure RotateExpand;
  var
    NewWidth, NewHeight, X, Y: Integer;
    NewBounds: TBox;
    OldX, OldY: Single;
    dX, dY, dxMinus1, dyMinus1: Single;
    p0, p1, p2, p3: TColorBGRA;
    topR, topG, topB, BtmR, btmG, btmB: Single;
    fX, fY, cX, cY: Integer;
    MidX, MidY: Single;
  begin
    NewBounds := GetRotatedSize(Image.Width, Image.Height, Radians);
    NewWidth := NewBounds.Width - 1;
    NewHeight := NewBounds.Height - 1;
    MidX := (NewWidth - 1) / 2;
    MidY := (NewHeight - 1) / 2;

    Result.SetSize(NewWidth, NewHeight);

    Dec(NewWidth);
    Dec(NewHeight);
    for Y := 0 to NewHeight do
      for X := 0 to NewWidth do
      begin
        OldX := (MidX + CosAngle * (X - MidX) - SinAngle * (Y - MidY));
        OldY := (MidY + SinAngle * (X - MidX) + CosAngle * (Y - MidY));

        fX := Trunc(OldX) + NewBounds.X1;
        fY := Trunc(OldY) + NewBounds.Y1;
        cX := Ceil(OldX)  + NewBounds.X1;
        cY := Ceil(OldY)  + NewBounds.Y1;

        if (fX >= 0) and (cX >= 0) and (fX < Image.Width) and (cX < Image.Width) and
           (fY >= 0) and (cY >= 0) and (fY < Image.Height) and (cY < Image.Height) then
        begin
          dx := OldX - (fX - NewBounds.X1);
          dy := OldY - (fY - NewBounds.Y1);
          dxMinus1 := 1 - dx;
          dyMinus1 := 1 - dy;

          p0 := Image.Data[fY * Image.Width + fX];
          p1 := Image.Data[fY * Image.Width + cX];
          p2 := Image.Data[cY * Image.Width + fX];
          p3 := Image.Data[cY * Image.Width + cX];

          TopR := dxMinus1 * p0.R + dx * p1.R;
          TopG := dxMinus1 * p0.G + dx * p1.G;
          TopB := dxMinus1 * p0.B + dx * p1.B;
          BtmR := dxMinus1 * p2.R + dx * p3.R;
          BtmG := dxMinus1 * p2.G + dx * p3.G;
          BtmB := dxMinus1 * p2.B + dx * p3.B;

          with Result.Data[Y * Result.Width + X] do
          begin
            R := EnsureRange(Round(dyMinus1 * TopR + dy * BtmR), 0, 255);
            G := EnsureRange(Round(dyMinus1 * TopG + dy * BtmG), 0, 255);
            B := EnsureRange(Round(dyMinus1 * TopB + dy * BtmB), 0, 255);
            A := ALPHA_OPAQUE;
          end;
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

