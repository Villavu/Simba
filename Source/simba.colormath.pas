{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.colormath;

{$DEFINE SIMBA_O4}
{$i simba.inc}

interface

uses
  classes, sysutils, graphics,
  simba.mufasatypes;

function RGBDistance(const Color, OtherColor: TRGB32): Integer; inline;

function BGRToRGB(BGR: TRGB32): TColor; inline;
function RGBToBGR(const Color: TColor): TRGB32; inline;
function RGBToBGR(const R, G, B: Integer): TRGB32; inline;
function RGBToColor(r,g,b: Byte): TColor; overload; inline;
function RGBToColor(r,g,b: Integer): TColor; overload; inline;
procedure ColorToRGB(Color: Integer;out r,g,b: Byte); overload; inline;
procedure ColorToRGB(Color: Integer;out r,g,b: Integer); overload; inline;
procedure RGBToXYZ(R,G,B: Byte;out x,y,z: Extended); inline;
procedure XYZToRGB(X,Y,Z: Extended;out R,G,B: Byte); overload; inline;
procedure XYZToRGB(X,Y,Z: Extended;out R,G,B: Integer); overload;
procedure RGBToHSL(RR,GG,BB: Byte;out H,S,L: Extended); overload;
procedure RGBToHSL(R,G,B: Byte; out H,S,L: Single); overload;
procedure RGBToHSLNonFixed(RR,GG,BB: Byte;out H,S,L: Extended); inline;
procedure HSLtoRGB(H,S,L: extended;out R,G,B: Byte); inline;
procedure ColorToHSL(Col: Integer; out h, s, l: Extended);
procedure ColorToXYZ(color: Integer; out X, Y, Z: Extended);
function XYZToColor(X, Y, Z: Extended): TColor;
function HSLToColor(H, S, L: Extended): TColor;
procedure XYZToHSL(X, Y, Z: Extended; out H, S, L: Extended);
procedure HSLToXYZ(H, S, L: Extended; out X, Y, Z: Extended);
procedure XYZtoCIELab(X, Y, Z: Extended; out L, a, b: Extended);
procedure CIELabtoXYZ(L, a, b: Extended; out X, Y, Z: Extended);
procedure CIELabToRGB(L, a, b: Extended; out rr, gg, bb: Byte); overload;
procedure CIELabToRGB(L, a, b: Extended; out rr, gg, bb: Integer); overload;
procedure RGBToCIELab(rr, gg, bb: Byte; out L, a, b: Extended); overload;
procedure RGBToCIELab(rr, gg, bb: Integer; out L, a, b: Extended); overload;
function CIELabToColor(L, a, b: Extended): TColor;
procedure ColorToCIELab(Color: Integer; out L, a, b: Extended);
procedure CIELabToHSL(L, a, b: Extended; out HH, SS, LL: Extended);
procedure HSLToCIELab(HH, SS, LL: Extended; out L, a, b: Extended);
function ColorToGray(const Color: Integer): Byte; inline;

procedure BestColor_CTS0(Colors: TIntegerArray; out Color, Tolerance: Int32);
procedure BestColor_CTS1(Colors: TIntegerArray; out Color, Tolerance: Int32);
procedure BestColor_CTS2(Colors: TIntegerArray; out Color, Tolerance: Int32; out Hue, Sat: Extended);

implementation

uses
  math,
  simba.math;

const
  OneDivThree = 1 / 3.0;
  TwoDivThree = 2 / 3.0;
  OneDivTwoPointFour = 1 / 2.4;

function RGBDistance(const Color, OtherColor: TRGB32): Integer;
begin
  Result := Sqr(Color.R - OtherColor.R) + Sqr(Color.G - OtherColor.G) + Sqr(Color.B - OtherColor.B);
end;

function BGRToRGB(BGR: TRGB32): TColor;
begin
  Result := BGR.R or BGR.g shl 8 or BGR.b shl 16;
end;

function RGBToBGR(const Color: TColor): TRGB32;
begin
  Result.R := Color and $ff;
  Result.G := Color shr 8 and $ff;
  Result.B := Color shr 16 and $ff;
  Result.A := 0;
end;

function RGBToBGR(const R, G, B: Integer): TRGB32;
begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
  Result.A := 0;
end;

function RGBToColor(r,g,b: Byte): TColor;
begin
  Result := R or g shl 8 or b shl 16;
end;

{/\
  Translates the given Red (R), Green (G) and Blue (B) components to a TColor.
  R, G and B are integers.
/\}

function RGBToColor(r,g,b: Integer): TColor;
begin
  Result := R or g shl 8 or b shl 16;
end;

{/\
   Translates the given win-32 color in the Red (R), Green (G) and Blue (B)
   components. R, G and B are bytes.
/\}

procedure ColorToRGB(Color: Integer;out r,g,b: Byte);
begin
  R := Color and $ff;
  G := Color shr 8 and $ff;
  B := Color shr 16 and $ff;
end;

{/\
   Translates the given win-32 color in the Red (R), Green (G) and Blue (B)
   components. R, G and B are integers.
/\}

procedure ColorToRGB(Color: Integer;out r,g,b: Integer);
begin
  R := Color and $ff;
  G := Color shr 8 and $ff;
  B := Color shr 16 and $ff;
end;

{/\
   Translates the given Red (R), Green (G) and Blue (B) components to
   X, Y and Z components.
/\}

procedure RGBToXYZ(R,G,B: Byte;out x,y,z: Extended);
var
  Red,Green,Blue: Extended;
begin
  Red := R / 255;
  Green := G / 255;
  Blue := B / 255;
  if Red > 0.04045  then
    Red := Power( ( Red + 0.055 ) / 1.055  , 2.4) * 100
  else
    Red := Red * 7.73993808;
  if Green > 0.04045  then
    Green := Power( ( Green + 0.055 ) / 1.055 , 2.4) *  100
  else
    Green := Green * 7.73993808;
  if  Blue > 0.04045 then
    Blue := Power(  ( Blue + 0.055 ) / 1.055  , 2.4) * 100
  else
    Blue := Blue * 7.73993808;
  X := Red * 0.4124 + Green * 0.3576 + Blue * 0.1805;
  Y := Red * 0.2126 + Green * 0.7152 + Blue * 0.0722;
  Z := Red * 0.0193 + Green * 0.1192 + Blue * 0.9505;
end;
{/\
   Translates the given X, Y and Z components to
   Red (R), Green (G) and Blue (B) components.
/\}

procedure XYZToRGB(X,Y,Z: Extended;out R,G,B: Byte);
var
   TempR,TempG,TempB,Tempx,tempy,tempz: Extended;
begin
  Tempx := X / 100;
  tempy := Y / 100;
  tempz := Z / 100;
  TempR := tempx *  3.2406 + tempy * -1.5372 + tempz * -0.4986;
  TempG := tempx * -0.9689 + tempy *  1.8758 + tempz *  0.0415;
  TempB := tempx *  0.0557 + tempy * -0.2040 + tempz *  1.0570;
  if TempR > 0.0031308  then
    TempR := 1.055 * ( Power(TempR, (OneDivTwoPointFour)) ) - 0.055
  else
    TempR := 12.92 * TempR;
  if TempG > 0.0031308 then
    TempG := 1.055 * ( Power(TempG, ( OneDivTwoPointFour)) ) - 0.055
  else
    TempG := 12.92 * TempG;
  if  TempB > 0.0031308 then
    TempB := 1.055 * ( Power(TempB , ( OneDivTwoPointFour )) ) - 0.055
  else
    TempB := 12.92 * TempB;
  R := Round(TempR * 255);
  G := Round(TempG * 255);
  B := Round(TempB * 255);
end;

procedure XYZToRGB(X,Y,Z: Extended;out R,G,B: Integer);
var
  TempR,TempG,TempB: Extended;
begin
  TempR := x *  3.2406 + y * -1.5372 + z * -0.4986;
  TempG := x * -0.9689 + y *  1.8758 + z *  0.0415;
  TempB := x *  0.0557 + y * -0.2040 + z *  1.0570;
  if TempR > 0.0031308  then
    TempR := 1.055 * ( Power(TempR, (OneDivTwoPointFour)) ) - 0.055
  else
    TempR := 12.92 * TempR;
  if TempG > 0.0031308 then
    TempG := 1.055 * ( Power(TempG, ( OneDivTwoPointFour)) ) - 0.055
  else
    TempG := 12.92 * TempG;
  if  TempB > 0.0031308 then
    TempB := 1.055 * ( Power(TempB , ( OneDivTwoPointFour )) ) - 0.055
  else
    TempB := 12.92 * TempB;
  R := Round(TempR * 255);
  G := Round(TempG * 255);
  B := Round(TempB * 255);
end;

{/\
   Translates the given Red (R), Green (G) and Blue (B) components to
   H (Hue), S (Saturation) and L (Luminance) components.
/\}

procedure RGBToHSL(RR,GG,BB: Byte;out H,S,L: Extended);
var
  R,  G,  B,   D,  Cmax, Cmin: Extended;
begin
  R := RR / 255;
  G := GG / 255;
  B := BB / 255;
  CMin := R;
  if G < Cmin then Cmin := G;
  if B  < Cmin then Cmin := B;
  CMax := R;
  if G > Cmax then Cmax := G;
  if B  > Cmax then Cmax := B;
  L := 0.5 * (Cmax + Cmin);
  if Cmax = Cmin then
  begin
    H := 0;
    S := 0;
  end else
  begin
    D := Cmax - Cmin;
    if L < 0.5 then
      S := D / (Cmax + Cmin)
    else
      S := D / (2 - Cmax - Cmin);
    if R = Cmax then
      H := (G - B) / D
    else
      if G = Cmax then
        H  := 2 + (B - R) / D
      else
        H := 4 +  (R - G) / D;
    H := H / 6;
    if H < 0 then
      H := H + 1;
  end;
  H := H * 100;
  S := S * 100;
  L := L * 100;
end;

procedure RGBToHSL(R,G,B: Byte; out H,S,L: Single);
var
  HH, SS, LL: Extended;
begin
  RGBToHSL(R, G, B, HH, SS, LL);

  H := HH;
  S := SS;
  L := LL;
end;

{/\
   Translates the given Red (R), Green (G) and Blue (B) components to
   H (Hue), S (Saturation) and L (Luminance) components.
   This function does not multiply it by 100.
/\}

procedure RGBToHSLNonFixed(RR,GG,BB: Byte;out H,S,L: Extended);
var
  R,  G,  B,   D,  Cmax, Cmin: Extended;
begin
  R := RR / 255;
  G := GG / 255;
  B := BB / 255;
  CMin := R;
  if G < Cmin then Cmin := G;
  if B  < Cmin then Cmin := B;
  CMax := R;
  if G > Cmax then Cmax := G;
  if B  > Cmax then Cmax := B;
  L := 0.5 * (Cmax + Cmin);
  if Cmax = Cmin then
  begin
    H := 0;
    S := 0;
  end else
  begin
    D := Cmax - Cmin;
    if L < 0.5 then
      S := D / (Cmax + Cmin)
    else
      S := D / (2 - Cmax - Cmin);
    if R = Cmax then
      H := (G - B) / D
    else
      if G = Cmax then
        H  := 2 + (B - R) / D
      else
        H := 4 +  (R - G) / D;
    H := H / 6;
    if H < 0 then
      H := H + 1;
  end;
end;

{/\
   Translates the given H (Hue), S (Saturation) and L (Luminance) components to
   Red (R), Green (G) and Blue (B) components.
/\}

procedure HSLtoRGB(H, S, L: extended; out R, G, B: Byte);
var
  Temp,Temp2: Extended;

  function Hue2RGB(TempHue: Extended): Integer;
  begin
    if TempHue < 0 then
      TempHue := TempHue + 1
    else if TempHue > 1 then
      TempHue := TempHue - 1;
    if ( ( 6 * TempHue ) < 1 ) then
      Result :=Round(255 * (( Temp + ( Temp2 - Temp ) * 6 * TempHue )))
    else if ( ( 2 * TempHue ) < 1 ) then
      Result :=Round(255 * Temp2)
    else if ( ( 3 * TempHue ) < 2 ) then
      Result :=Round(255 * (Temp + ( Temp2 - Temp ) * ( ( TwoDivThree ) - TempHue ) * 6))
    else
      Result :=Round(255 * Temp);
  end;

begin
  H := H / 100;
  S := S / 100;
  L := L / 100;
  if s = 0 then
  begin
    R := Byte(Round(L * 255));
    G := R;
    B := R;
  end else
  begin
    if (L < 0.5) then
      Temp2 := L * ( 1 + S )
    else
      Temp2 := (L + S) - ( S * L);
    Temp := 2 * L - Temp2;
    R := Hue2RGB( H + ( OneDivThree ) );
    G := Hue2RGB( H );
    B := Hue2RGB( H - ( OneDivThree ) );
  end;
end;

{/\
  Split the Given Color col in H, S, L components.
/\}

procedure ColorToHSL(Col: Integer; out h, s, l: Extended);
var
  R, G, B: Byte;
Begin
  ColorToRGB(Col, R, G, B);
  RGBToHSL(R, G, B, H, S, L);
End;

procedure ColorToXYZ(color: Integer; out X, Y, Z: Extended);
var
  R, G, B: Byte;
begin
  ColorToRGB(Color, R, G, B);
  RGBToXYZ(R, G, B, X, Y, Z);
end;

function HSLToColor(H, S, L: Extended): TColor;
var
  R, G, B: Byte;
begin
  HSLToRGB(H, S, L, R, G, B);
  Result := RGBToColor(R, G, B);
end;

function XYZToColor(X, Y, Z: Extended): TColor;
var
  R, G, B: Byte;
begin
  XYZToRGB(X, Y, Z, R, G, B);
  Result := RGBToColor(R, G, B);
end;

procedure XYZToHSL(X, Y, Z: Extended; out H, S, L: Extended);
var
  R, G, B: Byte;
begin
  XYZToRGB(X, Y, Z, R, G, B);
  RGBToHSL(R, G, B, H, S, L);
end;

procedure HSLToXYZ(H, S, L: Extended; out X, Y, Z: Extended);
var
  R, G, B: Byte;
begin
  HSLToRGB(H, S, L, R, G, B);
  RGBToXYZ(R, G, B, X, Y, Z);
end;

procedure XYZtoCIELab(X, Y, Z: Extended; out L, a, b: Extended);
begin
  X := X / 95.047;
  Y := Y / 100.000;
  Z := Z / 108.883;

  if ( X > 0.008856 ) then
    X := Power(X, 1.0/3.0)
  else
    X := ( 7.787 * X ) + ( 16.0 / 116.0 );
  if ( Y > 0.008856 ) then
    Y := Power(Y, 1.0/3.0)
  else
    Y := ( 7.787 * Y ) + ( 16.0 / 116.0 );
  if ( Z > 0.008856 ) then
    Z := Power(Z, 1.0/3.0)
  else
    Z := ( 7.787 * Z ) + ( 16.0 / 116.0 );

  L := (116.0 * Y ) - 16.0;
  a := 500.0 * ( X - Y );
  b := 200.0 * ( Y - Z );
end;

procedure CIELabtoXYZ(L, a, b: Extended; out X, Y, Z: Extended);
begin
  Y := ( L + 16 ) / 116.0;
  X := ( a / 500.0 )+ Y;
  Z := Y - ( b / 200.0 );

  if ( Power(Y, 3) > 0.008856 ) then
    Y := Power(Y, 3)
  else
    Y := ( Y - (16.0 / 116.0 )) / 7.787;
  if ( Power(X, 3) > 0.008856 ) then
    X := Power(X, 3)
  else
    X := ( X - (16.0 / 116.0) ) / 7.787;
  if ( Power(Z, 3) > 0.008856 ) then
    Z := Power(Z, 3)
  else
    Z := ( Z - (16.0 / 116.0) ) / 7.787;

  X := 95.047 * X;
  Y := 100.000 * Y;
  Z := 108.883 * Z;
end;

procedure CIELabToRGB(L, a, b: Extended; out rr, gg, bb: Byte);
var
  X, Y, Z: Extended;
begin
  CIELabToXYZ(L, a, b, X, Y, Z);
  XYZToRGB(X, Y, Z, rr, gg, bb);
end;

procedure CIELabToRGB(L, a, b: Extended; out rr, gg, bb: Integer);
var
  X, Y, Z: Extended;
begin
  CIELabToXYZ(L, a, b, X, Y, Z);
  XYZToRGB(X, Y, Z, rr, gg, bb);
end;

procedure RGBToCIELab(rr, gg, bb: Byte; out L, a, b: Extended);
var
  X, Y, Z: Extended;
begin
  RGBToXYZ(rr, gg, bb, X, Y, Z);
  XYZtoCIELab(X, Y, Z, L, a, b);
end;

procedure RGBToCIELab(rr, gg, bb: Integer; out L, a, b: Extended);
var
  X, Y, Z: Extended;
begin
  RGBToXYZ(rr, gg, bb, X, Y, Z);
  XYZtoCIELab(X, Y, Z, L, a, b);
end;

function CIELabToColor(L, a, b: Extended): TColor;
var
  X, Y, Z: Extended;
begin
  CIELabToXYZ(L, a, b, X, Y, Z);
  Result := XYZToColor(X, Y, Z);
end;

procedure ColorToCIELab(Color: Integer; out L, a, b: Extended);
var
  X, Y, Z: Extended;
begin
  ColorToXYZ(Color, X, Y, Z);
  XYZtoCIELab(X, Y, Z, L, a, b);
end;

procedure CIELabToHSL(L, a, b: Extended; out HH, SS, LL: Extended);
var
  rr, gg, bb: Byte;
begin
  CIELabToRGB(L, a, b, rr, gg, bb);
  RGBToHSL(rr, gg, bb, HH, SS, LL);
end;

procedure HSLToCIELab(HH, SS, LL: Extended; out L, a, b: Extended);
var
  rr, gg, bb: Byte;
begin
  HSLtoRGB(HH, SS, LL, rr, gg, bb);
  RGBToCIELab(rr, gg, bb, L, a, b);
end;

function ColorToGray(const Color: Integer): Byte;
begin
  Result := ((Color and $FF) + ((Color shr 8) and $FF) + ((Color shr 16) and $FF)) div 3;
end;

procedure BestColor_CTS0(Colors: TIntegerArray; out Color, Tolerance: Int32);
type
  TRGBCube = record R1, G1, B1, R2, G2, B2: Byte; end;

  function RGBCube(R1, G1, B1, R2, G2, B2: Byte): TRGBCube; overload;
  begin
    Result.R1 := R1;
    Result.G1 := G1;
    Result.B1 := B1;
    Result.R2 := R2;
    Result.G2 := G2;
    Result.B2 := B2;
  end;

  function RGBCube(Colors: TIntegerArray): TRGBCube; overload;
  var
    i: Int32;
    R, G, B: Byte;
  begin
    Result := RGBCube(255, 255, 255, 0, 0, 0);

    for i := 0 to High(Colors) do
    begin
      ColorToRGB(Colors[i], R, G, B);

      if R > Result.R2 then Result.R2 := R;
      if R < Result.R1 then Result.R1 := R;
      if G > Result.G2 then Result.G2 := G;
      if G < Result.G1 then Result.G1 := G;
      if B > Result.B2 then Result.B2 := B;
      if B < Result.B1 then Result.B1 := B;
    end;
  end;

var
  Cube: TRGBCube;
  R, G, B: Int32;
begin
  Cube := RGBCube(Colors);

  R := Ceil((Cube.R1 + Cube.R2) / 2);
  G := Ceil((Cube.G1 + Cube.G2) / 2);
  B := Ceil((Cube.B1 + Cube.B2) / 2);

  Color := RGBtoColor(R, G, B);
  Tolerance := Max(Max(Abs(R - Cube.R1), Abs(G - Cube.G1)), Abs(B - Cube.B1));
end;

procedure BestColor_CTS1(Colors: TIntegerArray; out Color, Tolerance: Int32);
type
  TRGBCube = record R1, G1, B1, R2, G2, B2: Byte; end;

  function RGBCube(R1, G1, B1, R2, G2, B2: Byte): TRGBCube; overload;
  begin
    Result.R1 := R1;
    Result.G1 := G1;
    Result.B1 := B1;
    Result.R2 := R2;
    Result.G2 := G2;
    Result.B2 := B2;
  end;

  function RGBCube(Colors: TIntegerArray): TRGBCube; overload;
  var
    i: Int32;
    R, G, B: Byte;
  begin
    Result := RGBCube(255, 255, 255, 0, 0, 0);

    for i := 0 to High(Colors) do
    begin
      ColorToRGB(Colors[i], R, G, B);

      if R > Result.R2 then Result.R2 := R;
      if R < Result.R1 then Result.R1 := R;
      if G > Result.G2 then Result.G2 := G;
      if G < Result.G1 then Result.G1 := G;
      if B > Result.B2 then Result.B2 := B;
      if B < Result.B1 then Result.B1 := B;
    end;
  end;

var
  Cube: TRGBCube;
  R, G, B: Int32;
begin
  Cube := RGBCube(Colors);

  R := Ceil((Cube.R1 + Cube.R2) / 2);
  G := Ceil((Cube.G1 + Cube.G2) / 2);
  B := Ceil((Cube.B1 + Cube.B2) / 2);

  Color := RGBtoColor(R, G, B);
  Tolerance := Ceil(Sqrt(Sqr(R - Cube.R1) + Sqr(G - Cube.G1) + Sqr(B - Cube.B1)));
end;

procedure BestColor_CTS2(Colors: TIntegerArray; out Color, Tolerance: Int32; out Hue, Sat: Extended);
type
  THSLCylinder = record H1, S1, L1, H2, S2, L2: Extended; end;

  function DeltaHue(DegA, DegB: Double): Double;
  begin
    Result := DegA - DegB;
    while Result < -50 do Result += 100;
    while Result > 50  do Result -= 100;
  end;

  function HSLCylinder(H1, S1, L1, H2, S2, L2: Extended): THSLCylinder; overload;
  begin
    Result.H1 := H1;
    Result.S1 := S1;
    Result.L1 := L1;
    Result.H2 := H2;
    Result.S2 := S2;
    Result.L2 := L2;
  end;

  function HSLCylinder(Colors: TIntegerArray): THSLCylinder; overload;
  var
    H1, S1, L1, H2, S2, L2, Delta, MaxDelta: Extended;
    i, j: Int32;
  begin
    Result := HSLCylinder(100, 100, 100, 0, 0, 0);

    if (Length(Colors) = 1) then
    begin
      ColorToHSL(Colors[0], H1, S1, L1);

      Exit(HSLCylinder(H1, S1, L1, H1, S1, L1));
    end;

    MaxDelta := 0;

    for i := 0 to High(Colors) do
    begin
      ColorToHSL(Colors[i], H1, S1 ,L1);

      for j := i + 1 to High(Colors) do
      begin
        ColorToHSL(Colors[j], H2, S2, L2);
        Delta := Abs(DeltaHue(H1, H2));
        if (Delta > MaxDelta) then
        begin
          Result.H1 := H1;
          Result.H2 := H2;

          MaxDelta := Delta;
        end;
      end;

      if S1 > Result.S2 then Result.S2 := S1;
      if S1 < Result.S1 then Result.S1 := S1;
      if L1 > Result.L2 then Result.L2 := L1;
      if L1 < Result.L1 then Result.L1 := L1;
    end;
  end;

  function GetHue(var H1, H2: Extended; Colors: TIntegerArray): Extended;
  var
    H, S, L, X, Y: Extended;
    i, Tol: Int32;
  begin
    X := Cos(Radians(H1 * 3.6)) + Cos(Radians(H2 * 3.6));
    Y := Sin(Radians(H1 * 3.6)) + Sin(Radians(H2 * 3.6));
    Result := Degrees(ArcTan2(Y / 2, X / 2)) / 3.6;

    for i := 0 to High(Colors) do
    begin
      Tol := Ceil(
        Max(Abs(DeltaHue(Result, H1)), Abs(DeltaHue(Result, H2)))
      );
      ColorToHSL(Colors[i], H, S, L);
      if DeltaHue(H, Result) > +Tol then H2 := H;
      if DeltaHue(H, Result) < -Tol then H1 := H;
    end;
  end;

var
  H, S, L, Tol: Extended;
  Cylinder: THSLCylinder;
begin
  Cylinder := HSLCylinder(Colors);

  L := (Cylinder.L1 + Cylinder.L2) / 2;
  S := (Cylinder.S1 + Cylinder.S2) / 2;
  H := GetHue(Cylinder.H1, Cylinder.H2, Colors);

  Color := HSLToColor(H, S, L);
  ColorToHSL(Color, H, S, L); // fix conversion rounding

  Tol := Max(1, Max(Abs(L - Cylinder.L2), Abs(L - Cylinder.L1)) + 0.1e-10);
  Sat := Max(Abs(S - Cylinder.S2), Abs(S - Cylinder.S1)) / Tol;
  Hue := Max(Abs(DeltaHue(H, Cylinder.H2)), Abs(DeltaHue(H, Cylinder.H1))) / Tol;

  Tolerance := Ceil(Tol);
  Hue := Hue + 0.1e-10;
  Sat := Sat + 0.1e-10;
end;

end.
