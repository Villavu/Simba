unit simba.aca_math;

{$mode objfpc}{$H+}

interface

uses
  simba.mufasatypes;

procedure BestColor_CTS0(Colors: TIntegerArray; out Color, Tolerance: Int32);
procedure BestColor_CTS1(Colors: TIntegerArray; out Color, Tolerance: Int32);
procedure BestColor_CTS2(Colors: TIntegerArray; out Color, Tolerance: Int32; out Hue, Sat: Extended);

implementation

uses
  math,
  simba.colormath, simba.math;

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

  R := Round((Cube.R1 + Cube.R2) / 2);
  G := Round((Cube.G1 + Cube.G2) / 2);
  B := Round((Cube.B1 + Cube.B2) / 2);

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

  R := Round((Cube.R1 + Cube.R2) / 2);
  G := Round((Cube.G1 + Cube.G2) / 2);
  B := Round((Cube.B1 + Cube.B2) / 2);

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

