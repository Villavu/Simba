{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.colorfinders;

{$i simba.inc}

{$IFNDEF SIMBA_HAS_DEBUG_INFO}
  {$OPTIMIZATION LEVEL4}
{$ENDIF}

interface

uses
  classes, sysutils,
  simba.mufasatypes;

{
function TMFinder.FindColorsTolerance(out Points: TPointArray; Color, xs, ys, xe, ye, Tol: Integer): Boolean;
var
  Data: TRetData;
  Buffer: TFindColorBuffer;
begin
  if GetData(Data, xs, ys, xe, ye) then
  begin
    Ptr := Data.Ptr;
    PtrInc := Data.IncPtrWith;
    X1 := xs;
    Y1 := ys;
    X2 := xe;
    Y2 := ye;

    Result := FindCTS2(Points, Color, Tol, Self.hueMod, Self.satMod);
  end else
    Result := False;
end;
}

type
  TFindColorBuffer = record
    Ptr: PRGB32;
    PtrInc: Integer;

    X1, Y1, X2, Y2: Integer;

    function Find(out Points: TPointArray; const Color: Integer): Boolean;
    function FindCTS0(out Points: TPointArray; const Color, Tolerance: Integer): Boolean;
    function FindCTS1(out Points: TPointArray; const Color, Tolerance: Integer): Boolean;
    function FindCTS2(out Points: TPointArray; const Color, Tolerance: Integer; const HueMod, SatMod: Extended): Boolean;
  end;

implementation

uses
  math,
  simba.colormath, simba.overallocatearray;

var
  Percentage: array[0..255] of Single;

function TFindColorBuffer.Find(out Points: TPointArray; const Color: Integer): Boolean;
var
  GoalR, GoalG, GoalB: Integer;

  function Match(const Color: TRGB32): Boolean; inline;
  begin
    Result := (GoalR = Color.R) and (GoalG = Color.G) and (GoalB = Color.B);
  end;

var
  X, Y: Integer;
  PointBuffer: specialize TSimbaOverAllocateArray<TPoint>;
begin
  PointBuffer.Init(4096);

  ColorToRGB(Color, GoalR, GoalG, GoalB);

  for Y := Y1 to Y2 do
  begin
    for X := X1 to X2 do
    begin
      if Match(Ptr^) then
        PointBuffer.Add(Point(X, Y));

      Inc(Ptr);
    end;

    Inc(Ptr, PtrInc);
  end;

  Points := PointBuffer.Trim();

  Result := Length(Points) > 0;
end;

// Future note: Having CTS0 & CTS1 seems pretty pointless.
function TFindColorBuffer.FindCTS0(out Points: TPointArray; const Color, Tolerance: Integer): Boolean;
var
  GoalR, GoalG, GoalB: Integer;
  GoalTolerance: Integer;

  function Match(const Color: TRGB32): Boolean; inline;
  begin
    Result := (Abs(GoalR - Color.R) <= GoalTolerance) and
              (Abs(GoalG - Color.G) <= GoalTolerance) and
              (Abs(GoalB - Color.B) <= GoalTolerance);
  end;

var
  X, Y: Integer;
  PointBuffer: specialize TSimbaOverAllocateArray<TPoint>;
begin
  PointBuffer.Init(4096);

  ColorToRGB(Color, GoalR, GoalG, GoalB);

  GoalTolerance := Sqr(Tolerance);

  for Y := Y1 to Y2 do
  begin
    for X := X1 to X2 do
    begin
      if Match(Ptr^) then
        PointBuffer.Add(Point(X, Y));

      Inc(Ptr);
    end;

    Inc(Ptr, PtrInc);
  end;

  Points := PointBuffer.Trim();

  Result := Length(Points) > 0;
end;

function TFindColorBuffer.FindCTS1(out Points: TPointArray; const Color, Tolerance: Integer): Boolean;
var
  GoalR, GoalG, GoalB: Integer;
  GoalTolerance: Integer;

  function Match(const Color: TRGB32): Boolean; inline;
  begin
    Result := Sqr(GoalR - Color.R) + Sqr(GoalG - Color.G) + Sqr(GoalB - Color.B) <= GoalTolerance;
  end;

var
  X, Y: Integer;
  PointBuffer: specialize TSimbaOverAllocateArray<TPoint>;
begin
  PointBuffer.Init(4096);

  ColorToRGB(Color, GoalR, GoalG, GoalB);

  GoalTolerance := Sqr(Tolerance);

  for Y := Y1 to Y2 do
  begin
    for X := X1 to X2 do
    begin
      if Match(Ptr^) then
        PointBuffer.Add(Point(X, Y));

      Inc(Ptr);
    end;

    Inc(Ptr, PtrInc);
  end;

  Points := PointBuffer.Trim();

  Result := Length(Points) > 0;
end;

function TFindColorBuffer.FindCTS2(out Points: TPointArray; const Color, Tolerance: Integer; const HueMod, SatMod: Extended): Boolean;
var
  GoalH, GoalS, GoalL: Single;
  GoalHueMod, GoalSatMod: Single;
  GoalTolerance: Single;

  function Match(const Color: TRGB32): Boolean; inline;
  var
    R, G, B: Single;
    H, S, L: Single;
    CMin, CMax, D: Single;
  begin
    R := Percentage[Color.R];
    G := Percentage[Color.G];
    B := Percentage[Color.B];

    CMin := R;
    CMax := R;
    if G < CMin then CMin := G;
    if B < CMin then CMin := B;
    if G > CMax then CMax := G;
    if B > CMax then CMax := B;

    L := 0.5 * (CMax + CMin);

    if Abs(L-GoalL) > GoalTolerance then
      Exit(False);
    if (CMax = CMin) then
      Exit(GoalS <= GoalSatMod);

    D := CMax - CMin;
    if (L < 0.5) then
      S := D / (CMax + CMin)
    else
      S := D / (2 - CMax - CMin);

    if Abs(S - GoalS) > GoalSatMod then
      Exit(False);

    if (R = CMax) then
      H := (G - B) / D
    else
    if (G = CMax) then
      H  := 2 + (B - R) / D
    else
      H := 4 + (R - G) / D;

    H := H / 6;
    if (H < 0) then
      H := H + 1;
    H := H * 100;

    if (H > GoalH) then
      Result := Min(H - GoalH, Abs(H - (GoalH + 100))) <= GoalHueMod
    else
      Result := Min(GoalH - H, Abs(GoalH - (H + 100))) <= GoalHueMod;
  end;

var
  X, Y: Integer;
  PointBuffer: specialize TSimbaOverAllocateArray<TPoint>;
begin
  PointBuffer.Init(4096);
  with RGBToBGR(Color) do
    RGBToHSL(R, G, B, GoalH, GoalS, GoalL);

  GoalHueMod    := (Tolerance * HueMod);
  GoalSatMod    := (Tolerance * SatMod) / 100;
  GoalTolerance := (Tolerance / 100);

  GoalL := GoalL / 100;
  GoalS := GoalS / 100;

  for Y := Y1 to Y2 do
  begin
    for X := X1 to X2 do
    begin
      if Match(Ptr^) then
        PointBuffer.Add(Point(X, Y));

      Inc(Ptr);
    end;

    Inc(Ptr, PtrInc);
  end;

  Points := PointBuffer.Trim();

  Result := Length(Points) > 0;
end;

procedure LoadPercentages;
var
  I: Integer;
begin
  for I := 0 to 255 do
    Percentage[I] := I / 255;
end;

initialization
  LoadPercentages();

end.

