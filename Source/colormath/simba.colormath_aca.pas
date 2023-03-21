{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Color math that finds the ""best"" color and modifiers from a given color array.

  Lots of code from: https://github.com/slackydev/colorlib
}
unit simba.colormath_aca;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics, Math,
  simba.mufasatypes, simba.math, simba.colormath, simba.colormath_distance;

const
  COLOR_FITTING_ACCURACY: Single = 1.0;

type
  TBestColor = record
    Color: TColor;
    Mods: TChannelMultipliers;
    Tolerance: Single;
  end;

function GetBestColor(Formula: EColorSpace; Colors: TColorArray): TBestColor;

implementation

type
  THueRange    = record Low,High: Double; end;
  TColorRange  = record A1,B1,C1, A2,B2,C2: Double; end;

  TSuperColor = record
    A,B,C: Single;

    class operator := (aValue: TColorRGB): TSuperColor;
    class operator := (aValue: TColorXYZ): TSuperColor;
    class operator := (aValue: TColorLAB): TSuperColor;
    class operator := (aValue: TColorLCH): TSuperColor;
    class operator := (aValue: TColorHSV): TSuperColor;
    class operator := (aValue: TColorHSL): TSuperColor;
  end;

class operator TSuperColor.:=(aValue: TColorRGB): TSuperColor;
begin
  Result.A := aValue.R;
  Result.B := aValue.G;
  Result.C := aValue.B;
end;

class operator TSuperColor.:=(aValue: TColorXYZ): TSuperColor;
begin
  Result.A := aValue.X;
  Result.B := aValue.Y;
  Result.C := aValue.Z;
end;

class operator TSuperColor.:=(aValue: TColorLAB): TSuperColor;
begin
  Result.A := aValue.L;
  Result.B := aValue.A;
  Result.C := aValue.B;
end;

class operator TSuperColor.:=(aValue: TColorLCH): TSuperColor;
begin
  Result.A := aValue.L;
  Result.B := aValue.C;
  Result.C := aValue.H;
end;

class operator TSuperColor.:=(aValue: TColorHSV): TSuperColor;
begin
  Result.A := aValue.H;
  Result.B := aValue.S;
  Result.C := aValue.V;
end;

class operator TSuperColor.:=(aValue: TColorHSL): TSuperColor;
begin
  Result.A := aValue.H;
  Result.B := aValue.S;
  Result.C := aValue.L;
end;

function DeltaAngle(DegA, DegB: Double): Double;
begin
  Result := DegA - DegB;
  while Result < -180 do Result += 360;
  while Result > 180  do Result -= 360;
end;

function MeanAngle(Arr: TDoubleArray): Double;
var
  i: Integer;
  x,y: Double;
begin
  x := 0;
  y := 0;
  for i:=0 to High(Arr) do
  begin
    x += Cos(Radians(Arr[i]));
    y += Sin(Radians(Arr[i]));
  end;
  Result := FixD(Degrees(ArcTan2(y / Length(Arr), x / Length(Arr))));
end;

function NormalizeMods(Mods: TChannelMultipliers): TChannelMultipliers;
var x: Double;
begin
  x := (Mods[0] + Mods[1] + Mods[2]) / 3;
  if IsZero(x) then
    Result := Mods
  else
  begin
    Result[0] := Mods[0] / x;
    Result[1] := Mods[1] / x;
    Result[2] := Mods[2] / x;
  end;
end;

function GetHue(Formula: EColorSpace; Colors: TColorArray; var H1,H2: Double): Double;
var
  hue,tol: Single;
  i: Integer;
begin
  Result := MeanAngle([H1, H2]);

  for i := 0 to High(Colors) do
  begin
    hue := 0;
    tol := Max(Abs(DeltaAngle(Result, H1)), Abs(DeltaAngle(Result, H2)));

    case Formula of
      EColorSpace.HSL: hue := Colors[i].ToHSL.H;
      EColorSpace.HSV: hue := Colors[i].ToHSV.H;
      EColorSpace.LCH, EColorSpace.DeltaE: hue := Colors[i].ToLCH.H;
    end;

    if DeltaAngle(hue, Result) < -Tol then
    begin
      H1 := hue;
      //Result := MeanAngle([MeanAngle([H1,Result]), MeanAngle([Result,H2])]);
    end;

    if DeltaAngle(hue, Result) > +Tol then
    begin
      H2 := hue;
      //Result := MeanAngle([MeanAngle([H1,Result]), MeanAngle([Result,H2])]);
    end;
  end;
end;

function ColorRange(Formula: EColorSpace; Colors: TColorArray): TColorRange;
var
  CL: TSuperColor;
  i: Integer;
begin
  if (Length(Colors) = 1) then
  begin
    case Formula of
      EColorSpace.RGB:    CL := Colors[0].ToRGB;
      EColorSpace.XYZ:    CL := Colors[0].ToXYZ;
      EColorSpace.LAB:    CL := Colors[0].ToLAB;
      EColorSpace.HSV:    CL := Colors[0].ToHSV;
      EColorSpace.HSL:    CL := Colors[0].ToHSL;
      EColorSpace.LCH:    CL := Colors[0].ToLCH;
      EColorSpace.DeltaE: CL := Colors[0].ToLCH;
    end;

    Result.A1 := CL.A;
    Result.B1 := CL.B;
    Result.C1 := CL.C;

    Result.A2 := CL.A;
    Result.B2 := CL.B;
    Result.C2 := CL.C;
  end else
  begin
    Result.A1 := 10000;
    Result.B1 := 10000;
    Result.C1 := 10000;

    Result.A2 := -10000;
    Result.B2 := -10000;
    Result.C2 := -10000;

    for i := 0 to High(Colors) do
    begin
      case Formula of
        EColorSpace.RGB:    CL := Colors[i].ToRGB;
        EColorSpace.XYZ:    CL := Colors[i].ToXYZ;
        EColorSpace.LAB:    CL := Colors[i].ToLAB;
        EColorSpace.HSV:    CL := Colors[i].ToHSV;
        EColorSpace.HSL:    CL := Colors[i].ToHSL;
        EColorSpace.LCH:    CL := Colors[i].ToLCH;
        EColorSpace.DeltaE: CL := Colors[i].ToLCH;
      end;

      if (CL.A > Result.A2) then Result.A2 := CL.A;
      if (CL.A < Result.A1) then Result.A1 := CL.A;
      if (CL.B > Result.B2) then Result.B2 := CL.B;
      if (CL.B < Result.B1) then Result.B1 := CL.B;
      if (CL.C > Result.C2) then Result.C2 := CL.C;
      if (CL.C < Result.C1) then Result.C1 := CL.C;
    end;
  end;
end;

function ColorRangeHue(Formula: EColorSpace; Colors: TColorArray): THueRange;
var
  Hue1, Hue2, delta, maxDelta: Double;
  i,j: Integer;
begin
  Hue1 := 0;
  Hue2 := 0;
  Result := Default(THueRange);
  if not (Formula in [EColorSpace.HSL, EColorSpace.HSV, EColorSpace.LCH, EColorSpace.DeltaE]) then
    Exit;

  if (Length(Colors) = 1) then
  begin
    case Formula of
      EColorSpace.HSL:
        begin
          Result.Low  := Colors[0].ToHSL.H;
          Result.High := Colors[0].ToHSL.H;
        end;
      EColorSpace.HSV:
        begin
          Result.Low  := Colors[0].ToHSV.H;
          Result.High := Colors[0].ToHSV.H;
        end;
      EColorSpace.LCH:
        begin
          Result.Low  := Colors[0].ToLCH.H;
          Result.High := Colors[0].ToLCH.H;
        end;
      EColorSpace.DeltaE:
        begin
          Result.Low  := Colors[0].ToLCH.H;
          Result.High := Colors[0].ToLCH.H;
        end;
   end;
  end else
  begin
    maxDelta := -1;
    for i:=0 to High(Colors) do
    begin
      case Formula of
        EColorSpace.HSL: Hue1 := Colors[i].ToHSL.H;
        EColorSpace.HSV: Hue1 := Colors[i].ToHSV.H;
        EColorSpace.LCH, EColorSpace.DeltaE: Hue1 := Colors[i].ToLCH.H;
      end;

      for j:=i+1 to High(Colors) do
      begin
        case Formula of
          EColorSpace.HSL: Hue2 := Colors[j].ToHSL.H;
          EColorSpace.HSV: Hue2 := Colors[j].ToHSV.H;
          EColorSpace.LCH, EColorSpace.DeltaE: Hue2 := Colors[j].ToLCH.H;
        end;

        delta := Abs(DeltaAngle(Hue1, Hue2));
        if delta > maxDelta then
        begin
          Result.Low  := Hue1;
          Result.High := Hue2;
          maxDelta    := delta;
        end;
      end;
    end;
  end;
end;

function GetTolerance(Formula: EColorSpace; Color: TColor; Colors: TColorArray; Mods: TChannelMultipliers): Single;

  function GetDistance(Color1, Color2: TColor): Single;
  begin
    case Formula of
      EColorSpace.RGB:    Result := DistanceRGB(Color1.ToRGB(), Color2.ToRGB(), Mods);
      EColorSpace.XYZ:    Result := DistanceXYZ(Color1.ToXYZ(), Color2.ToXYZ(), Mods);
      EColorSpace.LAB:    Result := DistanceLAB(Color1.ToLAB(), Color2.ToLAB(), Mods);
      EColorSpace.HSV:    Result := DistanceHSV(Color1.ToHSV(), Color2.ToHSV(), Mods);
      EColorSpace.HSL:    Result := DistanceHSL(Color1.ToHSL(), Color2.ToHSL(), Mods);
      EColorSpace.LCH:    Result := DistanceLCH(Color1.ToLCH(), Color2.ToLCH(), Mods);
      EColorSpace.DeltaE: Result := DistanceDeltaE(Color1.ToLAB(), Color2.ToLAB(), Mods);
    end;
  end;

var
  MaxDist: Single;
  i: Integer;
begin
  Result := 0;

  case Formula of
    EColorSpace.RGB:    MaxDist := DistanceRGB_Max(Mods);
    EColorSpace.XYZ:    MaxDist := DistanceXYZ_Max(Mods);
    EColorSpace.LAB:    MaxDist := DistanceLAB_Max(Mods);
    EColorSpace.HSV:    MaxDist := DistanceHSV_Max(Mods);
    EColorSpace.HSL:    MaxDist := DistanceHSL_Max(Mods);
    EColorSpace.LCH:    MaxDist := DistanceLCH_Max(Mods);
    EColorSpace.DeltaE: MaxDist := DistanceDeltaE_Max(Mods);
  end;
  if (maxDist = 0) then
    Exit;

  for i := 0 to High(Colors) do
    Result := Max(Result, GetDistance(Color, Colors[i]) / MaxDist * 100);
end;

function GetBestColor(Formula: EColorSpace; Colors: TColorArray): TBestColor;
var
  delA,delB,delC: Double;
  ABC: TSuperColor;
  range: TColorRange;
  hue: THueRange;
  RGB: TColorRGB;
begin
  Result := Default(TBestColor);

  range := ColorRange(Formula, Colors);
  hue   := ColorRangeHue(Formula, Colors);

  if (Formula in [EColorSpace.HSL, EColorSpace.HSV]) then
  begin
    ABC.A := GetHue(Formula, Colors, hue.Low, hue.High);
    delA  := Max(Abs(DeltaAngle(ABC.A, hue.Low)), Abs(DeltaAngle(ABC.A, Hue.High)));
  end else
  begin
    ABC.A := (range.A1 + range.A2) / 2;
    delA  := Abs(range.A1 - range.A2) / 2;
  end;

  ABC.B := (range.B1 + range.B2) / 2;
  delB  := Abs(range.B1 - range.B2) / 2;

  if (Formula in [EColorSpace.LCH, EColorSpace.DeltaE]) then
  begin
    ABC.C := GetHue(Formula, Colors, hue.Low, hue.High);
    delC  := Max(Abs(DeltaAngle(ABC.C, hue.Low)), Abs(DeltaAngle(ABC.C, Hue.High)));
  end else
  begin
    ABC.C := (range.C1 + range.C2) / 2;
    delC  := Abs(range.C1 - range.C2) / 2;
  end;

  Result.Mods[0] := 1 / Max(COLOR_FITTING_ACCURACY, delA);
  Result.Mods[1] := 1 / Max(COLOR_FITTING_ACCURACY, delB);
  Result.Mods[2] := 1 / Max(COLOR_FITTING_ACCURACY, delC);
  Result.Mods := NormalizeMods(Result.Mods);
  Result.Mods[0] := CeilTo(Result.Mods[0], 3);
  Result.Mods[1] := CeilTo(Result.Mods[1], 3);
  Result.Mods[2] := CeilTo(Result.Mods[2], 3);

  case Formula of
    EColorSpace.RGB:
      begin
        RGB.R := Trunc(ABC.A);
        RGB.G := Trunc(ABC.B);
        RGB.B := Trunc(ABC.C);

        Result.Color := RGB.ToColor;
      end;

    EColorSpace.XYZ:    Result.Color := TColorXYZ(ABC).ToColor;
    EColorSpace.LAB:    Result.Color := TColorLAB(ABC).ToColor;
    EColorSpace.HSV:    Result.Color := TColorHSV(ABC).ToColor;
    EColorSpace.HSL:    Result.Color := TColorHSL(ABC).ToColor;
    EColorSpace.LCH:    Result.Color := TColorLCH(ABC).ToColor;
    EColorSpace.DeltaE: Result.Color := TColorLCH(ABC).ToColor;
  end;

  Result.Tolerance := CeilTo(GetTolerance(Formula, Result.Color, Colors, Result.Mods), 3);
end;

{
function GetBestFormula(ColorArr: TColorArray): EDistanceFormula;
var
  tmpBest: Double;
  CS: EDistanceFormula;
begin
  Colors := TColorArray(ColorArr);
  tmpBest := 101;

  Result := EColorSpace.RGB;
  for CS in [EColorSpace.RGB, EColorSpace.HSV, EColorSpace.HSL,EColorSpace.XYZ, EColorSpace.LAB, EColorSpace.LCH, EColorSpace.DeltaE] do
  begin
    Self.SetFormula(CS);
    Self.Best := [];
    Self.ComputeBestColor();
    if Best.Tolerance < tmpBest then
    begin
      Result := CS;
      tmpBest := Best.Tolerance;
    end;
  end;
end;
}

//function Run(Formula: EColorSpace; Colors: TColorArray; DynamicFormula: Boolean=False): TBestColor;
////var i: Integer;
//begin
//  //if DynamicFormula then Self.SetFormula(Self.GetBestFormula(ColorArr));
//  //ComputeBestColor(Formula, Colors);
//  Result := Best;
//
//  WriteLn('+----| '+ToStr(Formula)+' |-----------------------------------------+');
//  WriteLn('| Color:     '+ ToStr(Best.Color)+ '  |  '+ ToStr(Best.Color.ToRGB));
//  WriteLn('| Tolerance: '+ ToStr(Best.Tolerance));
//  WriteLn('| Modifiers: '+ ToStr(Best.Mods));
//  WriteLn('+------------------------------------------------------+');
//end;

end.

