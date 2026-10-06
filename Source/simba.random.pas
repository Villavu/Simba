{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.random;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.image, simba.vartype_point, simba.vartype_box;

function RandomCenterTPA(Amount: Integer; Box: TBox): TPointArray;
function RandomTPA(Amount: Integer; Box: TBox): TPointArray;
function RandomShapes(Amount: Integer; ShapesPerRow: Integer = 5; RandScale: Single = 0.5; RandRotate: Single = 0.05): TSimbaImage;

function Random(Lo, Hi: Double): Double; overload;
function Random(Lo, Hi: Int64): Int64; overload;

function RandomLeft(Lo, Hi: Double): Double; overload;
function RandomLeft(Lo, Hi: Int64): Int64; overload;

function RandomRight(Lo, Hi: Double): Double; overload;
function RandomRight(Lo, Hi: Int64): Int64; overload;

function RandomMean(Lo, Hi: Double): Double; overload;
function RandomMean(Lo, Hi: Int64): Int64; overload;

function RandomMode(Mode, Lo, Hi: Double): Double; overload;
function RandomMode(Mode, Lo, Hi: Int64): Int64; overload;

function GaussRand(Mean, Dev: Double): Double;

procedure BetterRandomize;

var
  RandCutoff: Double = 5;

implementation

uses
  Math, DateUtils;

function nzRandom: Double;
begin
  Result := Max(Double(Random()), 1.0e-320);
end;

function RandomCenterTPA(Amount: Integer; Box: TBox): TPointArray;
var
  i, xcenter, ycenter: Integer;
  x, y, xstep, ystep: Single;
begin
  SetLength(Result, Amount);

  xcenter := Box.Center.X;
  ycenter := Box.Center.Y;
  xstep := (Box.Width div 2) / Amount;
  ystep := (Box.Height div 2) /Amount;

  x:=0;
  y:=0;

  for i := 0 to Amount - 1 do
  begin
    x := x + xstep;
    y := y + ystep;
    Result[i].x := RandomRange(Round(xcenter-x), Round(xcenter+x));
    Result[i].y := RandomRange(Round(ycenter-y), Round(ycenter+y));
  end;
end;

function RandomTPA(Amount: Integer; Box: TBox): TPointArray;
var
  i: Integer;
begin
  SetLength(Result, Amount);
  for i := 0 to Amount - 1 do
    Result[i] := Point(RandomRange(Box.X1, Box.X2), RandomRange(Box.Y1, Box.Y2));
end;

const
  // The resources RANDOM_SHAPE_0 and up (Images/shapes): a white shape on black each
  SHAPE_COUNT = 20;

function RandomShapes(Amount: Integer; ShapesPerRow: Integer; RandScale: Single; RandRotate: Single): TSimbaImage;

  function GetRandomShapes: TSimbaImageArray;
  var
    tmp: TSimbaImage;
    i: Integer;
  begin
    SetLength(Result, Amount);

    for I := 0 to High(Result) do
    begin
      Result[I] := TSimbaImage.Create();
      Result[I].FromResource('RANDOM_SHAPE_' + IntToStr(Random(SHAPE_COUNT)));

      if (RandScale > 0) then
      begin
        tmp := Result[I];
        Result[I] := tmp.Resize(EImageResizeAlgo.NEAREST_NEIGHBOUR, 1.0 + Random(-RandScale, RandScale));
        tmp.Free();
      end;

      if (RandRotate > 0) then
      begin
        tmp := Result[I];
        Result[I] := tmp.Rotate(EImageRotateAlgo.NEAREST_NEIGHBOUR, DegToRad(Random(-360*RandRotate,360*RandRotate)), True);
        tmp.Free();
      end;
    end;
  end;

var
  ShapeImages: TSimbaImageArray;
  Boxes: TBoxArray;
  I, MaxW, MaxH: Integer;
begin
  if (Amount < 1)       then Amount := 1;
  if (ShapesPerRow < 1) then ShapesPerRow := 1;

  ShapeImages := GetRandomShapes();
  MaxW := ShapeImages[0].Width;
  MaxH := ShapeImages[0].Height;
  for I := 1 to High(ShapeImages) do
  begin
    MaxW := Max(MaxW, ShapeImages[I].Width);
    MaxH := Max(MaxH, ShapeImages[I].Height);
  end;

  Boxes := TBoxArray.Create(TPoint.ZERO, ShapesPerRow, (Amount + ShapesPerRow - 1) div ShapesPerRow, MaxW, MaxH, TPoint.ZERO);

  Result := TSimbaImage.Create(Boxes.Merge.Width, Boxes.Merge.Height);
  for I := 0 to High(ShapeImages) do
  begin
    Result.Canvas.DrawImage(ShapeImages[I], Boxes[I].Center - ShapeImages[I].Center);

    ShapeImages[I].Free();
  end;
end;

function Random(Lo, Hi: Double): Double;
begin
  Result := Lo + Random() * (Hi - Lo);
end;

function Random(Lo, Hi: Int64): Int64;
begin
  Result := Trunc(Random(Lo * 1.00, Hi * 1.00));
end;

function RandomLeft(Lo, Hi: Double): Double;
begin
  Result := RandCutoff + 1;
  while Result >= RandCutoff do
    Result := Abs(Sqrt(-2 * Ln(nzRandom())) * Cos(2 * PI * Random()));
  Result := Result / RandCutoff * (Hi-Lo) + Lo;
end;

function RandomLeft(Lo, Hi: Int64): Int64;
begin
  Result := Trunc(RandomLeft(Lo * 1.00, Hi * 1.00));
end;

function RandomRight(Lo, Hi: Double): Double;
begin
  Result := RandomLeft(Hi, Lo);
end;

function RandomRight(Lo, Hi: Int64): Int64;
begin
  Result := Trunc(RandomRight(Lo * 1.00, Hi * 1.00));
end;

function RandomMean(Lo, Hi: Double): Double;
begin
  Result := RandomMode(Lo + ((Hi-Lo) / 2), Lo, Hi);
end;

function RandomMean(Lo, Hi: Int64): Int64;
begin
  Result := Trunc(RandomMean(Lo * 1.00, Hi * 1.00));
end;

function RandomMode(Mode, Lo, Hi: Double): Double;
var
  Top: Double;
begin
  Top := Lo;
  if Random() * (Hi-Lo) > Mode-Lo then
    Top := Hi;

  Result := RandCutoff + 1;
  while Result >= RandCutoff do
    Result := Abs(Sqrt(-2 * Ln(nzRandom())) * Cos(2 * PI * Random()));
  Result := Result / RandCutoff * (Top-Mode) + Mode;
end;

function RandomMode(Mode, Lo, Hi: Int64): Int64;
begin
  Result := Trunc(RandomMode(Mode * 1.00, Lo * 1.00, Hi * 1.00));
end;

function GaussRand(Mean, Dev: Double): Double;
var
  Len: Double;
begin
  Len := Dev * Sqrt(-2 * Ln(nzRandom()));
  Result := Mean + Len * Cos(2 * PI * Random());
end;

{$PUSH}
{$R-}
{$Q-}
procedure BetterRandomize; // https://github.com/dajobe/libmtwist/blob/master/seed.c

  procedure Mix(var A, B, C: UInt32);
  begin
    A -= C;  A := A xor ((C shl 4)  or (C shr (32-4)));   C += B;
    B -= A;  B := B xor ((A shl 6)  or (A shr (32-6)));   A += C;
    C -= B;  C := C xor ((B shl 8)  or (B shr (32-8)));   B += A;
    A -= C;  A := A xor ((C shl 16) or (C shr (32-16)));  C += B;
    B -= A;  B := B xor ((A shl 19) or (A shr (32-19)));  A += C;
    C -= B;  C := C xor ((B shl 4)  or (B shr (32-4)));   B += A;
  end;

var
  A, B, C: UInt32;
begin
  A := UInt32(GetTickCount64());             // SOURCE 1: System uptime
  B := UInt32(DateTimeToUnix(Now(), False)); // SOURCE 2: Unix time
  C := UInt32(GetProcessID());               // SOURCE 3: Process ID

  Mix(A, B, C);

  RandSeed := C;
end;
{$POP}

initialization
  BetterRandomize();

end.
