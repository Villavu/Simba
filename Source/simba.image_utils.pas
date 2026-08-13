{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.image_utils;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.image;

type
  TSimbaIntegralImageF = record
  public
  type
    TData = record Value: Double; ValueSqr: Double; end;
    TDataMatrix = array of array of TData;
  public
    Data: TDataMatrix;

    class function Create(const From: TByteMatrix): TSimbaIntegralImageF; static;
    procedure Query(const Left, Top, Right, Bottom: Integer; out Value, ValueSqr: Double); overload;
    function Query(const Left, Top, Right, Bottom: Integer): Double; overload;
  end;

  TSimbaIntegralImageRGB = record
  public
  type
    TData = record R, G, B: UInt64; end;
    TDataMatrix = array of array of TData;
  public
    Data: TDataMatrix;

    class function Create(const From: TSimbaImage): TSimbaIntegralImageRGB; static;
    procedure Query(const Left, Top, Right, Bottom: Integer; out RSum, GSum, BSum: UInt64);
  end;

// https://sashamaps.net/docs/resources/20-colors/
const
  DISTINCT_COLORS: TColorArray = (
    $0000FF, $4BB43C, $19E1FF, $D86343, $3182F5, $B41E91, $F4D442,
    $E632F0, $45EFBF, $D4BEFA, $909946, $FFBEDC, $24639A, $C8FAFF,
    $000080, $C3FFAA, $008080, $B1D8FF, $750000, $A9A9A9
  );

  ALPHA_OPAQUE      = Byte(255);
  ALPHA_TRANSPARENT = Byte(0);

procedure BlendPixel(const Data: PColorBGRA; const DataW, DataH: Integer; const X,Y: Integer; constref Color: TColorBGRA); overload; inline;
procedure BlendPixel(const Pixel: PColorBGRA; constref Color: TColorBGRA); overload;

function GetDistinctColor(const Index: Integer): Integer;
function GetRotatedSize(W, H: Integer; Angle: Single): TBox;

procedure FillData(const Data: PColorBGRA; const Count: SizeInt; constref Value: TColorBGRA);

implementation

uses
  simba.vartype_matrix, simba.vartype_pointarray, simba.geometry;

class function TSimbaIntegralImageF.Create(const From: TByteMatrix): TSimbaIntegralImageF;
var
  X, Y: Integer;
begin
  SetLength(Result.Data, From.Height, From.Width);

  // Compute the first row of the integral image
  for X := 0 to From.Width - 1 do
  begin
    Result.Data[0, X].Value := From[0, X];
    Result.Data[0, X].ValueSqr := Sqr(From[0, X]);
    if (X > 0) then
    begin
      Result.Data[0, X].Value += Result.Data[0, X - 1].Value;
      Result.Data[0, X].ValueSqr += Result.Data[0, X - 1].ValueSqr;
    end;
  end;

  // Compute the first column of the integral image
  for Y := 1 to From.Height - 1 do
  begin
    Result.Data[Y, 0].Value := From[Y, 0] + Result.Data[Y - 1, 0].Value;
    Result.Data[Y, 0].ValueSqr := Sqr(From[Y, 0]) + Result.Data[Y - 1, 0].ValueSqr;
  end;

  // Compute the rest of the integral image
  for Y := 1 to From.Height - 1 do
    for X := 1 to From.Width - 1 do
    begin
      Result.Data[Y, X].Value := From[Y, X] + Result.Data[Y - 1, X].Value + Result.Data[Y, X - 1].Value - Result.Data[Y - 1, X - 1].Value;
      Result.Data[Y, X].ValueSqr := Sqr(From[Y, X]) + Result.Data[Y - 1, X].ValueSqr + Result.Data[Y, X - 1].ValueSqr - Result.Data[Y - 1, X - 1].ValueSqr;
    end;
end;

procedure TSimbaIntegralImageF.Query(const Left, Top, Right, Bottom: Integer; out Value, ValueSqr: Double);
var
  A,B,C,D: TData;
begin
  A := Default(TData);
  B := Default(TData);
  C := Default(TData);
  D := Default(TData);

  if (Left - 1 >= 0) and (Top - 1 >= 0) then
    A := Data[Top - 1, Left - 1];
  if (Top - 1 >= 0) then
    B := Data[Top - 1, Right];
  if (Left - 1 >= 0) then
    C := Data[Bottom, Left - 1];
  D := Data[Bottom, Right];

  Value    := D.Value - B.Value - C.Value + A.Value;
  ValueSqr := D.ValueSqr - B.ValueSqr - C.ValueSqr + A.ValueSqr;
end;

function TSimbaIntegralImageF.Query(const Left, Top, Right, Bottom: Integer): Double;
var
  _: Double;
begin
  Query(Left, Top, Right, Bottom, Result, _);
end;

class function TSimbaIntegralImageRGB.Create(const From: TSimbaImage): TSimbaIntegralImageRGB;
var
  X, Y: Integer;
begin
  SetLength(Result.Data, From.Height, From.Width);

  // Compute the first row of the integral image
  for X := 0 to From.Width - 1 do
  begin
    Result.Data[0, X].R := From.Data[X].R;
    Result.Data[0, X].G := From.Data[X].G;
    Result.Data[0, X].B := From.Data[X].B;

    if (X > 0) then
    begin
      Result.Data[0, X].R += Result.Data[0, X - 1].R;
      Result.Data[0, X].G += Result.Data[0, X - 1].G;
      Result.Data[0, X].B += Result.Data[0, X - 1].B;
    end;
  end;

  // Compute the first column of the integral image
  for Y := 1 to From.Height - 1 do
  begin
    Result.Data[Y, 0].R := From.Data[Y * From.Width].R + Result.Data[Y - 1, 0].R;
    Result.Data[Y, 0].G := From.Data[Y * From.Width].G + Result.Data[Y - 1, 0].G;
    Result.Data[Y, 0].B := From.Data[Y * From.Width].B + Result.Data[Y - 1, 0].B;
  end;

  // Compute the rest of the integral image
  for Y := 1 to From.Height - 1 do
    for X := 1 to From.Width - 1 do
    begin
      Result.Data[Y, X].R := From.Data[Y * From.Width + X].R + Result.Data[Y - 1, X].R + Result.Data[Y, X - 1].R - Result.Data[Y - 1, X - 1].R;
      Result.Data[Y, X].G := From.Data[Y * From.Width + X].G + Result.Data[Y - 1, X].G + Result.Data[Y, X - 1].G - Result.Data[Y - 1, X - 1].G;
      Result.Data[Y, X].B := From.Data[Y * From.Width + X].B + Result.Data[Y - 1, X].B + Result.Data[Y, X - 1].B - Result.Data[Y - 1, X - 1].B;
    end;
end;

procedure TSimbaIntegralImageRGB.Query(const Left, Top, Right, Bottom: Integer; out RSum, GSum, BSum: UInt64);
var
  A,B,C,D: TData;
begin
  A := Default(TData);
  B := Default(TData);
  C := Default(TData);
  D := Default(TData);

  if (Left - 1 >= 0) and (Top - 1 >= 0) then
    A := Data[Top - 1, Left - 1];
  if (Top - 1 >= 0) then
    B := Data[Top - 1, Right];
  if (Left - 1 >= 0) then
    C := Data[Bottom, Left - 1];
  D := Data[Bottom, Right];

  RSum := D.R - B.R - C.R + A.R;
  GSum := D.G - B.G - C.G + A.G;
  BSum := D.B - B.B - C.B + A.B;
end;

procedure BlendPixel(const Pixel: PColorBGRA; constref Color: TColorBGRA);
{$IF DEFINED(IMAGE_ASM)}
  {$I asm/blendpixel_x86_64.inc}
{$ELSE}
var
  A, AInv: UInt32; // optimized for best fpc asm generation
begin
  if (Pixel^.A > 0) then
  begin
    A := Color.A;
    AInv := UInt32(255) - A;

    Pixel^.B := (Pixel^.B * AInv + Color.B * A) div 255;
    Pixel^.G := (Pixel^.G * AInv + Color.G * A) div 255;
    Pixel^.R := (Pixel^.R * AInv + Color.R * A) div 255;
    Pixel^.A := A + (Pixel^.A * AInv div 255);
  end
  else
    Pixel^ := Color;
end;
{$ENDIF}

procedure BlendPixel(const Data: PColorBGRA; const DataW, DataH: Integer; const X, Y: Integer; constref Color: TColorBGRA);
begin
  if (UInt32(X) < UInt32(DataW)) and (UInt32(Y) < UInt32(DataH)) then
    BlendPixel(@Data[Y * DataW + X], Color);
end;

function GetDistinctColor(const Index: Integer): Integer;
begin
  Result := DISTINCT_COLORS[Index mod Length(DISTINCT_COLORS)];
end;

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

procedure FillData(const Data: PColorBGRA; const Count: SizeInt; constref Value: TColorBGRA);
{$IF DEFINED(IMAGE_ASM)}
  {$I asm/filldata_x86_64.inc}
{$ELSE}
begin
  FillDWord(Data^, Count, UInt32(Value));
end;
{$ENDIF}

end.

