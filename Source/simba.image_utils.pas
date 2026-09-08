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
  simba.base;

const
  ALPHA_OPAQUE      = Byte(255);
  ALPHA_TRANSPARENT = Byte(0);

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

// Data[0..Count-1] := Value
procedure FillData(const Data: PColorBGRA; const Count: SizeInt; constref Value: TColorBGRA);
// Color over Pixel by Color's alpha.
// A transparent Pixel just takes Color.
procedure BlendPixel(const Pixel: PColorBGRA; const Color: PColorBGRA); {$IF NOT DEFINED(IMAGE_ASM)}inline;{$ENDIF}
// Src over Dest for Count pixels, each by its own alpha: an opaque pixel is
// copied, a transparent one skipped, anything between is blended.
procedure BlendData(Dest, Src: PColorBGRA; Count: SizeInt);
// As BlendData, but every source alpha is first scaled by Alpha/255 - a whole
// image opacity. Alpha = 255 is exactly BlendData.
procedure BlendDataAlpha(Dest, Src: PColorBGRA; Count: SizeInt; Alpha: Byte);
// Table of 24 distinct colors - wraps past the end.
function GetDistinctColor(const Index: Integer): Integer;
// The bounds of a W x H image rotated by Angle (radians) about its centre.
function GetRotatedSize(W, H: Integer; Angle: Single): TBox;

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

procedure FillData(const Data: PColorBGRA; const Count: SizeInt; constref Value: TColorBGRA);
{$IF DEFINED(IMAGE_ASM)}
  {$I asm/filldata_x86_64.inc}
{$ELSE}
begin
  FillDWord(Data^, Count, UInt32(Value));
end;
{$ENDIF}

procedure BlendPixel(const Pixel: PColorBGRA; const Color: PColorBGRA);
{$IF DEFINED(IMAGE_ASM)}
  {$I asm/blendpixel_x86_64.inc}
{$ELSE}
var
  A, AInv: UInt32; // optimized for best fpc asm generation
begin
  if (Pixel^.A > 0) then
  begin
    A := Color^.A;
    AInv := UInt32(255) - A;

    Pixel^.B := (Pixel^.B * AInv + Color^.B * A) div 255;
    Pixel^.G := (Pixel^.G * AInv + Color^.G * A) div 255;
    Pixel^.R := (Pixel^.R * AInv + Color^.R * A) div 255;
    Pixel^.A := A + (Pixel^.A * AInv div 255);
  end
  else
    Pixel^ := Color^;
end;
{$ENDIF}

procedure BlendData(Dest, Src: PColorBGRA; Count: SizeInt);
{$IF DEFINED(IMAGE_ASM)}
  {$I asm/blenddata_x86_64.inc}
{$ELSE}
const
  ALPHA_PAIR = UInt64($FF000000FF000000);
var
  SrcEnd, BlockEnd: PColorBGRA;
  Pair: PUInt64;
begin
  SrcEnd := Src + Count;

  // whole blocks of eight
  while (Src + 8 <= SrcEnd) do
  begin
    Pair := PUInt64(Src);

    // no alpha anywhere in the block: nothing to draw
    if (((Pair[0] or Pair[1] or Pair[2] or Pair[3]) and ALPHA_PAIR) = 0) then
    begin
      Inc(Src, 8);
      Inc(Dest, 8);
      Continue;
    end;

    // every alpha 255: a straight copy, as four qwords
    if (((Pair[0] and Pair[1] and Pair[2] and Pair[3]) and ALPHA_PAIR) = ALPHA_PAIR) then
    begin
      PUInt64(Dest)[0] := Pair[0];
      PUInt64(Dest)[1] := Pair[1];
      PUInt64(Dest)[2] := Pair[2];
      PUInt64(Dest)[3] := Pair[3];
      Inc(Src, 8);
      Inc(Dest, 8);
      Continue;
    end;

    // a soft pixel somewhere in the block: pixel by pixel
    BlockEnd := Src + 8;
    while (Src < BlockEnd) do
    begin
      if (Src^.A = ALPHA_OPAQUE) then
        Dest^ := Src^
      else if (Src^.A <> ALPHA_TRANSPARENT) then
        BlendPixel(Dest, Src);

      Inc(Src);
      Inc(Dest);
    end;
  end;

  // the last 0..7 pixels
  while (Src < SrcEnd) do
  begin
    if (Src^.A = ALPHA_OPAQUE) then
      Dest^ := Src^
    else if (Src^.A <> ALPHA_TRANSPARENT) then
      BlendPixel(Dest, Src);

    Inc(Src);
    Inc(Dest);
  end;
end;
{$ENDIF}

procedure BlendDataAlpha(Dest, Src: PColorBGRA; Count: SizeInt; Alpha: Byte);
{$IF DEFINED(IMAGE_ASM)}
  {$I asm/blenddataalpha_x86_64.inc}
{$ELSE}
var
  SrcEnd: PColorBGRA;
  Faded: TColorBGRA;
begin
  // opaque means no fade at all: the plain path, with its block fast paths
  if (Alpha = ALPHA_OPAQUE) then
  begin
    BlendData(Dest, Src, Count);
    Exit;
  end;

  SrcEnd := Src + Count;
  while (Src < SrcEnd) do
  begin
    if (Src^.A <> ALPHA_TRANSPARENT) then
    begin
      Faded := Src^;
      Faded.A := (Faded.A * Alpha) div 255;
      BlendPixel(Dest, @Faded);
    end;

    Inc(Src);
    Inc(Dest);
  end;
end;
{$ENDIF}

function GetDistinctColor(const Index: Integer): Integer;
const
  DISTINCT_COLORS: TColorArray = (
    $0000FF, $FF3714, $00EB00, $FFC300, $055F64, $C36EFF, $AFE600, $00C3FF,
    $AF5046, $7896FF, $FF00FF, $55D7AA, $4B00D7, $2D8200, $AA199B, $FFAACD,
    $0A7DFA, $73C8EB, $5F379B, $5AD200, $FF6987, $FF9B37, $0082B9, $E1009B
  );
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

end.

