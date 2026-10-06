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

// Data[0..Count-1] := Value
procedure FillData(const Data: PColorBGRA; const Count: SizeInt; constref Value: TColorBGRA);
// Dest[0..Count-1] := Src[0..Count-1]. The two may overlap.
procedure MoveData(Dest, Src: PColorBGRA; Count: SizeInt);
// Width x Height pixels from Src to Dest, whose rows are the given bytes apart. A negative BytesPerRow walks up.
procedure CopyRows(Dest: PColorBGRA; DestBytesPerRow: SizeInt; Src: PColorBGRA; SrcBytesPerRow: SizeInt; Width, Height: Integer);
// Color blended over each of Data[0..Count-1].
procedure BlendFill(Data: PColorBGRA; Count: SizeInt; constref Color: TColorBGRA);
// Color over Pixel by Color's alpha.
// A transparent Pixel just takes Color.
procedure BlendPixel(const Pixel: PColorBGRA; const Color: PColorBGRA); {$IF NOT DEFINED(IMAGE_ASM)}inline;{$ENDIF}
// Src over Dest for Count pixels, each by its own alpha: an opaque pixel is
// copied, a transparent one skipped, anything between is blended.
procedure BlendData(Dest, Src: PColorBGRA; Count: SizeInt);
// As BlendData, but every source alpha is first scaled by Alpha/255 - a whole
// image opacity. Alpha = 255 is exactly BlendData.
procedure BlendDataAlpha(Dest, Src: PColorBGRA; Count: SizeInt; Alpha: Byte);
// Whether every one of the Width * Height pixels has the given alpha.
function IsAlphaAll(Data: PColorBGRA; Width, Height: Integer; Alpha: Byte): Boolean;
// The pixel's grey value: 0.299 R + 0.587 G + 0.114 B, rounded.
function PixelToGrey(const Pixel: TColorBGRA): Byte; inline;
// Dest[0..Count-1] := the grey value of each of Src[0..Count-1].
procedure GreyData(Src: PColorBGRA; Dest: PByte; Count: SizeInt);
// Dest[0..Count-1] := each of Src[0..Count-1] as a TColor, its alpha dropped.
procedure BGRAToColors(Src: PColorBGRA; Dest: PInt32; Count: SizeInt);
// Src's channels into planes of Count bytes each. A may be nil, to leave alpha out.
procedure SplitChannels(Src: PColorBGRA; Count: SizeInt; B, G, R, A: PByte);
// Planes of Count bytes each into Dest. With A nil, every alpha is DefaultAlpha.
procedure MergeChannels(Dest: PColorBGRA; Count: SizeInt; B, G, R, A: PByte; DefaultAlpha: Byte);
// Table of 24 distinct colors - wraps past the end.
function GetDistinctColor(const Index: Integer): Integer;

implementation

uses
  simba.colormath, simba.colormath_conversion;

procedure FillData(const Data: PColorBGRA; const Count: SizeInt; constref Value: TColorBGRA);
{$IF DEFINED(IMAGE_ASM)}
  {$I asm/filldata_x86_64.inc}
{$ELSE}
begin
  FillDWord(Data^, Count, UInt32(Value));
end;
{$ENDIF}

procedure MoveData(Dest, Src: PColorBGRA; Count: SizeInt);
{$IF DEFINED(IMAGE_ASM)}
  {$I asm/movedata_x86_64.inc}
{$ELSE}
begin
  Move(Src^, Dest^, Count * SizeOf(TColorBGRA));
end;
{$ENDIF}

procedure CopyRows(Dest: PColorBGRA; DestBytesPerRow: SizeInt; Src: PColorBGRA; SrcBytesPerRow: SizeInt; Width, Height: Integer);
begin
  if (Width <= 0) or (Height <= 0) then
    Exit;

  // rows back to back in both: one block
  if (DestBytesPerRow = Width * SizeOf(TColorBGRA)) and (SrcBytesPerRow = DestBytesPerRow) then
  begin
    MoveData(Dest, Src, SizeInt(Width * Height));
    Exit;
  end;

  while (Height > 0) do
  begin
    MoveData(Dest, Src, Width);
    Src := PColorBGRA(PByte(Src) + SrcBytesPerRow);
    Dest := PColorBGRA(PByte(Dest) + DestBytesPerRow);
    Dec(Height);
  end;
end;

procedure BlendPixel(const Pixel: PColorBGRA; const Color: PColorBGRA);
{$IF DEFINED(IMAGE_ASM)}
  {$I asm/blendpixel_x86_64.inc}
{$ELSE}
var
  A, AInv: UInt32; // optimized for best fpc asm generation
begin
  if (Pixel^.A > ALPHA_TRANSPARENT) then
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

procedure BlendFill(Data: PColorBGRA; Count: SizeInt; constref Color: TColorBGRA);
var
  DataEnd: PColorBGRA;
begin
  DataEnd := Data + Count;
  while (Data < DataEnd) do
  begin
    BlendPixel(Data, @Color);
    Inc(Data);
  end;
end;

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

function IsAlphaAll(Data: PColorBGRA; Width, Height: Integer; Alpha: Byte): Boolean;
var
  DataEnd: PColorBGRA;
begin
  DataEnd := Data + Int64(Width * Height);
  while (Data < DataEnd) do
  begin
    if (Data^.A <> Alpha) then
      Exit(False);
    Inc(Data);
  end;

  Result := True;
end;

function PixelToGrey(const Pixel: TColorBGRA): Byte;
begin
  Result := Round(RED_TO_GREY[Pixel.R] + GREEN_TO_GREY[Pixel.G] + BLUE_TO_GREY[Pixel.B]);
end;

procedure GreyData(Src: PColorBGRA; Dest: PByte; Count: SizeInt);
var
  SrcEnd: PColorBGRA;
begin
  SrcEnd := Src + Count;
  while (Src < SrcEnd) do
  begin
    Dest^ := PixelToGrey(Src^);
    Inc(Src);
    Inc(Dest);
  end;
end;

procedure BGRAToColors(Src: PColorBGRA; Dest: PInt32; Count: SizeInt);
var
  SrcEnd: PColorBGRA;
begin
  SrcEnd := Src + Count;
  while (Src < SrcEnd) do
  begin
    Dest^ := Src^.ToColor;
    Inc(Src);
    Inc(Dest);
  end;
end;

procedure SplitChannels(Src: PColorBGRA; Count: SizeInt; B, G, R, A: PByte);
var
  SrcEnd: PColorBGRA;
begin
  SrcEnd := Src + Count;
  if (A = nil) then
    while (Src < SrcEnd) do
    begin
      B^ := Src^.B;
      G^ := Src^.G;
      R^ := Src^.R;

      Inc(Src);
      Inc(B);
      Inc(G);
      Inc(R);
    end
  else
    while (Src < SrcEnd) do
    begin
      B^ := Src^.B;
      G^ := Src^.G;
      R^ := Src^.R;
      A^ := Src^.A;

      Inc(Src);
      Inc(B);
      Inc(G);
      Inc(R);
      Inc(A);
    end;
end;

procedure MergeChannels(Dest: PColorBGRA; Count: SizeInt; B, G, R, A: PByte; DefaultAlpha: Byte);
var
  DestEnd: PColorBGRA;
begin
  DestEnd := Dest + Count;
  if (A = nil) then
    while (Dest < DestEnd) do
    begin
      Dest^.B := B^;
      Dest^.G := G^;
      Dest^.R := R^;
      Dest^.A := DefaultAlpha;

      Inc(Dest);
      Inc(B);
      Inc(G);
      Inc(R);
    end
  else
    while (Dest < DestEnd) do
    begin
      Dest^.B := B^;
      Dest^.G := G^;
      Dest^.R := R^;
      Dest^.A := A^;

      Inc(Dest);
      Inc(B);
      Inc(G);
      Inc(R);
      Inc(A);
    end;
end;

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

end.

