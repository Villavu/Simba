{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.bitmap_utils;

{$DEFINE SIMBA_MAX_OPTIMIZATION}
{$i simba.inc}

interface

uses
  classes, sysutils, graphics,
  simba.mufasatypes, simba.bitmap, simba.colormath;

procedure LoadBitmapAreaFromFile(Bitmap: TMufasaBitmap; FileName: String; Area: TBox);
function GetBitmapPixelFormat(Bitmap: TBitmap): String;

type
  TBitmapHelper = type helper for TBitmap
    // Draw TRGB32 data without changing bitmap format
    procedure FromData(AData: PColorBGRA; AWidth, AHeight: Integer);

    function ToMufasaBitmap: TMufasaBitmap;
    procedure LoadFromFile(FileName: String);
  end;

  TRGBSum = record
    R, G, B: Int64;

    class operator :=(const Right: TColorBGRA): TRGBSum;

    class operator +(const Left: TRGBSum; const Right: TColorBGRA): TRGBSum;
    class operator -(const Left: TRGBSum; const Right: TColorBGRA): TRGBSum;

    class operator +(const Left, Right: TRGBSum): TRGBSum;
    class operator -(const Left, Right: TRGBSum): TRGBSum;
  end;
  TRGBSumTable = array of array of TRGBSum;

  TRGBSumTableHelper = type helper for TRGBSumTable
  public
    class function Create(Bitmap: TMufasaBitmap): TRGBSumTable; static;

    function Query(Area: TBox): TRGBSum;
  end;

implementation

uses
  BMPcomn, GraphType, IntfGraphics;

type
  PARGB = ^TARGB;
  TARGB = packed record
    A, R, G, B: Byte;
  end;

  PRGB24 = ^TRGB24;
  TRGB24 = packed record
    B, G, R : Byte;
  end;

procedure LoadBitmapAreaFromFile(Bitmap: TMufasaBitmap; FileName: String; Area: TBox);

  function ColorRGBToBGRA(const ColorRGB: TColorRGB): TColorBGRA; inline;
  begin
    Result.B := ColorRGB.B;
    Result.G := ColorRGB.G;
    Result.R := ColorRGB.R;
    Result.A := 0;
  end;

var
  Stream: TFileStream;
  FileHeader: TBitmapFileHeader;
  Header: TBitmapInfoHeader;
  Row, Column, PixelOffset: Integer;
  ScanLineSize, Index: Int64;
  Buffer: PByte;
  BytesPerPixel: Byte;
begin
  Buffer := nil;

  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    if Stream.Read(FileHeader, SizeOf(TBitmapFileHeader)) <> SizeOf(TBitmapFileHeader) then
      raise Exception.Create('Invalid file header');

    {$IFDEF ENDIAN_BIG}
    SwapBMPFileHeader(FileHeader);
    {$ENDIF}
    if (FileHeader.bfType <> BMmagic) then
      raise Exception.Create('Invalid file header magic');

    if Stream.Read(Header, SizeOf(TBitmapInfoHeader)) <> SizeOf(TBitmapInfoHeader) then
      raise Exception.Create('Invalid info header');
    {$IFDEF ENDIAN_BIG}
    SwapBMPInfoHeader(Header);
    {$ENDIF}

    case Header.BitCount of
      32: BytesPerPixel := 4;
      24: BytesPerPixel := 3;
      else
        raise Exception.Create('Not a 32 or 24 bit bitmap');
    end;

    Area.Clip(TBox.Create(0, 0, Header.Width, Header.Height));
    if (Area.Width > 1) and (Area.Height > 1) then
    begin
      Bitmap.SetSize(Area.Width, Area.Height);

      PixelOffset := FileHeader.bfOffset;
      ScanLineSize := Int64((Header.Width * Header.BitCount) + 31) div 32 * 4;
      Buffer := GetMem(ScanLineSize);
      Index := 0;

      for Row := Area.Y1 to Area.Y2 do
      begin
        Stream.Position := PixelOffset + ((Header.Height - (Row + 1)) * ScanLineSize) + (Area.X1 * BytesPerPixel);
        Stream.Read(Buffer^, ScanLineSize);

        for Column := 0 to Area.Width - 1 do
        begin
          Bitmap.Data[Index] := ColorRGBToBGRA(PColorRGB(Buffer + (Column * BytesPerPixel))^);

          Inc(Index);
        end;
      end;
    end;
  finally
    if (Buffer <> nil) then
      FreeMem(Buffer);

    Stream.Free();
  end;
end;

function GetBitmapPixelFormat(Bitmap: TBitmap): String;
begin
  with Bitmap.RawImage.Description do // From BGRABitmap
  begin
    if (BitsPerPixel >= 32) and
       (AlphaPrec = 8) and
       (((AlphaShift = 0) and
         (RedShift = 8) and
         (GreenShift = 16) and
         (BlueShift = 24) and
         (ByteOrder = riboLSBFirst)) or
         ((AlphaShift = BitsPerPixel - 8) and
          (RedShift = BitsPerPixel - 16) and
          (GreenShift = BitsPerPixel - 24) and
          (BlueShift = BitsPerPixel - 32) and
          (ByteOrder = riboMSBFirst))) then
       begin
         Exit('ARGB');
       end
    else
    if (BitsPerPixel >= 32) and
       (AlphaPrec = 0) and
       (((RedShift = 8) and
         (GreenShift = 16) and
         (BlueShift = 24) and
         (ByteOrder = riboLSBFirst)) or
         ((RedShift = BitsPerPixel - 16) and
          (GreenShift = BitsPerPixel - 24) and
          (BlueShift = BitsPerPixel - 32) and
          (ByteOrder = riboMSBFirst))) then
       begin
         Exit('ARGB'); // alpha not used
       end
    else
    if (BitsPerPixel >= 24) and
       (((RedShift = 0) and
         (GreenShift = 8) and
         (BlueShift = 16) and
         (ByteOrder = riboLSBFirst)) or
         ((RedShift = BitsPerPixel - 8) and
          (GreenShift = BitsPerPixel - 16) and
          (BlueShift = BitsPerPixel - 24) and
          (ByteOrder = riboMSBFirst))) then
       begin
         if (BitsPerPixel = 24) then
           Exit('BGR')
         else
           Exit('BGRA');
       end
   else
   if (BitsPerPixel >= 24) and
      (((BlueShift = 0) and
        (GreenShift = 8) and
        (RedShift = 16) and
        (ByteOrder = riboLSBFirst)) or
        ((BlueShift = BitsPerPixel - 8) and
         (GreenShift = BitsPerPixel - 16) and
         (RedShift = BitsPerPixel - 24) and
         (ByteOrder = riboMSBFirst))) then
     begin
       if (BitsPerPixel = 24) then
         Exit('BGR')
       else
         Exit('BGRA');
     end;
  end;

  raise Exception.Create('Pixel format not supported: ' + Bitmap.RawImage.Description.AsString);
end;

procedure TBitmapHelper.FromData(AData: PColorBGRA; AWidth, AHeight: Integer);
var
  Source, Dest: PByte;
  SourceBytesPerLine, DestBytesPerLine: Integer;
  Upper: PtrUInt;

  procedure BGR;
  var
    RowUpper: PtrUInt;
    RowSource, RowDest: PByte;
  begin
    while PtrUInt(Source) < Upper do
    begin
      RowSource := Source;
      RowDest := Dest;
      RowUpper := PtrUInt(Source + SourceBytesPerLine);

      while PtrUInt(RowSource) < RowUpper do
      begin
        PRGB24(RowDest)^ := PRGB24(RowSource)^; // Can just use first three bytes

        Inc(RowSource, SizeOf(TRGB32));
        Inc(RowDest, SizeOf(TRGB24));
      end;

      Inc(Source, SourceBytesPerLine);
      Inc(Dest, DestBytesPerLine);
    end;
  end;

  procedure BGRA;
  var
    RowSource, RowDest: PByte;
  begin
    while PtrUInt(Source) < Upper do
    begin
      RowSource := Source;
      RowDest := Dest;

      Move(RowSource^, RowDest^, DestBytesPerLine);
      {
      RowUpper := PtrUInt(Source + SourceBytesPerLine);
      while PtrUInt(RowSource) < RowUpper do
      begin
        PColorBGRA(RowDest)^ := PColorBGRA(RowSource)^;

        Inc(RowSource, SizeOf(TRGB32));
        Inc(RowDest, SizeOf(TRGB32));
      end;
      }

      Inc(Source, SourceBytesPerLine);
      Inc(Dest, DestBytesPerLine);
    end;
  end;

  {
    TODO: Test flipping bytes

    function SwapLong(Value: LongWord): LongWord;
    {$IFDEF UseASM86}
    asm
      BSWAP  EAX
    end;
    {$ELSE}
    begin
      Result := Value shl 24 or Value shr 24 or Value shl 8 and $00FF0000 or Value shr 8 and $0000FF00;
    end;
    {$ENDIF}
  }
  procedure ARGB;
  var
    RowUpper: PtrUInt;
    RowSource, RowDest: PByte;
  begin
    while PtrUInt(Source) < Upper do
    begin
      RowSource := Source;
      RowDest := Dest;
      RowUpper := PtrUInt(Source + SourceBytesPerLine);

      while PtrUInt(RowSource) < RowUpper do
      begin
        PARGB(RowDest)^.R := PColorBGRA(RowSource)^.R;
        PARGB(RowDest)^.G := PColorBGRA(RowSource)^.G;
        PARGB(RowDest)^.B := PColorBGRA(RowSource)^.B;
        PARGB(RowDest)^.A := PColorBGRA(RowSource)^.A;

        Inc(RowSource, SizeOf(TRGB32));
        Inc(RowDest, SizeOf(TARGB));
      end;

      Inc(Source, SourceBytesPerLine);
      Inc(Dest, DestBytesPerLine);
    end;
  end;

begin
  BeginUpdate();

  SetSize(AWidth, AHeight);

  Dest := RawImage.Data;
  DestBytesPerLine := RawImage.Description.BytesPerLine;

  Source := PByte(AData);
  SourceBytesPerLine := AWidth * SizeOf(TRGB32);

  Upper := PtrUInt(Source + (SourceBytesPerLine * AHeight));

  case GetBitmapPixelFormat(Self) of
    'BGR':  BGR();
    'BGRA': BGRA();
    'ARGB': ARGB();
  end;

  EndUpdate();
end;

function TBitmapHelper.ToMufasaBitmap: TMufasaBitmap;
var
  Source, Dest: PByte;
  SourceBytesPerLine, DestBytesPerLine: Integer;
  Upper: PtrUInt;

  procedure BGR;
  var
    RowUpper: PtrUInt;
    RowSource, RowDest: PByte;
  begin
    while PtrUInt(Source) < Upper do
    begin
      RowSource := Source;
      RowDest := Dest;
      RowUpper := PtrUInt(Source + SourceBytesPerLine);

      while PtrUInt(RowSource) < RowUpper do
      begin
        PRGB24(RowDest)^ := PRGB24(RowSource)^; // Can just use first three bytes

        Inc(RowSource, SizeOf(TRGB24));
        Inc(RowDest, SizeOf(TRGB32));
      end;

      Inc(Source, SourceBytesPerLine);
      Inc(Dest, DestBytesPerLine);
    end;
  end;

  procedure BGRA;
  var
    RowSource, RowDest: PByte;
  begin
    while PtrUInt(Source) < Upper do
    begin
      RowSource := Source;
      RowDest := Dest;

      Move(RowSource^, RowDest^, DestBytesPerLine);

      {
      RowUpper := PtrUInt(Source + SourceBytesPerLine);
      while PtrUInt(RowSource) < RowUpper do
      begin
        PColorBGRA(RowDest)^ := PColorBGRA(RowSource)^;

        Inc(RowSource, SizeOf(TRGB32));
        Inc(RowDest, SizeOf(TRGB32));
      end;
      }

      Inc(Source, SourceBytesPerLine);
      Inc(Dest, DestBytesPerLine);
    end;
  end;

  procedure ARGB;
  var
    RowUpper: PtrUInt;
    RowSource, RowDest: PByte;
  begin
    while PtrUInt(Source) < Upper do
    begin
      RowSource := Source;
      RowDest := Dest;
      RowUpper := PtrUInt(Source + SourceBytesPerLine);

      while PtrUInt(RowSource) < RowUpper do
      begin
        PARGB(RowDest)^.R := PColorBGRA(RowSource)^.R;
        PARGB(RowDest)^.G := PColorBGRA(RowSource)^.G;
        PARGB(RowDest)^.B := PColorBGRA(RowSource)^.B;
        PARGB(RowDest)^.A := PColorBGRA(RowSource)^.A;

        Inc(RowSource, SizeOf(TARGB));
        Inc(RowDest, SizeOf(TRGB32));
      end;

      Inc(Source, SourceBytesPerLine);
      Inc(Dest, DestBytesPerLine);
    end;
  end;

begin
  Result := TMufasaBitmap.Create();
  Result.SetSize(Width, Height);

  Dest := PByte(Result.Data);
  DestBytesPerLine := Width * SizeOf(TRGB32);

  Source := RawImage.Data;
  SourceBytesPerLine := RawImage.Description.BytesPerLine;

  Upper := PtrUInt(Source + (SourceBytesPerLine * Height));

  case GetBitmapPixelFormat(Self) of
    'BGR':  BGR();
    'BGRA': BGRA();
    'ARGB': ARGB();
  end;
end;

procedure TBitmapHelper.LoadFromFile(FileName: String);
var
  LazImg: TLazIntfImage;
begin
  try
    LazImg := TLazIntfImage.Create(0, 0);
    LazImg.DataDescription := Self.RawImage.Description;
    LazImg.LoadFromFile(FileName);

    Self.LoadFromIntfImage(LazImg);
  finally
    LazImg.Free();
  end;
end;

class operator TRGBSum.:=(const Right: TColorBGRA): TRGBSum;
begin
  Result.R := Right.R;
  Result.G := Right.G;
  Result.B := Right.B;
end;

class operator TRGBSum.+(const Left: TRGBSum; const Right: TColorBGRA): TRGBSum;
begin
  Result.R := Left.R + Right.R;
  Result.G := Left.G + Right.G;
  Result.B := Left.B + Right.B;
end;

class operator TRGBSum.-(const Left: TRGBSum; const Right: TColorBGRA): TRGBSum;
begin
  Result.R := Left.R - Right.R;
  Result.G := Left.G - Right.G;
  Result.B := Left.B - Right.B;
end;

class operator TRGBSum.+(const Left, Right: TRGBSum): TRGBSum;
begin
  Result.R := Left.R + Right.R;
  Result.G := Left.G + Right.G;
  Result.B := Left.B + Right.B;
end;

class operator TRGBSum.-(const Left, Right: TRGBSum): TRGBSum;
begin
  Result.R := Left.R - Right.R;
  Result.G := Left.G - Right.G;
  Result.B := Left.B - Right.B;
end;

class function TRGBSumTableHelper.Create(Bitmap: TMufasaBitmap): TRGBSumTable;
var
  W, H, X, Y: Integer;
begin
  SetLength(Result, Bitmap.Height, Bitmap.Width);

  W := Bitmap.Width - 1;
  H := Bitmap.Height - 1;

  for Y := 0 to H do
    Result[0, Y] := Bitmap.Data[Y*Bitmap.Width];

  for Y := 1 to H do
    for X := 0 to W do
      Result[Y, X] := Bitmap.Data[Y*Bitmap.Width+X] + Result[Y-1, X];

  for Y := 0 to H do
    for X := 1 to W do
      Result[Y, X] += Result[Y, X-1];
end;

function TRGBSumTableHelper.Query(Area: TBox): TRGBSum;
begin
  Result := Self[Area.Y2, Area.X2];

  if (Area.Y1 > 0) then
  begin
    Result.R := Result.R - Self[Area.Y1 - 1, Area.X2].R;
    Result.G := Result.G - Self[Area.Y1 - 1, Area.X2].G;
    Result.B := Result.B - Self[Area.Y1 - 1, Area.X2].B;
  end;

  if (Area.X1 > 0) then
  begin
    Result.R := Result.R - Self[Area.Y2, Area.X1 - 1].R;
    Result.G := Result.G - Self[Area.Y2, Area.X1 - 1].G;
    Result.B := Result.B - Self[Area.Y2, Area.X1 - 1].B;
  end;

  if (Area.Y1 > 0) and (Area.X1 > 0) then
  begin
    Result.R := Result.R + Self[Area.Y1 - 1, Area.X1 - 1].R;
    Result.G := Result.G + Self[Area.Y1 - 1, Area.X1 - 1].G;
    Result.B := Result.B + Self[Area.Y1 - 1, Area.X1 - 1].B;
  end;
end;

end.

