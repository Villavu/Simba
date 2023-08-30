{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Convert TSimbaImage to TBitmap and back.
}
unit simba.image_lazbridge;

{$DEFINE SIMBA_MAX_OPTIMIZATION}
{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics, GraphType, IntfGraphics, FPImage,
  simba.mufasatypes, simba.bitmap;

procedure LazImage_FromData(LazImage: TBitmap; Data: PColorBGRA; Width, Height: Integer);
function LazImage_ToSimbaImage(LazImage: TBitmap): TSimbaImage;
function LazImage_PixelFormat(LazImage: TBitmap): String;

procedure SimbaImage_ToFPImageWriter(SimbaImage: TSimbaImage; WriterClass: TFPCustomImageWriterClass; Stream: TStream);
procedure SimbaImage_FromFPImageReader(SimbaImage: TSimbaImage; ReaderClass: TFPCustomImageReaderClass; Stream: TStream);
function SimbaImage_ToRawImage(SimbaImage: TSimbaImage): TRawImage;
function SimbaImage_ToLazImage(SimbaImage: TSimbaImage): TBitmap;

implementation

var
  SimbaRawImgDescription: TRawImageDescription;

procedure LazImage_FromData(LazImage: TBitmap; Data: PColorBGRA; Width, Height: Integer);
var
  Source, Dest: PByte;
  SourceBytesPerLine, DestBytesPerLine: Integer;
  Upper: PtrUInt;

  procedure BGR;
  var
    RowUpper: PtrUInt;
    RowSource, RowDest: PByte;
  begin
    while (PtrUInt(Source) < Upper) do
    begin
      RowSource := Source;
      RowDest := Dest;
      RowUpper := PtrUInt(Source + SourceBytesPerLine);

      while (PtrUInt(RowSource) < RowUpper) do
      begin
        PColorRGB(RowDest)^ := PColorRGB(RowSource)^; // Can just use first three bytes

        Inc(RowSource, SizeOf(TColorBGRA));
        Inc(RowDest, SizeOf(TColorRGB));
      end;

      Inc(Source, SourceBytesPerLine);
      Inc(Dest, DestBytesPerLine);
    end;
  end;

  procedure BGRA;
  var
    RowSource, RowDest: PByte;
  begin
    while (PtrUInt(Source) < Upper) do
    begin
      RowSource := Source;
      RowDest := Dest;

      Move(RowSource^, RowDest^, DestBytesPerLine);

      Inc(Source, SourceBytesPerLine);
      Inc(Dest, DestBytesPerLine);
    end;
  end;

  procedure ARGB;
  var
    RowUpper: PtrUInt;
    RowSource, RowDest: PByte;
  begin
    while (PtrUInt(Source) < Upper) do
    begin
      RowSource := Source;
      RowDest := Dest;
      RowUpper := PtrUInt(Source + SourceBytesPerLine);

      while PtrUInt(RowSource) < RowUpper do
      begin
        PUInt32(RowDest)^ := SwapEndian(PUInt32(RowSource)^);

        Inc(RowSource, SizeOf(TColorBGRA));
        Inc(RowDest, SizeOf(TColorARGB));
      end;

      Inc(Source, SourceBytesPerLine);
      Inc(Dest, DestBytesPerLine);
    end;
  end;

begin
  LazImage.BeginUpdate();
  LazImage.SetSize(Width, Height);

  Dest := LazImage.RawImage.Data;
  DestBytesPerLine := LazImage.RawImage.Description.BytesPerLine;

  Source := PByte(Data);
  SourceBytesPerLine := Width * SizeOf(TColorBGRA);

  Upper := PtrUInt(Source + (SourceBytesPerLine * Height));

  case LazImage_PixelFormat(LazImage) of
    'BGR':  BGR();
    'BGRA': BGRA();
    'ARGB': ARGB();
  end;

  LazImage.EndUpdate();
end;

function LazImage_ToSimbaImage(LazImage: TBitmap): TSimbaImage;
var
  Source, Dest: PByte;
  SourceBytesPerLine, DestBytesPerLine: Integer;
  Upper: PtrUInt;

  procedure BGR;
  var
    RowUpper: PtrUInt;
    RowSource, RowDest: PByte;
  begin
    while (PtrUInt(Source) < Upper) do
    begin
      RowSource := Source;
      RowDest := Dest;
      RowUpper := PtrUInt(Source + SourceBytesPerLine);

      while PtrUInt(RowSource) < RowUpper do
      begin
        PColorRGB(RowDest)^ := PColorRGB(RowSource)^; // Can just use first three bytes

        Inc(RowSource, SizeOf(TColorRGB));
        Inc(RowDest, SizeOf(TColorBGRA));
      end;

      Inc(Source, SourceBytesPerLine);
      Inc(Dest, DestBytesPerLine);
    end;
  end;

  procedure BGRA;
  var
    RowSource, RowDest: PByte;
  begin
    while (PtrUInt(Source) < Upper) do
    begin
      RowSource := Source;
      RowDest := Dest;

      Move(RowSource^, RowDest^, DestBytesPerLine);

      Inc(Source, SourceBytesPerLine);
      Inc(Dest, DestBytesPerLine);
    end;
  end;

  procedure ARGB;
  var
    RowUpper: PtrUInt;
    RowSource, RowDest: PByte;
  begin
    while (PtrUInt(Source) < Upper) do
    begin
      RowSource := Source;
      RowDest := Dest;
      RowUpper := PtrUInt(Source + SourceBytesPerLine);

      while PtrUInt(RowSource) < RowUpper do
      begin
        PUInt32(RowDest)^ := SwapEndian(PUInt32(RowSource)^);

        Inc(RowSource, SizeOf(TColorARGB));
        Inc(RowDest, SizeOf(TColorBGRA));
      end;

      Inc(Source, SourceBytesPerLine);
      Inc(Dest, DestBytesPerLine);
    end;
  end;

begin
  Result := TSimbaImage.Create();
  Result.SetSize(LazImage.Width, LazImage.Height);

  Dest := PByte(Result.Data);
  DestBytesPerLine := LazImage.Width * SizeOf(TColorBGRA);

  Source := LazImage.RawImage.Data;
  SourceBytesPerLine := LazImage.RawImage.Description.BytesPerLine;

  Upper := PtrUInt(Source + (SourceBytesPerLine * LazImage.Height));

  case LazImage_PixelFormat(LazImage) of
    'BGR':  BGR();
    'BGRA': BGRA();
    'ARGB': ARGB();
  end;
end;

function LazImage_PixelFormat(LazImage: TBitmap): String;

  function isARGB: Boolean;
  begin
    // ARGB or ARGB but alpha is not used
    with LazImage.RawImage.Description do
      Result := ((BitsPerPixel = 32) and (AlphaPrec = 8) and (((AlphaShift = 0) and (RedShift = 8) and (GreenShift = 16) and  (BlueShift = 24) and (ByteOrder = riboLSBFirst)) or ((AlphaShift = BitsPerPixel - 8) and (RedShift = BitsPerPixel - 16) and (GreenShift = BitsPerPixel - 24) and (BlueShift = BitsPerPixel - 32) and (ByteOrder = riboMSBFirst)))) or
                ((BitsPerPixel = 32) and (AlphaPrec = 0) and (((RedShift = 8) and (GreenShift = 16) and (BlueShift = 24) and (ByteOrder = riboLSBFirst)) or ((RedShift = BitsPerPixel - 16) and (GreenShift = BitsPerPixel - 24) and (BlueShift = BitsPerPixel - 32) and (ByteOrder = riboMSBFirst))));
  end;

  function isBGRA: Boolean;
  begin
    with LazImage.RawImage.Description do
      Result := ((BitsPerPixel = 32) and (((BlueShift = 0) and (GreenShift = 8) and (RedShift = 16) and (ByteOrder = riboLSBFirst)) or ((BlueShift = BitsPerPixel - 8) and (GreenShift = BitsPerPixel - 16) and (RedShift = BitsPerPixel - 24) and (ByteOrder = riboMSBFirst))));
  end;

  function isBGR: Boolean;
  begin
    with LazImage.RawImage.Description do
      Result := ((BitsPerPixel = 24) and (((BlueShift = 0) and (GreenShift = 8) and (RedShift = 16) and (ByteOrder = riboLSBFirst)) or ((BlueShift = BitsPerPixel - 8) and (GreenShift = BitsPerPixel - 16) and (RedShift = BitsPerPixel - 24) and (ByteOrder = riboMSBFirst))));
  end;

var
  ChannelCount: Integer;
begin
  with LazImage.RawImage.Description do
  begin
    if ((BitsPerPixel and 7) <> 0) then
      SimbaException('%d bit per pixel found but multiple of 8bit expected', [BitsPerPixel]);
    if (BitsPerPixel < 24) then
      SimbaException('%d bit per pixel found but at least 24bit expected', [BitsPerPixel]);

    ChannelCount := 0;
    if (RedPrec > 0)   then Inc(ChannelCount);
    if (GreenPrec > 0) then Inc(ChannelCount);
    if (BluePrec > 0)  then Inc(ChannelCount);

    if (ChannelCount < 3) then
      SimbaException('%d color channels found but 3 or 4 expected.', [ChannelCount]);

    if isARGB() then
       Result := 'ARGB'
    else
    if isBGRA() then
       Result := 'BGRA'
    else
    if isBGR() then
       Result := 'BGR'
    else
      SimbaException(
        'Pixel format not supported: '                                               +
        'BitsPerPixel: ' + IntToStr(BitsPerPixel)                                    + ', ' +
        'RedShit: '      + IntToStr(RedShift)     + ', Prec: ' + IntToStr(RedPrec)   + ', ' +
        'GreenShit: '    + IntToStr(GreenShift)   + ', Prec: ' + IntToStr(GreenPrec) + ', ' +
        'BlueShift: '    + IntToStr(BlueShift)    + ', Prec: ' + IntToStr(BluePrec)  + ', ' +
        'AlphaShift: '   + IntToStr(AlphaShift)   + ', Prec: ' + IntToStr(AlphaPrec)
      );
  end;
end;

procedure SimbaImage_ToFPImageWriter(SimbaImage: TSimbaImage; WriterClass: TFPCustomImageWriterClass; Stream: TStream);
var
  Writer: TFPCustomImageWriter;
  Img: TLazIntfImage;
begin
  Img := nil;
  Writer := nil;

  try
    Img := TLazIntfImage.Create(SimbaImage_ToRawImage(SimbaImage), False);

    Writer := WriterClass.Create();
    Writer.ImageWrite(Stream, Img);
  finally
    if Assigned(Img) then
      Img.Free();
    if Assigned(Writer) then
      Writer.Free();
  end;
end;

procedure SimbaImage_FromFPImageReader(SimbaImage: TSimbaImage; ReaderClass: TFPCustomImageReaderClass; Stream: TStream);
var
  Reader: TFPCustomImageReader;
  Img: TLazIntfImage;
begin
  Img := nil;
  Reader := nil;

  try
    Img := TLazIntfImage.Create(0, 0);
    Img.DataDescription := SimbaRawImgDescription;

    Reader := ReaderClass.Create();
    Reader.ImageRead(Stream, Img);

    SimbaImage.LoadFromData(Img.Width, Img.Height, PColorBGRA(Img.PixelData), Img.Width);
  finally
    if Assigned(Img) then
      Img.Free();
    if Assigned(Reader) then
      Reader.Free();
  end;
end;

function SimbaImage_ToRawImage(SimbaImage: TSimbaImage): TRawImage;
begin
  Result.Init();

  Result.Description.PaletteColorCount := 0;
  Result.Description.MaskBitsPerPixel  := 0;
  Result.Description.Width             := SimbaImage.Width;
  Result.Description.Height            := SimbaImage.Height;

  Result.Description.Format       := ricfRGBA;
  Result.Description.ByteOrder    := riboLSBFirst;
  Result.Description.BitOrder     := riboBitsInOrder; // should be fine
  Result.Description.Depth        := 24;
  Result.Description.BitsPerPixel := 32;
  Result.Description.LineOrder    := riloTopToBottom;
  Result.Description.LineEnd      := rileDWordBoundary;

  Result.Description.RedPrec   := 8;
  Result.Description.GreenPrec := 8;
  Result.Description.BluePrec  := 8;
  Result.Description.AlphaPrec := 0;

  Result.Description.RedShift   := 16;
  Result.Description.GreenShift := 8;
  Result.Description.BlueShift  := 0;

  Result.DataSize := Result.Description.Width * Result.Description.Height * (Result.Description.BitsPerPixel shr 3);
  Result.Data     := PByte(SimbaImage.Data);
end;

function SimbaImage_ToLazImage(SimbaImage: TSimbaImage): TBitmap;
begin
  Result := TBitmap.Create();

  LazImage_FromData(Result, SimbaImage.Data, SimbaImage.Width, SimbaImage.Height);
end;

initialization
  SimbaRawImgDescription.Init_BPP32_B8G8R8_BIO_TTB(0, 0); // TColorBGRA format

end.

