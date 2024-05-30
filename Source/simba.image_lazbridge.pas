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
  simba.base, simba.image, simba.colormath;

{$scopedenums on}
type
  ELazPixelFormat = (UNKNOWN, BGR, BGRA, RGB, RGBA, ARGB);
{$scopedenums off}

procedure LazImage_CopyRow_BGR(Source: PColorBGRA; SourceUpper: PtrUInt; Dest: PColorBGR); inline;
procedure LazImage_CopyRow_ARGB(Source: PColorBGRA; SourceUpper: PtrUInt; Dest: PColorARGB); inline;
procedure LazImage_CopyRow_BGRA(Source: PColorBGRA; SourceUpper: PtrUInt; Dest: PColorBGRA); inline;

procedure LazImage_FromData(LazImage: TBitmap; Data: PColorBGRA; Width, Height: Integer);
procedure LazImage_FromSimbaImage(LazImage: TBitmap; SimbaImage: TSimbaImage);
function LazImage_ToSimbaImage(LazImage: TBitmap): TSimbaImage;
function LazImage_PixelFormat(LazImage: TBitmap): ELazPixelFormat;

procedure SimbaImage_ToFPImageWriter(SimbaImage: TSimbaImage; WriterClass: TFPCustomImageWriterClass; Stream: TStream);
procedure SimbaImage_FromFPImageReader(SimbaImage: TSimbaImage; ReaderClass: TFPCustomImageReaderClass; Stream: TStream);
function SimbaImage_ToRawImage(SimbaImage: TSimbaImage): TRawImage;
function SimbaImage_ToLazImage(SimbaImage: TSimbaImage): TBitmap;

implementation

uses
  TypInfo, FPWritePNG;

procedure LazImage_CopyRow_BGR(Source: PColorBGRA; SourceUpper: PtrUInt; Dest: PColorBGR);
begin
  while (PtrUInt(Source) < SourceUpper) do
  begin
    PColorBGR(Dest)^ := PColorBGR(Source)^; // Can just use first three bytes

    Inc(Source);
    Inc(Dest);
  end;
end;

procedure LazImage_CopyRow_ARGB(Source: PColorBGRA; SourceUpper: PtrUInt; Dest: PColorARGB);
begin
  while (PtrUInt(Source) < SourceUpper) do
  begin
    PUInt32(Dest)^ := SwapEndian(PUInt32(Source)^); // reverse the bytes

    Inc(Source);
    Inc(Dest);
  end;
end;

procedure LazImage_CopyRow_BGRA(Source: PColorBGRA; SourceUpper: PtrUInt; Dest: PColorBGRA);
begin
  Move(Source^, Dest^, SourceUpper - PtrUInt(Source)); // same formats, copy whole row
end;

procedure LazImage_FromData(LazImage: TBitmap; Data: PColorBGRA; Width, Height: Integer);
var
  Source, Dest: PByte;
  SourceBytesPerLine, DestBytesPerLine: Integer;
  SourceUpper: PtrUInt;

  procedure BGRA;
  begin
    while (PtrUInt(Source) < SourceUpper) do
    begin
      LazImage_CopyRow_BGRA(PColorBGRA(Source), PtrUInt(Source + SourceBytesPerLine), PColorBGRA(Dest));

      Inc(Source, SourceBytesPerLine);
      Inc(Dest, DestBytesPerLine);
    end;
  end;

  procedure BGR;
  begin
    while (PtrUInt(Source) < SourceUpper) do
    begin
      LazImage_CopyRow_BGR(PColorBGRA(Source), PtrUInt(Source + SourceBytesPerLine), PColorBGR(Dest));

      Inc(Source, SourceBytesPerLine);
      Inc(Dest, DestBytesPerLine);
    end;
  end;

  procedure ARGB;
  begin
    while (PtrUInt(Source) < SourceUpper) do
    begin
      LazImage_CopyRow_ARGB(PColorBGRA(Source), PtrUInt(Source + SourceBytesPerLine), PColorARGB(Dest));

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
  SourceUpper := PtrUInt(Source + (SourceBytesPerLine * Height));

  case LazImage_PixelFormat(LazImage) of
    ELazPixelFormat.BGR:  BGR();
    ELazPixelFormat.BGRA: BGRA();
    ELazPixelFormat.ARGB: ARGB();
    else
      SimbaException('not supported');
  end;

  LazImage.EndUpdate();
end;

procedure LazImage_FromSimbaImage(LazImage: TBitmap; SimbaImage: TSimbaImage);
begin
  LazImage_FromData(LazImage, SimbaImage.Data, SimbaImage.Width, SimbaImage.Height);
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
    ELazPixelFormat.BGR:  BGR();
    ELazPixelFormat.BGRA: BGRA();
    ELazPixelFormat.ARGB: ARGB();
    else
      SimbaException('Not supported');
  end;
end;

function LazImage_PixelFormat(LazImage: TBitmap): ELazPixelFormat;

  function isRGBA: Boolean;
  begin
    with LazImage.RawImage.Description do
      Result := (BitsPerPixel = 32) and (Depth = 32) and (RedShift = 0) and (GreenShift = 8) and (BlueShift = 16) and (AlphaShift = 24);
  end;

  function isBGRA: Boolean;
  begin
    with LazImage.RawImage.Description do
      Result := ((BitsPerPixel = 32) and (Depth = 32) and (ByteOrder = riboLSBFirst) and (BlueShift = 0) and (GreenShift = 8) and (RedShift = 16) and (AlphaShift = 24)) or
                ((BitsPerPixel = 32) and (Depth = 24) and (ByteOrder = riboLSBFirst) and (BlueShift = 0) and (GreenShift = 8) and (RedShift = 16) and (AlphaShift = 0)); // 32bit but alpha not used
  end;

  function isARGB: Boolean;
  begin
    with LazImage.RawImage.Description do
      Result := ((BitsPerPixel = 32) and (Depth = 32) and (ByteOrder = riboMSBFirst) and (BlueShift = 0) and (GreenShift = 8) and (RedShift = 16) and (AlphaShift = 24)) or
                ((BitsPerPixel = 32) and (Depth = 24) and (ByteOrder = riboMSBFirst) and (BlueShift = 0) and (GreenShift = 8) and (RedShift = 16) and (AlphaShift = 0)); // 32bit but alpha not used
  end;

  function isBGR: Boolean;
  begin
    with LazImage.RawImage.Description do
      Result := (BitsPerPixel = 24) and (Depth = 24) and (ByteOrder = riboLSBFirst) and (BlueShift = 0) and (GreenShift = 8) and (RedShift = 16);
  end;

  function isRGB: Boolean;
  begin
    with LazImage.RawImage.Description do
      Result := (BitsPerPixel = 24) and (Depth = 24) and (ByteOrder = riboMSBFirst) and (BlueShift = 0) and (GreenShift = 8) and (RedShift = 16);
  end;

var
  ChannelCount: Integer;
begin
  Result := ELazPixelFormat.UNKNOWN;

  with LazImage.RawImage.Description do
  begin
    if ((BitsPerPixel and 7) <> 0) then
      SimbaException('LazImage_PixelFormat: %d BitsPerPixel found but expected multiple of 8', [BitsPerPixel]);
    if (BitsPerPixel < 24) or (BitsPerPixel > 32) then
      SimbaException('LazImage_PixelFormat: %d BitsPerPixel found but expected 24..32', [BitsPerPixel]);

    ChannelCount := 0;
    if (RedPrec > 0)   then Inc(ChannelCount);
    if (GreenPrec > 0) then Inc(ChannelCount);
    if (BluePrec > 0)  then Inc(ChannelCount);

    if (ChannelCount < 3) then
      SimbaException('LazImage_PixelFormat: %d channels found but 3 or 4 expected.', [ChannelCount]);

         if isRGBA() then Result := ELazPixelFormat.RGBA
    else if isBGRA() then Result := ELazPixelFormat.BGRA
    else if isARGB() then Result := ELazPixelFormat.ARGB
    else if isBGR()  then Result := ELazPixelFormat.BGR
    else if isRGB()  then Result := ELazPixelFormat.RGB
    else
      SimbaException(
        'LazImage_PixelFormat: Pixel format not supported.'                              +
        'ByteOrder: '    + GetEnumName(TypeInfo(TRawImageByteOrder), Integer(ByteOrder)) + ', ' +
        'Depth: '        + IntToStr(Depth)                                               + ', ' +
        'BitsPerPixel: ' + IntToStr(BitsPerPixel)                                        + ', ' +
        'RedShit: '      + IntToStr(RedShift)     + ', Prec: ' + IntToStr(RedPrec)       + ', ' +
        'GreenShit: '    + IntToStr(GreenShift)   + ', Prec: ' + IntToStr(GreenPrec)     + ', ' +
        'BlueShift: '    + IntToStr(BlueShift)    + ', Prec: ' + IntToStr(BluePrec)      + ', ' +
        'AlphaShift: '   + IntToStr(AlphaShift)   + ', Prec: ' + IntToStr(AlphaPrec)
      );
  end;
end;

procedure SimbaImage_ToFPImageWriter(SimbaImage: TSimbaImage; WriterClass: TFPCustomImageWriterClass; Stream: TStream);
var
  Img: TLazIntfImage;
  Writer: TFPCustomImageWriter;
begin
  Img := nil;
  Writer := nil;
  try
    Writer := WriterClass.Create();
    if (Writer is TFPWriterPNG) then
    begin
      TFPWriterPNG(Writer).WordSized := False;
      TFPWriterPNG(Writer).UseAlpha := True;
    end;

    Img := TLazIntfImage.Create(SimbaImage_ToRawImage(SimbaImage), False);

    Writer.ImageWrite(Stream, Img);
  finally
    if Assigned(Img) then
      Img.Free();
  end;
end;

procedure SimbaImage_FromFPImageReader(SimbaImage: TSimbaImage; ReaderClass: TFPCustomImageReaderClass; Stream: TStream);
var
  Img: TLazIntfImage;
  Reader: TFPCustomImageReader;
  Desc: TRawImageDescription;
begin
  Desc.Init_BPP32_B8G8R8A8_BIO_TTB(0, 0);

  Img := nil;
  Reader := nil;
  try
    Reader := ReaderClass.Create();
    Img := TLazIntfImage.Create(0, 0);
    Img.DataDescription := Desc;

    Reader.ImageRead(Stream, Img);

    SimbaImage.FromData(Img.Width, Img.Height, PColorBGRA(Img.PixelData), Img.Width);
  finally
    Img.Free();
    Reader.Free();
  end;
end;

function SimbaImage_ToRawImage(SimbaImage: TSimbaImage): TRawImage;
begin
  Result.Init();
  Result.Description.Init_BPP32_B8G8R8A8_BIO_TTB(SimbaImage.Width, SimbaImage.Height);
  Result.DataSize := Result.Description.Width * Result.Description.Height * SizeOf(TColorBGRA);
  Result.Data     := PByte(SimbaImage.Data);
end;

function SimbaImage_ToLazImage(SimbaImage: TSimbaImage): TBitmap;
begin
  Result := TBitmap.Create();

  LazImage_FromData(Result, SimbaImage.Data, SimbaImage.Width, SimbaImage.Height);
end;

end.

