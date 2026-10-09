{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Convert TSimbaImage to TBitmap and back.
}
unit simba.image_lazbridge;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics, GraphType,
  simba.base, simba.image, simba.colormath;

{$scopedenums on}
type
  ELazPixelFormat = (UNKNOWN, BGR, BGRA, RGB, RGBA, ARGB);
{$scopedenums off}

procedure LazImage_CopyRow_BGR(Source: PColorBGRA; SourceUpper: PtrUInt; Dest: PColorBGR); inline;
procedure LazImage_CopyRow_ARGB(Source: PColorBGRA; SourceUpper: PtrUInt; Dest: PColorARGB); inline;

// SourceBytesPerLine=0 means packed
procedure LazImage_FromData(LazImage: TBitmap; Data: PColorBGRA; Width, Height: Integer; SourceBytesPerLine: SizeInt = 0);
procedure LazImage_FromSimbaImage(LazImage: TBitmap; SimbaImage: TSimbaImage);
function LazImage_ToSimbaImage(LazImage: TBitmap): TSimbaImage;
function LazImage_PixelFormat(LazImage: TBitmap): ELazPixelFormat;

function SimbaImage_ToRawImage(SimbaImage: TSimbaImage): TRawImage;
function SimbaImage_ToLazImage(SimbaImage: TSimbaImage): TBitmap;

implementation

uses
  TypInfo,
  simba.image_utils;

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

procedure LazImage_FromData(LazImage: TBitmap; Data: PColorBGRA; Width, Height: Integer; SourceBytesPerLine: SizeInt);
var
  Source, Dest: PByte;
  DestBytesPerLine, RowBytes: Integer;
  SourceUpper: PtrUInt;

  procedure BGR;
  begin
    while (PtrUInt(Source) < SourceUpper) do
    begin
      LazImage_CopyRow_BGR(PColorBGRA(Source), PtrUInt(Source + RowBytes), PColorBGR(Dest));

      Inc(Source, SourceBytesPerLine);
      Inc(Dest, DestBytesPerLine);
    end;
  end;

  procedure ARGB;
  begin
    while (PtrUInt(Source) < SourceUpper) do
    begin
      LazImage_CopyRow_ARGB(PColorBGRA(Source), PtrUInt(Source + RowBytes), PColorARGB(Dest));

      Inc(Source, SourceBytesPerLine);
      Inc(Dest, DestBytesPerLine);
    end;
  end;

begin
  if (SourceBytesPerLine <= 0) then
    SourceBytesPerLine := SizeInt(Width * SizeOf(TColorBGRA));

  LazImage.BeginUpdate();
  LazImage.SetSize(Width, Height);

  Dest := LazImage.RawImage.Data;
  DestBytesPerLine := LazImage.RawImage.Description.BytesPerLine;

  Source := PByte(Data);
  RowBytes := Width * SizeOf(TColorBGRA);
  SourceUpper := PtrUInt(Source + (SourceBytesPerLine * Height));

  case LazImage_PixelFormat(LazImage) of
    ELazPixelFormat.BGR:  BGR();
    ELazPixelFormat.BGRA: CopyRows(PColorBGRA(Dest), DestBytesPerLine, Data, SourceBytesPerLine, Width, Height);
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

  procedure BGR(SourcePtr, DestPtr: PByte; const DestUpper: PtrUInt; const SourceRowSize, DestRowSize: Integer);
  var
    Ptr: PByte;
    RowUpper: PtrUInt;
  begin
    while (PtrUInt(DestPtr) < DestUpper) do
    begin
      RowUpper := PtrUInt(DestPtr) + DestRowSize;
      Ptr := SourcePtr;
      while (PtrUInt(DestPtr) < RowUpper) do
      begin
        PColorBGR(DestPtr)^ := PColorBGR(Ptr)^; // can just use first three bytes

        Inc(Ptr, SizeOf(TColorRGB));
        Inc(DestPtr, SizeOf(TColorBGRA));
      end;
      Inc(SourcePtr, SourceRowSize);
    end;
  end;

  procedure ARGB(SourcePtr, DestPtr: PByte; const DestUpper: PtrUInt; const SourceRowSize, DestRowSize: Integer);
  var
    Ptr: PByte;
    RowUpper: PtrUInt;
  begin
    while (PtrUInt(DestPtr) < DestUpper) do
    begin
      RowUpper := PtrUInt(DestPtr) + DestRowSize;
      Ptr := SourcePtr;
      while (PtrUInt(DestPtr) < RowUpper) do
      begin
        PUInt32(DestPtr)^ := SwapEndian(PUInt32(Ptr)^); // reverse the bytes

        Inc(Ptr, SizeOf(TColorARGB));
        Inc(DestPtr, SizeOf(TColorBGRA));
      end;
      Inc(SourcePtr, SourceRowSize);
    end;
  end;

var
  Source, Dest: PByte;
  SourceRowSize, DestRowSize, DestUpper: PtrUInt;
begin
  if not (LazImage_PixelFormat(LazImage) in [ELazPixelFormat.BGR, ELazPixelFormat.BGRA, ELazPixelFormat.ARGB]) then // before Result is made: nothing to free
    SimbaException('Not supported');

  Result := TSimbaImage.Create();
  Result.SetSize(LazImage.Width, LazImage.Height);

  Dest := PByte(Result.Data);
  DestUpper := PtrUInt(@Result.Data[Result.PixelCount]);
  DestRowSize := LazImage.Width * SizeOf(TColorBGRA);

  Source := LazImage.RawImage.Data;
  SourceRowSize := LazImage.RawImage.Description.BytesPerLine;

  case LazImage_PixelFormat(LazImage) of
    ELazPixelFormat.BGR:  BGR(Source, Dest, DestUpper, SourceRowSize, DestRowSize);
    ELazPixelFormat.BGRA: CopyRows(Result.Data, Result.BytesPerRow, PColorBGRA(Source), SourceRowSize, Result.Width, Result.Height);
    ELazPixelFormat.ARGB: ARGB(Source, Dest, DestUpper, SourceRowSize, DestRowSize);
  end;

  // 32 bits a pixel with no alpha declared: the fourth byte is padding, often zero, and never alpha
  if (LazImage.RawImage.Description.BitsPerPixel = 32) and (LazImage.RawImage.Description.AlphaPrec = 0) then
    Result.Canvas.FillWithAlpha(ALPHA_OPAQUE);
end;

function LazImage_PixelFormat(LazImage: TBitmap): ELazPixelFormat;

  function isRGBA: Boolean;
  begin
    with LazImage.RawImage.Description do
      Result := ((BitsPerPixel = 32) and (Depth = 32) and (ByteOrder = riboLSBFirst) and (RedShift = 0) and (GreenShift = 8) and (BlueShift = 16) and (AlphaShift = 24)) or
                ((BitsPerPixel = 32) and (Depth = 24) and (ByteOrder = riboLSBFirst) and (RedShift = 0) and (GreenShift = 8) and (BlueShift = 16) and (AlphaShift in [0,24])); // 32bit but alpha not used
  end;

  function isBGRA: Boolean;
  begin
    with LazImage.RawImage.Description do
      Result := ((BitsPerPixel = 32) and (Depth = 32) and (ByteOrder = riboLSBFirst) and (BlueShift = 0) and (GreenShift = 8) and (RedShift = 16) and (AlphaShift = 24)) or
                ((BitsPerPixel = 32) and (Depth = 24) and (ByteOrder = riboLSBFirst) and (BlueShift = 0) and (GreenShift = 8) and (RedShift = 16) and (AlphaShift in [0, 24])); // 32bit but alpha not used
  end;

  function isARGB: Boolean;
  begin
    with LazImage.RawImage.Description do
      Result := ((BitsPerPixel = 32) and (Depth = 32) and (ByteOrder = riboMSBFirst) and (BlueShift = 0) and (GreenShift = 8) and (RedShift = 16) and (AlphaShift = 24)) or
                ((BitsPerPixel = 32) and (Depth = 24) and (ByteOrder = riboMSBFirst) and (BlueShift = 0) and (GreenShift = 8) and (RedShift = 16) and (AlphaShift in [0, 24])); // 32bit but alpha not used
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
        'LazImage_PixelFormat: Pixel format not supported. '                             +
        'ByteOrder: '    + GetEnumName(TypeInfo(TRawImageByteOrder), Integer(ByteOrder)) + ', ' +
        'Depth: '        + IntToStr(Depth)                                               + ', ' +
        'BitsPerPixel: ' + IntToStr(BitsPerPixel)                                        + ', ' +
        'RedShift: '     + IntToStr(RedShift)     + ', Prec: ' + IntToStr(RedPrec)       + ', ' +
        'GreenShift: '   + IntToStr(GreenShift)   + ', Prec: ' + IntToStr(GreenPrec)     + ', ' +
        'BlueShift: '    + IntToStr(BlueShift)    + ', Prec: ' + IntToStr(BluePrec)      + ', ' +
        'AlphaShift: '   + IntToStr(AlphaShift)   + ', Prec: ' + IntToStr(AlphaPrec)
      );
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
  try
    LazImage_FromData(Result, SimbaImage.Data, SimbaImage.Width, SimbaImage.Height);
  except
    Result.Free();
    raise;
  end;
end;

end.

