{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Because bitmaps are not compressed we can load a select area without loading the entire image.
}
unit simba.image_bitmaparealoader;

{$DEFINE SIMBA_MAX_OPTIMIZATION}
{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.image;

procedure SimbaImage_LoadBitmapArea(Image: TSimbaImage; FileName: String; Area: TBox);

implementation

uses
  BMPcomn;

procedure SimbaImage_LoadBitmapArea(Image: TSimbaImage; FileName: String; Area: TBox);
var
  Stream: TFileStream;
  FileHeader: TBitmapFileHeader;
  Header: TBitmapInfoHeader;
  Row, PixelOffset: Integer;
  ScanLineSize: Int64;
  Buffer: PByte;
  BytesPerPixel: Byte;
  SrcPtr, DestPtr: Pointer;
  Upper, DestRowSize: PtrUInt;
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
      PixelOffset := FileHeader.bfOffset;
      ScanLineSize := Int64((Header.Width * Header.BitCount) + 31) div 32 * 4;
      Buffer := GetMem(ScanLineSize);

      Image.SetSize(Area.Width, Area.Height);
      DestPtr := Image.Data;
      DestRowSize := Area.Width * SizeOf(TColorBGRA);

      for Row := Area.Y1 to Area.Y2 do
      begin
        Stream.Position := PixelOffset + ((Header.Height - (Row + 1)) * ScanLineSize) + (Area.X1 * BytesPerPixel);
        Stream.Read(Buffer^, ScanLineSize);

        SrcPtr := Buffer;

        Upper := PtrUInt(DestPtr) + DestRowSize;
        while (PtrUInt(DestPtr) < Upper) do
        begin
          PColorRGB(DestPtr)^ := PColorRGB(SrcPtr)^; // dont care about alpha

          Inc(SrcPtr, BytesPerPixel);
          Inc(DestPtr, SizeOf(TColorBGRA));
        end;
      end;
    end;
  finally
    if (Buffer <> nil) then
      FreeMem(Buffer);

    Stream.Free();
  end;
end;

end.

