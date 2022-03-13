{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.bitmap_misc;

{$i simba.inc}

interface

uses
  classes, sysutils, graphics,
  simba.mufasatypes, simba.bitmap;

procedure LoadBitmapAreaFromFile(Bitmap: TMufasaBitmap; FileName: String; Area: TBox);
function GetBitmapPixelFormat(Bitmap: TBitmap): String;

implementation

uses
  BMPcomn, GraphType;

procedure LoadBitmapAreaFromFile(Bitmap: TMufasaBitmap; FileName: String; Area: TBox);

  function ColorRGBToBGRA(const ColorRGB: TColorRGB): TRGB32; inline;
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
      ScanLineSize := ((Header.Width * Header.BitCount) + 31) div 32 * 4;
      Buffer := GetMem(ScanLineSize);
      Index := 0;

      for Row := Area.Y1 to Area.Y2 - 1 do
      begin
        Stream.Position := PixelOffset + ((Header.Height - Row) * ScanLineSize) + (Area.X1 * BytesPerPixel);
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
var
  R, G, B, A: Byte;
begin
  Result := '';

  if (Bitmap.RawImage.Description.Format = ricfRGBA) then
  begin
    Bitmap.RawImage.Description.GetRGBIndices(R, G, B, A);

    case Bitmap.RawImage.Description.Depth of
      32:
        begin
          SetLength(Result, 4);

          Result[R+1] := 'R';
          Result[G+1] := 'G';
          Result[B+1] := 'B';
          Result[A+1] := 'A';
        end;

      24:
        begin
          SetLength(Result, 3);

          Result[R+1] := 'R';
          Result[G+1] := 'G';
          Result[B+1] := 'B';
        end;
    end;
  end;
end;

end.

