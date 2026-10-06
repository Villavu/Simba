{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Loading and saving images from file formats.
}
unit simba.image_file;

{$i simba.inc}

interface

uses
  Classes, SysUtils, FPImage,
  simba.base,
  simba.image;

procedure SimbaImage_Load(Image: TSimbaImage; FileName: String);
function SimbaImage_Save(Image: TSimbaImage; FileName: String; OverwriteIfExists: Boolean): Boolean;

// A plain bitmap has only that part read; anything else is loaded whole and cropped.
procedure SimbaImage_LoadArea(Image: TSimbaImage; FileName: String; X1, Y1, X2, Y2: Integer);
// Stream holds the image in the format FileName's extension names.
procedure SimbaImage_LoadStream(Image: TSimbaImage; Stream: TStream; FileName: String);
// Unzip a single (image) entry out of ZipFile.
procedure SimbaImage_LoadZip(Image: TSimbaImage; ZipFile, ZipEntry: String);
// The image in the RCDATA resource Name, in whichever format it holds.
procedure SimbaImage_LoadResource(Image: TSimbaImage; Name: String);

// X1,Y1,X2,Y2 of an uncompressed 24 or 32 bit bitmap
function SimbaImage_LoadBitmapArea(Image: TSimbaImage; Stream: TStream; X1, Y1, X2, Y2: Integer): Boolean;

procedure SimbaImage_LoadFPImage(Image: TSimbaImage; ReaderClass: TFPCustomImageReaderClass; Stream: TStream);
procedure SimbaImage_SaveFPImage(Image: TSimbaImage; WriterClass: TFPCustomImageWriterClass; Stream: TStream);

implementation

uses
  LCLType, Math, GraphType, IntfGraphics, FPReadBMP, FPWritePNG, BMPcomn,
  simba.vartype_box,
  simba.image_lazbridge,
  simba.image_utils,
  simba.zip;

procedure SimbaImage_Load(Image: TSimbaImage; FileName: String);
var
  Stream: TFileStream;
begin
  if (not FileExists(FileName)) then
    SimbaException('TImage.Load: File "%s" does not exist', [FileName]);

  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    SimbaImage_LoadStream(Image, Stream, FileName);
  finally
    Stream.Free();
  end;
end;

procedure SimbaImage_LoadArea(Image: TSimbaImage; FileName: String; X1, Y1, X2, Y2: Integer);
var
  Stream: TFileStream;
begin
  if (not FileExists(FileName)) then
    SimbaException('TImage.Load: File "%s" does not exist', [FileName]);

  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    // Bitmaps are not compressed, so an area can be read without the rest of the file.
    if FileName.EndsWith('.bmp', False) and SimbaImage_LoadBitmapArea(Image, Stream, X1, Y1, X2, Y2) then
      Exit;

    SimbaImage_LoadStream(Image, Stream, FileName);
  finally
    Stream.Free();
  end;

  X1 := EnsureRange(X1, 0, Image.Width - 1);
  Y1 := EnsureRange(Y1, 0, Image.Height - 1);
  X2 := EnsureRange(X2, 0, Image.Width - 1);
  Y2 := EnsureRange(Y2, 0, Image.Height - 1);
  if (Image.Width > 0) and (Image.Height > 0) and (X2 >= X1) and (Y2 >= Y1) then
    Image.Crop(TBox.Create(X1, Y1, X2, Y2))
  else
    Image.SetSize(0, 0);
end;

procedure SimbaImage_LoadStream(Image: TSimbaImage; Stream: TStream; FileName: String);
var
  ReaderClass: TFPCustomImageReaderClass;
begin
  // a plain bitmap reads fastest straight into the image, one row at a time
  if FileName.EndsWith('.bmp', False) and SimbaImage_LoadBitmapArea(Image, Stream, 0, 0, High(Int32), High(Int32)) then
    Exit;

  ReaderClass := TFPCustomImage.FindReaderFromFileName(FileName);
  if (ReaderClass = nil) then
    SimbaException('TImage.Load: Unknown image format "%s"', [FileName]);

  SimbaImage_LoadFPImage(Image, ReaderClass, Stream);
end;

procedure SimbaImage_LoadZip(Image: TSimbaImage; ZipFile, ZipEntry: String);
var
  Stream: TMemoryStream;
begin
  Stream := ZipExtractEntryToStream(ZipFile, ZipEntry);
  try
    SimbaImage_LoadStream(Image, Stream, ZipEntry);
  finally
    Stream.Free();
  end;
end;

procedure SimbaImage_LoadResource(Image: TSimbaImage; Name: String);
var
  Stream: TResourceStream;
  ReaderClass: TFPCustomImageReaderClass;
begin
  Stream := TResourceStream.Create(HINSTANCE, Name, RT_RCDATA);
  try
    // a resource has no file name to tell the format by
    ReaderClass := TFPCustomImage.FindReaderFromStream(Stream);
    if (ReaderClass = nil) then
      SimbaException('TImage.FromResource: Unknown image format in "%s"', [Name]);

    SimbaImage_LoadFPImage(Image, ReaderClass, Stream);
  finally
    Stream.Free();
  end;
end;

function SimbaImage_LoadBitmapArea(Image: TSimbaImage; Stream: TStream; X1, Y1, X2, Y2: Integer): Boolean;
var
  FileHeader: TBitmapFileHeader;
  Header: TBitmapInfoHeader;
  Masks: array[0..2] of UInt32;
  FileRow, BitmapHeight, BytesPerPixel, AreaWidth, AreaHeight: Integer;
  TopDown: Boolean;
  ScanLineSize, Step: Int64;
  Buffer: TByteArray;
  Src, SrcEnd: PByte;
  Dest, DestEnd: PColorBGRA;
begin
  Result := False;

  try
    if (Stream.Read(FileHeader, SizeOf(TBitmapFileHeader)) <> SizeOf(TBitmapFileHeader)) then
      Exit;
    {$IFDEF ENDIAN_BIG}
    SwapBMPFileHeader(FileHeader);
    {$ENDIF}
    if (FileHeader.bfType <> BMmagic) then
      Exit;
    if (Stream.Read(Header, SizeOf(TBitmapInfoHeader)) <> SizeOf(TBitmapInfoHeader)) then
      Exit;
    {$IFDEF ENDIAN_BIG}
    SwapBMPInfoHeader(Header);
    {$ENDIF}

    if (Header.Size < 40) or (FileHeader.bfOffset < SizeOf(TBitmapFileHeader) + Header.Size) then
      Exit;
    if (Header.BitCount <> 24) and (Header.BitCount <> 32) then
      Exit;
    if (Header.Compression <> BI_RGB) and (Header.Compression <> BI_BITFIELDS) then
      Exit;

    if (Header.Compression = BI_BITFIELDS) then
    begin
      Stream.ReadBuffer(Masks, SizeOf(Masks));
      {$IFDEF ENDIAN_BIG}
      Masks[0] := LEtoN(Masks[0]);
      Masks[1] := LEtoN(Masks[1]);
      Masks[2] := LEtoN(Masks[2]);
      {$ENDIF}
      if (Masks[0] <> $FF0000) or (Masks[1] <> $FF00) or (Masks[2] <> $FF) then
        Exit;
    end;

    TopDown := (Header.Height < 0); // rows are stored bottom up, unless the height is negative
    BitmapHeight := Abs(Header.Height);
    BytesPerPixel := Header.BitCount div 8;
    ScanLineSize := (Int64(Header.Width * Header.BitCount) + 31) div 32 * 4;

    Result := True;

    X1 := EnsureRange(X1, 0, Header.Width - 1);
    Y1 := EnsureRange(Y1, 0, BitmapHeight - 1);
    X2 := EnsureRange(X2, 0, Header.Width - 1);
    Y2 := EnsureRange(Y2, 0, BitmapHeight - 1);
    AreaWidth := X2 - X1 + 1;
    AreaHeight := Y2 - Y1 + 1;
    if (Header.Width <= 0) or (BitmapHeight = 0) or (AreaWidth <= 0) or (AreaHeight <= 0) then
    begin
      Image.SetSize(0, 0);
      Exit;
    end;

    if (BytesPerPixel = 3) then
      SetLength(Buffer, AreaWidth * 3);
    Image.SetSize(AreaWidth, AreaHeight);

    if TopDown then
    begin
      FileRow := Y1;
      Step := ScanLineSize - (AreaWidth * BytesPerPixel);
    end else
    begin
      FileRow := BitmapHeight - 1 - Y1;
      Step := -ScanLineSize - (AreaWidth * BytesPerPixel);
    end;
    Stream.Position := FileHeader.bfOffset + FileRow * ScanLineSize + X1 * BytesPerPixel;

    Dest := Image.Data;
    DestEnd := Dest + Image.PixelCount;
    while (Dest < DestEnd) do
    begin
      // already bgra
      if (BytesPerPixel = 4) then
      begin
        Stream.ReadBuffer(Dest^, AreaWidth * 4);
        Inc(Dest, AreaWidth);
      end else
      begin
        Stream.ReadBuffer(Buffer[0], Length(Buffer));
        Src := PByte(@Buffer[0]);
        SrcEnd := Src + Length(Buffer);
        while (Src < SrcEnd) do
        begin
          Dest^.B := Src[0];
          Dest^.G := Src[1];
          Dest^.R := Src[2];
          Dest^.A := ALPHA_OPAQUE;

          Inc(Src, 3);
          Inc(Dest);
        end;
      end;

      if (Dest < DestEnd) then
        Stream.Seek(Step, soCurrent);
    end;

    // A 32 bit bitmap's fourth byte is often unused
    if IsAlphaAll(Image.Data, Image.Width, Image.Height, ALPHA_TRANSPARENT) then
      Image.Canvas.FillWithAlpha(ALPHA_OPAQUE);
  finally
    Stream.Position := 0;
  end;
end;

procedure SimbaImage_LoadFPImage(Image: TSimbaImage; ReaderClass: TFPCustomImageReaderClass; Stream: TStream);
var
  Img: TLazIntfImage;
  Reader: TFPCustomImageReader;
  Desc: TRawImageDescription;
  IsBitmap: Boolean;
begin
  Desc.Init_BPP32_B8G8R8A8_BIO_TTB(0, 0);

  IsBitmap := ReaderClass.InheritsFrom(TFPReaderBMP);
  if IsBitmap then
    ReaderClass := TLazReaderBMP;

  Img := nil;
  Reader := nil;
  try
    Reader := ReaderClass.Create();
    Img := TLazIntfImage.Create(0, 0);
    Img.DataDescription := Desc;

    Reader.ImageRead(Stream, Img);

    Image.FromData(PColorBGRA(Img.PixelData), Img.Width, Img.Width, Img.Height);
  finally
    Img.Free();
    Reader.Free();
  end;

  if IsBitmap and IsAlphaAll(Image.Data, Image.Width, Image.Height, ALPHA_TRANSPARENT) then
    Image.Canvas.FillWithAlpha(ALPHA_OPAQUE);
end;

function SimbaImage_Save(Image: TSimbaImage; FileName: String; OverwriteIfExists: Boolean): Boolean;
var
  WriterClass: TFPCustomImageWriterClass;
  Stream: TFileStream;
begin
  if (Image.Width = 0) or (Image.Height = 0) then
    SimbaException('TImage.Save: Cannot save an empty image');
  if FileExists(FileName) and (not OverwriteIfExists) then
    SimbaException('TImage.Save: File already exists "%s"', [FileName]);

  WriterClass := TFPCustomImage.FindWriterFromFileName(FileName);
  if (WriterClass = nil) then
    SimbaException('TImage.Save: Unknown image format "%s"', [FileName]);

  Stream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
  try
    SimbaImage_SaveFPImage(Image, WriterClass, Stream);
  finally
    Stream.Free();
  end;

  Result := True;
end;

procedure SimbaImage_SaveFPImage(Image: TSimbaImage; WriterClass: TFPCustomImageWriterClass; Stream: TStream);
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

    Img := TLazIntfImage.Create(SimbaImage_ToRawImage(Image), False);

    Writer.ImageWrite(Stream, Img);
  finally
    Img.Free();
    Writer.Free();
  end;
end;

end.
