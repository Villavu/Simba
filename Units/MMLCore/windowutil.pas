unit windowutil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$IFDEF LINUX},
  x, xlib, // For X* stuff.
  GraphType // For TRawImage
  {$ENDIF};

         {$IFDEF LINUX}
         Procedure XImageToRawImage(XImg: PXImage; Var RawImage: TRawImage);
         {$ENDIF}

implementation

{$IFDEF LINUX}
Procedure XImageToRawImage(XImg: PXImage; Var RawImage: TRawImage);
Begin
  RawImage.Init; { Calls raw.Description.Init as well }

  RawImage.Description.PaletteColorCount:=0;
  RawImage.Description.MaskBitsPerPixel:=0;
  RawImage.Description.Width := XImg^.width;
  RawImage.Description.Height:= XImg^.height;

  RawImage.Description.Format := ricfRGBA;

  if XImg^.byte_order = LSBFirst then
      RawImage.Description.ByteOrder := riboLSBFirst
  else
      RawImage.Description.ByteOrder:= riboMSBFirst;

  RawImage.Description.BitOrder:= riboBitsInOrder; // should be fine

  RawImage.Description.Depth:=XImg^.depth;

  RawImage.Description.BitsPerPixel:=XImg^.bits_per_pixel;

  RawImage.Description.LineOrder:=riloTopToBottom;

  RawImage.Description.LineEnd := rileDWordBoundary;

  RawImage.Description.RedPrec := 8;
  RawImage.Description.GreenPrec:= 8;
  RawImage.Description.BluePrec:= 8;
  RawImage.Description.AlphaPrec:=0;


  // Can be adjusted to the XImage RedMask, etc.
  // For now I just assume the tester uses BGR.
  RawImage.Description.RedShift:=16;
  RawImage.Description.GreenShift:=8;
  RawImage.Description.BlueShift:=0;

  RawImage.DataSize := RawImage.Description.Width * RawImage.Description.Height
                       * (RawImage.Description.bitsperpixel shr 3);
  //RawImage.DataSize := RawImage.Description.Height * RawImage.Description.BitsPerLine;
  RawImage.Data := PByte(XImg^.data);

End;
{$ENDIF}

end.

