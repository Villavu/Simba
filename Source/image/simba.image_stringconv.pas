{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------

  Save/Load bitmaps from/to a base64 string.

  Strings are generated as PNG with a small header.
}
unit simba.image_stringconv;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.image;

const
  HeaderPrefix = 'IMG:';

type
  TImageStringHeader = packed record
    Version: Integer;
    Width, Height: Integer;
    Name: String[128];
  end;

procedure SimbaImage_FromString(Image: TSimbaImage; const Str: String);
function SimbaImage_ToString(Image: TSimbaImage): String;

implementation

uses
  FPReadPNG, FPWritePNG,
  simba.encoding, simba.image_lazbridge;

procedure SimbaImage_FromString(Image: TSimbaImage; const Str: String);
var
  Stream: TStringStream;
  Header: TImageStringHeader;
begin
  if not Str.StartsWith(HeaderPrefix, True) then
    SimbaException('TImage.FromString: Invalid string, should begin with "IMG:"');
  Str.DeleteRange(1, Length(HeaderPrefix));

  Stream := nil;
  try
    Stream := TStringStream.Create(BaseDecode(BaseEncoding.b64, Str));
    Stream.Read(Header, SizeOf(TImageStringHeader));

    Image.Name := Header.Name;

    SimbaImage_FromFPImageReader(Image, TFPReaderPNG, Stream);
  finally
    Stream.Free();
  end;
end;

function SimbaImage_ToString(Image: TSimbaImage): String;
var
  HeaderStream, ImageStream: TStringStream;
  Header: TImageStringHeader;
begin
  HeaderStream := nil;
  ImageStream := nil;

  try
    Header.Version := 1;
    Header.Width   := Image.Width;
    Header.Height  := Image.Height;
    Header.Name    := Image.Name;

    HeaderStream := TStringStream.Create();
    HeaderStream.Write(Header, SizeOf(TImageStringHeader));
    ImageStream := TStringStream.Create();

    SimbaImage_ToFPImageWriter(Image, TFPWriterPNG, ImageStream);

    Result := HeaderPrefix + BaseEncode(BaseEncoding.b64, HeaderStream.DataString + ImageStream.DataString);
  finally
    HeaderStream.Free();
    ImageStream.Free();
  end;
end;

end.

