{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.compress;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.encoding;

// TByteArray
function CompressBytes(Data: TByteArray): TByteArray;
function DeCompressBytes(Data: TByteArray): TByteArray;

// String
function CompressString(S: String; Encoding: BaseEncoding = BaseEncoding.b64): String;
function DeCompressString(S: String; Encoding: BaseEncoding = BaseEncoding.b64): String;

implementation

uses
  basenenc_simba, ZStream;

function CompressBytes(Data: TByteArray): TByteArray;
var
  InStream: Tcompressionstream;
  OutStream: TBytesStream;
begin
  OutStream := TBytesStream.Create();
  InStream := TCompressionStream.Create(clDefault, OutStream);
  try
    InStream.Write(Data[0], Length(Data));
    InStream.Flush();

    Result := OutStream.Bytes;
    SetLength(Result, OutStream.Size);
  finally
    InStream.Free();
    OutStream.Free();
  end;
end;

function DecompressBytes(Data: TByteArray): TByteArray;
var
  InStream: TBytesStream;
  OutStream: Tdecompressionstream;
  Count, Len: Integer;
begin
  SetLength(Result, 4096);
  Len := 0;

  InStream := TBytesStream.Create(Data);
  OutStream := TDeCompressionStream.Create(InStream);
  try
    repeat
      Count := OutStream.Read(Result[Len], Length(Result) - Len);
      Len := Len + Count;
    until (Count = 0);
  finally
    InStream.Free();
    OutStream.Free();
  end;

  SetLength(Result, Len);
end;

function CompressString(S: String; Encoding: BaseEncoding): String;
begin
  Result := BaseEncodeBytes(Encoding, CompressBytes(GetRawStringBytes(S)));
end;

function DeCompressString(S: String; Encoding: BaseEncoding): String;
begin
  Result := GetRawStringFromBytes(DeCompressBytes(BaseDecodeBytes(Encoding, S)));
end;

end.

