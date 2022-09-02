{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.stringutil;

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.mufasatypes;

function CompressString(const Str: String; Header: Boolean = True): String;
function DecompressString(const Str: String; Header: Boolean = True) : String;
function Base64Encode(const str : String) : String;
function Base64Decode(const str : String) : String;

implementation

uses
  base64, zstream;

function CompressString(const Str: String; Header: Boolean): String;
var
  Len: Integer;
  Output: TMemoryStream;
  Stream: TCompressionStream;
begin
  Result := '';

  Len := Length(Str);
  if (Len = 0) then
    Exit;

  Output := nil;
  Stream := nil;

  try
    Output := TMemoryStream.Create();
    if Header then
      Output.Write(Len, SizeOf(Integer)); // preappends the uncompressed string length for backwards compatibility (streams are now used - not needed)

    Stream := TCompressionStream.Create(clDefault, Output);
    Stream.Write(Str[1], Len);
    Stream.Flush();

    SetLength(Result, Output.Size);

    Output.Position := 0;
    Output.Read(Result[1], Output.Size);
  except
  end;

  if (Stream <> nil) then
    Stream.Free(); // Stream will free Output too
  if (Output <> nil) then
    Output.Free();
end;

function DecompressString(const Str: String; Header: Boolean): String;
var
  Input: TStringStream;
  Stream: TDeCompressionStream;
  Buffer: array[1..4096] of Char;
  Count: Integer;
begin
  Result := '';

  if (Str = '') then
    Exit;

  Input := nil;
  Stream := nil;

  try
    Input := TStringStream.Create(Str);
    if Header then
      Input.Position := 4; // skip preappended uncompressed string length. (not used anymore)

    Stream := TDeCompressionStream.Create(Input);

    repeat
      Count := Stream.Read(Buffer[1], Length(Buffer));
      if Count > 0 then
        Result := Result + System.Copy(Buffer, 1, Count);
    until Count = 0;
  except
  end;

  if (Stream <> nil) then
    Stream.Free();
  if (Input <> nil) then
    Input.Free();
end;

function Base64Encode(const str: String): String;
begin
  if (Str = '') then
    Result := ''
  else
    Result := EncodeStringBase64(str);
end;

function Base64Decode(const str: String): String;
begin
  if (Str = '') then
    Result := ''
  else
    Result := DecodeStringBase64(str);
end;

end.

