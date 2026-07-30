{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.compress_codec;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base;

type
  ECompressCodecException = class(ESimbaException);

  TCompressCodec = class
  public
    class procedure Compress(InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64); virtual; abstract; overload;
    class procedure Decompress(InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64); virtual; abstract; overload;

    class function Compress(InData: PByte; InSize: Int64): TByteArray; overload;
    class function Decompress(InData: PByte; InSize: Int64): TByteArray; overload;

    class function Compress(Bytes: TByteArray): TByteArray; overload;
    class function Decompress(Bytes: TByteArray): TByteArray; overload;

    class function Compress(Source: TStream; Count: Int64 = -1): TByteArray; overload;
    class function Decompress(Source: TStream; Count: Int64 = -1): TByteArray; overload;

    class procedure Compress(Source, Dest: TStream; Count: Int64 = -1); overload;
    class procedure Decompress(Source, Dest: TStream; Count: Int64 = -1); overload;
  end;

  TCompressCodecClass = class of TCompressCodec;

procedure CompressCodecException(Message: String; Args: array of const); overload;
procedure CompressCodecException(Message: String); overload;

implementation

procedure CompressCodecException(Message: String; Args: array of const);
begin
  raise ECompressCodecException.CreateFmt(Message, Args);
end;

procedure CompressCodecException(Message: String);
begin
  raise ECompressCodecException.Create(Message);
end;

function ReadWhole(Source: TStream; Count: Int64): TByteArray;
begin
  Result := nil;
  if (Count < 0) then
    Count := Source.Size - Source.Position;
  SetLength(Result, Count);
  if (Count > 0) then
    Source.ReadBuffer(Result[0], Count);
end;

class function TCompressCodec.Compress(InData: PByte; InSize: Int64): TByteArray;
var
  OutData: PByte;
  OutSize: Int64;
begin
  Result := nil;
  OutData := nil;
  try
    Compress(InData, InSize, OutData, OutSize);
    SetLength(Result, OutSize);
    if (OutSize > 0) then
      Move(OutData^, Result[0], OutSize);
  finally
    if (OutData <> nil) then
      FreeMem(OutData);
  end;
end;

class function TCompressCodec.Decompress(InData: PByte; InSize: Int64): TByteArray;
var
  OutData: PByte;
  OutSize: Int64;
begin
  Result := nil;
  OutData := nil;
  try
    Decompress(InData, InSize, OutData, OutSize);
    SetLength(Result, OutSize);
    if (OutSize > 0) then
      Move(OutData^, Result[0], OutSize);
  finally
    if (OutData <> nil) then
      FreeMem(OutData);
  end;
end;

class function TCompressCodec.Compress(Bytes: TByteArray): TByteArray;
begin
  Result := Compress(PByte(Pointer(Bytes)), Length(Bytes));
end;

class function TCompressCodec.Decompress(Bytes: TByteArray): TByteArray;
begin
  Result := Decompress(PByte(Pointer(Bytes)), Length(Bytes));
end;

class function TCompressCodec.Compress(Source: TStream; Count: Int64): TByteArray;
begin
  Result := Compress(ReadWhole(Source, Count));
end;

class function TCompressCodec.Decompress(Source: TStream; Count: Int64): TByteArray;
begin
  Result := Decompress(ReadWhole(Source, Count));
end;

class procedure TCompressCodec.Compress(Source, Dest: TStream; Count: Int64);
var
  Bytes: TByteArray;
begin
  Bytes := Compress(Source, Count);
  if (Length(Bytes) > 0) then
    Dest.WriteBuffer(Bytes[0], Length(Bytes));
end;

class procedure TCompressCodec.Decompress(Source, Dest: TStream; Count: Int64);
var
  Bytes: TByteArray;
begin
  Bytes := Decompress(Source, Count);
  if (Length(Bytes) > 0) then
    Dest.WriteBuffer(Bytes[0], Length(Bytes));
end;

end.
