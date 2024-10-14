{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------

  Compress images fast using SynLZ.
}
unit simba.image_fastcompress;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.image;

procedure SimbaImage_FastCompress(Images: TSimbaImageArray; var Data: Pointer; out DataSize: SizeUInt);
function SimbaImage_FastDecompress(Data: Pointer; DataLen: SizeUInt): TSimbaImageArray;

implementation

uses
  SynLZ;

type
  PImageHeader = ^TImageHeader;
  TImageHeader = packed record
    Size: SizeUInt; // compressed size in bytes
    Width, Height: Integer;
  end;

procedure SimbaImage_FastCompress(Images: TSimbaImageArray; var Data: Pointer; out DataSize: SizeUInt);
var
  TotalSize: SizeUInt;
  I: Integer;
  Ptr: PByte;
begin
  DataSize := 0;
  TotalSize := SizeOf(Integer) + (Length(Images) * SizeOf(TImageHeader));
  for I := 0 to High(Images) do
    Inc(TotalSize, Images[I].DataSize);

  if (Data = nil) or (MemSize(Data) < SynLZcompressdestlen(TotalSize)) then
    ReAllocMem(Data, SynLZcompressdestlen(TotalSize));

  Ptr := Data;
  PInteger(Ptr)^ := Length(Images);
  Inc(Ptr, SizeOf(Integer) + (Length(Images) * SizeOf(TImageHeader)));

  for I := 0 to High(Images) do
    with PImageHeader(Data + SizeOf(Integer) + (I * SizeOf(TImageHeader)))^ do
    begin
      Width := Images[I].Width;
      Height := Images[I].Height;
      Size := SynLZcompress(PByte(Images[I].Data), Images[I].DataSize, Ptr);
      Inc(Ptr, Size);
      Inc(DataSize, Size);
    end;
end;

function SimbaImage_FastDecompress(Data: Pointer; DataLen: SizeUInt): TSimbaImageArray;
var
  Ptr: PByte;
  I: Integer;
begin
  Ptr := Data;
  SetLength(Result, PInteger(Ptr)^);
  Inc(Ptr, SizeOf(Integer) + (Length(Result) * SizeOf(TImageHeader)));

  for I := 0 to High(Result) do
    with PImageHeader(Data + SizeOf(Integer) + (I * SizeOf(TImageHeader)))^ do
    begin
      Result[I] := TSimbaImage.Create(Width, Height);
      SynLZdecompress(Ptr, Size, PByte(Result[I].Data));
      Inc(Ptr, Size);
    end;
end;

end.

