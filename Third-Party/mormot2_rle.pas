// https://github.com/synopse/mORMot2

/// Framework Core Shared Types and RTL-like Functions
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md

unit mormot2_rle;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TRleByteArray = array of Byte;

/// simple Run-Length-Encoding compression of a memory buffer
// - SynLZ is not good with input of a lot of redundant bytes, e.g. chunks of
// zeros: you could pre-process RleCompress/RleUnCompress such data before SynLZ
// - see AlgoRleLZ as such a RLE + SynLZ algorithm
// - returns the number of bytes written to dst, or -1 on dstsize overflow
function RleCompress(src, dst: PByte; srcsize, dstsize: PtrUInt): PtrInt;
function RleCompressDestLen(PlainLen: integer): integer;

/// simple Run-Length-Encoding uncompression of a memory buffer
// - SynLZ is not good with input of a lot of redundant bytes, e.g. chunks of
// zeros: you could pre-process RleCompress/RleUnCompress such data before SynLZ
// - see AlgoRleLZ as such a RLE + SynLZ algorithm
function RleUnCompress(src, dst: PByte; size: PtrUInt): PtrUInt;

/// partial Run-Length-Encoding uncompression of a memory buffer
function RleUnCompressPartial(src, dst: PByte; size, max: PtrUInt): PtrUInt;

function RleCompressSimple(src: PByte; size: integer): TRleByteArray;
function RleUnCompressSimple(src: TRleByteArray; uncompressedSize: Integer): TRleByteArray;

implementation

const
  RLE_CW = $5a; // any byte would do - this one is nothing special but for me

function RleEncode(dst: PByte; v, n: PtrUInt): PByte; inline;
begin
  if (n > 3) or
     (v = RLE_CW) then // encode as dst[0]=RLE_CW dst[1]=count dst[2]=value
  begin
    v := v shl 16;
    inc(v, RLE_CW);
    while n > 255 do
    begin
      PCardinal(dst)^ := v + 255 shl 8;
      dst := @dst[3];
      dec(n, 255);
    end;
    inc(v, n shl 8);
    result := @dst[3];
  end
  else
  begin
    inc(v, (v shl 8) + (v shl 16)); // append the value n (=1,2,3) times
    result := @dst[n]; // seems faster with branchless move
  end;
  PCardinal(dst)^ := v;
end;

function RleCompress(src, dst: PByte; srcsize, dstsize: PtrUInt): PtrInt;
var
  dststart: PAnsiChar;
  c, b, n: PtrUInt;
begin
  dststart := PAnsiChar(dst);
  if srcsize <> 0 then
  begin
    dstsize := PtrUInt(@dst[dstsize - 3]); // pointer(dstsize) = dstmax
    b := src[0];
    n := 0;
    repeat
      c := src[0];
      inc(PByte(src));
      if c = b then
      begin
        inc(n);
        dec(srcsize);
        if (srcsize = 0) or
           (PtrUInt(dst) >= PtrUInt(dstsize)) then
          break;
      end
      else // dedicated if n = 1 then .. branch was slower
      begin
        dst := RleEncode(dst, b, n);
        n := 1;
        b := c;
        dec(srcsize);
        if (srcsize = 0) or
           (PtrUInt(dst) >= PtrUInt(dstsize)) then
          break;
      end;
    until false;
    dst := RleEncode(dst, b, n);
    if PtrUInt(dst) >= PtrUInt(dstsize) then
    begin
      result := -1;
      exit;
    end;
  end;
  result := PAnsiChar(dst) - dststart;
end;

function RleUnCompress(src, dst: PByte; size: PtrUInt): PtrUInt;
var
  dststart: PAnsiChar;
  v: PtrUInt;
begin
  dststart := PAnsiChar(dst);
  if size > 0 then
    repeat
      v := src[0];
      if v <> RLE_CW then
      begin
        dst[0] := v;
        inc(PByte(dst));
        inc(PByte(src));
        dec(size);
        if size = 0 then
          break;
      end
      else
      begin // here src[0]=RLE_CW src[1]=count src[2]=value
        v := src[1];
        FillChar(dst^, v, src[2]);
        inc(PByte(dst), v);
        inc(PByte(src), 3);
        dec(size, 3);
        if PtrInt(size) <= 0 then
          break;
      end;
    until false;
  result := PAnsiChar(dst) - dststart;
end;

function RleUnCompressPartial(src, dst: PByte; size, max: PtrUInt): PtrUInt;
var
  dststart: PAnsiChar;
  v, m: PtrUInt;
begin
  dststart := PAnsiChar(dst);
  inc(max, PtrUInt(dst));
  while (size > 0) and
        (PtrUInt(dst) < max) do
  begin
    v := src[0];
    if v = RLE_CW then
    begin
      v := src[1];
      m := max - PtrUInt(dst);
      if v > m then
        v := m; // compile as cmov on FPC
      FillChar(dst^, v, src[2]);
      inc(PByte(dst), v);
      inc(PByte(src), 3);
      dec(size, 3);
    end
    else
    begin
      dst[0] := v;
      inc(PByte(dst));
      inc(PByte(src));
      dec(size);
    end;
  end;
  result := PAnsiChar(dst) - dststart;
end;

function RleCompressSimple(src: PByte; size: integer): TRleByteArray;
begin
  SetLength(Result, RleCompressDestLen(size));
  SetLength(Result, RleCompress(src, @Result[0], size, Length(Result)));
end;

function RleUnCompressSimple(src: TRleByteArray; uncompressedSize: Integer): TRleByteArray;
begin
  SetLength(Result, uncompressedSize);
  RleUnCompress(@src[0], @Result[0], Length(src));
end;

function RleCompressDestLen(PlainLen: integer): integer;
begin
  Result := PlainLen + 16;
end;

end.

