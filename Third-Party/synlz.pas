/// SynLZ Compression routines
// - licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynLZ;

{$mode objfpc}{$H+}

{
    This file is part of Synopse SynLZ Compression.

    Synopse SynLZ Compression. Copyright (C) 2022 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse SynLZ Compression.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2022
  the Initial Developer. All Rights Reserved.

  Contributor(s):

  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****


     SynLZ Compression / Decompression library
     =========================================

    * SynLZ is a very FAST lossless data compression library
      written in optimized pascal code for FPC and Delphi 3 and up
      with a tuned asm version available
    * symetrical compression and decompression speed (which is
      very rare above all other compression algorithms in the wild)
    * good compression rate (usualy better than LZO)
    * fastest averrage compression speed (ideal for xml/text communication, e.g.)

    SynLZ implements a new LZ compression algorithm with the following features:
    * hashing+dictionary compression in one pass, with no huffman table
    * optimized 32bits control word, embedded in the data stream
    * in-memory compression (the dictionary is the input stream itself)
    * compression and decompression have the same speed (both use hashing)
    * thread safe and lossless algorithm
    * supports overlapping compression and in-place decompression
    * code size for compression/decompression functions is smaller than LZO's

    Implementation notes:
    - this format is NOT stream compatible with any lz* official format
       => meant for proprietary server-side content real-time compression
       => use it internally in your application, not as exchange format
       => consider our SynLizard.pas unit for Lizard (LZ5) compression standard
    - very small code size (less than 1KB for both compressor/decompressor)
    - the uncompressed data length is stored in the beginning of the stream
       and can be retrieved easily for proper out_p memory allocation
    - please give correct data to the decompressor (i.e. first CRC in_p data)
       => we recommend crc32c() from SynCommons, or a zip-like container
    - a 2nd more tuned algorithm is included, but is somewhat slower in practice
       => use SynLZ[de]compres1*() functions in your applications
    - tested and benchmarked with a lot of data types/sizes
       => use the asm code, which is very tuned: SynLZ[de]compress1asm()
    - a hashing limitation makes SynLZ sometimes unable to pack continuous
       blocks of same byte -> SynLZ is perfect for xml/text (e.g. log files),
       but SynZip or SynLizard may be prefered for database files
    - if you include it in your application, please give me some credits:
       "use SynLZ compression by https://synopse.info"
    - use at your own risk!

  Benchmark update - introducing LZ4 at http://code.google.com/p/lz4
  190 MB file containing pascal sources, on a Core 2 duo PC, using x86 asm:
   LZ4     compression = 1.25 sec, comp. size = 71 MB, decompression = 0.44 sec
   SynLZ   compression = 1.09 sec, comp. size = 63 MB, decompression = 0.51 sec
   zip (1) compression = 6.44 sec, comp. size = 52 MB, decompression = 1.49 sec
   zip (6) compression = 20.1 sec, comp. size = 42 MB, decompression = 1.35 sec
   Note: zip decompression here uses fast asm optimized version of SynZip.pas
  Decompression is slower in SynLZ, due to the algorithm used: it does recreate
   the hash table even at decompression, while it is not needed by LZ4.
  Having the hash table at hand allows more patterns to be available, so
   compression ratio is better, at the expand of a slower speed.

  Conclusion:
   SynLZ compresses better than LZ4, SynLZ is faster to compress than LZ4,
   but slower to decompress than LZ4. So SynLZ is still very competitive for
   our Client-Server mORMot purpose, since it is a simple pascal unit with
   no external .obj/.o/.dll dependency. ;)

  Updated benchmarks on a Core i7, with the 2017/08 x86 and x64 optimized asm:
    Win32 Processing devpcm.log = 98.7 MB
       Snappy compress in 125.07ms, ratio=84%, 789.3 MB/s
       Snappy uncompress in 70.35ms, 1.3 GB/s
       SynLZ compress in 103.61ms, ratio=93%, 952.8 MB/s
       SynLZ uncompress in 68.71ms, 1.4 GB/s
    Win64 Processing devpcm.log = 98.7 MB
       Snappy compress in 107.13ms, ratio=84%, 921.5 MB/s
       Snappy uncompress in 61.06ms, 1.5 GB/s
       SynLZ compress in 97.25ms, ratio=93%, 1015.1 MB/s
       SynLZ uncompress in 61.27ms, 1.5 GB/s

}

interface

/// get maximum possible (worse) compressed size for out_p
function SynLZcompressdestlen(in_len: integer): integer;

/// get uncompressed size from lz-compressed buffer (to reserve memory, e.g.)
function SynLZdecompressdestlen(in_p: PAnsiChar): integer;

/// 1st compression algorithm uses hashing with a 32bits control word
function SynLZcompress1pas(src: PAnsiChar; size: integer; dst: PAnsiChar): integer;

/// 1st compression algorithm uses hashing with a 32bits control word
// - this is the fastest pure pascal implementation
function SynLZdecompress1pas(src: PAnsiChar; size: integer; dst: PAnsiChar): integer;

/// 1st compression algorithm uses hashing with a 32bits control word
// - this overload function is slower, but will allow to uncompress only the start
// of the content (e.g. to read some metadata header)
// - it will also check for dst buffer overflow, so will be more secure than
// other functions, which expect the content to be verified (e.g. via CRC)
function SynLZdecompress1partial(src: PAnsiChar; size: integer; dst: PAnsiChar; maxDst: integer): integer;

implementation

{$R-}
{$Q-}

function SynLZcompressdestlen(in_len: integer): integer;
begin // get maximum possible (worse) compressed size for out_p
  result := in_len+in_len shr 3+16;
end;

type // some cross-platform and cross-compiler definitions
  TOffsets = array[0..4095] of PAnsiChar; // 16KB/32KB hashing code

function SynLZdecompressdestlen(in_p: PAnsiChar): integer;
begin // get uncompressed size from lz-compressed buffer (to reserve memory, e.g.)
  result := PWord(in_p)^;
  if result and $8000<>0 then
    result := (result and $7fff) or (integer(PWord(in_p+2)^) shl 15);
end;

function SynLZcompress1pas(src: PAnsiChar; size: integer; dst: PAnsiChar): integer;
var dst_beg,          // initial dst value
    src_end,          // real last byte available in src
    src_endmatch,     // last byte to try for hashing
    o: PAnsiChar;
    CWbit: byte;
    CWpoint: PCardinal;
    v, h, cached, t, tmax: PtrUInt;
    offset: TOffsets;
    cache: array[0..4095] of cardinal; // 16KB+16KB=32KB on stack (48KB under Win64)
begin
  dst_beg := dst;
  // 1. store in_len
  if size>=$8000 then begin // size in 32KB..2GB -> stored as integer
    PWord(dst)^ := $8000 or (size and $7fff);
    PWord(dst+2)^ := size shr 15;
    inc(dst,4);
  end else begin
    PWord(dst)^ := size ; // size<32768 -> stored as word
    if size=0 then begin
      result := 2;
      exit;
    end;
    inc(dst,2);
  end;
  // 2. compress
  src_end := src+size;
  src_endmatch := src_end-(6+5);
  CWbit := 0;
  CWpoint := pointer(dst);
  PCardinal(dst)^ := 0;
  inc(dst,sizeof(CWpoint^));
  fillchar(offset{%H-},sizeof(offset),0); // fast 16KB reset to 0
  // 1. main loop to search using hash[]
  if src<=src_endmatch then
  repeat
    v := PCardinal(src)^;
    h := ((v shr 12) xor v) and 4095;
    o := offset[h];
    offset[h] := src;
    cached := v xor {%H-}cache[h]; // o=nil if cache[h] is uninitialized
    cache[h] := v;
    if (cached and $00ffffff=0) and (o<>nil) and (src-o>2) then begin
      CWpoint^ := CWpoint^ or (cardinal(1) shl CWbit);
      inc(src,2);
      inc(o,2);
      t := 1;
      tmax := src_end-src-1;
      if tmax>=(255+16) then
        tmax := (255+16);
      while (o[t]=src[t]) and (t<tmax) do
        inc(t);
      inc(src,t);
      h := h shl 4;
      // here we have always t>0
      if t<=15 then begin // mark 2 to 17 bytes -> size=1..15
        PWord(dst)^ := integer(t or h);
        inc(dst,2);
      end else begin // mark 18 to (255+16) bytes -> size=0, next byte=t
        dec(t,16);
        PWord(dst)^ := h; // size=0
        dst[2] := ansichar(t);
        inc(dst,3);
      end;
    end else begin
      dst^ := src^;
      inc(src);
      inc(dst);
    end;
    if CWbit<31 then begin
      inc(CWbit);
      if src<=src_endmatch then continue else break;
    end else begin
      CWpoint := pointer(dst);
      PCardinal(dst)^ := 0;
      inc(dst,sizeof(CWpoint^));
      CWbit := 0;
      if src<=src_endmatch then continue else break;
    end;
  until false;
  // 2. store remaining bytes
  if src<src_end then
  repeat
    dst^ := src^;
    inc(src);
    inc(dst);
    if CWbit<31 then begin
      inc(CWbit);
      if src<src_end then continue else break;
    end else begin
      PCardinal(dst)^ := 0;
      inc(dst,4);
      CWbit := 0;
      if src<src_end then continue else break;
    end;
  until false;
  result := dst-dst_beg;
end;

procedure movechars(s,d: PAnsiChar; t: PtrUInt); {$ifdef HASINLINE}inline;{$endif}
// fast code for unaligned and overlapping (see {$define WT}) small blocks
// this code is sometimes used rather than system.move()
var c: AnsiChar; // better code generation on FPC
begin
  inc(PtrUInt(s), t);
  inc(PtrUInt(d), t);
  PtrInt(t) := -PtrInt(t);
  repeat
    c := s[t];
    d[t] := c;
    inc(t);
  until t=0;
end;

const
  bitlut: array[0..15] of integer =
    (4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0);

function SynLZdecompress1b(src: PAnsiChar; size: integer; dst: PAnsiChar): integer;
// this routine was trying to improve speed, but was slower
var last_hashed: PAnsiChar; // initial src and dst value
    src_end: PAnsiChar;
    CWbit: integer;
    CW, v, t, h: integer;
    offset: TOffsets;
label nextCW;
begin
//  src_beg := src;
//  dst_beg := dst;
  src_end := src+size;
  // 1. retrieve out_len
  result := PWord(src)^;
  if result=0 then exit;
  inc(src,2);
  if result and $8000<>0 then begin
    result := (result and $7fff) or (integer(PWord(src)^) shl 15);
    inc(src,2);
  end;
  // 2. decompress
  last_hashed := dst-1;
  CWbit := 32;
nextCW:
  CW := PCardinal(src)^;
  inc(src,4);
  CWbit := CWbit-32;
  if src<src_end then
  repeat
    if CW and 1=0 then begin
      if CWbit<(32-4) then begin
        PCardinal(dst)^ := PCardinal(src)^;
        v := bitlut[CW and 15];
        inc(src,v);
        inc(dst,v);
        inc(CWbit,v);
        CW := CW shr v;
        if src>=src_end then break;
        while last_hashed<dst-3 do begin
          inc(last_hashed);
          v := PCardinal(last_hashed)^;
          offset[((v shr 12) xor v) and 4095] := last_hashed;
        end;
      end else begin
        dst^ := src^;
        inc(src);
        inc(dst);
        if src>=src_end then break;
        if last_hashed<dst-3 then begin
          inc(last_hashed);
          v := PCardinal(last_hashed)^;
          offset[((v shr 12) xor v) and 4095] := last_hashed;
        end;
        inc(CWbit);
        CW := CW shr 1;
        if CWbit<32 then
          continue else
          goto nextCW;
      end;
    end else begin
      h := PWord(src)^;
      inc(src,2);
      t := (h and 15)+2;
      h := h shr 4;
      if t=2 then begin
        t := ord(src^)+(16+2);
        inc(src);
      end;
      if dst-{%H-}offset[h]<t then
        movechars(offset[h],dst,t) else
        move(offset[h]^,dst^,t);
      if last_hashed<dst then
        repeat
          inc(last_hashed);
          v := PCardinal(last_hashed)^;
          offset[((v shr 12) xor v) and 4095] := last_hashed;
        until last_hashed>=dst;
      inc(dst,t);
      if src>=src_end then break;
      last_hashed := dst-1;
      inc(CWbit);
      CW := CW shr 1;
      if CWbit<32 then
        continue else
        goto nextCW;
    end;
  until false;
//  assert(result=dst-dst_beg);
end;

// better code generation with sub-functions for raw decoding
procedure SynLZdecompress1passub(src, src_end, dst: PAnsiChar; var offset: TOffsets);
var last_hashed: PAnsiChar; // initial src and dst value
    {$ifdef CPU64}
    o: PAnsiChar;
    {$endif}
    CW, CWbit: cardinal;
    v, t, h: PtrUInt;
label nextCW;
begin
  last_hashed := dst-1;
nextCW:
  CW := PCardinal(src)^;
  inc(src,4);
  CWbit := 1;
  if src<src_end then
  repeat
    if CW and CWbit=0 then begin
      dst^ := src^;
      inc(src);
      inc(dst);
      if src>=src_end then break;
      if last_hashed<dst-3 then begin
        inc(last_hashed);
        v := PCardinal(last_hashed)^;
        offset[((v shr 12) xor v) and 4095] := last_hashed;
      end;
      CWbit := CWbit shl 1;
      if CWbit<>0 then
        continue else
        goto nextCW;
    end else begin
      h := PWord(src)^;
      inc(src,2);
      t := (h and 15)+2;
      h := h shr 4;
      if t=2 then begin
        t := ord(src^)+(16+2);
        inc(src);
      end;
      {$ifdef CPU64}
      o := offset[h];
      if PtrUInt(dst-o)<t then
        movechars(o,dst,t) else
        if t<=8 then
          PInt64(dst)^ := PInt64(o)^ else
          move(o^,dst^,t);
      {$else}
      if PtrUInt(dst-offset[h])<t then
        movechars(offset[h],dst,t) else
        if t>8 then // safe since src_endmatch := src_end-(6+5)
          move(offset[h]^,dst^,t) else
          PInt64(dst)^ := PInt64(offset[h])^; // much faster in practice
      {$endif}
      if src>=src_end then break;
      if last_hashed<dst then
        repeat
          inc(last_hashed);
          v := PCardinal(last_hashed)^;
          offset[((v shr 12) xor v) and 4095] := last_hashed;
        until last_hashed>=dst;
      inc(dst,t);
      last_hashed := dst-1;
      CWbit := CWbit shl 1;
      if CWbit<>0 then
        continue else
        goto nextCW;
    end;
  until false;
end;

function SynLZdecompress1pas(src: PAnsiChar; size: integer; dst: PAnsiChar): integer;
var offset: TOffsets;
    src_end: PAnsiChar;
begin
  src_end := src+size;
  result := PWord(src)^;
  if result=0 then exit;
  inc(src,2);
  if result and $8000<>0 then begin
    result := (result and $7fff) or (integer(PWord(src)^) shl 15);
    inc(src,2);
  end;
  SynLZdecompress1passub(src, src_end, dst, offset{%H-});
end;

procedure SynLZdecompress1partialsub(src, dst, src_end, dst_end: PAnsiChar; var offset: TOffsets);
var last_hashed: PAnsiChar; // initial src and dst value
    CWbit, CW: integer;
    v, t, h: PtrUInt;
    {$ifdef CPU64}
    o: PAnsiChar;
    {$endif}
label nextCW;
begin
  last_hashed := dst-1;
nextCW:
  CW := PCardinal(src)^;
  inc(src,4);
  CWbit := 1;
  if src<src_end then
  repeat
    if CW and CWbit=0 then begin
      dst^ := src^;
      inc(src);
      inc(dst);
      if (src>=src_end) or (dst>=dst_end) then
        break;
      if last_hashed<dst-3 then begin
        inc(last_hashed);
        v := PCardinal(last_hashed)^;
        offset[((v shr 12) xor v) and 4095] := last_hashed;
      end;
      CWbit := CWbit shl 1;
      if CWbit<>0 then
        continue else
        goto nextCW;
    end else begin
      h := PWord(src)^;
      inc(src,2);
      t := (h and 15)+2;
      h := h shr 4;
      if t=2 then begin
        t := ord(src^)+(16+2);
        inc(src);
      end;
      if dst+t>=dst_end then begin // avoid buffer overflow by all means
        movechars(offset[h],dst,dst_end-dst);
        break;
      end;
      {$ifdef CPU64}
      o := offset[h];
      if (t<=8) or (PtrUInt(dst-o)<t) then
        movechars(o,dst,t) else
        move(o^,dst^,t);
      {$else}
      if (t<=8) or (PtrUInt(dst-offset[h])<t) then
        movechars(offset[h],dst,t) else
        move(offset[h]^,dst^,t);
      {$endif}
      if src>=src_end then
        break;
      if last_hashed<dst then
        repeat
          inc(last_hashed);
          v := PCardinal(last_hashed)^;
          offset[((v shr 12) xor v) and 4095] := last_hashed;
        until last_hashed>=dst;
      inc(dst,t);
      last_hashed := dst-1;
      CWbit := CWbit shl 1;
      if CWbit<>0 then
        continue else
        goto nextCW;
    end;
  until false;
end;

function SynLZdecompress1partial(src: PAnsiChar; size: integer; dst: PAnsiChar; maxDst: integer): integer;
var offset: TOffsets;
    src_end: PAnsiChar;
begin
  src_end := src+size;
  result := PWord(src)^;
  if result=0 then exit;
  inc(src,2);
  if result and $8000<>0 then begin
    result := (result and $7fff) or (integer(PWord(src)^) shl 15);
    inc(src,2);
  end;
  if maxDst<result then
    result := maxDst;
  if result>0 then
    SynLZdecompress1partialsub(src, dst, src_end, dst+result, offset{%H-});
end;

end.
