// https://github.com/synopse/mORMot2

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
unit mormot2_synlz;

{$mode delphi}

interface

// get maximum possible (worse) compressed size for out_p
function SynLZcompressdestlen(in_len: integer): integer;

// get uncompressed size from lz-compressed buffer (to reserve memory, e.g.)
function SynLZdecompressdestlen(in_p: PByte): integer;

function SynLZcompress(src: PByte; size: integer; dst: PByte): integer;
function SynLZdecompress(src: PByte; size: integer; dst: PByte): integer;

// this function is slower, but will allow to uncompress only the start of the content (e.g. to read some metadata header)
function SynLZdecompress1partial(src: PByte; size: integer; dst: PByte; maxDst: integer): integer;

implementation

// disabled some FPC paranoid warnings
{$WARN 7119 off : Exported/global symbols should be accessed via the GOT }
{$WARN 7121 off : Check size of memory operand "$1: memory-operand-size is $2 bits, but expected [$3 bits]" }
{$WARN 7122 off : Check size of memory operand "$1: memory-operand-size is $2 bits, but expected [$3 bits + $4 byte offset]" }
{$WARN 7123 off : Check "$1: offset of memory operand is negative "$2 byte" }

{$R-}
{$Q-}

procedure MoveByOne(Source, Dest: Pointer; Count: PtrUInt); inline;
var
  c: Byte; // better code generation on FPC
begin
  inc(PtrUInt(Source), Count);
  inc(PtrUInt(Dest), Count);
  PtrInt(Count) := -PtrInt(Count);
  repeat
    c := PByte(Source)[Count];
    PByte(Dest)[Count] := c;
    inc(Count);
  until Count = 0;
end;

function SynLZcompressdestlen(in_len: integer): integer;
begin // get maximum possible (worse) compressed size for out_p
  result := in_len+in_len shr 3+16;
end;

type // some cross-platform and cross-compiler definitions
  TOffsets = array[0..4095] of PByte; // 16KB/32KB hashing code

function SynLZdecompressdestlen(in_p: PByte): integer;
begin // get uncompressed size from lz-compressed buffer (to reserve memory, e.g.)
  result := PWord(in_p)^;
  if result and $8000<>0 then
    result := (result and $7fff) or (integer(PWord(in_p+2)^) shl 15);
end;

{$IFDEF CPUX86}
{$ASMMODE INTEL}
{$DEFINE HasSynLZdecompress}
function SynLZdecompress(src: PByte; size: integer; dst: PByte): integer; nostackframe; assembler;
asm
        push    ebp
        push    ebx
        push    esi
        push    edi
        push    eax
        add     esp, -4092
        push    eax
        add     esp, -4092
        push    eax
        add     esp, -4092
        push    eax
        add     esp, -4092
        push    eax
        add     esp, -24
        mov     esi, ecx
        mov     ebx, eax
        add     edx, eax
        mov     [esp+8H], esi
        mov     [esp+10H], edx
        movzx   eax, word ptr [ebx]
        mov     [esp], eax
        or      eax,eax
        je      @@0917
        add     ebx, 2
        mov     eax, [esp]
        test    ah, 80H
        jz      @@0907
        and     eax, 7FFFH
        movzx   edx, word ptr [ebx]
        shl     edx, 15
        or      eax, edx
        mov     [esp], eax
        add     ebx, 2
@@0907: lea     ebp, [esi - 1]
@@0908: mov     ecx, [ebx]
        add     ebx, 4
        mov     [esp+14H], ecx
        mov     edi, 1             // edi=CWbit
        cmp     ebx, [esp+10H]
        jnc     @@0917
@@0909: mov     ecx, [esp+14H]
@@090A: test    ecx, edi
        jnz     @@0911
        mov     al, [ebx]
        inc     ebx
        mov     [esi], al
        inc     esi
        cmp     ebx, [esp+10H]
        lea     eax, [esi-3]
        jnc     @@0917
        cmp     eax, ebp
        jbe     @@0910
        inc     ebp
        mov     eax, [ebp]
        mov     edx, eax
        shr     eax, 12
        xor     eax, edx
        and     eax, 0FFFH
        mov     [esp+1CH + eax * 4], ebp
@@0910: add     edi, edi
        jnz     @@090A
        jmp     @@0908
@@0911: movzx   edx, word ptr [ebx]
        add     ebx, 2
        mov     eax, edx
        and     edx, 0FH
        add     edx, 2
        shr     eax, 4
        cmp     edx,2
        jnz     @@0912
        movzx   edx, byte ptr [ebx]
        inc     ebx
        add     edx, 18
@@0912: mov     eax, [esp+1CH + eax * 4]
        mov     ecx, esi
        mov     [esp+18H], edx
        sub     ecx, eax
        // inlined optimized move()
        cmp     ecx, edx
        jl      @@ovlap   // overlapping content requires per-byte copy
        cmp     edx, 32
        ja      @large
        sub     edx, 8
        jg      @9_32
{$ifdef HASNOSSE2}
        // slowest x87 FPU code on very old CPU with no SSE2 support
        mov     ecx, [eax]
        mov     eax, [eax + 4]     // always copy 8 bytes for 0..8
        mov     [esi], ecx         // safe since src_endmatch := src_end-(6+5)
        mov     [esi + 4], eax
        jmp     @movend
@9_32:  fild    qword ptr [eax + edx]
        fild    qword ptr [eax]
        cmp     edx, 8
        jle     @16
        fild    qword ptr [eax + 8]
        cmp     edx, 16
        jle     @24
        fild    qword ptr [eax + 16]
        fistp   qword ptr [esi + 16]
@24:    fistp   qword ptr [esi + 8]
@16:    fistp   qword ptr [esi]
        fistp   qword ptr [esi + edx]
        jmp     @movend
        nop
@large: push    esi
        fild    qword ptr [eax]
        lea     eax, [eax + edx - 8]
        lea     edx, [esi + edx - 8]
        fild    qword ptr [eax]
        push    edx
        neg     edx
        and     esi, -8
        lea     edx, [edx + esi + 8]
        pop     esi
@lrgnxt:fild    qword ptr [eax + edx]
        fistp   qword ptr [esi + edx]
        add     edx, 8
        jl      @lrgnxt
        fistp   qword ptr [esi]
        pop     esi
        fistp   qword ptr [esi]
{$else} // inlined SSE2 move
        movq    xmm0, qword ptr [eax]
        movq    qword ptr [esi], xmm0
        jmp     @movend
@9_32:  movq    xmm0, qword ptr [eax + edx]
        movq    xmm1, qword ptr [eax]
        cmp     edx, 8
        jle     @16
        movq    xmm2, qword ptr [eax + 8]
        cmp     edx, 16
        jle     @24
        movq    xmm3, qword ptr [eax + 16]
        movq    qword ptr [esi + 16], xmm3
@24:    movq    qword ptr [esi + 8], xmm2
@16:    movq    qword ptr [esi], xmm1
        movq    qword ptr [esi + edx], xmm0
        jmp     @movend
@large: push    esi
        movups  xmm2, dqword ptr [eax]
        lea     eax, [eax + edx - 16]
        lea     edx, [esi + edx - 16]
        movups  xmm1, dqword ptr [eax]
        push    edx
        neg     edx
        and     esi, -16
        lea     edx, [edx + esi + 16]
        pop     esi
@lrgnxt:movups  xmm0, dqword ptr [eax + edx]
        movaps  dqword ptr [esi + edx], xmm0
        add     edx, 16
        jl      @lrgnxt
        movups  dqword ptr [esi], xmm1
        pop     esi
        movups  dqword ptr [esi], xmm2
{$endif HASNOSSE2}
@movend:cmp     esi, ebp
        jbe     @@0916
@@0915: inc     ebp
        mov     edx, [ebp]
        mov     eax, edx
        shr     edx, 12
        xor     eax, edx
        and     eax, 0FFFH
        mov     [esp+1CH + eax * 4], ebp
        cmp     esi, ebp
        ja      @@0915
@@0916: add     esi, [esp+18H]
        cmp     ebx, [esp+10H]
        jnc     @@0917
        add     edi, edi
        lea     ebp, [esi - 1]
        jz      @@0908
        jmp     @@0909
@@ovlap:push    ebx
        push    esi
        lea     ebx, [eax + edx]
        add     esi, edx
        neg     edx
@s:     mov     al, [ebx + edx]
        mov     [esi + edx], al
        inc     edx
        jnz     @s
        pop     esi
        pop     ebx
        jmp     @movend
@@0917: mov     eax, [esp]
        add     esp, 16412
        pop     edi
        pop     esi
        pop     ebx
        pop     ebp
end;
{$ENDIF}

{$IFDEF CPUX86_64}
{$ASMMODE INTEL}
{$DEFINE HasSynLZdecompress}
function SynLZdecompress(src: PByte; size: integer; dst: PByte): integer;
var
  off: TOffsets; // pointers are faster and use only 32KB on stack
asm     // rcx=src, edx=size, r8=dest
        {$ifdef WIN64} // additional registers to preserve
        push    rsi
        push    rdi
        {$else} // Linux 64-bit ABI
        mov     r8, rdx
        mov     rdx, rsi
        mov     rcx, rdi
        {$endif WIN64}
        push    rbx
        push    r12
        push    r13
        movzx   eax, word ptr [rcx]    // rcx=src   eax=result
        lea     r9, [rcx + rdx]        // r9=src_end
        test    eax, eax
        je      @36
        add     rcx, 2
        mov     r10d, eax
        and     r10d, $8000
        jz      @21                   // dest size < 32768
        movzx   ebx, word ptr [rcx]
        shl     ebx, 15
        and     eax, $7fff
        or      eax, ebx
        add     rcx, 2
@21:    push    rax                    // save result for function exit
        lea     rax, [r8 - 1]          // rax=last_hashed  r8=dest
@22:    mov     edi, dword ptr [rcx]   // edi=CW
        add     rcx, 4
        mov     r10d, 1                // r10d=CWBit
        cmp     rcx, r9
        jnc     @35
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@23:    test    r10d, edi
        jnz     @25
        mov     bl, byte ptr [rcx]    // CWBit was not set: copy byte and hash
        mov     byte ptr [r8], bl
        add     rcx, 1
        lea     rbx, [r8 - 2]
        add     r8, 1
        cmp     rcx, r9
        jnc     @35
        cmp     rbx, rax
        jbe     @24
        add     rax, 1
        mov     esi, dword ptr [rax]
        mov     ebx, esi
        shr     esi, 12
        xor     ebx, esi
        and     ebx, 4095
        mov     qword ptr [off + rbx * 8], rax
@24:    shl     r10d, 1
        jnz     @23
        jmp     @22
@25:    movzx   rbx, word ptr [rcx] // CWBit was set: copy bytes from offset
        add     rcx, 2
        mov     r11, rbx
        shr     ebx, 4                 // ebx=h
        and     r11, 0FH               // r11=t
        lea     r11, [r11 + 2]
        jnz     @26
        movzx   r11, byte ptr [rcx]
        add     rcx, 1
        lea     r11, [r11 + (16 + 2)]
@26:    mov     r13, qword ptr [off + rbx * 8] // r13=o
        mov     rbx, r8
        xor     rsi, rsi
        sub     rbx, r13
        mov     r12, r11
        cmp     rbx, r11
        jc      @29       // PtrUInt(dst - o) < t overlap -> MoveByOne
        mov     rbx, qword ptr [r13]
        mov     qword ptr [r8], rbx
        sub     r12, 8
        jbe     @31       // 1..8 bytes
        mov     rbx, qword ptr [r13 + r12] // last 8 bytes (may overlap)
        mov     qword ptr [r8 + r12], rbx
        cmp     r12, 8
        jbe     @31      // 9..16 bytes
        shr     r12, 3
@27:    add     rsi, 8
        mov     rbx, qword ptr [r13 + rsi] // inlined move by 8 bytes
        mov     qword ptr [r8 + rsi], rbx
        sub     r12, 1
        jnz     @27
@31:    cmp     rcx, r9
        jz      @35
        cmp     rax, r8
        jnc     @34
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@32:    add     rax, 1               // hash decompressed bytes
        mov     ebx, dword ptr [rax]
        mov     esi, ebx
        shr     ebx, 12
        xor     esi, ebx
        and     esi, 4095
        mov     qword ptr [off + rsi * 8], rax
        cmp     rax, r8
        jc      @32
@34:    add     r8, r11
        lea     rax, [r8 - 1]
        shl     r10d, 1
        jnz     @23
        jmp     @22
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@29:    mov     bl, byte ptr [r13 + rsi] // overlaping move
        mov     byte ptr [r8 + rsi], bl
        add     rsi, 1
        sub     r12, 1
        jnz     @29
        jmp     @31
@35:    pop     rax     // returns stored dest length
@36:    pop     r13
        pop     r12
        pop     rbx
        {$ifdef WIN64} // additional registers to preserve
        pop     rdi
        pop     rsi
        {$endif WIN64}
end;
{$ENDIF}

{$IFDEF CPUX86}
{$ASMMODE INTEL}
{$DEFINE HasSynLZCompress}
function SynLZcompress(src: PByte; size: integer; dst: PByte): integer; nostackframe; assembler;
asm
        push    ebp
        push    ebx
        push    esi
        push    edi
        push    eax
        add     esp, -4092
        push    eax
        add     esp, -4092
        push    eax
        add     esp, -4092
        push    eax
        add     esp, -4092
        push    eax
        add     esp, -4092
        push    eax
        add     esp, -4092
        push    eax
        add     esp, -4092
        push    eax
        add     esp, -4092
        push    eax
        add     esp, -32
        mov     esi, eax // esi=src
        mov     edi, ecx // edi=dst
        mov     [esp+08H], ecx
        mov     eax, edx
        cmp     eax, 32768
        jl      @@0889
        or      ax, 8000H
        mov     [edi], eax
        mov     eax, edx
        shr     eax, 15
        mov     [edi + 2], eax
        add     edi, 4
        jmp     @@0891
@@0890: mov     eax, 2
        jmp     @@0904
@@0889: mov     [edi], eax
        test    eax, eax
        jz      @@0890
        add     edi, 2
@@0891: lea     eax, [edx + esi]
        mov     [esp+18H], edi
        mov     [esp+0CH], eax
        sub     eax, 11
        mov     [esp+4], eax
        xor     eax, eax
        mov     [edi], eax
        lea     edi, [esp+24H] // reset 16KB offsets lookup table using ERMS
        mov     ecx, 4096
        cld
        rep stosd
        mov     edi, [esp+18H]
        add     edi, 4
        mov     ebx, 1 // ebx=1 shl CWbit
        // main loop:
        cmp     esi, [esp+4]
        ja      @@0900
@@0892: mov     edx, [esi]
        mov     eax, edx
        shr     edx, 12
        xor     edx, eax
        and     edx, 0FFFH
        mov     ebp, [esp+24H + edx * 4]
        mov     ecx, [esp+4024H + edx * 4]
        mov     [esp+24H + edx * 4], esi
        xor     ecx, eax
        test    ecx, 0FFFFFFH
        mov     [esp+4024H + edx * 4], eax
        jnz     @@0897
        mov     eax, esi
        or      ebp, ebp
        jz      @@0897
        sub     eax, ebp
        mov     ecx, [esp+18H]
        cmp     eax, 2
        jle     @@0897
        add     esi, 2
        or      dword ptr [ecx], ebx
        mov     ecx, [esp+0CH]
        add     ebp, 2
        mov     eax, 1
        sub     ecx, esi
        dec     ecx
        mov     [esp], ecx
        cmp     ecx, 271
        jl      @@0894
        mov     dword ptr [esp], 271
        jmp     @@0894
@@0893: inc     eax
@@0894: mov     ecx, [ebp + eax]
        cmp     cl, [esi + eax]
        jnz     @@0895
        cmp     eax, [esp]
        jge     @@0895
        inc     eax
        cmp     ch, [esi + eax]
        jnz     @@0895
        shr     ecx, 16
        cmp     eax, [esp]
        jge     @@0895
        inc     eax
        cmp     cl, [esi + eax]
        jnz     @@0895
        cmp     eax, [esp]
        jge     @@0895
        inc     eax
        cmp     ch, [esi + eax]
        jnz     @@0895
        cmp     eax, [esp]
        jl      @@0893
@@0895: add     esi, eax
        shl     edx, 4
        cmp     eax, 15
        jg      @@0896
        or      eax, edx
        mov     word ptr [edi], ax
        add     edi, 2
        jmp     @@0898
@@0896: sub     eax, 16
        mov     [edi], dx
        mov     [edi + 2H], al
        add     edi, 3
        jmp     @@0898
@@0897: mov     al, [esi] // movsb is actually slower!
        mov     [edi], al
        inc     esi
        inc     edi
@@0898: add     ebx, ebx
        jz      @@0899
        cmp     esi, [esp+4]
        jbe     @@0892
        jmp     @@0900
@@0899: mov     [esp+18H], edi
        mov     [edi], ebx
        inc     ebx
        add     edi, 4
        cmp     esi, [esp+4]
        jbe     @@0892
@@0900: cmp     esi, [esp+0CH]
        jnc     @@0903
@@0901: mov     al, [esi]
        mov     [edi], al
        inc     esi
        inc     edi
        add     ebx, ebx
        jz      @@0902
        cmp     esi, [esp+0CH]
        jc      @@0901
        jmp     @@0903
@@0902: mov     [edi], ebx
        inc     ebx
        add     edi, 4
        cmp     esi, [esp+0CH]
        jc      @@0901
@@0903: mov     eax, edi
        sub     eax, [esp+08H]
@@0904: add     esp, 32804
        pop     edi
        pop     esi
        pop     ebx
        pop     ebp
end;
{$ENDIF}

{$IFDEF CPUX86_64}
{$ASMMODE INTEL}
{$DEFINE HasSynLZCompress}
function SynLZcompress(src: PByte; size: integer; dst: PByte): integer;
var
  off: TOffsets;                     // 32KB hash table
  cache: array[0..4095] of cardinal; // 16KB = total 48KB on stack
asm // rcx=src, edx=size, r8=dest
        {$ifdef WIN64} // additional registers to preserve
        push    rdi
        push    rsi
        {$else} // Linux 64-bit ABI
        mov     r8, rdx
        mov     rdx, rsi
        mov     rcx, rdi
        {$endif}
        push    rbx
        push    r12
        push    r13
        push    r8                 // save r8=dest to compute result
        mov     rbx, rcx           // rbx=src
        cmp     edx, 32768         // edx=size
        jc      @03
        mov     eax, edx
        mov     ecx, edx
        and     eax, 7FFFH
        shr     ecx, 15
        or      eax, 8000H
        shl     ecx, 16
        or      eax, ecx
        mov     dword ptr [r8], eax
        add     r8, 4
        jmp     @05
@03:    mov     dword ptr [r8], edx
        add     r8, 2
        test    edx, edx
        jz      @18
@05:    xor     eax, eax
        lea     rdi, [off]
        mov     rcx, 4096
        cld
        rep stosq                  // reset 32KB offsets lookup table using ERMS
        lea     r9,  [rdx + rbx]   // r9=src_end
        lea     r10, [r9 - 11]     // r10=src_endmatch
        mov     r11, r8            // r11=CWpoint
        mov     ecx, 1             // ecx=CWBits
        mov     dword ptr [r8], eax
        add     r8, 4
        cmp     rbx, r10
        ja      @15
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@07:    mov     edx, dword ptr [rbx]
        mov     eax, edx
        shr     eax, 12
        xor     eax, edx
        and     eax, 4095              // rax=h
        mov     r12, rdx
        //lea [off + rax * 8] pre-compute is not faster: 2 AGUs since Nehalem
        mov     r13, qword ptr [off + rax * 8]    // r13=o
        mov     edx, dword ptr [cache + rax * 4]
        mov     qword ptr [off + rax * 8], rbx
        mov     dword ptr [cache + rax * 4], r12d
        xor     rdx, r12
        test    r13, r13
        lea     rdi, [r9 - 1]
        je      @12
        and     rdx, $00ffffff
        jne     @12
        mov     rdx, rbx
        sub     rdx, r13
        cmp     rdx, 2
        jbe     @12
        or      dword ptr [r11], ecx
        add     rbx, 2
        add     r13, 2
        mov     esi, 1
        sub     rdi, rbx
        cmp     rdi, 271
        jc      @09
        mov     edi, 271
        jmp     @09
@08:    add     rsi, 1
@09:    mov     edx, dword ptr [r13 + rsi]
        cmp     dl, byte ptr [rbx + rsi]
        jnz     @10
        cmp     rsi, rdi
        jge     @10
        add     rsi, 1
        cmp     dh, byte ptr [rbx + rsi]
        jnz     @10
        shr     edx, 16
        cmp     rsi, rdi
        jge     @10
        add     rsi, 1
        cmp     dl, byte ptr [rbx + rsi]
        jnz     @10
        cmp     rsi, rdi
        jge     @10
        add     rsi, 1
        cmp     dh, byte ptr [rbx + rsi]
        jnz     @10
        cmp     rsi, rdi
        jc      @08
@10:    add     rbx, rsi
        shl     rax, 4
        cmp     rsi, 15
        ja      @11
        or      rax, rsi
        mov     word ptr [r8], ax
        add     r8, 2
        jmp     @13
@11:    sub     rsi, 16
        mov     word ptr [r8], ax
        mov     byte ptr [r8 + 2], sil
        add     r8, 3
        jmp     @13
@12:    mov     al, byte ptr [rbx]
        mov     byte ptr [r8], al
        add     rbx, 1
        add     r8, 1
@13:    add     ecx, ecx
        jnz     @14
        mov     r11, r8
        mov     [r8], ecx
        add     r8, 4
        add     ecx, 1
@14:    cmp     rbx, r10
        jbe     @07
@15:    cmp     rbx, r9
        jnc     @18
@16:    mov     al, byte ptr [rbx]
        mov     byte ptr [r8], al
        add     rbx, 1
        add     r8, 1
        add     ecx, ecx
        jnz     @17
        mov     dword ptr [r8], ecx
        add     r8, 4
        add     ecx, 1
@17:    cmp     rbx, r9
        jc      @16
@18:    pop     rax
        sub     r8, rax // result := dst - dstbeg
        mov     rax, r8
        pop     r13
        pop     r12
        pop     rbx
        {$ifdef WIN64} // additional registers to preserve
        pop     rsi
        pop     rdi
        {$endif WIN64}
end;
{$ENDIF}

// pure pascal fallback for compress
{$IFNDEF HasSynLZCompress}
function SynLZcompress(src: PByte; size: integer; dst: PByte): integer;
var
  dst_beg,          // initial dst value
  src_end,          // real last byte available in src
  src_endmatch,     // last byte to try for hashing
  o: PByte;
  CWbit: byte;
  CWpoint: PCardinal;
  v, h, cached, t, tmax: PtrUInt;
  offset: TOffsets;
  cache: array[0..4095] of cardinal; // 16KB+16KB=32KB on stack (48KB for cpu64)
begin
  dst_beg := dst;
  // 1. store in_len
  if size >= $8000 then
  begin
    // size in 32KB..2GB -> stored as integer
    PWord(dst)^ := $8000 or (size and $7fff);
    PWord(dst + 2)^ := size shr 15;
    inc(dst, 4);
  end
  else
  begin
    PWord(dst)^ := size; // size<32768 -> stored as word
    if size = 0 then
    begin
      result := 2;
      exit;
    end;
    inc(dst, 2);
  end;
  // 2. compress
  src_end := src + size;
  src_endmatch := src_end - (6 + 5);
  CWbit := 0;
  CWpoint := pointer(dst);
  PCardinal(dst)^ := 0;
  inc(dst, SizeOf(CWpoint^));
  FillChar(offset, SizeOf(offset), 0); // fast 16KB reset to 0
  // 1. main loop to search using hash[]
  if src <= src_endmatch then
    repeat
      v := PCardinal(src)^;
      h := ((v shr 12) xor v) and 4095;
      o := offset[h];
      offset[h] := src;
      cached := v xor {%H-}cache[h]; // o=nil if cache[h] is uninitialized
      cache[h] := v;
      if (cached and $00ffffff = 0) and
         (o <> nil) and
         (src - o > 2) then
      begin
        CWpoint^ := CWpoint^ or (cardinal(1) shl CWbit);
        inc(src, 2);
        inc(o, 2);
        t := 1;
        tmax := src_end - src - 1;
        if tmax >= (255 + 16) then
          tmax := (255 + 16);
        while (o[t] = src[t]) and
              (t < tmax) do
          inc(t);
        inc(src, t);
        h := h shl 4;
        // here we have always t>0
        if t <= 15 then
        begin
          // mark 2 to 17 bytes -> size=1..15
          PWord(dst)^ := integer(t or h);
          inc(dst, 2);
        end
        else
        begin
          // mark 18 to (255+16) bytes -> size=0, next byte=t
          dec(t, 16);
          PWord(dst)^ := h; // size=0
          dst[2] := Byte(t);
          inc(dst, 3);
        end;
      end
      else
      begin
        dst^ := src^;
        inc(src);
        inc(dst);
      end;
      if CWbit < 31 then
      begin
        inc(CWbit);
        if src <= src_endmatch then
          continue
        else
          break;
      end
      else
      begin
        CWpoint := pointer(dst);
        PCardinal(dst)^ := 0;
        inc(dst, SizeOf(CWpoint^));
        CWbit := 0;
        if src <= src_endmatch then
          continue
        else
          break;
      end;
    until false;
  // 2. store remaining bytes
  if src < src_end then
    repeat
      dst^ := src^;
      inc(src);
      inc(dst);
      if CWbit < 31 then
      begin
        inc(CWbit);
        if src < src_end then
          continue
        else
          break;
      end
      else
      begin
        PCardinal(dst)^ := 0;
        inc(dst, 4);
        CWbit := 0;
        if src < src_end then
          continue
        else
          break;
      end;
    until false;
  result := dst - dst_beg;
end;
{$ENDIF}

// pure pascal fallback for decompress
{$IFNDEF HasSynLZdecompress}
function SynLZdecompress(src: PByte; size: integer; dst: PByte): integer;

  procedure SynLZdecompress1passub(src, src_end, dst: PByte; var offset: TOffsets);
  var
    last_hashed: PByte; // initial src and dst value
    {$ifdef CPU64}
    o: PByte;
    {$endif CPU64}
    CW, CWbit: cardinal;
    v, t, h: PtrUInt;
  label
    nextCW;
  begin
    last_hashed := dst - 1;
  nextCW:
    CW := PCardinal(src)^;
    inc(src, 4);
    CWbit := 1;
    if src < src_end then
      repeat
        if CW and CWbit = 0 then
        begin
          dst^ := src^;
          inc(src);
          inc(dst);
          if src >= src_end then
            break;
          if last_hashed < dst - 3 then
          begin
            inc(last_hashed);
            v := PCardinal(last_hashed)^;
            offset[((v shr 12) xor v) and 4095] := last_hashed;
          end;
          CWbit := CWbit shl 1;
          if CWbit <> 0 then
            continue
          else
            goto nextCW;
        end
        else
        begin
          h := PWord(src)^;
          inc(src, 2);
          t := (h and 15) + 2;
          if t = 2 then
          begin
            t := ord(src^) + (16 + 2);
            inc(src);
          end;
          h := h shr 4;
          {$ifdef CPU64}
          o := offset[h];
          if PtrUInt(dst - o) < t then // overlap -> move byte-by-byte
            MoveByOne(o, dst, t)
          else if t <= 8 then
            PInt64(dst)^ := PInt64(o)^ // much faster in practice
          else
            Move(o^, dst^, t);     // safe since src_endmatch := src_end-(6+5)
          {$else}
          if PtrUInt(dst - offset[h]) < t then
            MoveByOne(offset[h], dst, t)
          else if t > 8 then
            Move(offset[h]^, dst^, t)
          else
            PInt64(dst)^ := PInt64(offset[h])^;
          {$endif CPU64}
          if src >= src_end then
            break;
          if last_hashed < dst then
            repeat // decompressed bytes should update the hash table
              inc(last_hashed);
              v := PCardinal(last_hashed)^;
              offset[((v shr 12) xor v) and 4095] := last_hashed;
            until last_hashed >= dst;
          inc(dst, t);
          last_hashed := dst - 1;
          CWbit := CWbit shl 1;
          if CWbit <> 0 then
            continue
          else
            goto nextCW;
        end;
      until false;
  end;

var offset: TOffsets;
    src_end: PByte;
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
{$ENDIF}

function SynLZdecompress1partial(src: PByte; size: integer; dst: PByte; maxDst: integer): integer;

  procedure SynLZdecompresspartialsub(src, dst, src_end, dst_end: PByte; var offset: TOffsets);
  var
    lasthashed: PByte; // initial src and dst value
    cwbit, cw: integer;
    v, t, h: PtrUInt;
    {$ifdef CPU64}
    o: PByte;
    {$endif CPU64}
  label
    nextCW;
  begin
    lasthashed := dst - 1;
  nextCW:
    cw := PCardinal(src)^;
    inc(src, 4);
    cwbit := 1;
    if src < src_end then
      repeat
        if cw and cwbit = 0 then
        begin
          dst^ := src^;
          inc(src);
          inc(dst);
          if (src >= src_end) or
             (dst >= dst_end) then
            break;
          if lasthashed < dst - 3 then
          begin
            inc(lasthashed);
            v := PCardinal(lasthashed)^;
            offset[((v shr 12) xor v) and 4095] := lasthashed;
          end;
          cwbit := cwbit shl 1;
          if cwbit <> 0 then
            continue
          else
            goto nextCW;
        end
        else
        begin
          h := PWord(src)^;
          inc(src, 2);
          t := (h and 15) + 2;
          h := h shr 4;
          if t = 2 then
          begin
            t := ord(src^) + (16 + 2);
            inc(src);
          end;
          if dst + t >= dst_end then
          begin
            // avoid buffer overflow by all means
            MoveByOne(offset[h], dst, dst_end - dst);
            break;
          end;
          {$ifdef CPU64}
          o := offset[h];
          if (t <= 8) or
             (PtrUInt(dst - o) < t) then
            MoveByOne(o, dst, t)
          else
            Move(o^, dst^, t);
          {$else}
          if (t <= 8) or
             (PtrUInt(dst - offset[h]) < t) then
            MoveByOne(offset[h], dst, t)
          else
            Move(offset[h]^, dst^, t);
          {$endif CPU64}
          if src >= src_end then
            break;
          if lasthashed < dst then
            repeat
              inc(lasthashed);
              v := PCardinal(lasthashed)^;
              offset[((v shr 12) xor v) and 4095] := lasthashed;
            until lasthashed >= dst;
          inc(dst, t);
          lasthashed := dst - 1;
          cwbit := cwbit shl 1;
          if cwbit <> 0 then
            continue
          else
            goto nextCW;
        end;
      until false;
  end;

var
  offset: TOffsets;
  srcend: PByte;
begin
  srcend := src + size;
  result := PWord(src)^;
  if result = 0 then
    exit;
  inc(src, 2);
  if result and $8000 <> 0 then
  begin
    result := (result and $7fff) or (integer(PWord(src)^) shl 15);
    inc(src, 2);
  end;
  if maxDst < result then
    result := maxDst;
  if result > 0 then
    SynLZdecompresspartialsub(src, dst, srcend, dst + result, offset);
end;

end.
