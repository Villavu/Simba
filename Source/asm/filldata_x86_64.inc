// x86-64 fill for a row of pixels (pure asm). Small/medium fills use cached movdqu stores
// (fast, and keeps the pixels in cache for the draw that usually follows); fills of 4 MB or
// more use aligned non-temporal (movntdq) stores - ~3x faster for the large image-clear case.
// `const Value` is passed by reference. All registers touched are volatile, hence nostackframe.
assembler; nostackframe;
asm
{$IFDEF UNIX}
  mov    rcx, rdi                 // Data
  mov    r8,  rdx                 // @Value
  mov    rdx, rsi                 // Count
{$ENDIF}
  mov    r9d, [r8]                // Value (const record, passed by reference)
  test   rdx, rdx
  jle    @done                    // Count <= 0
  movd   xmm0, r9d
  pshufd xmm0, xmm0, 0            // broadcast the pixel to all 4 lanes
  mov    rax, rcx                 // rax = running destination
  mov    rcx, rdx                 // rcx = pixel count
  cmp    rcx, 1048576             // >= 4 MB (1M pixels) -> non-temporal
  jae    @large

  // small / medium: cached stores, 8 px (32 bytes) per iteration - two store ports keep up
  mov    edx, ecx
  and    rcx, -8                   // whole blocks of 8 pixels
  jz     @stail
@sloop:
  movdqu [rax], xmm0
  movdqu [rax+16], xmm0
  add    rax, 32
  sub    rcx, 8
  jnz    @sloop
@stail:
  and    edx, 7                    // 0..7 leftover pixels
  jz     @done
  cmp    edx, 4
  jb     @stloop                   // fewer than 4 -> scalar tail
  movdqu [rax], xmm0               // one 4-pixel block
  add    rax, 16
  sub    edx, 4
  jz     @done
@stloop:
  mov    [rax], r9d
  add    rax, 4
  dec    edx
  jnz    @stloop
  jmp    @done

  // large: align to 16 bytes, then non-temporal
@large:
@align:
  test   rax, 15
  jz     @lbulk
  mov    [rax], r9d
  add    rax, 4
  dec    rcx
  jmp    @align
@lbulk:
  mov    edx, ecx
  and    rcx, -4
  jz     @ltail
@lloop:
  movntdq [rax], xmm0
  add    rax, 16
  sub    rcx, 4
  jnz    @lloop
@ltail:
  and    edx, 3
  jz     @lfence
@ltloop:
  mov    [rax], r9d
  add    rax, 4
  dec    edx
  jnz    @ltloop
@lfence:
  sfence
@done:
end;
