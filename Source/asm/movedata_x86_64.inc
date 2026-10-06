// x86-64 move for a run of pixels (pure asm). Four bytes a pixel, so there are no byte tails
assembler; nostackframe;
asm
{$IFDEF UNIX}
  mov    rcx, rdi                 // Dest
  mov    r8,  rdx                 // Count
  mov    rdx, rsi                 // Src
{$ENDIF}
  test   r8, r8
  jle    @done                    // Count <= 0
  shl    r8, 2                    // r8 = bytes
  mov    rax, rcx
  sub    rax, rdx                 // Dest - Src
  jz     @done                    // onto itself
  cmp    rax, r8
  jb     @backward                // Dest starts inside Src
  cmp    r8, 2097152              // >= 2 MB (512K pixels) -> non-temporal
  jae    @large

  // small / medium: 16 px (64 bytes) per iteration, r9 = the end of the block.
  // Every load is before the first store, so a Dest just before Src is safe.
@cached:
  mov    r9, 64
  cmp    r9, r8
  ja     @tail64
@loop64:
  movdqu xmm0, [rdx + r9 - 64]
  movdqu xmm1, [rdx + r9 - 48]
  movdqu xmm2, [rdx + r9 - 32]
  movdqu xmm3, [rdx + r9 - 16]
  movdqu [rcx + r9 - 64], xmm0
  movdqu [rcx + r9 - 48], xmm1
  movdqu [rcx + r9 - 32], xmm2
  movdqu [rcx + r9 - 16], xmm3
  add    r9, 64
  cmp    r9, r8
  jbe    @loop64
@tail64:
  sub    r9, 48                   // the end of the next 4 px
  cmp    r9, r8
  ja     @tail16
@loop16:
  movdqu xmm0, [rdx + r9 - 16]
  movdqu [rcx + r9 - 16], xmm0
  add    r9, 16
  cmp    r9, r8
  jbe    @loop16
@tail16:
  sub    r9, 16                   // r9 = bytes moved
  cmp    r9, r8
  jae    @done
@loop4:                           // 1..3 px left
  mov    eax, [rdx + r9]
  mov    [rcx + r9], eax
  add    r9, 4
  cmp    r9, r8
  jb     @loop4
  jmp    @done

  // large: align Dest to 16 bytes, then non-temporal
@large:
  test   rcx, 3
  jnz    @cached                  // Dest is not on a pixel boundary: it can never align
@align:
  test   rcx, 15
  jz     @lbulk
  mov    eax, [rdx]
  mov    [rcx], eax
  add    rdx, 4
  add    rcx, 4
  sub    r8, 4
  jmp    @align
@lbulk:
  mov    r9, 64
@lloop:
  movdqu xmm0, [rdx + r9 - 64]
  movdqu xmm1, [rdx + r9 - 48]
  movdqu xmm2, [rdx + r9 - 32]
  movdqu xmm3, [rdx + r9 - 16]
  movntdq [rcx + r9 - 64], xmm0
  movntdq [rcx + r9 - 48], xmm1
  movntdq [rcx + r9 - 32], xmm2
  movntdq [rcx + r9 - 16], xmm3
  add    r9, 64
  cmp    r9, r8
  jbe    @lloop
  sfence
  jmp    @tail64                  // under 64 bytes left, cached

  // from the end, r8 = bytes left
@backward:
  cmp    r8, 16
  jb     @btail
@bloop16:
  movdqu xmm0, [rdx + r8 - 16]
  movdqu [rcx + r8 - 16], xmm0
  sub    r8, 16
  cmp    r8, 16
  jae    @bloop16
@btail:
  test   r8, r8
  jz     @done
@bloop4:
  mov    eax, [rdx + r8 - 4]
  mov    [rcx + r8 - 4], eax
  sub    r8, 4
  jnz    @bloop4
@done:
end;
