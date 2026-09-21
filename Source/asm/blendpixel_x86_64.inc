// x86-64 SSE alpha blend of one pixel: dst = (dst*(255-a) + src*a) / 255 per channel
assembler; nostackframe;
asm
{$IFDEF UNIX}
  mov    rcx, rdi                 // Pixel
  mov    rdx, rsi                 // Color (pointer)
{$ENDIF}
  mov    eax, [rcx]               // dst pixel (single load, reused below)
  test   eax, $FF000000           // dst.A (top byte) == 0?
  jz     @copy
  movd   xmm0, eax                // dst pixel (from the register, no second load)
  movd   xmm1, [rdx]              // src color
  pxor   xmm2, xmm2
  punpcklbw xmm0, xmm2            // dst -> 4 words
  punpcklbw xmm1, xmm2            // src -> 4 words
  pshuflw xmm3, xmm1, $FF         // a = src.A, broadcast to 4 words
  mov    eax, 255
  pinsrw xmm1, eax, 3             // src.A lane -> 255, so the A addend is 255*a
  pcmpeqw xmm4, xmm4
  psrlw  xmm4, 8                  // 255 in each word
  psubw  xmm4, xmm3               // ia = 255 - a
  pmullw xmm0, xmm4               // dst * ia
  pmullw xmm1, xmm3               // addend: src*a  (A lane = 255*a)
  paddw  xmm0, xmm1
  mov    eax, 32897
  movd   xmm5, eax
  pshuflw xmm5, xmm5, 0           // div-255 magic, broadcast
  pmulhuw xmm0, xmm5              // (t * 32897) >> 16
  psrlw  xmm0, 7                  // >> 7  =>  t / 255
  packuswb xmm0, xmm0
  movd   [rcx], xmm0
  jmp    @done
@copy:
  mov    eax, [rdx]
  mov    [rcx], eax
@done:
end;
