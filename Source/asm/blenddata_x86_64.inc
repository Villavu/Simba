// x86-64 SSE2 blend of a row of pixels (pure asm): src over dst by each pixel's alpha.
// Alphas are tested sixteen at a time (all 0 skipped, all 255 copied), a soft block of four is blended with
// the BlendPixel arithmetic.
assembler; nostackframe;
asm
{$IFDEF UNIX}
  mov    r8, rdx                  // Count
  mov    rdx, rsi                 // Src
  mov    rcx, rdi                 // Dest
{$ENDIF}
  test   r8, r8
  jle    @done

  sub    rsp, 80
  movdqu [rsp], xmm6
  movdqu [rsp+16], xmm7
  movdqu [rsp+32], xmm8
  movdqu [rsp+48], xmm9
  mov    rax, r8
  and    eax, 7
  mov    [rsp+64], rax            // the tail
  shr    r8, 3                    // blocks of eight

  pxor   xmm7, xmm7               // zero
  pcmpeqb xmm8, xmm8              // all ones
  movdqa xmm6, xmm8
  psrlw  xmm6, 8                  // 255 in each word: 255 - a is a xor this
  movdqa xmm9, xmm6
  psllq  xmm9, 48                 // 255 in the A word of each pixel
  mov    eax, 32897
  movd   xmm5, eax
  pshuflw xmm5, xmm5, 0
  punpcklqdq xmm5, xmm5           // div-255 magic in each word

  cmp    r8, 2
  jb     @last

@loop16:
  movdqu xmm0, [rdx]              // sixteen src pixels
  movdqu xmm1, [rdx+16]
  movdqu xmm2, [rdx+32]
  movdqu xmm3, [rdx+48]
  movdqa xmm4, xmm0
  por    xmm4, xmm1
  por    xmm4, xmm2
  por    xmm4, xmm3
  pcmpeqb xmm4, xmm7              // bytes that are 0 in all four
  pmovmskb eax, xmm4
  and    eax, $8888               // the alpha bytes
  cmp    eax, $8888
  je     @next16                  // all transparent: nothing to do
  movdqa xmm4, xmm0
  pand   xmm4, xmm1
  pand   xmm4, xmm2
  pand   xmm4, xmm3
  pcmpeqb xmm4, xmm8              // bytes that are 255 in all four
  pmovmskb eax, xmm4
  and    eax, $8888
  cmp    eax, $8888
  jne    @mixed16
  movdqu [rcx], xmm0              // all opaque: copy
  movdqu [rcx+16], xmm1
  movdqu [rcx+32], xmm2
  movdqu [rcx+48], xmm3
  jmp    @next16

@mixed16:
  xor    r10d, r10d               // the first block of eight ...
  call   @block
  mov    r10d, 32                 // ... and the second
  call   @block

@next16:
  add    rdx, 64
  add    rcx, 64
  sub    r8, 2
  cmp    r8, 2
  jae    @loop16

@last:
  test   r8, r8
  jz     @tail
  xor    r10d, r10d
  call   @block
  add    rdx, 32
  add    rcx, 32

  // the last 0..7 pixels, one at a time
@tail:
  mov    r8, [rsp+64]
  test   r8, r8
  jz     @restore
@tloop:
  mov    eax, [rdx]
  cmp    eax, $FF000000
  jae    @tcopy                   // alpha 255
  test   eax, $FF000000
  jz     @tnext                   // alpha 0
  mov    r9d, [rcx]
  test   r9d, $FF000000
  jz     @tcopy                   // dst.A = 0: the source is taken
  movd   xmm0, eax
  movd   xmm1, r9d
  punpcklbw xmm0, xmm7            // src as words
  punpcklbw xmm1, xmm7            // dst as words
  pshuflw xmm4, xmm0, $FF         // a in all four words
  por    xmm0, xmm9               // src.A -> 255, so the A addend is 255*a
  pmullw xmm0, xmm4               // src * a
  pxor   xmm4, xmm6               // 255 - a
  pmullw xmm1, xmm4               // dst * (255 - a)
  paddw  xmm0, xmm1
  pmulhuw xmm0, xmm5
  psrlw  xmm0, 7                  // / 255
  packuswb xmm0, xmm0
  movd   [rcx], xmm0
  jmp    @tnext
@tcopy:
  mov    [rcx], eax
@tnext:
  add    rdx, 4
  add    rcx, 4
  dec    r8
  jnz    @tloop

@restore:
  movdqu xmm6, [rsp]
  movdqu xmm7, [rsp+16]
  movdqu xmm8, [rsp+32]
  movdqu xmm9, [rsp+48]
  add    rsp, 80
  jmp    @done

@block:
  movdqu xmm0, [rdx+r10]
  movdqa xmm4, xmm0
  pcmpeqb xmm4, xmm7
  pmovmskb eax, xmm4              // bytes that are 0
  movdqa xmm4, xmm0
  pcmpeqb xmm4, xmm8
  pmovmskb r11d, xmm4             // bytes that are 255
  shl    r11d, 16
  or     eax, r11d
  and    eax, $88888888           // the alpha bytes
  mov    r9, r10
  call   @half
  lea    r9, [r10+16]
  movdqu xmm0, [rdx+r9]
  movdqa xmm4, xmm0
  pcmpeqb xmm4, xmm7
  pmovmskb eax, xmm4
  movdqa xmm4, xmm0
  pcmpeqb xmm4, xmm8
  pmovmskb r11d, xmm4
  shl    r11d, 16
  or     eax, r11d
  and    eax, $88888888
  jmp    @half                    // its ret is ours

  // Src[r9] over Dest[r9], four pixels: xmm0 holds them, eax their masks.
  // Clobbers xmm0-xmm4, eax, r11.
@half:
  cmp    eax, $00008888
  je     @halfdone                // all transparent
  cmp    eax, $88880000
  je     @halfcopy                // all opaque
  mov    r11d, eax
  shr    r11d, 16
  or     r11d, eax
  cmp    r11d, $8888              // every alpha 0 or 255: no blending to do,
  je     @halfselect              // the opaque pixels are copied and the rest kept

  movdqu xmm1, [rcx+r9]           // four dst pixels
  movdqa xmm2, xmm0
  punpckhbw xmm2, xmm7            // src pixels 2, 3 as words
  movdqa xmm3, xmm1
  punpckhbw xmm3, xmm7            // dst pixels 2, 3
  punpcklbw xmm0, xmm7            // src pixels 0, 1
  punpcklbw xmm1, xmm7            // dst pixels 0, 1

  pshuflw xmm4, xmm0, $FF         // a of pixel 0 in its four words ...
  pshufhw xmm4, xmm4, $FF         // ... and of pixel 1 in its four
  por    xmm0, xmm9               // src.A lanes -> 255, so the A addend is 255*a
  pmullw xmm0, xmm4               // src * a
  pxor   xmm4, xmm6               // 255 - a
  pmullw xmm1, xmm4               // dst * (255 - a)
  paddw  xmm0, xmm1
  pmulhuw xmm0, xmm5
  psrlw  xmm0, 7                  // / 255, pixels 0, 1

  pshuflw xmm4, xmm2, $FF
  pshufhw xmm4, xmm4, $FF
  por    xmm2, xmm9
  pmullw xmm2, xmm4
  pxor   xmm4, xmm6
  pmullw xmm3, xmm4
  paddw  xmm2, xmm3
  pmulhuw xmm2, xmm5
  psrlw  xmm2, 7                  // pixels 2, 3
  packuswb xmm0, xmm2             // the four blended pixels

  // where dst.A = 0 and src.A <> 0, the source is taken whole
  movdqu xmm2, [rdx+r9]           // src and dst again
  movdqu xmm3, [rcx+r9]
  psrld  xmm3, 24
  pcmpeqd xmm3, xmm7              // dst.A = 0
  movdqa xmm4, xmm2
  psrld  xmm4, 24
  pcmpeqd xmm4, xmm7              // src.A = 0
  pandn  xmm4, xmm3               // dst.A = 0 and not src.A = 0
  pand   xmm2, xmm4               // ... take src
  pandn  xmm4, xmm0               // ... else the blend
  por    xmm4, xmm2
  movdqu [rcx+r9], xmm4
  ret

@halfselect:
  movdqu xmm1, [rcx+r9]
  movdqa xmm4, xmm0
  psrld  xmm4, 24
  movdqa xmm2, xmm8
  psrld  xmm2, 24
  pcmpeqd xmm4, xmm2              // pixels where src.A = 255
  pand   xmm0, xmm4               // ... take src
  pandn  xmm4, xmm1               // ... else keep dst
  por    xmm4, xmm0
  movdqu [rcx+r9], xmm4
  ret

@halfcopy:
  movdqu [rcx+r9], xmm0
@halfdone:
  ret

@done:
end;
