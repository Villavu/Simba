// but each src alpha is first scaled by Alpha/255.
// Alpha = 255 is the plain blend, so that tail-jumps to BlendData.
assembler; nostackframe;
asm
{$IFDEF UNIX}
  cmp    cl, 255
  je     @toplain                 // no fade: the plain blend, args already in place
  mov    r8, rdx                  // Count
  mov    rdx, rsi                 // Src
  mov    rcx, rdi                 // Dest
  movzx  r9d, cl                  // Alpha
{$ELSE}
  cmp    r9b, 255
  je     @toplain                 // no fade: the plain blend, args already in place
  movzx  r9d, r9b                 // Alpha
{$ENDIF}
  test   r8, r8
  jle    @done

  sub    rsp, 160
  movdqu [rsp], xmm6
  movdqu [rsp+16], xmm7
  movdqu [rsp+32], xmm8
  movdqu [rsp+48], xmm9
  movdqu [rsp+64], xmm10
  movdqu [rsp+80], xmm11
  movdqu [rsp+96], xmm12
  movdqu [rsp+112], xmm13
  movdqu [rsp+128], xmm14

  pxor   xmm7, xmm7               // zero
  pcmpeqb xmm6, xmm6
  psrlw  xmm6, 8                  // 255 in each word: 255 - a is a xor this
  movdqa xmm9, xmm6
  psllq  xmm9, 48                 // 255 in the A word of each pixel
  mov    eax, 32897
  movd   xmm5, eax
  pshuflw xmm5, xmm5, 0
  punpcklqdq xmm5, xmm5           // div-255 magic in each word
  movd   xmm8, r9d
  pshuflw xmm8, xmm8, 0
  punpcklqdq xmm8, xmm8           // Alpha in each word
  pcmpeqd xmm10, xmm10
  pslld  xmm10, 24                // $FF000000 in each pixel: the A byte

  cmp    r8, 4
  jb     @tail

@loop4:
  movdqu xmm0, [rdx]              // four src pixels
  movdqa xmm1, xmm0
  pcmpeqb xmm1, xmm7
  pmovmskb eax, xmm1
  and    eax, $8888              // the alpha bytes
  cmp    eax, $8888
  je     @next4                   // all transparent: nothing to draw

  movdqu xmm1, [rcx]             // four dst pixels
  movdqa xmm2, xmm0
  punpcklbw xmm2, xmm7            // src pixels 0, 1 as words
  movdqa xmm3, xmm0
  punpckhbw xmm3, xmm7            // src pixels 2, 3
  movdqa xmm11, xmm1
  punpcklbw xmm11, xmm7           // dst pixels 0, 1
  movdqa xmm12, xmm1
  punpckhbw xmm12, xmm7           // dst pixels 2, 3

  pshuflw xmm4, xmm2, $FF         // src.A of pixel 0 in its four words ...
  pshufhw xmm4, xmm4, $FF         // ... and of pixel 1 in its four
  pmullw xmm4, xmm8              // src.A * Alpha
  pmulhuw xmm4, xmm5
  psrlw  xmm4, 7                 // a = (src.A * Alpha) / 255, pixels 0, 1
  movdqa xmm13, xmm4             // keep it to build the faded A byte
  por    xmm2, xmm9              // src.A lanes -> 255, so the A addend is 255*a
  pmullw xmm2, xmm4             // src * a
  pxor   xmm4, xmm6             // 255 - a
  pmullw xmm11, xmm4            // dst * (255 - a)
  paddw  xmm2, xmm11
  pmulhuw xmm2, xmm5
  psrlw  xmm2, 7               // blended pixels 0, 1

  pshuflw xmm4, xmm3, $FF
  pshufhw xmm4, xmm4, $FF
  pmullw xmm4, xmm8
  pmulhuw xmm4, xmm5
  psrlw  xmm4, 7               // a, pixels 2, 3
  movdqa xmm14, xmm13
  packuswb xmm14, xmm4          // a in every byte of each pixel
  por    xmm3, xmm9
  pmullw xmm3, xmm4
  pxor   xmm4, xmm6
  pmullw xmm12, xmm4
  paddw  xmm3, xmm12
  pmulhuw xmm3, xmm5
  psrlw  xmm3, 7               // blended pixels 2, 3
  packuswb xmm2, xmm3           // the four blended pixels

  // the faded source pixel: src.BGR with a in the A byte
  movdqa xmm4, xmm10
  pandn  xmm4, xmm0             // src with its A byte cleared
  pand   xmm14, xmm10           // a in the A byte only
  por    xmm4, xmm14            // faded src

  // where dst.A = 0 and src.A <> 0, that faded source is taken whole
  movdqa xmm3, xmm1
  psrld  xmm3, 24
  pcmpeqd xmm3, xmm7            // dst.A = 0
  movdqa xmm11, xmm0
  psrld  xmm11, 24
  pcmpeqd xmm11, xmm7           // src.A = 0
  pandn  xmm11, xmm3            // dst.A = 0 and not src.A = 0
  pand   xmm4, xmm11            // ... take the faded source
  pandn  xmm11, xmm2            // ... else the blend
  por    xmm4, xmm11
  movdqu [rcx], xmm4

@next4:
  add    rdx, 16
  add    rcx, 16
  sub    r8, 4
  cmp    r8, 4
  jae    @loop4

  // the last 0..3 pixels, one at a time
@tail:
  test   r8, r8
  jz     @restore
@tloop:
  mov    eax, [rdx]
  test   eax, $FF000000
  jz     @tnext                 // src.A = 0: skipped
  mov    r10d, eax
  shr    r10d, 24               // src.A
  imul   r10d, r9d              // src.A * Alpha
  imul   r10d, 32897
  shr    r10d, 23              // a = (src.A * Alpha) / 255
  mov    r11d, eax
  and    r11d, $00FFFFFF
  shl    r10d, 24
  or     r11d, r10d             // the faded source pixel
  mov    eax, [rcx]
  test   eax, $FF000000
  jz     @tcopy                 // dst.A = 0: the faded source is taken
  movd   xmm0, r11d
  movd   xmm1, eax
  punpcklbw xmm0, xmm7
  punpcklbw xmm1, xmm7
  pshuflw xmm4, xmm0, $FF        // a in all four words
  por    xmm0, xmm9
  pmullw xmm0, xmm4
  pxor   xmm4, xmm6
  pmullw xmm1, xmm4
  paddw  xmm0, xmm1
  pmulhuw xmm0, xmm5
  psrlw  xmm0, 7
  packuswb xmm0, xmm0
  movd   [rcx], xmm0
  jmp    @tnext
@tcopy:
  mov    [rcx], r11d
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
  movdqu xmm10, [rsp+64]
  movdqu xmm11, [rsp+80]
  movdqu xmm12, [rsp+96]
  movdqu xmm13, [rsp+112]
  movdqu xmm14, [rsp+128]
  add    rsp, 160
  jmp    @done

@toplain:
  jmp    BlendData                // Alpha = 255: (src.A * 255) / 255 = src.A, so exactly the plain blend

@done:
end;
