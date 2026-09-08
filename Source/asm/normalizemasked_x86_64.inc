// x86-64 SSE2 masked normalise of a match result (pure asm): Res := clamp(Res / sqrt(constFac*energy)),
// or degenVal where energy <= maxEnergy*1e-4 or the product is not positive. Four lanes per step,
// the constants broadcast once. Single precision throughout, like the inline asm it replaces.
//
// var Res, const energy: TSingleMatrix (arrays of row pointers; the FPC dynarray headers give the
// counts); maxEnergy, constFac: Double; degenVal, loClamp, hiClamp: Single - all by value.
// xmm6-xmm12 are saved for win64.
assembler; nostackframe;
asm
{$IFDEF UNIX}
  mov      rcx, rdi                       // @Res
  mov      rdx, rsi                       // energy
{$ENDIF}
  mov      r10, [rcx]                     // Res rows
  test     r10, r10
  jz       @done
  mov      r11, [r10-8]                   // High(Res)
  test     r11, r11
  js       @done
  inc      r11                            // row count
  mov      rax, [r10]                     // Res[0]
  mov      r8, [rax-8]                    // High(Res[0])
  inc      r8                             // width
  mov      rcx, rdx                       // energy rows

  sub      rsp, 112                       // xmm6-xmm12 are callee saved on win64: saved
  movdqu   [rsp], xmm6                    // before any parameter is moved into them
  movdqu   [rsp+16], xmm7
  movdqu   [rsp+32], xmm8
  movdqu   [rsp+48], xmm9
  movdqu   [rsp+64], xmm10
  movdqu   [rsp+80], xmm11
  movdqu   [rsp+96], xmm12

{$IFDEF UNIX}
  movaps   xmm12, xmm4                    // hiClamp   (the sysv registers, put where the
  movaps   xmm11, xmm3                    // loClamp    win64 code below expects them)
  movaps   xmm10, xmm2                    // degenVal
  movaps   xmm3, xmm1                     // constFac
  movaps   xmm2, xmm0                     // maxEnergy
{$ELSE}
  movss    xmm10, [rsp+152]               // degenVal   (the stack arguments, 112 further away now)
  movss    xmm11, [rsp+160]               // loClamp
  movss    xmm12, [rsp+168]               // hiClamp
{$ENDIF}

  // the constants, each in all four lanes
  cvtsd2ss xmm6, xmm3
  shufps   xmm6, xmm6, 0                  // constFac
  mov      eax, $38D1B717                 // 1e-4 as a single
  movd     xmm7, eax
  cvtss2sd xmm7, xmm7
  mulsd    xmm7, xmm2
  cvtsd2ss xmm7, xmm7
  shufps   xmm7, xmm7, 0                  // thr = maxEnergy * 1e-4
  mov      eax, $3F800000
  movd     xmm8, eax
  shufps   xmm8, xmm8, 0                  // 1.0
  pxor     xmm9, xmm9                     // 0.0
  shufps   xmm10, xmm10, 0                // degenVal
  shufps   xmm11, xmm11, 0                // loClamp
  shufps   xmm12, xmm12, 0                // hiClamp

@row:
  mov      rax, [r10]                     // this row of Res
  mov      rdx, [rcx]                     // and of energy
  mov      r9, r8
  shr      r9, 2                          // steps of four
  jz       @rem

@loop4:
  movups   xmm0, [rdx]                    // e
  movaps   xmm1, xmm0
  mulps    xmm1, xmm6                     // arg = constFac * e
  movaps   xmm4, xmm0
  cmpps    xmm4, xmm7, 2                  // e <= thr
  movaps   xmm5, xmm1
  cmpps    xmm5, xmm9, 2                  // arg <= 0
  orps     xmm4, xmm5                     // the degenerate lanes
  movaps   xmm3, xmm4
  andnps   xmm3, xmm1                     // arg where not degenerate ...
  movaps   xmm5, xmm4
  andps    xmm5, xmm8                     // ... 1.0 where it is: the sqrt is always of something > 0
  orps     xmm5, xmm3
  sqrtps   xmm5, xmm5
  movups   xmm2, [rax]                    // r
  divps    xmm2, xmm5                     // v = r / sqrt(arg)
  maxps    xmm2, xmm11
  minps    xmm2, xmm12                    // clamped
  movaps   xmm5, xmm10
  andps    xmm5, xmm4                     // degenVal where degenerate ...
  andnps   xmm4, xmm2                     // ... v elsewhere
  orps     xmm5, xmm4
  movups   [rax], xmm5
  add      rdx, 16
  add      rax, 16
  dec      r9
  jnz      @loop4

@rem:
  mov      r9, r8
  and      r9, 3                          // the last 0..3 of the row
  jz       @rownext
@rloop:
  movss    xmm0, [rdx]                    // e
  movaps   xmm1, xmm0
  mulss    xmm1, xmm6                     // arg
  ucomiss  xmm0, xmm7
  jbe      @rdeg                          // e <= thr
  ucomiss  xmm1, xmm9
  jbe      @rdeg                          // arg <= 0
  sqrtss   xmm1, xmm1
  movss    xmm2, [rax]
  divss    xmm2, xmm1
  maxss    xmm2, xmm11
  minss    xmm2, xmm12
  movss    [rax], xmm2
  jmp      @rnext
@rdeg:
  movss    [rax], xmm10
@rnext:
  add      rdx, 4
  add      rax, 4
  dec      r9
  jnz      @rloop

@rownext:
  add      r10, 8                         // next row pointers
  add      rcx, 8
  dec      r11
  jnz      @row

  movdqu   xmm6, [rsp]
  movdqu   xmm7, [rsp+16]
  movdqu   xmm8, [rsp+32]
  movdqu   xmm9, [rsp+48]
  movdqu   xmm10, [rsp+64]
  movdqu   xmm11, [rsp+80]
  movdqu   xmm12, [rsp+96]
  add      rsp, 112
@done:
end;
