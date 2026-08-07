assembler; nostackframe;
asm
{$IFDEF UNIX}
  mov      r8, rdx
  mov      rdx, rsi
  mov      rcx, rdi
{$ENDIF}
  pxor     xmm1, xmm1
  pxor     xmm2, xmm2
  pxor     xmm0, xmm0
  movzx    r9d, byte ptr [rdx+2]      // C2.R
  movzx    eax, byte ptr [rcx]        // C1.R
  sub      eax, r9d
  movzx    r9d, byte ptr [rdx+1]      // C2.G
  movzx    edx, byte ptr [rdx]        // C2.B  (rdx pointer now free)
  cvtsi2ss xmm1, eax
  movzx    eax, byte ptr [rcx+1]      // C1.G
  mulss    xmm1, [r8]                 // dR * mul[0]
  sub      eax, r9d
  cvtsi2ss xmm2, eax
  movzx    eax, byte ptr [rcx+2]      // C1.B
  mulss    xmm2, [r8+4]               // dG * mul[1]
  mulss    xmm1, xmm1                 // (dR * mul[0])^2
  sub      eax, edx
  cvtsi2ss xmm0, eax
  mulss    xmm0, [r8+8]               // dB * mul[2]
  mulss    xmm2, xmm2                 // (dG * mul[1])^2
  mulss    xmm0, xmm0                 // (dB * mul[2])^2
  addss    xmm1, xmm2
  addss    xmm0, xmm1
  sqrtss   xmm0, xmm0
end;
