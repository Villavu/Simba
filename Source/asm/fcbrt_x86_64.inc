assembler; nostackframe;
asm
  movd   eax, xmm0             // seed: y = bits(4/3 * 127*2^23) - bits(x)/3
  mov    ecx, eax
  mov    eax, $AAAAAAAB        // bits div 3  via  (bits * 0xAAAAAAAB) >> 33
  mul    ecx
  shr    edx, 1
  mov    eax, 1419910245
  sub    eax, edx
  movd   xmm1, eax             // xmm1 = y0
  movss  xmm4, dword ptr [rip+ONE_DIV_THREE]   // xmm4 = 1/3
  mov    eax, $40800000        // xmm5 = 4.0f, materialised inline (no data const)
  movd   xmm5, eax
  movaps xmm2, xmm1            // Newton step 1:  y := y * (4 - x*y^3) / 3
  mulss  xmm2, xmm1
  mulss  xmm2, xmm1
  mulss  xmm2, xmm0
  movaps xmm3, xmm5            // 4.0
  subss  xmm3, xmm2
  mulss  xmm1, xmm3
  mulss  xmm1, xmm4
  movaps xmm2, xmm1            // Newton step 2
  mulss  xmm2, xmm1
  mulss  xmm2, xmm1
  mulss  xmm2, xmm0
  movaps xmm3, xmm5            // 4.0
  subss  xmm3, xmm2
  mulss  xmm1, xmm3
  mulss  xmm1, xmm4
  mulss  xmm1, xmm1            // cbrt = x * y^2
  mulss  xmm0, xmm1
end;
