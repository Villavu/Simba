assembler; nostackframe;
asm
{$IFDEF UNIX}
  mov      r8, rdx
  mov      rdx, rsi
  mov      rcx, rdi
{$ENDIF}
  sub rsp, 56
  movaps [rsp], xmm6
  movaps [rsp+16], xmm7
  movaps [rsp+32], xmm8
  pxor xmm0, xmm0
  pxor xmm1, xmm1
  pxor xmm3, xmm3
  mov r9d, 998277249
  movd xmm2, r9d
  mov rax, rcx
  movzx ecx, byte ptr [rdx+2]
  cvtsi2ss xmm0, ecx
  movzx ecx, byte ptr [rdx+1]
  movzx edx, byte ptr [rdx]
  cvtsi2ss xmm1, ecx
  cvtsi2ss xmm3, edx
  mulss xmm0, xmm2
  mulss xmm1, xmm2
  mulss xmm3, xmm2
  comiss xmm3, xmm1
  jbe @M141
  movaps xmm2, xmm1
  mov r9d, 1059760810
  movd xmm6, r9d
  movaps xmm1, xmm3
  movaps xmm3, xmm2
  mov r9d, 3212836864
  movd xmm2, r9d
@M120:
  comiss xmm1, xmm0
  ja @M122
  movaps xmm6, xmm2
  movaps xmm2, xmm1
  movaps xmm1, xmm0
  movaps xmm0, xmm2
@M122:
  movaps xmm2, xmm0
  movaps xmm5, xmm1
  mov r9d, 786163455
  movd xmm7, r9d
  minss xmm2, xmm3
  subss xmm5, xmm2
  mov r9d, 786163455
  movd xmm2, r9d
  addss xmm2, xmm1
  movaps xmm4, xmm5
  divss xmm4, xmm2
  mov r9d, 1120403456
  movd xmm2, r9d
  mulss xmm2, xmm1
  movss xmm1, [rax+4]
  mov r9d, 1120403456
  movd xmm8, r9d
  mulss xmm4, xmm8
  comiss xmm7, xmm4
  ja @M130
  mov r9d, 1086324736
  movd xmm8, r9d
  mulss xmm5, xmm8
  comiss xmm7, xmm1
  subss xmm0, xmm3
  mov r9d, 507307272
  movd xmm8, r9d
  addss xmm5, xmm8
  divss xmm0, xmm5
  pxor xmm5, xmm5
  addss xmm0, xmm6
  movaps xmm6, xmm0
  movaps xmm3, xmm0
  mov r9d, 2147483648
  movd xmm8, r9d
  xorps xmm3, xmm8
  cmpltss xmm0, xmm5
  andps xmm3, xmm0
  andnps xmm0, xmm6
  orps xmm0, xmm3
  jbe @M143
@M130:
  pxor xmm3, xmm3
@M127:
  mulss xmm3, [r8]
  subss xmm1, xmm4
  movss xmm0, [rax+8]
  mulss xmm1, [r8+4]
  movaps xmm6, [rsp]
  subss xmm0, xmm2
  mulss xmm0, [r8+8]
  movaps xmm7, [rsp+16]
  movaps xmm8, [rsp+32]
  mulss xmm3, xmm3
  mulss xmm1, xmm1
  mulss xmm0, xmm0
  addss xmm1, xmm3
  addss xmm0, xmm1
  sqrtss xmm0, xmm0
  add rsp, 56
  ret
@M141:
  mov r9d, 3198855851
  movd xmm6, r9d
  pxor xmm2, xmm2
  jmp @M120
@M143:
  mov r9d, 1135869952
  movd xmm8, r9d
  mulss xmm0, xmm8
  movss xmm5, [rax]
  pxor xmm3, xmm3
  subss xmm5, xmm0
  movaps xmm6, xmm5
  movaps xmm0, xmm5
  mov r9d, 2147483648
  movd xmm8, r9d
  xorps xmm0, xmm8
  cmpltss xmm5, xmm3
  andps xmm0, xmm5
  andnps xmm5, xmm6
  orps xmm5, xmm0
  mov r9d, 1127481344
  movd xmm8, r9d
  comiss xmm5, xmm8
  jb @M133
  mov r9d, 1135869952
  movd xmm7, r9d
  subss xmm7, xmm5
  movaps xmm5, xmm7
@M133:
  movaps xmm3, xmm4
  maxss xmm3, xmm1
  mov r9d, 1120403456
  movd xmm8, r9d
  divss xmm3, xmm8
  mulss xmm3, xmm5
  jmp @M127
end;
