assembler; nostackframe;
asm
{$IFDEF UNIX}
  mov      r8, rdx
  mov      rdx, rsi
  mov      rcx, rdi
{$ENDIF}
  sub rsp, 120
  movaps [rsp], xmm6
  movaps [rsp+16], xmm7
  movaps [rsp+32], xmm8
  movaps [rsp+48], xmm9
  movaps [rsp+64], xmm10
  movaps [rsp+80], xmm11
  movaps [rsp+96], xmm12
  pxor xmm0, xmm0
  pxor xmm6, xmm6
  pxor xmm3, xmm3
  mov r9d, 998277249
  movd xmm1, r9d
  pxor xmm2, xmm2
  mov r9d, 1056964608
  movd xmm10, r9d
  mov r9d, 1120403456
  movd xmm5, r9d
  mov rax, rcx
  movzx ecx, byte ptr [rdx+2]
  cvtsi2ss xmm0, ecx
  movzx ecx, byte ptr [rdx+1]
  movzx edx, byte ptr [rdx]
  cvtsi2ss xmm6, ecx
  cvtsi2ss xmm3, edx
  mulss xmm0, xmm1
  mulss xmm6, xmm1
  mulss xmm3, xmm1
  movaps xmm7, xmm0
  movaps xmm1, xmm6
  minss xmm1, xmm3
  minss xmm7, xmm1
  movaps xmm1, xmm3
  maxss xmm1, xmm6
  movaps xmm11, xmm7
  maxss xmm1, xmm0
  addss xmm11, xmm1
  movaps xmm9, xmm1
  movaps xmm8, xmm1
  movss xmm1, [rax+4]
  subss xmm9, xmm7
  mulss xmm10, xmm11
  ucomiss xmm9, xmm2
  movaps xmm4, xmm10
  mulss xmm4, xmm5
  jp @N106
  jne @N106
@N88:
  mulss xmm2, [r8]
  movaps xmm6, [rsp]
  mulss xmm1, [r8+4]
  movaps xmm7, [rsp+16]
  movaps xmm8, [rsp+32]
  movaps xmm9, [rsp+48]
  movaps xmm10, [rsp+64]
  movaps xmm11, [rsp+80]
  movaps xmm12, [rsp+96]
  movaps xmm0, xmm2
  movss xmm2, [rax+8]
  mulss xmm0, xmm0
  subss xmm2, xmm4
  mulss xmm2, [r8+8]
  mulss xmm1, xmm1
  mulss xmm2, xmm2
  addss xmm1, xmm0
  addss xmm1, xmm2
  movaps xmm0, xmm1
  sqrtss xmm0, xmm0
  add rsp, 120
  ret
@N106:
  mov r9d, 1056964608
  movd xmm2, r9d
  comiss xmm2, xmm10
  ja @N118
  mov r9d, 1073741824
  movd xmm10, r9d
  movaps xmm2, xmm9
  subss xmm10, xmm8
  subss xmm10, xmm7
  divss xmm2, xmm10
@N92:
  ucomiss xmm0, xmm8
  jp @N93
  jne @N93
  subss xmm6, xmm3
  movaps xmm0, xmm6
  divss xmm0, xmm9
  mov r9d, 1114636288
  movd xmm12, r9d
  mulss xmm0, xmm12
@N95:
  pxor xmm3, xmm3
  comiss xmm3, xmm0
  jbe @N98
  mov r9d, 1135869952
  movd xmm12, r9d
  addss xmm0, xmm12
@N98:
  mulss xmm2, xmm5
  mov r9d, 786163455
  movd xmm3, r9d
  movaps xmm6, xmm1
  comiss xmm3, xmm1
  subss xmm6, xmm2
  ja @N100
  comiss xmm3, xmm2
  ja @N100
  maxss xmm2, xmm1
  movss xmm3, [rax]
  pxor xmm7, xmm7
  movaps xmm1, xmm6
  subss xmm3, xmm0
  divss xmm2, xmm5
  movaps xmm8, xmm3
  movaps xmm0, xmm3
  mov r9d, 2147483648
  movd xmm12, r9d
  xorps xmm0, xmm12
  cmpltss xmm3, xmm7
  andps xmm0, xmm3
  andnps xmm3, xmm8
  orps xmm3, xmm0
  mov r9d, 1135869952
  movd xmm0, r9d
  subss xmm0, xmm3
  minss xmm3, xmm0
  mulss xmm2, xmm3
  jmp @N88
@N118:
  movaps xmm2, xmm9
  divss xmm2, xmm11
  jmp @N92
@N93:
  ucomiss xmm6, xmm8
  jp @N96
  jne @N96
  subss xmm3, xmm0
  divss xmm3, xmm9
  mov r9d, 1073741824
  movd xmm12, r9d
  addss xmm3, xmm12
  mov r9d, 1114636288
  movd xmm12, r9d
  mulss xmm3, xmm12
  movaps xmm0, xmm3
  jmp @N95
@N100:
  movaps xmm1, xmm6
  pxor xmm2, xmm2
  jmp @N88
@N96:
  subss xmm0, xmm6
  divss xmm0, xmm9
  mov r9d, 1082130432
  movd xmm12, r9d
  addss xmm0, xmm12
  mov r9d, 1114636288
  movd xmm12, r9d
  mulss xmm0, xmm12
  jmp @N95
end;
