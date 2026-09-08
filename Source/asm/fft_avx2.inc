// x86-64 AVX2 fftpack complex passes (radix 2, 3, 4, 5), four complex per step
procedure passf2_avx2(const ido, l1: Int32; const cc,chp,wa1: PSingle; const isign: Int32);
begin
  asm
    mov      eax, isign
    vpcmpeqd ymm7, ymm7, ymm7
    vpslld   ymm7, ymm7, 31
    cmp      eax, 1
    je       @@even
    vpsllq   ymm7, ymm7, 32         // ODD
    jmp      @@mdone
  @@even:
    vpsrlq   ymm7, ymm7, 32         // EVEN
  @@mdone:
    mov      r8d, ido
    shl      r8, 2                  // idob (A1 stride / per-k ac gap)
    mov      eax, l1
    imul     rax, r8                // d1b
    mov      r10, rax
    mov      r13d, ido
    shr      r13d, 1                // ido_complex
    mov      r14d, r13d
    and      r14d, 3                // rem
    shr      r13d, 2                // nquad
    mov      rax, cc
    mov      rdx, chp
    mov      rsi, wa1
    mov      ebx, l1
    test     ebx, ebx
    jz       @@done
    cmp      r8, 8
    jne      @@k                    // rows of one complex: the last pass, twiddles all 1, the
    cmp      ebx, 4                 // two inputs of a butterfly consecutive. Four butterflies
    jl       @@k                    // at a time, gathered so each output row stores whole.
  @@one4:
    vmovups   ymm0, [rax]           // A0k A1k | A0k+1 A1k+1
    vmovups   ymm1, [rax+32]        // k+2, k+3
    vunpcklpd ymm2, ymm0, ymm1      // A0k A0k+2 | A0k+1 A0k+3
    vunpckhpd ymm3, ymm0, ymm1      // A1k A1k+2 | A1k+1 A1k+3
    vpermpd   ymm2, ymm2, $d8       // A0 of k..k+3
    vpermpd   ymm3, ymm3, $d8       // A1
    vaddps    ymm0, ymm2, ymm3      // out0 = A0+A1
    vmovups   [rdx], ymm0
    vsubps    ymm2, ymm2, ymm3      // out1 = A0-A1
    vmovups   [rdx+r10], ymm2
    add      rax, 64
    add      rdx, 32
    sub      ebx, 4
    cmp      ebx, 4
    jge      @@one4
    test     ebx, ebx
    jz       @@done
  @@k:
    mov      rcx, rsi
    mov      r12d, r13d
    test     r12d, r12d
    jz       @@rem
  @@quad:
    vmovups   ymm0, [rax]           // A0
    vmovups   ymm1, [rax+r8]        // A1
    vaddps    ymm2, ymm0, ymm1      // out0
    vmovups   [rdx], ymm2
    vsubps    ymm0, ymm0, ymm1      // D
    vmovups   ymm5, [rcx]           // W
    vshufps   ymm1, ymm0, ymm0, $b1
    vmovshdup ymm6, ymm5
    vmulps    ymm1, ymm1, ymm6
    vxorps    ymm1, ymm1, ymm7
    vmovsldup ymm5, ymm5
    vmulps    ymm0, ymm0, ymm5
    vaddps    ymm0, ymm0, ymm1
    vmovups   [rdx+r10], ymm0       // out1
    add      rax, 32
    add      rdx, 32
    add      rcx, 32
    dec      r12d
    jnz      @@quad
  @@rem:
    test     r14d, r14d
    jz       @@knext
    mov      r15d, r14d
  @@remloop:
    vmovq     xmm0, [rax]
    vmovq     xmm1, [rax+r8]
    vaddps    xmm2, xmm0, xmm1
    vmovq     [rdx], xmm2
    vsubps    xmm0, xmm0, xmm1
    vmovq     xmm5, [rcx]
    vshufps   xmm1, xmm0, xmm0, $b1
    vmovshdup xmm6, xmm5
    vmulps    xmm1, xmm1, xmm6
    vxorps    xmm1, xmm1, xmm7
    vmovsldup xmm5, xmm5
    vmulps    xmm0, xmm0, xmm5
    vaddps    xmm0, xmm0, xmm1
    vmovq     [rdx+r10], xmm0
    add      rax, 8
    add      rdx, 8
    add      rcx, 8
    dec      r15d
    jnz      @@remloop
  @@knext:
    add      rax, r8                // gap = idob
    dec      ebx
    jnz      @@k
  @@done:
    vzeroupper
  end ['rax','rbx','rcx','rdx','rsi','r8','r10','r12','r13','r14','r15',
       'xmm0','xmm1','xmm2','xmm5','xmm6','xmm7'];
end;

procedure passf3_avx2(const ido, l1: Int32; const cc, chp, wa1,wa2: PSingle; const isign: Int32);
begin
  asm
    vpcmpeqd ymm7, ymm7, ymm7
    vpslld   ymm7, ymm7, 31
    vpsrlq   ymm7, ymm7, 32         // EVEN
    mov      eax, $BF000000
    vmovd    xmm4, eax
    vbroadcastss ymm4, xmm4         // taur
    mov      eax, isign
    cmp      eax, 1
    jne      @@neg
    mov      eax, $3F5DB3D7
    vmovd    xmm6, eax
    vbroadcastss ymm6, xmm6         // staui = +taui
    vmovaps  ymm8, ymm7             // TW = EVEN
    jmp      @@cdone
  @@neg:
    mov      eax, $BF5DB3D7
    vmovd    xmm6, eax
    vbroadcastss ymm6, xmm6         // staui = -taui
    vpcmpeqd ymm8, ymm8, ymm8
    vpslld   ymm8, ymm8, 31
    vpsllq   ymm8, ymm8, 32         // TW = ODD
  @@cdone:
    mov      r8d, ido
    shl      r8, 2                  // idob
    mov      eax, l1
    imul     rax, r8                // d1b
    mov      r10, rax
    mov      r13d, ido
    shr      r13d, 1
    mov      r14d, r13d
    and      r14d, 3
    shr      r13d, 2                // nquad
    mov      rax, cc                // X0 base
    mov      rdx, chp
    mov      rsi, wa1
    mov      ebx, l1
    test     ebx, ebx
    jz       @@done
    cmp      r8, 8
    jne      @@k                    // rows of one complex: the last pass, twiddles all 1
  @@one:
    vmovq     xmm1, [rax+8]         // X1
    vmovq     xmm2, [rax+16]        // X2
    vaddps    xmm0, xmm1, xmm2      // T2
    vsubps    xmm1, xmm1, xmm2      // Xd
    vmulps    xmm1, xmm1, xmm6      // C3 = staui*Xd
    vmovq     xmm2, [rax]           // X0
    vaddps    xmm3, xmm2, xmm0      // out0
    vmovq     [rdx], xmm3
    vmulps    xmm0, xmm0, xmm4      // T2*taur
    vaddps    xmm0, xmm0, xmm2      // C2
    vshufps   xmm1, xmm1, xmm1, $b1
    vxorps    xmm1, xmm1, xmm7      // iC3
    vaddps    xmm3, xmm0, xmm1      // out1 = D2
    vmovq     [rdx+r10], xmm3
    vsubps    xmm0, xmm0, xmm1      // out2 = D3
    vmovq     [rdx+r10*2], xmm0
    add      rax, 24
    add      rdx, 8
    dec      ebx
    jnz      @@one
    jmp      @@done
  @@k:
    mov      rcx, rsi
    mov      r12d, r13d
    test     r12d, r12d
    jz       @@rem
  @@quad:
    vmovups   ymm1, [rax+r8]        // X1
    vmovups   ymm2, [rax+r8*2]      // X2
    vaddps    ymm0, ymm1, ymm2      // T2
    vsubps    ymm1, ymm1, ymm2      // Xd
    vmulps    ymm1, ymm1, ymm6      // C3 = staui*Xd
    vmovups   ymm2, [rax]           // X0
    vaddps    ymm3, ymm2, ymm0      // out0
    vmovups   [rdx], ymm3
    vmulps    ymm0, ymm0, ymm4      // T2*taur
    vaddps    ymm0, ymm0, ymm2      // C2
    vshufps   ymm1, ymm1, ymm1, $b1
    vxorps    ymm1, ymm1, ymm7      // iC3
    vaddps    ymm3, ymm0, ymm1      // D2
    vsubps    ymm0, ymm0, ymm1      // D3
    vmovups   ymm5, [rcx]           // wa1
    vshufps   ymm1, ymm3, ymm3, $b1
    vmovshdup ymm2, ymm5
    vmulps    ymm1, ymm1, ymm2
    vxorps    ymm1, ymm1, ymm8
    vmovsldup ymm5, ymm5
    vmulps    ymm3, ymm3, ymm5
    vaddps    ymm3, ymm3, ymm1
    vmovups   [rdx+r10], ymm3       // out1
    vmovups   ymm5, [rcx+r8]        // wa2
    vshufps   ymm1, ymm0, ymm0, $b1
    vmovshdup ymm2, ymm5
    vmulps    ymm1, ymm1, ymm2
    vxorps    ymm1, ymm1, ymm8
    vmovsldup ymm5, ymm5
    vmulps    ymm0, ymm0, ymm5
    vaddps    ymm0, ymm0, ymm1
    vmovups   [rdx+r10*2], ymm0     // out2
    add      rax, 32
    add      rdx, 32
    add      rcx, 32
    dec      r12d
    jnz      @@quad
  @@rem:
    test     r14d, r14d
    jz       @@knext
    mov      r15d, r14d
  @@remloop:
    vmovq     xmm1, [rax+r8]
    vmovq     xmm2, [rax+r8*2]
    vaddps    xmm0, xmm1, xmm2
    vsubps    xmm1, xmm1, xmm2
    vmulps    xmm1, xmm1, xmm6
    vmovq     xmm2, [rax]
    vaddps    xmm3, xmm2, xmm0
    vmovq     [rdx], xmm3
    vmulps    xmm0, xmm0, xmm4
    vaddps    xmm0, xmm0, xmm2
    vshufps   xmm1, xmm1, xmm1, $b1
    vxorps    xmm1, xmm1, xmm7
    vaddps    xmm3, xmm0, xmm1
    vsubps    xmm0, xmm0, xmm1
    vmovq     xmm5, [rcx]
    vshufps   xmm1, xmm3, xmm3, $b1
    vmovshdup xmm2, xmm5
    vmulps    xmm1, xmm1, xmm2
    vxorps    xmm1, xmm1, xmm8
    vmovsldup xmm5, xmm5
    vmulps    xmm3, xmm3, xmm5
    vaddps    xmm3, xmm3, xmm1
    vmovq     [rdx+r10], xmm3
    vmovq     xmm5, [rcx+r8]
    vshufps   xmm1, xmm0, xmm0, $b1
    vmovshdup xmm2, xmm5
    vmulps    xmm1, xmm1, xmm2
    vxorps    xmm1, xmm1, xmm8
    vmovsldup xmm5, xmm5
    vmulps    xmm0, xmm0, xmm5
    vaddps    xmm0, xmm0, xmm1
    vmovq     [rdx+r10*2], xmm0
    add      rax, 8
    add      rdx, 8
    add      rcx, 8
    dec      r15d
    jnz      @@remloop
  @@knext:
    lea      rax, [rax+r8*2]        // X0 gap = 2*ido
    dec      ebx
    jnz      @@k
  @@done:
    vzeroupper
  end ['rax','rbx','rcx','rdx','rsi','r8','r10','r12','r13','r14','r15',
       'xmm0','xmm1','xmm2','xmm3','xmm4','xmm5','xmm6','xmm7','xmm8'];
end;

procedure passf4_avx2(const o1, l1: Int32; const cc, chp, wa1, wa2, wa3: PSingle; const isign: Int32);
begin
  asm
    mov      eax, isign
    vpcmpeqd ymm7, ymm7, ymm7
    vpslld   ymm7, ymm7, 31
    cmp      eax, 1
    je       @@even
    vpsllq   ymm7, ymm7, 32         // ODD  {0,s,0,s,...}
    jmp      @@mdone
  @@even:
    vpsrlq   ymm7, ymm7, 32         // EVEN {s,0,s,0,...}
  @@mdone:
    mov      r8d, o1
    shl      r8, 2                  // o1b
    lea      r9, [r8+r8*2]          // o3b = 3*o1b (A3 offset + per-k ac gap)
    mov      eax, l1
    imul     rax, r8                // d1b = l1*o1b
    mov      r10, rax
    lea      r11, [r10+r10*2]       // d3b = 3*d1b
    mov      r13d, o1
    shr      r13d, 1                // ido_complex
    mov      r14d, r13d
    and      r14d, 3                // rem (0..3)
    shr      r13d, 2                // nquad
    mov      rax, cc
    mov      rdx, chp
    mov      rsi, wa1
    mov      ebx, l1
    test     ebx, ebx
    jz       @@done
    cmp      r8, 8
    jne      @@k                    // rows of one complex: the last pass. Its twiddles are all
    cmp      ebx, 4                 // 1, and the four inputs of a butterfly are consecutive, so
    jl       @@k                    // take four butterflies, transpose, and do them vertically.
  @@one4:
    vmovups   ymm0, [rax]           // butterfly k:   A0 A1 A2 A3
    vmovups   ymm1, [rax+32]        // k+1
    vmovups   ymm2, [rax+64]        // k+2
    vmovups   ymm3, [rax+96]        // k+3
    vunpcklpd ymm4, ymm0, ymm1      // A0k A0k+1 | A2k A2k+1
    vunpckhpd ymm5, ymm0, ymm1      // A1k A1k+1 | A3k A3k+1
    vunpcklpd ymm6, ymm2, ymm3
    vunpckhpd ymm8, ymm2, ymm3
    vperm2f128 ymm0, ymm4, ymm6, $20  // A0 of k..k+3
    vperm2f128 ymm2, ymm4, ymm6, $31  // A2
    vperm2f128 ymm1, ymm5, ymm8, $20  // A1
    vperm2f128 ymm3, ymm5, ymm8, $31  // A3
    vaddps    ymm4, ymm0, ymm2      // T2
    vsubps    ymm0, ymm0, ymm2      // T1
    vaddps    ymm5, ymm1, ymm3      // T3
    vsubps    ymm1, ymm1, ymm3      // TD
    vpermilps ymm1, ymm1, $b1
    vxorps    ymm1, ymm1, ymm7      // iD_eff
    vaddps    ymm6, ymm4, ymm5      // out0 = T2+T3
    vmovups   [rdx], ymm6
    vsubps    ymm4, ymm4, ymm5      // out2 = T2-T3
    vmovups   [rdx+r10*2], ymm4
    vaddps    ymm6, ymm0, ymm1      // out1 = T1+iD_eff
    vmovups   [rdx+r10], ymm6
    vsubps    ymm0, ymm0, ymm1      // out3 = T1-iD_eff
    vmovups   [rdx+r11], ymm0
    add      rax, 128
    add      rdx, 32
    sub      ebx, 4
    cmp      ebx, 4
    jge      @@one4
    test     ebx, ebx
    jz       @@done                 // the last 0..3 butterflies go through the row loop below
  @@k:
    mov      rcx, rsi               // wa = wa1
    mov      r12d, r13d
    test     r12d, r12d
    jz       @@rem
  @@quad:
    vmovups   ymm0, [rax]           // A0 (4 complex)
    vmovups   ymm1, [rax+r8*2]      // A2
    vaddps    ymm2, ymm0, ymm1      // T2
    vsubps    ymm0, ymm0, ymm1      // T1
    vmovups   ymm1, [rax+r8]        // A1
    vmovups   ymm3, [rax+r9]        // A3
    vaddps    ymm4, ymm1, ymm3      // T3
    vsubps    ymm1, ymm1, ymm3      // TD = A1-A3
    vshufps   ymm1, ymm1, ymm1, $b1
    vxorps    ymm1, ymm1, ymm7      // iD_eff
    vaddps    ymm3, ymm2, ymm4      // out0 = T2+T3
    vmovups   [rdx], ymm3
    vsubps    ymm2, ymm2, ymm4      // C3 = T2-T3
    vmovups   ymm5, [rcx+r8]        // wa2
    vshufps   ymm3, ymm2, ymm2, $b1
    vmovshdup ymm6, ymm5
    vmulps    ymm3, ymm3, ymm6
    vxorps    ymm3, ymm3, ymm7
    vmovsldup ymm5, ymm5
    vmulps    ymm2, ymm2, ymm5
    vaddps    ymm2, ymm2, ymm3
    vmovups   [rdx+r10*2], ymm2     // out2 = wa2 (x) C3
    vaddps    ymm2, ymm0, ymm1      // C2 = T1+iD_eff
    vmovups   ymm5, [rcx]           // wa1
    vshufps   ymm3, ymm2, ymm2, $b1
    vmovshdup ymm6, ymm5
    vmulps    ymm3, ymm3, ymm6
    vxorps    ymm3, ymm3, ymm7
    vmovsldup ymm5, ymm5
    vmulps    ymm2, ymm2, ymm5
    vaddps    ymm2, ymm2, ymm3
    vmovups   [rdx+r10], ymm2       // out1 = wa1 (x) C2
    vsubps    ymm0, ymm0, ymm1      // C4 = T1-iD_eff
    vmovups   ymm5, [rcx+r8*2]      // wa3
    vshufps   ymm3, ymm0, ymm0, $b1
    vmovshdup ymm6, ymm5
    vmulps    ymm3, ymm3, ymm6
    vxorps    ymm3, ymm3, ymm7
    vmovsldup ymm5, ymm5
    vmulps    ymm0, ymm0, ymm5
    vaddps    ymm0, ymm0, ymm3
    vmovups   [rdx+r11], ymm0       // out3 = wa3 (x) C4
    add      rax, 32
    add      rdx, 32
    add      rcx, 32
    dec      r12d
    jnz      @@quad
  @@rem:
    test     r14d, r14d
    jz       @@knext
    mov      r15d, r14d
  @@remloop:
    vmovq     xmm0, [rax]
    vmovq     xmm1, [rax+r8*2]
    vaddps    xmm2, xmm0, xmm1
    vsubps    xmm0, xmm0, xmm1
    vmovq     xmm1, [rax+r8]
    vmovq     xmm3, [rax+r9]
    vaddps    xmm4, xmm1, xmm3
    vsubps    xmm1, xmm1, xmm3
    vshufps   xmm1, xmm1, xmm1, $b1
    vxorps    xmm1, xmm1, xmm7
    vaddps    xmm3, xmm2, xmm4
    vmovq     [rdx], xmm3
    vsubps    xmm2, xmm2, xmm4
    vmovq     xmm5, [rcx+r8]
    vshufps   xmm3, xmm2, xmm2, $b1
    vmovshdup xmm6, xmm5
    vmulps    xmm3, xmm3, xmm6
    vxorps    xmm3, xmm3, xmm7
    vmovsldup xmm5, xmm5
    vmulps    xmm2, xmm2, xmm5
    vaddps    xmm2, xmm2, xmm3
    vmovq     [rdx+r10*2], xmm2
    vaddps    xmm2, xmm0, xmm1
    vmovq     xmm5, [rcx]
    vshufps   xmm3, xmm2, xmm2, $b1
    vmovshdup xmm6, xmm5
    vmulps    xmm3, xmm3, xmm6
    vxorps    xmm3, xmm3, xmm7
    vmovsldup xmm5, xmm5
    vmulps    xmm2, xmm2, xmm5
    vaddps    xmm2, xmm2, xmm3
    vmovq     [rdx+r10], xmm2
    vsubps    xmm0, xmm0, xmm1
    vmovq     xmm5, [rcx+r8*2]
    vshufps   xmm3, xmm0, xmm0, $b1
    vmovshdup xmm6, xmm5
    vmulps    xmm3, xmm3, xmm6
    vxorps    xmm3, xmm3, xmm7
    vmovsldup xmm5, xmm5
    vmulps    xmm0, xmm0, xmm5
    vaddps    xmm0, xmm0, xmm3
    vmovq     [rdx+r11], xmm0
    add      rax, 8
    add      rdx, 8
    add      rcx, 8
    dec      r15d
    jnz      @@remloop
  @@knext:
    add      rax, r9                // ac gap to next k
    dec      ebx
    jnz      @@k
  @@done:
    vzeroupper
  end ['rax','rbx','rcx','rdx','rsi','r8','r9','r10','r11','r12','r13','r14','r15',
       'xmm0','xmm1','xmm2','xmm3','xmm4','xmm5','xmm6','xmm7','xmm8'];
end;

procedure passf5_avx2(const ido, l1: Int32; const cc,chp,wa1,wa2,wa3,wa4: PSingle; const isign: Int32);
begin
  asm
    vpcmpeqd ymm11, ymm11, ymm11
    vpslld   ymm11, ymm11, 31
    vpsrlq   ymm11, ymm11, 32       // EVEN
    mov      eax, $3E9E377A
    vmovd    xmm7, eax
    vbroadcastss ymm7, xmm7         // tr11
    mov      eax, $BF4F1BBD
    vmovd    xmm8, eax
    vbroadcastss ymm8, xmm8         // tr12
    mov      eax, isign
    cmp      eax, 1
    jne      @@neg
    mov      eax, $3F737871
    vmovd    xmm9, eax
    vbroadcastss ymm9, xmm9         // sti11 = +ti11
    mov      eax, $3F167918
    vmovd    xmm10, eax
    vbroadcastss ymm10, xmm10       // sti12 = +ti12
    vmovaps  ymm12, ymm11           // TW = EVEN
    jmp      @@cdone
  @@neg:
    mov      eax, $BF737871
    vmovd    xmm9, eax
    vbroadcastss ymm9, xmm9         // sti11 = -ti11
    mov      eax, $BF167918
    vmovd    xmm10, eax
    vbroadcastss ymm10, xmm10       // sti12 = -ti12
    vpcmpeqd ymm12, ymm12, ymm12
    vpslld   ymm12, ymm12, 31
    vpsllq   ymm12, ymm12, 32       // TW = ODD
  @@cdone:
    mov      r8d, ido
    shl      r8, 2                  // idob
    lea      r9, [r8+r8*2]          // ido3b
    mov      eax, l1
    imul     rax, r8                // idlb
    mov      r10, rax
    lea      r11, [r10+r10*2]       // idl3b
    mov      r13d, ido
    shr      r13d, 1
    mov      r14d, r13d
    and      r14d, 3
    shr      r13d, 2                // nquad
    mov      rax, cc                // Z0 base
    mov      rdx, chp
    mov      rsi, wa1
    mov      ebx, l1
    test     ebx, ebx
    jz       @@done
    cmp      r8, 8
    jne      @@k                    // rows of one complex: the last pass, twiddles all 1
  @@one:
    vmovq     xmm0, [rax+8]         // Z1
    vmovq     xmm1, [rax+32]        // Z4
    vaddps    xmm2, xmm0, xmm1
    vsubps    xmm0, xmm0, xmm1
    vmovq     xmm1, [rax+16]        // Z2
    vmovq     xmm3, [rax+24]        // Z3
    vaddps    xmm4, xmm1, xmm3
    vsubps    xmm1, xmm1, xmm3
    vmovq     xmm3, [rax]           // Z0
    vaddps    xmm5, xmm3, xmm2
    vaddps    xmm5, xmm5, xmm4
    vmovq     [rdx], xmm5           // out0
    vmulps    xmm5, xmm2, xmm7
    vaddps    xmm5, xmm5, xmm3
    vmulps    xmm6, xmm4, xmm8
    vaddps    xmm5, xmm5, xmm6      // C2
    vmulps    xmm6, xmm2, xmm8
    vaddps    xmm6, xmm6, xmm3
    vmulps    xmm2, xmm4, xmm7
    vaddps    xmm6, xmm6, xmm2      // C3
    vmulps    xmm2, xmm0, xmm9
    vmulps    xmm3, xmm1, xmm10
    vaddps    xmm2, xmm2, xmm3      // C5
    vmulps    xmm0, xmm0, xmm10
    vmulps    xmm1, xmm1, xmm9
    vsubps    xmm0, xmm0, xmm1      // C4
    vshufps   xmm1, xmm2, xmm2, $b1
    vxorps    xmm1, xmm1, xmm11
    vaddps    xmm3, xmm5, xmm1      // D2
    vmovq     [rdx+r10], xmm3       // out1
    vsubps    xmm5, xmm5, xmm1      // D5
    vmovq     [rdx+r10*4], xmm5     // out4
    vshufps   xmm1, xmm0, xmm0, $b1
    vxorps    xmm1, xmm1, xmm11
    vaddps    xmm4, xmm6, xmm1      // D3
    vmovq     [rdx+r10*2], xmm4     // out2
    vsubps    xmm6, xmm6, xmm1      // D4
    vmovq     [rdx+r11], xmm6       // out3
    add      rax, 40
    add      rdx, 8
    dec      ebx
    jnz      @@one
    jmp      @@done
  @@k:
    mov      rcx, rsi
    mov      r12d, r13d
    test     r12d, r12d
    jz       @@rem
  @@quad:
    vmovups   ymm0, [rax+r8]        // Z1
    vmovups   ymm1, [rax+r8*4]      // Z4
    vaddps    ymm2, ymm0, ymm1      // T2  -> need T2,T5 ; keep T2 in ymm0? use temps
    vsubps    ymm0, ymm0, ymm1      // T5 (ymm0)  ; T2 in ymm2
    vmovups   ymm1, [rax+r8*2]      // Z2
    vmovups   ymm3, [rax+r9]        // Z3
    vaddps    ymm4, ymm1, ymm3      // T3 (ymm4)
    vsubps    ymm1, ymm1, ymm3      // T4 (ymm1)
    vmovups   ymm3, [rax]           // Z0
    vaddps    ymm5, ymm3, ymm2      // Z0+T2
    vaddps    ymm5, ymm5, ymm4      // out0
    vmovups   [rdx], ymm5
    // C2 = Z0 + tr11*T2 + tr12*T3 ; C3 = Z0 + tr12*T2 + tr11*T3
    vmulps    ymm5, ymm2, ymm7      // tr11*T2
    vaddps    ymm5, ymm5, ymm3
    vmulps    ymm6, ymm4, ymm8      // tr12*T3
    vaddps    ymm5, ymm5, ymm6      // C2 (ymm5)
    vmulps    ymm6, ymm2, ymm8      // tr12*T2
    vaddps    ymm6, ymm6, ymm3
    vmulps    ymm2, ymm4, ymm7      // tr11*T3   (T2 freed)
    vaddps    ymm6, ymm6, ymm2      // C3 (ymm6)
    // C5 = sti11*T5 + sti12*T4 ; C4 = sti12*T5 - sti11*T4   (T5=ymm0, T4=ymm1)
    vmulps    ymm2, ymm0, ymm9      // sti11*T5
    vmulps    ymm3, ymm1, ymm10     // sti12*T4
    vaddps    ymm2, ymm2, ymm3      // C5 (ymm2)
    vmulps    ymm0, ymm0, ymm10     // sti12*T5
    vmulps    ymm1, ymm1, ymm9      // sti11*T4
    vsubps    ymm0, ymm0, ymm1      // C4 (ymm0)
    // D2=C2+iC5, D5=C2-iC5 ; D3=C3+iC4, D4=C3-iC4
    vshufps   ymm1, ymm2, ymm2, $b1
    vxorps    ymm1, ymm1, ymm11     // iC5
    vaddps    ymm3, ymm5, ymm1      // D2 (ymm3)
    vsubps    ymm5, ymm5, ymm1      // D5 (ymm5)
    vshufps   ymm1, ymm0, ymm0, $b1
    vxorps    ymm1, ymm1, ymm11     // iC4
    vaddps    ymm4, ymm6, ymm1      // D3 (ymm4)
    vsubps    ymm6, ymm6, ymm1      // D4 (ymm6)
    // out1=wa1(x)D2 -> [rdx+r10]
    vmovups   ymm0, [rcx]
    vshufps   ymm1, ymm3, ymm3, $b1
    vmovshdup ymm2, ymm0
    vmulps    ymm1, ymm1, ymm2
    vxorps    ymm1, ymm1, ymm12
    vmovsldup ymm0, ymm0
    vmulps    ymm3, ymm3, ymm0
    vaddps    ymm3, ymm3, ymm1
    vmovups   [rdx+r10], ymm3
    // out2=wa2(x)D3 -> [rdx+r10*2]
    vmovups   ymm0, [rcx+r8]
    vshufps   ymm1, ymm4, ymm4, $b1
    vmovshdup ymm2, ymm0
    vmulps    ymm1, ymm1, ymm2
    vxorps    ymm1, ymm1, ymm12
    vmovsldup ymm0, ymm0
    vmulps    ymm4, ymm4, ymm0
    vaddps    ymm4, ymm4, ymm1
    vmovups   [rdx+r10*2], ymm4
    // out3=wa3(x)D4 -> [rdx+r11]
    vmovups   ymm0, [rcx+r8*2]
    vshufps   ymm1, ymm6, ymm6, $b1
    vmovshdup ymm2, ymm0
    vmulps    ymm1, ymm1, ymm2
    vxorps    ymm1, ymm1, ymm12
    vmovsldup ymm0, ymm0
    vmulps    ymm6, ymm6, ymm0
    vaddps    ymm6, ymm6, ymm1
    vmovups   [rdx+r11], ymm6
    // out4=wa4(x)D5 -> [rdx+r10*4]
    vmovups   ymm0, [rcx+r9]
    vshufps   ymm1, ymm5, ymm5, $b1
    vmovshdup ymm2, ymm0
    vmulps    ymm1, ymm1, ymm2
    vxorps    ymm1, ymm1, ymm12
    vmovsldup ymm0, ymm0
    vmulps    ymm5, ymm5, ymm0
    vaddps    ymm5, ymm5, ymm1
    vmovups   [rdx+r10*4], ymm5
    add      rax, 32
    add      rdx, 32
    add      rcx, 32
    dec      r12d
    jnz      @@quad
  @@rem:
    test     r14d, r14d
    jz       @@knext
    mov      r15d, r14d
  @@remloop:
    vmovq     xmm0, [rax+r8]
    vmovq     xmm1, [rax+r8*4]
    vaddps    xmm2, xmm0, xmm1
    vsubps    xmm0, xmm0, xmm1
    vmovq     xmm1, [rax+r8*2]
    vmovq     xmm3, [rax+r9]
    vaddps    xmm4, xmm1, xmm3
    vsubps    xmm1, xmm1, xmm3
    vmovq     xmm3, [rax]
    vaddps    xmm5, xmm3, xmm2
    vaddps    xmm5, xmm5, xmm4
    vmovq     [rdx], xmm5
    vmulps    xmm5, xmm2, xmm7
    vaddps    xmm5, xmm5, xmm3
    vmulps    xmm6, xmm4, xmm8
    vaddps    xmm5, xmm5, xmm6      // C2
    vmulps    xmm6, xmm2, xmm8
    vaddps    xmm6, xmm6, xmm3
    vmulps    xmm2, xmm4, xmm7
    vaddps    xmm6, xmm6, xmm2      // C3
    vmulps    xmm2, xmm0, xmm9
    vmulps    xmm3, xmm1, xmm10
    vaddps    xmm2, xmm2, xmm3      // C5
    vmulps    xmm0, xmm0, xmm10
    vmulps    xmm1, xmm1, xmm9
    vsubps    xmm0, xmm0, xmm1      // C4
    vshufps   xmm1, xmm2, xmm2, $b1
    vxorps    xmm1, xmm1, xmm11
    vaddps    xmm3, xmm5, xmm1      // D2
    vsubps    xmm5, xmm5, xmm1      // D5
    vshufps   xmm1, xmm0, xmm0, $b1
    vxorps    xmm1, xmm1, xmm11
    vaddps    xmm4, xmm6, xmm1      // D3
    vsubps    xmm6, xmm6, xmm1      // D4
    vmovq     xmm0, [rcx]
    vshufps   xmm1, xmm3, xmm3, $b1
    vmovshdup xmm2, xmm0
    vmulps    xmm1, xmm1, xmm2
    vxorps    xmm1, xmm1, xmm12
    vmovsldup xmm0, xmm0
    vmulps    xmm3, xmm3, xmm0
    vaddps    xmm3, xmm3, xmm1
    vmovq     [rdx+r10], xmm3
    vmovq     xmm0, [rcx+r8]
    vshufps   xmm1, xmm4, xmm4, $b1
    vmovshdup xmm2, xmm0
    vmulps    xmm1, xmm1, xmm2
    vxorps    xmm1, xmm1, xmm12
    vmovsldup xmm0, xmm0
    vmulps    xmm4, xmm4, xmm0
    vaddps    xmm4, xmm4, xmm1
    vmovq     [rdx+r10*2], xmm4
    vmovq     xmm0, [rcx+r8*2]
    vshufps   xmm1, xmm6, xmm6, $b1
    vmovshdup xmm2, xmm0
    vmulps    xmm1, xmm1, xmm2
    vxorps    xmm1, xmm1, xmm12
    vmovsldup xmm0, xmm0
    vmulps    xmm6, xmm6, xmm0
    vaddps    xmm6, xmm6, xmm1
    vmovq     [rdx+r11], xmm6
    vmovq     xmm0, [rcx+r9]
    vshufps   xmm1, xmm5, xmm5, $b1
    vmovshdup xmm2, xmm0
    vmulps    xmm1, xmm1, xmm2
    vxorps    xmm1, xmm1, xmm12
    vmovsldup xmm0, xmm0
    vmulps    xmm5, xmm5, xmm0
    vaddps    xmm5, xmm5, xmm1
    vmovq     [rdx+r10*4], xmm5
    add      rax, 8
    add      rdx, 8
    add      rcx, 8
    dec      r15d
    jnz      @@remloop
  @@knext:
    lea      rax, [rax+r8*4]        // Z0 gap = 4*ido
    dec      ebx
    jnz      @@k
  @@done:
    vzeroupper
  end ['rax','rbx','rcx','rdx','rsi','r8','r9','r10','r11','r12','r13','r14','r15',
       'xmm0','xmm1','xmm2','xmm3','xmm4','xmm5','xmm6','xmm7','xmm8','xmm9',
       'xmm10','xmm11','xmm12'];
end;

