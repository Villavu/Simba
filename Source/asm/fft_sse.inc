procedure passf2_sse(const ido, l1: Int32; const cc,chp,wa1: PSingle; const isign: Int32);
begin
  asm
  // --- strides, mask, bases ---
  mov     r9d, ido              // raw ido (= idot)
  mov     eax, l1
  imul    eax, r9d              // d1 = l1*ido
  mov     r10d, eax
  shl     r10, 2                // r10 = d1b
  mov     r8d, r9d
  shl     r8, 2                 // r8 = idob (A1 stride / per-k ac gap)
  mov     eax, isign
  pcmpeqd xmm7, xmm7
  pslld   xmm7, 31              // {s,s,s,s}
  cmp     eax, 1
  je      @@evenmask
  psllq   xmm7, 32              // ODD  {0,s,0,s}
  jmp     @@maskdone
@@evenmask:
  psrlq   xmm7, 32              // EVEN {s,0,s,0}
@@maskdone:
  mov     rax, cc               // ac
  mov     rdx, chp              // ah
  mov     rsi, wa1              // wa1 base
  mov     ebx, l1               // k counter
  test    ebx, ebx
  jz      @@done
  // The ido=2 case is just ido_complex=1: the first twiddle wa1[0..1]=(1,0) makes
  // the multiply an identity, so the general path (npairs=0, tail=1) computes it
  // bit-exactly. No special case needed.
  shr     r9d, 1                // ido_complex
  mov     r13d, r9d
  and     r13d, 1               // r13 = tail flag (1 if ido_complex odd)
  shr     r9d, 1
  mov     r11d, r9d             // r11 = npairs
@@gk:
  mov     rcx, rsi              // wa = wa1 base (i resets to 0 each k)
  mov     r12d, r11d            // inner = npairs
  test    r12d, r12d
  jz      @@gtail
@@gp:
  movups  xmm0, [rax]           // A0 (2 complex)
  movups  xmm1, [rax+r8]        // A1
  movaps  xmm2, xmm0
  addps   xmm0, xmm1            // out0
  movups  [rdx], xmm0
  subps   xmm2, xmm1            // D
  movups  xmm5, [rcx]           // W
  movaps  xmm1, xmm2
  shufps  xmm1, xmm1, $b1
  movaps  xmm3, xmm5
  shufps  xmm3, xmm3, $f5
  mulps   xmm1, xmm3
  xorps   xmm1, xmm7
  shufps  xmm5, xmm5, $a0
  mulps   xmm2, xmm5
  addps   xmm2, xmm1
  movups  [rdx+r10], xmm2       // out1
  add     rax, 16
  add     rdx, 16
  add     rcx, 16
  dec     r12d
  jnz     @@gp
@@gtail:
  test    r13d, r13d
  jz      @@gknext
  movq     xmm0, [rax]           // last single complex
  movq     xmm1, [rax+r8]
  movaps  xmm2, xmm0
  addps   xmm0, xmm1
  movq     [rdx], xmm0
  subps   xmm2, xmm1
  movq     xmm5, [rcx]
  movaps  xmm1, xmm2
  shufps  xmm1, xmm1, $b1
  movaps  xmm3, xmm5
  shufps  xmm3, xmm3, $f5
  mulps   xmm1, xmm3
  xorps   xmm1, xmm7
  shufps  xmm5, xmm5, $a0
  mulps   xmm2, xmm5
  addps   xmm2, xmm1
  movq     [rdx+r10], xmm2
  add     rax, 8
  add     rdx, 8
  add     rcx, 8
@@gknext:
  add     rax, r8               // ac: skip gap to next k (inner advanced ido, k-stride 2*ido)
  // ah already flowed exactly one k-stride
  dec     ebx
  jnz     @@gk
@@done:
  end ['rax','rbx','rcx','rdx','rsi','r8','r9','r10','r11','r12','r13',
       'xmm0','xmm1','xmm2','xmm3','xmm5','xmm7'];
end; // passf2

procedure passf3_sse(const ido, l1: Int32; const cc, chp, wa1,wa2: PSingle; const isign: Int32);
begin
  asm
    pcmpeqd xmm7, xmm7
    pslld   xmm7, 31
    psrlq   xmm7, 32              // EVEN {s,0,s,0}
    mov     eax, $BF000000
    movd    xmm4, eax
    shufps  xmm4, xmm4, 0         // taur broadcast
    mov     eax, isign
    cmp     eax, 1
    jne     @@neg
    mov     eax, $3F5DB3D7        // +taui
    movd    xmm6, eax
    shufps  xmm6, xmm6, 0
    movaps  xmm8, xmm7            // TW = EVEN
    jmp     @@cdone
  @@neg:
    mov     eax, $BF5DB3D7        // -taui
    movd    xmm6, eax
    shufps  xmm6, xmm6, 0
    pcmpeqd xmm8, xmm8
    pslld   xmm8, 31
    psllq   xmm8, 32              // TW = ODD
  @@cdone:
    mov     r8d, ido
    shl     r8, 2                 // idob (group stride and wa1->wa2)
    mov     eax, l1
    imul    rax, r8               // d1b = l1*ido*4
    mov     r10, rax              // d1b
    mov     r13d, ido
    shr     r13d, 1               // ido_complex
    mov     r14d, r13d
    and     r14d, 1               // tail
    shr     r13d, 1               // npairs
    mov     rax, cc               // X0 base = cc + 3*(k-1)*ido ; k=0 -> cc
    mov     rdx, chp
    mov     rsi, wa1
    mov     ebx, l1
    test    ebx, ebx
    jz      @@done
  @@k:
    mov     rcx, rsi              // wa = wa1
    mov     r12d, r13d
    test    r12d, r12d
    jz      @@tail
  @@pair:
    movups  xmm1, [rax+r8]        // X1
    movups  xmm2, [rax+r8*2]      // X2
    movaps  xmm0, xmm1
    addps   xmm0, xmm2            // T2
    subps   xmm1, xmm2            // Xd
    mulps   xmm1, xmm6            // C3 = staui*Xd
    movups  xmm2, [rax]           // X0
    movaps  xmm3, xmm2
    addps   xmm3, xmm0            // out0 = X0+T2
    movups  [rdx], xmm3
    mulps   xmm0, xmm4            // T2*taur
    addps   xmm0, xmm2            // C2
    shufps  xmm1, xmm1, $b1
    xorps   xmm1, xmm7            // iC3 (EVEN)
    movaps  xmm3, xmm0
    addps   xmm3, xmm1            // D2 = C2+iC3
    subps   xmm0, xmm1            // D3 = C2-iC3
    movups  xmm5, [rcx]           // wa1
    movaps  xmm1, xmm3
    shufps  xmm1, xmm1, $b1
    movaps  xmm2, xmm5
    shufps  xmm2, xmm2, $f5
    mulps   xmm1, xmm2
    xorps   xmm1, xmm8            // TW
    shufps  xmm5, xmm5, $a0
    mulps   xmm3, xmm5
    addps   xmm3, xmm1
    movups  [rdx+r10], xmm3       // out1
    movups  xmm5, [rcx+r8]        // wa2
    movaps  xmm1, xmm0
    shufps  xmm1, xmm1, $b1
    movaps  xmm2, xmm5
    shufps  xmm2, xmm2, $f5
    mulps   xmm1, xmm2
    xorps   xmm1, xmm8
    shufps  xmm5, xmm5, $a0
    mulps   xmm0, xmm5
    addps   xmm0, xmm1
    movups  [rdx+r10*2], xmm0     // out2
    add     rax, 16
    add     rdx, 16
    add     rcx, 16
    dec     r12d
    jnz     @@pair
  @@tail:
    test    r14d, r14d
    jz      @@knext
    movq    xmm1, [rax+r8]
    movq    xmm2, [rax+r8*2]
    movaps  xmm0, xmm1
    addps   xmm0, xmm2
    subps   xmm1, xmm2
    mulps   xmm1, xmm6
    movq    xmm2, [rax]
    movaps  xmm3, xmm2
    addps   xmm3, xmm0
    movq    [rdx], xmm3
    mulps   xmm0, xmm4
    addps   xmm0, xmm2
    shufps  xmm1, xmm1, $b1
    xorps   xmm1, xmm7
    movaps  xmm3, xmm0
    addps   xmm3, xmm1
    subps   xmm0, xmm1
    movq    xmm5, [rcx]
    movaps  xmm1, xmm3
    shufps  xmm1, xmm1, $b1
    movaps  xmm2, xmm5
    shufps  xmm2, xmm2, $f5
    mulps   xmm1, xmm2
    xorps   xmm1, xmm8
    shufps  xmm5, xmm5, $a0
    mulps   xmm3, xmm5
    addps   xmm3, xmm1
    movq    [rdx+r10], xmm3
    movq    xmm5, [rcx+r8]
    movaps  xmm1, xmm0
    shufps  xmm1, xmm1, $b1
    movaps  xmm2, xmm5
    shufps  xmm2, xmm2, $f5
    mulps   xmm1, xmm2
    xorps   xmm1, xmm8
    shufps  xmm5, xmm5, $a0
    mulps   xmm0, xmm5
    addps   xmm0, xmm1
    movq    [rdx+r10*2], xmm0
    add     rax, 8
    add     rdx, 8
    add     rcx, 8
  @@knext:
    lea     rax, [rax+r8*2]       // X0 base gap = 2*ido (inner advanced ido, k-stride 3*ido)
    dec     ebx
    jnz     @@k
  @@done:
  end ['rax','rbx','rcx','rdx','rsi','r8','r10','r12','r13','r14',
       'xmm0','xmm1','xmm2','xmm3','xmm4','xmm5','xmm6','xmm7','xmm8'];
end; (* passf3 *)

procedure passf4_sse(const o1, l1: Int32; const cc, chp, wa1, wa2, wa3: PSingle; const isign: Int32);
begin
  asm
    mov     eax, isign
    pcmpeqd xmm7, xmm7
    pslld   xmm7, 31
    cmp     eax, 1
    je      @@even
    psllq   xmm7, 32              // ODD  {0,s,0,s}
    jmp     @@mdone
  @@even:
    psrlq   xmm7, 32              // EVEN {s,0,s,0}
  @@mdone:
    mov     r8d, o1
    shl     r8, 2                 // o1b
    lea     r9, [r8+r8*2]         // o3b = 3*o1b  (A3 offset and per-k ac gap)
    mov     eax, l1
    imul    rax, r8               // d1b = l1*o1b
    mov     r10, rax              // d1b
    lea     r11, [r10+r10*2]      // d3b = 3*d1b
    mov     r13d, o1
    shr     r13d, 1               // ido_complex
    mov     r14d, r13d
    and     r14d, 1               // tail flag
    shr     r13d, 1               // npairs
    mov     rax, cc
    mov     rdx, chp
    mov     rsi, wa1
    mov     ebx, l1
    test    ebx, ebx
    jz      @@done
  @@k:
    mov     rcx, rsi              // wa = wa1
    mov     r12d, r13d            // inner = npairs
    test    r12d, r12d
    jz      @@tail
  @@pair:
    movups  xmm0, [rax]           // A0
    movups  xmm1, [rax+r8*2]      // A2
    movaps  xmm2, xmm0
    addps   xmm0, xmm1            // T2
    subps   xmm2, xmm1            // T1
    movups  xmm1, [rax+r8]        // A1
    movups  xmm3, [rax+r9]        // A3
    movaps  xmm4, xmm1
    addps   xmm1, xmm3            // T3
    subps   xmm4, xmm3            // TD = A1-A3
    shufps  xmm4, xmm4, $b1
    xorps   xmm4, xmm7            // iD_eff
    movaps  xmm3, xmm0
    addps   xmm3, xmm1            // out0 = T2+T3
    movups  [rdx], xmm3
    subps   xmm0, xmm1            // C3 = T2-T3
    movups  xmm5, [rcx+r8]        // wa2
    movaps  xmm1, xmm0
    shufps  xmm1, xmm1, $b1
    movaps  xmm3, xmm5
    shufps  xmm3, xmm3, $f5
    mulps   xmm1, xmm3
    xorps   xmm1, xmm7
    shufps  xmm5, xmm5, $a0
    mulps   xmm0, xmm5
    addps   xmm0, xmm1
    movups  [rdx+r10*2], xmm0     // out2
    movaps  xmm0, xmm2
    addps   xmm0, xmm4            // C2 = T1+iD_eff
    movups  xmm5, [rcx]           // wa1
    movaps  xmm1, xmm0
    shufps  xmm1, xmm1, $b1
    movaps  xmm3, xmm5
    shufps  xmm3, xmm3, $f5
    mulps   xmm1, xmm3
    xorps   xmm1, xmm7
    shufps  xmm5, xmm5, $a0
    mulps   xmm0, xmm5
    addps   xmm0, xmm1
    movups  [rdx+r10], xmm0       // out1
    subps   xmm2, xmm4            // C4 = T1-iD_eff
    movups  xmm5, [rcx+r8*2]      // wa3
    movaps  xmm1, xmm2
    shufps  xmm1, xmm1, $b1
    movaps  xmm3, xmm5
    shufps  xmm3, xmm3, $f5
    mulps   xmm1, xmm3
    xorps   xmm1, xmm7
    shufps  xmm5, xmm5, $a0
    mulps   xmm2, xmm5
    addps   xmm2, xmm1
    movups  [rdx+r11], xmm2       // out3
    add     rax, 16
    add     rdx, 16
    add     rcx, 16
    dec     r12d
    jnz     @@pair
  @@tail:
    test    r14d, r14d
    jz      @@knext
    movq     xmm0, [rax]
    movq     xmm1, [rax+r8*2]
    movaps  xmm2, xmm0
    addps   xmm0, xmm1
    subps   xmm2, xmm1
    movq     xmm1, [rax+r8]
    movq     xmm3, [rax+r9]
    movaps  xmm4, xmm1
    addps   xmm1, xmm3
    subps   xmm4, xmm3
    shufps  xmm4, xmm4, $b1
    xorps   xmm4, xmm7
    movaps  xmm3, xmm0
    addps   xmm3, xmm1
    movq     [rdx], xmm3
    subps   xmm0, xmm1
    movq     xmm5, [rcx+r8]
    movaps  xmm1, xmm0
    shufps  xmm1, xmm1, $b1
    movaps  xmm3, xmm5
    shufps  xmm3, xmm3, $f5
    mulps   xmm1, xmm3
    xorps   xmm1, xmm7
    shufps  xmm5, xmm5, $a0
    mulps   xmm0, xmm5
    addps   xmm0, xmm1
    movq     [rdx+r10*2], xmm0
    movaps  xmm0, xmm2
    addps   xmm0, xmm4
    movq     xmm5, [rcx]
    movaps  xmm1, xmm0
    shufps  xmm1, xmm1, $b1
    movaps  xmm3, xmm5
    shufps  xmm3, xmm3, $f5
    mulps   xmm1, xmm3
    xorps   xmm1, xmm7
    shufps  xmm5, xmm5, $a0
    mulps   xmm0, xmm5
    addps   xmm0, xmm1
    movq     [rdx+r10], xmm0
    subps   xmm2, xmm4
    movq     xmm5, [rcx+r8*2]
    movaps  xmm1, xmm2
    shufps  xmm1, xmm1, $b1
    movaps  xmm3, xmm5
    shufps  xmm3, xmm3, $f5
    mulps   xmm1, xmm3
    xorps   xmm1, xmm7
    shufps  xmm5, xmm5, $a0
    mulps   xmm2, xmm5
    addps   xmm2, xmm1
    movq     [rdx+r11], xmm2
    add     rax, 8
    add     rdx, 8
    add     rcx, 8
  @@knext:
    add     rax, r9               // ac gap to next k (inner advanced o1b, k-stride 4*o1b)
    dec     ebx
    jnz     @@k
  @@done:
  end ['rax','rbx','rcx','rdx','rsi','r8','r9','r10','r11','r12','r13','r14',
       'xmm0','xmm1','xmm2','xmm3','xmm4','xmm5','xmm7'];
end; (* passf4 *)

procedure passf5_sse(const ido, l1: Int32; const cc,chp,wa1,wa2,wa3,wa4: PSingle; const isign: Int32);
begin
  asm
    pcmpeqd xmm11, xmm11
    pslld   xmm11, 31
    psrlq   xmm11, 32             // EVEN
    mov     eax, $3E9E377A
    movd    xmm7, eax
    shufps  xmm7, xmm7, 0         // tr11
    mov     eax, $BF4F1BBD
    movd    xmm8, eax
    shufps  xmm8, xmm8, 0         // tr12
    mov     eax, isign
    cmp     eax, 1
    jne     @@neg
    mov     eax, $3F737871
    movd    xmm9, eax
    shufps  xmm9, xmm9, 0         // sti11 = +ti11
    mov     eax, $3F167918
    movd    xmm10, eax
    shufps  xmm10, xmm10, 0       // sti12 = +ti12
    movaps  xmm12, xmm11          // TW = EVEN
    jmp     @@cdone
  @@neg:
    mov     eax, $BF737871
    movd    xmm9, eax
    shufps  xmm9, xmm9, 0         // sti11 = -ti11
    mov     eax, $BF167918
    movd    xmm10, eax
    shufps  xmm10, xmm10, 0       // sti12 = -ti12
    pcmpeqd xmm12, xmm12
    pslld   xmm12, 31
    psllq   xmm12, 32             // TW = ODD
  @@cdone:
    mov     r8d, ido
    shl     r8, 2                 // idob
    lea     r9, [r8+r8*2]         // ido3b = 3*idob (Z3 / wa4)
    mov     eax, l1
    imul    rax, r8               // idlb = l1*ido*4
    mov     r10, rax              // idlb
    lea     r11, [r10+r10*2]      // idl3b = 3*idlb
    mov     r13d, ido
    shr     r13d, 1
    mov     r14d, r13d
    and     r14d, 1               // tail
    shr     r13d, 1               // npairs
    mov     rax, cc               // Z0 base = cc + 5*(k-1)*ido ; k=0 -> cc
    mov     rdx, chp
    mov     rsi, wa1
    mov     ebx, l1
    test    ebx, ebx
    jz      @@done
  @@k:
    mov     rcx, rsi
    mov     r12d, r13d
    test    r12d, r12d
    jz      @@tail
  @@pair:
    movups  xmm0, [rax+r8]        // Z1
    movups  xmm1, [rax+r8*4]      // Z4
    movaps  xmm2, xmm0
    addps   xmm0, xmm1            // T2
    subps   xmm2, xmm1            // T5
    movups  xmm1, [rax+r8*2]      // Z2
    movups  xmm3, [rax+r9]        // Z3
    movaps  xmm4, xmm1
    addps   xmm1, xmm3            // T3
    subps   xmm4, xmm3            // T4
    movups  xmm3, [rax]           // Z0
    movaps  xmm5, xmm3
    addps   xmm5, xmm0
    addps   xmm5, xmm1            // out0 = Z0+T2+T3
    movups  [rdx], xmm5
    movaps  xmm5, xmm0
    mulps   xmm5, xmm7
    addps   xmm5, xmm3
    movaps  xmm6, xmm1
    mulps   xmm6, xmm8
    addps   xmm5, xmm6            // C2
    movaps  xmm6, xmm0
    mulps   xmm6, xmm8
    addps   xmm6, xmm3
    movaps  xmm0, xmm1
    mulps   xmm0, xmm7
    addps   xmm6, xmm0            // C3
    movaps  xmm0, xmm2
    mulps   xmm0, xmm9
    movaps  xmm1, xmm4
    mulps   xmm1, xmm10
    addps   xmm0, xmm1            // C5
    movaps  xmm1, xmm2
    mulps   xmm1, xmm10
    movaps  xmm3, xmm4
    mulps   xmm3, xmm9
    subps   xmm1, xmm3            // C4
    movaps  xmm2, xmm0
    shufps  xmm2, xmm2, $b1
    xorps   xmm2, xmm11           // iC5
    movaps  xmm3, xmm5
    addps   xmm3, xmm2            // D2
    subps   xmm5, xmm2            // D5
    movaps  xmm0, xmm1
    shufps  xmm0, xmm0, $b1
    xorps   xmm0, xmm11           // iC4
    movaps  xmm4, xmm6
    addps   xmm4, xmm0            // D3
    subps   xmm6, xmm0            // D4
    movups  xmm0, [rcx]
    movaps  xmm1, xmm3
    shufps  xmm1, xmm1, $b1
    movaps  xmm2, xmm0
    shufps  xmm2, xmm2, $f5
    mulps   xmm1, xmm2
    xorps   xmm1, xmm12
    shufps  xmm0, xmm0, $a0
    mulps   xmm3, xmm0
    addps   xmm3, xmm1
    movups  [rdx+r10], xmm3       // out1
    movups  xmm0, [rcx+r8]
    movaps  xmm1, xmm4
    shufps  xmm1, xmm1, $b1
    movaps  xmm2, xmm0
    shufps  xmm2, xmm2, $f5
    mulps   xmm1, xmm2
    xorps   xmm1, xmm12
    shufps  xmm0, xmm0, $a0
    mulps   xmm4, xmm0
    addps   xmm4, xmm1
    movups  [rdx+r10*2], xmm4     // out2
    movups  xmm0, [rcx+r8*2]
    movaps  xmm1, xmm6
    shufps  xmm1, xmm1, $b1
    movaps  xmm2, xmm0
    shufps  xmm2, xmm2, $f5
    mulps   xmm1, xmm2
    xorps   xmm1, xmm12
    shufps  xmm0, xmm0, $a0
    mulps   xmm6, xmm0
    addps   xmm6, xmm1
    movups  [rdx+r11], xmm6       // out3
    movups  xmm0, [rcx+r9]
    movaps  xmm1, xmm5
    shufps  xmm1, xmm1, $b1
    movaps  xmm2, xmm0
    shufps  xmm2, xmm2, $f5
    mulps   xmm1, xmm2
    xorps   xmm1, xmm12
    shufps  xmm0, xmm0, $a0
    mulps   xmm5, xmm0
    addps   xmm5, xmm1
    movups  [rdx+r10*4], xmm5     // out4
    add     rax, 16
    add     rdx, 16
    add     rcx, 16
    dec     r12d
    jnz     @@pair
  @@tail:
    test    r14d, r14d
    jz      @@knext
    movq    xmm0, [rax+r8]
    movq    xmm1, [rax+r8*4]
    movaps  xmm2, xmm0
    addps   xmm0, xmm1
    subps   xmm2, xmm1
    movq    xmm1, [rax+r8*2]
    movq    xmm3, [rax+r9]
    movaps  xmm4, xmm1
    addps   xmm1, xmm3
    subps   xmm4, xmm3
    movq    xmm3, [rax]
    movaps  xmm5, xmm3
    addps   xmm5, xmm0
    addps   xmm5, xmm1
    movq    [rdx], xmm5
    movaps  xmm5, xmm0
    mulps   xmm5, xmm7
    addps   xmm5, xmm3
    movaps  xmm6, xmm1
    mulps   xmm6, xmm8
    addps   xmm5, xmm6            // C2
    movaps  xmm6, xmm0
    mulps   xmm6, xmm8
    addps   xmm6, xmm3
    movaps  xmm0, xmm1
    mulps   xmm0, xmm7
    addps   xmm6, xmm0            // C3
    movaps  xmm0, xmm2
    mulps   xmm0, xmm9
    movaps  xmm1, xmm4
    mulps   xmm1, xmm10
    addps   xmm0, xmm1            // C5
    movaps  xmm1, xmm2
    mulps   xmm1, xmm10
    movaps  xmm3, xmm4
    mulps   xmm3, xmm9
    subps   xmm1, xmm3            // C4
    movaps  xmm2, xmm0
    shufps  xmm2, xmm2, $b1
    xorps   xmm2, xmm11
    movaps  xmm3, xmm5
    addps   xmm3, xmm2            // D2
    subps   xmm5, xmm2            // D5
    movaps  xmm0, xmm1
    shufps  xmm0, xmm0, $b1
    xorps   xmm0, xmm11
    movaps  xmm4, xmm6
    addps   xmm4, xmm0            // D3
    subps   xmm6, xmm0            // D4
    movq    xmm0, [rcx]
    movaps  xmm1, xmm3
    shufps  xmm1, xmm1, $b1
    movaps  xmm2, xmm0
    shufps  xmm2, xmm2, $f5
    mulps   xmm1, xmm2
    xorps   xmm1, xmm12
    shufps  xmm0, xmm0, $a0
    mulps   xmm3, xmm0
    addps   xmm3, xmm1
    movq    [rdx+r10], xmm3
    movq    xmm0, [rcx+r8]
    movaps  xmm1, xmm4
    shufps  xmm1, xmm1, $b1
    movaps  xmm2, xmm0
    shufps  xmm2, xmm2, $f5
    mulps   xmm1, xmm2
    xorps   xmm1, xmm12
    shufps  xmm0, xmm0, $a0
    mulps   xmm4, xmm0
    addps   xmm4, xmm1
    movq    [rdx+r10*2], xmm4
    movq    xmm0, [rcx+r8*2]
    movaps  xmm1, xmm6
    shufps  xmm1, xmm1, $b1
    movaps  xmm2, xmm0
    shufps  xmm2, xmm2, $f5
    mulps   xmm1, xmm2
    xorps   xmm1, xmm12
    shufps  xmm0, xmm0, $a0
    mulps   xmm6, xmm0
    addps   xmm6, xmm1
    movq    [rdx+r11], xmm6
    movq    xmm0, [rcx+r9]
    movaps  xmm1, xmm5
    shufps  xmm1, xmm1, $b1
    movaps  xmm2, xmm0
    shufps  xmm2, xmm2, $f5
    mulps   xmm1, xmm2
    xorps   xmm1, xmm12
    shufps  xmm0, xmm0, $a0
    mulps   xmm5, xmm0
    addps   xmm5, xmm1
    movq    [rdx+r10*4], xmm5
    add     rax, 8
    add     rdx, 8
    add     rcx, 8
  @@knext:
    lea     rax, [rax+r8*4]       // Z0 base gap = 4*ido (inner advanced ido, k-stride 5*ido)
    dec     ebx
    jnz     @@k
  @@done:
  end ['rax','rbx','rcx','rdx','rsi','r8','r9','r10','r11','r12','r13','r14',
       'xmm0','xmm1','xmm2','xmm3','xmm4','xmm5','xmm6','xmm7','xmm8','xmm9',
       'xmm10','xmm11','xmm12'];
end; (* passf5 *)
