begin
  asm
    push   rsi
    push   rdi
    push   rbx
    push   r12
    push   r13
    push   r14
    push   r15
    pxor   xmm3, xmm3                    // zero (byte->word unpack)
    mov    eax, $2000                    // 1<<13 bias
    movd   xmm4, eax
    pshufd xmm4, xmm4, 0
    mov    eax, $FF000000                // alpha-force mask
    movd   xmm5, eax

    mov    r8d, RowCount
    test   r8d, r8d
    jle    @@hdone
    mov    r12, Src
    mov    r13, Dst
    mov    r14d, SrcStride
    mov    r15d, OutDim
  @@hrow:
    mov    r10, Starts                   // reset per row (per-column tables)
    mov    rbx, Counts
    mov    r11, KK
    xor    r9d, r9d                       // xx = 0
  @@hxx:
    cmp    r9d, r15d
    jge    @@hrowend
    mov    eax, dword ptr [r10]          // xmin
    mov    ecx, dword ptr [rbx]          // count
    lea    rsi, [r12 + rax*4]            // P = srcrow + xmin
    mov    rdi, r11                       // K
    pxor   xmm0, xmm0
  @@hpair:
    cmp    ecx, 2
    jl     @@htail
    movd   xmm1, dword ptr [rsi]
    movd   xmm2, dword ptr [rsi+4]
    punpcklbw xmm1, xmm2
    punpcklbw xmm1, xmm3
    movd   xmm2, dword ptr [rdi]
    pshufd xmm2, xmm2, 0
    pmaddwd xmm1, xmm2
    paddd  xmm0, xmm1
    add    rsi, 8
    add    rdi, 4
    sub    ecx, 2
    jmp    @@hpair
  @@htail:
    test   ecx, ecx
    jz     @@hstore
    movd   xmm1, dword ptr [rsi]
    punpcklbw xmm1, xmm1
    punpcklbw xmm1, xmm3
    movd   xmm2, dword ptr [rdi]
    pshufd xmm2, xmm2, 0
    pmaddwd xmm1, xmm2
    paddd  xmm0, xmm1
  @@hstore:
    paddd  xmm0, xmm4
    psrad  xmm0, 14
    packssdw xmm0, xmm0
    packuswb xmm0, xmm0
    por    xmm0, xmm5
    movd   dword ptr [r13 + r9*4], xmm0
    add    r10, 4                         // Starts++
    add    rbx, 4                         // Counts++
    mov    edx, KSize
    lea    r11, [r11 + rdx*2]
    inc    r9d
    jmp    @@hxx
  @@hrowend:
    lea    r12, [r12 + r14*4]            // srcrow += SrcStride
    lea    r13, [r13 + r15*4]            // dstrow += OutDim
    dec    r8d
    jnz    @@hrow
  @@hdone:
    pop    r15
    pop    r14
    pop    r13
    pop    r12
    pop    rbx
    pop    rdi
    pop    rsi
  end;
end;
