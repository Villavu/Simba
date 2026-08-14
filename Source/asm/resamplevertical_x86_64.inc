begin
  asm
    push   rsi
    push   rdi
    push   r12
    push   r13
    push   r14
    push   r15
    pxor   xmm3, xmm3
    mov    eax, $2000
    movd   xmm4, eax
    pshufd xmm4, xmm4, 0
    mov    eax, $FF000000
    movd   xmm5, eax

    mov    r8d, RowCount
    test   r8d, r8d
    jle    @@vdone
    mov    r13, Dst
    mov    r15d, OutDim                   // row stride == width (src and dst)
    mov    r10, Starts
    mov    r14, Counts
    mov    r11, KK
  @@vrow:
    mov    eax, dword ptr [r10]          // ymin
    mov    ecx, dword ptr [r14]          // count
    imul   rax, r15                       // ymin * stride
    mov    r12, Src
    lea    r12, [r12 + rax*4]            // srcbase = Src + ymin*stride
    xor    r9d, r9d
  @@vxx:
    cmp    r9d, r15d
    jge    @@vrowend
    lea    rsi, [r12 + r9*4]             // P = srcbase + xx
    mov    rdi, r11
    mov    edx, ecx
    pxor   xmm0, xmm0
  @@vpair:
    cmp    edx, 2
    jl     @@vtail
    movd   xmm1, dword ptr [rsi]
    lea    rax, [rsi + r15*4]            // + one row
    movd   xmm2, dword ptr [rax]
    punpcklbw xmm1, xmm2
    punpcklbw xmm1, xmm3
    movd   xmm2, dword ptr [rdi]
    pshufd xmm2, xmm2, 0
    pmaddwd xmm1, xmm2
    paddd  xmm0, xmm1
    lea    rsi, [rsi + r15*8]            // += two rows
    add    rdi, 4
    sub    edx, 2
    jmp    @@vpair
  @@vtail:
    test   edx, edx
    jz     @@vstore
    movd   xmm1, dword ptr [rsi]
    punpcklbw xmm1, xmm1
    punpcklbw xmm1, xmm3
    movd   xmm2, dword ptr [rdi]
    pshufd xmm2, xmm2, 0
    pmaddwd xmm1, xmm2
    paddd  xmm0, xmm1
  @@vstore:
    paddd  xmm0, xmm4
    psrad  xmm0, 14
    packssdw xmm0, xmm0
    packuswb xmm0, xmm0
    por    xmm0, xmm5
    movd   dword ptr [r13 + r9*4], xmm0
    inc    r9d
    jmp    @@vxx
  @@vrowend:
    mov    edx, KSize
    lea    r11, [r11 + rdx*2]
    lea    r13, [r13 + r15*4]
    add    r10, 4
    add    r14, 4
    dec    r8d
    jnz    @@vrow
  @@vdone:
    pop    r15
    pop    r14
    pop    r13
    pop    r12
    pop    rdi
    pop    rsi
  end;
end;
