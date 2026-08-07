{
  This assembler is Arnaud Bouchez's from SynLZ:
    https://synopse.info
    https://github.com/synopse/mORMot2 (src/core/mormot.core.base.asmx64.inc)
}
function SynLZDecompressAsm(Src: PByte; Size: Int32; Dst: PByte): Int32; assembler;
var
  off: TOffsets; // pointers are faster and use only 32KB on stack
asm     // rcx=src, edx=size, r8=dest
        {$IFDEF WIN64} // additional registers to preserve
        push    rsi
        push    rdi
        {$ELSE} // Linux 64-bit ABI
        mov     r8, rdx
        mov     rdx, rsi
        mov     rcx, rdi
        {$ENDIF WIN64}
        push    rbx
        push    r12
        push    r13
        movzx   eax, word ptr [rcx]    // rcx=src   eax=result
        lea     r9, [rcx + rdx]        // r9=src_end
        test    eax, eax
        je      @36
        add     rcx, 2
        mov     r10d, eax
        and     r10d, $8000
        jz      @21                   // dest size < 32768
        movzx   ebx, word ptr [rcx]
        shl     ebx, 15
        and     eax, $7fff
        or      eax, ebx
        add     rcx, 2
@21:    push    rax                    // save result for function exit
        lea     rax, [r8 - 1]          // rax=last_hashed  r8=dest
@22:    mov     edi, dword ptr [rcx]   // edi=CW
        add     rcx, 4
        mov     r10d, 1                // r10d=CWBit
        cmp     rcx, r9
        jnc     @35
        align 16
@23:    test    r10d, edi
        jnz     @25
        mov     bl, byte ptr [rcx]    // CWBit was not set: copy byte and hash
        mov     byte ptr [r8], bl
        add     rcx, 1
        lea     rbx, [r8 - 2]
        add     r8, 1
        cmp     rcx, r9
        jnc     @35
        cmp     rbx, rax
        jbe     @24
        add     rax, 1
        mov     esi, dword ptr [rax]
        mov     ebx, esi
        shr     esi, 12
        xor     ebx, esi
        and     ebx, 4095
        mov     qword ptr [off + rbx * 8], rax
@24:    shl     r10d, 1
        jnz     @23
        jmp     @22
@25:    movzx   rbx, word ptr [rcx] // CWBit was set: copy bytes from offset
        add     rcx, 2
        mov     r11, rbx
        shr     ebx, 4                 // ebx=h
        and     r11, 0FH               // r11=t
        lea     r11, [r11 + 2]
        jnz     @26
        movzx   r11, byte ptr [rcx]
        add     rcx, 1
        lea     r11, [r11 + (16 + 2)]
@26:    mov     r13, qword ptr [off + rbx * 8] // r13=o
        mov     rbx, r8
        xor     rsi, rsi
        sub     rbx, r13
        mov     r12, r11
        cmp     rbx, r11
        jc      @29       // PtrUInt(dst - o) < t overlap -> MoveByOne
        mov     rbx, qword ptr [r13]
        mov     qword ptr [r8], rbx
        sub     r12, 8
        jbe     @31       // 1..8 bytes
        mov     rbx, qword ptr [r13 + r12] // last 8 bytes (may overlap)
        mov     qword ptr [r8 + r12], rbx
        cmp     r12, 8
        jbe     @31      // 9..16 bytes
        shr     r12, 3
@27:    add     rsi, 8
        mov     rbx, qword ptr [r13 + rsi] // inlined move by 8 bytes
        mov     qword ptr [r8 + rsi], rbx
        sub     r12, 1
        jnz     @27
@31:    cmp     rcx, r9
        jz      @35
        cmp     rax, r8
        jnc     @34
        align 16
@32:    add     rax, 1               // hash decompressed bytes
        mov     ebx, dword ptr [rax]
        mov     esi, ebx
        shr     ebx, 12
        xor     esi, ebx
        and     esi, 4095
        mov     qword ptr [off + rsi * 8], rax
        cmp     rax, r8
        jc      @32
@34:    add     r8, r11
        lea     rax, [r8 - 1]
        shl     r10d, 1
        jnz     @23
        jmp     @22
        align 16
@29:    mov     bl, byte ptr [r13 + rsi] // overlaping move
        mov     byte ptr [r8 + rsi], bl
        add     rsi, 1
        sub     r12, 1
        jnz     @29
        jmp     @31
@35:    pop     rax     // returns stored dest length
@36:    pop     r13
        pop     r12
        pop     rbx
        {$IFDEF WIN64} // additional registers to preserve
        pop     rdi
        pop     rsi
        {$ENDIF WIN64}
end;
