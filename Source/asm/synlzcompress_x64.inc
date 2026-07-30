{
  This assembler is Arnaud Bouchez's from SynLZ:
    https://synopse.info
    https://github.com/synopse/mORMot2 (src/core/mormot.core.base.asmx64.inc)
}
function SynLZCompressAsm(Src: PByte; Size: Int32; Dst: PByte): Int32; assembler;
var
  off: TOffsets;                    // 32KB hash table
  cache: array[0..4095] of UInt32;  // 16KB = total 48KB on stack
asm // rcx=src, edx=size, r8=dest
        {$IFDEF WIN64} // additional registers to preserve
        push    rdi
        push    rsi
        {$ELSE} // Linux 64-bit ABI
        mov     r8, rdx
        mov     rdx, rsi
        mov     rcx, rdi
        {$ENDIF WIN64}
        push    rbx
        push    r12
        push    r13
        push    r8                 // save r8=dest to compute result
        mov     rbx, rcx           // rbx=src
        cmp     edx, 32768         // edx=size
        jc      @03
        mov     eax, edx
        mov     ecx, edx
        and     eax, 7FFFH
        shr     ecx, 15
        or      eax, 8000H
        shl     ecx, 16
        or      eax, ecx
        mov     dword ptr [r8], eax
        add     r8, 4
        jmp     @05
@03:    mov     dword ptr [r8], edx
        add     r8, 2
        test    edx, edx
        jz      @18
@05:    xor     eax, eax
        lea     rdi, [off]
        mov     rcx, 4096
        cld
        rep stosq                  // reset 32KB offsets lookup table using ERMS
        lea     r9,  [rdx + rbx]   // r9=src_end
        lea     r10, [r9 - 11]     // r10=src_endmatch
        mov     r11, r8            // r11=CWpoint
        mov     ecx, 1             // ecx=CWBits
        mov     dword ptr [r8], eax
        add     r8, 4
        cmp     rbx, r10
        ja      @15
        align 16
@07:    mov     edx, dword ptr [rbx]
        mov     eax, edx
        shr     eax, 12
        xor     eax, edx
        and     eax, 4095              // rax=h
        mov     r12, rdx
        //lea [off + rax * 8] pre-compute is not faster: 2 AGUs since Nehalem
        mov     r13, qword ptr [off + rax * 8]    // r13=o
        mov     edx, dword ptr [cache + rax * 4]
        mov     qword ptr [off + rax * 8], rbx
        mov     dword ptr [cache + rax * 4], r12d
        xor     rdx, r12
        test    r13, r13
        lea     rdi, [r9 - 1]
        je      @12
        and     rdx, $00ffffff
        jne     @12
        mov     rdx, rbx
        sub     rdx, r13
        cmp     rdx, 2
        jbe     @12
        or      dword ptr [r11], ecx
        add     rbx, 2
        add     r13, 2
        mov     esi, 1
        sub     rdi, rbx
        cmp     rdi, 271
        jc      @09
        mov     edi, 271
        jmp     @09
@08:    add     rsi, 1
@09:    mov     edx, dword ptr [r13 + rsi]
        cmp     dl, byte ptr [rbx + rsi]
        jnz     @10
        cmp     rsi, rdi
        jge     @10
        add     rsi, 1
        cmp     dh, byte ptr [rbx + rsi]
        jnz     @10
        shr     edx, 16
        cmp     rsi, rdi
        jge     @10
        add     rsi, 1
        cmp     dl, byte ptr [rbx + rsi]
        jnz     @10
        cmp     rsi, rdi
        jge     @10
        add     rsi, 1
        cmp     dh, byte ptr [rbx + rsi]
        jnz     @10
        cmp     rsi, rdi
        jc      @08
@10:    add     rbx, rsi
        shl     rax, 4
        cmp     rsi, 15
        ja      @11
        or      rax, rsi
        mov     word ptr [r8], ax
        add     r8, 2
        jmp     @13
@11:    sub     rsi, 16
        mov     word ptr [r8], ax
        mov     byte ptr [r8 + 2], sil
        add     r8, 3
        jmp     @13
@12:    mov     al, byte ptr [rbx]
        mov     byte ptr [r8], al
        add     rbx, 1
        add     r8, 1
@13:    add     ecx, ecx
        jnz     @14
        mov     r11, r8
        mov     [r8], ecx
        add     r8, 4
        add     ecx, 1
@14:    cmp     rbx, r10
        jbe     @07
@15:    cmp     rbx, r9
        jnc     @18
@16:    mov     al, byte ptr [rbx]
        mov     byte ptr [r8], al
        add     rbx, 1
        add     r8, 1
        add     ecx, ecx
        jnz     @17
        mov     dword ptr [r8], ecx
        add     r8, 4
        add     ecx, 1
@17:    cmp     rbx, r9
        jc      @16
@18:    pop     rax
        sub     r8, rax // result := dst - dstbeg
        mov     rax, r8
        pop     r13
        pop     r12
        pop     rbx
        {$IFDEF WIN64} // additional registers to preserve
        pop     rsi
        pop     rdi
        {$ENDIF WIN64}
end;
