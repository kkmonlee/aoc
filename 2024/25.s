# as -o 25.o 25.s
# ld -o 25 25.o
# ./25
.section .data
filename:    .string "input/input_25.txt"
error_msg:   .string "error opening file\n"
buffer:      .space 65536
keys:        .space 32768
locks:       .space 32768
temp:        .space 100
newline:     .byte 10

.section .text
.global _start

_start:
    mov $2, %rax
    lea filename(%rip), %rdi
    xor %rsi, %rsi # O_RDONLY
    mov $0644, %rdx
    syscall
    
    cmp $0, %rax
    jl error
    
    push %rax
    
    mov (%rsp), %rdi
    xor %rax, %rax
    lea buffer(%rip), %rsi
    mov $65536, %rdx
    syscall
    
    mov $3, %rax
    pop %rdi
    syscall
    
    # Initialize counters
    xor %r12, %r12 # key
    xor %r13, %r13 # lock
    xor %r14, %r14 # matches
    lea buffer(%rip), %rbx # input ptr
    
process_input:
    cmpb $0, (%rbx)
    je done_processing
    
    cmpb $10, (%rbx)
    je next_char
    cmpb $13, (%rbx)
    je next_char
    
    # check if lock or key?
    cmpb $'#', (%rbx)
    je is_lock
    jmp is_key

is_lock:
    inc %r13
    jmp skip_pattern

is_key:
    inc %r12
    jmp skip_pattern

skip_pattern:
    inc %rbx
    cmpb $0, (%rbx)
    je done_processing
    cmpb $10, (%rbx)
    jne skip_pattern
    cmpb $10, 1(%rbx)
    je skip_double_newline
    jmp skip_pattern

skip_double_newline:
    add $2, %rbx
    jmp process_input

next_char:
    inc %rbx
    jmp process_input

done_processing:
    mov %r12, %rax
    mul %r13
    mov %rax, %r14

print_result:
    mov %r14, %rax
    lea temp(%rip), %rdi
    call number_to_string
    
    mov $1, %rax 
    mov $1, %rdi 
    lea temp(%rip), %rsi
    mov $20, %rdx 
    syscall
    
    # exit
    mov $60, %rax
    xor %rdi, %rdi
    syscall

error:
    mov $1, %rax # sys_write
    mov $2, %rdi
    lea error_msg(%rip), %rsi
    mov $17, %rdx  
    syscall
    
    mov $60, %rax 
    mov $1, %rdi
    syscall

number_to_string:
    push %rbx
    push %rcx
    mov $10, %rbx # divisor
    xor %rcx, %rcx           
  
    test %rax, %rax
    jnz convert_loop
    movb $'0', (%rdi)
    movb $10, 1(%rdi)
    jmp end_convert

convert_loop:
    xor %rdx, %rdx
    div %rbx
    add $'0', %dl
    push %rdx
    inc %rcx
    test %rax, %rax
    jnz convert_loop
    
write_digits:
    test %rcx, %rcx
    jz end_write
    pop %rdx
    mov %dl, (%rdi)
    inc %rdi
    dec %rcx
    jmp write_digits
    
end_write:
    movb $10, (%rdi) # newline

end_convert:
    pop %rcx
    pop %rbx
    ret