section .data
         HEAP:    times 1024 dq 0
         section .text
         global start_here
         extern print_snake_val
         extern snake_error
         extern ischar
         extern isstring
         extern isarray
main:
        mov rax, 581
        mov QWORD [rsp + -8], rax
        mov rax, 813
        mov QWORD [rsp + -16], rax
        mov rax, 869
        mov QWORD [rsp + -24], rax
        mov rax, 869
        mov QWORD [rsp + -32], rax
        mov rax, 893
        mov QWORD [rsp + -40], rax
        mov rax, 261
        mov QWORD [rsp + -48], rax
        mov rax, 957
        mov QWORD [rsp + -56], rax
        mov rax, 893
        mov QWORD [rsp + -64], rax
        mov rax, 917
        mov QWORD [rsp + -72], rax
        mov rax, 869
        mov QWORD [rsp + -80], rax
        mov rax, 805
        mov QWORD [rsp + -88], rax
        mov QWORD [r15 + 0], 11
        mov r9, QWORD [rsp + -8]
        mov QWORD [r15 + 8], r9
        mov r9, QWORD [rsp + -16]
        mov QWORD [r15 + 16], r9
        mov r9, QWORD [rsp + -24]
        mov QWORD [r15 + 24], r9
        mov r9, QWORD [rsp + -32]
        mov QWORD [r15 + 32], r9
        mov r9, QWORD [rsp + -40]
        mov QWORD [r15 + 40], r9
        mov r9, QWORD [rsp + -48]
        mov QWORD [r15 + 48], r9
        mov r9, QWORD [rsp + -56]
        mov QWORD [r15 + 56], r9
        mov r9, QWORD [rsp + -64]
        mov QWORD [r15 + 64], r9
        mov r9, QWORD [rsp + -72]
        mov QWORD [r15 + 72], r9
        mov r9, QWORD [rsp + -80]
        mov QWORD [r15 + 80], r9
        mov r9, QWORD [rsp + -88]
        mov QWORD [r15 + 88], r9
        mov rax, r15
        add r15, 96
        add rax, 3
        mov QWORD [rsp + -8], rax
        mov rdi, QWORD [rsp + -8]
        sub rsp, 16
        call print_snake_val
        add rsp, 16
        ret
start_here:
         push r15
         sub rsp, 8
         lea r15, [rel HEAP]
         call main
         add rsp, 8
         pop r15
         ret

overflow_err:
        mov rdi, 0
        call snake_error
arith_err:
        mov rdi, 1
        call snake_error
cmp_err:
        mov rdi, 2
        call snake_error
log_err:
        mov rdi, 3
        call snake_error
if_err:
        mov rdi, 4
        call snake_error
range_dec_err:
        mov rdi, 5
        call snake_error
index_oob_err:
        mov rdi, 6
        call snake_error
index_non_both:
        mov rdi, 7
        call snake_error
set_non_char:
        mov rdi, 8
        call snake_error
index_nan:
        mov rdi, 9
        call snake_error
length_not_array_error:
        mov rdi, 10
        call snake_error
 