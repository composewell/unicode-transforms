# Compare this line by line with previous revision of the file which had just
# one more branch, removed in this version. The blank line in this file
# correspond to code that got that was present earlier but not now.
# Heap check + 48
=> 0x408897:  add    $0x30,%r12
=> 0x40889b:  cmp    0x358(%r13),%r12
=> 0x4088a2:  ja     0x408d7f

#
# Loop realloc check logic
=> 0x4088a8:  lea    0xb(%rsi),%r11
=> 0x4088ac:  cmp    %r11,%rdi
=> 0x4088af:  jl     0x408ce3

#
# Loop termination condition
=> 0x4088b5:  cmp    %rcx,%rbx
=> 0x4088b8:  jge    0x408e0f

#
# Loop real logic begin
=> 0x4088be:  movzwl 0x10(%rax,%rbx,2),%r11d
=> 0x4088c4:  cmp    $0xd800,%r11
=> 0x4088cb:  jae    0x408a61

#
# Heap - 48
=> 0x4088d1:  add    $0xffffffffffffffd0,%r12

=> 0x4088d5:  inc    %rbx


# _n4se:
# Register shuffle

=> 0x4088d8:  mov    %rbx,%r14
=> 0x4088db:  mov    %r11,%rbx
























# Logic condition - that was removed







# Loop real processing logic
# Uses rax = char, r10 = ri, r9 = di
=> 0x4088de:  cmp    $0xc0,%rbx
=> 0x4088e5:  jl     0x408ae9
=> 0x408ae9:  cmp    $0x1,%rdx
=> 0x408aed:  jl     0x408b3c
=> 0x408aef:  cmp    $0x1,%rdx
=> 0x408af3:  jne    0x408dbb
=> 0x408af9:  cmp    $0x300,%rbx
=> 0x408b00:  jl     0x408b2c
=> 0x408b2c:  xor    %ebx,%ebx
=> 0x408b2e:  inc    %rsi

#
# Register management
=> 0x408b31:  mov    %rbx,%rdx
=> 0x408b34:  mov    %r14,%rbx









# Jump back to start of loop
=> 0x408b37:  jmpq   0x408897
