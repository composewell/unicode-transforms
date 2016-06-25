# Heap check
=> 0x408fc0:  lea    0x30(%r12),%rax
=> 0x408fc5:  cmp    0x358(%r13),%rax
=> 0x408fcc:  jbe    0x408fe9                   # taken

=> 0x408fe9:  mov    0x10(%rbp),%rdx            # load
=> 0x408fed:  mov    0x18(%rbp),%rcx            # load
=> 0x408ff1:  lea    0xb(%rdx),%rsi
=> 0x408ff5:  cmp    %rsi,0x40(%rbp)
=> 0x408ff9:  jge    0x40905a                   # taken

=> 0x40905a:  cmp    0x50(%rbp),%rcx            # load
=> 0x40905e:  jge    0x40909e                   # not taken

=> 0x409060:  mov    0x38(%rbp),%rdx            # load
=> 0x409064:  movzwl 0x10(%rdx,%rcx,2),%eax
=> 0x409069:  cmp    $0xd7ff,%rax
=> 0x40906f:  jbe    0x4090e1                   # taken

=> 0x4090e1:  mov    %rax,0x18(%rbp)            # save
=> 0x4090e5:  inc    %rcx
=> 0x4090e8:  mov    %rcx,0x0(%rbp)             # save
=> 0x4090ec:  add    $0xfffffffffffffff8,%rbp
=> 0x4090f0:  jmpq   0x4093e8                   # taken

# Heap check
=> 0x4093e8:  lea    0x20(%r12),%rax
=> 0x4093ed:  cmp    0x358(%r13),%rax
=> 0x4093f4:  jbe    0x409411                   # taken

=> 0x409411:  mov    0x18(%rbp),%r14            # load
=> 0x409415:  mov    0x10(%rbp),%rsi            # load
=> 0x409419:  mov    0x20(%rbp),%rdx            # load
=> 0x40941d:  lea    -0xac00(%rdx),%rcx
=> 0x409424:  cmp    $0x2ba3,%rcx
=> 0x40942b:  jbe    0x40944e                   # not taken

=> 0x40942d:  mov    0x8(%rbp),%rcx             # load
=> 0x409431:  lea    0x8(%rbp),%rax             # load
=> 0x409435:  cmp    $0xc0,%rdx
=> 0x40943c:  jge    0x409484
=> 0x40943e:  test   %rsi,%rsi
=> 0x409441:  jg     0x409492                   # taken

=> 0x409492:  cmp    $0x1,%rsi
=> 0x409496:  jne    0x4094cd                   # not taken
=> 0x409498:  cmp    $0x300,%rdx
=> 0x40949f:  jge    0x4094b9                   # not taken
=> 0x4094a1:  mov    %rcx,0x20(%rbp)            # save
=> 0x4094a5:  jmp    0x4094c6                   # taken
=> 0x4094c6:  inc    %r14
=> 0x4094c9:  jmp    0x4094fa                   # taken

=> 0x4094fa:  mov    %r14,0x18(%rbp)
=> 0x4094fe:  movq   $0x0,0x10(%rbp)            # save
=> 0x409506:  mov    %rax,%rbp
=> 0x409509:  jmpq   0x408fc0
