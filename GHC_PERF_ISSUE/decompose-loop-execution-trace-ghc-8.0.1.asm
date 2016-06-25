# GHC 8.0.1
=> 0x406cb4:  add    $0x30,%r12
=> 0x406cb8:  cmp    0x358(%r13),%r12
=> 0x406cbf:  ja     0x4072ef
=> 0x406cc5:  lea    0xb(%r10),%r14
=> 0x406cc9:  cmp    %r14,%r8
=> 0x406ccc:  jl     0x40739b
=> 0x406cd2:  cmp    %rsi,%r11
=> 0x406cd5:  jge    0x406b31
=> 0x406cdb:  movzwl 0x10(%rcx,%r11,2),%r14d
=> 0x406ce1:  cmp    $0xd800,%r14
=> 0x406ce8:  jae    0x40710a
=> 0x406cee:  add    $0xffffffffffffffd0,%r12
=> 0x406cf2:  inc    %r11

# rbx -> r10 -> r9 -> r8 -> rdi -> rsi -> rdx -> rcx -> rbx
=> 0x406cf5:  mov    %rcx,0x58(%rsp)
=> 0x406cfa:  mov    %rdx,%rcx
=> 0x406cfd:  mov    %rsi,%rdx
=> 0x406d00:  mov    %rdi,%rsi
=> 0x406d03:  mov    %r8,%rdi
=> 0x406d06:  mov    %r9,%r8
=> 0x406d09:  mov    %r10,%r9
=> 0x406d0c:  mov    %rbx,%r10
=> 0x406d0f:  mov    0x58(%rsp),%rbx

# jump to core logic of the loop
=> 0x406d14:  jmpq   0x406bd8

=> 0x406bd8:  add    $0x20,%r12
=> 0x406bdc:  cmp    0x358(%r13),%r12
=> 0x406be3:  ja     0x4072af
=> 0x406be9:  mov    %rax,0x80(%rsp)
=> 0x406bf1:  mov    %r14,%rax
=> 0x406bf4:  cmp    $0xac00,%r14
=> 0x406bfb:  jl     0x407277
=> 0x407277:  add    $0xffffffffffffffe0,%r12
=> 0x40727b:  cmp    $0xc0,%rax
=> 0x407281:  jl     0x40706b
=> 0x40706b:  cmp    $0x1,%r10
=> 0x40706f:  jl     0x407035
=> 0x407071:  cmp    $0x2,%r10
=> 0x407075:  jge    0x407261
=> 0x40707b:  cmp    $0x300,%rax
=> 0x407081:  jl     0x4070e0
=> 0x4070e0:  xor    %eax,%eax
=> 0x4070e2:  inc    %r9

# shuffle back
=> 0x4070e5:  mov    %r9,%r10
=> 0x4070e8:  mov    %r8,%r9
=> 0x4070eb:  mov    %rdi,%r8
=> 0x4070ee:  mov    %rsi,%rdi
=> 0x4070f1:  mov    %rdx,%rsi
=> 0x4070f4:  mov    %rcx,%rdx
=> 0x4070f7:  mov    %rbx,%rcx
=> 0x4070fa:  mov    %rax,%rbx
=> 0x4070fd:  mov    0x80(%rsp),%rax

=> 0x407105:  jmpq   0x406cb4
