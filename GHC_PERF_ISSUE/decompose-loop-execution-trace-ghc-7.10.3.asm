# GHC 7.10.3 NO llvm
# Start of loop
# c4ic:
# Heap check + 48
=> 0x408d2a:	add    $0x30,%r12
=> 0x408d2e:	cmp    0x358(%r13),%r12
=> 0x408d35:	ja     0x4094af               # not taken

# c4mw:
# Loop realloc check logic
=> 0x408d3b:	lea    0xb(%r10),%r14
=> 0x408d3f:	cmp    %r14,%r8
=> 0x408d42:	jl     0x409413             # not taken

# c4pw:
# Loop termination condition
=> 0x408d48:	cmp    %rsi,%r11
=> 0x408d4b:	jge    0x40969f             # not taken

# c4ps:
# Loop real logic begin
=> 0x408d51:	movzwl 0x10(%rcx,%r11,2),%r14d
=> 0x408d57:	cmp    $0xd800,%r14
=> 0x408d5e:	jae    0x4091f1             # not taken

# c4p8:
# Heap - 48
=> 0x408d64:	add    $0xffffffffffffffd0,%r12

=> 0x408d68:	inc    %r11

# _n4se:
# Register shuffle
# r14 <-> r11
=> 0x408d6b:	mov    %r11,0x98(%rsp)
=> 0x408d73:	mov    %r14,%r11
=> 0x408d76:	mov    0x98(%rsp),%r14

# stack.rbx -> r10 -> r9 -> r8 -> rdi -> rsi -> rdx -> rcx -> rbx -> stack.rbx
=> 0x408d7e:	mov    %rbx,0x90(%rsp)
=> 0x408d86:	mov    %rcx,%rbx
=> 0x408d89:	mov    %rdx,%rcx
=> 0x408d8c:	mov    %rsi,%rdx
=> 0x408d8f:	mov    %rdi,%rsi
=> 0x408d92:	mov    %r8,%rdi
=> 0x408d95:	mov    %r9,%r8
=> 0x408d98:	mov    %r10,%r9
=> 0x408d9b:	mov    0x90(%rsp),%r10

# c4jd:
# Heap check + 32
=> 0x408da3:	add    $0x20,%r12
=> 0x408da7:	cmp    0x358(%r13),%r12
=> 0x408dae:	ja     0x4094eb         # not taken

# c4mz:
# r11 -> rax -> stack
=> 0x408db4:	mov    %rax,0x88(%rsp)
=> 0x408dbc:	mov    %r11,%rax

# Logic condition
=> 0x408dbf:	cmp    $0xac00,%r11
=> 0x408dc6:	jl     0x40952b        # taken

# c4oH:
# Heap -32
=> 0x40952b:	add    $0xffffffffffffffe0,%r12

# Loop real processing logic
# Uses rax = char, r10 = ri, r9 = di
=> 0x40952f:	cmp    $0xc0,%rax
=> 0x409535:	jl     0x409058      # taken
=> 0x409058:	cmp    $0x1,%r10
=> 0x40905c:	jl     0x409104      # not taken
=> 0x409062:	cmp    $0x1,%r10
=> 0x409066:	jne    0x409552      # not taken
=> 0x40906c:	cmp    $0x300,%rax
=> 0x409072:	jl     0x4090d7      # taken
=> 0x4090d7:	xor    %eax,%eax
=> 0x4090d9:	inc    %r9

# _n4s8:
# Register shuffle back
=> 0x4090dc:	mov    %r14,%r11
=> 0x4090df:	mov    %r9,%r10
=> 0x4090e2:	mov    %r8,%r9
=> 0x4090e5:	mov    %rdi,%r8
=> 0x4090e8:	mov    %rsi,%rdi
=> 0x4090eb:	mov    %rdx,%rsi
=> 0x4090ee:	mov    %rcx,%rdx
=> 0x4090f1:	mov    %rbx,%rcx
=> 0x4090f4:	mov    %rax,%rbx
=> 0x4090f7:	mov    0x88(%rsp),%rax

# Jump back to start of loop
=> 0x4090ff:	jmpq   0x408d2a
