.intel_syntax noprefix

# Cons a (Cons b (Cons c (Cons d xs)))
# &[r12 +   8, r12 +  16, r12 +  24, r12 +  32] = [I#, a, I# c]
# &[r12 +  40, r12 +  48, r12 +  56, r12 +  64] = [I#, b, I# d]
# &[r12 +  72, r12 +  80, r12 +  88, r12 +  96] = [Cons, r12 + 8 + 1, r12 + 96 + 2, Cons]
# &[r12 + 104, r12 + 112, r12 + 120, r12 + 128] = [r12 + 40 + 1, r12 + 120 + 2, Cons, r12 + 24 + 1]
# &[r12 + 136, r12 + 144, r12 + 152, r12 + 160] = [r12 + 144 + 2, Cons, r12 + 56, old ? _ : r12 - 88 + 2]

.data
.align 32
.postfix_sums_round.conss1:
  .quad offset ghczmprim_GHCziTypes_ZC_con_info
  .quad 8 + 1
  .quad 96 + 2
  .quad offset ghczmprim_GHCziTypes_ZC_con_info

.align 32
.postfix_sums_round.conss2:
  .quad 40 + 1
  .quad 120 + 2
  .quad offset ghczmprim_GHCziTypes_ZC_con_info
  .quad 24 + 1

.align 32
.postfix_sums_round.conss3:
  .quad 144 + 2
  .quad offset ghczmprim_GHCziTypes_ZC_con_info
  .quad 56 + 1
  .quad 0

.align 32
.postfix_sums_round.offs1:
  .quad 0
  .quad 160
  .quad 160
  .quad 0

.align 32
.postfix_sums_round.offs2:
  .quad 160
  .quad 160
  .quad 0
  .quad 160

.align 32
.postfix_sums_round.offs3:
  .quad 160
  .quad 0
  .quad 160
  .quad 160

.align 32
.postfix_sums_round.ints:
  .quad offset ghczmprim_GHCziTypes_Izh_con_info
  .quad offset ghczmprim_GHCziTypes_Izh_con_info
  .quad offset ghczmprim_GHCziTypes_Izh_con_info
  .quad offset ghczmprim_GHCziTypes_Izh_con_info

.text
.align 64
.globl postfix_sums_round
# rbx = begin
# r14 = offset
# rsi = acc
# rdi = list
# invariant: offset % 32 == 0
postfix_sums_round:

.postfix_sums_round.init:

  vmovq xmm0, r12
  vpbroadcastq ymm0, xmm0

  vmovdqu ymm3, [rip + .postfix_sums_round.conss1]
  vpaddq ymm1, ymm3, ymm0
  vpblendd ymm3, ymm3, ymm1, 0b00111100
  vmovdqu ymm4, [rip + .postfix_sums_round.conss2]
  vpaddq ymm1, ymm4, ymm0
  vpblendd ymm4, ymm4, ymm1, 0b11001111
  vmovdqu ymm5, [rip + .postfix_sums_round.conss3]
  vpaddq ymm1, ymm5, ymm0
  vpblendd ymm5, ymm5, ymm1, 0b11110011
  vmovq xmm0, rdi
  vpbroadcastq ymm0, xmm0
  vpblendd ymm5, ymm5, ymm0, 0b11000000

  vmovdqu ymm6, [rip + .postfix_sums_round.ints]

  vmovq xmm7, rsi
  vpbroadcastq ymm7, xmm7

  vmovdqu ymm8, [rip + .postfix_sums_round.offs1]
  vmovdqu ymm9, [rip + .postfix_sums_round.offs2]
  vmovdqu ymm10, [rip + .postfix_sums_round.offs3]

  mov r8d, 1
  jmp .postfix_sums_round.head

# ymm3 = conss1
# ymm4 = conss2
# ymm5 = conss3
# ymm6 = ints
# ymm7 = accs
# ymm8 = offs1
# ymm9 = offs2
# ymm10 = offs3
.postfix_sums_round.loop:

  # Heap check
  add r12, 160
  cmp r12, [r13 + 856]
  ja .postfix_sums_round.gc

  # Postfix sum
  vmovdqu ymm0, [rbx + r14]
  vpsrldq ymm1, ymm0, 8
  vpaddq ymm0, ymm0, ymm1
  vextracti128 xmm1, ymm0, 1
  vpunpcklqdq xmm1, xmm1, xmm1
  vpaddq ymm0, ymm0, ymm1
  vpaddq ymm0, ymm0, ymm7
  vpbroadcastq ymm7, xmm0

  # Ints
  vpunpckhqdq ymm1, ymm6, ymm0
  vmovdqu [r12 - 120], ymm1
  vpunpcklqdq ymm0, ymm6, ymm0
  vmovdqu [r12 - 152], ymm0

  # Lists
  vmovdqu [r12 - 88], ymm3
  vpaddq ymm3, ymm3, ymm8
  vmovdqu [r12 - 56], ymm4
  vpaddq ymm4, ymm4, ymm9
  vmovdqu [r12 - 24], ymm5
  vpaddq ymm5, ymm5, ymm10

  # Result, if needed. Could be avoided by loop peeling, but I'm not sure if it's worth duplicating entire body.
  test r8d, r8d
  jnz .postfix_sums_round.fixup

.postfix_sums_round.head:
  sub r14, 32
  jns .postfix_sums_round.loop

  # Result
  vextracti128 xmm0, ymm5, 1
  vpunpckhqdq xmm0, xmm0, xmm0
  vmovq rbx, xmm0

  jmp [rbp]

.postfix_sums_round.fixup:

  # Result
  lea rdi, [r12 - 88]
  vmovq xmm0, rdi
  vpbroadcastq ymm0, xmm0
  vpblendd ymm5, ymm5, ymm0, 0b11000000

  xor r8d, r8d
  jmp .postfix_sums_round.head


.postfix_sums_round.gc:
  add r14, 32
  sub rbp, 40
  vmovdqu [rbp + 8], ymm5
  vmovdqu [rbp + 16], xmm7
  mov [rbp + 8], rbx
  mov [rbp + 16], r14
  lea rbx, [rip + .postfix_sums_round.gc.post]
  mov [rbp + 0], rbx
  mov qword ptr [r13 + 904], 160
  jmp stg_gc_noregs

.align 8
.quad 0b111000100 # nnnp, 4
.long 30          # RET_SMALL
.long 0           # SRT
.postfix_sums_round.gc.post:
  mov rbx, [rbp + 8]
  mov r14, [rbp + 16]
  mov rsi, [rbp + 24]
  mov rdi, [rbp + 32]
  add rbp, 40
  jmp .postfix_sums_round.init
