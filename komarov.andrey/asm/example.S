.data
x: .word 0

.text

xx: .word x

.global f
.global fact

f:
ldr r1, xx
ldr r0, [r1]
add r0, #1
str r0, [r1]
mov pc, lr

# fact(r1, r2)
fact:
push {fp, lr}   @
mov fp, sp      @ ENTER
sub sp, sp, #4 @

mov r2, #0
teq r0, r2
beq ret1

str r0, [fp, #-4]
sub r0, r0, #1
bl fact
ldr r1, [fp, #-4]
mul r0, r1, r0
b ret

ret1:
mov r0, #1
b ret

ret:
mov sp, fp    @
pop {fp, lr}  @ EXIT
mov pc, lr    @


