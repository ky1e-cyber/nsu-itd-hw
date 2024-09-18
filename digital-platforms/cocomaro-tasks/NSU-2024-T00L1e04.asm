asect  0x00

ldi r1, 0b1
ldi r2, 0x2
ldi r3, 3

push r3
push r2
push r1
ldsa r0,0  # load the address of the current stack pointer into r0 for the robot
halt       # Brings execution to a halt

INPUTS>
ENDINPUTS>

end



