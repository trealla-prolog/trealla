set architecture arm
target remote :1234
set $sp = *(unsigned int *)0x0
set $pc = *(unsigned int *)0x4
set $xpsr = $xpsr | 0x01000000
continue
