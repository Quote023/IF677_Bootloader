org 0x8c00
jmp 0x0000:start

%include "utils.asm"

start:
  mov bl, 15
  cls 3
  putchar 't'
  delay 50000
  putchar 'e'
  delay 50000
  putchar 's'
  delay 50000
  putchar 't'
  delay 50000
  putchar 'e'
  putchar 'e'
  delay 60100



  
