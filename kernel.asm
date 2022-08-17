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
  delay 60100
  jogo:
    mov ax,0x9c00    
    mov es,ax
    xor bx,bx
    mov ah,0x02
    mov al,10  
    mov dl,0
    mov ch,0
    mov cl,7
    mov dh,0
    int 13h
    jc jogo
  jmp 0x9c00


  
