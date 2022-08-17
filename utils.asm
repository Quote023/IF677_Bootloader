; %ifndef M_UTILS
; %define M_UTILS
;;Utilities

%define DELAY_L(a) (a	& 0xff00)
%define DELAY_H(b) (b >> 8 & 0xff00)

%macro putchar 1
  push %1 ; passar parametro pra stack
  call _putchar
  add sp, 2 ; volta stack pra posição inicial
%endmacro

%macro prints 1
  push si ; salva o estado de si
  push ax ; salva o estado de ax 
  push %1 ; passar parametro pra stack
  call _prints
  add sp, 2 ; volta stack pra posição inicial
  pop ax  ; retorna estado de ax
  pop si  ; retorna o estado de si
%endmacro

%macro getchar 0
  call _getchar
%endmacro

%macro gets 0
  push cx ; salva o estado de cx
  push ax ; salva o estado de ax 
  call _gets
  pop ax  ; retorna estado de ax
  pop cx  ; retorna o estado de cx
%endmacro

%macro delchar 0; Substitui o ulltimo caracter na tela por um espaço em branco (apaga)
  putchar 0x08    ;Backspace                
  putchar ' '     ;Espaço em branco
  putchar 0x08    ;Backspace               
%endmacro
%macro endl 0 ; pula uma linha na tela             
  putchar 0x0d ;\r                  
  putchar 0x0a ;\n
%endmacro

%macro putpx 3
  push ax
  push dx
  push cx
  mov al, %1 ; cor
  mov dx, %2 ; x
  mov cx, %3 ; y
  call _putpx
  pop cx
  pop dx
  pop ax
%endmacro

%macro putpx 1
  push ax
  mov al, %1 ; cor
  call _putpx
  pop ax
%endmacro

%macro putpx 0
  call _putpx
%endmacro

%macro cls 1
  push ax
  push dx
  push cx
  mov al, %1 ; cor
  call _cls
  pop cx
  pop dx
  pop ax
%endmacro

%macro cls 0
  cls 0
%endmacro

%macro mode13h 0
  push ax
  call _initVideo
  pop ax
%endmacro

%macro setCursorPos 3
  push bx
  push dx
  push cx
  mov dx, %1 ; x
  mov cx, %2 ; y
  mov bx, %3 ; y
  call _setCursorPos
  pop cx
  pop dx
  pop bx
%endmacro

%macro setCursorPos 2
  push ax
  xor ax, ax  ;zera ax
  setCursorPos %1 %2 ax
  pop ax
%endmacro


%macro delay 1
  push si ; salva o estado de si
  push ax ; salva o estado de ax 
  push dx ; salva o estado de dx 
  push cx ; salva o estado de cx 
  push DELAY_H(%1) ; passar parametro pra stack
  push DELAY_L(%1) ; passar parametro pra stack
  call _delay
  add sp, 2 ; volta stack pra posição inicial
  pop cx  ; retorna estado de cx
  pop dx  ; retorna estado de dx
  pop ax  ; retorna estado de ax
  pop si  ; retorna o estado de si
%endmacro

%macro delay 0
  delay 5000
%endmacro

_initVideo:
  mov al, 13h
  mov ah, 0
  int 10h
  ret

_setCursorPos:
  mov ah, 02h
  int 10h
  ret

_delay:
  mov si, sp
	mov ah, 86h
  mov cx, [si+4] ;10.000 microssegundos High Byte
	mov dx, [si+2] ;10.000 microssegundos Low Byte
	int 15h 
ret


_putchar: ; imprime caracter que estiver na stack
  mov si, sp
  mov al, [si+2] ; pega valor passado como parametro pela stack
  mov ah, 0x0e ; AH == 0eh => imprime o caracter que tiver em al
  int 10h ; interrupção de vídeo.
  ret

_getchar: ; guarda o caracter digitado em AX (AL = valor da tabela ascii/AX = scancode)
  mov ah, 0x00 ; AH == 0 => Ler teclado
  int 16h ; interrupção de teclaod
  ret

_gets:  ; guarda a string digitada em DI                          
  .loop1:
    getchar
    cmp al, 0x08   ; ASCII 8 => backspace        
    je .backspace ; apaga último caracter lido
    cmp al, 0x0d  ; ASCII 15(0D) => \r (ENTER)          
    je .done
    cmp cl, 13 ; contador == 13
    je .loop1 ; if contador == 13: trava no loop até a pessoa apagar/confirmar;
    
    stosb ; guarda o char que está em AL no endereço de memória ES:DI
    inc cl ; cl++
    putchar ax 
    jmp .loop1

    .backspace:
      cmp cl, 0  ; Apagar no inicio da string = não fazer nada         
      je .loop1
      dec cl ;diminuior contador do loop
      dec di ;diminuir contador do DI (onde o stosb salva)
      mov byte[di], 0 ;apagar o último caracter salvo pelo stosb
      delchar ; apaga o caracter na tela visualmente
      jmp .loop1 ; volta pro inicio do loop
  .done:
    mov al, 0
    stosb ;indica que a string terminou com um 0
    endl
  ret

_prints: ;Imprime uma string
  mov si, sp
  mov si, [si+2]
  .loop:
    lodsb                   ;Carrega caracter em al              
    cmp al, 0
    je .endloop
    putchar ax
    jmp .loop
  .endloop:
  ret 

_putpx:
  mov ah, 0Ch ; imprime um pixel na tela na cor (al) na posiçao (dx,cx)
  int 10h ; interrupção de video
  ret

_cls:
  ;Percorre toda a janela printando pixels da cor preta
  mov dx, 0
  _cls.loopLine:
      cmp dx, 200
      je _cls.endLoopLine
      mov cx, 0
      _cls.loopColumn:
          cmp cx, 320
          je _cls.endLoopColumn
          putpx
          inc cx
          jmp _cls.loopColumn
      _cls.endLoopColumn:
          inc dx
          jmp _cls.loopLine
  _cls.endLoopLine:
  ret

;;End Macros
; %endif
