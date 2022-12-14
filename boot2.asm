org 0x500
jmp 0x0000:start

msgInicial db 'Iniciando o sistema', 0


p0 db ' _.........._ ',13,10,0
p1 db '| |Cin     | |',13,10,0
p2 db '| |        | |',13,10,0
p3 db '| |________| |',13,10,0
p4 db '|   ______   |',13,10,0
p5 db '|  |    | |  |',13,10,0
p6 db '|__|____|_|__|',13,10,0

;desc:
; define a posição do cursor na tela
;input:
; DH = linha/eixo y
; DL = coluna/eixo x
set_cursor: 
	mov ah, 02h ;set cursor position 
	mov bh, 00h ;page number
	int 10h
	ret

;desc:
; pausa a aplicação por um tempo
;input:
; DX = quantidade de delay
delay:
	dec dx
	mov cx, 0
		.time:
			inc cx
			cmp cx, 10000
			jne .time 
	cmp dx, 0
	jne delay
ret

;desc:
; imprime texto na tela com delay entre letras
;input:
; SI = string a ser impressa
print_string_delay:  ;string tem que estar em SI
	lodsb   
	cmp al,0
	je end

	mov ah, 0eh  ;video teletype output
	int 10h

	mov dx, 4000
    call delay

	jmp print_string_delay

    end:
	; call endl
ret


;desc:
; imprime string na tela
;input:
; SI = string a ser impressa
printString:
    .loop:
        lodsb ; carrega caracter em al
        cmp al, 0

        je .endloop
        call putchar

        jmp .loop
    .endloop:
ret

;desc:
; imprime caracter na tela
;input:
; AL = caracter a ser impresso
putchar:
    mov ah, 0x0e ;ah = 14 é o modo de vídeo de 10h para imprimir na tela
    int 10h
ret


print_pontos:
    mov bh, 3
    .printT:
        mov dx, 50000
        call delay
        mov ah, 0eh ;modo de vídeo
        mov al, '.' ;caractere p/ imprimir
        int 10h

        dec bh
        cmp bh, 0
        jne .printT

    ;call endl

ret

start:
    xor ax, ax
    mov ds, ax
    mov es, ax

    mov ah, 0   ;set video mode
	mov al, 12h ;vga
	int 10h

  mov bl, 3 ;cor verde

  mov dh, 0   ;posição no eixo Y 
	mov dl, 15  ;posição no eixo X
	call set_cursor

  mov al, '<'
  call putchar

  mov dh, 0
	mov dl, 40
	call set_cursor

  mov al, '>'
  call putchar

  mov dh, 1
	mov dl, 0
	call set_cursor

    ;printando o icone
    mov si, p0
    call printString
    mov si, p1
    call printString
    mov si, p2
    call printString
    mov si, p3         
    call printString       
    mov si, p4        
    call printString
    mov si, p5
    call printString
    mov si, p6
    call printString 

    mov dh, 0
	mov dl, 18
	call set_cursor

    mov si, msgInicial
    call print_string_delay
    call print_pontos ;printa os pontos com delay 

    reset:
        mov ah, 00h ;reseta o controlador de disco
        mov dl, 0   ;floppy disk
        int 13h

        jc reset    ;se o acesso falhar, tenta novamente

        jmp load_kernel

    load_kernel:
        ;Setando a posição do disco onde kernel.asm foi armazenado(ES:BX = [0x7E00:0x0])
        mov ax,0x7E0	;0x7E0<<1 + 0 = 0x7E00
        mov es,ax
        xor bx,bx		;Zerando o offset

        mov ah, 0x02 ;le o setor do disco
        mov al, 20  ;porção de setores ocupados pelo kernel.asm
        mov ch, 0   ;track 0
        mov cl, 3   ;setor 3
        mov dh, 0   ;head 0
        mov dl, 0   ;drive 0
        int 13h

        jc load_kernel ;se o acesso falhar, tenta novamente

        jmp 0x7e00  ;pula para o setor de endereco 0x7e00, que é o kernel

  
    times 510-($-$$) db 0 ;512 bytes
    dw 0xaa55	