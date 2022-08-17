org 0x7e00
use16

%define var_base 8000h ; in RAM right after the boot sector
%define var(X)   [bp+(X)]
%define DELAY_L(a) (a	& 0xff00)
%define DELAY_H(b) (b >> 8 & 0xff00)

%define clock_ms    0x0000
%define cactus_test 0x0008
%define randomness  0x000A
%define dino_vel    0x000C
%define next_cactus 0x000D

%define bg_color 0x1e
%define fg_color 0x18

entry:
    ; Inicializar video no modo 13H (320x200 ; 256 cores)
    mov ax, 0x13 ; also sets AH=0 as function selector
    int 10h

    mov ax, 0xa000 ; 0xa000: inicio do buffer de vídeo (A000:0000 -> A000:F7FF)
    mov es, ax ; colocado no es pra ser usado nas instruções de string

    ; preencher tela:
    mov cx, 320 * 200
    mov al, bg_color
    rep stosb ; repete CX vezes o comando stosb (que move al p/ es:di e incrementa di (es sendo o buffer de vídeo))

    ; Chão:
    mov di, 158 * 320 ; na linha 158 (de 0-199)
    mov cx, 320 ; percorrer as 320 colunas
    mov al, fg_color ; pintar de cinza mais escuro
    rep stosb

    ; pontos no chão
    mov cx, 320*7 
    .dot:
        in al, 0x40 ; ler o valor atual de um timer de hardware para pseudo-aleatoriedade
        and al, 0x55 ; caso al & 0x55 = 0
        jz .black_dot ; marcar ponto no chão
        mov al, bg_color ; caso contrário, deixa da cor do fundo
        jmp .dot_cont
        .black_dot:
        mov al, fg_color
        .dot_cont:
        stosb ; não precisa mudar o di, porque nesse ponto do código ele já começa na primeira linha abaixo da linha do chão (159)
        loop .dot

    ; inicializando valores
    mov bp, var_base ; base pointer -> usado pra encontrar os ponteiros com o macro var etc
    mov cx, 0xff ; roda 255 vezes
    .rand_iter:
        in al, 0x40 ; ler o valor atual de um timer de hardware para pseudo-aleatoriedade
        xor byte var(randomness), al ; atualiza a variável aleatoria com esse contador
        in al, 0x40
        xor byte var(randomness+1), al
        loop .rand_iter

dino_y: dw 136 ; posição da altura do dinossauro

main:
  .game_loop:

    ; ; apaga o jogador na posição atual
    mov si, dino_sprite
    mov dl, bg_color
    mov bx, 20
    mov dh, byte [dino_y]
    call draw_sprite

    test byte var(clock_ms), 3 ; a cada 3 ciclos move o chão para a esquerda
    jnz .ground_cont
    ; mover obstaculos para esquerda
    mov cx, 33
    mov bx, 125
    clc ; não dar a volta
    call shift_rect_left
    ; mover o chão para a esquerda
    mov cx, 8
    mov bx, 158
    stc ; ao chegar no final, voltar pro inicio
    call shift_rect_left
    .ground_cont:

    ;; draw new cacti
    mov eax, dword var(clock_ms)
    cmp eax, dword var(next_cactus)
    jl .cactus_cont
    mov si, cactus_sprite
    mov dl, fg_color
    mov bx, 304
    mov dh, 146
    call draw_sprite

    ;; choose next cactus value
    ror word var(randomness), 2
    xor ax, ax
    mov al, byte var(randomness)
    and al, 3
    or al, 4
    shl ax, 7
    add eax, dword var(clock_ms)
    mov dword var(next_cactus), eax
    .cactus_cont:

    ;;update dino position
    test byte var(clock_ms), 15
    jnz .noupd
    ;; update pos
    mov al, byte var(dino_vel)
    sub byte [dino_y], al
    cmp byte [dino_y], 136
    jae .nodown
    sub byte var(dino_vel), 1
    jmp .noupd
    .nodown:
    mov byte [dino_y], 136
    .noupd:

    ; check keypress
    mov ah, 1
    int 16h
    jz .nostroke
    ; remove from buffer
    xor ah, ah
    int 16h
    ; check dino pos
    cmp byte [dino_y], 136 ; se n tiver na posição inicial
    jne .nostroke
    ; make our dino jump up and play a sound
    mov al, 3
    mov byte var(dino_vel), 8
    mov eax, dword var(clock_ms)
    .nostroke:

    ; draw dino at new position
     mov dl, fg_color
    mov si, dino_sprite
    mov bx, 20
    mov dh, byte [dino_y]
    call draw_sprite

    ; check collision
    mov bx, word [dino_y]
    mov ax, 320
    mul bx
    mov bx, ax
    mov dl, fg_color
    cmp byte [es:bx+(15*320)+33], dl
    je reset
    cmp byte [es:bx+(18*320)+32], dl
    je reset

    ; advance clock
    inc dword var(clock_ms)
    
    pusha 
    mov ah, 86h
    mov cx, DELAY_H(1500) ;10.000 microssegundos High Byte
    mov dx, DELAY_L(1500) ;10.000 microssegundos Low Byte
    int 15h 
    popa
  
  jmp .game_loop 
  ret


reset:
  int 19h

;description:
; shifts a row of pixels to the left
;input:
; BX = Y position
; CF = whether or not to wrap around (0 = false, 1 = true)
shift_row_left:
    ; save regs
    push cx
    pushf ; save because we need to check CF and mul below affects it
    ; calculate start of line
    mov ax, 320
    mul bx
    mov di, ax
    mov cx, 319
    ; do the thing
    push ds
    push es
    pop ds
    lea si, [di+1]
    rep movsb
    pop ds
    ; wrap around (restore flags prematurely to check if we actually need to)
    popf
    jnc .nowrap
    mov al, byte [es:di-319]
    mov byte [es:di], al
    .nowrap:
    ; restore regs
    pop cx
    ret

;description:
; shifts a rectangle starting at X=0 with a width of 320 to the left
;input:
; BX = Y position
; CX = height
; DX = whether or not to wrap around (0 = false, 1..255 = true)
shift_rect_left:
    .iter:
        call shift_row_left
        inc bx
        loop .iter
    ret

;description:
; draws a sprite
;input:
; DS:SI = sprite data
; BX = X coord
; CX = Y coord
; DL = foreground color
draw_sprite:
    ; save regs
    pusha
    ; calcula a posição na tela
    mov ax, 160 ; altura da tela / 2
    mul dh ; multiplica linha desejada
    shl ax, 1 ; left shift pra dobrar
    mov di, ax
    add di, bx
    ; read height into cx
    mov dh, byte [si]
    movzx cx, dh
    and cx, 0x1f
    add cl, 2
    ; keep width in dh
    shr dh, 5
    inc si
.row:
    push dx
    .chunk:
        ; read chunk
        mov ah, byte [si]
        mov al, 8
        .pixel:
            test ah, 0x80 ; test leftmost pixel
            jz .bg_pixel
            mov byte [es:di], dl ; foreground pixel
            .bg_pixel:
            shl ah, 1
            inc di
            dec al
            jnz .pixel
        inc si
        dec dh
        jnz .chunk
    ; go to second row
    pop dx
    add di, 320
    movzx ax, dh
    shl ax, 3
    sub di, ax
    loop .row
.return:
    ; restore regs
    popa
    ret

cactus_sprite: ; 11 bytes
    db (1 << 5) | (10 << 0)
    db 00001000b
    db 00011010b
    db 10011011b
    db 11011011b
    db 11011011b
    db 11011011b
    db 11011111b
    db 11111110b
    db 01111000b
    db 00011000b
    db 00011000b
    db 00011000b
dino_sprite: ; 67 bytes! MUHHHHH BLOATT!!!!!
    db (3 << 5) | (20 << 0)
    db 00000000b, 00011111b, 11100000b
    db 00000000b, 00111111b, 11110000b
    db 00000000b, 00110111b, 11110000b
    db 00000000b, 00111111b, 11110000b
    db 00000000b, 00111111b, 11110000b
    db 00000000b, 00111111b, 11110000b
    db 00000000b, 00111110b, 00000000b
    db 00000000b, 00111111b, 11000000b
    db 10000000b, 01111100b, 00000000b
    db 10000001b, 11111100b, 00000000b
    db 11000011b, 11111111b, 00000000b
    db 11100111b, 11111101b, 00000000b
    db 11111111b, 11111100b, 00000000b
    db 11111111b, 11111100b, 00000000b
    db 01111111b, 11111000b, 00000000b
    db 00111111b, 11111000b, 00000000b
    db 00011111b, 11110000b, 00000000b
    db 00001111b, 11110000b, 00000000b
    db 00000111b, 00110000b, 00000000b
    db 00000110b, 00100000b, 00000000b
    db 00000100b, 00100000b, 00000000b
    db 00000110b, 00110000b, 00000000b
