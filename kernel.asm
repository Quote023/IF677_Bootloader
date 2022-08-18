org 0x7e00
use16

%define var_base 8000h ; in RAM right after the boot sector
%define var(X)   [bp+(X)]
%define DELAY_L(a) (a	& 0xff00)
%define DELAY_H(b) (b >> 8 & 0xff00)

; ponteiros pra variáveis que não precisam de um valor inicial
%define clock_ms    0x0000 
%define randomness  0x000A
%define dino_vel    0x000C
%define next_obst 0x000D

%define player_base_y 141
%define ground_y 157
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
    mov di, ground_y * 320 ; na linha 158 (de 0-199)
    mov cx, 320 ; percorrer as 320 colunas
    mov al, fg_color ; pintar de cinza mais escuro
    rep stosb

    ; pontos no chão
    mov cx, 320*7 
    .dot:
        in al, 0x40 ; ler o valor atual de um contador de hardware para pseudo-aleatoriedade
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

player_y: dw player_base_y ; posição da altura do player

main:
  .game_loop:

    ; ; apaga o jogador na posição atual
    mov si, player_sprite_walk
    mov dl, bg_color
    mov bx, 20
    mov dh, byte [player_y]
    call draw_sprite

    test byte var(clock_ms), 3 ; move o chão para a esquerda quando o valor binário do clock começar com 11
    jnz .ground_cont
    ; mover obstaculos para esquerda
    mov cx, 35
    mov bx, 125
    clc ; não dar a volta
    call shift_rect_left
    ; mover o chão para a esquerda
    mov cx, 8
    mov bx, ground_y
    stc ; ao chegar no final, voltar pro inicio
    call shift_rect_left
    .ground_cont:

    ;; desenha obstaculo
    mov eax, dword var(clock_ms)
    cmp eax, dword var(next_obst) 
    jl .obst_cont ; caso ainda não seja a hora de um novo obstaculo (clock_ms < next_obst) 
    mov si, obstacle_sprite
    mov dl, fg_color
    mov bx, 303
    mov dh, 141 ;desenha um obstaculo no canto direito da tela
    call draw_sprite

    ;; define quando será o próximo obstaculo
    ror word var(randomness), 2 ; rotaciona o valor aleatorio 2 vezes
    xor ax, ax
    mov al, byte var(randomness)
    and al, 3
    or al, 4
    shl ax, 7
    add eax, dword var(clock_ms)
    mov dword var(next_obst), eax ; calcula e define quando o próximo obstaculo sera impresso (clock_ms + random)
    .obst_cont:

    ;; atualiza posição do player
    test byte var(clock_ms), 15 ;ignora alguns frames
    jnz .noupd
    ;; atualiza posição
    mov al, byte var(dino_vel)
    sub byte [player_y], al ; desce o player
    cmp byte [player_y], player_base_y
    jae .nodown ; não precisa descer se já tiver no chão
    sub byte var(dino_vel), 1 ; reduz a velocidade de queda/pulo
    jmp .noupd
    .nodown:
    mov byte [player_y], player_base_y
    .noupd:

    ; checa teclado com interrupção de teclado
    mov ah, 1
    int 16h
    jz .nostroke ; caso não tenha nenhuma tecla pressionada
    ; caso tenha, limpar o buffer do teclado
    xor ah, ah
    int 16h
    cmp byte [player_y], player_base_y ; não fazer nada se já estiver no ar
    jne .nostroke

    mov al, 3
    mov byte var(dino_vel), 8 ; adiciona velocidade => no próximo frame o player irá pular
    mov eax, dword var(clock_ms) ; guarda o frame atual do jogo
    .nostroke:

    ; desenha o player na posição atual
    mov si, player_sprite_walk  
    .drawPlayer_cont:
    mov dl, fg_color
    mov bx, 20
    mov dh, byte [player_y]
    call draw_sprite

    ; check collision
    mov bx, word [player_y]
    mov ax, 320
    mul bx
    mov bx, ax ; calcula a altura atual do player
    mov dl, fg_color
    cmp byte [es:bx+(9*320)+35], dl ; caso na posição da cabeça haja um pixel pintado = colisão
    je reset
    cmp byte [es:bx+(12*320)+36], dl ; caso na posição da perna haja um pixel pintado = colisão
    je reset

    ; aumenta o contador de frames
    inc dword var(clock_ms)
    
    ; interrupção pra esperar algum tempo entre frames
    pusha 
    mov ah, 86h
    mov cx, DELAY_H(1000) 
    mov dx, DELAY_L(1000) 
    int 15h 
    popa
  
  jmp .game_loop 
  ret


reset:
  int 19h

;desc:
; move uma linha de pixeis p/ esquerda
;input:
; BX = Posição em Y
; CF = shift circular (0 = false, 1 = true)
shift_row_left:
    push cx
    pushf ; salva flags: a cf é usada caso o shift seja circular
    ; calcular posição na linha
    mov ax, 320
    mul bx 
    mov di, ax
    mov cx, 319 ; loop do 0 -> 319
    push ds
    push es
    pop ds ; ds = es
    lea si, [di+1] ; indice do segmento ds (ds:si) com offset de +1
    rep movsb ; move o conteudo de (ds:si) p/ (es:di)(o segmento de vídeo)
    pop ds
    popf ; recupera a flag CF
    jnc .nowrap ; caso ela não seja 1 -> pula esse passo
    mov al, byte [es:di-319] ; pega o primeiro valor do segmento
    mov byte [es:di], al ; joga pro final
    .nowrap: 
    pop cx ; restaura o registrador
    ret

;desc:
; move a tela inteira (X=0 -> X=320) para a esquerda por 1 pixel
;input:
; BX = posição em Y
; CX = altura
; CF = move de forma circular (o lado esquerdo da tela vai pra direita ao chegar no fim)(0 = false, 1 = true)
shift_rect_left:
    .shiftLoop:
        call shift_row_left
        inc bx
        loop .shiftLoop ; loop até cx = 0
    ret

;desc:
; Desenha um Sprite
;input:
; DS:SI = Sprite
; BX = Posição X
; CX = Posição Y
; DH = Cor
draw_sprite:
    pusha
    ; calcula a posição na tela
    mov ax, 160 ; altura da tela / 2
    mul dh ; multiplica pela linha desejada
    shl ax, 1 ; left shift pra multiplicar por 2
    mov di, ax
    add di, bx ; soma a posição em x pra achar a coluna inicial

    ; largura (qtd de bytes por linha)
    lodsb ; primeiro byte do sprite
    mov dh, al 
    ; altura (qtd de linhas)
    lodsb ; segundo byte do sprite
    mov ch, 0
    mov cl,al
.row:
    push dx
    .chunk:
        mov ah, byte [si] ; pega cada byte da linha
        mov al, 8 ; cada bit
        .pixel:
            test ah, 10000000b ; faz um add dos 2 valores
            jz .bgPixel ; caso o 1º digito seja 0 ele é ignorado
            mov byte [es:di], dl ; caso contrario o pixel é pintado
            .bgPixel:
            shl ah, 1 ; left shift pra pegar o próximo digito
            inc di ; próximo pixel na tela
            dec al ; menos 1 bit à ser lido
            jnz .pixel ; loop até que todos os 8 bits sejam lidos
        inc si ; próximo byte da linha
        dec dh ; menos 1 byte à ser lido 
        jnz .chunk ; loop até que todos os n bytes de largura sejam lidos
    pop dx ; recupera o tamanho da largura (dh)
    add di, 320 ; pula 1 linha
    movzx ax, dh ; move dh -> ax e preenche o espaço restante (dh = 1 byte, ax = 2 bytes) com 0s
    shl ax, 3 
    sub di, ax ; subtrai a largura do di (pois após somar 320 o indice está apontando pra ponta direita do sprite)
    loop .row ; loop pela altura
.return:
    popa ; restaura registradores
    ret

; Estrutura dos sprites:
; primeiro byte: (qtd de byes << 5) | ((altura - 2) << 0)

obstacle_sprite:
    db 2, 16
    db 00000011b,11000000b
    db 00000111b,11100000b
    db 00001111b,11110000b
    db 00011111b,11111000b
    db 00111111b,11111100b
    db 01110111b,11101110b
    db 01110101b,10101110b
    db 11110101b,10101111b
    db 11110001b,10001111b
    db 11111111b,11111111b
    db 01111100b,00111110b
    db 00001000b,00010000b
    db 00111000b,00010000b
    db 01111100b,00111000b
    db 01111111b,11111000b
    db 00111110b,01110000b

; player_sprite_idle:
;     db 2, 16
;     db 00000000b,00000000b
;     db 00000001b,11111000b
;     db 00000010b,00000110b
;     db 00000100b,00000001b
;     db 00000111b,00101110b
;     db 00001001b,10100001b
;     db 00001001b,10010001b
;     db 00000110b,00111110b
;     db 00000011b,00000100b
;     db 00000100b,11001000b
;     db 00001000b,01100100b
;     db 00001000b,01111100b
;     db 00000100b,01101100b
;     db 00000100b,00011100b
;     db 00000010b,00001000b
;     db 00000011b,11111000b

player_sprite_walk:
    db 2, 16
    db 00000011b,11110000b
    db 00000100b,00001100b
    db 00001000b,00000010b
    db 00001110b,01011100b
    db 00010011b,01000010b
    db 00010011b,00100010b
    db 00001100b,01111100b
    db 00001110b,00001000b
    db 01110001b,10011100b
    db 10010000b,11001010b
    db 10010011b,11111001b
    db 01101111b,10110101b
    db 00010111b,11110010b
    db 00100111b,01100010b
    db 00100010b,01000100b
    db 00011100b,00111000b


  
