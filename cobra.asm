;org 07C00h		
org 8000h		

jmp setup_game 

;; Constante
Video 		equ 0B800h	
Largura	    equ 80
Altura		equ 25
UP			equ 0
DOWN		equ 1
LEFT		equ 2
RIGHT		equ 3

;; Variaveis
PlayerX:     dw 18
PlayerY:     dw 16
ComidaX: 	 dw 10
ComidaY:	 dw 12
direction:	 db 4

%macro draw 3
	mov ax, [%1]
    mov bx, [%2]
    mov cx, 80
    mul cx
    add ax, bx
    mov di, ax
    mov dl, %3
    mov [es:di], dl
%endmacro

%macro colisao 4
	mov ax, [%1]
	cmp ax, [%3]
	jz game

	mov ax, [%2]
	cmp ax, [%4]
	jz game
%endmacro

%macro mudar_movimento 2
	mov bl, %1
	mov [%2], bl
%endmacro

%macro mover 1
	mov al, [%1]
	cmp al, UP
	dec word [PlayerY]
	cmp al, DOWN
	inc word [PlayerY]
	cmp al, LEFT
	dec word [PlayerX]
	cmp al, RIGHT
	inc word [PlayerX]

	jmp player_input
%endmacro

;; Logica
setup_game:
	mov ax, 0003h
	int 10h

	mov ax, Video
	mov es, ax

;; Game loop
game:
	drawscreen:
	mov ax, 500h
	xor di, di
	mov cx, Largura*Altura
	rep stosw

	drawsnake:
	;mov bl, 1
	;mov ch, 0
	;mov cl, [PlayerX]
	;mov ch, 0
	;mov dl, [PlayerY]

	;mov ax, [PlayerX]
    ;mov bx, [PlayerY]
    ;mov cx, 80
    ;mul cx
    ;add ax, bx
    ;mov di, ax
    ;mov dl, 7
    ;mov [es:di], dl

    drawapple:
    mov ax, [ComidaX]
    mov bx, [ComidaY]
    mov cx, 80
    mul cx
    add ax, bx
    mov di, ax
    mov dl, 7
    mov [es:di], dl

	mov_automatico:
	mov al, [direction]
	cmp al, UP
	je move_up
	cmp al, DOWN
	je move_down
	cmp al, LEFT
	je move_left
	cmp al, RIGHT
	je move_right

	jmp player_input

	move_up:
		dec word [PlayerY]		
		jmp player_input

	move_down:
		inc word [PlayerY]		
		jmp player_input

	move_left:
		dec word [PlayerX]		
		jmp player_input

	move_right:
		inc word [PlayerX]		
		jmp player_input

	player_input:		
		mov ah, 1
		int 16h					
        jz check_colisao

		xor ah, ah
		int 16h					
		
		cmp al, 'w'
		je w_pressed
		cmp al, 's'
		je s_pressed
		cmp al, 'a'
		je a_pressed
		cmp al, 'd'
		je d_pressed
        ;cmp al, 'r'
        ;je r_pressed

		w_pressed:
			mov ax, UP
			mov [direction], ax
			jmp check_colisao

		s_pressed:
			mov ax, DOWN
			mov [direction], ax
			jmp check_colisao

		a_pressed:
			mov ax, LEFT
			mov [direction], ax
			jmp check_colisao

		d_pressed:
			mov ax, RIGHT
			mov [direction], ax
			jmp check_colisao
		
		;r_pressed:

    check_colisao:		
		mov ax, [PlayerX]
		cmp ax, [ComidaX]
		jz game

		mov ax, [PlayerY]
		cmp ax, [ComidaY]
        jz game

    next_apple:
		xor ah, ah
		int 1Ah			
		mov ax, dx		
		xor dx, dx		
		mov cx, 80
		div cx			
		mov word [ComidaX], dx
			
		xor ah, ah
		int 1Ah			
		mov ax, dx		
		xor dx, dx		
		mov cx, 25
		div cx			 
		mov word [ComidaY], dx

	jmp game

	resetar:

		xor ah, ah
		int 16h
		int 19h

times 510 - ($-$$) db 0
dw 0AA55h