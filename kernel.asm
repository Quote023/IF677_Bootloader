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
	jz

	mov ax, [%2]
	cmp ax, [%4]
	jz
%endmacro

;; Logica
setup_game:

	mov ax, 0003h
	int 10h

	mov ax, Video
	mov es, ax
	
	;; Posicao Cobra
	mov ax, [PlayerX]
	mov ax, [PlayerY]

;; Game loop
game:
	.drawscreen:
	mov ax, 1020h
	xor di, di
	mov cx, Largura*Altura

	.drawsnake:
	mov ax, [PlayerX]
    mov bx, [PlayerY]
    mov cx, 80
    mul cx
    add ax, bx
    mov di, ax
    mov dl, 7
    mov [es:di], dl

    .drawapple:
    mov ax, [ComidaX]
    mov bx, [ComidaY]
    mov cx, 80
    mul cx
    add ax, bx
    mov di, ax
    mov dl, 7
    mov [es:di], dl

	.mov_automatico:
	mov al, [direction]
	cmp al, UP
	je move_up
	cmp al, DOWN
	je move_down
	cmp al, LEFT
	je move_left
	cmp al, RIGHT
	je move_right

	move_up:
		dec word [PlayerY]		; Move up 1 row on the screen

	move_down:
		inc word [PlayerY]		; Move down 1 row on the screen

	move_left:
		dec word [PlayerX]		; Move left 1 column on the screen

	move_right:
		inc word [PlayerX]		; Move right 1 column on the screen

	player_input:
		mov bl, [direction]		
		
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
        cmp al, 'r'
        je r_pressed

		jmp check_colisao

		w_pressed:
            
			mov bl, UP
			jmp check_colisao

		s_pressed:
            
			mov bl, DOWN
			jmp check_colisao

		a_pressed:
            
			mov bl, LEFT
			jmp check_colisao

		d_pressed:
            
			mov bl, RIGHT
			jmp check_colisao

		r_pressed:
            
			int 19h

        check_colisao:
		
		mov ax, [PlayerX]
		cmp ax, [ComidaX]
		jz game

		mov ax, [PlayerY]
		cmp ax, [ComidaY]
        jz game

        next_apple:
		xor ah, ah
		int 1Ah			; Timer ticks since midnight in CX:DX
		mov ax, dx		; Lower half of timer ticks
		xor dx, dx		; Clear out upper half of dividend
		mov cx, 80
		div cx			; (DX/AX) / CX; AX = quotient, DX = remainder (0-79) 
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



  
