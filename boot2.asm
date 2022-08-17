org 0x500 
jmp 0x0000:start

%define DELAY_L(a) (a	& 0xff00)
%define DELAY_H(b) (b >> 8 & 0xff00)
%define LOADING_LMT_HL 32 ; limite esquerdo da barra de loading
%define LOADING_LMT_HR 288 ; limite direito da barra de loading
%define LOADING_LMT_VT 150 ; limite superior da barra de loading
%define LOADING_LMT_VB 165 ; limite inferior da barra de loading
%define COLOR 0x0b


string1 db 'Carregando Estruturas do Kernel...',13,10,0
string2 db ' _.........._ ',13,10,0
string3 db '| |Cin     | |',13,10,0
string4 db '| |        | |',13,10,0
string5 db '| |        | |',13,10,0
string6 db '| |________| |',13,10,0
string7 db '|   ______   |',13,10,0
string8 db '|  |    | |  |',13,10,0
string9 db '|__|____|_|__|',13,10,0

line dw 0
max dw 0

start:
	xor ax,ax ; limpar ax
	mov ds,ax

	Reset_Disk_Drive:
		mov ah,0		;INT 13h AH=00h: Reset Disk Drive
		mov dl,0		;floppydisk 
		int 13h			;interrupção de acesso ao disco
  jc Reset_Disk_Drive		;se der erro CF é setado, daí voltaria para o Reset_Disk_Drive

	mov ax,0x0013 ; ah = 0 (mudar o modo de video) ; al = 13h -> 256 cores 320x200px
	int 10h

	call loading_limit
	mov al,0x00
	call cursor_blink

  mov bl, COLOR
  mov si, string1
  call printString	
  ; mov si,string2
	; call printString
  ; mov si,string3
	; call printString
  ; mov si,string4
	; call printString
  ; mov si,string5
	; call printString
  ; mov si,string6
	; call printString
  ; mov si,string7
	; call printString
  ; mov si,string8
	; call printString
  ; mov si,string9
	; call printString

  call loading_limit
	call loading
	call loading_off

	mov ax,0x08c0;mov ax,0x07e0
	mov es,ax ; 
	mov bx,0x0000 ; 07e0:0000 -> 0x07e00
	Load_Kernel:
		mov ah, 0x02		;;INT 13h AH=02h: Read Sectors From Drive
		mov al, 30	;numero de setores ocupados pelo kernel
		mov ch,0		;trilha 0
		mov cl,3	;vai comecar a ler do setor 3
		mov dh,0		;cabeca 0
		mov dl,0		;drive 0
		int 13h			;interrupcao de disco
	jc Load_Kernel	;se der erro CF é setado, daí voltaria para o Load_Kernel	

jmp 0x8c00


cursor_blink:
	mov ah,0x00
	mov cl,0x10
	mul cl
	inc ax 
	mov di,line
	mov word [di],ax
	mov cx,0x02
	call cursor_on
	call delay
	call cursor_off
	call delay
	call cursor_on
	call delay
	call cursor_off
	ret 


cursor_on:
	mov di,line
	mov si,max
	xor ax,ax
	add ax,0x000b
	add ax,word [di]
	mov word[si],ax
	mov ah,0x0c ;ah=0x0c (pixel na cordenada dx,cx)
	mov al,COLOR ;al é a cor  
	mov bh,0x00
	xor cx,cx
	loop_cursor_on:
		mov dx,word [di]
		int 10h
		loop2_cursor_on:
			inc dx
			cmp dx,word[si]	
			int 10h
			jne loop2_cursor_on
		inc cx
		cmp cx,0x0008
		jne loop_cursor_on
	ret

cursor_off:
	mov di,line
	mov si,max
	xor ax,ax
	add ax,0x000b
	add ax,word [di]
	mov word[si],ax
	mov ax,0x0c00 ;ah=0x0c (pixel na cordenada dx,cx) e al é a cor 0x00(pretos)
	mov bh,0x00
	xor cx,cx
	loop_cursor_off:
		mov dx,word [di]
		int 10h
		loop2_cursor_off:
			inc dx
			cmp dx,word[si]	
			int 10h
			jne loop2_cursor_off
		inc cx
		cmp cx,0x0008
		jne loop_cursor_off
	ret

loading:
	mov cx,LOADING_LMT_HL
	loop_loading:
		call loading_unit
		inc cx
		push cx
		xor cx,cx
		call delay
		pop cx
		cmp cx,LOADING_LMT_HR
		jne loop_loading
		mov ah, 86h
		mov cx, 10	
		xor dx, dx
		mov dx, 40	
		int 15h
	ret

loading_off:
	mov cx,LOADING_LMT_HL
	loop_loading_off:
		call loading_unit_off
		inc cx
		cmp cx,LOADING_LMT_HR
		jne loop_loading_off
	ret

loading_unit_off:
	mov ax,0x0c00
	mov bh,0x00
	mov dx,LOADING_LMT_VT
	loop_loading_unit_off:
		int 10h
		inc dx
		cmp dx,LOADING_LMT_VB
		jne loop_loading_unit_off
	ret 


loading_limit:
  mov ah,0x0c ;ah=0x0c (pixel na cordenada dx,cx)
	mov al,COLOR ;al é a cor  
	mov bh,0x00   ; pagina 0
	mov dx,LOADING_LMT_VT
	loading_limit_vloop:
		mov cx,LOADING_LMT_HL 
		int 10h
    mov cx,LOADING_LMT_HR 
		int 10h
		inc dx
		cmp dx,LOADING_LMT_VB ; Y = 110
		jne loading_limit_vloop
	ret

printString:
    xor ax, ax
    .loop:
        lodsb ; carrega caracter em al
        cmp al, 0
        je .endloop
        call putchar
        jmp .loop
    .endloop:
ret

putchar:
    mov ah, 0x0e
    int 10h
ret

set_cursor:     ;dh = linha/eixo y ~ dl = coluna/eixo x
	mov ah, 02h ;set cursor position 
	mov bh, 00h ;page number
	int 10h
	ret

delay: ;0.01 segundos
	mov ah, 86h
  mov cx, DELAY_H(5000) ;10.000 microssegundos High Byte
	mov dx, DELAY_L(5000) ;10.000 microssegundos Low Byte
	int 15h 
ret

loading_unit:
  mov ah,0x0c ;ah=0x0c (pixel na cordenada dx,cx)
	mov al,COLOR ;al é a cor  
	mov bh,0x00
	mov dx,LOADING_LMT_VT
	loop_loading_unit:
		int 10h	
		inc dx
		cmp dx,LOADING_LMT_VB
		jne loop_loading_unit
	ret 
jmp $