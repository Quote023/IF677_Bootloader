org 0x7c00 ; soma ao offset
jmp 0x0000:start ;CS:IP (CS == 0)
	
start:

	xor ax, ax
	mov ds, ax

	;https://en.wikipedia.org/wiki/INT_13H
	Reset_Disk_Drive:
		mov ah,0		;INT 13h AH=00h: Reset Disk Drive
		mov dl,0		;floppydisk 
		int 13h			;interrupção de acesso ao disco
	jc Reset_Disk_Drive		;se der erro CF é setado, daí voltaria para o Reset_Disk_Drive

	

	mov ax,0x50
	mov es,ax
	xor bx,bx
	Load_Boot2:
		mov ah, 0x02	;INT 13h AH=02h: Read Sectors From Drive
		mov al,1	    ;numero de setores ocupados pelo boot2 
		mov ch,0		;trilha 0
		mov cl,2		;setor 2
		mov dh,0		;cabeca 0
		mov dl,0		;drive 0
		;o endereço de interesse é o es:bx
		;nosso boot2 está escrito em 0x0500(físico)
		;assim, deseja-se ler 0050:0000 -> (0x0050 << 1)+0x0000 -> 0x00500 + 0x0000 -> 0x00500
		int 13h
	jc Load_Boot2		;se der erro CF é setado, daí voltaria para o Load_Boot2

	jmp 0x500
	

times 510-($-$$) db 0		; zera o resto do bootsector
dw 0xaa55					; coloca a assinatura de boot no final
							; do setor (x86 : little endian)