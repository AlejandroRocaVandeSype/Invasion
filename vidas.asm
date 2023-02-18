;----------------------------------------------------
;----------------------------------------------------	
; FICHERO CON TODO LO RELACIONADO CON LAS VIDAS DE LOS NIVELES
;----------------------------------------------------	
;----------------------------------------------------

.include "cabecera.h.s" 



.section "FUNCIONES VIDAS" FREE



;-----------------------------------------------------------------------------
; Funcion para actualizar todo lo relacionado con las vidas de los niveles
; IN: 
; MODIFICA: HL, DE, BC, IY, A
;-----------------------------------------------------------------------------
vidas_update:


	ld iy, player
	call checkColision

	ld a, (ix+game_object.colision)
	cp play_id
	jr z, _coger_vida

		; El player no ha cogido la vida
		; Dibujado vida en SAT
		ld hl, vidaccs 						
		ld e, (ix+game_object.cc) 					; 
		ld d, (ix+game_object.cc+1)     			; DE = Posicion inicial de los charcodes  						
		ld bc, 1 	   							
		call cargarSpriteBuffer 				; Cargamos el sprite en el buffer del SAT.

		ld l, (ix+game_object.PH) 					; 
		ld h, (ix+game_object.PH+1)     			; HL = Primera pos del buffer para las posiciones horizontales del sprite
		ld e, (ix+game_object.PV) 					; 
		ld d, (ix+game_object.PV+1)     			; DE = Primera pos del buffer para las posiciones verticales del sprite
		call updateBufferSAT  					; Actualizamos el buffer del SAT con la pos X e Y del sprite a dibujar.

		ret

	_coger_vida:

	; Incrementamos en 1 la vida del jugador
	ld a, (player.vida)
	inc a
	ld (player.vida), a

	; Borrar vida al recoger
	ld l, (ix+game_object.PV) 					; 
	ld h, (ix+game_object.PV+1)     			; HL = Primera pos del buffer para las posiciones horizontales del sprite
	ld e, (ix+game_object.PH) 					; 
	ld d, (ix+game_object.PH+1)    				; DE = Primera pos del buffer para las posiciones verticales del sprite) 	
	call borrarEntidad 							; Borramos la vida

ret


;-----------------------------------------------------------------------------
; Funcion para dibujar por pantalla el valor que indica el numero de vidas 
; restantes del jugador
; IN: 
; MODIFICA: HL, A, B, DE
;-----------------------------------------------------------------------------
contadorVidas:
	
	ld hl, numeroccs 							; HL = valores de indices de tiles de los numeros
	ld a, (player.vida) 						; Vidas restantes del jugador
	ld b, a
	cp 0 										; 
	jr z, _primervalor 							; Si el valor es 0, ya estamos apuntando al charcode correcto
--  	inc hl 									; Siguiente digito
		djnz -- 								; Bucle para dibujar el valor de digito correcto

	_primervalor:
	; HL --> Apuntando a charcode o copiar
	ld e, (ix+game_object.cc) 					; 
	ld d, (ix+game_object.cc+1)     			; DE = Posicion inicial de los charcodes
	ldi  										; Copiamos el contenido de Hl a DE


ret


.ends