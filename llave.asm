;----------------------------------------------------
;----------------------------------------------------	
; FICHERO CON TODO LO RELACIONADO CON LAS LLAVES
;----------------------------------------------------	
;----------------------------------------------------

.include "cabecera.h.s" 


.section "FUNCIONES LLAVE" FREE



;-----------------------------------------------------------------------------
; Funcion para actualizar todo lo relacionado con la llave del nivel
; IN: 
; MODIFICA: HL, DE, BC, llave
;-----------------------------------------------------------------------------
llave_update:


	ld a, (ix+game_object.estado)
	cp 0
	ret z 										; Si la llave no esta activa, salimos

		; Llave activa
		ld iy, player
		call checkColision

		ld a, (ix+game_object.colision)
		cp play_id
		jr z, _col_llave 						; Comprobamos si el jugador a colisionado con la llave

			; El jugador no ha recogido la llave, la dibujamos
			; Dibujado llave en SAT
			ld hl, llaveccs 						
		   	ld e, (ix+game_object.cc) 						; 
			ld d, (ix+game_object.cc+1)     				; DE = Posicion inicial de los charcodes    						
		    ld bc, 4 	   							
			call cargarSpriteBuffer 				; Cargamos el sprite en el buffer del SAT.

			
			ld l, (ix+game_object.PH) 					; 
			ld h, (ix+game_object.PH+1)     			; HL = Primera pos del buffer para las posiciones horizontales del sprite
			ld e, (ix+game_object.PV) 					; 
			ld d, (ix+game_object.PV+1)     			; DE = Primera pos del buffer para las posiciones verticales del sprite
			call updateBufferSAT  					; Actualizamos el buffer del SAT con la pos X e Y del sprite a dibujar.

			ret

		_col_llave:

		ld a, (ix+game_object.id)
		ld (llave), a 								; Le pasamos el id de la llave al player para saber que la lleva

		; Borrar llave al recoger
		ld l, (ix+game_object.PV) 					; 
		ld h, (ix+game_object.PV+1)     			; HL = Primera pos del buffer para las posiciones horizontales del sprite
		ld e, (ix+game_object.PH) 					; 
		ld d, (ix+game_object.PH+1)    				; DE = Primera pos del buffer para las posiciones verticales del sprite) 	
		call borrarEntidad 							; Borramos la llave



ret


.ends