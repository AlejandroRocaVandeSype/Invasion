;----------------------------------------------------
;----------------------------------------------------	
; FICHERO CON TODO LO RELACIONADO CON LAS CRISTALES DEL LEVEL FINAL
;----------------------------------------------------	
;----------------------------------------------------


.include "cabecera.h.s" 

.ramsection "Variables bala" slot 3	 ; Es en la ranura 2 donde tenemos el espacio de ram.
	
	cristalesDestruidos db 			; Variable para saber cuantos cristales ha destruido el jugador

.ends

.section "FUNCIONES CRISTAL" FREE


;-----------------------------------------------------------------------------
; Funcion para actualizar todo lo relacionado con el cristal del nivel final
; IN: 
;  		IX --> Puntero a los datos del cristal correspondiente
; MODIFICA: A, BC, HL, DE, 
;-----------------------------------------------------------------------------
cristal_update:
	
	ld a, (ix+game_object.colision)
	cp bala_identif
	jr z, _destruirCristal

		ld a, b
		cp 2
		jr nz, _no_mover 							; Si no es el segundo cristal no hay que moverlo

			; 2º cristal. Se mueve
			call moverCristal

		_no_mover:
		ld hl, cristalccs 						
		ld e, (ix+game_object.cc) 						; 
		ld d, (ix+game_object.cc+1)     				; DE = Posicion inicial de los charcodes    						
		ld bc, 2 	   							
		call cargarSpriteBuffer 						; Cargamos el sprite en el buffer del SAT.

				
		ld l, (ix+game_object.PH) 					; 
		ld h, (ix+game_object.PH+1)     			; HL = Primera pos del buffer para las posiciones horizontales del sprite
		ld e, (ix+game_object.PV) 					; 
		ld d, (ix+game_object.PV+1)     			; DE = Primera pos del buffer para las posiciones verticales del sprite
		call updateBufferSAT  						; Actualizamos el buffer del SAT con la pos X e Y del sprite a dibujar.

		ret

	_destruirCristal:

		; Borrar cristal
		ld l, (ix+game_object.PV) 					; 
		ld h, (ix+game_object.PV+1)     			; HL = Primera pos del buffer para las posiciones horizontales del sprite
		ld e, (ix+game_object.PH) 					; 
		ld d, (ix+game_object.PH+1)    				; DE = Primera pos del buffer para las posiciones verticales del sprite) 	
		call borrarEntidad 							; Borramos el cristal

		; Un cristal mas destruido por el jugador
		ld a, (cristalesDestruidos)
		inc a
		ld (cristalesDestruidos), a

		; Se comprueba si se han destruido todos los cristales
		cp total_cristales
		jr nz, _NoFinJuego

			; Todos los cristales destruidos. El jugador ha terminado el juego
			ld a, 3
			ld (mundo_state), a 					; Cargamos a la variable del estado del mundo. Ha estado de fin del juego.
			call checkEstadoMundo


		_NoFinJuego:


ret


;-----------------------------------------------------------------------------
; Funcion 
; IN: 
; MODIFICA: A, BC, HL, DE, 
;-----------------------------------------------------------------------------
moverCristal:

	ld a, (ix+game_object.dir)
	cp 1
	jr z, _moveDown
	
		_moveUp:
		  	; MOVIMIENTO HACIA ARRIBA 
		  	ld a, (ix+game_object.y)
		  	sub 1
		  	ld (ix+game_object.y), a

		  	cp 40
		  	jr nc, _noLimit

		  		ld a, 1
		  		ld (ix+game_object.dir), a 				; Cambiamos la dir de movimiento del cirstal

		ret


	_moveDown:
	  	; MOVIMIENTO HACIA ABAJO 
	  	ld a, (ix+game_object.y)
	  	add a, 1
	  	ld (ix+game_object.y), a

	  	cp 120
	  	jr c, _noLimit 								; Se comprueba si ha superado el limite inferior. Si se produce acarreo no se ha superado.

	  		ld a, 0
	  		ld (ix+game_object.dir), a 				; Cambiamos la dir de movimiento del cristal


	 _noLimit:


ret





.ends