;----------------------------------------------------
;----------------------------------------------------	
; FICHERO CON TODO LO RELACIONADO CON LAS PUERTAS
;----------------------------------------------------	
;----------------------------------------------------

.include "cabecera.h.s" 



.section "FUNCIONES PUERTAS" FREE



;-----------------------------------------------------------------------------
; Funcion para actualizar todo lo relacionado con la puerta del nivel
; IN: 
; MODIFICA: HL, DE, BC
;-----------------------------------------------------------------------------
puerta_update:


	; Dibujado puerta en SAT
	ld hl, puertaAccs 						
  	ld e, (ix+game_object.cc) 						; 
	ld d, (ix+game_object.cc+1)     				; DE = Posicion inicial de los charcodes  						
    ld bc, 9 	   							
	call cargarSpriteBuffer 				; Cargamos el sprite en el buffer del SAT.

	ld l, (ix+game_object.PH) 					; 
	ld h, (ix+game_object.PH+1)     			; HL = Primera pos del buffer para las posiciones horizontales del sprite
	ld e, (ix+game_object.PV) 					; 
	ld d, (ix+game_object.PV+1)     			; DE = Primera pos del buffer para las posiciones verticales del sprite
	call updateBufferSAT  					; Actualizamos el buffer del SAT con la pos X e Y del sprite a dibujar.


	; Comprobamos si hay que hacer un cambio de nivel
	call checkCambioNivel


ret


;-----------------------------------------------------------------------------
; Funcion para acomprobar si el jugador colisiona con la puerta y, en caso afir-
; mativo, comprobar tambien si pulsa la tecla "alt" para cambiar de nivel.
; IN: 
; MODIFICA: IY, A, NIVEL, CAMBIONIVEL
;-----------------------------------------------------------------------------
checkCambioNivel:

	ld iy, player
	call checkColision 						; Comprobamos si hay colision entre la puerta y el player

	ld a, (ix+game_object.colision)
	cp play_id								; 
	jr nz, _nocambio_lvl 					; Comprobamos si ha colisionado la puerta con el jugador

		; Colision entre la puerta y el player
		ld a, (numLlaves)
		cp 0
		jr z, _no_llaves 					; Comprobamos si hay llaves en el nivel actual

			; Hay llaves en el nivel. Debemos tener la llave para poder abrir
			ld a, (llave)					; A = id de la llave que lleva el jugador
			cp 0 
			ret z 							; Si no lleva llave, salimos ya que no se puede abrir la puerta

		_no_llaves: 
		; No hay llaves, se puede abrir la puerta
		call getInput 						; Comprobamos que tecla esta pulsando el jugador
		bit 5, a 							;
		jr nz, _nocambio_lvl 				; Si el jugador pulsa la tecla "alt" cuando esta en la puerta, se cambia de nivel.

			; Tecla ALT pulsada
			ld a, (nivel) 					; A = nivel actual en el que esta el jugador
			inc a  							;
			ld (nivel), a 					; Siguiente nivel
			ld a, 1 						;
			ld (cambioNivel), a 			; Indicamos que queremos hacer un cambio de nivel
			ld a, 0
			ld (llave), a 					; Llave usada.

	_nocambio_lvl:
	; No se hace cambio de nivel


ret





.ends