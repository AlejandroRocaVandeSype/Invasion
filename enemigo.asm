;=========================================================
;
; FICHERO CON TODOS LAS DATOS Y FUNCIONES DEL ENEMIGO
;
;=========================================================
.include "cabecera.h.s"


.ramsection "Variables enemigo" slot 3	 ; Es en la ranura 2 donde tenemos el espacio de ram.
	
    enemigoBasInf instanceof enemigo_var 2

.ends


.section "FUNCIONES ENEMIGO" free

;-----------------------------------------------------------------------------
; Funcion para actualizar todo lo relacionado con el enemigo.
; IN: 
; MODIFICA:  
;-----------------------------------------------------------------------------
enemigo_update:

	call checkEstadoEnemigo

	ld a, (ix+game_object.estado)
	cp ent_muerta 											;
	ret z 													; Si A == 0 entonces el enemigo esta muerto
		
		; La entidad esta viva.
		ld a, (iy+enemigo_var.IA_estado)
		cp 0 												;
		jr nz, IA_movimiento 								; Comprobamos en que estado se encuentra la IA del enemigo

			call IA_estadoReposo  							; Comenzamos la IA de reposo

			jr pintadoEnemigo

		IA_movimiento:

			call IA_estadoMovimiento 						; Comenzamos la IA de movimiento

			; DIBUJAMOS POR PANTALLA EL SPRITE CORRESPONDIENTE DE MOVIMIENTO
			ld hl, enemigAndar2ccs								; Sprite de enemigo mirando a la derecha y moviendose

			ld a, (ix+game_object.dir)
			cp player_derecha 
			jr z, _cargarCCI 		 							; Comprobamos la direcc a la que mira				
			  
			; Hay que cargar los ccs del sprite moviendose a la izquierda
			ld hl, enemigAndar2Iccs 							; Sprite de enemigo mirando a la izquierda y moviendose

			_cargarCCI:

			; Cargamos los ccs al buffer
			ld e, (ix+game_object.cc) 							;
			ld d, (ix+game_object.cc+1)	 						; DE = posicion primer charcode del sprite
			ld bc, 12 	   				
			call cargarSpriteBuffer 							; Cargamos el sprite del enemigo en el buffer del SAT.

		pintadoEnemigo:

		call dibujarEnemigo 									; Dibujamos al enemigo basico por pantalla

ret

;-----------------------------------------------------------------------------
; Comprueba el estado del enemigo: Si ha colisionado con algo o no
; IN:
; MODIFICA A, IX, HL, DE
;-----------------------------------------------------------------------------
checkEstadoEnemigo

	ld a, (ix+game_object.colision)
	cp 0  								;
	jr z, _no_colision  				; Si A = 0, no ha colisionado con nada

		; Enemigo ha colisionado con algo. Comprobamos con que
		cp bala_identif 				;
		jr nz, _no_colision

			call checkVida_enemigos 		; Si ha colisionado con la bala (A==3) llamamos a la funcion para restar una vida

		ld a, (ix+game_object.vida)
		cp 0 							;
		jr z, _colision 	 			; Si el valor devuelto en A es 0 quiere decir que el enemigo se ha quedado sin vidas

			; No colision con la bala. Aqui habria que hacer mas comprobaciones si puede recibir daño de otra fuente

			ret

		_colision:

		; Borrar enemigo
		ld l, (ix+game_object.PV) 		; 
		ld h, (ix+game_object.PV+1)     ; HL = Primera pos del buffer para las posiciones horizontales del sprite
		ld e, (ix+game_object.PH) 		; 
		ld d, (ix+game_object.PH+1)    	; DE = Primera pos del buffer para las posiciones verticales del sprite) 	
		call borrarEntidad 				; Borramos al enemigo

	_no_colision:

ret


;-----------------------------------------------------------------------------
; Funcion para realizar la IA del enemigo cuando este se encuentra en estado
; reposo. Esto quiere decir que el enemigo estara quiero durante unos segundos.
; Pasados esos segundos se cambiara el estado al de movimiento
; IN:
; MODIFICA: 
;-----------------------------------------------------------------------------
IA_estadoReposo

	ld a, (iy+enemigo_var.tiempo_Pr)
	cp 0
	jr nz, _empezado

		; El estado de reposo acaba de empezar
		ld a, (tiempo)
		ld (iy+enemigo_var.tiempo_Pr), a 				; Almacenamos el segundo en el que empieza
		ret

	_empezado:
	; Comprobamos cuanto lleva de reposo
	ld a, (iy+enemigo_var.tiempo_Pr)
	call calcularTranscursoTiempo
	cp 2
	jr c, _no_terminado

		; Tiempo reposo terminado
		ld a, 0
		ld (iy+enemigo_var.tiempo_Pr), a 				; Reseteamos el valor del tiempo de reposo

		ld a, 1
		ld (iy+enemigo_var.IA_estado), a 					; Cambiamos la IA a estado de movimiento
		ret

	_no_terminado:

	; Aqui se podrian meter una animacion de mirando derecha/izquierda

	; DIBUJAMOS EN PANTALLA EL SPRITE CORRESPONDIENTE DE REPOSO
	ld hl, enemigccs								; Sprite de enemigo mirando a la derecha y quieto
	ld a, (ix+game_object.dir)
	cp player_derecha 
	jr z, _cargarCCD 		 						; Comprobamos la direcc a la que mira				
  
	; Hay que cargar los ccs del sprite quieto a la izquierda
	ld hl, enemIzqdccs 							; Sprite de enemigo mirando a la izquierda

	_cargarCCD:
	; Cargamos los ccs al buffer
	ld e, (ix+game_object.cc) 						;
	ld d, (ix+game_object.cc+1)	 					; DE = posicion primer charcode del sprite
	ld bc, 12 	   				
	call cargarSpriteBuffer 						; Cargamos el sprite del enemigo en el buffer del SAT.
ret

;-----------------------------------------------------------------------------
; Funcion para controlar el movimiento tanto del enemigo basico como el avanzado
; IN:
; MODIFICA A, tiempo_Seg, IA_estado
;-----------------------------------------------------------------------------
IA_estadoMovimiento

	ld a, (iy+enemigo_var.tiempo_Seg)
	cp 0
	jr nz, _empezado2

		; El estado de movimiento acaba de empezar
		ld a, (tiempo)
		ld (iy+enemigo_var.tiempo_Seg), a 				; Almacenamos el segundo en el que empieza
		ret

	_empezado2:
	; Comprobamos cuanto lleva de movimiento
	ld a, (iy+enemigo_var.tiempo_Seg)
	call calcularTranscursoTiempo
	cp 6
	jr c, _no_terminado2 								; Si no se produce acarreo quiere decir que ha trasncurrido mas tiempo del que podia

		; Tiempo movimiento terminado
		ld a, 0
		ld (iy+enemigo_var.tiempo_Seg), a 				; Reseteamos el valor del tiempo de movimiento

		ld a, 0
		ld (iy+enemigo_var.IA_estado), a 				; Cambiamos la IA a estado de reposo
		ret

	_no_terminado2:

	call moverEnemigo 									; Movemos al enemigo

ret

;-----------------------------------------------------------------------------
; Funcion para mover al enemigo
; IN:
; MODIFICA: A, enemigo.x, BC, enemigo.dir
;-----------------------------------------------------------------------------
moverEnemigo:

	ld a, (ix+game_object.dir) 						; A = direcc a la que se movera el enemigo
	cp 0
	jr z, _izqd

		_derech:
		; Movimiento hacia la derecha
		ld a, 1
		ld (ix+game_object.dir), a 					; Indicamos que el enemigo va hacia la derecha
		ld a, (iy+enemigo_var.distancia_rec) 		; A = distancia maxima que puede recorrer el enemigo
		ld b, a
		ld a, (iy+enemigo_var.pos_x_inicial) 		; A = pos inicial donde aparece el enemigo
		add a, b 									; Obtenemos el limite derecho hasta donde puede correr el enemigo
		push af
		ld a, (ix+game_object.x) 					; A = pos x actual del enemigo					
		ld b, a
		pop af
		cp b 										; Comprobamos si el enemigo ya esta en el limite para correr a la derecha
		jr z, _izqd 								; Si esta en el limite entonces que se mueva a la izquierda

			; No esta en el limite
			ld a, (iy+enemigo_var.velocidad_mov) 	; A = vel movimiento enemigo
			ld c, a 
			ld a, b 								; A = pos x enemigo
			add a, c 								; A = pos x + vel mov
			ld (ix+game_object.x), a
	
			ret

	_izqd:
	; Movimiento hacia la izqd
	ld a, 0
	ld (ix+game_object.dir), a 						; Indicamos que el enemigo va hacia la izqd
	ld a, (iy+enemigo_var.distancia_rec) 			; A = distancia maxima que puede recorrer el enemigo
	ld b, a
	ld a, (iy+enemigo_var.pos_x_inicial) 
	sub b
	push af
	ld a, (ix+game_object.x)
	ld b, a
	pop af
	cp b
	jr z, _derech

		ld a, (iy+enemigo_var.velocidad_mov)
		ld c, a
		ld a, b
		sub c 										; A = pos x - vel mov
		ld (ix+game_object.x), a


ret


;-----------------------------------------------------------------------------
; Funcion para restar una vida al enemigo y comprobar si le quedan vidas o no
; IN:
; 		IX --> Puntero a datos enemigo
; OUT: 
; 		A --> Vidas restantes
; MODIFICA: A, enemigo.vida
;-----------------------------------------------------------------------------
checkVida_enemigos:

	ld a, (ix+game_object.vida)
	dec a 							; Vida --
	; Guardamos la vida que le queda
	ld (ix+game_object.vida), a

	call moverImpacto_Enemigos

ret


;-----------------------------------------------------------------------------
; Funcion para desplazar el enemigo cuando este recibe daño
; IN:
; 		IX --> Puntero a datos enemigo
; MODIFICA: A, enemigo.vida
;-----------------------------------------------------------------------------
moverImpacto_Enemigos:
	

	ld a, (posicionDisp) 			; 
	ld b, a 						; B = posicion X del jugador justo al disparar
	ld a, (ix+game_object.x) 		; A = pos x del enemigo
	cp b
	jr c, _dirDerec

		; Desplazamiento enemigo hacia derecha
		ld b, a
		ld a, 5 
		add a, b
		ld (ix+game_object.x), a
		ret

	_dirDerec:
	; Desplazamiento izquierda
	ld a, 5
	ld b, a
	ld a, (ix+game_object.x) 
	sub b
	ld (ix+game_object.x), a


ret

;-----------------------------------------------------------------------------
; Funcion para cargar  en el buffer el sprite del enemigo basico.
; IN:
; MODIFICA: HL, DE
;-----------------------------------------------------------------------------
dibujarEnemigo:

	; Copiamos el sprite del ENEMIGO al buffer del SAT
	ld l, (ix+game_object.PH) 							
	ld h, (ix+game_object.PH+1)     					
	ld e, (ix+game_object.PV) 							
	ld d, (ix+game_object.PV+1)    							
	call updateBufferSAT  			; Actualizamos el buffer del SAT con la pos X e Y del sprite a dibujar.

ret



.ends