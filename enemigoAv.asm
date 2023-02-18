;=========================================================
;
; FICHERO CON TODOS LAS DATOS Y FUNCIONES DEL ENEMIGO AVANZADO
;
;=========================================================
.include "cabecera.h.s"


.ramsection "Variables enemigoAV" slot 3	 ; Es en la ranura 2 donde tenemos el espacio de ram.
	
	enemigoAvInf instanceof enemigo_var 2
	enemigoPosY db 							; Posicion Y del enemigo
	enemigoPosX db 							; Posicion X del enemigo

.ends


.section "FUNCIONES ENEMIGOAV" free


 ;-----------------------------------------------------------------------------
; Funcion para actualizar todo lo relacionado con el enemigo avanzado.
; IN: 
; 		IX --> puntero datos game_object enemigo
; 		IY --> Puntero datos variables extra enemigo
; MODIFICA:  A, HL, DE, BC
;-----------------------------------------------------------------------------
enemigoAV_update:
	
	call checkEstadoEnemigo 				; Para saber si el enemigo ha colisionado con algo o no etc

	ld a, (ix+game_object.estado)
	cp ent_muerta 							;
	jr z, _muerto 							; Si A == 0 entonces el enemigo esta muerto

		; Enemigo vivo
		ld a, (iy+enemigo_var.IA_estado)
		cp 1
		jr z, _seg_estado 					; Comprobamos en que estado esta la IA del enemigo

			; Estado atacando
			call IA_ataque 					; Estado de ataque del enemigo

			jr _dibujado

		_seg_estado:

		; Estado movimiento
		call IA_estadoMovimiento

		; DIBUJAMOS POR PANTALLA EL SPRITE CORRESPONDIENTE DE MOVIMIENTO
		ld hl, enemigAvanzccs								; Sprite de enemigo mirando a la derecha y moviendose

		ld a, (ix+game_object.dir)
		cp player_derecha 
		jr z, _cargarCCI 		 							; Comprobamos la direcc a la que mira				
			  
		; Hay que cargar los ccs del sprite moviendose a la izquierda
		ld hl, enemAvanzIzqdccs 							; Sprite de enemigo mirando a la izquierda y moviendose

		_cargarCCI:

		; Cargamos los ccs al buffer
		ld e, (ix+game_object.cc) 							;
		ld d, (ix+game_object.cc+1)	 						; DE = posicion primer charcode del sprite
		ld bc, 9 	   				
		call cargarSpriteBuffer 							; Cargamos el sprite del enemigo en el buffer del SAT.

		_dibujado:

		call dibujarEnemigo 								; Dibujamos al enemigo avanzado por pantalla

	_muerto:
	; Enemigo muerto. 

	call controlBalaEnemigo 				; Control de la bala en caso de que esta este activa (aunque el enemigo este muerto)

		
ret


;-----------------------------------------------------------------------------
; Funcion para hacer el ataque del enemigo avanzado
; IN: 
; MODIFICA:  A, HL, DE, BC
;-----------------------------------------------------------------------------
IA_ataque:
	
	; Segundos que el enemigo prepara el disparo y la direccion
	ld a, (iy+enemigo_var.tiempo_Pr)
	cp 0
	jr nz, _empezado

		; Este estado acaba de empezar
		ld a, (tiempo)
		ld (iy+enemigo_var.tiempo_Pr), a 				; Almacenamos el segundo en el que empieza
		ret

	_empezado:

	; Comprobamos cuanto lleva en este estado
	ld a, (iy+enemigo_var.tiempo_Pr)
	call calcularTranscursoTiempo
	cp 2
	jr c, _no_terminado

		; El enemigo tiene que disparar una bala en la direcc en la que esta el jugador
		ld a, (ix+game_object.y)
		ld (enemigoPosY), a
		ld a, (ix+game_object.x)
		ld (enemigoPosX), a

		ld a, 0
		ld (iy+enemigo_var.tiempo_Pr), a 				; Reseteamos el valor del tiempo de ataque

		ld a, 1
		ld (iy+enemigo_var.IA_estado), a 				; Cambiamos la IA a estado de movimiento

		call activarDisparo 							; Cuando el tiempo de preparacion de disparo a pasado, activamos el disparo


		ret



	_no_terminado:

ret


;-----------------------------------------------------------------------------
; Funcion para acceder a los datos de la bala que va a usar el enemigo y para
; preparar la bala para ser disparada.
; IN: 
; MODIFICA: IX, IY, BC, DE, 
;-----------------------------------------------------------------------------
activarDisparo:

	; Accedemos a la direcc donde estan los datos de la bala que va a usar el enemigo para 
	; disparar. 
	ld a, (iy+enemigo_var.bala_uso) 			 		; Bala que va a usar el enemigo 		
	push iy 											; Guardamos datos var extras enemigo

	ld iy, balaInfo
	ld b, a 											; Bala que va a usar el enemigo 
	ld de, 00
	push bc 											; Guardamos indicador de bala a usar
- 	add iy, de
	ld de, _sizeof_bala_variables											
	djnz - 												; Avanzamos hasta la zona de datos de dicha bala

	pop bc
	push ix 											; Guardamos la direcc a la que estabamos apuntando
	ld ix, bala 
	ld de, 00
-- 	add ix, de
	ld de, _sizeof_game_object											
	djnz -- 											; Avanzamos hasta la zona de datos de dicha bala


	; Ya estamos apuntando a los datos de la bala que queremos disparar.
	; Reseteamos la bala a su pos delante del enemigo y la preparamos para ser disparada
	call resetBala

	pop ix
	pop iy

ret

;-----------------------------------------------------------------------------
; Funcion para resetear la bala del enemigo
; IN: 
; 		IX --> Puntero datos game_object bala
; 		IY --> Puntero datos variables extra bala
; MODIFICA: 
;-----------------------------------------------------------------------------
resetBala:
	
	; RESETEAMOS VARIABLES BALA
	ld a, ent_no_colision						;
	ld (ix+game_object.colision), a 			; bala_colision = 0, indicando que la bala no ha colisionado

	ld a, ent_viva
	ld (ix+game_object.estado), a 				; bala_state = 1, ademas indicamos que la bala existe.

	ld a, (tiempo) 								; 			
	ld (iy+bala_variables.tiempoBala), a 		; Guardamos el segundo exacto en el que se dispara la bala

	; RESETEAMOS POS DE LA BALA
	ld a, (enemigoPosY)
	add a, 10
	ld (ix+game_object.y), a 					; La bala aparecera en la Y del enemigo 

	ld a, (player.x) 							; A = posicion x del jugador
	ld b, a 					
	ld a, (enemigoPosX) 				  		; A = posicion x del enemigo
	cp b 										;
	jr c, _derecha 								; Comparamos si la posicion del enemigo es menor o igual que la del player

		; Pos player menor o igual que la del enemigo. Bala direcc izquierda			
		sub 20 									; A -(20)
		ld (ix+game_object.x), a 	
		ld a, bala_izquierda
		ld (ix+game_object.dir), a 				; Indicamos direcc movimiento de la bala

		ret 


    _derecha:
    ; Pos player mayor que la del enemigo. Bala direcc Derecha
    add a, 20 									; A + B (20)
	ld (ix+game_object.x), a 					; Reseteamos la pos de la bala un poco mas adelante del player
	ld a, bala_derecha
	ld (ix+game_object.dir), a 					; Indicamos direcc movimiento de la bala
   
ret


;-----------------------------------------------------------------------------
; Funcion para controlar todo sobre la bala del enemigo: si ha colisionado,
; finalizado, moverla etc
; IN: 
; MODIFICA: 
;-----------------------------------------------------------------------------
controlBalaEnemigo:
	
	; Accedemos a la direcc donde estan los datos de la bala que va a usar el enemigo para 
	; disparar. 
	ld a, (iy+enemigo_var.bala_uso) 			 			; Bala que va a usar el enemigo 		
	push iy 												; Guardamos datos var extras enemigo

	ld iy, balaInfo
	ld b, a 												; Bala que va a usar el enemigo 
	ld de, 00
	push bc 												; Guardamos indicador de bala a usar
- 	add iy, de
	ld de, _sizeof_bala_variables											
	djnz - 													; Avanzamos hasta la zona de datos de dicha bala

	pop bc
	push ix 												; Guardamos la direcc a la que estabamos apuntando
	ld ix, bala 
	ld de, 00
-- 	add ix, de
	ld de, _sizeof_game_object											
	djnz -- 												; Avanzamos hasta la zona de datos de dicha bala

	; Trabajamos ahora con la info de la bala del enemigo

	; 1º Comprobamos si la bala existe para saber si hay que hacer algo con ella o no
	ld a, (ix+game_object.estado) 							; A = estado de la bala
	cp ent_muerta
	jr z, _no_update 										; Si A = 0, bala no activa

		; Bala activa
		; 2º Comprobamos si ha colisionado o no para saber si hay que borrarla o seguir moviendola.
		ld a, (ix+game_object.colision) 					; A = valor que indica si ha colisionado o no.
		cp ent_no_colision 									;
		jr nz, _colision  									; Siempre que A!=0 querra decir que ha colisionado, nos da igual con que, hay que borrar la bala.

			; Bala no ha colisionado
			; Checkeamos el tiempo que ha transcurrido desde que se disparo la bala
			ld a, (iy+bala_variables.tiempoBala)
			call calcularTranscursoTiempo
			cp 2  											;
			jr c, _no_terminado2 							; Comprobamos si ha pasado el tiempo suficiente desde que se disparo

				; Ha pasado mas del tiempo disponible, hay que desactivarla	
				ld a, 0
				ld (iy+bala_variables.tiempoBala), a 		; Reseteamos el valor del tiempoBala para que se pueda volver a disparar

				jr _colision 								; Hay que borrarla

			_no_terminado2:

			; Aun no ha terminado la bala su recorrido. La movemos
			call moverBalaEnemigo 							; Función para mover la bala al disparar

			; Solo actualizamos sus pos verticales y horizontales, ya que el cc dependera del sprite a dibujar
			ld l, (ix+game_object.PH) 						; 
			ld h, (ix+game_object.PH+1)     				; HL = Primera pos del buffer para las posiciones horizontales del sprite
			ld e, (ix+game_object.PV) 						; 
			ld d, (ix+game_object.PV+1)     				; DE = Primera pos del buffer para las posiciones verticales del sprite
			call updateBufferSAT 							; Actualizamos los valores x e y de la bala en el buffer del SAT.

			jr _no_update


		_colision: ; Bala ha colisionado, por lo tanto hay que destruirla

		; Borramos la bala
		ld a, (ix+game_object.PV) 
		ld l, a												; 
		ld a, (ix+game_object.PV+1)
		ld h, a     										; HL = Primera pos del buffer para las posiciones horizontales del sprite
		ld a, (ix+game_object.PH)
		ld e, a 											;		 
		ld a, (ix+game_object.PH+1) 
		ld d, a    											; DE = Primera pos del buffer para las posiciones verticales del sprite
		call borrarEntidad	 								; Borramos la bala del SAT y reseteamos sus variables

		; Reseteamos sus variables. A = 0.
		ld (iy+bala_variables.fin), a  						; Recorrido finalizado
		ld (iy+bala_variables.tiempoBala), a  				; Tiempo activa = 0.


	_no_update:
	; Bala no activa.

	
	pop ix
	pop iy 													; Recuperamos punteros a datos enemigo


ret


;-----------------------------------------------------------------------------
; Funcion para mover la bala a la direcc que apunta el enemigo y comprobar
; si esta colisiona con algun elemento del mapa
; IN: 
; MODIFICA: A, BC, bala.colision, bala_variables.fin, bala_variables.tiempoBala, bala.x
;-----------------------------------------------------------------------------
moverBalaEnemigo:

	ld a, (ix+game_object.dir) 					; A = direcc a la que ira la bala
	cp bala_izquierda 							;
	jr z, _move_izq 							; Si A=0, quiere movimiento izqd, si no, movi derech

		; Bala direcc derecha
		;Comprobamos si a la derecha la bala se puede mover
		ld a, col_derecha 						;
		ld c, a  								; C = indicamos que queremos detectar colision de columna a la derecha
		push iy 								; Guardamos puntero datos variables extra bala
		call obtenerValorTilemap 				; Obtenemos el valor del tilemap en el que se encuentra la bala

		; Colision con paredes/suelo
		ld b, (iy+level.tile_suelo)
		cp b								
		jr z, _end_bala						
			
			ld b, (iy+level.tile_paredS)
			cp b
			jr z, _end_bala

				ld b, (iy+level.tile_pared)
				cp b
				jr z, _end_bala
			
				;Movimiento derecha
				pop iy
				ld a, bala_no_finalizado 			;
				ld (iy+bala_variables.fin), a  	 	; balaInfo.fin = 1. Indicamos que la bala no ha finalizado su recorrido

		    	ld a, (ix+game_object.x) 			; A = pos x de la bala 
				add a, velMovBala  					; A + velMomieminto bala
				ld (ix+game_object.x), a 			; bala_x = nueva pos x

				; Indicamos que queremos dibujar el sprite derecho de la bala
				ld hl, balaccs  							; HL = tiles de la bala
				ld e, (ix+game_object.cc) 					; 
				ld d, (ix+game_object.cc+1)     			; DE = Primera pos del buffer para los indices de tile
				ld bc, 2									; BC = Numero total de indices de tile
				call cargarSpriteBuffer

			ret

	_move_izq:
	; Bala direcc izqd

	;Comprobamos en el limite izquierdo del mapa
	ld a, col_izqd 								;
	ld c, a  									; C = indicamos que queremos detectar colision de
	push iy
	call obtenerValorTilemap

	; Colision con paredes/suelo
	ld b, (iy+level.tile_suelo)
	cp b								
	jr z, _end_bala						
			
		ld b, (iy+level.tile_paredS)
		cp b
		jr z, _end_bala

			ld b, (iy+level.tile_pared)
			cp b
			jr z, _end_bala

			; MOVIMIENTO IZQUIERDA
			pop iy
			ld a, bala_no_finalizado  				;
			ld (iy+bala_variables.fin), a   		; balaInfo.fin = 1. Indicamos que la bala no ha finalizado su recorrido

			ld a, (ix+game_object.x)  				; A = pos x de la bala
			ld b, velMovBala 						; B = vel movimiento bala
			sub b 									; A = A - B
			ld (ix+game_object.x), a 		

			; Indicamos que queremos dibujar el sprite izquierdo de la bala
			ld hl, balaIzqdccs  						; HL = tiles de la bala izquierda
			ld e, (ix+game_object.cc) 					; 
			ld d, (ix+game_object.cc+1)     			; DE = Primera pos del buffer para los indices de tile
			ld bc, 2									; BC = Numero total de indices de tile
			call cargarSpriteBuffer

		ret

	_end_bala:

	pop iy
	ld a, ent_colision
	ld (ix+game_object.colision), a 		; Colision de la bala con un elemento del mapa
	ld a, 0
	ld (iy+bala_variables.tiempoBala), a 	; Reset tiempo bala
	ld (iy+bala_variables.fin), a    		; balaInfo.fin = 0. la bala ha terminado su recorrido

ret



.ends