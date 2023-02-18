;----------------------------------------------------
;----------------------------------------------------	
; FICHERO CON TODOS LAS DATOS Y FUNCIONES DEL DISPARO
;----------------------------------------------------	
;----------------------------------------------------

.include "cabecera.h.s" 

.ramsection "Variables bala" slot 3	 ; Es en la ranura 2 donde tenemos el espacio de ram.
	
	balaInfo instanceof bala_variables 3

.ends


.section "FUNCIONES BALA" FREE

;-----------------------------------------------------------------------------
; Funcion para inicializar todos los datos de la bala 
; IN: 
; MODIFICA: A, balaInfo.fin, balaInfo.tiempoBala
;-----------------------------------------------------------------------------
initBala:
	
	ld a, bala_finalizado 			;
	ld (balaInfo.fin), a 			; Inicialmente la bala ha finalizado su recorrido.
	ld (balaInfo.tiempoBala), a 
	ld (balaInfo.2.fin), a 	
	ld (balaInfo.2.tiempoBala), a 	
	ld (balaInfo.3.fin), a 	
	ld (balaInfo.3.tiempoBala), a 		

	  
 ret


;-----------------------------------------------------------------------------
; Funcion para actualizar todo lo relacionado con la bala en el caso de que esta exista.
; IN: 
; MODIFICA:  A, bala.dir, B, bala.fin, bala.colision, bala.tiempoBala
;-----------------------------------------------------------------------------
balaUpdate:
	
	call checkEstadoBala 									; Funcion para comprobar el estado de la bala (si ha colisionado o no/ si esta activa o no)

	; 1º Comprobamos si la bala existe para saber si hay que hacer algo con ella o no
	ld a, (bala.estado) 							; A = estado de la bala
	cp ent_muerta
	jr z, _no_update 										; Si A = 0, bala no activa

		; 2º Comprobamos si ha colisionado o no para saber si hay que borrarla o seguir moviendola.
		ld a, (bala.colision) 								; A = valor que indica si ha colisionado o no.
		cp ent_no_colision 									;
		jr nz, _colision  									; Siempre que A!=0 querra decir que ha colisionado, nos da igual con que, hay que borrar la bala.

			; Bala no ha colisionado. 
			ld a, (balaInfo.tiempoBala)
			call calcularTranscursoTiempo
			cp 2  											;
			jr c, no_terminado 								; Comprobamos si ha pasado mas de 1 segundo desde que se disparo

				; Ha pasado mas del tiempo disponible, hay que desactivarla
				ld a, bala_finalizado
				ld (balaInfo.fin), a 				; Indicamos que la bala ha finalizado su recorrido

				ld a, ent_colision
				ld (bala.colision), a

				ld (balaInfo.tiempoBala), a 		; Reseteamos el valor del tiempoBala para que se pueda volver a disparar

				ret 
				
			no_terminado:  ; Aun no ha superado el tiempo maximo 

			; 3º Comprobamos si es porque acaba de ser disparada o porque aun no ha chocado con nada en su recorrido.
			ld a, (balaInfo.fin) 					; A = valor que indica si la bala ha llegado al final
			cp bala_finalizado  							;
			jr nz, _cont  									; Si A = 1, entonces quiere decir que la bala aun no ha llegado al final


			_cont: ; LA BALA AUN NO HA TERMINADO, HACEMOS QUE SIGA MOVIENDOSE
			call moverBala 									; Función para mover la bala al disparar
			ret

		_colision: ; Bala ha colisionado, por lo tanto hay que destruirla

		ld a, (bala.PV) 
		ld l, a												; 
		ld a, (bala.PV+1)
		ld h, a     										; HL = Primera pos del buffer para las posiciones horizontales del sprite
		ld a, (bala.PH)
		ld e, a 											;		 
		ld a, (bala.PH+1) 
		ld d, a    								; DE = Primera pos del buffer para las posiciones verticales del sprite
		call borrarEntidad	 								; Borramos la bala del SAT y reseteamos sus variables

		; Reseteamos sus variables. A = 0.
		ld (balaInfo.fin), a  							; Recorrido finalizado
		ld (balaInfo.tiempoBala), a  						; Tiempo activa = 0.

	_no_update:
	; BALA NO EXISTE. NO HACEMOS NADA

	  
 ret

;-----------------------------------------------------------------------------
; Funcion para comprobar el estado de la bala. Se comprueba si el jugador quiere
; disparar o no y si la bala ya ha finalizado su recorrido o no. Si se esta dis-
; parando y ademas se ha finalizado el recorrido, se resetea la pos de la bala 
; a la pos de inicio de disparo en funcion de la dir a la que mira el player, 
; indicando ademas de que esta viva y que no ha colisionado.
; Si no se esta disparando y ha finalizado la bala su recorrido, entonces se 
; le indica que ha colisionado.
; IN: 
; 		disparando --> Info sobre si el player esta disparando o no
; MODIFICA: A, bala.fin, bala.colision
;-----------------------------------------------------------------------------
checkEstadoBala:
	
	ld a, (disparando) 								; A = variable que indica si el player esta disparando o no
	cp 0 											;
	jr z, _no_disparando 							; Si A=0, entonces el player no esta disparando

		; PLAYER DISPARANDO. 
		; Comprobamos si ha llegado al final ya, para poder disparar otra vez
		ld a, (balaInfo.fin) 				; A = valor que indica si la bala ha llegado al final
		cp bala_no_finalizado  						;
		ret z   									; Si A = 1, entonces quiere decir que la bala aun no ha llegado al final

			 call resetBalaPlayer	 				; Reseteamos el game_object de la bala

			ret

	_no_disparando:  ; PLAYER NO DISPARANDO

	; Comprobamos si ha llegado al final, para poder desactivar la bala
	ld a, (balaInfo.fin) 							; A = valor que indica si la bala ha llegado al final
	cp bala_no_finalizado  							;
	ret z   										; Si A = 1, entonces quiere decir que la bala aun no ha llegado al final

		; RECORRIDO FINALIZADO.
		ld a, ent_colision							;
		ld (bala.colision), a 						; bala_colision = 1, la bala ha colisionado (en este caso con la pared)

ret

;-----------------------------------------------------------------------------
; Funcion para resetear las variables de la bala y su posicion a la posicion 
; x e y donde se encuentra el player.
; MODIFICA: A, , tiempoBala, colision bala, estado bala
;-----------------------------------------------------------------------------
resetBalaPlayer:
	
	; RESETEAMOS VARIABLES BALA
	ld a, ent_no_colision					;
	ld (bala.colision), a 					; bala_colision = 0, indicando que la bala no ha colisionado

	ld a, ent_viva
	ld (bala.estado), a 					; bala_state = 1, ademas indicamos que la bala existe.

	ld a, (tiempo) 							; 			
	ld (balaInfo.tiempoBala), a 			; Guardamos el segundo exacto en el que se dispara la bala

	; RESETEAMOS POS DE LA BALA
	;La bala ha llegado al final, por lo tanto la volvemos a activar y poner en la pos correcta.
	ld a, (player.y) 						;
	add a, 7
	ld (bala.y), a  						; Cambiamos la y de la bala 

	ld a, (player.x) 						; A = pos actual del player
	push af 								; Guardamos la pos del player en la pila

	ld a, (player.dir) 						;
	ld (bala.dir), a  						; La direcc a la que mira el player es a la que ira la bala.
	cp bala_izquierda
	jr z, izqd

		; BALA A LA DERECHA
		pop af 								; A = pos x del player
		add a, 20 							; A + B (20)
		ld (bala.x), a 						; Reseteamos la pos de la bala un poco mas adelante del player
		ret 

	izqd:  ; BALA A LA IZQUIERDA

	pop af  								; A = pos x del player
	ld b, 20  				
	sub b 									; A - B(20)
	ld (bala.x), a 	


ret

;-----------------------------------------------------------------------------
; Funcion para mover la bala al disparar el player, en funcion de la direcc a 
; la que mira, comprobando a su vez si colisiona con un elemento del mapa
; IN: 
; 		bala_width --> ancho del sprite de la bala
; 		bala_dir --> direcc a la que va ir la bala en funcion de donde mira el player
; MODIFICA: A, HL, bala.x, balaInfo.fin, 
;-----------------------------------------------------------------------------
moverBala:
	
	ld a, (bala.dir) 				; A = direcc a la que ira la bala
	cp bala_izquierda 						;
	jr z, move_izq 							; Si A=0, quiere movimiento izqd, si no, movi derech

		;Comprobamos si a la derecha la bala se puede mover
		ld a, col_derecha 					;
		ld c, a  							; C = indicamos que queremos detectar colision de columna a la derecha
		call obtenerValorTilemap 			; Obtenemos el valor del tilemap en el que se encuentra la bala

		ld b, (iy+level.tile_suelo)
		cp b								;
		jr z, end_bala						; Si a la derecha hay una pared no se puede mover
			
			ld b, (iy+level.tile_paredS)
			cp b
			jr z, end_bala

				ld b, (iy+level.tile_pared)
				cp b
				jr z, end_bala

				;Movimiento derecha
				ld a, bala_no_finalizado 		;
				ld (balaInfo.fin), a   ; balaInfo.fin = 1. Indicamos que la bala no ha finalizado su recorrido

		    	ld a, (ix+game_object.x) 		; A = pos x de la bala 
				add a, velMovBala  				; A + velMomieminto bala
				ld (ix+game_object.x), a 		; bala_x = nueva pos x

				; Indicamos que queremos dibujar el sprite derecho de la bala
				ld hl, balaccs  							; HL = tiles de la bala
				ld e, (ix+game_object.cc) 					; 
				ld d, (ix+game_object.cc+1)     			; DE = Primera pos del buffer para los indices de tile
				ld bc, 2									; BC = Numero total de indices de tile
				call cargarSpriteBuffer

			ret

	move_izq: ;Mover izqd

	;Comprobamos en el limite izquierdo del mapa
	ld a, col_izqd 							;
	ld c, a  								; C = indicamos que queremos detectar colision de
	call obtenerValorTilemap

	ld b, (iy+level.tile_suelo)
	cp b									;
	jr z, end_bala 							; Si a la izquierda hay una pared no nos podemos mover

			ld b, (iy+level.tile_pared)
			cp b
			jr z, end_bala

				ld b, (iy+level.tile_paredS)
				cp b
				jr z, end_bala

				; MOVIMIENTO IZQUIERDA
				ld a, bala_no_finalizado  		;
				ld (balaInfo.fin), a   ; balaInfo.fin = 1. Indicamos que la bala no ha finalizado su recorrido

				ld a, (bala.x)  				; A = pos x de la bala
				ld b, velMovBala 				; B = vel movimiento bala
				sub b 							; A = A - B
				ld (bala.x), a 		

				; Indicamos que queremos dibujar el sprite izquierdo de la bala
				ld hl, balaIzqdccs  						; HL = tiles de la bala izquierda
				ld e, (ix+game_object.cc) 					; 
				ld d, (ix+game_object.cc+1)     			; DE = Primera pos del buffer para los indices de tile
				ld bc, 2									; BC = Numero total de indices de tile
				call cargarSpriteBuffer
			ret

	end_bala:	

	ld a, bala_finalizado  					;
	ld (balaInfo.fin), a    		; balaInfo.fin = 0. la bala ha terminado su recorrido
	ld (balaInfo.tiempoBala), a

ret

.ends