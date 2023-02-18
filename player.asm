;----------------------------------------------------
;----------------------------------------------------	
; FICHERO CON TODO LO RELACIONADO CON EL SPRITE 
; CONTROLADO POR EL JUGADOR
;----------------------------------------------------	
;----------------------------------------------------

.include "cabecera.h.s" 


.ramsection "Variables player" slot 3	 			; Es en la ranura 2 donde tenemos el espacio de ram.	
	
	entrada db  				  					; Para controlar lo que introduce el jugador por teclado/controlador.
	jump: db 				  	  					; Para saber si el player esta saltando o no. 0 --> salto terminado /-1 --> Fin salto subir /!0 && !-1 --> Saltando
	subiendo: db 									; Indica que el jugador esta subiendo o no
	disparando: db 				  					; Para saber si esta disparando o no (0-->No disparando/ 1-->disparando)
	caida_idx: db 				  					; Indice para la tabla de caida del player
	tiempoDisparo: db 			  					; Tiempo que ha pasado desde que se ha disparado
	primVezDisparo: db 				  				; primera vez que se dispara (0 --> No primera vez/ 1 --> primera vez)
	invulnerabilidad: db 							; Valor que indica el tiempo de invulnerabilidad del jugador tras recibir daño
	primVezDmg: db 									; Primera vez que recibe daño el player 
	llave db 										; Para saber la llave que tiene
	cambioSprite: db
	invulnerable: db 								; Indica si el jugador esta en estado invulnerable o no (0--> No/1--> Si)
	parpadeo: db 									; Variable para saber cuando borrar o dibujar durante la invulnerabilidad
	posicionDisp: db 								; Posicion X del personaje cuando se dispara

.ends


.section "TABLAS DE VALORES" FREE

; Valores para hacer el salto del player
tablaSalto: 

	; TIPO SALTO 1
	;.db 0, -2, -4, -4   							; SUBIENDO RAPIDO
	;.db -6, -6, -8, -8   							; SUBIENDO RAPIDO
	;.db -1, 00, 00, $80  							; FIN SUBIENDO

	; TIPO SALTO 2
	;.db -8, -7, -6, -5
	;.db -4, -3, -2, -1 
	;.db 0, 0, 0

	; TIPO SALTO 3
	.db -8, -7, -6, -5
	.db -4, -2, -2,-1
	.db -1, 0, 0, 0
	.db $80 

; Valores para simular la caida del player (ya sea al saltar o simplemente caer de una plataforma)
tablaCaida: 
	.db 00, 01, 02
	.db 02, 03, 04  								; Cayendo

.ends


.section "FUNCIONES PLAYER" FREE

;-----------------------------------------------------------------------------
; Funcion para inicializar todos los datos del player 
; IN: 
; MODIFICA: A, jump, subiendo, resultado, disparando, tiempoDisparo, caida_idx
;-----------------------------------------------------------------------------
initPlayer:
	
	
	ld a, 0
	ld (jump), a 				; Jump = 0 = No salto

	ld (subiendo), a 			

	ld (disparando), a

	ld (tiempoDisparo), a

	ld (invulnerabilidad), a

	ld (llave), a

	ld a, 1 					;	
	ld (caida_idx), a  			; Inicialmente empezamos en la pos 1 de la tabla

	ld (primVezDisparo), a
	ld (primVezDmg), a

 ret

;-----------------------------------------------------------------------------
; Funcion para actualizar todo lo relacionado con el personaje controlable por 
; el jugador: Mover derecha/izqd
; IN: 
; MODIFICA: 
;-----------------------------------------------------------------------------
playerUpdate:
	
	ld a, (player.dir)
	cp player_derecha
	jr nz, _izqdSpr

		ld hl, playerccs 			; Sprite mirando a la derecha
		jr _nextSpr

	_izqdSpr:

	ld hl, playerIccs 				; Sprite mirando a la izquierda

	_nextSpr:
	ld de, playercc   				; DE = direcc en el buffer donde se va a copiar el primer indice de tile del sprite del player.
	ld bc, 9 	   					; Numero total de charcodes a copiar en el buffer.
	call cargarSpriteBuffer 		; Cargamos el sprite del player en el buffer del SAT.

	call checkCaida 				; Para comprobar si el jugador se ha caido fuera del mapa
	call checkVidas 				; Comprobamos la vida del player
	call resetInvulnerabilidad 		; Se resetea el indicador de invulnerabilidad en caso de superar el tiempo 

	ld a, (numEnemAv)
	cp 0
	jr z, _n

		call checkColBala 			; Comprobamos si una bala de un enemigo le ha dado
	_n:

	call jumpControl 				; Control del salto del player
	call getInput 					; Comprobamos que tecla/controlador esta pulsando el jugador
	call checkSalto 				; Comprobamos si quiere saltar o no.
	call checkMover 				; Comprobamos si se quiere mover a la izquierda o a la derecha.
	call checkDisparo 				; Comprobamos si quiere disparo el jugador o no

	ld a, (jump)
	cp 0 							;
	jr nz, _saltando 				; Comprobamos si se esta saltando o no

			; No saltamos.
			call checkGravedad 		; Comprobamos si el prota se cae para abajo o no

	_saltando: ; Estamos saltando

	ld a, (subiendo)
	cp -1 							;
	jr nz, _subiendo 				; Comprobamos si se ha terminado de subir en el salto o no

		; Terminado de subir el salto
		call checkGravedad 

	_subiendo:
	; No terminado de subir

	  
 ret

;-----------------------------------------------------------------------------
; Función para leer el input del teclado del jugador para saber que boton/tecla
; ha pulsado.
; IN: 
; MODIFICA: A, entrada
;-----------------------------------------------------------------------------
getInput:

	  in a, portJostyck 			; Leemos del puerto $dc para obtner el input del jugador
	  ld (entrada), a 				; Input = A
 ret

;-----------------------------------------------------------------------------
; Funcion que comprueba si el jugador se quiere mover a la derecha o la izquierda
; siempre teniendo en cuenta que puede moverse en alguna de las dos direcc
; IN: 
; 	  entrada --> Input del jugador por teclado indicando la accion a realizar
; MODIFICA: A, HL
;-----------------------------------------------------------------------------
checkMover:

	; MOVIMIENTO DERECHA
	ld a, (entrada)  				; A = Input del jugador.
	bit 3, a 						; Comprueba el bit 3 del registro A para ver si se quiere mover a la derecha.
	jr nz, checkIzquierda   		; Si el bit 3 esta a 0 quiere decir que nos queremos mover a la derecha, sino, comprobamos si se quiere mover a la izqd.

		; Bit 3 a 0, movimiento del sprite a la derecha
		ld a, col_derecha 				;
		ld c, a  						; C = indicamos que queremos detectar colision de
		ld ix, player
		call obtenerValorTilemap

		ld b, (iy+level.tile_suelo)
		cp b						;
		jr z, checkIzquierda 			; Si a la derecha hay una pared no nos podemos mover
			
			; Mas comprobaciones de colisiones
			ld b, (iy+level.tile_pared)
			cp b	
			jr z, checkIzquierda

				ld b, (iy+level.tile_paredS)
				cp b	
				jr z, checkIzquierda

			; No estamos en el limite de la derecha, nos podemos mover a la derecha.

			call moverDerecha 							; Movemos el sprite a la derecha
			jp finCheck 								; Terminamos la comprobacion de teclado.

;MOVIMIENTO IZQUIERDA
checkIzquierda: 
	
	ld a, (entrada) 	 				; A = entrada de teclado jugador.
	bit 2, a 							; Comprobamos si el bit 2 del registro A esta a 0, indica mov izquierda.
	jr nz, finCheck 					; Si no es z terminamos las comprobaciones

		; Bit 2 a 0. Nos queremos mover a la izquierda.
		ld a, col_izqd 					;
		ld c, a  						; C = indicamos que queremos detectar colision de
		ld ix, player
		call obtenerValorTilemap

		ld b, (iy+level.tile_suelo)
		cp b							;
		jr z, finCheck 					; Si a la izquierda hay una pared no nos podemos mover

			ld b, (iy+level.tile_pared)
			cp b
			jr z, finCheck

				ld b, (iy+level.tile_paredS)
				cp b
				jr z, finCheck

			call moverIzqd 								; Movemos el sprite a la izqda


finCheck: ;Ya no hay mas comprobaciones.



ret


;--------------------------------------------------------------------------------
; Función para mover el sprite hacia la derecha.
; IN: 
; MODIFICA:  A, player_x, player_dir
;-----------------------------------------------------------------------------
moverDerecha:
	
	ld a, (player.x) 						; A = Pos x del sprite
	add a, velMovPlayer 					; A + 3. Velocidad de movimiento del sprite
	ld (player.x), a 						; player_x = Nueva pos a la derecha del objeto 

	ld a, player_derecha 					;
	ld (player.dir), a 						; player_dir = 1. Indicamos que el player esta mirando a la derecha.

 ret

;--------------------------------------------------------------------------------
; Función para mover el sprite hacia la izquierda.
; IN: 
; MODIFICA:  A, player_x, B, player_dir
;-----------------------------------------------------------------------------
moverIzqd:
	
	ld a, (player.x) 			; A = Pos x del sprite
	ld b, velMovPlayer 			; B = velocida de movimiento
	sub b 						; A = A -B. Restamos la velocidad.
	ld (player.x), a 			; player_x = Nueva pos a la izquierda del objeto

	ld a, player_izquierda 		;
	ld (player.dir), a 			; player_dir = 0. Indicamos que el player esta mirando a la izquierda.
 ret


;--------------------------------------------------------------------------------
; Función para controlar el salto del player y saber en que fase del salto se encuentra
; si esta subiendo, bajando o no esta saltando
; IN: 
; 	   
; MODIFICA: HL, A, BC, player_y, JUMP, subiendo
;-----------------------------------------------------------------------------
jumpControl:

	ld hl, tablaSalto 			; Direcc donde se encuentra la tabla para el salto

	; Comprobamos si el player esta saltando o no
	ld a, (jump) 				; A = Valor que indica si esta saltando o no
	cp player_no_salto
	ret z						; Si el valor en A=0 entonces no estamos saltando y terminamos el control del salto

		;A!=0 por lo tanto estamos saltando.
		ld c, a  				; C = A = valor d
		ld b, 0
		add hl, bc 				; HL = HL + BC. Avanzamos a la posicion de la tabla de salto donde se encuentra el player

		ld a, (hl) 				; A = Valor de la tabla de salto
		cp $80
		jr z, fin_control 		; Si en A esta el valor 80 entonces el salto ya ha terminado.

			; Si no esta el valor 80, entonces aun estamos saltando
			ld b, a 			; B = indice de la tabla de salto
			ld a, (player.y) 	; Almacenamos la coord Y del player
			add a, b 			; A = A+B. Movemos la y del player en funcion del valor de la tabla de salto
			ld (player.y), a 	; Actualizamos la y del player

			ld a, (jump)  		; A = valor del salto a sumar a la tabla
			inc a 				; A ++ 
			ld (jump), a 		; jump = A

			; CARGAMOS LOS CC DEL SALTO
			ld a, (player.dir)
			cp player_izquierda
			jr z, _izqd

				; Derecha
				ld hl, playerSccs
				jr _cargarCC

			_izqd:
			ld hl, playerSIccs

			_cargarCC:
			ld de, playercc   							; DE = direcc en el buffer donde se va a copiar el primer indice de tile del sprite del player.
		    ld bc, 9 	   								; Numero total de charcodes a copiar en el buffer.
			call cargarSpriteBuffer 					; Cargamos el sprite del player en el buffer del SAT.

			ret 

	fin_control:  ; Terminado el control de salto.

	ld a, -1				;
	ld (subiendo), a 			; Jump = A, para indicar que la subida del salto ha terminado pero aun hay que caer

ret



;--------------------------------------------------------------------------------
; FUncion para comprobar si el usuario quiere saltar o no
; IN: 
; 	   entrada --> Byte con infor del puerto DC 
; MODIFICA: A
;-----------------------------------------------------------------------------
checkSalto:

	ld a, (entrada) 		; A = entrada = byte con info del puerto DC	
	bit 0, a 				; Comprobamos el bit 0 del registro A que es el que contiene la info de si se quiere saltar o no
	ret nz 

		;El usuario quiere saltar
		call startJump  	;Funcion para iniciar el salto en caso de que no este saltando ya.
ret

;--------------------------------------------------------------------------------
; FUncion para comprobar si el usuario quiere disparar o no
; IN: 
; 	   entrada --> Byte con infor del puerto DC 
; MODIFICA: A, bala_state
;-----------------------------------------------------------------------------
checkDisparo:

	ld a, (entrada) 					; A = entrada = byte con info del puerto DC	
	bit 4, a 							; Comprobamos el bit 4 del registro A que es el que contiene la info de si se quiere disparar o no
	jr nz, _no_disparo 

		;DISPARANDO
		ld a, (balaInfo.fin)
		cp bala_finalizado   			;
		jr nz, _no_disparo 				; 

			ld a, (tiempoDisparo) 		; A = tiempo que ha pasado desde que disparamos la ultima vez
			cp 0  						
			jr nz, _calcularTiempo 		; Si A !=0 quiere decir que ya hemos disparado y por lo tanto hay que ver cuanto tiempo ha pasado

				; tiempoDisparo = 0. Primer disparo?
				ld a, (primVezDisparo)
				cp 1
				jr z, _disparo 			; Si A == 1 quiere decir que es la primera vez que se dispara.

			_calcularTiempo:

			; Calculamos el tiempo que ha pasado desde que se disparo
			ld a, (tiempoDisparo)
			call calcularTranscursoTiempo
			cp 1 						;
			jr c, _no_disparo 			; Si transcurso de tiempo > tiempo a esperar (1 seg) --> Disparamos la siguiente bala

				_disparo:
				ld a, player_disparando ; 
				ld (disparando), a 		; activamos el disparando

				ld a, (tiempo) 			; 
				ld (tiempoDisparo), a 	; Registramos el segundo exacto cuando se produce el disparo

				ld a, (player.x) 		;
				ld (posicionDisp), a 	; Se guarda la pos X del personaje justo cuando dispara para saber la direcc de la bala
				ret

	_no_disparo:  					; No estamos disparando (etiqueta local con el _ delante)

	ld a, player_no_disparando
	ld (disparando), a 				; El player ha dejado de pulsar el boton de disparo 

ret


;--------------------------------------------------------------------------------
; FUncion para empezar el salto si no está empezado ya
; IN: 
; 	   entrada --> Byte con infor del puerto DC 
; MODIFICA: A
;-----------------------------------------------------------------------------
startJump:
	

	ld a, (jump)  			; A = jump.
	cp player_no_salto
	ret nz  				; Si A!=0 entonces ya estamos saltando y no podemos saltar hasta que termine el salto anterior

		ld a, player_salto 	;
		ld (jump), a    	; Activamos el salto del player.
ret

;--------------------------------------------------------------------------------
; FUncion para comprobar si el player se cae o no, en funcion de si esta en el suelo
; o no.
; IN: 
; 	   
; MODIFICA: A, BC, IX, HL, caida_idx
;-----------------------------------------------------------------------------
checkGravedad:

	ld a, col_caida 			;
	ld c, a  					; C = indicamos que queremos detectar colision de caida con el mapa

	ld ix, player
	call obtenerValorTilemap

	; Registro A contendra el valor del tile
	ld b, (iy+level.tile_suelo)
	cp b				;
	jr z, colision_suelo 		; Se comprueba el valor del tile con el valor que indica que es suelo, si coinciden estamos en el suelo

		ld b, (iy+level.tile_pared)
		cp b
		jr z, colision_suelo 	; Segundo valor de suelo que puede colisionar

		ld b, (iy+level.tile_paredS)
		cp b
		jr z, colision_suelo 	; Segundo valor de suelo que puede colisionar

			; Cayendo
			ld hl, tablaCaida 		; Direcc tabla de caida del player

			ld a, (caida_idx)
			ld c, a  				; C = A = indice de la tabla
			ld b, 0
			add hl, bc 				; HL = HL + BC. Avanzamos a la posicion de la tabla de caida del player

			ld a, (hl) 				; A = valor de la tabla de caida
			cp 04 					; 
			jr z, ult_valor 		; Si A==04, es el ultimo valor y usaremos este hasta que se termine la caida

				; No estamos en el ultimo valor de la tabla
				push af
				ld a, (caida_idx)
				inc a 				;
				ld (caida_idx), a  	; Siguiente posicion en la tabla
				pop af

			ult_valor:
			; No hace falta avanzar al siguiente.
			ld b, a 			; B = indice de la tabla de salto
			ld a, (player.y) 	; Almacenamos la coord Y del player
			add a, b 			; A = A+B. Movemos la y del player en funcion del valor de la tabla de caida
			ld (player.y), a 	; Actualizamos la y del player


			; CARGAMOS LOS CC DE LA CAIDA
			ld a, (player.dir)
			cp player_izquierda
			jr z, _izqdC

				; Derecha
				ld hl, playerCccs
				jr _cargarCCC

			_izqdC:
			ld hl, playerCIccs

			_cargarCCC:
			ld de, playercc   							; DE = direcc en el buffer donde se va a copiar el primer indice de tile del sprite del player.
		    ld bc, 9 	   								; Numero total de charcodes a copiar en el buffer.
			call cargarSpriteBuffer 					; Cargamos el sprite del player en el buffer del SAT.

			ret

	colision_suelo:

	ld a, 1
	ld (caida_idx), a 			; Volvemos al principio de la tabla para la proxima caida

	ld a, player_no_salto 		;
	ld (jump), a 				; Jump = A, para indicar que el salto ha terminado
	ld (subiendo), a 			; subiendo = A, para indicar que se ha terminado de subir

 
ret

;--------------------------------------------------------------------------------
; Funcion para comprobar si hay que restar una vida al player o no
; IN: 
; 	   
; MODIFICA: IX, IY, A, invulnerabilidad
;-----------------------------------------------------------------------------
checkVidas:

	ld a, (numEnemBas) 							; A = num enemigos basicos
	ld iy, enemigo 								; IY = datos primer enemigo
	cp 0
	jr z, _nada 								; Si no hay no hacemos nada

	_comprobarCol: ; Comprobamos si los enemigos hacen daño al jugador

		ld ix, player 							; Puntero datos player
		ld b, a 								; B = Contador enemigos
		ld de, 00
_next: 	add iy, de 								; Siguiente enemigo
		push bc
		
		call checkColision 						; Comprobamos colision entre el player y el enemigo

		ld a, (iy+game_object.id) 				; A = id del enemigo que estamos comprobando colision
		ld b, a 								
		ld a, (player.colision)
		cp b 									; Comprobamos si colisiona con el enemigo 
		jr nz, _no_colision 					; Si el valor devuelto en A coincide con el ident del enemigo entonces player.vida --

			; Colision con enemigo
			call checkInvulnerabilidad 			; Comprobamos si el player puede recibir daño o no

	_no_colision:

	ld de, _sizeof_game_object
	pop bc
	djnz _next 									; Volvemos a checkear si quedan enemigos (si B!=0)

	_nada:

	ld a, (iy+game_object.id) 					; A = id de los enemigos avanzados
	cp 4
	ret z 										; Si A coincide con el id de los avanzados, ya hemos hecho el bucle para estos enemigos

		ld a, (numEnemAv) 						; A = num de enemigos avanzados
		ld iy, enemigoAv 						; IY = primera entidad de enemigos avanzados
		cp 0
		jr nz, _comprobarCol 					; Comprobamos colision con estos enemigos si hay en el nivel


ret

;--------------------------------------------------------------------------------
; Funcion para comprobar si el jugador colisiona con la bala del enemigo
; IN: 
; 	   
; MODIFICA: 
;-----------------------------------------------------------------------------
checkColBala:
	
	ld ix, player 								;
	ld iy, bala.2 								; Las balas que pueden hacer daño estan a partir de la segunda
	ld a, 3 									; Numero de balas en total que hay
	ld b, a 
	ld de, 00

_sig: push bc 
	  add iy, de 	
	  ld a, (iy+game_object.estado)
	  cp 0
	  jr z, _muerta_bala

	  	; Bala activa
	  	call checkColision

	  	ld a, (player.colision)
	  	cp bala_identif
	  	call z, checkInvulnerabilidad 			; Comprobamos si el jugador puede recibir daño o no

	  _muerta_bala:
	  ld de, _sizeof_game_object
	  pop bc
	  djnz _sig 								; Bucle para recorrer todas las balas

ret


;--------------------------------------------------------------------------------
; Funcion para restar una vida al player. Si el numero de vidas es 0 se salta a 
; la direcc de memoria encargada de gestionar la muerte del mismo.
; IN: 
; 	  
; MODIFICA: A, player.vida, mundo_state
;-----------------------------------------------------------------------------
quitarVidaPLayer:
	
	ld a, 0
	ld (player.colision), a 		; Quitamos el indicador de colision en el player

	ld a, (player.vida)     		; 
	dec a 							; 
	ld (player.vida), a     		; Le restamos una vida al player

	cp 0 							; Si A == 0 quiere decir que el player esta muerto
	ret nz

		ld a, 2
		ld (mundo_state), a 		; Cambiamos al estado de muerte
		call checkEstadoMundo 		; Saltamos a la direcc donde se gestiona la muerte del jugador

ret

;--------------------------------------------------------------------------------
; Funcion que activa/desactiva la invulnerabilidad del player. Si estaba ya activa,
; comprueba si hay que desactivarla y restar vida al jugador. En caso contrario, 
; se activa la invulnerabilidad y ya esta.
; IN: 
; 	  
; MODIFICA: A, player.colision, invulnerabilidad
;-----------------------------------------------------------------------------
checkInvulnerabilidad:

	ld a, 0
	ld (player.colision), a

	ld a, (invulnerabilidad) 			; A = tiempo que ha pasado desde la ultima vez que recibio daño
	cp 0  						
	jr nz, _calcular 					; Si A !=0 quiere decir que ya ha recibido daño y por lo tanto hay que ver si ha pasado el tiempo de invulnerabilidad

		; invulnerabilidad = 0. Primera vez que se recibe daño?
		ld a, (primVezDmg)
		cp 1
		jr z, dmg 					; Si A == 1 quiere decir que es la primera vez que se recibe daño.

	_calcular:

	; Calculamos el tiempo que ha pasado desde que se recibio daño
	ld a, (invulnerabilidad)
	call calcularTranscursoTiempo
	cp time_invulnerabilidad 		;
	jr c, _no_tiempo 				; Si invulnerabilidad > tiempo a esperar --> Puede recibir daño otra vez

		dmg:  ; Una vida menos del player e invulnerabilidad durante x seg
					
		ld a, (tiempo) 				; 
		ld (invulnerabilidad), a 	; Registramos el segundo exacto cuando empieza la invulnerabilidad
		ld a, 1
		ld (invulnerable), a 		; Invulnerable = 1 (activada)
		
		call quitarVidaPLayer 		; Le quitamos una vida

	_no_tiempo:

ret

;--------------------------------------------------------------------------------
; Funcion para saber si el jugador esta en estado invulnerable o no
; IN: 
; 	  
; MODIFICA: 
;-----------------------------------------------------------------------------
resetInvulnerabilidad:
	
	ld a, (invulnerable)
	cp 0
	ret z 							; Si la invulnerabilidad no esta activa salimos.

	; Invulnerabilidad activa
	ld a, (invulnerabilidad)
	call calcularTranscursoTiempo
	cp time_invulnerabilidad 							;
	jr c, _no_tiempo2 				; Si invulnerabilidad > tiempo a esperar --> Puede recibir daño otra vez

		ld a, 0
		ld (invulnerable), a

	_no_tiempo2:



ret

;--------------------------------------------------------------------------------
; Funcion para comprobar si el jugador se ha salido fuera del mapa, en caso 
; afirmativo, este es eliminado y se inicia la pantalla de muerte.
; IN: 
; 	  
; MODIFICA: A, mundo_state
;-----------------------------------------------------------------------------
checkCaida:
	
	; Se comprueba que la posicion y del player esta dentro de un rango. Fuera de ese rango no se consdira muerte por caida
	ld a, (player.y)
	cp 170
	jr c, _no_caida 	

		cp 180
		jr nc, _no_caida

		; El jugador se ha caido fuera del mapa. Se le resta una vida
		ld a, (player.vida)
		dec a
		ld (player.vida), a
		cp 0
		jr nz, _reinicioLevel

			; FIN JUEGO
			ld a, 2
			ld (mundo_state), a 		; Cambiamos al estado de muerte
			call checkEstadoMundo 		; Saltamos a la direcc donde se gestiona la muerte del jugador

			ret

		_reinicioLevel:
		; AUN QUEDA VIDAS. SE REINICIA EL LEVEL
			ld a, 1 						;
			ld (cambioNivel), a 			; Indicamos que queremos hacer un cambio de nivel
			ld a, 0
			ld (llave), a 					; Llave usada.



	_no_caida:


ret

;--------------------------------------------------------------------------------
; Funcion para copiar todo lo necesario del player al buffer del SAT y conseguir 
; asi dibujarlo correctamente en pantalla
; IN: 
; 	  
; MODIFICA: IX, HL, DE, BC
;-----------------------------------------------------------------------------
dibujadoPlayer:

	ld ix, player

	; Proceso para copiar el sprite del PLAYER al buffer del SAT
	ld hl, playerccs 								; HL = direcc donde estan todos los indices de tile del sprite del player que queremos dibujar.
	ld de, playercc   								; DE = direcc en el buffer donde se va a copiar el primer indice de tile del sprite del player.
	ld bc, 9 	   									; Numero total de charcodes a copiar en el buffer.
	call cargarSpriteBuffer 						; Cargamos el sprite del player en el buffer del SAT.

	ld hl, playerPH 								; HL = Direcc del SAT donde esta situado horizontalmente el objeto
	ld de, playerPV 								; DE = Direcc del SAT donde esta situado verticalmente el objeto
	call updateBufferSAT  							; Actualizamos el buffer del SAT con la pos X e Y del sprite a dibujar.


ret

.ends