;----------------------------------------------------
;----------------------------------------------------	
; FICHERO QUE CONTIENE LAS FUNCIONES GENERALES QUE 
; NECESITAN TODOS LOS GAME_OBJECTS DEL JUEGO
;----------------------------------------------------	
;----------------------------------------------------

.include "cabecera.h.s"


.ramsection "CREACION GAME OBJECTS" slot 3
	
	player instanceof game_object 						; 1 instancia del player
	bala instanceof game_object 3 						
	enemigo instanceof game_object 2
	enemigoAv instanceof game_object 2					
	puerta instanceof game_object 1
	llaves instanceof game_object 1
	vidas instanceof game_object 1
	cristal instanceof game_object 3

.ends


.section "FUNCIONES GAME OBJECTS" free

;--------------------------------------------------------------------------------
; Funcion para cargar un sprite en el buffer del SAT
; IN:
; 	 HL --> Direcc donde estan todos los indices de tile (charcodes) del sprite a cargar
; 	 DE --> direcc en el buffer donde se va a copiar el primer indice de tile del sprite del player.
; 	 BC --> Numero total de charcodes a copiar en el buffer.
; MODIFICA: HL, BC, A, frame
;---------------------------------------------------------------------------------
cargarSpriteBuffer:

    call setSpritecc  			; Ponemos todos los índices de tile necesarios para que se dibuje correctamente el sprite en el buffer.

    ;xor a               		; ponemos A = 0.
    ;ld (frame),a        		; Reseteamos el contador de frames

    ld hl,finspr        		; hl = fin del sprite activo
    ld (hl),$d0         		; Indicamos que ya no hay mas sprites que dibujar.

ret

;-----------------------------------------------------------------------------
; Función para establecer todos los charcodes del sprite a dibujar
; IN: Hl = Puntero a todos los charcodes del sprite
; 	  DE = Puntero al primer charcode del sprite en el buffer
; 	  BC = Tam total de indices de tiles a pasar al buffer.
; MODIFICA: BC, HL, DE, A
;-----------------------------------------------------------------------------
setSpritecc:

	  ldi 				; (HL) --> (DE). BC --. HL ++(siguiente charcode). DE ++(siguiente pos en el buffer del SAT)
	  inc de 			; DE ++. Hay que incrementar una vez mas ya que si no estamos en una hpos del sprite y no en el siguiente cc.
	  ld a, c 			; A = C
	  or b  			; OR B con A. Dara 0 si ambos son 0, indicando que BC ha llegado a 0.
	  jr nz, setSpritecc   ; Mientras que BC no sea 0, seguimos pasando cc del sprite a dibujar
	
 ret
;-----------------------------------------------------------------------------
; Función para actualizar el buffer del SAT. Actualiza la pos X e Y del objeto.
; IN: 
; 	  HL --> Puntero a la direcc del SAT donde esta situado horizontalmente el objeto (hpos)
;  	  DE --> Puntero a la direcc del SAT donde esta situado verticalmente el objeto (vpos)
; 	  IX --> Puntero a los datos del game object que se quiere actualizar su pos
; MODIFICA: A
;-----------------------------------------------------------------------------
updateBufferSAT:

	  ;Pos Y del sprite del player a dibujar.
	  ld a, (ix+game_object.y)  ; A = Pos y del objeto
	  call updateSpriteY   		; Actualiza el buffer con el valor de Y

	  ld a, (ix+game_object.x)  ; A = Pos x del objeto.
	  call updateSpriteX   		; Actualiza el buffer con el valor de X

 ret

;-----------------------------------------------------------------------------
; Función para generar las posiciones verticales(vpos) en el buffer del SAT a partir 
; de la pos Y del sprite.
; IN: A --> coord Y del sprite (ObjY)
; 	  HL --> Direcc en el buffer de la vpos
; 	  IX --> Direcc donde estan los datos del sprite
; MODIFICA: A, BC, DE
;-----------------------------------------------------------------------------
updateSpriteY:
	;; Repetir para 4 filas del sprite
	;; Cada fila +8 píxeles
	ld c, (ix+game_object.h)      	; C = Alto del sprite. Numero de filas a añadir
spb4filas:
	   ;; Meter 1 fila
	   ld b, (ix+game_object.width)	  	; B = Ancho del sprite. Numero de columnas que tiene la fila
-	      ld  (de), a   			; Valor de vpos
	      inc  de 					; Siguiente vpos
	   djnz - 						; B --
	   add a, 8 					; Pasamos a la siguiente fila que esta 8 pixeles mas adelante
	   dec c 						; 1 fila menos
	jr nz, spb4filas  				; Mientras que C no sea 0 aun quedan filas por poner vpos
	
 ret

;-----------------------------------------------------------------------------
; Función para generar laa hpos en el buffer del SAT a partir de la pos x del sprite
; IN: A --> Coord X del sprite (ObjX)
; 	  HL --> Direcc en el buffer de la hpos
; 	  IX --> Direcc donde estan los datos del sprite
; MODIFICA: A, D, BC, HL
;-----------------------------------------------------------------------------
updateSpriteX:

	ld d, a 							; D = A. Guardamos en D el valor de hpos del principio de la columna para las siguiente columnas
	ld c, (ix+game_object.h)  			; Repetir para el numero de filas que ocupa el sprite de alto
spb4colum:
		ld a, d  						; A = D = primer valor de hpos para la columna
		ld b, (ix+game_object.width) 	; Columnas en total por fila del sprite
-		ld (hl), a 						; Almacenamos el valor de OBJx en hpos del buffer
		inc hl 							;
		inc hl 							; Vamos a la siguiente hpos del buffer, ya que entre hpos's esta el charcode
		add a, 8  						; El siguiente tile esta en la siguiente fila 8 pixeles mas adelante
		djnz -  						; B--.

		; Siguiente fila del sprite
		dec c
	jr nz, spb4colum 		; Si C no es 0, aun quedan filas por poner. Repetimos otra vez todo

 ret


;-----------------------------------------------------------------------------
; Función para borrar el sprite indicado en HL y DE de la pantalla. Para ello,
; a partir de las posiciones de PV y PH del sprite se ponen a 0 todas las pos
; correspondientes del sprite a borrar
; IN: 
; 	  IX --> Direcc donde estan los datos del sprite
; 	  DE --> Direcc primer VPOS del sprite a borrar
; 	  HL --> Direcc primer HPOS del sprite a borrar
; MODIFICA: A, DE, BC
;-----------------------------------------------------------------------------
borrarSpriteSAT:
	
	ld a, 0 				; A = valor para borrar el sprite

	; BORRAR VPOS (Y)
	ld c, (ix+game_object.h)      	; C = Alto del sprite. Numero de filas a añadir
	spb4filas2:
	   ;; Meter 1 fila
	   ld b, (ix+game_object.width)	  	; B = Ancho del sprite. Numero de columnas que tiene la fila
-	      ld  (de), a   			; Valor de vpos
	      inc  de 					; Siguiente vpos
	   djnz - 						; B --
	   dec c 						; 1 fila menos
	jr nz, spb4filas2  				; Mientras que C no sea 0 aun quedan filas por poner vpos
	

	; BORRAR HPOS (X)
	ld c, (ix+game_object.h)  			; Repetir para el numero de filas que ocupa el sprite de alto
	spb4colum2:
		ld b, (ix+game_object.width) 	; Columnas en total por fila del sprite
-		ld (hl), a 						; Almacenamos el valor de OBJx en hpos del buffer
		inc hl 							;
		ld (hl), a 						; Borramos tambien el cc
		inc hl 							; Vamos a la siguiente hpos del buffer, ya que entre hpos's esta el charcode
		djnz -  						; B--.

		; Siguiente fila del sprite
		dec c
	jr nz, spb4colum2 		; Si C no es 0, aun quedan filas por poner. Repetimos otra vez todo


ret



;-----------------------------------------------------------------------------
; Funcion para comprobar si una entidad del juego colisiona con otra entidad  o no.
; IN:  
; 		IX --> Puntero a los datos de la 1º entidad
; 		IY --> Puntero a los datos de la 2º entidad
; OUT:
; 		A --> Indica si se ha producido una colision o no entre las entidades.
; 			  Devuelve el ident de la entidad con la que colisiona en caso de colision
; 			  y en caso contrario devuelve un 0.
; MODIFICA: A, game_object.colision
;-----------------------------------------------------------------------------
checkColision:
		
	; 1º Comprobamos si las ambas entidades que colisionan existen o no.
	ld a, (ix+game_object.estado) 						; A = estado de la 1º entidad: vivo o muerto.
	cp ent_muerta  										;
	jr z, no_colision 									; Si A==0, quiere decir que la entidad no existe y por lo tanto no hay que comprobar colision

		; Comprobamos si la 2º entidad existe o no
		ld a, (iy+game_object.estado) 					; A = estado de la 2º entidad: vivo o muerto.
		cp ent_muerta  				
		jr z, no_colision 			

			; Ambas entidades existen por lo tanto hacemos las comprobaciones.
			call checkColisionX 						; Comprueba si han colisionado en X

			; Comprobamos si se ha producido una colision en X o no
			cp ent_colision 							; A = 1?
			jr nz, no_colision 							; SI A != 1, entonces no colision en X.

				; Hay colision en X, comprobamos si hay tambien colision en Y
				call checkColisionY 					; Comprueba si han colisionado en Y

				; Comprobamos si se ha producido una colision en Y o no
				cp ent_colision 						; A = 1?
				jr nz, no_colision 						; SI A != 1, entonces no colision en Y.

					; Colision en Y. Como en X tambien, entonces colision de las 2 entidades.
					; Guardamos en la variable colision de ambas entidades, el ID de la entidad con la que han colisionado
					ld a, (ix+game_object.id)  		 	; A = ID de la 1º entidad
					ld (iy+game_object.colision), a     ; Id de la 1º ent almacenado en var colision de la 2º ent

					ld a, (iy+game_object.id)  		 	; A = ID de la 2º entidad
					ld (ix+game_object.colision), a     ; ID de la 2º ent almacenado en var colision de la 1º ent

					ret

	; No colision en X o en Y, o una entidad (o ambas) no existe.
	no_colision:

	; Indicamos que no se ha producido colision 
	; En A ya tenemos el valor de no colision
	ld (iy+game_object.colision), a  			; No colision en 2º entidad 
	ld (ix+game_object.colision), a  			; No colision en 1º entidad

ret


;--------------------------------------------------------------------------------
; Funcion para comprobar si una entidad del juego colisiona con otra entidad en X,
; es decir, se comprueba si la 2º entidad no esta ni delante ni detras de la 1º
; entidad, dando lugar en ese caso a una colision en X.
; IN:  
; 		IX --> Puntero a los datos de la 1º entidad
; 		IY --> Puntero a los datos de la 2º entidad
; OUT:
; 		A --> Indica si se ha colisionado en X o no (0-->No colision/1-->Colision)
; MODIFICA: A, C, B
;--------------------------------------------------------------------------------
checkColisionX:

	
	; 1º COMPROBACION: 2º entidad a la izquierda de la 1º entidad?
	; if(ent2_x + ent2_w <= ent1_x) no hay colision, 2º ent a la izqd  -->
	; ent2_x + ent2_w - ent1_x <= 0. 
	; Si se cumple la comprobacion quiere decir que la 2º ent esta a la izqd de la 1º
	ld c, (iy+game_object.x) 				; C = pos_x de la 2º entidad
	ld a, (iy+game_object.wP)				; A = ancho de la 2º entidad
	add a, c 								; A = ancho + pos_x (2º entidad)	
	sub (ix+game_object.x) 					; A = (ancho +pos_x) - (pos_x de 1º entidad)

	; Las instrucciones de salto no modifican los flags por lo que podemos poner 2 jumps seguidos
	; if(<=0)
	jr z, no_colisionX 				; Si da 0 quiere decir que no hemos colisionado en X
	jp m, no_colisionX 				; Si no da 0 se comprueba su signo. Si es negativo entonces no hay 
									; colision ya que la 2º ent esta a la izqd. En caso contrario (valor positivo)
									; se comprueba el lado derecho para ver si hay colision

		; La 2º entidad no esta a la izqd de la 1º entidad

		; 2º COMPROBACION: 2º entidad a la derecha de la 1º entidad?
		; if(ent1_x + ent1_w <= ent2_x) no hay colision, 2º ent a la derecha -->
		; ent1_x + ent1_w - ent2_x <= 0
		; Si se cumple quiere decir que la 2º ent esta a la derecha de la 1º

		ld a, (ix+game_object.x) 			; A = pos_x de 1º ent
		add a, (ix+game_object.wP) 			; A = pos_x + ancho de 1º ent
		ld b, (iy+game_object.x) 			; B = pos_x de 2º ent
		sub b 						; A = (pos_x + ancho 1º ent) - pos_x 2º ent

		jr z, no_colisionX 			; Si da 0 quiere decir que no hemos colisionado en X
		jp m, no_colisionX 			; Si no da 0 se comprueba su signo. Si es negativo entonces no hay 
									; colision ya que la 2º ent esta a la derech. En caso contrario (valor positivo)
									; se comprueba el lado derecho para ver si hay colision

			;; 2º entidad no esta ni a la izquierda ni a la derecha de la 1º entidad.
			;; Por lo tanto hay colision en X entre ambas.
			ld a, ent_colision 		; A = 1. Colision en X
	 
			ret 

	; La 2º entidad se encuenta o a la derecha o a la izquierda de la 1º entidad, por lo tanto, no hay colision
	no_colisionX:  

	ld a, ent_no_colision 			; A = 0. No colision en X


ret

;--------------------------------------------------------------------------------
; Funcion para comprobar si una entidad del juego colisiona con otra entidad en Y,
; es decir, se comprueba si la 2º entidad no esta ni arriba ni debajo de la 1º
; entidad, dando lugar en ese caso a una colision en Y.
; IN:  
; 		IX --> Puntero a los datos de la 1º entidad
; 		IY --> Puntero a los datos de la 2º entidad
; OUT:
; 		A --> Indica si se ha colisionado en Y o no (0-->No colision/1-->Colision)
; MODIFICA: A, B
;--------------------------------------------------------------------------------
checkColisionY:

	; 1º COMPROBACION: 2º entidad arriba de la 1º entidad?
	; if(ent1_y => ent2_y + ent2_h) no_colision arriba -->
	; 0 => - ent1_y + ent2_y + ent2_h

	ld c, (iy+game_object.y) 				; C = pos_y 2º ent
	ld a, (iy+game_object.hP)				; A = alto 2º ent
	add a, c 						; A = pos_y + alto 2º ent
	sub (ix+game_object.y) 					; A = (pos_y + alto 2º ent) - pos_y 1º ent


	jr z, no_colisionY 				; Si da 0 quiere decir que no hemos colisionado en Y
	jp m, no_colisionY 				; Si no da cero, comprueba si es positivo o no, si no lo es, entonces no hay colision

		; 2º entidad no esta arriba
		; 2º COMPROBACION: 2º entidad abajo de la 1º entidad?
		; if(ent1_y + ent1_h <= ent2_y) no colision abajo -->
		; ent1_y + ent1_h - ent2_y <= 0

		ld c, (ix+game_object.y) 				; C = pos_y 1º ent
		ld a, (ix+game_object.hP)				; A = alto 1º ent
		add a, c 						; A = pos_y + alto 1º ent
		sub (iy+game_object.y) 				; A = (pos_y + alto 1º ent) - pos_y 2º ent

		jr z, no_colisionY 				; Si da 0 quiere decir que no hemos colisionado en Y
		jp m, no_colisionY 				; Si no da cero, comprueba si es positivo o no, si no lo es, entonces no hay colision

			; La 2º entidad no esta ni arriba ni abajo de la 1º entidad por lo tanto colision en Y
			ld a, ent_colision 		; A = 1. Colision en Y
	 
			ret 

	; La 2º entidad se encuenta o arriba o a abajo de la 1º entidad, por lo tanto, no hay colision
	no_colisionY:  

	ld a, ent_no_colision 			; A = 0. No colision en Y

ret


;--------------------------------------------------------------------------------
; FUncion para borrar del buffer del SAT la entidad pasada por parametro. Ademas,
; modifica la var de estado del sprite para indicar que ha muerto
; IN:  
; 		IX --> Puntero a los datos de la entidad a borrar
; 		HL --> Almacena la direccion a la definicion que indica la primera pos
; 				del sprite en el buffer donde empieza sus pos verticales.
; 		DE --> Almacena la direcc donde empiezan las posiciones horizontales del sprite.
; OUT:
;  		A --> Valor de ent_muerta para resetear var de los sprites
; MODIFICA: A, BC, HL, DE, spr_state
;--------------------------------------------------------------------------------
borrarEntidad:

	ld a, (ix+game_object.h)		; A = valor de altura del sprite. Servira para saber cuantas veces sumar
	ld b, a 				; B = A
	ld a, (ix+game_object.width) 		; A = valor de ancho del sprite. Servira para saber la cantidad a sumar cada vez
	ld c, a 				; C = A

	ld a, 00 				; Vaciamos A
	; Obtenemos la cantidad de pos a borrar (tanto de vpos como de hpos)
-	add a, c 				; A = A + C
	djnz -  				; B--. Hasta que B no sea 0 no hemos obtenido la multiplacion

	; Borramos las vpos del buffer
	ld b, a 				; B = Almacenamos el valor obtenido
-	ld (hl), 00 			; HL = 00
	inc hl
	djnz - 					; B--, hasta que B no sea 0 faltan vpos del buffer por vaciar.

	; Borramos las hpos y los cc del buffer
	ld b, a 				; B = Almacenamos el valor obtenido
	ld a, 00 				; A = Valor a copiar al buffer
-	ld (de), a
	inc de
	ld (de), a
	inc de
	djnz -

	ld a, ent_muerta      ;
	ld (ix+game_object.estado), a  ; Indicamos que la entidad ha sido destruida

	ld a, ent_no_colision
	ld (ix+game_object.colision), a ; No hay colision ya
 	

ret

;--------------------------------------------------------------------------------
; Funcion para obtener el valor del tile dentro del tilemap en funcion de la pos
; del sprite en pantalla. Tambien se obtiene las coord x e y del tile en el que 
; se encuentra el sprite a comprobar colision
; IN: 
; 	   IX --> Puntero datos entidad
; 	   C --> Valor que indica la cantidad a sumar
; OUT: 
; 	   A --> Valor del tile en el tilemap
; MODIFICA: A, DE, HL, B, IY
;-----------------------------------------------------------------------------
obtenerValorTilemap:
	
	;1º Calculamos las coordenadas X e Y del tile
	; El valor tanto de x como de y variara en funcion del valor pasado en C

	; Hay que dividir la posicion del sprite entre 8
	; Obtenemos posicion x del tile
	ld a, c 						; A = indicador del tipo de colision
	cp col_caida
	jr nz, no_caida

		; Tipo de colision caida
		; Tomamos el alto y la mitad del ancho del sprite
		ld a, (ix+game_object.wP) 	; A = ancho en pixeles del sprite
		ld b, a
		srl b 						; Dividimos entre 2 el ancho

		ld a, (ix+game_object.x) 	; A = pos x del player
		add a, b 					; obtenemos el ancho total	
    	
    	call dividirEntre8 			; Dividimos entre 8 lo que hay en A

	    ld d, a 					; Guardamos el valor de X en D	

	    ; Obtenemos posicion y del tile
	    ld a, (ix+game_object.y) 	; A = pos y del player
	    ld b, a 					; Guardamos el valor para no perderlo
	    ld a, (ix+game_object.hP)
	    add a, b  					; A = a + alto que ocupa el sprite	

    	call dividirEntre8 			; Dividimos entre 8 lo que hay en A	

	    ld e, a 					; Guardamos el valor de y en A
	    push de 					; Guardamos los valores x e y de coord

	    jr fin

	no_caida:

	cp col_izqd
	jr nz, no_izqd

		; Tipo de colision izquierda
		; Tomamos el alto a la mitad y nada de ancho del sprite
		ld a, (ix+game_object.x) 	; A = pos x del player

		call dividirEntre8 			; Dividimos entre 8 lo que hay en A

	    ld d, a 					; Guardamos el valor de X en D	

	    ; Obtenemos posicion y del tile
	    ld a, (ix+game_object.hP) 	; A = Alto del sprite en pixeles
	    ld b, a 				 
	    srl b 						; B = B/2

	    ld a, (ix+game_object.y) 	; A = pos y del player
	    add a, b  					; A = a + alto que ocupa el sprite		

    	call dividirEntre8 			; Dividimos entre 8 lo que hay en A	

	    ld e, a 					; Guardamos el valor de y en A
	    push de 					; Guardamos los valores x e y de coord

	    jr fin

   no_izqd:

   cp col_derecha
   jr nz, fin

   		; Tipo de colision derecha
   		; Tomamos el alto a la mitad y el ancho entero del sprite
   		ld a, (ix+game_object.wP) 	; A = ancho del sprite en pixeles
   		ld b, a
   		ld a, (ix+game_object.x) 	; A = pos x del player
   		add a, b 					; A = x del sprite + ancho

   		call dividirEntre8

   		ld d, a 					; Guardamos el valor de X en D	

   		; Obtenemos posicion y del tile
	    ld a, (ix+game_object.hP) 	; A = Alto del sprite en pixeles
	    ld b, a 				 
	    srl b 						; B = B/2

	    ld a, (ix+game_object.y) 			; A = pos y del player
	    add a, b  					; A = a + alto que ocupa el sprite		

    	call dividirEntre8 			; Dividimos entre 8 lo que hay en A	

	    ld e, a 					; Guardamos el valor de y en A
	    push de 					; Guardamos los valores x e y de coord

   fin:

    ; 2º Obtenemos el valor del tile en el tilemap

    ld iy, mapa 					; Puntero al primer mapa						
    ld a, (nivel) 					; A = nivel actual
    dec a
    cp 0 							
    jr z, mapa_actual 				; Si A==0 IX ya esta apuntado a los datos del mapa actual

    	ld de, _sizeof_level
    	ld b, a
-    	add iy, de 	   				 ; Bucle para hacer apuntar IX a los datos del mapa en el que nos encontramos	
    	djnz -

    mapa_actual:


    ; En DE tenemos las coord x e y del tile
    pop de
	push de 						; Guardamos el valor de pos x del tile
	ld b, e 						; B = pos y tile. Numero de filas a recorrer
	ld de, 64 						; DE = tamaño que ocupa cada fila del tilemap
	ld l, (iy+level.tilemap)
	ld h, (iy+level.tilemap+1) 		; Direcc donde esta la info de los tiles por pantalla


	ld a, b
	cp 1
	jr z, primera_fila	 			; Si es 0 quiere decir que es la primera fila y no hay que sumar los 64
		; Obtenemos el valor total a sumar para obtener la fila del tilemap
-		add hl, de 					; HL = HL + C
		djnz -  					; B--. Hasta que B no sea 0 no hemos obtenido la multiplacion

	primera_fila:
	; Ya estoy en la fila correspondiente
	; Ahora hay que acceder a la columna correspondiente
	pop de 							; Obtenemos el valor de pox x del tile
	ld b, d 						; B = pos x tile. Numero de columnas a recorrer
	ld de, 02						; Cada columna son 2 bytes


	ld a, b
	cp 1
	jr z, primera_col 				; Si es 0 no hace falta sumar los 2 bytes de columna
- 		add hl, de
		djnz - 						; Mientras que b no sea 0 aun hay valores a sumar.


	primera_col:
	ld a, (hl) 						; A = HL. valor del tilemap
	;ld (random), a 					; este es el que varia


ret


;--------------------------------------------------------------------------------
; Funcion para dividir entre 8 el valor almacenado en el acumulador
; IN: 
; 		A --> Valor a dividir
; OUT: 
; 	   A --> Valor divido entre 8
; MODIFICA: A
;-----------------------------------------------------------------------------
dividirEntre8:

	; Para dividir entre 8 un valor, hay que desplazar 3 veces los bits a la derecha		
	srl a 						; Desplaza una vez los bits a la derecha
	srl a 
	srl a 						; A = A/8	

ret

;--------------------------------------------------------------------------------
; Funcion para calcular el tiempo que ha pasado a partir del valor de tiempo 
; que se pasa por paramentro. Se resta ese valor con el valoc actual del reloj
; IN: 
; 		A --> Segundo desde el cual se quiere calcular el tiempo transcurrido
; OUT: 
; 	   A --> Tiempo transcurrido.
; MODIFICA: A, B
;-----------------------------------------------------------------------------
calcularTranscursoTiempo:

	ld b, a 					; B = tiempo a restar
	ld a, (tiempo) 				; A = tiempo actual
	sub b 						; A = tiempo actual - tiempo desde el cual se quiere calcular = transcurso de tiempo que ha pasado

ret



;--------------------------------------------------------------------------------
; Funcion que llena de 0s el buffer del SAT 
; MODIFICA: A, B, HL
;-----------------------------------------------------------------------------
vaciarBufferSAT:

	; Borramos todos los sprites del buffer del SAT
	ld a, 255
	ld b, a
	ld a, 0
	ld hl, satbuf
-	ld (hl), a
	inc hl
	djnz -

ret


;--------------------------------------------------------------
; Funcion que mata a todas las entidades del nivel anterior al 
; actual, es decir, pone su estado a 00.
; IN:
; 
; MODIFICA: A, game_object.estado, IX, DE, B
;--------------------------------------------------------------
matarEntidadesNivel:
	; Enemigos basicos
	ld a, (numEnemBas)
	cp 0
	jr z, _sig 										; Si A==0, no habian enemigos basicos

		ld ix, enemigo
		ld b, a
		ld a, 00 									; A = 00. Indicador de estado muerto del game_object
		ld de, _sizeof_game_object

- 		ld (ix+game_object.estado), a
		add ix, de 									; Siguiente enemigo basico
		djnz -

		; Enemigos avanzados
		_sig:
		ld a, (numEnemAv)
		cp 0
		jr z, _sig2

			ld ix, enemigoAv
			ld b, a
			ld a, 00
			ld de, _sizeof_game_object

-- 			ld (ix+game_object.estado), a
			add ix, de
			djnz --

		; Puerta
		_sig2:

		ld a, 00
		ld (puerta.estado), a


		; Llaves
		ld a, (numLlaves)
		cp 0
		jr z, _sig3

			ld ix, llaves
			ld b, a
			ld a, 00
			ld de, _sizeof_game_object

--- 		ld (ix+game_object.estado), a
			add ix, de
			djnz ---


		_sig3:

		; VIDAS
		ld a, (vidas.estado)
		cp 0
		jr z, _sig4

			ld a, 00
			ld (vidas.estado), a


		_sig4:

ret

.ends