;----------------------------------------------------
;----------------------------------------------------	
; FICHERO QUE CONTIENE LAS FUNCIONES GENERALES DEL 
; CONTADOR DE TIEMPO DEL JUEGO
;----------------------------------------------------	
;----------------------------------------------------

.include "cabecera.h.s"


.ramsection "variables contadores" slot 3

	numero instanceof game_object 2 

.ends


.section "FUNCIONES CONTADOR TIEMPO" free

;--------------------------------------------------------------------------------
; Funcion para inicializar el contador de tiempo a su valor inicial. Habra un total
; de 300 segundos para completar el juego. Tambien se coloca el sprite de contador 
; de vidas del jugador en su pos correcta.
; IN:
; MODIFICA: HL, A
;---------------------------------------------------------------------------------
init_contador:

	; Inicializacion del contador de tiempo a su valor inicial (300 seg)
	ld hl, contador_tiempo 						; Apuntamos al digito que representa las centenas
	ld a, 1 									; A = valor que queremos poner en ese digito
	ld (hl), a 									; Actualizamos el numero

	inc hl 										; Siguiente digito (decenas)
	ld a, 5
	ld (hl), a
	
	ld a, 0
	inc hl 										; HL = unidades
	ld (hl), a 

	; Contador del reloj
	call dibujarContadorRelojVida

ret


;--------------------------------------------------------------------------------
; Funcion para actualizar todo lo relacionado con el contador de tiempo
; IN:
; MODIFICA: 
;---------------------------------------------------------------------------------
contador_update:
	
	ld a, (tiempoReloj)
	cp 0
	jr nz, _calc

		ld a, (tiempo)
		ld (tiempoReloj), a
		jr _cont

	_calc:
	ld a, (tiempoReloj)
	call calcularTranscursoTiempo
	cp 1
	jr c, _cont

		call decrementarTiempo  					; Cada vez que pasa 1 seg se decrementa el reloj en 1
		ld a, 0
		ld (tiempoReloj), a

	_cont:

	call dibujarCantidadTiempo					; Funcion que dibuja los numeros correspondientes del contador

ret

;--------------------------------------------------------------------------------
; Funcion para dibujar el numero correcto por pantalla del reloj de tiempo. 
; Este dependera de los valores almacenados en la variable contador_tiempo.
; IN:
; MODIFICA: HL, B, DE, A, tiempo.cc
;---------------------------------------------------------------------------------
dibujarCantidadTiempo:
	
	ld e, (ix+game_object.cc) 					; 
	ld d, (ix+game_object.cc+1)     			; DE = Posicion inicial de los charcodes
	push de

	ld hl, contador_tiempo 						; HL = valor del contador de tiempo
	ld b, 3 									; B = numero de digitos a actualizar
	ld de, 00  									

- 	add hl, de  								; Siguiente valor de la direcc HL
	push bc 									; Guardamos el numero de digitos que quedan por actualizar
	push hl 									; Guardamos el valor por el que hay que comprobar
	ld a, (hl) 									; A = valor del digito
	ld hl, numeroccs 							; HL = valores de indices de tiles de los numeros
	ld b, a 									
	cp 0 										; 
	jr z, _primervalor 							; Si el valor es 0, ya estamos apuntando al charcode correcto
--  	inc hl 									; Siguiente digito
		djnz -- 								; Bucle para dibujar el valor de digito correcto

	_primervalor:
	; HL --> Apuntando a charcode o copiar
	ld e, (ix+game_object.cc) 					; 
	ld d, (ix+game_object.cc+1)     			; DE = Posicion del buffer a actualizar
	ldi  										; Copiamos el contenido de Hl a DE
	inc de 										; Siguiente charcode estara 2 posicion mas adelante (ldi ya ha incrementado nuna vez de)

	; Guardamos la direcc del siguiente charcode que hay que actualizar
	ld a, e
	ld (ix+game_object.cc), a 					
	ld a, d 								
	ld (ix+game_object.cc+1), a

	pop hl
	pop bc
	ld de, 01 			 						; Siguiente valor de la direcc HL
	djnz -

	; Volvemos a apuntar a la primera pos del buffer de charcodes de los numeros para la siguiente vez que volvamos
	pop de 										; Recuperamos primera pos cc
	ld a, e
	ld (ix+game_object.cc), a
	ld a, d
	ld (ix+game_object.cc+1), a

ret

;--------------------------------------------------------------------------------
; Funcion para decrementar en 1 el numero que representa el tiempo restante para
; completar el juego. Si el digito esta a 0, se pondra a 9 y el siguiente digito 
; se restara 1. Si es el digito de las centenas que esta a 0, entonces quiere decir
; que el tiempo ha terminado y se ha terminado el juego.
; IN:
; MODIFICA: HL, A
;---------------------------------------------------------------------------------
decrementarTiempo:
	

	ld hl, contador_tiempo+2 					; Apuntamos a las unidades del numero
	ld a, (hl)
	cp 0
	jr z, _decenas

		; No es el ultimo valor de las unidades
		dec a 									; - 1 seg
		ld (hl), a
		ret

	_decenas:

	ld a, 9 									; 
	ld (hl), a 									; El siguiente numero sera un 9 al restar 1 al 0

	ld hl, contador_tiempo+1 					; Apuntamos a las decenas del numero
	ld a, (hl)
	cp 0
	jr z, _centenas 		

		; No es el ultimo valor de las decenas
		dec a 									; - 1 seg
		ld (hl), a
		ret

	_centenas:

	ld a, 9 									; 
	ld (hl), a 									; El siguiente numero sera un 9 al restar 1 al 0

	ld hl, contador_tiempo 						; Apuntamos a las centenas del numero
	ld a, (hl)
	cp 0
	jr z, _fin_juego

		; No es el ultimo valor de las centenas
		dec a 									; - 1 seg
		ld (hl), a
		ret

	_fin_juego:

	; El reloj de tiempo ha llegado a 000. GAME OVER
	ld a, 2
	ld (mundo_state), a 		; Cambiamos al estado de muerte
	call checkEstadoMundo 		; Saltamos a la direcc donde se gestiona la muerte del jugador
ret


;--------------------------------------------------------------------------------
; Funcion para copiar todos los datos necesarios tanto del contador de vidas como 
; el reloj de tiempo al buffer del SAT, para conseguir asi su dibujado por pantalla
; IN:
; MODIFICA: HL, A
;---------------------------------------------------------------------------------
dibujarContadorRelojVida:

	ld ix, numero

	ld l, (ix+game_object.PH) 					; 
	ld h, (ix+game_object.PH+1)     			; HL = Primera pos del buffer para las posiciones horizontales del sprite
	ld e, (ix+game_object.PV) 					; 
	ld d, (ix+game_object.PV+1)     			; DE = Primera pos del buffer para las posiciones verticales del sprite
	call updateBufferSAT 						; Actualizamos los valores x e y de los numeros en el buffer del SAT.

	; Contador de vidas del jugador
	ld ix, numero.2

	ld l, (ix+game_object.PH) 					; 
	ld h, (ix+game_object.PH+1)     			; HL = Primera pos del buffer para las posiciones horizontales del sprite
	ld e, (ix+game_object.PV) 					; 
	ld d, (ix+game_object.PV+1)     			; DE = Primera pos del buffer para las posiciones verticales del sprite
	call updateBufferSAT 						; Actualizamos los valores x e y de los numeros en el buffer del SAT.

ret

.ends