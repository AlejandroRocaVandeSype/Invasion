;----------------------------------------------------
;----------------------------------------------------	
;			FICHERO MAIN DEL VIDEOJUEGO
;----------------------------------------------------	
;----------------------------------------------------


.include "cabecera.h.s" 		  			; Contiene definiciones, estructura de la ROM y el cartucho, y validacion de la cabecera de la ROM.



;=====================================================
;		Sección de arranque de la consola
;=====================================================
.bank 0 slot 0
.org $0000 									; Codigo que debe estar si o si al principio de la ROM

	di 										; Desactivamos las interrupciones
	im 1									; Modo 1 de interrupcion del Z80 (Todas las interrupciones saltaran a la direcc $0038)
	ld sp, $dff0  							; Direcc recomendada por la doc. oficial de Sega.
	jp main 								; Saltamos al programa principal del juego

;=====================================================
; 			Gestión de interrupciones
;=====================================================
.org $0038  					 			; Direcc donde se van a gestionar todas las interrupciones que se produzcan

.section "FRAME INTERRUPT" FORCE  			; FORCE indica que este codigo tiene que estar si o si donde indica org.

	; Gestion del frame interrupt. Cada vez que se genere un frame saltara la interrup. Es la unica interrup activada
	push af 								; Guardamos lo que hay en A en la pila para no perderlo.
	in a, portControl   					; Leemos del puerto de control lo que nos devuelve el estado de las flags.
	ld (estadoFlags), a     				; Guardamos el estado de las flags en ram

	ld a, (frame) 							; A = variable que almacena el contador de frames
	cp 60 									;
	jr c, cont 								; Solo reseteamos el contador de frames cuando llega a 60

		ld a, 0 							;
		ld (frame), a 						; frame = 0

		ld a, (tiempo) 						;
		inc a 								;
		ld (tiempo), a 						; tiempo + 1 cada 60 frames

	cont:

	ld a, (frame)
	inc a 					
	ld (frame), a

	pop af 									; Desapilamos el valor de A
	ei 										; habilitamos la interrupciones
ret 										; Retorno de interrupcion

.ends

.org $0066  ; Direcc para gestionar la interrupcion de pausa 

.section "PAUSA" FORCE
	retn 									; Desactivamos el boton de pausa
.ends


;=====================================================
; 		PROGRAMA PRINCIPAL DEL JUEGO (MAIN)
;=====================================================
.section "MAIN" FREE
main 
	
	call initVDP 						    ; Inicializamos los valores de los registros del VDP.

	; Inicializacion de libreria de sonido
    ;call PSGInit       						; Libreria de sverx!

	; COPIA DATOS MAPAS DEL JUEGO
	ld hl, init_mapas
	ld de, mapa
	ld bc, tamMapas
	ldir 

	call cargaTilesMapas 					; Relizamos la carga de todos los tiles de los mapas que hay en el juego

	;ld hl, Musica                           ; HL = Cargamos el tema de musica
    ;call  PSGPlay                           ; Se reproduce

	call checkEstadoMundo 					; Antes de empezar a ejecutar el bucle principal del juego, cargamos el menu.
	
	; Cuando el jugador pulsa la tecla "alt" empieza el juego
	call mundoInit 							;Inicilizamos todos los datos y recursos necesarios para el correcto funcionamiento del juego.

buclePrincipal  halt						; El bucle principal empezara cuando se produzca la interrup de vblank.

		;call PSGFrame			; process next music frame
		; here your can do bank switching if needed
		;call PSGSFXFrame              ; process next SFX frame
		
		call mundoUpdate 					; Ponemos en marcha nuestro mundo del juego para que haga todo lo que queremos

	jr buclePrincipal 						; Bucle infinito

ret


.ends