;----------------------------------------------------
;----------------------------------------------------	
; CABECERA CON VALIDACION DE LA ROM, CONFIGURACION 
; DE BANCOS Y RANURAS, DEFINICIONES, ESTRUCTURAS DEL
; JUEGO ETC.
;----------------------------------------------------	
;----------------------------------------------------

;=====================================================
; 		Configuración de bancos y ranuras de la ROM
;=====================================================
.memorymap  		;Definicion del mapa de memoria
defaultslot 0
slotsize $4000 		;16 kb de tam
slot 0 $0000   		; 0 - 16 kb	($0000 - $4000)
slot 1 $4000   		; 16 - 32 kb ($4000 - $7FFF)
slot 2 $8000 		; 32 - 48 KB ($8000 - $BFFF)
slotsize $2000 		; 8kb
slot 3  $c000  		;RAM ($C000 - $DFFF). Dirección recomendada por la doc. oficial de Sega.
.endme

.rombankmap 		; Definicion del mapa de bancos de la ROM
bankstotal 3
banksize $4000
banks 3  			; 3 bancos de ROM de 16 kb
.endro

;=====================================================
; Validacion de la cabecera de la ROM (etiqueta SDSC)
;=====================================================
.sdsctag 1.0, "Invasion", "TFG 2018/2019", "Alex Roca Vande Sype"



;====================================================
; 			DEFINICIONES/CONSTANTES/STRUCTS
;====================================================

; STRUCTS 
; Estructura para todos los game objects/entidades que van a haber en el juego
.struct game_object
 
 	estado db 					; Indica el estado del game_object (1--> Vivo/ 0 --> Muerto)
    id db 			  			; Para saber que entidad del juego es
	x db 			  			; Posicion x del game_object		  
	y db  			 			; Posicion y del game_object
	width db 					; Anchura del sprite de el game_object (en tiles de 8x8)
	h db 			  			; Altura del sprite de el game_object (en tiles de 8x8)
	dir db 			  			; Direcc a la que ira el game_object (Izquierda--> 0 / Derecha-->1)
	wP db 			  			; Anchura del sprite del game_object (en pixeles)
	hP db  			  			; Altura del sprite del game_object (en pixeles)
	colision db		  			; Indica si se ha producido colision o no y con quien (0--> No colision/>0 --> ID de la entidad con la que colisiono)
	PV dw 						; Pos inicial vertical del objeto en el buffer
	PH dw 						; Pos inicial horizontal del objeto
	cc dw 			   			; Pos del primer charcode(indice de tile?) del objeto
	vida db 					; Vida de los game objects

.endst

; Estructura con variables adiciones para las balas
.struct bala_variables
 
 	fin db 			 			; Para saber si la bala ha terminado su recorrido o no (0-->finalizado / 1--> No finalizado)
	tiempoBala db 				; Tiempo que lleva la bala disparada

.endst

; Estructura con variables adicionales para los niveles del juego
.struct level 
	
    tilemap dw 					; Etiqueta fichero tilemap
    tile_suelo db 				; Valor que representa el tile del suelo para colision
    enemigosBas db 				; Numero de enemigos basicos en el nivel
    enemigosAV db 				; Numero de enemigos avanzados en el nivel
    llaves db 					; Numero de llaves en el nivel
    tile_pared db 				; Valor que representa el tile de pared para colision
    tile_paredS db

.endst

; Estructura con variables extra para los enemigos
.struct enemigo_var

	IA_estado db 						; Indica en que estado se encuentra la IA del enemigo.
	tiempo_Pr db 						; Tiempo que el enemigo se encuentra en un estado
    tiempo_Seg db 						; Tiempo que el enemigo lleva en otro estado
    pos_x_inicial db 					; Posicion x inicial del enemigo
    velocidad_mov db 					; Indica la velocidad de movimiento del enemigo
    distancia_rec db 					; Valor de distancia que puede recorrer desde su posicion
    bala_uso db 						; Indica la bala que esta usando el enemigo
    cambioSpr db 						; Para saber cuando tiene que cambiar de sprite en la animacion de andar

.endst


; DEFINICIONES GENERALES
.equ portControl $bf           	; Puerto de control del VDP
.equ portData $be              	; Puerto de datos del VDP
.equ portJostyck $dc 		  	; Puerto de control del jostyck 
.equ ent_no_colision 0 	 		; Valor de no colision entre 2 entidades			
.equ ent_colision 1  			; Valor de colision entre 2 entidades
.equ ent_viva 1 				; Indica que la entidad (player, enemigo, bala etc) esta viva
.equ ent_muerta 0   			; Indica que la entidad ha sido eliminada, es decir, ya no existe

; PLAYER
.equ velMovPlayer 2 		  	  ; Velocidad de movimiento del sprite
.equ player_disparando 1  		  ; Indica que el jugador ha pulsado el boton de disparo
.equ player_no_disparando 0 	  ; Indica que el jugador no ha pulsado el boton de disparo
.equ player_no_salto 0 			  ; Indica que el player no esta haciendo la animacion de salto
.equ player_salto 1 			  ; Indica que el player si esta haciendo la animacion de salto
.equ player_izquierda 0 		  ; Indica que el player va a hacia la izquierda
.equ player_derecha 1 		  	  ; Indica que el player va a hacia la izquierda
.equ player_identif 1 			  ; Valor que identifica a la entidad del player

; BALA 
.equ velMovBala 2 		  	  	; Velocidad de movimiento del sprite de la bala
.equ bala_finalizado 0 		  	; Indica que la bala ha finalizado su recorrido
.equ bala_no_finalizado 1     	; Indica que la bala no ha finalizado su recorrido
.equ bala_sinDisparar 2     	; Indica que aun no se ha disparado la bala correspondiente
.equ bala_izquierda 0 		  	; Indica que la bala va a hacia la izquierda
.equ bala_derecha 1 		  	; Indica que la bala va a hacia la izquierda
.equ bala_identif 3 			; Valor que identifica a la entidad de la bala

; ENEMIGO 
.equ enemigo_identif 2 			  ; Valor que identifica a la entidad del enemigo

; ENEMIGO AVANZADO
.equ enemigoAV_id 4 			  ; Valor que identifica a la entidad del enemigo avanzado

; PUERTA
.equ puerta_id 5

; LLAVE
.equ llave_id 6

; VIDA
.equ vida_id 7

; MAPAS
.equ tamMapas 72 				  ; Cantidad de bytes que ocupan los datos de los mapas (Cada mapa tiene datos que ocupan 8 bytes)

; Mapa del SAT(Sprite Attribute Table). Informacion para el dibujado de sprites por pantalla.
; Aqui estan las posiciones de los sprites que no van a variar en ningun nivel.
.equ playerPV $c000				; Pos inicial vertical del objeto 
.equ playerPH $c080				; Pos inicial horizontal del objeto
.equ playercc $c081    	 		; Pos del primer charcode(indice de tile?) del objeto

.equ balaPV $C009  				; bala player vpos
.equ balaPH $C092    			; bala player hpos
.equ balacc $C093    			; bala player primer cc

.equ puertaPV $c030 
.equ puertaPH $c0e0
.equ puertaCC $c0e1




.equ finspr $c050    			; Fin del sprite. Indicamos donde metemos el valor $D0 indicando que no hay mas sprites para pintar.

; VALORES COLISIONES CON EL MAPA (para indicar como calcular el hitbox del sprite)
.equ col_caida 1 				  ; Indica que la colision debe ser de caida
.equ col_izqd 2
.equ col_derecha 3

;-------------------------------
; VALORES INICIALES GAME OBJECTS
;-------------------------------
; PLAYER
; ------
.equ play_estado 1  			; play activo
.equ play_id 1 					; ID del play
.equ play_wP 24 				; Ancho en pixeles 
.equ play_hP 24 				; Alto en pixeles
.equ play_w 3 					; Ancho en tiles
.equ play_h 3 					; Alto en tiles
.equ play_PV $c000
.equ play_PH $c080
.equ play_CC $c081
.equ play_dir 1 				; Inicialmente mirando derecha 
.equ play_colision 0 			; NO colision
.equ play_vida 3 				; Numero de vidas del player
.equ time_invulnerabilidad 4	; Tiempo que el jugador es invulnerable al recibir daño (seg)

; ENEMIGO BASICO
; -------
.equ enem_id 2 					; ID del enem
.equ enem_w 3 					; Ancho en tiles
.equ enem_h 4 					; Alto en tiles
.equ enem_dir 1 				; Inicialmente mirando derecha 
.equ enem_wP 16 				; Ancho en pixeles (24 original)
.equ enem_hP 25 				; Alto en pixeles (32 original)
.equ enem_colision 0 			; NO colision
.equ enem_PV $c008
.equ enem_PH $c090
.equ enem_CC $c091
.equ enem_vida 3 

; BALA PLAYER
; -------
.equ bal_w 2 					; Ancho en tiles
.equ bal_h 1 					; Alto en tiles
.equ bal_dir 1 					; Inicialmente mirando derecha 
.equ bal_wP 16 					; Ancho en pixeles 
.equ bal_hP 8 					; Alto en pixeles
.equ bal_colision 0 			; NO colision 
.equ bal_PV $c020
.equ bal_PH $c0C0 
.equ bal_CC $c0C1
.equ bal_id 3 					; ID del bal
.equ bal_vida 1


; CRISTALES
.equ cris_w 1
.equ cris_h 2
.equ cris_d 1
.equ cris_wP 8
.equ cris_hP 16
.equ cris_col 0
.equ cris_id 8
.equ cris_vida 1
.equ total_cristales 3
.equ pos_iniCristal 80



; ENEMIGOS AVANZADOS
.equ avanz_vida 2
.equ enemAv_hP 18 		; Original (24 pixeles)

; LEVEL 1
;--------
.equ num_game_objects 3 		; Indicamos el num de game objects que hay en este nivel
; PLAYER
; ------
.equ play_x 60 					; X inicial
.equ play_y 30 					; Y inicial


; ENEMIGO
; -------
.equ enem_estado 1  			; enem activo
.equ enem_x 150 				; X inicial
.equ enem_y 150 				; Y inicial

	
; BALA PLAYER
; -------
.equ max_bala 01 				; Numero de balas que hay
.equ bal_estado 0  				; bal inactivo
.equ bal_x 80 					; X inicial
.equ bal_y 160 					; Y inicial
