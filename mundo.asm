;----------------------------------------------------
;----------------------------------------------------	
; FICHERO QUE CONTIENE TODO LO RELACIONADO CON EL 
; MUNDO DEL JUEGO.
;----------------------------------------------------	
;----------------------------------------------------

.include "cabecera.h.s" 		  			; Contiene definiciones, estructura de la ROM y el cartucho, y validacion de la cabecera de la ROM.

; Espacio en la RAM para almacenar el buffer del SAT para su posterior copia a la VRAM.
.ramsection "Buffer SAT" slot 3	
	
	satbuf dsb 256      					; Tam del Buffer del SAT. Definimos 256 bytes para el buffer.
              
	estadoFlags db 							; Para almacenar el estado de las Flags del VDP.
	frame db 								; Contador de frames.
	tiempo db 								; Contador de tiempo que ha pasado. Se genera 1 segundo cada 60 frames.
	scroll db
	
.ends

.ramsection "VAR MUNDO" slot 3	
	
	mundo_state db 							; Indica el estado del mundo (Menu(0)/Jugando(1)/Muerte(2))
	nivel: db 								; Variable que indica en que nivel se encuentra el juego
	cambioNivel: db  		 				; Variable para indicar	que hay que cambiar de nivel (0--> No cambio/1--> Cambio)
	maxLevels: db 							; Numero maximo de mapas que hay
	mapa instanceof level 9 				; Numero de mapas a crear
	pause_flag db 							; Para gestionar la pausa
	numEnemBas db 							; Numero de enemigos basicos en el nivel
	numEnemAv db  							; Numero de enemigos avanzados en el nivel
	numLlaves db 							; Numero de llaves en el nivel
	contador_tiempo dsb 3					; Contador del tiempo restante de juego (sera un numero de 3 digitos 0-300)
	tiempoReloj db

.ends

.equ   vspeed 4 

.section "MUNDO" FREE

;----------------------------------------------------------------------
; Funcion para inicializar el juego: VDPs, cargar los assets necesarios,
; vaciar la VRAM, activar la pantalla etc
; MODIFICA: 
;----------------------------------------------------------------------
mundoInit:


	; VACIAR VRAM
	;ld hl, $0000 							; HL = $0000. Preparamos la VRAM para datos en la pos $0000
	;call prepararVDP						; Enviamos la direcc y orden a realizar al puerto de control del VDP.
	;call vaciarVRAM  						; Limpiamos la VRAM

    call initVarMundo                       ; Inicializamos las variables del mundo del juego

	; SPRITES
	call cargaAssetsSprite  				; Cargamos la paleta y los tiles necesarios para los sprites iniciales.

	call copiaDatosIniciales

	; PLAYER
	call dibujadoPlayer 					; Copiamos todos los datos necesarios del player al buffer del SAT

	ei 										; Activamos las interrupciones. Frame interrupt.
    halt 									; Esperamos a que ocurra una.	

    call cargarSAT 		  					; Copiamos todos los datos que hay en el buffer del SAT al SAT de VRAM para que se vea por pantalla.

    ld a, 0
    ld (scroll), a

	call activarPantalla					; Activamos la visualizacion de la pantalla

ret


;----------------------------------------------------------------------
; Funcion utilizada principalmente para realizar las llamadas necesa-
; rias de todas las funciones de nuestro juego y de esta manera conse-
; guir que funcione.
; MODIFICA: 
;----------------------------------------------------------------------
mundoUpdate:
	
	call checkEstadoMundo 					; Comprobamos en que estado esta el mundo (Menu/Jugando/Muerte)

	call checkNivel 						; Funcion que comprueba el nivel en el que se encuentra el jugador   

    call levelControl                       ; Control especifico para los ultimos niveles                     

	call mundo_llave 						; Llaves del nivel

	call mundo_puerta 						; Puerta del nivel

	call mundo_tiempo 						; Contador del tiempo del nivel

	call mundo_vidas 						; Dibujado de las vidas

	call cargarSAT 							; Copiamos el buffer del SAT al SAT del VRAM para actualizar la pantalla.

	; PLAYER
	call mundo_player 						; Control del player en el mundo

	; BALA 
	call mundo_bala

	; ENEMIGOS BASICOS
	call mundo_enemigo 						; enemigos basicos

	; ENEMIGOS AVANZADOS
	call mundo_enemigoAV 					; enemigos avanzados

ret



;=====================================================
;				 FUNCIONES LOCALES
;=====================================================

;-----------------------------------------------
; Funcion para inicilizar los registros del VDP.
; MODIFICA: HL, BC
;-----------------------------------------------
initVDP:
	
	ld hl, VDPDatos 						; HL = direcc datos VDP
	ld b, VDPDatosEnd-VDPDatos				; B = Tam en bytes a enviar al puerto
	ld c, portControl 						; C = $bf. Puerto a enviar los datos 
	otir 									; A partir de los datos anterior realiza la copia de datos

ret

;----------------------------------------------------------------------------
; Funcion para enviar al VDP la orden a realizar.
; IN: 
; 		HL--> Orden y direcc
; MODIFICA: A, HL
;----------------------------------------------------------------------------
prepararVDP:
	
	ld a, l 								; A = L = 1º byte con la direcc.
	out (portControl), a            		; Enviamos los datos a $BF
	ld a, h  								; A = H = 2º byte con direcc y orden
	or $40
	out (portControl), a 					; Enviamos los datos a $BF

ret

;-----------------------------------------------------------------------------
; Función para cargar una paleta de colores en CRAM
; IN: 
; 	  Hl = Puntero a inicio datos paleta
; 	  B = Tam en bytes de la paleta
; MODIFICA: C
;-----------------------------------------------------------------------------
cargaPaleta:

	ld c,portData 							; C = puerto a enviar los datos ($be)
	otir 									; Copiamos los datos al puerto indicado con el tam especficado

 ret

;-----------------------------------------------
; Funcion para escribir en la VRAM
; IN: BC --> Contador. Tam a copiar
;     HL --> Puntero al inicio de los datos a copiar
; MODIFICA: BC, HL, A
;-----------------------------------------------
loadVRAM:
	
	ld a, (hl) 								; A = primer valor al que apunta HL
	out (portData), a
	inc hl 									; Hl ++ 
	dec bc 									; BC --. Cantidad a enviar
	ld a, c 								; A = C
	or b 									; Combina el byte que hay en el acumulador con el byte en C. Dando como resultado 1 si uno de los
                     						; dos tiene un 1, por lo que solo dará 0 si ambos son 0. Afecta al flag Z
    jr nz, loadVRAM                         ; Mientras que b no sea 0,quiere decir que no hemos terminado de copiar todos los datos.

ret

;----------------------------------------------------------------------------
; Funcion rellenar la VRAM de 0s y asi vaciarla
; IN: 
; MODIFICA: 
;----------------------------------------------------------------------------
vaciarVRAM:
	


ret

;----------------------------------------------------------------------------
; Funcion para activar la visualizacion de pantalla
; IN: 
; MODIFICA: 
;----------------------------------------------------------------------------
activarPantalla:

	ld a, %11100000 						; Activamos la pantalla y ponemos los sprites normales
	ld b, 1 								; B = 1
	call setRegistroVDP 					; Actualizamos los valores del registro 1


ret

;----------------------------------------------------------------------------
; Funcion para actualizar/establecer los valores del registro correspondiente
; del VDP.
; IN: 
; 	  A --> Valores nuevos para el registro
;     B --> Indice del registro
; MODIFICA: A
;----------------------------------------------------------------------------
setRegistroVDP:
	
	out (portControl), a 	; PortCOntrol = valores nuevos del VDP
	ld a, $80 				; A = $80.
	or b 					
	out (portControl), a

ret

;-----------------------------------------------------------------------------
; Función para copiar los datos desde el buffer del SAT a la tabla del SAT en VRAM
; IN: 
; MODIFICA: HL, BC
;-----------------------------------------------------------------------------
cargarSAT:

	  ld hl, $3F00 		; Hl = Direcc donde se encuentra la SAT en VRAM y orden de escribir en VRAM.
	  call prepararVDP

	  	ld hl, satbuf   ; HL = Direcc donde se encuentra el buffer del SAT
	  	ld b, 255 		; Tam total del buffer del sat
	  	ld c, portData 	; Puerto a enviar los datos
	  	otir

 ret
;---------------------------------------------------------------------------
; Funcion para cargar todos los assets necesarios para los sprites.
; Esto son sus tiles y la 2º paleta de colores.
; MODIFICA: HL, BC
;---------------------------------------------------------------------------
cargaAssetsSprite:
	
	; CARGA DE LA PALETA DE COLORES PARA LOS SPRITES
	ld hl, $c010 							; Copiamos en la 2º paleta para los sprites. color bank 2, color 0 (sprites).
	call prepararVDP						; Enviamos la direcc y orden a realizar al puerto de control del VDP.

	ld hl,PaletaSprite  					; Hl = Puntero a inicio datos paleta				
	ld b,(PaletaSpriteEnd-PaletaSprite)  	; B = Tam en bytes de la paleta
	call cargaPaleta 						; Copiamos la paleta de colores de los sprites en CRAM

    ;CARGA DE LOS TILES DEL PLAYER
    ld hl, $2000 							; ; HL = 2000. Primer tile, indice 256. Vamos a guardar los tiles de los sprites en esta direcc
    call prepararVDP 						
 	
    ld hl, tilesSprite 						; HL = Direcc donde estan los tiles del sprite
    ld bc, 9*32 							; BC = Tam que ocupan los tiles. tiles de 32 bytes de tam cada uno
    call loadVRAM 							; Copiamos los tiles en VRAM justo despues de los del primer sprite

    ;CARGA DE LOS TILES DEL DISPARO
    ld hl, $2120							; HL = Primer tile de la bala, indice 272 (256 + 15 tiles del primer sprite)
    call prepararVDP 					

   	ld hl, tilesSpriteDisparo 				; HL = direcc donde se encuentran los tiles del sprite del disparo
    ld bc, 2*32 							; BC = Tam que ocupan los tiles. 9 tiles de 32 bytes de tam cada uno
    call loadVRAM 							; Copiamos los tiles a la VRAM

    ;CARGA DE LOS TILES DEL PLAYER IZQUIERDA
    ld hl, $2160							; HL = primer tile del sprite
    call prepararVDP

    ld hl, tilesSpriteIzq 					; HL = direcc donde se encuentran los tiles del sprite del player izquierda
    ld bc, 9*32  							; BC = tam que ocupan los tiles.
    call loadVRAM 			

    ;CARGA DE LOS TILES DEL PLAYER SALTO DERECHA
    ld hl, $2280							
    call prepararVDP

    ld hl, tilesSpriteS 					
    ld bc, 9*32  							; BC = tam que ocupan los tiles.
    call loadVRAM 			

     ;CARGA DE LOS TILES DEL PLAYER SALTO IZQUIERDA
    ld hl, $23a0							
    call prepararVDP

    ld hl, tilesSpriteSI 					
    ld bc, 9*32  							; BC = tam que ocupan los tiles.
    call loadVRAM 			

    ;CARGA DE LOS TILES DEL PLAYER CAIDA DERECHA
    ld hl, $24c0							
    call prepararVDP

    ld hl, tilesSpriteC 					
    ld bc, 9*32  							; BC = tam que ocupan los tiles.
    call loadVRAM 			

    ;CARGA DE LOS TILES DEL PLAYER SALTO IZQUIERDA
    ld hl, $25e0							
    call prepararVDP

    ld hl, tilesSpriteCI 					
    ld bc, 9*32  							; BC = tam que ocupan los tiles.
    call loadVRAM 		

    ; CARGA TILES DISPARO IZQD
    ld hl, $2700
    call prepararVDP

    ld hl, tilesSpriteDisparoIzqd
    ld bc, 2*32
    call loadVRAM

    ;CARGA DE LOS TILES DEL PLAYER CORRER 1 DERECHA
    ld hl, $2740							
    call prepararVDP

    ld hl, tilesSpriteRD 					
    ld bc, 9*32  							; BC = tam que ocupan los tiles.
    call loadVRAM 	

     ;CARGA DE LOS TILES DEL PLAYER CORRER 2 DERECHA
    ld hl, $2860							
    call prepararVDP

    ld hl, tilesSpriteRD2 					
    ld bc, 9*32  							; BC = tam que ocupan los tiles.
    call loadVRAM 	

    ;CARGA DE LOS TILES DEL ENEMIGO BASICO QUIETO
    ld hl, $2980							; HL = primer tile del enemigo
    call prepararVDP

    ld hl, tilesSpriteEnemigo 				; HL = direcc donde se encuentran los tiles del sprite del enemigo
    ld bc, 12*32  							; BC = tam que ocupan los tiles.
    call loadVRAM 			

    ;CARGA DE LOS TILES DEL ENEMIGO BASICO QUIETO Izquierda
    ld hl, $2b00							
    call prepararVDP

    ld hl, tilesSpriteEnemigoIzqd 				
    ld bc, 12*32  							; BC = tam que ocupan los tiles.
    call loadVRAM 			

    ;CARGA DE LOS TILES DEL ENEMIGO BASICO ANDAR 1
    ld hl, $2c80							
    call prepararVDP

    ld hl, tilesSpriteEnemAndar1				
    ld bc, 12*32  							; BC = tam que ocupan los tiles.
    call loadVRAM 			

     ;CARGA DE LOS TILES DEL ENEMIGO BASICO ANDAR 2
    ld hl, $2e00							
    call prepararVDP

    ld hl, tilesSpriteEnemAndar2				
    ld bc, 12*32  							; BC = tam que ocupan los tiles.
    call loadVRAM 			

    ; CARGA TILES NUMEROS 
    ld hl, $2f80
    call prepararVDP

    ld hl, tilesSpriteNumeros
    ld bc, 10*32
    call loadVRAM

     ;CARGA DE LOS TILES DEL ENEMIGO BASICO ANDAR 1 IZQUIERDA
    ld hl, $30c0							
    call prepararVDP

    ld hl, tilesSpriteEnemAndar1I				
    ld bc, 12*32  							; BC = tam que ocupan los tiles.
    call loadVRAM 			

     ;CARGA DE LOS TILES DEL ENEMIGO BASICO ANDAR 2 IZQUIERDA
    ld hl, $3240						
    call prepararVDP

    ld hl, tilesSpriteEnemAndar2I				
    ld bc, 12*32  							; BC = tam que ocupan los tiles.
    call loadVRAM 		

    ; CARGA TILES VIDAS
    ld hl, $33c0
    call prepararVDP

    ld hl, tilesSpriteVida
    ld bc, 1*32
    call loadVRAM

    ; CARGA DE LOS TILES DE LAS LLAVES
    ld hl, $33e0 							; HL = primer tile llave
    call prepararVDP

    ld hl, tilesSpriteLlave
    ld bc, 4*32
    call loadVRAM

    ; CARGA DE LOS TILES DE LAS PUERTAS
    ld hl, $3460 							; HL = primer tile puerta Abierta
    call prepararVDP

    ld hl, tilesSpritePuertaA
    ld bc, 9*32
    call loadVRAM

    ; CARGA TILES ENEMIGO AVANZADO
    ld hl, $3580
    call prepararVDP

    ld hl, tilesSpriteAvanzado
    ld bc, 9*32
    call loadVRAM

    ; CARGA TILES ENEMIGO AVANZADO IZQUIERDA
    ld hl, $36a0
    call prepararVDP

    ld hl, tilesSpriteAvanzIzqd
    ld bc, 9*32
    call loadVRAM

    ; CARGA TILES CRISTALES
    ld hl, $37c0
    call prepararVDP

    ld hl, tilesSpriteCristal
    ld bc, 2*32
    call loadVRAM

ret



;--------------------------------------------------------------
; Funcion para copiar todos los tiles de los mapas a partir de 
; la direcc $0000 de VRAM
; MODIFICA: HL, BC
;--------------------------------------------------------------
cargaTilesMapas:

	; TILES MAPA 1
	ld hl, $0000 							; HL = Primer tile, indice 0. LOs tiles van a ser guardados a partir de la direcc 0000
	call prepararVDP

	ld hl, tilesLevel01
	ld bc, 448
	call loadVRAM  							; Copiamos los tiles en la VRAM

	; TILES MAPA 2
	ld hl, $01c0 							; HL = posicion primer tile siguient mapa
	call prepararVDP

	ld hl, tilesMapa2 						
	ld bc, 448
	call loadVRAM

    ; TILES MAPA 3
    ld hl, $0380                            
    call prepararVDP

    ld hl, tilesMapa3                       
    ld bc, 448
    call loadVRAM

    ; TILES PANTALLA MUERTE
    ld hl, $0540
    call prepararVDP

    ld hl, tilesMuerte                      
    ld bc, 544
    call loadVRAM

    ; TILES PANTALLA MENU
    ld hl, $0760
    call prepararVDP

    ld hl, tilesMenu                     
    ld bc, 416
    call loadVRAM

    ; Tiles MAPA 4
    ld hl, $0900
    call prepararVDP

    ld hl, tilesMapa4                     
    ld bc, 448
    call loadVRAM

    ; Tiles MAPA 5
    ld hl, $0ac0
    call prepararVDP

    ld hl, tilesMapa5                     
    ld bc, 448
    call loadVRAM

    ; Tiles MAPA 6
    ld hl, $0c80
    call prepararVDP

    ld hl, tilesMapa6                     
    ld bc, 384
    call loadVRAM

    ; Tiles MAPA 7
    ld hl, $0e00
    call prepararVDP

    ld hl, tilesMapa7                     
    ld bc, 448
    call loadVRAM

    ; Tiles MAPA 8
    ld hl, $0fc0
    call prepararVDP

    ld hl, tilesMapa8                     
    ld bc, 384
    call loadVRAM

    ; Tiles MAPA 9
    ld hl, $1140
    call prepararVDP

    ld hl, tilesMapa9                     
    ld bc, 640
    call loadVRAM

    ; Tiles Pantalla FIN JUEGO
    ld hl, $13c0
    call prepararVDP

    ld hl, tilesEND                     
    ld bc, 672
    call loadVRAM



ret
;--------------------------------------------------------------
; Funcion para cargar el tilemap del mapa correspondiente al que 
; apunta IX para copiarlo a la tabla de VRAM y visualizarlo por pantalla
; Tambien se actualiza la variable de cambioNivel para indicar 
; que ya se ha realizado el cambio de nivel.
; IN: 
; 		IX --> Puntero datos mapa
; 		BC --> Tamaño tilemap
; MODIFICA: HL, A, cambioNivel
;--------------------------------------------------------------
cargaTilemapMapa:

	ld hl, $3800 							; HL = Puntero al nombre de la tabla
	call prepararVDP

	ld l, (ix+level.tilemap)
	ld h, (ix+level.tilemap+1) 				; HL = datos del tilemap del Fondo
	call loadVRAM  							; Copiamos el tilemap a la VRAM

	ld a, 0 								; 
	ld (cambioNivel), a 					; Cambio de nivel realizado

ret



;--------------------------------------------------------------
; Funcion para inicializar las variables del mundo
; MODIFICA: cambioNivel, nivel, A
;--------------------------------------------------------------
initVarMundo:

	ld a, 1
	ld (cambioNivel), a  							; Hay que cambiar nivel
	ld a, 1
	ld (nivel), a 									; Nivel inicial = level 1

	ld a, 9
	ld (maxLevels), a

	ld a, 00
	ld (pause_flag), a 								; Pausa desactivada

	ld (numEnemAv), a
	ld (numEnemBas), a
	ld (numLlaves), a


ret


;--------------------------------------------------------------
; Funcion que comprueba en que nivel se encuentra el jugador
; para cargar el mapa correspondiente al nivel y todos los game
; obejcts del mismo.
; IN:
; 
; MODIFICA: A, IX, HL, BC, 
;--------------------------------------------------------------
checkNivel:

	ld a, (cambioNivel) 				 					; A = valor para indicar si queremos hacer cambio de nivel
	cp 1
	jp nz, no_cambioLvl 									; Si A == 1 entonces hay que hacer un cambio de nivel

		ld a, (nivel) 										; A = nivel en el que se encuentra el juego
		cp 1 								
		jr nz, checkLvl2

			;-------------------------- 
			;	NIVEL 1
			;--------------------------
			ld ix, mapa.1 									; Puntero a datos del mapa

			; PALETA DE COLORES MAPA 1
			ld hl, $c000 									; Hl = $C000. H = Orden. 3-> Escribir en CRAM (Paleta de colores). Copiamos en la 1º paleta. color bank 1, color 0.
			call prepararVDP								; Enviamos la direcc y orden a realizar al puerto de control del VDP.

			; Copiamos la paleta de colores del fondo en CRAM
			ld hl,PaletaFondo
			ld b,(PaletaColoresEnd-PaletaFondo)  
			call cargaPaleta 								; Copiamos la paleta del fondo en CRAM. 

			ld bc, 32*24*2 									; BC = tam del tilemap
			call cargaTilemapMapa 							; Carga el tilemap a VRAM del mapa correspondiente para mostrarlo por pantalla


			; TODOS LOS DATOS YA ESTAN INICIALIZADOS EN ESTE NIVELs
            ld a, 16
            ld (player.x), a
            ld (player.y), a

			ret 

        checkLvl2:
        cp 2
        jr nz, level3

			;-------------------------- 
			;	NIVEL 2
			;--------------------------
			call vaciarBufferSAT 							; Borramos todos los sprites de la pantalla

			call matarEntidadesNivel 						; Matamos todas las entidades del nivel anterior

			ld ix, mapa.2 									; Puntero a los datos del mapa 2

			; PALETA DE COLORES MAPA 2
			ld hl, $c000 									; Hl = $C000. H = Orden. 3-> Escribir en CRAM (Paleta de colores). Copiamos en la 1º paleta. color bank 1, color 0.
			call prepararVDP								; Enviamos la direcc y orden a realizar al puerto de control del VDP.

			; Copiamos la paleta de colores del fondo en CRAM
			ld hl,PaletaMapa2
			ld b,(PaletaMapa2End-PaletaMapa2)  
			call cargaPaleta 								; Copiamos la paleta del fondo en CRAM. 

			ld bc, 32*24*2 									; BC = tam del tilemap
			call cargaTilemapMapa 							; Carga el tilemap a VRAM del mapa correspondiente para mostrarlo por pantalla


			; PLAYER
			call dibujadoPlayer 							; Copiamos todos los datos necesarios del player al buffer del SAT

			; POSICION INICIAL DEL PLAYER EN EL NIVEL
			ld a, 8
			ld (player.x), a

			ld a, 15
			ld (player.y), a


			; PUERTA
			ld hl, init_game_objects2
			ld de, puerta
			ld bc, 17
			ldir

			; Vida
            ld hl, vida2
			ld de, vidas
			ld bc, 17
			ldir

			; CONTADORES
			call dibujarContadorRelojVida 					; Copiamos los datos necesarios del contador de reloj y vida al buffer del SAT

			ret


        level3:
        cp 3
        jr nz, level4
            ;-------------------------- 
            ;   NIVEL 3
            ;--------------------------
            call vaciarBufferSAT                            ; Borramos todos los sprites de la pantalla

            call matarEntidadesNivel                        ; Matamos todas las entidades del nivel anterior

            ld ix, mapa.3                                   ; Puntero a los datos del mapa 3

            ; VALORES DE CANTIDAD DE ENEMIGOS, LLAVES, VIDAS ETC
            call copiaEntidadesPorNivel                     ; Copiamos los valores que indican el numero de cada game_object en el nivel actual

            ; PALETA DE COLORES MAPA 3
            ld hl, $c000                                    ; Hl = $C000. H = Orden. 3-> Escribir en CRAM (Paleta de colores). Copiamos en la 1º paleta. color bank 1, color 0.
            call prepararVDP                                ; Enviamos la direcc y orden a realizar al puerto de control del VDP.

            ; Copiamos la paleta de colores del fondo en CRAM
            ld hl,PaletaMapa3
            ld b,(PaletaMapa3End-PaletaMapa3)  
            call cargaPaleta                                ; Copiamos la paleta del fondo en CRAM. 

            ld bc, 32*24*2                                  ; BC = tam del tilemap
            call cargaTilemapMapa                           ; Carga el tilemap a VRAM del mapa correspondiente para mostrarlo por pantalla

            ; PLAYER
            call dibujadoPlayer                             ; Copiamos todos los datos necesarios del player al buffer del SAT

            ; POSICION INICIAL DEL PLAYER EN EL NIVEL
            ld a, 8
            ld (player.x), a

            ld a, 20
            ld (player.y), a

            ; Enemigos basicos
            ld hl, init_game_objects3
            ld de, enemigo
            ld bc, 17
            ldir

            ld hl, init_var_extra_enemBas3
            ld de, enemigoBasInf
            ld bc, 8
            ldir

            ; PUERTA
            ld hl, puerta3
            ld de, puerta
            ld bc, 17
            ldir

            ; Llave
            ld de, llaves
            ld bc, 17
            ldir

            ; CONTADORES
            call dibujarContadorRelojVida                   ; Copiamos los datos necesarios del contador de reloj y vida al buffer del SAT

            ret


        level4:
        cp 4
        jr nz, level5

            ;-------------------------- 
            ;   NIVEL 4
            ;--------------------------
            call vaciarBufferSAT                            ; Borramos todos los sprites de la pantalla

            call matarEntidadesNivel                        ; Matamos todas las entidades del nivel anterior

            ld ix, mapa.4                                   ; Puntero a los datos del mapa 4

            ; VALORES DE CANTIDAD DE ENEMIGOS, LLAVES, VIDAS ETC
            call copiaEntidadesPorNivel                     ; Copiamos los valores que indican el numero de cada game_object en el nivel actual

            ; PALETA DE COLORES MAPA 4
            ld hl, $c000                                    ; Hl = $C000. H = Orden. 3-> Escribir en CRAM (Paleta de colores). Copiamos en la 1º paleta. color bank 1, color 0.
            call prepararVDP                                ; Enviamos la direcc y orden a realizar al puerto de control del VDP.

            ; Copiamos la paleta de colores del fondo en CRAM
            ld hl,PaletaMapa4
            ld b,(PaletaMapa4End-PaletaMapa4)  
            call cargaPaleta                                ; Copiamos la paleta del fondo en CRAM. 

            ld bc, 32*24*2                                  ; BC = tam del tilemap
            call cargaTilemapMapa                           ; Carga el tilemap a VRAM del mapa correspondiente para mostrarlo por pantalla

            ; PLAYER
            call dibujadoPlayer                             ; Copiamos todos los datos necesarios del player al buffer del SAT

            ; POSICION INICIAL DEL PLAYER EN EL NIVEL
            ld a, 8
            ld (player.x), a

            ld a, 20
            ld (player.y), a

            ; Enemigos basicos
            ld hl, init_game_objects4
            ld de, enemigo
            ld bc, 34
            ldir

            ld hl, init_var_extra_enemBas4
            ld de, enemigoBasInf
            ld bc, 16
            ldir

            ; PUERTA
            ld hl, puerta4
            ld de, puerta
            ld bc, 17
            ldir

            ; Llave
            ld de, llaves
            ld bc, 17
            ldir

            ; CONTADORES
            call dibujarContadorRelojVida                   ; Copiamos los datos necesarios del contador de reloj y vida al buffer del SAT

            ret


        level5:
        cp 5
        jp nz, level6

            ;-------------------------- 
            ;   NIVEL 5
            ;--------------------------
            call vaciarBufferSAT                            ; Borramos todos los sprites de la pantalla

            call matarEntidadesNivel                        ; Matamos todas las entidades del nivel anterior

            ld ix, mapa.5                                   ; Puntero a los datos del mapa 5

            ; VALORES DE CANTIDAD DE ENEMIGOS, LLAVES, VIDAS ETC
            call copiaEntidadesPorNivel                     ; Copiamos los valores que indican el numero de cada game_object en el nivel actual

            ; PALETA DE COLORES MAPA 4
            ld hl, $c000                                    ; Hl = $C000. H = Orden. 3-> Escribir en CRAM (Paleta de colores). Copiamos en la 1º paleta. color bank 1, color 0.
            call prepararVDP                                ; Enviamos la direcc y orden a realizar al puerto de control del VDP.

            ; Copiamos la paleta de colores del fondo en CRAM
            ld hl,PaletaMapa5
            ld b,(PaletaMapa5End-PaletaMapa5)  
            call cargaPaleta                                ; Copiamos la paleta del fondo en CRAM. 

            ld bc, 32*24*2                                  ; BC = tam del tilemap
            call cargaTilemapMapa                           ; Carga el tilemap a VRAM del mapa correspondiente para mostrarlo por pantalla

            ; PLAYER
            call dibujadoPlayer                             ; Copiamos todos los datos necesarios del player al buffer del SAT

            ; POSICION INICIAL DEL PLAYER EN EL NIVEL
            ld a, 180
            ld (player.x), a

            ld a, 15
            ld (player.y), a

            ; Enemigo basico
            ld hl, init_game_objects5
            ld de, enemigo
            ld bc, 17
            ldir

            ld hl, init_var_extra_enemBas5
            ld de, enemigoBasInf
            ld bc, 8
            ldir

            ; Enemigo Avanzado
            ld hl, avanzado5
            ld de, enemigoAv
            ld bc, 17
            ldir

            ld hl, init_var_extra_enemAv5
            ld de, enemigoAvInf
            ld bc, 8
            ldir

            ; BALAS
            ld hl, init_balas5                                 
            ld de, bala.2                                  ; Copiamos a partir de la segunda bala ya que la primera es del player                          
            ld bc, 17                                      
            ldir       

            call initBala                                  ; Variables extra de las balas


            ; PUERTA
            ld hl, puerta5
            ld de, puerta
            ld bc, 17
            ldir

            ; Llave
            ld de, llaves
            ld bc, 17
            ldir

            ; Vida
            ld de, vidas
            ld bc, 17
            ldir

            ; CONTADORES
            call dibujarContadorRelojVida                   ; Copiamos los datos necesarios del contador de reloj y vida al buffer del SAT

            ret 


        level6:
        cp 6
        jp nz, level7

            ;-------------------------- 
            ;   NIVEL 6
            ;--------------------------
            call vaciarBufferSAT                            ; Borramos todos los sprites de la pantalla

            call matarEntidadesNivel                        ; Matamos todas las entidades del nivel anterior

            ld ix, mapa.6                                   ; Puntero a los datos del mapa 6

            ; VALORES DE CANTIDAD DE ENEMIGOS, LLAVES, VIDAS ETC
            call copiaEntidadesPorNivel                     ; Copiamos los valores que indican el numero de cada game_object en el nivel actual

            ; PALETA DE COLORES MAPA 4
            ld hl, $c000                                    ; Hl = $C000. H = Orden. 3-> Escribir en CRAM (Paleta de colores). Copiamos en la 1º paleta. color bank 1, color 0.
            call prepararVDP                                ; Enviamos la direcc y orden a realizar al puerto de control del VDP.

            ; Copiamos la paleta de colores del fondo en CRAM
            ld hl,PaletaMapa6
            ld b,(PaletaMapa6End-PaletaMapa6)  
            call cargaPaleta                                ; Copiamos la paleta del fondo en CRAM. 

            ld bc, 32*24*2                                  ; BC = tam del tilemap
            call cargaTilemapMapa                           ; Carga el tilemap a VRAM del mapa correspondiente para mostrarlo por pantalla

            ; PLAYER
            call dibujadoPlayer                             ; Copiamos todos los datos necesarios del player al buffer del SAT

            ; POSICION INICIAL DEL PLAYER EN EL NIVEL
            ld a, 20
            ld (player.x), a

            ld a, 130
            ld (player.y), a

            ; Enemigo basico
            ld hl, init_game_objects6
            ld de, enemigo
            ld bc, 34
            ldir

            ld hl, init_var_extra_enemBas6
            ld de, enemigoBasInf
            ld bc, 16
            ldir

            ; Enemigo Avanzado
            ld hl, avanzado6
            ld de, enemigoAv
            ld bc, 17
            ldir

            ld hl, init_var_extra_enemAv6
            ld de, enemigoAvInf
            ld bc, 8
            ldir

            ; BALAS
            ld hl, init_balas6                                 
            ld de, bala.2                                  ; Copiamos a partir de la segunda bala ya que la primera es del player                          
            ld bc, 17                                      
            ldir       

            call initBala                                  ; Variables extra de las balas


            ; PUERTA
            ld hl, puerta6
            ld de, puerta
            ld bc, 17
            ldir

            ; Vida
            ld hl, vida6
            ld de, vidas
            ld bc, 17
            ldir

            ; CONTADORES
            call dibujarContadorRelojVida                   ; Copiamos los datos necesarios del contador de reloj y vida al buffer del SAT

            ret 


        level7:
        cp 7
        jr nz, level8

            ;-------------------------- 
            ;   NIVEL 7
            ;--------------------------
            call vaciarBufferSAT                            ; Borramos todos los sprites de la pantalla

            call matarEntidadesNivel                        ; Matamos todas las entidades del nivel anterior

            ld ix, mapa.7                                   ; Puntero a los datos del mapa 6

            ; VALORES DE CANTIDAD DE ENEMIGOS, LLAVES, VIDAS ETC
            call copiaEntidadesPorNivel                     ; Copiamos los valores que indican el numero de cada game_object en el nivel actual

            ; PALETA DE COLORES MAPA 4
            ld hl, $c000                                    ; Hl = $C000. H = Orden. 3-> Escribir en CRAM (Paleta de colores). Copiamos en la 1º paleta. color bank 1, color 0.
            call prepararVDP                                ; Enviamos la direcc y orden a realizar al puerto de control del VDP.

            ; Copiamos la paleta de colores del fondo en CRAM
            ld hl,PaletaMapa7
            ld b,(PaletaMapa7End-PaletaMapa7)  
            call cargaPaleta                                ; Copiamos la paleta del fondo en CRAM. 

            ld bc, 32*24*2                                  ; BC = tam del tilemap
            call cargaTilemapMapa                           ; Carga el tilemap a VRAM del mapa correspondiente para mostrarlo por pantalla

            ; PLAYER
            call dibujadoPlayer                             ; Copiamos todos los datos necesarios del player al buffer del SAT

            ; POSICION INICIAL DEL PLAYER EN EL NIVEL
            ld a, 10
            ld (player.x), a

            ld a, 60
            ld (player.y), a

            ; Enemigo basico
            ld hl, init_game_objects7
            ld de, enemigo
            ld bc, 17
            ldir

            ld hl, init_var_extra_enemBas7
            ld de, enemigoBasInf
            ld bc, 8
            ldir

            ; Enemigo avanzado
            ld hl, avanzado7
            ld de, enemigoAv
            ld bc, 17
            ldir

            ld hl, init_var_extra_enemAv7
            ld de, enemigoAvInf
            ld bc, 8
            ldir

            ; CONTADORES
            call dibujarContadorRelojVida                   ; Copiamos los datos necesarios del contador de reloj y vida al buffer del SAT

            ret 

        level8:
        cp 8
        jr nz, level9

            ;-------------------------- 
            ;   NIVEL 8
            ;--------------------------
            call vaciarBufferSAT                            ; Borramos todos los sprites de la pantalla

            call matarEntidadesNivel                        ; Matamos todas las entidades del nivel anterior

            ld ix, mapa.8                                   ; Puntero a los datos del mapa 8

            ; PALETA DE COLORES MAPA 8
            ld hl, $c000                                    ; Hl = $C000. H = Orden. 3-> Escribir en CRAM (Paleta de colores). Copiamos en la 1º paleta. color bank 1, color 0.
            call prepararVDP                                ; Enviamos la direcc y orden a realizar al puerto de control del VDP.

            ; Copiamos la paleta de colores del fondo en CRAM
            ld hl,PaletaMapa8
            ld b,(PaletaMapa8End-PaletaMapa8)  
            call cargaPaleta                                ; Copiamos la paleta del fondo en CRAM. 

            ld bc, 32*24*2                                  ; BC = tam del tilemap
            call cargaTilemapMapa                           ; Carga el tilemap a VRAM del mapa correspondiente para mostrarlo por pantalla

            ; PLAYER
            call dibujadoPlayer                             ; Copiamos todos los datos necesarios del player al buffer del SAT

            ; POSICION INICIAL DEL PLAYER EN EL NIVEL
            ld a, 90
            ld (player.x), a

            ld a, 120
            ld (player.y), a

            ; Vida
            ld hl, init_game_objects8
            ld de, vidas
            ld bc, 17
            ldir

            ; CONTADORES
            call dibujarContadorRelojVida                   ; Copiamos los datos necesarios del contador de reloj y vida al buffer del SAT

            ret

        level9:
        cp 9
        jr nz, no_cambioLvl


            ;-------------------------- 
            ;   NIVEL 9
            ;--------------------------
            call vaciarBufferSAT                            ; Borramos todos los sprites de la pantalla

            call matarEntidadesNivel                        ; Matamos todas las entidades del nivel anterior

            ld ix, mapa.9                                   ; Puntero a los datos del mapa 9

             ; VALORES DE CANTIDAD DE ENEMIGOS, LLAVES, VIDAS ETC
            call copiaEntidadesPorNivel                     ; Copiamos los valores que indican el numero de cada game_object en el nivel actual

            ; PALETA DE COLORES MAPA 9
            ld hl, $c000                                    ; Hl = $C000. H = Orden. 3-> Escribir en CRAM (Paleta de colores). Copiamos en la 1º paleta. color bank 1, color 0.
            call prepararVDP                                ; Enviamos la direcc y orden a realizar al puerto de control del VDP.

            ; Copiamos la paleta de colores del fondo en CRAM
            ld hl,PaletaMapa9
            ld b,(PaletaMapa9End-PaletaMapa9)  
            call cargaPaleta                                ; Copiamos la paleta del fondo en CRAM. 

            ld bc, 32*24*2                                  ; BC = tam del tilemap
            call cargaTilemapMapa                           ; Carga el tilemap a VRAM del mapa correspondiente para mostrarlo por pantalla

            ; PLAYER
            call dibujadoPlayer                             ; Copiamos todos los datos necesarios del player al buffer del SAT

            ; POSICION INICIAL DEL PLAYER EN EL NIVEL
            ld a, 15
            ld (player.x), a

            ld a, 10
            ld (player.y), a

            ; ENEMIGOS
            ld hl, init_game_objects9
            ld de, enemigo
            ld bc, 34
            ldir

            ld hl, init_var_extra_enemBas9
            ld de, enemigoBasInf
            ld bc, 16
            ldir

             ; Enemigo avanzado
            ld hl, avanzado9
            ld de, enemigoAv
            ld bc, 17
            ldir

            ld hl, init_var_extra_enemAv9
            ld de, enemigoAvInf
            ld bc, 8
            ldir

            ; CRISTALES
            ld hl, cristal9
            ld de, cristal
            ld bc, 51
            ldir

             ; CONTADORES
            call dibujarContadorRelojVida                   ; Copiamos los datos necesarios del contador de reloj y vida al buffer del SAT

            ret

	no_cambioLvl:

ret


;--------------------------------------------------------------
; Zono con todo lo relacionado con el player
; IN:
; 
; MODIFICA: IX, DE, HL
;--------------------------------------------------------------
mundo_player:
	
	call playerUpdate 							; Actualizamos los datos del personaje controlable por el jugador
	ld ix, player 								; IX = puntero a los datos del game object del player

    ld hl, playerPH                             ; HL = Direcc del SAT donde esta situado horizontalmente el objeto
    ld de, playerPV                             ; DE = Direcc del SAT donde esta situado verticalmente el objeto

    ld a, (invulnerable)
    cp 0
    jr z, _dibujarPlayer

        ; Recibido daño. Comienza efecto parpadeo
        ld a, (parpadeo)
        cp 0
        jr nz, _dib

            ; Borrado
            call borrarSpriteSAT                        ; Se vacia la zona del SAT donde esta el sprite a borrar
            ld a, 1
            ld (parpadeo), a
            ret

        _dib: ; Dibujado
        call updateBufferSAT
        ld a, 0
        ld (parpadeo), a

        ret

    _dibujarPlayer:
    call updateBufferSAT                        ; Actualizamos los valores x e y del sprite en el buffer del SAT.ar

    _noNada:



ret

;--------------------------------------------------------------
; Zona con todo lo relacionado con la bala
; IN:
; 
; MODIFICA: IX, IY, A, BC, DE, HL
;--------------------------------------------------------------
mundo_bala:
	
	; BALA
	ld ix, bala

	call balaUpdate 									; Update de la bala

	ld a, (ix+game_object.estado)
	cp 0
	jr z, _no_comprobar 								; Si la bala no existe no dibujamos ni comprobamos colision

			ld l, (ix+game_object.PH) 					; 
			ld h, (ix+game_object.PH+1)     			; HL = Primera pos del buffer para las posiciones horizontales del sprite
			ld e, (ix+game_object.PV) 					; 
			ld d, (ix+game_object.PV+1)     			; DE = Primera pos del buffer para las posiciones verticales del sprite
			call updateBufferSAT 						; Actualizamos los valores x e y de la bala en el buffer del SAT.


			call checkEnemigosConBala 					; Comprobamos colisiones entre enmigos y la bala del jugador

            ld a, (ix+game_object.colision)
            cp 0
            jr nz, _no_comprobar                        ; Si la bala ya ha colisionado con algo, no se comprueba

                ld a, (nivel)
                cp 9
                jr nz, _no_comprobar

                    ; Solo si el jugador esta en el ultimo nivel se comprueba colision de las balas con el cristal
                    call checkCristalesConBala                  ; Se comprueba si la bala del jugador colisiona con el cristal
			
	_no_comprobar:	


ret


;--------------------------------------------------------------
; Zona con todo lo relacionado con la puerta de cada nivel
; IN:
; 
; MODIFICA: IX, A
;--------------------------------------------------------------
mundo_puerta:

	ld ix, puerta

	ld a, (ix+game_object.estado)
	cp 0
	jr z, _no_puerta 									; Comprobamos si la puerta existe o no, es decir, si hay una puerta en el nivel

		; Hay una puerta en el nivel.
		call puerta_update

	_no_puerta:



ret


;--------------------------------------------------------------
; Funcion para recorrer todos los enemigos del nivel y comprobar
; si colisionan con la bala del player o no
; IN:
;       IX --> Puntero a datos de la bala
; MODIFICA: A, IY, DE, B
;--------------------------------------------------------------
checkEnemigosConBala:
; Bucle para comprobar si la bala colisiona con algun enemigo
	ld a, (numEnemBas) 							; A = num enemigos basicos
	ld iy, enemigo 								; IY = datos primer enemigo
	cp 0
	jr z, _no_comp 								; Check si numEnemigosBasicos > 0

		_comprobarCol:
  		; Comprobamos si hay colision entre los enemigos y la bala
		ld b, a 								; B = Contador enemigos
		ld de, 00

next: 	add iy, de 								; Apuntamos siguiente entidad de enemigos
		push bc
			call checkColision  				; Comprobamos colision entre el enemigo y la bala

			ld a, (iy+game_object.id) 			; A = id del enemigo que estamos comprobando colision
			ld b, a 								
            ld a, (ix+game_object.colision)     
			cp b 								; 
			jr z, _no_comp    				    ; Si la bala ya colisiona con algun enemigo, salimos.

			ld de, _sizeof_game_object
			pop bc 								; B = Recuperamos valor contador enemigos

		djnz next 								; Mientras que queden enemigos por comprobar

		; Comprobacion 2º tipo de enemigos
		ld a, (iy+game_object.id) 					; A = id de los enemigos avanzados
		cp 4
		ret z 										; Si A coincide con el id de los avanzados, ya hemos hecho el bucle para estos enemigos

		ld a, (numEnemAv) 						; A = num de enemigos avanzados
		ld iy, enemigoAv 						; IY = primera entidad de enemigos avanzados
		cp 0
		jr nz, _comprobarCol 					; Comprobamos colision con estos enemigos si hay en el nivel

        ret

        _no_comp:
        pop bc


ret

;--------------------------------------------------------------
; Funcion para  comprobar si la bala colisiona con algun cristal
; IN:
;       IX --> Puntero a datos de la bala
; MODIFICA: A, B, IY, DE
;--------------------------------------------------------------
checkCristalesConBala:

    ld a, total_cristales               
    ld b, a                             ; B = total de cristales
    ld iy, cristal                      ; IY = direcc datos cristales


    ld de, 00

-   add iy, de                          ; Apuntamos al principio de los datos del cristal
    push bc                             ; Guardamos el valor del total de cristales
    ld a, (iy+game_object.estado)
    cp ent_muerta
    jr z, _nextCristals                  ; Si estado == 0, cristal destruido

        ; Cristal activo aun
        call checkColision              ; Se comprueba colision entre la bala y el cristal


    _nextCristals:
    pop bc
    ld de, _sizeof_game_object
    djnz -




ret

;--------------------------------------------------------------
; Zona con todo lo relacionado con los enemigos basicos
; IN:
; 
; MODIFICA: IX, DE, A, B, IY
;--------------------------------------------------------------
mundo_enemigo:
	
	ld a, (numEnemBas)
	cp 0
	ret z 									; Si no hay enemigos basicos no se hace nada
	ld b, a 								; B = numero de enemigos basicos que hay en el nivel actual
	ld ix, enemigo
	ld iy, enemigoBasInf

--		push bc 							; Guardamos el valor de enemigos
		ld a, (ix+game_object.estado)
		cp ent_muerta
		jr z, enem_muerto 					; A==0 --> Enemigo muerto

			; Enemigo vivo. 
			call enemigo_update 		 	; Update del enemigo	

		enem_muerto:
		ld de, _sizeof_game_object 			; DE = tam game_objects
		add ix, de 							; Datos siguiente enemigo
		ld de, _sizeof_enemigo_var  		; DE = tam enemBas_var
		add iy, de
		pop bc 								; BC = Numero de enemigos restantes a actualizar
		djnz -- 							; Mientras que queden enemigos por checkear en el nivel seguimos en el bucle
ret

;--------------------------------------------------------------
; Zono con todo lo relacionado con los enemigos avanzados
; IN:
; 
; MODIFICA: IX, DE, A, B, IY
;--------------------------------------------------------------
mundo_enemigoAV:
	
	ld a, (numEnemAv)
	cp 0
	ret z 									; Si no hay enemigos avanzados no se hace nada
	
	ld b, a 								; B = numero de enemigos avanzados que hay en el nivel actual
	ld ix, enemigoAv
	ld iy, enemigoAvInf

--		push bc 							; Guardamos el valor de enemigos

		call enemigoAV_update 		 		; Update del enemigo avanzado

		ld de, _sizeof_game_object 			; DE = tam game_objects
		add ix, de 							; Datos siguiente enemigo
		ld de, _sizeof_enemigo_var  		; DE = tam enemBas_var
		add iy, de
		pop bc 								; BC = Numero de enemigos restantes a actualizar
		djnz -- 							; Mientras que queden enemigos por checkear en el nivel seguimos en el bucle

ret
;--------------------------------------------------------------
; Zona con todo lo relacionado con los llaves
; IN:
; 
; MODIFICA: IX, DE, A, B
;--------------------------------------------------------------
mundo_llave:
	
	ld a, (numLlaves)
	cp 0
	ret z 									; Si no hay llaves en el nivel, salimos

	ld ix, llaves
	ld b, a
	ld de, 00

---	push bc
	add ix, de

		call llave_update

	pop bc
	ld de, _sizeof_game_object
	djnz --- 								; Bucle para hacer update de todas las llaves


ret

;--------------------------------------------------------------
; Zona con todo lo relacionado con las vidas que hay por los niveles
; IN:
; 
; MODIFICA: IX, DE, A, B
;--------------------------------------------------------------
mundo_vidas:
	
	ld ix, vidas

	ld a, (ix+game_object.estado)
	cp 0
	jr z, _contador

		call vidas_update
	_contador:

	ld ix, numero.2 				; Apuntamos a los datos del contador de vidas

	call contadorVidas

ret


;--------------------------------------------------------------
; Zona con todo lo relacionado con el contador de tiempo
; IN:
; 
; MODIFICA: IX
;--------------------------------------------------------------
mundo_tiempo:
	
	ld ix, numero 							; Direcc donde estan los datos de los numeros a dibujar
	
	call contador_update 					; Actualizamos el reloj de tiempo

ret


;--------------------------------------------------------------
; Zona con todo lo relacionado con los cristales del ultimo nivel
; IN:
; 
; MODIFICA: IX, A, DE, B
;--------------------------------------------------------------
mundo_cristales:

    ld a, total_cristales                   ; 
    ld b, a                                 ; B = Total de cristales que hay

    ld ix, cristal                          ; IX = Zona datos del primer cristal
    ld de, 00                               ; Para que la primera vez no se avance ninguna pos 
-   push bc                                 ; Guardamos en la pila el contador de cristales

        add ix, de                          ; Avanzamos a la primera pos de los datos del crista
        ld a, (ix+game_object.estado)
        cp 0                                ;
        jr z, _nextCristal                  ; Si estado cristal == 0 --> Cristal destruido. No se hace su update

            ; Cristal no destruido aun
            call cristal_update             ; Update del cristal

        _nextCristal:
        pop bc
        ld de, _sizeof_game_object
        djnz -


ret

;--------------------------------------------------------------
; Funcion para reiniciar el juego. Se vuelve al nivel 1, se 
; establecen las variables a sus valores iniciales, 
; se dibuja al player y se carga los datos al SAT. 
; Todo esto cuando el jugador pulse la tecla alt
; IN:
; 
; MODIFICA: 
;--------------------------------------------------------------
resetGame:

	call initVarMundo 						; Volvemos al primer nivel, indicamos que queremos cambiar de nivel etc

	call copiaDatosIniciales

	call dibujadoPlayer 					; Copiamos todos los datos necesarios del player al buffer del SAT

	call cargarSAT 		  					; Copiamos todos los datos que hay en el buffer del SAT al SAT de VRAM para que se vea por pantalla.

ret

;--------------------------------------------------------------
; Funcion para copiar los valores iniciales de los game objects 
; del primer nivel
; IN:
; 
; MODIFICA: HL, DE, BC
;--------------------------------------------------------------
copiaDatosIniciales:

	; COPIA DATOS INICIALES PARA GAME OBJECTS
	ld hl, init_game_objects 				; HL = puntero a donde estan los datos de los game objects
	ld de, player 							; DE = puntero a donde se tienen que copiar los datos
	ld bc, 34 								; BC = tam de los datos
	ldir 									; Hacemos la copia de datos

	ld hl, vidas1
	ld de, vidas
	ld bc, 17
	ldir

	ld de, puerta
	ld bc, 17
	ldir

	; Cargamos los datos del contador de tiempo de nivel y contador de vidas
	; El ancho del sprite indicara el tamaño del numero (3 digitos para el reloj)
	ld hl, init_contadores
	ld de, numero
	ld bc, 34
	ldir

	; Inicializacion de variables extra que pueda necesitar cada entidad de manera independiente.
	call initPlayer 						
	call initBala 						
	call init_contador 						; Reseteamos el contador de tiempo a su valor inicial

ret



;--------------------------------------------------------------
; Funcion que comprueba el estado en el que esta el juego. Si 
; estamos en el menu, jugando o pantalla de muerte. En los estados
; de muerte o menu, se cargara en el fondo la pantalla correspondiente
; y se mantendrá en ese estado hasta que se pulse la tecla "alt"
; MODIFICA: A, mundo.state, hl, B
;--------------------------------------------------------------
checkEstadoMundo:

	ld a, (mundo_state) 					              ; A = estado del mundo
	cp 1
	ret z 									              ; Si estado del mundo == 1 (jugando) salimos

		; No estamos en el estado de jugando
		cp 0
		jr nz, _muerteJugador 				              ; Si no es el estado == 0 (menu) saltamos al estado de muerte

			call activarPantalla					      ; Activamos la visualizacion de la pantalla

		   ; Carga Pantalla Menu
            ld hl, $c000                                    ; Hl = $C000. H = Orden. 3-> Escribir en CRAM (Paleta de colores). Copiamos en la 1º paleta. color bank 1, color 0.
            call prepararVDP                                ; Enviamos la direcc y orden a realizar al puerto de control del VDP.

            ld hl,PaletaMenu
            ld b,(PaletaMenuEnd-PaletaMenu)  
            call cargaPaleta                                ; Copiamos la paleta del fondo en CRAM. 

            ld bc, 32*24*2                                  ; BC = tam del tilemap
            ld hl, $3800                                    ; HL = Puntero al nombre de la tabla
            call prepararVDP

            ld hl, tilemapMenu
            call loadVRAM                                   ; Copiamos el tilemap a la VRAM

			call pausaJuego
			
            jr next_state                                   ; Cambiamos de estado de mundo (jugando)

		_muerteJugador:

        cp 2
        jr nz, _JuegoTerminado 

		; Estado muerte (2)
		; Cargamos pantalla de muerte al VRAM 
        call vaciarBufferSAT                            ; Borramos todos los sprites de la pantalla
        call cargarSAT

        ld hl, $c000                                    ; Hl = $C000. H = Orden. 3-> Escribir en CRAM (Paleta de colores). Copiamos en la 1º paleta. color bank 1, color 0.
        call prepararVDP                                ; Enviamos la direcc y orden a realizar al puerto de control del VDP.

        ld hl,PaletaMuerte
        ld b,(PaletaMuerteEnd-PaletaMuerte)  
        call cargaPaleta                                ; Copiamos la paleta del fondo en CRAM. 

        ld bc, 32*24*2                                  ; BC = tam del tilemap
        ld hl, $3800                                    ; HL = Puntero al nombre de la tabla
        call prepararVDP

        ld hl, tilemapMuerte
        call loadVRAM                                   ; Copiamos el tilemap a la VRAM

		call pausaJuego 					            ; Pausamos el juego
		call resetGame 						            ; Reiniciamos el juego

        jr next_state                                   ; Cambiamos de estado de mundo (jugando)

        _JuegoTerminado:
        ; Estado FIN DEL JUEGO (3)
        ; Cargamos pantalla de fin del juego al VRAM 
        call vaciarBufferSAT                            ; Borramos todos los sprites de la pantalla
        call cargarSAT

        ld hl, $c000                                    ; Hl = $C000. H = Orden. 3-> Escribir en CRAM (Paleta de colores). Copiamos en la 1º paleta. color bank 1, color 0.
        call prepararVDP                                ; Enviamos la direcc y orden a realizar al puerto de control del VDP.

        ld hl,PaletaFIN
        ld b,(PaletaFINEnd-PaletaFIN)  
        call cargaPaleta                                ; Copiamos la paleta del fondo en CRAM. 

        ld bc, 32*24*2                                  ; BC = tam del tilemap
        ld hl, $3800                                    ; HL = Puntero al nombre de la tabla
        call prepararVDP

        ld hl, tilemapEND
        call loadVRAM                                   ; Copiamos el tilemap a la VRAM

        call pausaJuego                                 ; Pausamos el juego
        call resetGame                                  ; Reiniciamos el juego

		next_state:

        ld a, 1
        ld (mundo_state), a                             ; Cambiamos al estado de jugando
ret

;--------------------------------------------------------------
; Funcion que "pausa" el juego, es decir, lo deja en un bucle
; infinito el cual comprueba todo el rato la entrada de tecla-
; do del jugador y si este pulsa la tecla "alt" se termina el bucle
; MODIFICA:
;--------------------------------------------------------------
pausaJuego:

	_bucle_pausa:
	call getInput 							;
	bit 5, a 								;
	jr nz, _bucle_pausa 					; Pausamos el juego hasta que el jugador decida reiniciar(pulsa "alt").

ret


;--------------------------------------------------------------
; Funcion que copia los valores que indican el numero de game_objects
; de cada tipo que hay en cada nivel. Este info se obtiene de los
; structs de los niveles y se usa para saber cuantas veces se tienen
; que hacer los bucles de los updates de las entidades.
; IN: 
;  		IX --> apuntando al struct del nivel correspondiente
; MODIFICA: A, numEnemBas, numEnemAv, numLlaves
;--------------------------------------------------------------
copiaEntidadesPorNivel:

	ld a, (ix+level.enemigosBas) 					; 
	ld (numEnemBas), a 								; Numero de enemigos basicos que hay en este nivel

	ld a, (ix+level.enemigosAV) 					; 
	ld (numEnemAv), a 								; Numero de enemigos avanzados que hay en este nivel

	; Llaves
	ld a, (ix+level.llaves)
	ld (numLlaves), a



ret


;--------------------------------------------------------------
; Funcion para comprobar si estamos en alguno de los ultimos 
; 3 niveles ya que para cambiar al siguiente nivel desde ellos
; son necesarios requisitos especificos.
; IN: 
;      
; MODIFICA: A
;--------------------------------------------------------------
levelControl:

    ld a, (nivel)
    cp 7
    jr z, mundo_salirCuevas                      ; Se comprueba si el jugador esta en el nivel 7

    ld a, (nivel)
    cp 8
    jr z, mundo_pasarLvlFinal                    ; Se comprueba si se esta en el nivel 8

    ld a, (nivel)
    cp 9
    jr z, mundo_levelFinal                      ; Se comprueba si estamos en el ultimo nivel del juego



ret


;--------------------------------------------------------------
; Funcion que comprueba si el jugador esta colocado en la posicion
; derecha final del mapa para pasar al siguiente mapa
; IN: 
;      
; MODIFICA: A
;--------------------------------------------------------------
mundo_pasarLvlFinal:

    ; Jugador en el nivel 8
    ld a, (player.x)
    cp 240
    jr c, _no_salida                      ; Check si pos X del player menor que el valor del CP (en caso afirmativo salimos)

        ; Pos X mayor que el CP
        cp 248
        jr nc, _no_salida                 ; Check si pos X del player mayor que el valor del CP (en caso afirmativo salimos)

            ld a, (player.y)
            cp 128
            jr c, _no_salida             ; Check si pos y del player menor que el valor del CP (en caso afirmativo salimos)

                ; Pos Y mayor que el CP
                cp 160
                jr nc, _no_salida        ; Check si pos y del player mayor que el valor del CP (en caso afirmativo salimos)

                    ;Siguiente nivel
                    ld a, (nivel)                   ; A = nivel actual en el que esta el jugador
                    inc a                           ;
                    ld (nivel), a                   ; Siguiente nivel
                    ld a, 1                         ;
                    ld (cambioNivel), a             ; Indicamos que queremos hacer un cambio de nivel

    _no_salida:


ret


;--------------------------------------------------------------
; FUncion que comprueba si el jugador esta en la zona de la escalera 
; para salir fuera de las cuevas y cambiar de nivel
; IN: 
;      
; MODIFICA: A, NIVEL, CAMBIONIVEL
;--------------------------------------------------------------
mundo_salirCuevas:

    ; Jugador en el nivel 7
    ld a, (player.x)
    cp 40
    jr c, _no_escalera                      ; Check si pos X del player menor que el valor del CP (en caso afirmativo salimos)

        ; Pos X mayor que el CP
        cp 80
        jr nc, _no_escalera                 ; Check si pos X del player mayor que el valor del CP (en caso afirmativo salimos)

            ; Pos X menor que el CP
            ld a, (player.y)
            cp 16
            jr c, _no_escalera             ; Check si pos y del player menor que el valor del CP (en caso afirmativo salimos)

                ; Pos Y mayor que el CP
                cp 56
                jr nc, _no_escalera        ; Check si pos y del player mayor que el valor del CP (en caso afirmativo salimos)


                    ; JUGADOR en pos escalera
                    call getInput           ; Comprobamos al entrada por teclado
                    bit 5, a                            ;
                    jr nz, _no_escalera                ; Si el jugador pulsa la tecla "alt" cuando esta en la puerta, se cambia de nivel.

                        ; Tecla ALT pulsada
                        ld a, (nivel)                   ; A = nivel actual en el que esta el jugador
                        inc a                           ;
                        ld (nivel), a                   ; Siguiente nivel
                        ld a, 1                         ;
                        ld (cambioNivel), a             ; Indicamos que queremos hacer un cambio de nivel

    _no_escalera:


ret



;--------------------------------------------------------------
; Funcion para controlar todo lo que ocurre el nivel final del juego:
; Destruccion de los cristales y terminar el juego.
; IN: 
;      
; MODIFICA: 
;--------------------------------------------------------------
mundo_levelFinal:

    call mundo_cristales


ret


.ends

;Musica:
;.incbin "Resources\Ace of Aces - 06 - Game Over.vgm"


;.include "Resources/PSGlib.inc"             ; Libreira SFX para la musica (c) sverx.
.include "datosMundo.inc" 					; Fichero que contiene todos los datos necesarios para nuestro juego (sprites, mapas, indices de tile etc)