;=====================================================
; 			DATOS DEL MUNDO
;=====================================================
.bank 1 slot 1

.orga $4000

.section "DATOS MUNDO" FREE

;VALORES DE INICIALIZACION DEL VDP
;Bit  76543210
;	  |||||||| 
	; 00000110 Registro 0 - Activamos el modo 4 de visualizacion y la altura extra ($06)
	; 10100001 Registro 1 - Activamos el VBlank (interrupcion de frame),  zoom(x2 tam) en los sprites y no activamos al visualizacion de pantalla ($A5)
	; 11111111 Registro 2 - Direcc base del nombre de la tabla por defecto $3800 ($FF)
	; 11111111 Registro 3 - No se usa. Todos los bits a 1 ($FF)
	; 11111111 Registro 4 - No se usa. Todos los bits a 1 $FF)
	; 11111111 Registro 5 - Direcc base de la tabla con info de los sprites, direcc por defecto $3F00 ($FF)
	; 11111111 Registro 6 - Usamos los tiles definidos a partir de la direcc $2000. ($FF)
	; 11110011 Registro 7 - Color 3 a usar en el borde/fondo. Este color se toma de la paleta de colores de los sprites
	; 00000000 Registro 8 - Valor del scroll horizontal a 0 ($00)
	; 00000000 Registro 9 - Valor del scroll vertical a 0 ($00)
	; 11111111 Registro 10 - Desact las interrup de contador de linea ($FF)
VDPDatos:
.db $06,$80,$a0,$81,$ff,$82,$ff,$83,$ff,$84,$ff,$85,$ff,$86,$f1,$87,$00,$88,$00,$89,$ff,$8a
VDPDatosEnd:


;PALETEAS SPRITES
; Colores paleta player
PaletaSprite:
.db $15 $00 $2B $01 $04 $06 $02
PaletaSpriteEnd:


; COlores paleta enemigos basicos
PaletaEnemBas:
.db $05 $3F $15 $00 $2A
PaletaEnemBasEnd:

; ASSETS SPRITE PLAYER
; Fichero con los tiles del sprite del player
tilesSprite .include "Resources\sprites\soldadoWW2(Quieto) (tiles).inc"

; Indices de tiles (charcodes) para el sprite del player a dibujar.
playerccs .db 0 1 2 3 4 5 6 7 8

; ASSETS SPRITE BALA
tilesSpriteDisparo .include "Resources\sprites\bala (tiles).inc"

balaccs .db 9 10

; PLAYER IZQUIERDA
tilesSpriteIzq .include "Resources\sprites\soldadoWW2(QuietoI) (tiles).inc"

; Indices de tiles (charcodes) para el sprite del player a dibujar.
playerIccs .db 11 12 13 14 15 16 17 18 19

; PLAYER SALTO DERECHA
tilesSpriteS .include "Resources\sprites\soldadoWW2(Salto-CorrerD) (tiles).inc"

playerSccs .db 20 21 22 23 24 25 26 27 28

; PLAYER SALTO IZQUIERDA
tilesSpriteSI .include "Resources\sprites\soldadoWW2(Salto-CorrerI) (tiles).inc"

playerSIccs .db 29 30 31 32 33 34 35 36 37

; PLAYER CAIDA DERECHA
tilesSpriteC .include "Resources\sprites\soldadoWW2(Correr2) (tiles).inc"

playerCccs .db 38 39 40 41 42 43 44 45 46

; PLAYER CAIDA IZQUIERDA
tilesSpriteCI .include "Resources\sprites\soldadoWW2(Correr2I) (tiles).inc"

playerCIccs .db 47 48 49 50 51 52 53 54 55

; ASSETS SPRITE BALA IQUIERDA
tilesSpriteDisparoIzqd .include "Resources\sprites\balaIzqd (tiles).inc"

balaIzqdccs .db 56 57

; PLAYER CORRER 1 derecha
tilesSpriteRD .include "Resources\sprites\soldadoWW2(Correr3) (tiles).inc"

playerRDccs .db 58 59 60 61 62 63 64 65 66

; PLAYER CORRER 2 derecha
tilesSpriteRD2 .include "Resources\sprites\soldadoWW2(Correr2) (tiles).inc"

playerRD2ccs .db 67 68 69 70 71 72 73 74 75

; ASSETS SPRITE ENEMIGO
tilesSpriteEnemigo .include "Resources\sprites\esqueleto(Quieto) (tiles).inc"

enemigccs .db 76 77 78 79 80 81 82 83 84 85 86 87

; ASSETS SPRITE ENEMIGO IZQUIERDA
tilesSpriteEnemigoIzqd .include "Resources\sprites\esqueleto(QuietoI) (tiles).inc"

enemIzqdccs .db 88 89 90 91 92 93 94 95 96 97 98 99

; ASSETS SPRITE ENEMIGO ANDAR DERECHA 1
tilesSpriteEnemAndar1 .include "Resources\sprites\esqueleto(andar1) (tiles).inc"

enemigAndar1ccs .db 100 101 102 103 104 105 196 107 108 109 110 111

; ASSETS SPRITE ENEMIGO ANDAR DERECHA 2
tilesSpriteEnemAndar2 .include "Resources\sprites\esqueleto(andar4) (tiles).inc"

enemigAndar2ccs .db 112 113 114 115 116 117 118 119 120 121 122 123

; ASSETS SPRITE NUMEROS
tilesSpriteNumeros .include "Resources\sprites\characters_blue (tiles).inc"

numeroccs .db 124 125 126 127 128 129 130 131 132 133

; ASSETS SPRITE ENEMIGO ANDAR DERECHA 1 IZQUIERDA
tilesSpriteEnemAndar1I .include "Resources\sprites\esqueleto(andar1I) (tiles).inc"

enemigAndar1Iccs .db 134 135 136 137 138 139 140 141 142 143 144 145

; ASSETS SPRITE ENEMIGO ANDAR DERECHA 2 IZQUIERDA
tilesSpriteEnemAndar2I .include "Resources\sprites\esqueleto(andar4I) (tiles).inc"

enemigAndar2Iccs .db 146 147 148 149 150 151 152 153 154 155 156 157

; ASSETS SPRITE VIDA
tilesSpriteVida .include "Resources\sprites\corazon8x8 (tiles).inc"

vidaccs .db 158

; ASSETS SPRITE LLAVE
tilesSpriteLlave .include "Resources\sprites\llave (tiles).inc"

llaveccs .db 159 160 161 162

; ASSETS SPRITE PUERTA CERRADA
tilesSpritePuertaA .include "Resources\sprites\puertaCerrada_resized (tiles).inc"

puertaAccs .db 163 164 165 166 167 168 169 170 171

; ASSETS SPRITE ENEMIGO AVANZADO
tilesSpriteAvanzado .include "Resources\sprites\avanzado(Quieto) (tiles).inc"

enemigAvanzccs .db 172 173 174 175 176 177 178 179 180

; ASSETS SPRITE ENEMIGO AVANZADO IZQUIERDA
tilesSpriteAvanzIzqd .include "Resources\sprites\avanzado(QuietoI) (tiles).inc"

enemAvanzIzqdccs .db 181 182 183 184 185 186 187 188 189


; ASSETS SPRITE CRISTALES
tilesSpriteCristal  .include "Resources\sprites\cristal (tiles).inc"

cristalccs .db 190 191




; ZONA DE DATOS ROM

; CONTADOR DE RELOJ Y DE VIDAS
init_contadores:
	; RELOJ
	.db 1
	.db 0
	.db 226
	.db 7
	.db 3	 				; EL ancho del sprite indica la cantidad de numeros a salir (3 DIGITOS)
	.db 1
	.db 0
	.db 24
	.db 8
	.db 0
	.dw $C00B
	.dw $c096
	.dw $C097 
	.db 0

	; NUMERO VIDAS
	.db 1
	.db 0
	.db 42
	.db 7
	.db 1	 				; Solo queremos un digito para representar las vidas
	.db 1
	.db 0
	.db 8
	.db 8
	.db 0
	.dw $C00E
	.dw $c09C
	.dw $C09D
	.db 0



;---------------------------------- MENU ---------------------------------------------
PaletaMenu:
.db $00 $00 $00 $00 $00 $3F $3F $3F $3F $3F $0B
PaletaMenuEnd:

tilesMenu .include "Resources\Mapas\menu (tiles).inc"
tilemapMenu  .include "Resources\Mapas\menu (tilemap).inc"


;---------------------------------- MUERTE ---------------------------------------------
PaletaMuerte:
.db $00 $02 $03 $3F $17 $01
PaletaMuerteEnd:

tilesMuerte .include "Resources\Mapas\muerte (tiles).inc"
tilemapMuerte  .include "Resources\Mapas\muerte (tilemap).inc"



; --------------------------------- LEVEL 01 -----------------------------------------
; ASSETS LEVEL 1
PaletaFondo: ; Colores para el fondo
.db $01 $00 $2A $24 $05 $14 $3F
PaletaColoresEnd:

tilesLevel01 .include "Resources\Mapas\level01(v2) (tiles).inc"
tilemapLevel01  .include "Resources\Mapas\level01(v2) (tilemap).inc"

; Datos de inicializacion de game objects al ejecutar el juego o al reiniciar
init_game_objects:
	;PLAYER
	.db ent_viva 			; Estado
	.db play_id 			; ID
	.db 16 					; Pos x 
	.db 16 					; Pos y
	.db play_w 				; Ancho tiles
	.db play_h 				; Alto tiles
	.db play_dir 			; Direc
	.db play_wP 			; Ancho pixeles
	.db play_hP 			; Alto pixeles
	.db ent_no_colision 	; Colision
	.dw playerPV 			; Pos vertical 
	.dw playerPH 			; Pos horiz
	.dw playercc 			; Pos cc
	.db 1 					; Vidas

	; BALA PLAYER
	.db ent_muerta
	.db bal_id
	.db bal_x
	.db bal_y
	.db bal_w
	.db bal_h
	.db bal_dir
	.db bal_wP
	.db bal_hP
	.db ent_no_colision
	.dw balaPV
	.dw balaPH
	.dw balacc
	.db 0

	vidas1:
	; VIDA
	.db ent_viva
	.db vida_id
	.db 56
	.db 160
	.db 1
	.db 1
	.db 0
	.db 8
	.db 8
	.db ent_no_colision
	.dw $c03d
	.dw $c0fa
	.dw $c0fb
	.db 0

	; PUERTA ABIERTA
	.db ent_viva
	.db puerta_id
	.db 216
	.db 48
	.db 3
	.db 3
	.db 0
	.db 24
	.db 24
	.db ent_no_colision
	.dw puertaPV 
	.dw puertaPH
	.dw puertaCC
	.db 0


; --------------------------------- LEVEL 02 -----------------------------------------
; Inicializacion de los game_objects del nivel 2
init_game_objects2:

	; PUERTA ABIERTA
	.db ent_viva
	.db puerta_id
	.db 220
	.db 152
	.db 3
	.db 3
	.db 0
	.db 24
	.db 24
	.db ent_no_colision
	.dw puertaPV 
	.dw puertaPH
	.dw puertaCC
	.db 0

	; VIDA
	vida2:
	.db ent_viva
	.db vida_id
	.db 20
	.db 90
	.db 1
	.db 1
	.db 0
	.db 8
	.db 8
	.db ent_no_colision
	.dw $c03d
	.dw $c0fa
	.dw $c0fb
	.db 0


; --------------------------------- LEVEL 03 -----------------------------------------
init_var_extra_enemBas3:
	; ENEMIGO BASICO 1
	.db 0 					; Estado IA
	.db 0 					; TIempo estado 1
	.db 0 					; Tiempo estado 2
	.db 180 				; Pos x inicial
	.db 1 					; vel mov
	.db 40 					; Distanc recorr
	.db 0 					; Bala usar
	.db 0  					; Cambio de sprite

; Inicializacion de los game_objects del nivel 3
init_game_objects3:
	 ;ENEMIGO BASICO 1
	.db enem_estado
	.db enem_id
	.db 185
	.db 144
	.db enem_w
	.db enem_h
	.db 0
	.db enem_wP
	.db enem_hP
	.db enem_colision
	.dw $C018
	.dw $c0B0
	.dw $C0B1 
	.db enem_vida

	puerta3:
	; PUERTA ABIERTA
	.db ent_viva
	.db puerta_id
	.db 216
	.db 40
	.db 3
	.db 3
	.db 0
	.db 24
	.db 24
	.db ent_no_colision
	.dw puertaPV 
	.dw puertaPH
	.dw puertaCC
	.db 0

	; LLAVE
	.db 1
	.db llave_id
	.db 230
	.db 150
	.db 2
	.db 2
	.db 0
	.db 16
	.db 16
	.db 0
	.dw $c039
	.dw $c0f2
	.dw $c0f3
	.db 0

; --------------------------------- LEVEL 04-----------------------------------------
init_var_extra_enemBas4:
	; ENEMIGO BASICO 1
	.db 0 					; Estado IA
	.db 0 					; TIempo estado 1
	.db 0 					; Tiempo estado 2
	.db 135 				; Pos x inicial
	.db 1 					; vel mov
	.db 80 					; Distanc recorr
	.db 0 					; Bala usar
	.db 0  					; Cambio de sprite

	; ENEMIGO BASICO 2
	.db 1
	.db 0
	.db 0
	.db 112
	.db 1
	.db 70
	.db 0
	.db 0


init_game_objects4:
	; ENEMIGO BASICO 1
	.db enem_estado
	.db enem_id
	.db 135
	.db 70
	.db enem_w
	.db enem_h
	.db enem_dir
	.db enem_wP
	.db enem_hP
	.db enem_colision
	.dw $c00F
	.dw $C09E
	.dw $c09F
	.db enem_vida

	; ENEMIGO BASICO 2
	.db enem_estado
	.db enem_id
	.db 112
	.db 144
	.db enem_w
	.db enem_h
	.db enem_dir
	.db enem_wP
	.db enem_hP
	.db enem_colision
	.dw $C01B
	.dw $c0B6
	.dw $C0B7 
	.db enem_vida

	puerta4:
	; PUERTA ABIERTA
	.db ent_viva
	.db puerta_id
	.db 10
	.db 151
	.db 3
	.db 3
	.db 0
	.db 24
	.db 24
	.db ent_no_colision
	.dw puertaPV 
	.dw puertaPH
	.dw puertaCC
	.db 0

	; LLAVE
	.db 1
	.db llave_id
	.db 230
	.db 150
	.db 2
	.db 2
	.db 0
	.db 16
	.db 16
	.db 0
	.dw $c039
	.dw $c0f2
	.dw $c0f3
	.db 0


; --------------------------------- LEVEL 05 -----------------------------------------
init_var_extra_enemBas5:
	; ENEMIGO BASICO 1
	.db 0 					; Estado IA
	.db 0 					; TIempo estado 1
	.db 0 					; Tiempo estado 2
	.db 75					; Pos x inicial
	.db 1 					; vel mov
	.db 35 					; Distanc recorr
	.db 0 					; Bala usar
	.db 0  					; Cambio de sprite

init_var_extra_enemAv5:
	; ENEMIGO AV 1
	.db 1
	.db 0
	.db 0
	.db 120
	.db 1
	.db 60
	.db 2
	.db 0

init_balas5:
	; BALA ENEMIGOS
	.db bal_estado
	.db bal_id
	.db bal_x
	.db bal_y
	.db bal_w
	.db bal_h
	.db bal_dir
	.db bal_wP
	.db bal_hP
	.db bal_colision
	.dw $C020
	.dw $C0C0
	.dw $C0C1
	.db bal_vida


init_game_objects5:
	; ENEMIGO BASICO 1
	.db enem_estado
	.db enem_id
	.db 75
	.db 80
	.db enem_w
	.db enem_h
	.db enem_dir
	.db enem_wP
	.db enem_hP
	.db enem_colision
	.dw $C01B
	.dw $c0B6
	.dw $C0B7
	.db enem_vida

	avanzado5:
	; ENEMIGO AVANZADO 1
	.db enem_estado
	.db enemigoAV_id
	.db 120
	.db 127
	.db 3
	.db 3
	.db enem_dir
	.db enem_wP
	.db enemAv_hP
	.db enem_colision
	.dw $c00F
	.dw $C09E
	.dw $c09F
	.db avanz_vida

	puerta5:
	; PUERTA ABIERTA
	.db ent_viva
	.db puerta_id
	.db 17
	.db 144
	.db 3
	.db 3
	.db 0
	.db 24
	.db 24
	.db ent_no_colision
	.dw puertaPV 
	.dw puertaPH
	.dw puertaCC
	.db 0

	; LLAVE
	.db 1
	.db llave_id
	.db 129
	.db 148
	.db 2
	.db 2
	.db 0
	.db 16
	.db 16
	.db 0
	.dw $c039
	.dw $c0f2
	.dw $c0f3
	.db 0


	; VIDA
	.db ent_viva
	.db vida_id
	.db 115
	.db 98
	.db 1
	.db 1
	.db 0
	.db 8
	.db 8
	.db ent_no_colision
	.dw $c03d
	.dw $c0fa
	.dw $c0fb
	.db 0


; --------------------------------- LEVEL 06 -----------------------------------------
init_var_extra_enemBas6:
	; ENEMIGO BASICO 1
	.db 0 					; Estado IA
	.db 0 					; TIempo estado 1
	.db 0 					; Tiempo estado 2
	.db 100 				; Pos x inicial
	.db 1 					; vel mov
	.db 50 					; Distanc recorr
	.db 0 					; Bala usar
	.db 0  					; Cambio de sprite

	; ENEMIGO BASICO 2
	.db 1
	.db 0
	.db 0
	.db 112
	.db 1
	.db 70
	.db 0
	.db 0

init_var_extra_enemAv6:
	; ENEMIGO AV 1
	.db 1
	.db 0
	.db 0
	.db 130
	.db 1
	.db 70
	.db 2
	.db 0

init_balas6:
	; BALA ENEMIGOS
	.db bal_estado
	.db bal_id
	.db bal_x
	.db bal_y
	.db bal_w
	.db bal_h
	.db bal_dir
	.db bal_wP
	.db bal_hP
	.db bal_colision
	.dw $C039
	.dw $C0F2
	.dw $C0F3
	.db bal_vida


init_game_objects6:
	; ENEMIGO BASICO 1
	.db enem_estado
	.db enem_id
	.db 100
	.db 103
	.db enem_w
	.db enem_h
	.db enem_dir
	.db enem_wP
	.db enem_hP
	.db enem_colision
	.dw $c00F
	.dw $C09E
	.dw $c09F
	.db enem_vida

	; ENEMIGO BASICO 2
	.db enem_estado
	.db enem_id
	.db 112
	.db 150
	.db enem_w
	.db enem_h
	.db enem_dir
	.db enem_wP
	.db enem_hP
	.db enem_colision
	.dw $C01B
	.dw $c0B6
	.dw $C0B7 
	.db enem_vida

	avanzado6:
	; ENEMIGO AVANZADO 1
	.db enem_estado
	.db enemigoAV_id
	.db 130
	.db 14
	.db 3
	.db 3
	.db enem_dir
	.db enem_wP
	.db enemAv_hP
	.db enem_colision
	.dw $c027
	.dw $C0CE
	.dw $c0CF
	.db avanz_vida

	puerta6:
	; PUERTA ABIERTA
	.db ent_viva
	.db puerta_id
	.db 20
	.db 15
	.db 3
	.db 3
	.db 0
	.db 24
	.db 24
	.db ent_no_colision
	.dw $c030 
	.dw $c0e0
	.dw $c0e1
	.db 0

	; VIDA
	vida6:
	.db ent_viva
	.db vida_id
	.db 110
	.db 120
	.db 1
	.db 1
	.db 0
	.db 8
	.db 8
	.db ent_no_colision
	.dw $c03d
	.dw $c0fa
	.dw $c0fb
	.db 0

; --------------------------------- LEVEL 07 -----------------------------------------
init_var_extra_enemBas7:
	; ENEMIGO BASICO 1
	.db 0 					; Estado IA
	.db 0 					; TIempo estado 1
	.db 0 					; Tiempo estado 2
	.db 60					; Pos x inicial
	.db 1 					; vel mov
	.db 60 					; Distanc recorr
	.db 0 					; Bala usar
	.db 0  					; Cambio de sprite

init_var_extra_enemAv7:
	; ENEMIGO AV 1
	.db 1
	.db 0
	.db 0
	.db 110
	.db 1
	.db 50
	.db 2
	.db 0


init_game_objects7:
	; ENEMIGO BASICO 1
	.db enem_estado
	.db enem_id
	.db 60
	.db 142
	.db enem_w
	.db enem_h
	.db enem_dir
	.db enem_wP
	.db enem_hP
	.db enem_colision
	.dw $C01B
	.dw $c0B6
	.dw $C0B7
	.db enem_vida


	avanzado7:
	; ENEMIGO AVANZADO 1
	.db enem_estado
	.db enemigoAV_id
	.db 110
	.db 15
	.db 3
	.db 3
	.db enem_dir
	.db enem_wP
	.db enemAv_hP
	.db enem_colision
	.dw $c00F
	.dw $C09E
	.dw $c09F
	.db avanz_vida

; --------------------------------- LEVEL 08 -----------------------------------------
init_game_objects8:
	; VIDA
	.db ent_viva
	.db vida_id
	.db 20
	.db 110
	.db 1
	.db 1
	.db 0
	.db 8
	.db 8
	.db ent_no_colision
	.dw $c03d
	.dw $c0fa
	.dw $c0fb
	.db 0


; --------------------------------- LEVEL 09 -----------------------------------------
init_var_extra_enemBas9:
	; ENEMIGO BASICO 1
	.db 0 					; Estado IA
	.db 0 					; TIempo estado 1
	.db 0 					; Tiempo estado 2
	.db 125 				; Pos x inicial
	.db 1 					; vel mov
	.db 33 					; Distanc recorr
	.db 0 					; Bala usar
	.db 0  					; Cambio de sprite

	; ENEMIGO BASICO 2
	.db 1
	.db 0
	.db 0
	.db 95
	.db 1
	.db 38
	.db 0
	.db 0

init_var_extra_enemAv9:
	; ENEMIGO AV 1
	.db 1
	.db 0
	.db 0
	.db 50
	.db 1
	.db 50
	.db 2
	.db 0

init_balas9:
	; BALA ENEMIGOS
	.db bal_estado
	.db bal_id
	.db bal_x
	.db bal_y
	.db bal_w
	.db bal_h
	.db bal_dir
	.db bal_wP
	.db bal_hP
	.db bal_colision
	.dw $C039
	.dw $C0F2
	.dw $C0F3
	.db bal_vida

init_game_objects9:
	; ENEMIGO BASICO 1
	.db enem_estado
	.db enem_id
	.db 125
	.db 16
	.db enem_w
	.db enem_h
	.db enem_dir
	.db enem_wP
	.db enem_hP
	.db enem_colision
	.dw $c00F
	.dw $C09E
	.dw $c09F
	.db enem_vida

	; ENEMIGO BASICO 2
	.db enem_estado
	.db enem_id
	.db 95
	.db 72
	.db enem_w
	.db enem_h
	.db enem_dir
	.db enem_wP
	.db enem_hP
	.db enem_colision
	.dw $C01B
	.dw $c0B6
	.dw $C0B7 
	.db enem_vida

	avanzado9:
	; ENEMIGO AVANZADO 1
	.db enem_estado
	.db enemigoAV_id
	.db 50
	.db 134
	.db 3
	.db 3
	.db enem_dir
	.db enem_wP
	.db enemAv_hP
	.db enem_colision
	.dw $c027
	.dw $C0CE
	.dw $c0CF
	.db avanz_vida

	cristal9:
	;CRISTAL 1
	.db ent_viva
	.db cris_id
	.db 20
	.db 143
	.db cris_w
	.db cris_h
	.db cris_d
	.db cris_wP
	.db cris_hP
	.db ent_no_colision
	.dw $c030 
	.dw $c0e0
	.dw $c0e1
	.db cris_vida

	;CRISTAL 2
	.db ent_viva
	.db cris_id
	.db 195
	.db pos_iniCristal
	.db cris_w
	.db cris_h
	.db cris_d
	.db cris_wP
	.db cris_hP
	.db ent_no_colision
	.dw $c032 
	.dw $c0e4
	.dw $c0e5
	.db cris_vida

	;CRISTAL 3
	.db ent_viva
	.db cris_id
	.db 228
	.db 26
	.db cris_w
	.db cris_h
	.db cris_d
	.db cris_wP
	.db cris_hP
	.db ent_no_colision
	.dw $c034 
	.dw $c0e8
	.dw $c0e9
	.db cris_vida






; MAPAS
; DATOS PARA TODOS LOS NIVELES DEL JUEGO (Colisiones, tilemap a usar, Nº enem etc)
init_mapas:
	; LEVEL 01
	.dw tilemapLevel01
	.db $0B 				; Colision suelo
	.db 00 					; Enemigos basicos
	.db 00 					;   "      avanzados
	.db 00 					; Llaves
	.db $07 				; Colision pared 1
	.db $09 				; Colision pared 2

	; LEVEL 02
	.dw tilemapMapa2
	.db $18
	.db 00
	.db 00
	.db 00
	.db $17
	.db $15

	; LEVEL 03
	.dw tilemapMapa3
	.db $26
	.db 01
	.db 00
	.db 01
	.db $23
	.db $25

	; LEVEL 04
	.dw tilemapMapa4
	.db $52
	.db 02
	.db 00
	.db 01
	.db $4F
	.db $51

	; LEVEL 05
	.dw tilemapMapa5
	.db $61
	.db 01
	.db 01
	.db 01
	.db $5F
	.db $5D

	; LEVEL 06
	.dw tilemapMapa6
	.db $6E
	.db 02
	.db 01
	.db 00
	.db $6D
	.db $6B

	; LEVEL 07
	.dw tilemapMapa7
	.db $7C
	.db 01
	.db 01
	.db 01
	.db $77
	.db $7A

	; LEVEL 08
	.dw tilemapMapa8
	.db $87
	.db 00
	.db 00
	.db 00
	.db $85
	.db $87

	; LEVEL 09
	.dw tilemapMapa9
	.db $93
	.db 02
	.db 01
	.db 00
	.db $91
	.db $93

; --------------------------------- PANTALLA FIN JUEGO -----------------------------------------
PaletaFIN:
.db $00 $3F $03 $17 $01
PaletaFINEnd:

tilesEND .include "Resources\Mapas\FIN (tiles).inc"
tilemapEND .include "Resources\Mapas\FIN (tilemap).inc"


.ends




.bank 2 slot 2 		; A partir de aqui todo el codigo estara en la ranura 3 de ROM ($8000 - $DFFF)

.orga $8000


.section "MAPAS" FREE

; --------------------------------- LEVEL 02 -----------------------------------------
PaletaMapa2:
.db $01 $00 $24 $2A $05 $14 $3F
PaletaMapa2End:

tilesMapa2 .include "Resources\Mapas\level2 (tiles).inc"
tilemapMapa2 .include "Resources\Mapas\level2 (tilemap).inc"

; --------------------------------- LEVEL 03 -----------------------------------------
PaletaMapa3:
.db $01 $00 $2A $14 $05 $3F
PaletaMapa3End:

tilesMapa3 .include "Resources\Mapas\level3 (tiles).inc"
tilemapMapa3 .include "Resources\Mapas\level3 (tilemap).inc"



; --------------------------------- LEVEL 04 -----------------------------------------
PaletaMapa4:
.db $01 $00 $2A $14 $05 $3F
PaletaMapa4End:

tilesMapa4 .include "Resources\Mapas\level4 (tiles).inc"
tilemapMapa4 .include "Resources\Mapas\level4 (tilemap).inc"


; --------------------------------- LEVEL 05 -----------------------------------------
PaletaMapa5:
.db $01 $00 $2A $24 $05 $14 $3F
PaletaMapa5End:

tilesMapa5 .include "Resources\Mapas\level5 (tiles).inc"
tilemapMapa5 .include "Resources\Mapas\level5 (tilemap).inc"


; --------------------------------- LEVEL 06 -----------------------------------------
PaletaMapa6:
.db $01 $2A $00 $14 $05 $3F
PaletaMapa6End:

tilesMapa6 .include "Resources\Mapas\level6 (tiles).inc"
tilemapMapa6 .include "Resources\Mapas\level6 (tilemap).inc"


; --------------------------------- LEVEL 07 -----------------------------------------
PaletaMapa7:
.db $01 $00 $2A $24 $14 $05 $3F $1A $16
PaletaMapa7End:

tilesMapa7 .include "Resources\Mapas\level7 (tiles).inc"
tilemapMapa7 .include "Resources\Mapas\level7 (tilemap).inc"

; --------------------------------- LEVEL 08 -----------------------------------------
PaletaMapa8:
.db $39 $01 $00 $04 $1A $05 $2F $3F $16 $14
PaletaMapa8End:

tilesMapa8 .include "Resources\Mapas\level9 (tiles).inc"
tilemapMapa8 .include "Resources\Mapas\level9 (tilemap).inc"


; --------------------------------- LEVEL 09 -----------------------------------------
PaletaMapa9:
.db $28 $00 $01 $05 $04 $1A $2F $14 $18 $2A $3F $19 $2E
PaletaMapa9End:

tilesMapa9 .include "Resources\Mapas\level10 (tiles).inc"
tilemapMapa9 .include "Resources\Mapas\level10 (tilemap).inc"



.ends