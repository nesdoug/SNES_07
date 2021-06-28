; example 7 SNES code

.p816
.smart


.include "regs.asm"
.include "variables.asm"
.include "macros.asm"
.include "init.asm"
.include "library.asm"





.segment "CODE"

; enters here in forced blank
Main:
.a16 ; the setting from init code
.i16
	phk
	plb
	
	
; COPY PALETTES to PAL_BUFFER	
;	BLOCK_MOVE  length, src_addr, dst_addr
	BLOCK_MOVE  512, BG_Palette, PAL_BUFFER
	A8 ;block move will put AXY16. Undo that.
	
; DMA from PAL_BUFFER to CGRAM
	jsr DMA_Palette ; in init.asm	
	
	
; DMA from Tiles to VRAM	
	lda #V_INC_1 ; the value $80
	sta VMAIN  ; $2115 = set the increment mode +1

	DMA_VRAM  (End_BG_Tiles - BG_Tiles), BG_Tiles, $0000
	
	
; DMA from Tiles2 to VRAM $3000
	
	DMA_VRAM  (End_HUD_Tiles - HUD_Tiles), HUD_Tiles, $3000
	
	
; DMA from Spr_Tiles to VRAM $4000
	
	DMA_VRAM  (End_Spr_Tiles - Spr_Tiles), Spr_Tiles, $4000
	
	
; DMA from Map1 to VRAM	$6000 
	
	DMA_VRAM  $700, Map1, $6000


; DMA from Map3 to VRAM	$7000
	
	DMA_VRAM  $700, Map3, $7000	
	
	
	
	
; a is still 8 bit.
	lda #1|BG3_TOP ; mode 1, tilesize 8x8 all
	sta BGMODE ; $2105
	
; 210b = tilesets for bg 1 and bg 2
; (210c for bg 3 and bg 4)
; steps of $1000 -321-321... bg2 bg1
	stz BG12NBA ; $210b BG 1 and 2 TILES at VRAM address $0000
	lda #$03
	sta BG34NBA ; $210c BG3 TILES at VRAM address $3000
	
	; 2107 map address bg 1, steps of $400... -54321yx
	; y/x = map size... 0,0 = 32x32 tiles
	; $6000 / $100 = $60
	lda #$60 ; bg1 map at VRAM address $6000
	sta BG1SC ; $2107
	
	lda #$68 ; bg2 map at VRAM address $6800
	sta BG2SC ; $2108
	
	lda #$70 ; bg3 map at VRAM address $7000
	sta BG3SC ; $2109	
	
	
;$2101 sssnn-bb
;sss = sprite sizes, 000 = 8x8 and 16x16 sprites
;nn = displacement for the 2nd set of sprite tiles, 00 = normal
;-bb = where are the sprite tiles, in steps of $2000
;that upper bit is useless, as usual, so I marked it with a dash -
	lda #2 ;sprite tiles at $4000
	sta OBSEL ;= $2101

;allow everything on the main screen	
	lda #ALL_ON_SCREEN ; $1f
	sta TM ; $212c
	
	;turn on NMI interrupts and auto-controller reads
	lda #NMI_ON|AUTO_JOY_ON
	sta NMITIMEN ;$4200
	
	lda #FULL_BRIGHT ; $0f = turn the screen on, full brighness
	sta INIDISP ; $2100

;set initial values
	stz ball_active
	
	lda #$10
	sta paddle1_x
	lda #$e8
	sta paddle2_x
	
	lda #$70
	sta paddle1_y
	sta paddle2_y
	
	
	
	
	
Infinite_Loop:	
;game loop
	A8
	XY16
	jsr Wait_NMI ;wait for the beginning of v-blank
	jsr DMA_OAM  ;copy the OAM_BUFFER to the OAM
	jsr Print_Score
	jsr Pad_Poll ;read controllers
	jsr Clear_OAM
;	A8
	
	inc frame_count
	
	
	AXY16
	
	lda pad1
	and #KEY_UP
	beq @not_up
@up:
	A8
	lda paddle1_y
	cmp #$20 ;max up
	beq @not_up ;too far up
	bcc @not_up
	
	dec paddle1_y
	dec paddle1_y
	
	dec paddle2_y
	dec paddle2_y

@not_up:
	A16
	lda pad1
	and #KEY_DOWN
	beq @not_down
@down:
	A8
	lda paddle1_y
	cmp #$9E ;max down
	bcs @not_down ;too far down
	inc paddle1_y
	inc paddle1_y
	
	inc paddle2_y
	inc paddle2_y

@not_down:
	A8
	
	lda ball_active
	bne @skip_start
	A16
	lda pad1
	and #KEY_START
	beq @skip_start
;if either score is 9, reset to zero	
	A8
	lda points_L
	cmp #9
	bcc @ok
	jsr Reset_Score
@ok:
	lda points_R
	cmp #9
	bcc @ok2
	jsr Reset_Score
@ok2:
	
;start a new ball	
	A8
	lda #1
	sta ball_active
	
	lda #$74
	sta ball_x
	sta ball_y
	lda frame_count
	and #1
	bne @sp_x
	lda #$ff ;-1
@sp_x:
	sta ball_x_speed
	
	lda frame_count
	lsr a
	and #1
	bne @sp_y
	lda #$ff ;-1
@sp_y:
	sta ball_y_speed
	
	
@skip_start:
	
	
	
	
;move ball
	A8
	lda ball_active
	bne @ball_active
	jmp ball_not_active
	
@ball_active:
	lda ball_x
	clc
	adc ball_x_speed
	sta ball_x
	
	lda ball_y
	clc
	adc ball_y_speed
	sta ball_y
;bounce off ceilings	
	cmp #$20
	bcs @above20
	lda #$20
	sta ball_y
	lda #1
	sta ball_y_speed
	jmp @ball_done
	
@above20:
;bounce off floor
	lda ball_y
	cmp #$c7
	bcc @ball_done
	lda #$c7
	sta ball_y
	lda #$ff ; -1
	sta ball_y_speed
;	jmp @ball_done

@ball_done:	


;check collision left
	A8
	lda paddle1_x
	sta obj1x
	lda #PADDLE_W
	sta obj1w
	lda paddle1_y
	sta obj1y
	lda #PADDLE_H
	sta obj1h
	lda ball_x
	sta obj2x
	lda #BALL_SIZE
	sta obj2w
	lda ball_y
	sta obj2y
	lda #BALL_SIZE
	sta obj2h
	
	jsr Check_Collision
	A8
	lda collision ;1 or 0
	beq @no_collision

;is left paddle more left than ball?
	lda paddle1_x
	cmp ball_x
	bcs @no_collision
;make ball go right	
	lda #1
	sta ball_x_speed
	
@no_collision:
;check collision right
	A8
	lda paddle2_x
	sta obj1x
	lda #PADDLE_W
	sta obj1w
	lda paddle2_y
	sta obj1y
	lda #PADDLE_H
	sta obj1h
;	skip ball, still loaded

	jsr Check_Collision
	A8
	lda collision ;1 or 0
	beq @past_collisions
;is left paddle more right than ball?
	lda paddle2_x
	cmp ball_x
	beq @past_collisions
	bcc @past_collisions
;make ball go left	
	lda #$ff ;-1
	sta ball_x_speed


@past_collisions:

;check lose left
	A8
	lda ball_x
	cmp #$4
	bcs @above4
	inc points_R
	stz ball_active
	
@above4:

;check lose right
	lda ball_x
	cmp #$fb
	bcc @belowfc
	inc points_L
	stz ball_active
	
@belowfc:

ball_not_active:

	jsr Draw_Sprites
	jmp Infinite_Loop
	
	
	
Reset_Score:
	stz points_L
	stz points_R
	rts
	
	
Draw_Sprites:
	php
	
;left paddle
	A8
	stz sprid
	
	stz spr_x+1 ;9th bit of X
	lda paddle1_x
	sta spr_x
	lda paddle1_y
	sta spr_y
	A16
	lda #.loword(Meta_00) ;left paddle
	ldx #^Meta_00
	jsr OAM_Meta_Spr
		
;right paddle
	A8
	stz spr_x+1
	lda paddle2_x
	sta spr_x
	lda paddle2_y
	sta spr_y
	A16
	lda #.loword(Meta_01) ;right paddle
	ldx #^Meta_01
	jsr OAM_Meta_Spr
	
	A8
	lda ball_active
	beq @skip_ball
;ball

; spr_x - x (9 bit)
; spr_y - y
; spr_c - tile #
; spr_a - attributes, flip, palette, priority
; spr_sz = sprite size, 0 or 2
	lda ball_x
	sta spr_x
	lda ball_y
	sta spr_y 
	lda #2
	sta spr_c
	lda #SPR_PAL_5|SPR_PRIOR_2
	sta spr_a
	stz spr_sz ;8x8 
	jsr OAM_Spr
	
@skip_ball:
	plp
	rts
	
	
	

	
	
	
Print_Score:
	php
;we should be in v-blank
	A8
	lda #V_INC_32 ;downward increment
	sta VMAIN ;$2115

;print left score
	A16
	XY8
	ldx #12
	ldy #1
	jsr Map_Offset ; returns a16 = vram address offset
	clc
	adc #$7000 ;layer 3 map
	sta VMADDL ;$2116
	A8
	lda points_L
	clc
	adc #$10
	A16
	and #$00ff ;blank the upper byte, = palette 0
	sta VMDATAL ;$2118
	clc
	adc #$0010
	sta VMDATAL ;$2118
	
;print right score
	A16
	XY8
	ldx #19
	ldy #1
	jsr Map_Offset ; returns a16 = vram address offset
	clc
	adc #$7000 ;layer 3 map
	sta VMADDL ;$2116
	A8
	lda points_R
	clc
	adc #$10
	A16
	and #$00ff ;blank the upper byte, = palette 0
	sta VMDATAL
	clc
	adc #$0010
	sta VMDATAL
	
	plp
	rts
	
	
	
Wait_NMI:
.a8
.i16
;should work fine regardless of size of A
	lda in_nmi ;load A register with previous in_nmi
@check_again:	
	WAI ;wait for an interrupt
	cmp in_nmi	;compare A to current in_nmi
				;wait for it to change
				;make sure it was an nmi interrupt
	beq @check_again
	rts

	
	
Pad_Poll:
.a8
.i16
; reads both controllers to pad1, pad1_new, pad2, pad2_new
; auto controller reads done, call this once per main loop
; copies the current controller reads to these variables
; pad1, pad1_new, pad2, pad2_new (all 16 bit)
	php
	A8
@wait:
; wait till auto-controller reads are done
	lda $4212
	lsr a
	bcs @wait
	
	A16
	lda pad1
	sta temp1 ; save last frame
	lda $4218 ; controller 1
	sta pad1
	eor temp1
	and pad1
	sta pad1_new
	
	lda pad2
	sta temp1 ; save last frame
	lda $421a ; controller 2
	sta pad2
	eor temp1
	and pad2
	sta pad2_new
	plp
	rts
	


	
	
.include "Sprites/metasprites.asm"	
.include "header.asm"	



.segment "RODATA1"

BG_Palette:
; 256 bytes
.incbin "Background/Background.pal"
Spr_Palette:
; 256 bytes
.incbin "Sprites/sprites.pal"

BG_Tiles:
.incbin "Background/Grad.chr"
End_BG_Tiles:

HUD_Tiles:
.incbin "Background/HUD.chr"
End_HUD_Tiles:

Spr_Tiles:
.incbin "Sprites/sprites.chr"
End_Spr_Tiles:

.segment "RODATA2"

Map1:
.incbin "Background/Grad2.map"

Map3:
.incbin "Background/HUD.map"
