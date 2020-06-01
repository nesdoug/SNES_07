; example 7 SNES code

.p816
.smart


BALL_SIZE = 7
PADDLE_W = 7
PADDLE_H = 47



.segment "ZEROPAGE"

temp1: .res 2
temp2: .res 2
temp3: .res 2
temp4: .res 2
sprid: .res 1
spr_x: .res 1 ; for sprite setting code
spr_y: .res 1
spr_c: .res 1
spr_a: .res 1
spr_h: .res 1 ; high 2 bits
spr_x2:	.res 2 ; for meta sprite code
spr_y2: .res 1
spr_h2:	.res 1
spr_pri: .res 1 ; priority

; for collision code
obj1x: .res 1
obj1w: .res 1
obj1y: .res 1
obj1h: .res 1
obj2x: .res 1
obj2w: .res 1
obj2y: .res 1
obj2h: .res 1
collision: .res 1

pad1: .res 2
pad1_new: .res 2
pad2: .res 2
pad2_new: .res 2
in_nmi: .res 2

ball_active: .res 1
ball_x: .res 1
ball_y: .res 1
ball_x_speed: .res 1
ball_y_speed: .res 1

paddle1_x: .res 1
paddle1_y: .res 1

paddle2_x: .res 1
paddle2_y: .res 1

points_L: .res 1
points_R: .res 1

game_pause: .res 1
frame_count: .res 1




.segment "BSS"

PAL_BUFFER: .res 512

OAM_BUFFER: .res 512 ;low table
OAM_BUFFER2: .res 32 ;high table



.include "defines.asm"
.include "macros.asm"
.include "init.asm"
.include "library.asm"






.segment "CODE"

; enters here in forced blank
main:
.a16 ; just a standardized setting from init code
.i16
	phk
	plb
	jsr oam_clear
	
	
; COPY PALETTES to PAL_BUFFER	
;	BLOCK_MOVE  length, src_addr, dst_addr
	BLOCK_MOVE  512, BG_Palette, PAL_BUFFER
	
	
; DMA from PAL_BUFFER to CGRAM
	A8
	stz pal_addr ; $2121 cg address = zero

	stz $4300 ; transfer mode 0 = 1 register write once
	lda #$22  ; $2122
	sta $4301 ; destination, pal data
	ldx #.loword(PAL_BUFFER)
	stx $4302 ; source
	lda #^PAL_BUFFER
	sta $4304 ; bank
	ldx #512 ; full palette size
	stx $4305 ; length
	lda #1
	sta $420b ; start dma, channel 0
	
	
; DMA from Tiles to VRAM	
	lda #V_INC_1 ; the value $80
	sta vram_inc  ; $2115 = set the increment mode +1

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
	sta bg_size_mode ; $2105
	
; 210b = tilesets for bg 1 and bg 2
; (210c for bg 3 and bg 4)
; steps of $1000 -321-321... bg2 bg1
	stz bg12_tiles ; $210b BG 1 and 2 TILES at VRAM address $0000
	lda #$03
	sta bg34_tiles ; $210c BG3 TILES at VRAM address $3000
	
	; 2107 map address bg 1, steps of $400... -54321yx
	; y/x = map size... 0,0 = 32x32 tiles
	; $6000 / $100 = $60
	lda #$60 ; bg1 map at VRAM address $6000
	sta tilemap1 ; $2107
	
	lda #$68 ; bg2 map at VRAM address $6800
	sta tilemap2 ; $2108
	
	lda #$70 ; bg3 map at VRAM address $7000
	sta tilemap3 ; $2109	
	
	
;$2101 sssnn-bb
;sss = sprite sizes, 000 = 8x8 and 16x16 sprites
;nn = displacement for the 2nd set of sprite tiles, 00 = normal
;-bb = where are the sprite tiles, in steps of $2000
;that upper bit is useless, as usual, so I marked it with a dash -
	lda #2 ;sprite tiles at $4000
	sta spr_addr_size ;= $2101

;allow everything on the main screen	
	lda #ALL_ON_SCREEN ; $1f
	sta main_screen ; $212c
	
	;turn on NMI interrupts and auto-controller reads
	lda #NMI_ON|AUTO_JOY_ON
	sta $4200
	
	lda #FULL_BRIGHT ; $0f = turn the screen on, full brighness
	sta fb_bright ; $2100

;set initial values
	stz ball_active
	
	lda #$10
	sta paddle1_x
	lda #$e8
	sta paddle2_x
	
	lda #$70
	sta paddle1_y
	sta paddle2_y
	
	
	
	
	
InfiniteLoop:	
;game loop a8 xy16
	jsr wait_nmi ;wait for the beginning of v-blank
	jsr dma_oam  ;copy the OAM_BUFFER to the OAM
	jsr print_score
	jsr pad_poll ;read controllers
	jsr oam_clear
	A8
	inc frame_count
	
	
	AXY16
	lda pad1
	and #KEY_LEFT
	beq @not_left
@left:
	A8
	dec ball_x
	
@not_left:
	A16
	lda pad1
	and #KEY_RIGHT
	beq @not_right
@right:
	A8
	inc ball_x

@not_right:
	A16
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
	jsr reset_score
@ok:
	lda points_R
	cmp #9
	bcc @ok2
	jsr reset_score
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
	
	jsr check_collision
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

	jsr check_collision
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

	jsr draw_sprites
	jmp InfiniteLoop
	
	
	
reset_score:
	stz points_L
	stz points_R
	rts
	
	
draw_sprites:
	php
	
;left paddle
	A8
	stz spr_h ;9th bit of X
	lda paddle1_x
	sta spr_x
	lda paddle1_y
	sta spr_y
	A16
	lda #.loword(Meta_00) ;left paddle
	ldx #^Meta_00
	jsr oam_meta_spr
	
;right paddle
	A8
	stz spr_h
	lda paddle2_x
	sta spr_x
	lda paddle2_y
	sta spr_y
	A16
	lda #.loword(Meta_01) ;right paddle
	ldx #^Meta_01
	jsr oam_meta_spr
	
	A8
	lda ball_active
	beq @skip_ball
;ball

; spr_x - x
; spr_y - y
; spr_c - tile #
; spr_a - attributes, flip, palette, priority
; spr_h - 0-3, optional, keep zero if not needed
	lda ball_x
	sta spr_x
	lda ball_y
	sta spr_y 
	lda #2
	sta spr_c
	lda #SPR_PAL_5|SPR_PRIOR_2
	sta spr_a
	stz spr_h ;8x8 
	jsr oam_spr
	
@skip_ball:
	plp
	rts
	
	
	
clear_sp_buffer:
.a8
.i16
	php
	A8
	XY16
	lda #224 ;put all y values just below the screen
	ldx #$0000
	ldy #128 ;number of sprites
@loop:
	sta OAM_BUFFER+1, x
	inx
	inx
	inx
	inx ;add 4 to x
	dey
	bne @loop
	plp
	rts
	
	
	
print_score:
	php
;we should be in v-blank
	A8
	lda #V_INC_32
	sta vram_inc

;print left score
	A16
	XY8
	ldx #12
	ldy #1
	jsr map_offset ; returns a16 = vram address offset
	clc
	adc #$7000 ;layer 3 map
	sta vram_addr
	A8
	lda points_L
	clc
	adc #$10
	A16
	and #$00ff ;blank the upper byte, = palette 0
	sta vram_data
	clc
	adc #$0010
	sta vram_data
	
;print right score
	A16
	XY8
	ldx #19
	ldy #1
	jsr map_offset ; returns a16 = vram address offset
	clc
	adc #$7000 ;layer 3 map
	sta vram_addr
	A8
	lda points_R
	clc
	adc #$10
	A16
	and #$00ff ;blank the upper byte, = palette 0
	sta vram_data
	clc
	adc #$0010
	sta vram_data
	
	plp
	rts
	
	
	
wait_nmi:
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

	
dma_oam:
.a8
.i16
	php
	A8
	XY16
	ldx #$0000
	stx oam_addr_L ;$2102 (and 2103)
	
	stz $4300 ; transfer mode 0 = 1 register write once
	lda #4 ;$2104 oam data
	sta $4301 ; destination, oam data
	ldx #.loword(OAM_BUFFER)
	stx $4302 ; source
	lda #^OAM_BUFFER
	sta $4304 ; bank
	ldx #544
	stx $4305 ; length
	lda #1
	sta $420b ; start dma, channel 0
	plp
	rts
	
	
pad_poll:
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
.incbin "Background/Background.pal"

Spr_Palette:
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
