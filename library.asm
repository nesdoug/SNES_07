;from easySNES
;Doug Fraker 2020



.segment "CODE"


	

OAM_Spr:
.a8
.i16
; to put one sprite on screen
; copy all the sprite values to these 8 bit variables
; spr_x - x (9 bit)
; spr_y - y
; spr_c - tile #
; spr_a - attributes, flip, palette, priority
; spr_sz = sprite size, 0 or 2

	php
	rep #$30 ;axy16
	lda sprid
	and #$007f
	tax
	asl a
	asl a ; 0-511
	tay
	
	txa
	sep #$20 ;a8
	lsr a
	lsr a ; 0-31
	tax
	lda spr_x ;x low byte
	sta a:OAM_BUFFER, y
	lda spr_y ;y
	sta a:OAM_BUFFER+1, y
	lda spr_c ;tile
	sta a:OAM_BUFFER+2, y
	lda spr_a ;attribute
	sta a:OAM_BUFFER+3, y
	
; handle the high table
; two bits, shift them in
; this is slow, so if this is zero, skip it, it was
; zeroed in oam_clear

	lda spr_x+1 ;9th x bit
	and #1 ;we only need 1 bit
	ora spr_sz ;size
	beq @end
	sta spr_h
	
	lda sprid
	and #3
	beq @zero
	dec a
	beq @one
	dec a
	beq @two
	bne @three
	
@zero:
	lda spr_h
	sta a:OAM_BUFFER+$200, x
	bra @end
	
@one:
	lda spr_h
	asl a
	asl a
	ora a:OAM_BUFFER+$200, x
	sta a:OAM_BUFFER+$200, x
	bra @end
	
@two:
	lda spr_h
	asl a
	asl a
	asl a
	asl a
	ora a:OAM_BUFFER+$200, x
	sta a:OAM_BUFFER+$200, x
	bra @end

@three:
	lda spr_h
	lsr a ; 0000 0001 c
	ror a ; 1000 0000 c
	ror a ; 1100 0000 0
	ora a:OAM_BUFFER+$200, x
	sta a:OAM_BUFFER+$200, x	
	
@end:
	lda sprid
	inc a
	and #$7f ; keep it 0-127
	sta sprid
	plp
	rts


OAM_Meta_Spr:	
.a16
.i16
;update 6/2021
; to put multiple sprites on screen
; copy all the sprite values to these 8 bit variables
; spr_x - x 9 bit
; spr_y - y

; A16 = metasprite data address
; X = bank of metasprite data

; format (5 bytes per sprite)
; relative x, relative y, tile #, attributes, size
; end in 128

	php
	rep #$30 ;axy16
	sta temp1 ;address of metasprite
	stx temp2
	
	ldy #$0000
	sty temp3 ;clear these
	sty temp4 ;high table index
	sty temp5
	sty temp6
	
	lda spr_x ;16 bits
	and #$01ff ;9 bits
	sta spr_x2
	
	sep #$20 ;a8
	lda sprid
	and #3
	sta temp3
	lda #3
	sec
	sbc temp3
	sta temp3 ;loop counter
	
	lda sprid
	lsr a
	lsr a ; 0-31
	sta temp4 ;high table index
	
	lda sprid
	rep #$20 ;a16
	and #$007f
	asl a
	asl a ; 0-511
	tax ;x = low table index
	
@loop:
	sep #$20 ; a8
	lda [temp1], y
	cmp #128 ; end of data
	beq @done
;first byte is rel x (signed)	
	rep #$20 ;a16
	and #$00ff
	cmp #$0080 ;is negative?
	bcc @pos_x
@neg_x:
	ora #$ff00 ; extend the sign
@pos_x:
	clc
	adc spr_x2
;the high byte holds the X 9th bit
	sep #$20 ;a8
	sta a:OAM_BUFFER, x
;keep that high byte 9th x
	iny
	lda [temp1], y ;y byte
	clc
	adc spr_y	
	sta a:OAM_BUFFER+1, x
	iny
	lda [temp1], y ;tile
	sta a:OAM_BUFFER+2, x
	iny
	lda [temp1], y ;attributes
	sta a:OAM_BUFFER+3, x
	iny
	lda [temp1], y ;size
	iny
	sta spr_h
	xba ;that 9th x bit
	and #1
	ora spr_h
	phx ;save for later
	ldx temp3
	sta temp5, x
	plx
	
	inx
	inx
	inx
	inx
	inc sprid
	
	dec temp3 ;loop counter
	bpl @loop
; we have 4, push them to the high table now
	phx ;save for later
	ldx temp4
	lda temp5
	asl a
	asl a
	ora temp5+1
	asl a
	asl a
	ora temp5+2
	asl a
	asl a
	ora temp5+3
	ora a:OAM_BUFFER+$200, x
	sta a:OAM_BUFFER+$200, x
	inc temp4

	ldx #$0000
	stx temp5
	stx temp6
	
	plx
;overflow check	
	cpx #$0200
	bcc @ok
	ldx #$0000 ;low table index
	stz sprid
	stz temp4 ;high table index
@ok:
	
	lda #3
	sta temp3 ;loop counter
	bra @loop
	
@done:
.a8
.i16
	inc temp3
	beq @exit
;handle one more high table byte.
	ldx temp4
	lda temp5
	asl a
	asl a
	ora temp5+1
	asl a
	asl a
	ora temp5+2
	asl a
	asl a
	ora temp5+3
	ora a:OAM_BUFFER+$200, x
	sta a:OAM_BUFFER+$200, x
	
@exit:	
	plp
	rts

	
	
;Clear_OAM:
;see init.asm
	
	
	
Map_Offset: 
.a16
.i8
; A should be 16, XY size doesn't matter
; converts pixel coordinates in a map to tile address offset
; the idea is that you add this value to the map_base
; works for 32x32,64x32,and 32x64 maps
; x -L = tile's x position, 0-31 [0-63 large map]
; y -L = tile's y position, 0-31 [0-63 large map]
; y max 27 if non-scrolling and screen size 224 pixel tall 
; to convert pixels to tiles >> 3 (if 16x16 tile size >> 4)

; returns a16 = vram address offset (add it to the base address)
	php
	rep #$20 ;A16
	sep #$10 ;XY8
	tya
	and #$0020
	sta temp1
	txa
	and #$0020
	ora temp1 ; if either high bit is set, offset + $400
	beq @zero
	lda #$400
@zero:
	sta temp1 
	
offset_common:	
	tya
	and #$001f
	asl a
	asl a
	asl a
	asl a
	asl a
	ora temp1
	sta temp1
	txa
	and #$001f
	ora temp1
; returns a = map offset
	plp
	rts	
	
	
	
	
	
Check_Collision:
.a8
.i16
;copy each object's value to these varibles and jsr here.
;obj1x: .res 1
;obj1w: .res 1
;obj1y: .res 1
;obj1h: .res 1
;obj2x: .res 1
;obj2w: .res 1
;obj2y: .res 1
;obj2h: .res 1
;returns collision = 1 or 0
	php
	A8
;first check if obj1 R (obj1 x + width) < obj2 L

	lda obj1x
	clc
	adc obj1w
	cmp obj2x
	bcc @no
		
;now check if obj1 L > obj2 R (obj2 x + width)

	lda obj2x
	clc
	adc obj2w
	cmp obj1x
	bcc @no

;first check if obj1 Bottom (obj1 y + height) < obj2 Top
	
	lda obj1y
	clc
	adc obj1h
	cmp obj2y
	bcc @no
		
;now check if obj1 Top > obj2 Bottom (obj2 y + height)

	lda obj2y
	clc
	adc obj2h
	cmp obj1y
	bcc @no
	
@yes:
	lda #1
	sta collision
	plp
	rts
	
@no:
	stz collision
	plp
	rts









