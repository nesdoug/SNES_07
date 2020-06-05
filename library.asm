;from easySNES
;Doug Fraker 2020

oam_buffer = OAM_BUFFER

.segment "CODE"

oam_spr:
.a8
.i16
; to put one sprite on screen
; copy all the sprite values to these 8 bit variables
; spr_x - x
; spr_y - y
; spr_c - tile #
; spr_a - attributes, flip, palette, priority
; spr_h - 0-3, optional, keep zero if not needed
;  bit 0 = X high bit (neg)
;  bit 1 = sprite size
	php
	sep #$20
	lda sprid ; 0-127
	lsr a
	lsr a
	sta temp1 ; 0-31
	lda sprid
	rep #$30
	and #$007f
	asl a
	asl a ; 0-511
	tay
	sep #$20
	lda spr_x
	sta a:oam_buffer, y
	lda spr_y
	sta a:oam_buffer+1, y
	lda spr_c
	sta a:oam_buffer+2, y
	lda spr_a
	sta a:oam_buffer+3, y
	
; handle the high table
; two bits, shift them in
; this is slow, so if this is zero, skip it, it was
; zeroed in oam_clear

	lda spr_h ; if zero, skip
	beq @end
	and #3 ; to be safe, we only need 2 bits
	sta spr_h
	
	lda #0
	xba ; clear that H byte, a is 8 bit
	lda temp1 ; sprid >> 2
	tay ; should be 0-31
	
	lda sprid
	and #3
	beq @zero
	cmp #1
	beq @one
	cmp #2
	beq @two
	bne @three
@zero:
	lda a:oam_buffer+$200, y
	and #$fc
	sta temp1
	lda spr_h
	ora temp1
	sta a:oam_buffer+$200, y
	bra @end
	
@one:
	lda a:oam_buffer+$200, y
	and #$f3
	sta temp1
	lda spr_h
	asl a
	asl a
	ora temp1
	sta oam_buffer+$200, y
	bra @end
	
@two:
	lda a:oam_buffer+$200, y
	and #$cf
	sta temp1
	lda spr_h
	asl a
	asl a
	asl a
	asl a
	ora temp1
	sta a:oam_buffer+$200, y	
	bra @end

@three:
	lda a:oam_buffer+$200, y
	and #$3f
	sta temp1
	lda spr_h
	lsr a ; 0000 0001 c
	ror a ; 1000 0000 c
	ror a ; 1100 0000 0
	ora temp1
	sta a:oam_buffer+$200, y	
	
@end:	
	lda sprid
	clc
	adc #1
	and #$7f ; keep it 0-127
	sta sprid
	plp
	rts


oam_meta_spr:	
.a16
.i16
; to put multiple sprites on screen
; copy all the sprite values to these 8 bit variables
; spr_x - x
; spr_y - y
; spr_h - 0-1, optional, keep zero if not needed
;  bit 0 = X high bit (neg)
; (these values are trashed... rewrite them each use.)

; A16 = metasprite data address
; X = bank of metasprite data
; format (5 bytes per sprite)
; relative x, relative y, tile #, attributes, size
; end in 128
	php
	rep #$30
	; temp1 is used by oam_spr, don't use it here
	sta temp2
	stx temp3
	sep #$20
	lda spr_x
	sta spr_x2
	lda spr_y
	sta spr_y2
	lda spr_h
	and #$01 ; high x 0-1
	beq @zero
	lda #$ff ; high x = -1
@zero:	
	sta spr_x2+1
	lda spr_h
	and #$02
	sta spr_h2 ; size
	
@loop:
	sep #$30 ; axy8
	lda [temp2]
	cmp #128 ; end of data
	beq @done
	jsr meta_sub
.a8	
.i8
	tyx
	beq @skip
	
	jsr oam_spr ; call the 1 sprite subroutine
	
@skip:
	rep #$30
	lda #$0005
	clc
	adc temp2
	sta temp2
	bra @loop
	
@done:	
.a8
.i8
	plp
	rts
	
meta_sub:
.a8
.i8
	
; a = rel x
	rep #$20 ; a16
	and #$00ff ; clear that upper byte...
; need to extend the sign for a negative rel X	
	cmp #$0080
	bcc @pos_x
@neg_x:
	ora #$ff00 ; extend the sign
@pos_x:
	clc
	adc spr_x2 ; either 0000 or ff00
	sep #$20 ; a8
	sta spr_x ; 8 bit low X
	xba ; are we in range?
	cmp #$ff
	bne @check_x
; set high x	
	lda spr_h2
	ora #$01
	sta spr_h
	bra @x_done
	
@check_x:
	cmp #$01 ; too far right
	bcs @skip
; clear high x 	
	lda spr_h2
	sta spr_h
	
@x_done:
.a8
	ldy #1 ; rel y
	lda [temp2], y
	clc
	adc spr_y2
	sta spr_y

	iny ; y=2 char
	lda [temp2], y
	sta spr_c
	iny ; y=3 attributes
	lda [temp2], y
	sta spr_a
	iny ; y=4 size
	lda [temp2], y
	ora spr_h
	sta spr_h
	rts
	
@skip:
	ldy #0 ; signal to skip the sprite routine
	rts	
	
	
oam_clear:
.a8
.i16
; do at the start of each frame	
; clears the sprite buffer
; put all y at 224
	php
	sep #$20
	rep #$10
	stz sprid
	lda #224
	ldy #1
@loop:
; more efficient than a one lined sta
	sta a:oam_buffer, y
	sta a:oam_buffer+$40, y
	sta a:oam_buffer+$80, y
	sta a:oam_buffer+$c0, y
	sta a:oam_buffer+$100, y
	sta a:oam_buffer+$140, y
	sta a:oam_buffer+$180, y
	sta a:oam_buffer+$1c0, y
	iny
	iny
	iny
	iny
	cpy #$40 ; 41, but whatever
	bcc @loop
	
; clear the high table too
; then the oam_spr code can skip the 5th byte, if zero

	ldx #30
	rep #$20
@loop2:
	stz a:oam_buffer+$200, x
	dex
	dex
	bpl @loop2
	plp
	rts	
	
	
	
map_offset: 
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
	
	
	
	
	
check_collision:
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
	rts
	
@no:
	stz collision
	rts









