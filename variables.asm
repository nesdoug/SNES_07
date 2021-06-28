; variables declared

BALL_SIZE = 7
PADDLE_W = 7
PADDLE_H = 47



.segment "ZEROPAGE"

in_nmi: .res 2
temp1: .res 2
temp2: .res 2
temp3: .res 2
temp4: .res 2
temp5: .res 2
temp6: .res 2

; for sprite code
sprid: .res 1
spr_x: .res 2 ; 9 bit
spr_y: .res 1 
spr_c: .res 1 ; tile #
spr_a: .res 1 ; attributes
spr_sz:	.res 1 ; sprite size, 0 or 2
spr_h: .res 1 ; high 2 bits
spr_x2:	.res 2 ; for meta sprite code

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

PAL_BUFFER: .res 512 ;palette

OAM_BUFFER: .res 512 ;low table
OAM_BUFFER2: .res 32 ;high table

