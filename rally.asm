    processor 6502
    include "vcs.h"
    include "macro.h"
    include "xmacro.h"

    org  $f000
    
	MAC SKIP_SCANLINES
.LINES  SET {1}
	ldx #.LINES
.vblank	sta WSYNC
	dex
	bne .vblank
	ENDM
    

xpos equ $80

start	
	CLEAN_START
NextFrame
	VERTICAL_SYNC
	TIMER_SETUP 37

; Clear sprite
	lda $0
	sta GRP0
; Set Sprite Color
	lda $1A
	sta COLUP0
        
; Read joystick
	ldx xpos
    beq NotMovingLeft
    bit SWCHA
    bvs NotMovingLeft
	dex
NotMovingLeft
	cpx #154
    beq NotMovingRight
    bit SWCHA
    bmi NotMovingRight
    inx
NotMovingRight
	stx xpos

; Set Sprite Pos
	txa
	sec
	sta WSYNC
	sta HMCLR
DivideLoop
	sbc #15
	bcs DivideLoop

	eor #7
	asl
	asl
	asl
	asl
	sta HMP0
	sta RESP0 
	sta WSYNC
	sta HMOVE

	TIMER_WAIT	
	sta WSYNC
        
	TIMER_SETUP 192
        
	SKIP_SCANLINES 150
        
	ldy #8
SpriteLoop
	sta WSYNC	
	lda SpriteData,y
	sta GRP0
        
	dey
	bpl SpriteLoop

	TIMER_WAIT	
    sta WSYNC

	TIMER_SETUP 30
	TIMER_WAIT	
    sta WSYNC
        
	jmp NextFrame

	align $100
SpriteData
    .byte #%00000000;$1A
    .byte #%01111000;$1A
    .byte #%11111100;$1A
    .byte #%11111100;$1A
    .byte #%01111000;$1A
    .byte #%01001000;$1A
    .byte #%11111100;$1A
    .byte #%11111100;$1A
    .byte #%01111000;$1A
;---End Graphics Data---


; Epilogue

	org $fffc
	.word start
	.word start
