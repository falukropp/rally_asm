    processor 6502
    include "vcs.h"
    include "macro.h"
    include "xmacro.h"

        MAC SKIP_SCANLINES
.LINES  SET {1}
        ldx #.LINES
.vblank sta WSYNC
        dex
        bne .vblank
        ENDM


        MAC BACKGROUND_LINES
        dec rowcounter
        lda rowcounter
        and #$3F

        bne .blackrow
        lda #33
        jmp .end
.blackrow
        lda #00  
.end
        ENDM

        seg.u Variables
        org $80
        
xpos    .byte #0
speedi  .byte #0
speedf  .byte #0
disti   .byte #0
distf   .byte #0
zone    .byte #0
fueli    .byte #0
fuelf    .byte #0
fuellines .byte #0


rowcounter .byte #0

        seg Code
        org $f000                
    
start   
        CLEAN_START
    
NewGame
FuelOut
Crash
        lda #20
        sta fueli
        lda #$FF
        sta fuelf
NextFrame
        VERTICAL_SYNC
        TIMER_SETUP 37

; Clear sprite
        lda $0
        sta GRP0
; Set Sprite Color
        lda $1A
        sta COLUP0
        
; Read joystick left right
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
        
; Read joystick up/down
    lda speedi
        cmp #4
        beq NotAccelerating
        lda #$10
        and SWCHA
        bne NotAccelerating
        lda speedf
        clc
        adc #$10
        sta speedf
        bcc Done
        inc speedi
        jmp Done
NotAccelerating  
    lda speedi
        cmp #-2
        beq Done
        lda #$20
        and SWCHA
        bne Done
        lda speedf
        sec
        sbc #$10
        sta speedf
        bcs Done
        dec speedi        
Done
        
        lda speedf
        clc
        adc distf
        sta distf
        lda disti
        adc speedi
        sta disti
        
; Update Fuel
        lda fuelf
        sec
        sbc #$04
        sta fuelf
        lda fueli
        sbc #0
        bpl FuelLeft
        
        jmp FuelOut
FuelLeft
    sta fueli
        

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

        ldx #0
        lda disti
        sta rowcounter
        
;        SKIP_SCANLINES 150

        lda fueli
        asl
        adc fueli ; fuellines < 128, no need to clc
; A = 3 * fueli
        tay
        lda FuelMeterData,Y
        sta PF0
        iny
        lda FuelMeterData,Y
        sta PF1
        iny
        lda FuelMeterData,Y
        sta PF2
        lda #%00000010
        sta CTRLPF
        lda #$80
        sta COLUP0
        lda #$00
        sta COLUP1
        
        TIMER_WAIT      
        lda #8
        sta fuellines
FuelMeter
        sta WSYNC                
        dec fuellines
        bne FuelMeter
        
        lda #0
        sta PF0
        sta PF1
        sta PF2
        
BeforeSpriteLoop
        BACKGROUND_LINES
        sta WSYNC
        sta COLUBK
        inx
        cpx #141
        bcc BeforeSpriteLoop
        
        
        ldy #8
        ldx SpriteData,y
        dey
        BACKGROUND_LINES
        
SpriteLoop
        sta WSYNC       
        stx GRP0
        sta COLUBK
        
        ldx SpriteData,y
        
        BACKGROUND_LINES
        
        dey
        bpl SpriteLoop

        sta WSYNC       
        stx GRP0
        ldx #31
AfterSpriteLoop        
        sta WSYNC
        sta COLUBK
        BACKGROUND_LINES
        dex
        bne AfterSpriteLoop
        
        sta WSYNC
        
        ldx #0
        stx COLUBK

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

; PF0, PF1, PF2
FuelMeterData
    .byte #%00000000, #%00000000, #%00000000
    .byte #%00010000, #%00000000, #%00000000
    .byte #%00110000, #%00000000, #%00000000
    .byte #%01110000, #%00000000, #%00000000
    .byte #%11110000, #%00000000, #%00000000
    
    .byte #%11110000, #%10000000, #%00000000
    .byte #%11110000, #%11000000, #%00000000
    .byte #%11110000, #%11100000, #%00000000
    .byte #%11110000, #%11110000, #%00000000
    .byte #%11110000, #%11111000, #%00000000
    .byte #%11110000, #%11111100, #%00000000
    .byte #%11110000, #%11111110, #%00000000
    .byte #%11110000, #%11111111, #%00000000

    .byte #%11110000, #%11111111, #%00000001
    .byte #%11110000, #%11111111, #%00000011
    .byte #%11110000, #%11111111, #%00000111
    .byte #%11110000, #%11111111, #%00001111
    .byte #%11110000, #%11111111, #%00011111
    .byte #%11110000, #%11111111, #%00111111
    .byte #%11110000, #%11111111, #%01111111
    .byte #%11110000, #%11111111, #%11111111

; Epilogue

        org $fffc
        .word start
        .word start
