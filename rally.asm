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
Temp        .byte
        
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

THREE_COPIES    equ %011 ;

; Pointers to bitmap for each digit
Digit0      .word
Digit1      .word
Digit2      .word
Digit3      .word
Digit4      .word
Digit5      .word

BCDScore    hex 000000
LoopCount   .byte ; counts scanline when drawing

        seg Code
        org $f000                
    
start   
        CLEAN_START
    
NewGame
FuelOut
Crash
    lda #0
        sta BCDScore
        sta BCDScore+1
        sta BCDScore+2
        lda #20
        sta fueli
        lda #$FF
        sta fuelf
        
; ----------------------------------------------
; Vertical Sync
; ----------------------------------------------

NextFrame
        VERTICAL_SYNC
        
; ----------------------------------------------
; Vertical Blank
; ----------------------------------------------

        TIMER_SETUP 37

; Clear sprite
; ---------------------
        lda $0
        sta GRP0
        
; Set Sprite Color
; ---------------------
        lda $1A
        sta COLUP0
        sta COLUP1
        
; Read joystick left right
; ---------------------
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
; ---------------------

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
; --------------
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
        

; Setup rows for frame
; --------------
        ldx #0
        lda disti
        sta rowcounter
        
; Setup Score
        lda #THREE_COPIES
        sta NUSIZ0
        sta NUSIZ1
; set horizontal position of player objects
        sta WSYNC
        SLEEP 26 ; Works with 26
        sta RESP0
        sta RESP1
        lda #0
        sta HMP0
        lda #$10
        sta HMP1
        sta WSYNC
        sta HMOVE
        sta HMCLR
        lda #1
        sta VDELP0
        sta VDELP1
        
        TIMER_WAIT      
        
; ----------------------------------------------
; Visible frame
; ----------------------------------------------

; Score
; ---------

        lda #0
        sta PF0
        sta PF1
        sta PF2

    jsr GetDigitPtrs    ; get pointers        
        jsr DrawDigits      ; draw digits
                
        lda #0
        sta PF0
        sta PF1
        sta PF2
        
; Set Sprite Pos
; --------------
        lda xpos
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
        lda #0
        sta NUSIZ0
        sta VDELP0
        
; Playfield Before Sprite
; -----------------------

BeforeSpriteLoop
        BACKGROUND_LINES
        sta WSYNC
        sta COLUBK
        inx
        cpx #125
        bcc BeforeSpriteLoop
        
        
        ldy #8
        ldx SpriteData,y
        dey
        BACKGROUND_LINES
        
; Sprite
; -----------------------
SpriteLoop

        sta WSYNC       
        stx GRP0
        sta COLUBK
        
        ldx SpriteData,y
        
        BACKGROUND_LINES
        
        dey
        bpl SpriteLoop

; Playfield After Sprite
; -----------------------

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
        
; Setup Fuelmeter
; --------------
        ldx #0
        stx COLUBK
        sta PF0
        sta PF1
        sta PF2        
        lda #%00000010
        sta CTRLPF
        lda #$80
        sta COLUP0
        lda #$00
        sta COLUP1
        sta WSYNC
        
        lda fueli        
        asl
        adc fueli ; fuellines < 128, no need to clc
        tay
; Y = 3 * fueli
        lda FuelMeterData,Y
        sta PF0
        lda FuelMeterData+1,Y
        sta PF1
        lda FuelMeterData+2,Y
        sta PF2
        
        ldx #7
FuelmeterLoop:        
        sta WSYNC
        dex
        bne FuelmeterLoop
        
        ldx #0
        stx PF0
        stx PF1
        stx PF2        

; ----------------------------------------------
; Overscan
; ----------------------------------------------

        TIMER_SETUP 30
        lda #01
        ldx #00
        ldy #00
        jsr AddScore
        TIMER_WAIT      
        jmp NextFrame
        
        
; ==============================================        
; Subroutines
; ==============================================        

; Adds value to 6-BCD-digit score.
; A = 1st BCD digit
; X = 2nd BCD digit
; Y = 3rd BCD digit
AddScore subroutine
        sed ; enter BCD mode
        clc ; clear carry
        sta Temp
        lda BCDScore
        adc Temp
        sta BCDScore
        stx Temp
        lda BCDScore+1
        adc Temp
        sta BCDScore+1
        sty Temp
        lda BCDScore+2
        adc Temp
        sta BCDScore+2
        cld ; exit BCD mode
        rts
        
GetDigitPtrs subroutine
    ldx #0  ; leftmost bitmap
        ldy #2  ; start from most-sigificant BCD value
.Loop
        lda BCDScore,y  ; get BCD value
        and #$f0    ; isolate high nibble (* 16)
        lsr     ; shift right 1 bit (* 8)
        sta Digit0,x    ; store pointer lo byte
        lda #>FontTable
        sta Digit0+1,x  ; store pointer hi byte
        inx
        inx     ; next bitmap pointer
        lda BCDScore,y  ; get BCD value (again)
        and #$f     ; isolate low nibble
        asl
        asl
        asl     ; * 8
        sta Digit0,x    ; store pointer lo byte
        lda #>FontTable
        sta Digit0+1,x  ; store pointer hi byte
        inx
        inx     ; next bitmap pointer
        dey     ; next BCD value
        bpl .Loop   ; repeat until < 0
    rts

; Display the resulting 48x8 bitmap
; using the Digit0-5 pointers.
    align $100
DrawDigits subroutine
    sta WSYNC
        SLEEP 40
        lda #7
        sta LoopCount
BigLoop
    ldy LoopCount   ; counts backwards
        lda (Digit0),y  ; load B0 (1st sprite byte)
        sta GRP0    ; B0 -> [GRP0]
        lda (Digit1),y  ; load B1 -> A
        sta GRP1    ; B1 -> [GRP1], B0 -> GRP0
        sta WSYNC   ; sync to next scanline
        lda (Digit2),y  ; load B2 -> A
        sta GRP0    ; B2 -> [GRP0], B1 -> GRP1
        
        lda (Digit5),y  ; load B5 -> A
        sta Temp    ; B5 -> temp
        lda (Digit4),y  ; load B4
        tax     ; -> X
        lda (Digit3),y  ; load B3 -> A
        ldy Temp    ; load B5 -> Y
        sta GRP1    ; B3 -> [GRP1]; B2 -> GRP0
        stx GRP0    ; B4 -> [GRP0]; B3 -> GRP1
        sty GRP1    ; B5 -> [GRP1]; B4 -> GRP0
        sta GRP0    ; ?? -> [GRP0]; B5 -> GRP1
        dec LoopCount   ; go to next line
    bpl BigLoop ; repeat until < 0
        
        lda #0      ; clear the sprite registers
        sta GRP0
        sta GRP1
        sta GRP0
        sta GRP1
        rts

; Font table for digits 0-9 (8x8 pixels)
        align $100 ; make sure data doesn't cross page boundary
FontTable
    hex 003c6666766e663c007e181818381818
        hex 007e60300c06663c003c66061c06663c
        hex 0006067f661e0e06003c6606067c607e
        hex 003c66667c60663c00181818180c667e
        hex 003c66663c66663c003c66063e66663c
        

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
