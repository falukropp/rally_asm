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

        seg.u Variables
        org $80
Temp        .byte
        
; Car Horiz pointer        
xpos    .byte #0
; Lines advancement per frame
speedi  .byte #0
speedf  .byte #0
; Which line in Zone the bumper of the car is 
disti   .byte #0 
distf   .byte #0
; Which zone is the car (bumper) in?
zone    .byte #0
; In which zone did the player pick up fuel last?
lastfuelpickup .byte #0
; Fuel left
fueli    .byte #0
fuelf    .byte #0
fuellines .byte #0
scanline  .byte #0
; If fuel pellet is to be shown, at what line?
; If not to be shown, set to 255 or something > playfield lines.
pelletline .byte #0

THREE_COPIES    equ %011 ;
PLAYFIELD_SIZE  equ #162 ;

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
NewGame
FuelOut
Crash
        CLEAN_START
    
        lda #20
        sta fueli
        lda #$FF
        sta fuelf
        
        ; Start a bit in, in zone 1.
        ; Makes it so zone > lastfuelpickup so there is pellet
        ; in first zone.
        ; Also, gives some room for zone 0 to be special wall zone.
        lda #10
        sta disti
        lda #1
        sta zone
                
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
; So rowcounter starts at the dist for the topmost row
        
; Check if fuel pellet is on screen.        
    lda zone
        cmp lastfuelpickup
        bpl setupscore
; Pellet should be a line 125 in zone... 
; But the front buffer of the car is at scanline 125.
; pelletline should be at 125 + (disti - 125)  = disti
        lda disti
    sta pelletline
        
; Setup Score
setupscore:
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
        
         TIMER_SETUP 192
        
; ----------------------------------------------
; Visible frame
; ----------------------------------------------

; Score ( 9+ lines )
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

        lda #0
        sta NUSIZ0
        sta VDELP0
        
; Set Sprite Horiz Pos ( 1 line )
; --------------
        lda xpos
        sec
        sta WSYNC
        sta HMCLR
SpriteLoop
        sbc #15
        bcs SpriteLoop

        eor #7
        asl
        asl
        asl
        asl
        sta HMP0
        sta RESP0 
                
; Set Fuel Horiz Pelletpos ( 2 line )
; --------------
        lda #64
        sec
        sta WSYNC
PelletLoop
        sbc #15
        bcs PelletLoop

        eor #7
        asl
        asl
        asl
        asl
        sta HMM1
        sta RESM1
        sta WSYNC
        sta HMOVE

        lda #0
        sta NUSIZ1
        sta VDELP1
        
        
; PlayfieldLoop (167 lines )       

    ldx #0
        lda #0
        sta scanline
PlayFieldLoop
        sta WSYNC
        sta ENAM1
        stx GRP0

        lda scanline
        sec
        sbc #125
        cmp #9 ; SpriteHeight
        bcc InSprite
        lda #0
InSprite
        tay        
        ldx SpriteData,y


        ldy #0
        lda scanline
        cmp pelletline
        bne DontShowPellet
        ldy #2
DontShowPellet        
        tya   
        inc scanline
        ldy scanline
        cpy #166
        bcc PlayFieldLoop

        
        lda #0
        sta ENAM1
        sta WSYNC
        
; Setup Fuelmeter ( 1 + 7 lines )
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
        
        TIMER_WAIT

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

; SetHorizPos - Sets the horizontal position of an object.
; The X register contains the index of the desired object:
;  X=0: player 0
;  X=1: player 1
;  X=2: missile 0
;  X=3: missile 1
;  X=4: ball
; This routine does a WSYNC and HMOVE before executing,
; so whatever you do here will not take effect until you
; call the routine again or do your own WSYNC and HMOVE.
        align $100
SetHorizPos
    sta WSYNC   ; start a new line
    sec     ; set carry flag
DivideLoop
    sbc #15     ; subtract 15
    bcs DivideLoop  ; branch until negative
    eor #7      ; calculate fine offset
    asl
    asl
    asl
    asl
    sta RESP0,x ; fix coarse position
    sta HMP0,x  ; set fine offset
    rts     ; return to caller


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
