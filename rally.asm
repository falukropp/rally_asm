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
xpos    .byte
pelletpos .byte
; Lines advancement per frame
speedi  .byte
speedf  .byte
speedz  .byte ; Only needed for negative speed (Should only be FF or 00)
; Which line in the zone is the topmost scanline in?
disti   .byte
distf   .byte
; Which zone is the topmost scanline in?
zone     .byte
; Which zone is the current chunk in?
zoneForChunk .byte
; In which zone did the player pick up fuel last?
distifrac .byte
currblock .byte
lastfuelpickup .byte
; Fuel left
fueli    .byte
fuelf    .byte
fuellines .byte
scanline  .byte
;For collisioncheck
xposprev .byte
distiprev .byte
zoneprev .byte


THREE_COPIES    equ %011 
PLAYFIELD_SIZE  equ #167 
SPRITE_SIZE     equ #9   
CAR_SCAN_LINE   equ #41   

; Pointers to bitmap for each digit
Digit0      .word
Digit1      .word
Digit2      .word
Digit3      .word
Digit4      .word
Digit5      .word

BCDScore    hex 000000
LoopCount   .byte ; counts scanline when drawing

ZonedataPtr .word

ZonedataLeft    ds 16
ZonedataRight   ds 16
ZonedataFuel    ds 16


        seg Code
        org $f000                
    
start   
NewGame
FuelOut
Crash
        CLEAN_START
    
        lda #80
        sta xpos
        lda #20
        sta fueli
        lda #$FF
        sta fuelf
        
        ; Start a bit in, in zone 1.
        ; Makes it so zone > lastfuelpickup so there is pellet
        ; in first zone.
        ; Also, gives some room for zone 0 to be special wall zone.
        lda #200
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
        lda #0
        sta GRP0
        
; Set Sprite Color
; ---------------------
        lda #$1A
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
    ldx #00
    lda speedi
        bpl WasPositiveSpeed
        ldx #$FF
WasPositiveSpeed
    stx speedz
        
        lda speedf
        clc
        adc distf
        sta distf
        lda disti
        adc speedi
        sta disti
        lda zone
        adc speedz
        sta zone
        
; Update Fuel
; --------------
        lda fuelf
        sec
        sbc #4
        sta fuelf
        lda fueli
        sbc #0
        bpl FuelLeft
        
        jmp FuelOut
FuelLeft
    sta fueli
    
; Setup playfield        


    
SetupRoom
        lda zone
        sta zoneForChunk
        asl
        tay
; Set ZonePtr to current zone.        
        lda Zonedata,Y
        sta ZonedataPtr
        lda Zonedata+1,Y
        sta ZonedataPtr+1
; Set X to the chunk in Zone.
; The Rooms are drawn upside down, so start in 255-disti
; There are 16 chunks, so shift >> 4
    lda #$FF 
        sec
        sbc disti
        lsr
        lsr
        lsr
        lsr
        sta Temp
; (Yes, asl and adc, there a three bytes per chunk.)
        asl
        clc
        adc Temp
        tay
        ldx #0
        
NextIndex   
        lda (ZonedataPtr),Y
        sta ZonedataLeft,X
        iny
        lda (ZonedataPtr),Y
        sta ZonedataRight,X
        iny
        lda (ZonedataPtr),Y
        beq NoFuel
        sta pelletpos
        lda lastfuelpickup
        cmp zoneForChunk
        bcc Fuel        
        jmp NoFuel
Fuel
        lda #2
        jmp SaveFuel
NoFuel 
        lda #0
SaveFuel
        sta ZonedataFuel,X
        iny
        stx Temp
        cpy #48
        bcc SameZone
; Load the Room for the previous zone.
        lda zone
        dec zoneForChunk
        asl
        tay
        dey
        dey
        lda Zonedata,Y
        sta ZonedataPtr
        lda Zonedata+1,Y
        sta ZonedataPtr+1
        ldy #0
        
SameZone        
    ldx Temp 
        inx
        cpx #16 
        bne NextIndex

; Setup rows for frame
; --------------
        
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
                
; Set Fuel Horiz Pelletpos ( 2 lines )
; --------------
        lda pelletpos
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

        lda $10
        sta NUSIZ1
        lda $0
        sta VDELP1
        
        
; PlayfieldLoop (167 lines )       
        lda #1 ; Reflect playfield
        sta CTRLPF
        lda #$dd
        sta COLUPF

        ldx #0
        lda #$ff
        sec
        sbc disti
        asl
        asl
        asl
        asl
        sta distifrac
        ldy #0
        sty currblock
        
        lda #PLAYFIELD_SIZE-1
        sta scanline
        lda #$ff
        sta PF0
        lda #0
PlayFieldLoop
        sta WSYNC
        sta ENAM1
        stx GRP0
        
        lda ZonedataLeft,y

;        sta PF0
        sta PF1
        lda ZonedataRight,y
        sta PF2
                

        lda scanline
        sec
        sbc #CAR_SCAN_LINE
        cmp #SPRITE_SIZE    ; SpriteHeight
        bcc InSprite
        lda #0
InSprite
        tay        
        ldx SpriteData,y

        lda distifrac
        clc
        adc #$0f
        sta distifrac
        lda currblock
        adc #0
        sta currblock
        tay
        lda ZonedataFuel,y
        dec scanline
        bne PlayFieldLoop

        
        sta WSYNC
        lda #0
        sta ENAM1
        
; Setup Fuelmeter ( 1 + 7 lines )
; --------------
        ldx #0
        stx COLUBK
        sta PF0
        sta PF1
        sta PF2        
        lda #%00000010
        sta CTRLPF
        lda #$1A
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
        
; Check fuel pellet pickup <-> player collision
        bit CXM1P
        bpl DoneWithFuelPickup
        
; Picked up fuel. In which zone is the pellet/car?        
        ldx zone
; If disti > (PLAYFIELD_SIZE-CAR_SCAN_LINE) the car is in the same zone as the top line.
        lda disti
        cmp #PLAYFIELD_SIZE-CAR_SCAN_LINE
        bcs SaveLastFuelPickup
; ... otherwise the car was still in the previous zone.        
        dex
SaveLastFuelPickup
        stx lastfuelpickup
        
        lda fueli
        clc
        adc #04
        cmp #21
        bcc StoreFuel  
        lda #$FF
        sta fuelf
        lda #20
StoreFuel        
        sta fueli
        
        lda fueli
        lsr
        ldx #00
        ldy #00
        jsr AddScore
        
DoneWithFuelPickup

; Check playfield <-> player collision
    bit CXP0FB
        bpl DoneWithPFCollision
        lda xposprev
    sta xpos
        lda zoneprev
        sta zone
        lda distiprev
        sta disti
        lda #0
        sta distf
        sta speedi
        sta speedf
        sta speedz
        
DoneWithPFCollision
    lda xpos
        sta xposprev
        lda disti
        sta distiprev
        lda zone
        sta zoneprev
        
        sta CXCLR
        
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
    .byte #%01110000;$1A
    .byte #%11111000;$1A
    .byte #%11111000;$1A
    .byte #%01110000;$1A
    .byte #%01010000;$1A
    .byte #%11111000;$1A
    .byte #%11111000;$1A
    .byte #%01110000;$1A
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
    
        align $100
Zonedata
    .word Room0
    .word Room1
    .word Room2
    .word Room3
    .word Room4
    .word Room3
    .word Room2
    .word Room1
    .word Room0
    
Fueldata 
; xpos, scanline.
; -1 on xpos means no fuel in that room.
    .byte  #-1, #0
    
    .byte  #-1, #0
    .byte  #-1, #0
    .byte  #-1, #0
    .byte  #-1, #0
    .byte  #-1, #0
    .byte  #-1, #0
    .byte  #-1, #0
    
    .byte  #-1, #0

Room0
    .byte #%11111111, #%11111111, #0
    .byte #%11111111, #%11111111, #0
    .byte #%11111111, #%11111111, #0
    .byte #%11111111, #%11111111, #0
    .byte #%11111111, #%11111111, #0
    .byte #%11111111, #%11111111, #0
    .byte #%11111111, #%11111111, #0
    .byte #%11111111, #%11111111, #0
    .byte #%11111111, #%11111111, #0
    .byte #%11111111, #%11111111, #0
    .byte #%11111111, #%11111111, #0
    .byte #%11111111, #%11111111, #0
    .byte #%11111111, #%11111111, #0
    .byte #%11111111, #%11111111, #0
    .byte #%11111111, #%11111111, #0
    .byte #%11111111, #%11111111, #0

Room1
    .byte #%10000000, #%00000000, #0
    .byte #%10000000, #%00000000, #0
    .byte #%11111100, #%01111100, #0
    .byte #%10000000, #%00000000, #90
    .byte #%10000000, #%00000000, #0
    .byte #%11000000, #%00000000, #0
    .byte #%11000000, #%00000000, #0
    .byte #%11100001, #%00000001, #0
    .byte #%11100001, #%00000001, #0
    .byte #%11110000, #%00000000, #0
    .byte #%11110000, #%00000000, #0
    .byte #%11110000, #%00000000, #0
    .byte #%11111000, #%00000000, #0
    .byte #%11111100, #%00000000, #0
    .byte #%11111100, #%01111100, #0
    .byte #%10001100, #%00000000, #0

Room2
    .byte #%00000000, #%00000000, #0
    .byte #%11001100, #%11001100, #0
    .byte #%11001100, #%11001100, #0
    .byte #%11001100, #%11001100, #60
    .byte #%11001100, #%11001100, #0
    .byte #%11001100, #%11001100, #0
    .byte #%11001100, #%11001100, #0
    .byte #%11001100, #%11001100, #0
    .byte #%11001100, #%11001100, #0
    .byte #%11001100, #%11001100, #0
    .byte #%11001100, #%11001100, #0
    .byte #%11001100, #%11001100, #0
    .byte #%11001100, #%11001100, #0
    .byte #%11001100, #%11001100, #0
    .byte #%11001100, #%11001100, #0
    .byte #%00000000, #%00000000, #0

Room3
    .byte #%00000000, #%00000000, #0
    .byte #%00000000, #%00000000, #0
    .byte #%00000000, #%00000000, #130
    .byte #%00000000, #%00000000, #0
    .byte #%00000000, #%10011000, #0
    .byte #%11111111, #%10011111, #0
    .byte #%00000000, #%10011000, #0
    .byte #%00000000, #%00000000, #0
    .byte #%10011000, #%00000000, #0
    .byte #%10011111, #%11111111, #0
    .byte #%10011000, #%00000000, #0
    .byte #%00000000, #%00000000, #0
    .byte #%00000000, #%00000000, #0
    .byte #%00000000, #%00000000, #0
    .byte #%00000000, #%00000000, #0
    .byte #%00000000, #%00000000, #0

Room4
    .byte #%00000000, #%00000000, #0
    .byte #%00000000, #%00000000, #20
    .byte #%00000000, #%00000000, #0
    .byte #%00000000, #%00000000, #0
    .byte #%00000000, #%00000000, #0
    .byte #%11111111, #%11111001, #0
    .byte #%10000000, #%10000001, #0
    .byte #%10000000, #%10000001, #0
    .byte #%10011000, #%10000001, #0
    .byte #%10011000, #%10011111, #0
    .byte #%10011000, #%00000000, #0
    .byte #%10011000, #%00000000, #0
    .byte #%10011000, #%00000000, #0
    .byte #%10011111, #%11111111, #0
    .byte #%00000000, #%00000000, #0
    .byte #%00000000, #%00000000, #0

; Epilogue

        org $fffc
        .word start
        .word start
