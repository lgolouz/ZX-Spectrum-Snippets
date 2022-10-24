            org $8000
            JP main

textline    DEFM "SCROLL LINE TEST! This is a scroll message that should be printed and scrolled... "
            DEFB 0
charsetaddr DEFW $3D00

;-------------------------------------------------------
;FUNCTION getLineAddr
;Input:   C - `y` line position in pixels
;Output: HL - line address in screen memory
;Dirty:  AF
;-------------------------------------------------------
;Description:
;The address of the pixel line is
;+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;|15|14|13|12|11|10| 9| 8| 7| 6| 5| 4| 3| 2| 1| 0|
;+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;| 0| 1| 0|Y7|Y6|Y2|Y1|Y0|Y5|Y4|Y3|X4|X3|X2|X1|X0|
;+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;Where Y7-Y0 - bits of the Y location of the pixel
;      X4-X0 - bits of the X location of the pixel line
;              (i.e. location of the character cell)
;and bits 15, 14 and 13 represents the base address
;of the video memory
;-------------------------------------------------------
getLineAddr
            LD HL, SCREENPTR ;Store the base address of video memory
            LD A, C          ;Cut the firts
            AND 00000111b    ;three bits of the `y`
            OR H             ;Combine the H register of the base video address
            LD H, A          ;with the firs three bits
            LD A, C          ;Cut the last
            AND 11000000b    ;two bits of the `y`
            RRA              ;and put them
            RRA              ;to the fourth and
            RRA              ;third bit of the register H
            OR H             ;of the address
            LD H, A          ;in video memory
            LD A, C          ;Finally cut the
            AND 00111000b    ;5th, 4th and 3rd bits
            RLA              ;and put them to
            RLA              ;bits 7, 6 and 5
            LD L, A          ;of the L register of the address in video memory
            RET
;-------------------------------------------
;END OF FUNCTION getLineAddr
;-------------------------------------------

;-------------------------------------------------------
;FUNCTION getCharBitmap
;Input:  A  - character for which bitmap should be get
;Output: HL - character bitmap address
;Dirty:  DE
;-------------------------------------------------------
getCharBitmapAddr
            SUB CHAROFFSET
            LD H, 0
            RLA
            RL H                ;so it is no need to
            RLA                 ;clear it by force.
            RL H
            RLA
            RL H
            LD L, A
            LD DE, (CHARSVAR)
            ADD HL, DE
            RET
;-------------------------------------------------------
;END OF FUNCTION getCharBitmapAddr
;------------------------------------------------------- 

;-------------------------------------------------------
;FUNCTION getBit
;Input:  A  - byte for which bit should be get
;        B  - bit count to shift
;Output: CF - bit value
;-------------------------------------------------------
getBit
            RLA
            DJNZ getBit
            RET
;-------------------------------------------------------
;END OF FUNCTION getBit
;------------------------------------------------------- 

main:
            EI
            LD HL, textline
            
scrollloop:
            LD A, (HL)
            CP 0
            JR Z, main
            EXX
            CALL getCharBitmapAddr
            PUSH HL
            LD C, LINEY
            LD B, CHARWIDTH
charhshiftloop:
            LD A, B
            EX AF, AF'
            PUSH BC
            PUSH HL
            LD B, CHARHEIGHT
charvshiftloop:
            CALL getLineAddr
            LD DE, SCREENWIDTH_C - 1
            ADD HL, DE                   ;HL contains current line on-screen address
            POP DE                       ;DE contains current character line bitmap address
            PUSH BC
            EX AF, AF'
            LD B, A
            EX AF, AF'
            LD A, CHARWIDTH
            SUB B
            INC A
            LD B, A
            LD A, (DE)
            CALL getBit
            LD B, SCREENWIDTH_C
lineshiftloop:
            LD C, (HL)
            RL C
            LD (HL), C
            DEC HL
            DJNZ lineshiftloop
            POP BC
            INC C
            INC DE
            PUSH DE
            DJNZ charvshiftloop
            HALT                         ;Delay
            POP DE
            POP BC
            POP HL
            PUSH HL
            DJNZ charhshiftloop
            POP HL
            EXX
            INC HL
            JR scrollloop
            RET

SCREENPTR     EQU 16384
SCREENWIDTH_C EQU 32
LINEY         EQU 184
CHARHEIGHT    EQU 8
CHARWIDTH     EQU 8
CHAROFFSET    EQU 32
CHARSVAR      EQU charsetaddr

