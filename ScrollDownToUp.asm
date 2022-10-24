; Code file
            org $8000
;-------------------------------------------------------
;PROGRAM ENTRY POINT
;-------------------------------------------------------
            DI
            JP main

textlines:
            DEFM "Text line1"
            DEFB 0
            DEFM "Text line2"
            DEFB 0
            DEFM "Text line3"
            DEFB 0
            DEFM "THIS IS ANOTHER LINE"
            DEFB 0
            DEFM "And another one!"
            DEFB 0, 0

currsympos  DEFB 0
currstrlen  DEFB 0
isrestart   DEFB 0
lineaddr    DEFW 0
charsetaddr DEFW $3D00

;-------------------------------------------------------
;FUNCTION strlen
;Description: calculates a lenght of string
;-------------------------------------------------------
;Input:  HL - pointer to the text line
;Output: HL - pointer to the next line
;        B  - string lenght
;-------------------------------------------------------
strlen
            XOR A
            LD B, A
strlenloop:
            CP (HL)
            INC HL
            JR Z, strlenend
            INC B
            JR strlenloop
strlenend:
            RET
;-------------------------------------------------------
;END OF FUNCTION strlen
;-------------------------------------------------------

;-------------------------------------------------------
;FUNCTION getLineAddr
;Input:  B  - `y` line position in pixels
;Output: HL - line address in screen memory
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
            LD A, B          ;Cut the firts
            AND 00000111b    ;three bits of the `y`
            OR H             ;Combine the H register of the base video address
            LD H, A          ;with the firs three bits
            LD A, B          ;Cut the last
            AND 11000000b    ;two bits of the `y`
            RRA              ;and put them
            RRA              ;to the fourth and
            RRA              ;third bit of the register H
            OR H             ;of the address
            LD H, A          ;in video memory
            LD A,B           ;Finally cut the
            AND 00111000b    ;5th, 4th and 3rd bits
            RLA              ;and put them to
            RLA              ;bits 7, 6 and 5
            LD L, A          ;of the L register of the address in video memory
            RET
;-------------------------------------------
;END OF FUNCTION getLineAddr
;-------------------------------------------

main:
            XOR A
            LD (isrestart), A
            LD B, SCREENROWS
            PUSH BC
            LD HL, textlines
repeatcycle:
;Store the initial line address
            LD (lineaddr), HL
            CALL strlen
            LD A, B
            LD (currstrlen), A
            CP 0
            JR Z, restart
;Store the results of strlen
repeatscroll1:
            PUSH HL            ;Store the next line address
            LD B, SYMBOLHEIGHT
repeatscroll:
            LD A, B
            LD (currsympos), A ;Store the symbol height to iterate
;Make the screen scrolls up
            LD B, SCREENHEIGHT - 1
            XOR A              ;Start scroll from the 0 line
scrollscreenupcycle:
            EXX
            LD B, A
            CALL getLineAddr
            EX DE, HL
            INC B
            CALL getLineAddr
            PUSH BC
            LD BC, LINESIZE
            LDIR
            POP AF
            EXX
            DJNZ scrollscreenupcycle
            LD DE, SCREENPTR + SCREENSIZE - LINESIZE
            LD A, (currstrlen)
            LD B, A             ;Restore the line length
            LD A, LINESIZE
            SUB B
            LD C, A
            LD HL, (lineaddr)
            LD A, B
            CP 0
            JR Z, zerostrlen
repeatprintcycle:
            LD A, (HL)
            SUB CHAROFFSET
            EXX
            LD HL, (CHARSVAR)
            LD B, 0             ;Multiply A by 8
            LD C, A
            RL C                ;Carry flag will be 0 in any case,
            RL B                ;so it is no need to
            RL C                ;clear it by force.
            RL B
            RL C
            RL B                ;BC = A*8
            ADD HL, BC
            LD A, (currsympos)  ;Restore the current
            LD B, A             ;symbol height pos
            LD A, SYMBOLHEIGHT
            SUB B
            LD B, 0
            LD C, A
            ADD HL, BC
            LD A, (HL)
            EXX
            LD (DE), A
            INC DE
            INC HL
            DJNZ repeatprintcycle
            LD A, C
            CP 0
            JR zerotoclear
zerostrlen:
            XOR A
            LD B, C
repeatclearcycle:
            LD (DE), A
            INC DE
            DJNZ repeatclearcycle
zerotoclear:
            LD A, (currsympos)
            LD B, A             ;Restore the current symbol height pos
            DJNZ repeatscroll
            POP HL              ;Restore the next line address
            LD A, (isrestart)
            CP 0
            JP Z, repeatcycle
restart:
            LD A, 1
            LD (isrestart), A
            LD HL, (lineaddr)
            LD A, (currstrlen)
            POP BC
            DJNZ repeatscroll1caller
            JP main
repeatscroll1caller:
            PUSH BC
            JP repeatscroll1
            EI
            RET

SCREENPTR    EQU 16384
SCREENSIZE   EQU 6144
SCREENROWS   EQU 24
SCREENHEIGHT EQU SCREENROWS*8
LINESIZE     EQU 32
SYMBOLHEIGHT EQU 8
CHAROFFSET   EQU 32
CHARSVAR     EQU charsetaddr
