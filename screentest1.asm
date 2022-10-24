            org 50000

            LD B, YPOS
            CALL getLineAddr
            LD B, 0
            LD C, SCREENWIDTH_C - 1
            ADD HL, BC
            LD B, C
            INC B
loop1:
            LD A, (HL)
            RLA
            LD (HL), 10101010b
            DEC HL
            DJNZ loop1
            RET

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

YPOS          EQU 186
SCREENWIDTH_C EQU 32
SCREENPTR     EQU 16384

