            org 50000

            LD HL, 0101100000000000b
            LD B, 24
            LD A, 0
            LD DE, 32
loop2:
            LD (HL), A
            ADD A, 8
            ADD HL, DE
            DJNZ loop2

;            LD HL, 0101100000000000b
;            EX DE, HL
;                          yyyxxxxx 
            LD C, 191
            CALL getLineAddr
;            LD DE, 31
;            ADD HL, DE
            LD (HL), 10101010b

;------------------------------------
;Determine of color attribute address
;Input: HL - pixel screen address
;------------------------------------
            LD A, H
            AND 00011000b           ;Cut the Y7 and Y6 bits of the address
            RRA                     ;make them
            RRA                     ;last bits
            RRA                     ;of address
            OR 01011000b            ;combine with base attibutes memory address
            LD H, A                 ;and assign to H
;------------------------------------

            LD (HL), 10101010b
;            LD A, H
;            OR D
;            LD H, A
;loop1:
;            LD (HL), 10101010b
            ;INC H
            ;DJNZ loop1

;            LD HL, 16897
;            LD (HL), 10101010b
;            LD HL, 22529
;            LD (HL), 10101010b

            RET

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

SCREENPTR     EQU 16384

