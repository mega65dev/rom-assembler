; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      joy.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



;*************************************************************
; JOY (n)  -- Return joystick status
;
; where: n =  1  return position of joystick-1
;       2  return position of joystick-2
;
; result:      0  no direction, no button
;       1-8    direction (see below), no button
;       128 no direction, button
;       129-136 direction & button  128 + [1...8]
;
; button--->  128        1
;       8     2
; stick--->  7           3
;       6     4
;          5
;
;*************************************************************

joy             jsr     conint                          ; get 1 byte arg in x
                dex
                cpx     #2                              ; make sure arg. is valid
                +lbcs   fcerr                           ; >1, error

                txa
                eor     #1                              ; invert to match legends on case
                tax
                php                                     ; save status

; jsr put_io_in_map
                sei                                     ; disable IRQ to inhibit kybd
                lda     d1pra
                pha                                     ; save kybd output lines
                ldy     #$ff
                sty     d1pra                           ; set to not read any kybd inputs

l144_1          lda     d1pra,x                         ; read joystick values
                cmp     d1pra,x                         ; debounce
                bne     l144_1

                tax                                     ; save joystick values
                pla
                sta     d1pra                           ; reset kybd output lines
                txa                                     ; restore joystick values
                plp                                     ; restore status
                and     #$0f                            ; test which direction
                tay
                lda     joytab-5,y                      ; get direction indicator
                tay                                     ; save direction : 0-8
                txa                                     ; restore joystick value
                and     #$10                            ; test if button triggered
                bne     l144_2                          ; skip if not
                tya
                ora     #$80                            ; show trigger depressed
                tay
l144_2          +lbra   sngflt                          ; float 1 byte arg in y.

joytab          !text 4,2,3,0,6,8,7,0,5,1,0

;.end


; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
