; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      init.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************

; INIT_STACK Routine (formerly STKINI)
;
;   Init_Stack resets the stack pointer.  String temporaries are freed up,
;   SUBFLG is reset, continuing is prohibited.

init_stack
                ply                                     ; pop return address
                pla
                ldx     #stkend-257                     ; reset system stack pointer
                txs
                pha                                     ; push return address
                phy
                ldx     #tempst                         ; reset string temporaries
                stx     temppt
                lda     #0
                sta     subflg                          ; allow subscripted & integer vars
                sta     oldtxt+1                        ; disallow continuing
                sta     bits                            ; reset math bit/byte flag

stkrts          rts



reset_txtptr
                clc                                     ; load (txtptr) with (txttab)-1
                lda     txttab
                adc     #$ff
                sta     txtptr
                lda     txttab+1
                adc     #$ff
                sta     txtptr+1                        ; set up text pointers
                rts

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
