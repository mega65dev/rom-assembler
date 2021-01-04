; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      peekpoke.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



peek            phw     poker                           ; ..also happens to be LINNUM!   [910911]
                jsr     chknum
                jsr     getadr
                ldy     #0                              ; index
                bit     current_bank
                bmi     l140_1                          ; NOMAP?

                phz
                ldz     current_bank                    ; set up bank number for Kernel's fetch
                ldx     #poker                          ; ..and address
                jsr     _lda_far                        ; lda (poker),y
                plz
                !text $2c

l140_1          lda     (poker),y
                tay                                     ; get byte into .y
                pla
                sta     poker+1                         ; restore linnum
                pla
                sta     poker
                +lbra   sngflt                          ; float it


poke            jsr     getnum
l141_1          txa                                     ; set up value to store for Kernel 'stash' routine
                ldy     #0                              ; ..and index
                sei                                     ; to allow poking IRQ vector, etc.  [910612]
                bit     current_bank
                bmi     l141_2                          ; (anything >1Meg means NOMAP)

                phz
                ldx     #poker                          ; ..and address
                ldz     current_bank                    ; ..finally, get the bank number
                jsr     _sta_far                        ; sta (poker),y
                plz
                !text $2c

l141_2          sta     (poker),y                       ; NoMap

l141_3          jsr     chrgot                          ; eol?
                beq     l141_4                          ; yes
                inw     poker                           ; no- increment address
; lda poker  ; check for segment wrap (FFFF->0000) [910911]
; ora poker+1
                +lbeq   omerr                           ; [910916]
                jsr     optbyt                          ; & get next [,byte]
                bcs     l141_1

l141_4          cli                                     ; [910612]
                rts


;.end


; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
