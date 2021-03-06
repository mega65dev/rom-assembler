; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      hexstr.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



hexd            jsr     chknum
                phw     poker                           ; save linnum    [910911]
                jsr     getadr                          ; 2 byte val in (poker)
                lda     #4
                jsr     strspa
                ldy     #0
                lda     poker+1
                jsr     hexit
                lda     poker
                jsr     hexit
                pla                                     ; restore linnum
                sta     poker+1
                pla
                sta     poker
                +lbra   chrd1                           ; pla,pla,jmp putnew

hexit           pha
                lsr
                lsr
                lsr
                lsr
                jsr     dohex
                pla

dohex           and     #$0f
                cmp     #$0a
                bcc     l143_1
                adc     #6
l143_1          adc     #'0'
                phx
                ldx     #dsctmp+1
                jsr     sta_far_ram1                    ; sta (dsctmp+1),y
                plx
                iny
                rts

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
