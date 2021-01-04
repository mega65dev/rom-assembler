; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      text,print.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



;***********************************************************
;*
;* PRINT   PRINT#   CMD
;*
;**********************************************************

printn          jsr     cmd                             ; docmd
                +lbra   release_channels                ; restore terminal


cmd             jsr     getbyt
                beq     l54_1
                lda     #','                            ; comma?
                jsr     synchr

l54_1           php                                     ; save stat (beq=eof)
                pha                                     ; save char
                stx     channl                          ; channel to output on
                jsr     coout
                pla                                     ; get char back
                plp                                     ; get stat back
                bra     print


strdon          jsr     strprt

newchr          jsr     chrgot                          ; reget last character

print           beq     crdo                            ; terminator only, so print crlf
                cmp     #using_token
                +lbeq   using



printc          beq     prtrts  ;here after seeing TAB(x) or "," or "; " in which case
;a terminator does not mean a crlf but just RTS
                cmp     #tab_token                      ; TAB function?
                beq     taber                           ; yes (c=1)
                cmp     #spc_token                      ; space function?
                clc                                     ; clear carry
                beq     taber                           ; yes (c=0)
                cmp     #','                            ; comma?
                beq     comprt                          ; yes
                cmp     #';'                            ; a semicolon?
                beq     notabr                          ; yes

                jsr     frmevl                          ; evaluate the formula
                bbs7    valtyp,strdon                   ; branch if a string
                jsr     fout
                jsr     strlit                          ; build descriptor
                jsr     strprt                          ; print the number
                jsr     outspc                          ; print a space
                bra     newchr                          ; always goes



crdo            lda     #cr
                jsr     outch                           ; outdo

crfin           bbr7    channl,prtrts
                lda     #lf
                jsr     outch                           ; outdo
; eor #$ff  ;????

prtrts          rts



comprt          sec
                jsr     _plot                           ; get tab position in x
                tya
                sec
morco1          sbc     #column_width
                bcs     morco1
                eor     #$ff
                adc     #1
                bne     aspac

taber           php                                     ; remember if SPC(c=0) or TAB(c=1) function
                sec
                jsr     _plot                           ; read tab position
                sty     trmpos
                jsr     gtbytc                          ; get value into accx
                cmp     #')'
                +lbne   snerr
                plp
                bcc     xspac
                txa
                sbc     trmpos
                bcc     notabr                          ; negative, don't print any
aspac           tax
xspac           inx
xspac2          dex
                bne     xspac1


notabr          jsr     chrget                          ; reget last character
                bra     printc                          ; don't call crdo


xspac1          jsr     outspc
                bne     xspac2


; STROUT Routine
;
; Print the string pointed to by .x.  It must end with a null byte.

strout          jsr     strlit                          ; get a string literal

strprt          jsr     frefac                          ; return temp pointer
                tax                                     ; put count into counter
                ldy     #0
                inx                                     ; move one ahead
strpr2          dex
                beq     prtrts                          ; all done
                jsr     indin1_ram1                     ; lda (index),y
                jsr     outch                           ; outdo
                iny
                cmp     #cr
                bne     strpr2
                jsr     crfin                           ; type rest of carriage return
                bra     strpr2                          ; and on and on

outspc          lda     channl                          ; if terminal print skip chr., else print space
                bne     realsp
                lda     #29                             ; CBM cursor right (non-destructive skip char)
                !text $2c

realsp          lda     #' '                            ; space
                !text $2c

outqst          lda     #'?'

;outdo
                jmp     outch                           ; output char in .a
; and #$ff ;????
; rts

;.end


; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
