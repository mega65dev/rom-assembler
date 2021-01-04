; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      utils.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************


n32768          !text $90,$80,0,0,0


flpint          jsr     ayint
                lda     facmo
                ldy     faclo
                rts


intidx          jsr     chrget
                jsr     frmevl                          ; get a number


posint          jsr     chknum
                lda     facsgn
                bmi     nonono                          ; if negative, blow him out


ayint           lda     facexp
                cmp     #$90                            ; FAC > 32767?
                bcc     qintgo
                lda     #<n32768                        ; get address of -32768
                ldy     #>n32768
                jsr     fcomp                           ; see if FAC=((x))

nonono          +lbne   fcerr                           ; no, FAC is too big
qintgo          +lbra   qint                            ; go shove it


; Float an unsigned double byte integer
; Entry:  MSB in (a), LSB in (y)

nosflt          jsr     stoint
                sec                                     ; sign is positive
                +lbra   floatc



pos             sec
                jsr     _plot                           ; get tab pos in .y

sngflt          lda     #0
                +lbra   givayf                          ; float it



stoint          ldx     #0                              ; move int to fac & compute proper exponents
                stx     valtyp
                sta     facho
                sty     facho+1
                ldx     #$90
storts          rts



; See if we are in direct mode, and complain if so.

errdir          bbs7    runmod,storts                   ; goto error if not in run mode

                ldx     #errid                          ; input direct error code
                !text $2c

errguf          ldx     #erruf
                +lbra   error


errind          bbr7    runmod,storts                   ; goto error if not in direct mode
                ldx     #erroid
                +lbra   error

;.end


; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
