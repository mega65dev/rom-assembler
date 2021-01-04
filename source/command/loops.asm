; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      loops.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************


do              ldy     #1
l95_1           lda     txtptr,y                        ; save current pointers for stack entry
                sta     tmptxt,y
                lda     curlin,y
                sta     tmplin,y
                dey
                bpl     l95_1

                jsr     chrgot                          ; look for 'while' or 'until'
                beq     doyes                           ; unconditional
                cmp     #until_token
                beq     do10
                cmp     #while_token
                bne     snrjmp


;  Here for WHILE

                jsr     frmjmp
                lda     facexp
                bne     doyes                           ; conditional evaluated true

dono            jsr     chrgot
                bra     fnd010                          ; advance to end of block, do rts


;  Here for UNTIL

do10            jsr     frmjmp
                lda     facexp
                bne     dono

doyes           lda     #5                              ; 'do' needs 5 bytes on the run-time stack
                jsr     getstk
                ldy     #4                              ; ..now stuff those 5 bytes!
                lda     tmptxt+1
                sta     (tos),y                         ; (common area)
                dey
                lda     tmptxt
                sta     (tos),y                         ; (common area)
                dey
                lda     tmplin+1
                sta     (tos),y                         ; (common area)
                dey
                lda     tmplin
                sta     (tos),y                         ; (common area)
                dey
                lda     #do_token
                sta     (tos),y                         ; (common area)
                rts


;  Here for EXIT

exit            jsr     popdgo                          ; pop do entry off stack
                jsr     chrgot
                beq     fnd010
snrjmp          +lbra   snerr



;  Find end of current block

fndend          jsr     chrget

fnd010          beq     l96_2                           ; end of statement
                cmp     #loop_token
                +lbeq   data                            ; a hit!  read to end of statement, rts
                cmp     #'"'                            ; quote
                beq     l96_1
                cmp     #do_token
                bne     fndend                          ; keep looking
                jsr     fndend                          ; recursivly
                bra     dono                            ; do a chrgot, go to fnd010


l96_1           jsr     un_quote                        ; look for terminating quote, or end of statement
                bne     fndend                          ; character after quote wasn't terminator, keep going

l96_2           cmp     #':'                            ; end of line or end of stmt?
                beq     fndend                          ; just stmt, keep going
                bbr7    runmod,fnderr                   ; if direct mode, not found error
                ldy     #2
                jsr     indtxt                          ; end of text?
                beq     fnderr                          ; 'fraid so
                iny                                     ; y=3
                jsr     indtxt                          ; update pointers
                sta     curlin
                iny
                jsr     indtxt
                sta     curlin+1
                tya
                clc
                adc     txtptr
                sta     txtptr
                bcc     fndend
                inc     txtptr+1
                bra     fndend


loop            beq     popngo                          ; no conditionals, just do it
                cmp     #while_token
                beq     loop10
                cmp     #until_token
                bne     snrjmp

;  Here for UNTIL

                jsr     frmjmp
                lda     facexp
                beq     popngo                          ; false, do it again!

popdgo          lda     #do_token                       ; pop, but don't go
                jsr     search
                bne     poperr                          ; branch if not found
                jsr     movfnd
                ldy     #5
                +lbra   rlsstk


fnderr
                lda     tmplin                          ; loop not found error: must make curlin match oldtxt
                ldx     tmplin+1
                sta     curlin
                stx     curlin+1

                ldx     #errlnf
                !text $2c
poperr
                ldx     #errlwd                         ; loop without do
                +lbra   error



;  Here for WHILE

loop10
                jsr     frmjmp
                beq     popdgo                          ; false, exit
popngo
                bsr     popdgo
; dey
; lda (fndpnt),y ;restore pointers
; sta txtptr+1
; dey
; lda (fndpnt),y
; sta txtptr
; dey
; lda (fndpnt),y
                jsr     retpat                          ; (** 01/18/84 fixes 'loop' to a direct mode 'do')
; lda (fndpnt),y
; sta curlin
                +lbra   do

frmjmp
                jsr     chrget
                +lbra   frmevl

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
