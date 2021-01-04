; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      for.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************

; FOR
;
; Push the following information on the run-time stack:
;
; (bottom)   highest memory
; =========================
;  txtptr    address of next statement
;  txtptr+1
;  ========
;  curlin+1  current line number
;  curlin
;  ========
;  to lo
;  to mo
;  to moh    'to' value
;  to ho
;  to exp
;  ========
;  step sign
;  step lo
;  step mo
;  step moh  'step' value
;  step ho
;  step exp
;  ========
;  forpnt+1  'for' variable pointer
;  forpnt
;  ========
;  'for' token       <== (tos) top of stack pointer
; ============================
; (top of stack)  lowest memory


for             lda     #$80
                sta     subflg                          ; no arrays(), no integers%
                jsr     let                             ; get & set FOR variables
                lda     #for_token                      ; set up for call to see if
                jsr     search                          ; ..this 'for' variable is unique
                beq     l82_1                           ; branch if not

; If the variable is not unique, (fndpnt) will point to last occurance
; in stack, and we will reset the stack to that point.  Otherwise we
; will adjust the pointer by 'lenfor' and start from that point.

                lda     #lenfor
                jsr     getstk                          ; updates stack pointer, error if overflow
                jsr     movtos                          ; (tos) => (fndpnt)

l82_1           jsr     movfnd                          ; (fndpnt) => (tos)   (redundant for new entries)
                jsr     datan                           ; find address of next statement
                tya                                     ; offset from (txtptr) in y
                ldy     #lenfor-1

                clc                                     ; Push address of next statement on stack
                adc     txtptr
                sta     (tos),y                         ; (common area)
                lda     txtptr+1
                adc     #0
                dey
                sta     (tos),y                         ; (common area)

                lda     curlin+1                        ; Push current line number on stack
                dey
                sta     (tos),y                         ; (common area)
                lda     curlin
                dey
                sta     (tos),y                         ; (common area)

                lda     #to_token                       ; Look for TO, must appear
                jsr     synchr
                jsr     chknum                          ; get TO value
                jsr     frmnum
                lda     facsgn
                ora     #$7f
                and     facho
                sta     facho

                ldx     #4
                ldy     #lenfor-5
l82_2           lda     facexp,x                        ; Push faclo,mo,moh,ho,exp
                sta     (tos),y                         ; (common area)
                dex
                dey
                bpl     l82_2

                lda     #<fone                          ; Push STEP value
                ldy     #>fone                          ; (point to default 'one' in ROM)
                jsr     movfm
                jsr     chrgot
                cmp     #step_token
                bne     l82_3                           ; branch if no step given
                jsr     chrget
                jsr     frmnum

l82_3           jsr     sign
                pha                                     ; save sign for a moment
                jsr     round
                pla

                ldy     #lenfor-10
                ldx     #5
l82_4           sta     (tos),y                         ; (common area)
                lda     facexp-1,x
                dey
                dex
                bpl     l82_4

                lda     forpnt+1                        ; Finally push pointer to 'for' variable, & 'for' token
                sta     (tos),y                         ; (common area)
                lda     forpnt
                dey
                sta     (tos),y                         ; (common area)
                lda     #for_token
                dey
                sta     (tos),y                         ; (common area)
                rts

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
