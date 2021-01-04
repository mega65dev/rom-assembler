


;*********************************************************************
; RETURN Routine
;
; Restores the line number and text pointer from the stack, and
; eliminates all the FOR entries in front of the GOSUB entry.
;
;*********************************************************************

return

; Ok, pay attention: we got here by a pseudo-jsr which left a return to NEWSTT
; on the stack for us to return to.  There is also a return to NEWSTT left on
; the stack from the GOSUB we are returning from.  This is true UNLESS we got
; here on a sprite collision, in which case we still have the NEWSUB return
; recently left by our current call, but the second return goes back to the
; trapping mechanism.  The bottom line is: we have an extra return address on
; the stack, which we have to get rid of before leaving.

                 pla                                      ; mea culpa, mea culpa, mea culpa
                 pla
                 lda #gosub_token
                 jsr search                               ; look for GOSUB on runtime stack
                 beq ret010                               ; found
                 ldx #errrg                               ; else error
                 +lbra error

ret010           jsr movfnd                               ; (fndpnt) => (tos)
                 ldy #lengos
                 jsr rlsstk                               ; effectivly pop GOSUB off run-time stack
; dey
; lda (fndpnt),y
; sta txtptr+1
; dey
; lda (fndpnt),y
; sta txtptr
; dey
; lda (fndpnt),y
                 jsr retpat                               ; 01/18/84 patch: correct RETURN to GOSUB from direct mode
; lda (fndpnt),y
; sta curlin ;jump to DATA to waste rest of stmt (in case of ON..GOSUB)
                 bra data

;.end