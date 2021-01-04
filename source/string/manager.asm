; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      manager.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************
; FRETMP is passed a string descriptor pntr in (a,y).  A check is made to see
; if the string descriptor points to the last temporary descriptor allocated by
; putnew.  If so, the temporary is freed up by the updating of (temppt).  If a
; string is freed up, a further check sees if it was the last one created and if
; so, (fretop) is updated to reflect the fact that the space is no longer in use.
; The address of the actual string is returned in (x,y) and its length in (a).

frmstr          jsr     frmevl

frestr          jsr     chkstr                          ; make sure it's a string
frefac          lda     facmo                           ; free up string pointed to by FAC
                ldy     facmo+1
fretmp          sta     index                           ; get length for later
                sty     index+1
                jsr     fretms                          ; check desc. if last
                bne     l160_3                          ; one then scratch it
                jsr     stradj                          ; index points to link
                bcc     l160_3                          ; literal no fix

                phx                                     ; .x=length
                dey                                     ; .y=1
                ldx     #index
                lda     #$ff                            ; flag string as garbage
                jsr     sta_far_ram1                    ; sta (index),y
                pla
                pha                                     ; get length, but leave copy on stack
                dey
                ldx     #index
                jsr     sta_far_ram1 ;sta (index),y     ; put in length

                eor     #$ff                            ; put index back
                sec                                     ; to first byte
                adc     index
                ldy     index+1
                bcs     l160_1
                dey
l160_1          sta     index
                sty     index+1

                tax                                     ; lo into x
                pla                                     ; pull length from stack
                cpy     fretop+1                        ; = to fretop?
                bne     frerts
                cpx     fretop
                bne     frerts


; The string was the last one put into string space.  Save garbage
; collection some time by freeing up. (length + 2)

                pha                                     ; save length on stack
                sec                                     ; plus one
                adc     fretop
                sta     fretop
                bcc     l160_2
                inc     fretop+1
l160_2          inw     fretop                          ; + one more
                pla                                     ; pull length off stack
                rts


l160_3          ldy     #0                              ; set up x,y,a and index
                jsr     indin1_ram1                     ; length
                pha                                     ; on stack
                iny
                jsr     indin1_ram1                     ; pointer lo
                tax
                iny
                jsr     indin1_ram1                     ; pointer hi
                tay
                stx     index
                sty     index+1
                pla                                     ; get back length
                rts



fretms          cpy     lastpt+1                        ; last entry to temp?
                bne     frerts
                cmp     lastpt
                bne     frerts
                sta     temppt
                sbc     #strsiz                         ; point to lst one
                sta     lastpt                          ; update temp pointer
                ldy     #0                              ; also clears zflg so we do rest of fretmp
frerts          rts                                     ; all done

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
