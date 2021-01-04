


; Here for new statement. Character -> by txtptr is ':' or eol. The adr of
; this loc is left on the stack when a statement is executed so that it can
; merely do a rts when it is done.
; Get char, exit via xeqcm3, and return to newstt.

xeqcm            jmp (igone)

; Check if there is an interrupt from VIC that needs to be serviced

ngone            bbr7 runmod,l12_3                        ; get off here if we are in direct mode
                 lda intval                               ; check if there is an interrupt already in progress
                 bmi l12_3                                ; yes, don't go any further

                 ldx #2                                   ; check for 3 types of interrupts: s/s, s/b, & lp
l12_1            lda int_trip_flag,x
                 beq l12_2                                ; this wasn't set, go check next

                 lda #0
                 sta int_trip_flag,x                      ; reset this flag to show 'serviced'
                 lda int_adr_lo,x                         ; install the trap address as linnum
                 sta linnum
                 lda int_adr_hi,x
                 sta linnum+1
                 phx                                      ; save counter & text pointer
                 phw txtptr
                 lda #$80                                 ; flag 'no other interrupt traps, please'
                 tsb intval

                 jsr chrget                               ; skip over 2nd byte of line number
                 jsr gosub_sub                            ; fake a 'gosub' from here, so trap rx can do a RETURN
                 jsr goto_1
                 jsr newstt

                 lda #$80
                 trb intval
                 pla
                 sta txtptr+1
                 pla
                 sta txtptr
                 plx

l12_2            dex
                 bpl l12_1


l12_3            jsr chrget                               ; get statement type
xeqdir           jsr xeqcm3

newstt           jsr is_stop_key_down
                 bbr7 runmod,l13_1                        ; branch if direct mode

; In run mode...save txtptr for CONTinue command

                 jsr tto                                  ; transfer txtptr to oldtxt
                 tsx
                 stx oldstk

l13_1            ldy #0
                 jsr indtxt                               ; end of the line?
                 +lbne morsts                             ; no...out of statement

l13_2            bit runmod                               ; in direct mode?
                 +lbpl ready                              ; yes, go to ready
                 ldy #2
                 jsr indtxt                               ; end of text?
                 +lbeq ready                              ; yes...finished
                 iny                                      ; y=3
                 jsr indtxt                               ; extract line# lo byte
                 sta curlin
                 iny
                 jsr indtxt                               ; extract line # hi byte
                 sta curlin+1
                 tya                                      ; y=4
                 clc
                 adc txtptr                               ; point @ character before line start
                 sta txtptr
                 bcc l13_3
                 inc txtptr+1
l13_3            +lbra xeqcm                              ; execute new line



tto              lda txtptr
                 ldy txtptr+1
                 sta oldtxt
                 sty oldtxt+1
xeqrts           rts


; Set up for command processing and set processor address on stack.
; Exit via jmp to CHRGET

xeqcm3           beq xeqrts                               ; nothing here...null statement
                 bbr5 runmod,xeqcm2                       ; trcflg. branch if trace not enabled
                 bbr7 runmod,xeqcm2                       ; branch if direct mode- can't trace

                 pha                                      ; save token
                 lda #'['                                 ; print '[line-number]'
                 jsr outch                                ; outdo
                 jsr curprt                               ; print curlin
                 lda #']'
                 jsr outch                                ; outdo
                 pla                                      ; restore token


xeqcm2           cmp #esc_command_token                   ; special case: escape token
                 beq xeqesc
                 cmp #go_token                            ; special case: go to
                 +lbeq go_without_to
                 cmp #mid_token                           ; special case: mid$()=
                 beq xeqmid

; Command can be in the range END...NEW (old BASIC) & ELSE...MONITOR
; (new extensions).  Although there is a gap between these two blocks,
; it will be quickest & easiest to collapse them into one continuous block.

                 cmp #monitor_token+1
                 bcs snerr1
                 cmp #new_token+1
                 bcc xeqcm4                               ; no need to collapse
                 cmp #else_token
                 bcc snerr1
                 sbc #else_token-new_token-1

xeqcm4           sec                                      ; convert adjusted token into an index into a jump table.
                 sbc #end_token
                 +lbcc let                                ; it wasn't a token after all!  assume an assignment

xeqcm5           asl                                      ; *2 to convert into word pointer
                 tay
                 bcs l14_1                                ; dispatch table 1 or 2?     [901212]
                 lda stmdsp+1,y                           ; one
                 pha
                 lda stmdsp,y
                 bra l14_2

l14_1            lda stmdsp2+1,y                          ; two      [901212]
                 pha
                 lda stmdsp2,y

l14_2            pha
                 jmp chrget                               ; execution will commence after chrget's RTS



xeqmid                                                    ; handle special case of MID$= (what we call a kludge)
                 lda #>midwrk                             ; midd2-1
                 pha
                 lda #<midwrk
                 pha
xeqchr
                 jmp chrget




xeqesc                                                    ; execute escape token
                 jsr chrget                               ; let's have us a look at the second char
                 beq snerr1                               ; oops, there wasn't any!
                 cmp #first_esc_command_token             ; is it one of our esc tokens?
                 bcc l15_1                                ; no, foreign.
                 cmp #last_esc_command_token+1
                 bcs l15_1                                ; foreign

; It's one of our own.  Convert to index into command dispatch table

                 adc #monitor_token-else_token+new_token-end_token-first_esc_command_token+2
                 bra xeqcm5                               ; always

l15_1            sec                                      ; set up flag for a trip into the users code
                 jmp (iescex)

nescex           bcc xeqchr                               ; jmp chrget

snerr1           +lbra snerr

morsts           cmp #':'
                 +lbeq xeqcm                              ; if ':', continue statement
                 bra snerr1

