

;************************************************************************
;*   RMOUSE Returns in variable list current status of mouse *
;*         *
;*   Syntax: RMOUSE [Xposition [,Yposition [, Buttons] ]]  *
;*         *
;*   Where: X,Yposition = current position of mouse pointer sprite *
;*  Button      = current status of mouse buttons  *
;*         *
;*   0   = no button     *
;*   1   = right button    *
;*   128 = left button    *
;*   129 = both buttons    *
;*         *
;* If a mouse is not installed, "-1" is returned for all vars. *
;* If both ports are enabled, buttons from each port are merged. *
;************************************************************************

rmouse           lda #0                                   ; Init
                 sta count                                ; variable count = 0
                 dec
                 ldx #6-1
l260_1           sta grapnt,x                             ; positions/buttons = -1
                 dex
                 bpl l260_1

                 lda _mouse_enable                        ; Is there a mouse in the house?
                 and #%11000000
                 beq l260_5                               ; no, exit
                 pha                                      ; yes, save port assigns for later
                 sei
                 ldy _mouse_pointer                       ; Where is it?  Get pointer to sprite
                 lda vic,y                                ; Get X position    ???vic_save
                 sta grapnt                               ; lsb
                 lda sbits,y
                 and vic+16                               ; msb    ???vic_save
                 beq l260_2
                 lda #1                                   ; convert to 0 or 1
l260_2           sta grapnt+1
                 iny                                      ; Get Y position
                 lda vic,y                                ; lsb    ???vic_save
                 sta grapnt+2
                 lda #0                                   ; msb (fake it)
                 sta grapnt+3

                 sta grapnt+4                             ; Init button status
                 sta grapnt+5
                 ldz d1pra                                ; Set up port & read buttons
                 lda #$ff                                 ; save kybd output lines (IRQ already disabled)
                 sta d1pra                                ; set to not read any kybd inputs

                 ldy #0                                   ; which port?
                 plx                                      ; recall port assignments
l260_3           txa
                 asl                                      ; .c=1 if this one
                 tax
                 bcc l260_4                               ; not this one
                 lda d1pra,y                              ; read it (logical port is opposite physical port)
                 and #%00010001                           ; want left, right buttons only
                 eor #%00010001                           ; (invert, since low means button down)
                 tsb grapnt+4
                 and #%00010000                           ; shift left button to msb
                 beq l260_4
                 smb7 grapnt+4
l260_4           iny                                      ; next port
                 cpy #2
                 bcc l260_3

                 lda #%01111110                           ; clean up
                 trb grapnt+4                             ; fix button register
                 stz d1pra                                ; restore port for Kernel
                 cli

; At this point, we have snapshot the current mouse status.
; Now pass requested info along in a manner very similar to RREG...

l260_5           jsr chrgot                               ; Get a variable name from variable list
                 beq l260_8                               ; eol- exit
                 cmp #','                                 ;
                 beq l260_7                               ; null- skip this arg
                 jsr ptrget                               ; Get pointer to target variable
                 sta forpnt                               ; set up so we can share LET code
                 sty forpnt+1
                 lda valtyp                               ; what kind of variable name did ptrget find?
                 +lbne chkerr                             ; string- type mismatch error

l260_6           ldx count                                ; Make assignment
                 ldy grapnt,x                             ; low byte
                 lda grapnt+1,x                           ; high byte
                 jsr givayf                               ; float it
                 lda intflg                               ; set flags for type of var (int/float)
                 jsr qintgr                               ; use part of LET to do the work

l260_7           inc count                                ; Next assignment
                 inc count
                 ldx count
                 cpx #6                                   ; there are 3 possible
                 bcs l260_8                               ; done all 3, exit
                 jsr chrgot                               ; check terminator
                 beq l260_8                               ; eol- exit
                 jsr chkcom                               ; check delimiter
                 bra l260_5                               ; loop until done

l260_8           rts

;.end