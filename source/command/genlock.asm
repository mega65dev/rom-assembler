


;*****************************************************************
;* GENLOCK  set/reset genlock mode & color registers
;*
;*  Syntax: GENLOCK <ON[,color#[,...]] | OFF[,color#,R,G,B]>
;*****************************************************************

genlock          sta GKI__parm1                           ; save token as flag for set palette   [910107]
                 cmp #on_token
                 beq l282_4
                 jsr chkesc
                 cmp #off_token
                 +lbne snerr
;TURN GENLOCK OFF
                 lda vic+49                               ; any interlaced bitplanes on?
                 and #%00011001
                 cmp #%00011001
                 beq l282_1                               ; yes, leave interlace mode on

                 lda #%00000001
                 trb vic+49                               ; no, turn interlace off
l282_1           lda #%00000010
                 trb vic+48                               ; reset external sync mode
; beq l282_2   ;       [910114]
; lda vic+63  ;       [910111]
; inc a   ;  adjust vert. position (chip bug ????)
; inc a   ;  (to be handled by a custom C65 genlock board)
; inc a
; sta vic+63

l282_2           jsr chrget                               ; eat token
                 jsr optbyt                               ; get (optional) color reg# in .X
                 stx GKI__parm2                           ; save it
                 +lbcs set_palette                        ; if present, go do it & exit
l282_3           rts                                      ; if not present (eol), exit


l282_4           lda #%00000001                           ; TURN GENLOCK ON
                 tsb vic+49                               ; set interlace mode
                 asl
                 tsb vic+48                               ; set external sync mode
; bne l282_5   ;       [910114]
; lda vic+63  ;       [910111]
; dec a   ;  adjust vert. position (chip bug ????)
; dec a   ;  (to be handled by a custom C65 genlock board)
; dec a
; sta vic+63

l282_5           jsr chrget                               ; eat token
l282_6           jsr optbyt                               ; get (optional) color reg# in .X
                 bcc l282_3                               ; if not present (eol), exit
                 lda #%00010000                           ; if present, set FGBG bit in red palette
                 sta _red,x
                 bra l282_6                               ; loop
