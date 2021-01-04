; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      color.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************




;*****************************************************************
;* COLOR       <ON | OFF> Enable|Disable SW & HW color
;* FOREGROUND  color# Set Foreground color (text)
;* HIGHLIGHT   color# Set Highlight color (text)
;* BACKGROUND  color# Set VIC Background color
;* BORDER      color# Set VIC Border color
;*****************************************************************

color           cmp     #','                            ; optional first arg
                beq     l283_3
                cmp     #on_token                       ; SOFTWARE (Editor) color mode
                beq     l283_2
                jsr     chkesc
                cmp     #off_token
l283_1          +lbne   snerr

                ldy     #'['                            ; OFF (color & attributes)
                !text $2c
l283_2          ldy     #']'                            ; ON
                lda     #esc
                jsr     _bsout                          ; do it
                tya
                jsr     _bsout
                jsr     chrget                          ; eat token
                beq     l283_6                          ; eol- exit

l283_3          jsr     chkcom                          ; else must be comma, eat & get next
; jsr chrgot  ;      [910930]
                cmp     #on_token                       ; HARDWARE (Vic) color mode
                beq     l283_4
                jsr     chkesc
                cmp     #off_token
                bne     l283_1

                lda     #%00000010                      ; OFF (monochrome)
                tsb     vic+49
                bra     l283_5

l283_4          lda     #%00000010                      ; ON
                trb     vic+49
l283_5          +lbra   chrget                          ; exit after eating last token

l283_6          rts                                     ; exit after encountering eol


foreground
                jsr     getnyb                          ; Set text foreground color
                stx     _color
                rts



highlight
                +lbeq   snerr                           ; missing args??     [911017]
                cmp     #','
                beq     l284_1                          ; options byte only

                jsr     getbyt                          ; Set text highlight color
                stx     highlight_color

l284_1          jsr     optzer                          ; set options:     [911001]
                bcc     l284_2                          ; comma but no value not given??
                txa
                and     #3                              ; 0= error msgs only
                asl                                     ; 1= REMs
                asl                                     ; 2= tokens
                asl
                sta     helper
l284_2          rts



background
                jsr     getnyb                          ; Set Vic background color
                stx     vic+33
                rts



border
                jsr     getnyb                          ; Set Vic border color
                stx     vic+32
                rts


getcomnyb
                jsr     chkcom                          ; check for comma
getnyb
                jsr     getbyt                          ; Get a nybble, check range (0-15)
chknyb
                cpx     #16
                +lbcs   fcerr
                rts



chkesc                                                  ; Check for escape token, error if not, else get next token
                cmp     #esc_command_token
                +lbne   snerr
                jsr     chrget
                +lbeq   snerr                           ; eos? report error if so
                rts



chkeos                                                  ; Check for next byte = end of statement, error if not
                jsr     chrget
                +lbne   snerr                           ; eos? report error if not
                rts


;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
