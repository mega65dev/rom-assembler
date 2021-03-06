; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      mouse.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



;***********************************************************************
;*   MOUSE  ON  [,[port] [,[sprite] [,[hotspot] [,X/Yposition] ]]]
;*   MOUSE  OFF
;*    where: port     = (1...3) for joyport 1, 2, or either (both)
;*  sprite   = (0...7) sprite pointer
;*  hotspot  = x,y offset in sprite, default 0,0
;*  position = normal, relative, or angluar coordinates
;*
;*      (defaults to sprite 0, port 2, last hotspot & position)
;***********************************************************************

mouse           cmp     #on_token                       ; new [910122]
                beq     l259_1
                jsr     chkesc
                cmp     #off_token
                +lbne   snerr

;    The Kernel MOUSE_CMD is called to install or remove mouse driver.
; .a= B7,6 set to install mouse in game port 2 ($80), 1 ($40), or both ($C0)
; .a= 0 to disable mouse driver
; .x= 0-7 physical sprite pointer

                lda     #0                              ; TURN MOUSE OFF
                jsr     _mouse                          ; do it
                +lbra   chkeos                          ; eat token & exit after checking for eos

;TURN MOUSE ON
l259_1          jsr     chrget                          ; eat token
                ldx     #2                              ; get (optional) port# in .X
                jsr     optbyt                          ; if not present default to port 2
                cpx     #4                              ;
                +lbcs   fcerr                           ; illegal value
                phx

                ldx     #0                              ; get (optional) sprite# in .X
                jsr     optbyt                          ; if not present default to sprite 0
                cpx     #8
                +lbcs   fcerr                           ; illegal value
                stx     z_p_temp_1
                ldy     sproff,x                        ; kill moving sprite
                lda     #0                              ; get offset to speed data
                sta     sprite_data,y                   ; reset sprite's speed value

                pla                                     ; setup for Kernel call- get port# into b7,6
                ror                                     ; .a= port(s), .x=sprite
                ror
                ror
                jsr     _mouse                          ; do it (???? do after coord error check)


                jsr     optbyt                          ; get (optional) hotspot, x  new [910307]
                bcc     l259_2                          ; not given
                cpx     #24
                +lbcs   fcerr                           ; out of range (0-23)
                txa
                neg
                tax
                adc     #24
                sta     _mouse_left
                txa
                clc
                adc     #87
                sta     _mouse_right

l259_2          jsr     optbyt                          ; get (optional) hotspot, y
                bcc     l259_3                          ; not given
                cpx     #21
                +lbcs   fcerr                           ; out of range (0-20)
                txa
                neg
                tax
                adc     #50
                sta     _mouse_top
                txa
                clc
                adc     #250
                sta     _mouse_bottom

l259_3          jsr     chrgot                          ; get (optional) position coordinate  [910123]
                beq     l259_4                          ; eol, use this sprite's last position
                jsr     sprcor                          ; else get first coordinate
                bit     numcnt                          ; test coordinate type
                +lbvs   snerr                           ; syntax error
                sty     xdest                           ; save coordinate value
                sty     xdest+2
                sta     xdest+1
                sta     xdest+3

                lda     #$7f                            ; flag 'mouse' for movspr call  [910808]
                sta     op
                jsr     sprcor                          ; get second coordinate
                bit     numcnt                          ; test type of coordinate
                +lbvc   movspr_normal                   ; position sprite, normal coordinates
                +lbmi   movspr_angle                    ; angular coordinates
                +lbra   snerr                           ; else error

l259_4          rts

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
