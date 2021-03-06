; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      viewport.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



;*****************************************************************
;*   VIEWPORT <CLR | DEF>  x, y, viewport_width, viewport_height
;*
;* assumes SCREEN already opened   910626 FAB
;*****************************************************************

C65__Viewport
                pha                                     ; save secondary command
                jsr     chrget                          ; advance past it
                jsr     CheckGraphicMode                ; make sure a screen is open

                jsr     sadwrd                          ; get x0
                sty     GKI__parm1
                sta     GKI__parm2

                jsr     comsad                          ; get y0
                sty     GKI__parm3
                sta     GKI__parm4

                jsr     comsad                          ; get width (delta-x)
                sty     GKI__parm5
                sta     GKI__parm6

                jsr     comsad                          ; get height (delta-y)
                sty     GKI__parm7
                sta     GKI__parm8

                pla                                     ; dispatch per secondary token...
                cmp     #clr_token
                beq     l281_1
                cmp     #def_token
                +lbne   snerr                           ; error

                jmp     ($8030)                         ; define viewport & return

l281_1          jmp     ($8022)                         ; clear viewport (???? make this a box command)


C65__copy
C65__cut
C65__paste
                jmp     bad_command


; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
