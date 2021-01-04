; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      screen.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



                * = $af00                               ; [911001]

;*****************************************************************
; SCREEN DEF      define a screen
; SCREEN SET  set draw, view screen
; SCREEN CLR  clear a screen
; SCREEN OPEN  open a screen
; SCREEN CLOSE  close a screen
;*****************************************************************

Screen
                cmp     #open_token                     ; else dispatch per secondary token...
                +lbeq   ScreenOpen
                cmp     #close_token
                +lbeq   ScreenClose
                cmp     #def_token
                beq     ScreenDef
                cmp     #clr_token
                beq     ScreenClr

                jsr     chkesc                          ; [910930]
; cmp #esc_command_token
; bne l266_1
; jsr chrget  ; get past escape token
                cmp     #set_token
                beq     ScreenSet
l266_1          +lbra   snerr                           ; report syntax error


CheckGraphicMode
                bit     $1f4b                           ; Check draw screen allocation   [910711]
                bmi     NoGraphicArea
                rts                                     ; ok


NoGraphicArea
                ldx     #errng                          ; bad- no graphic area????
                +lbra   error


RestoreTextScreen                                        ; [910404]
                lda     #$ff                            ; [910930]
                sta     GKI__parm1                      ; leave drawscreen as is
                sta     GKI__parm2                      ; set viewscreen to text
                jmp     ($800e)                         ; kg65.screen

; lda vic+49  ;Check graphic screen allocation
; and #%00010000
; beq 99$   ; we're in text mode
;; bit $1f43
;; bmi NoGraphicArea
;
; sei
; lda #$80
; bit _mode  ;40/80 mode, 0=80 128=40
; bmi l267_1
;
; tsb vic+49  ; 80
; lda #1
; trb vic+22  ;  fix x-scroll register
; bra 99$
;
;l267_1 trb vic+49  ; 40
; lda #1
; tsb vic+22  ;  fix x-scroll register
;
;99$ cli
;; lda #0
;; sta _graphm  ;text mode????
; rts


;*****************************************************************
; SCNCLR  clear a text or graphic screen
;
;  Syntax : SCNCLR  [ColorReg]
;
; if [ColorReg] not specified, clears text screen
; else clears the graphic screen with given value.
;*****************************************************************

ScreenClr
                jsr     chrget                          ; eat token & fall into SCNCLR

scnclr
                bne     C65__screenclear                ; have a parameter, go clear graphic screen

                lda     #$93
                jmp     _bsout                          ; no parameter, clear text screen
; rts



;*****************************************************************
;* SCREEN CLR  clear a graphic screen
;*
;*  Syntax : SCREEN CLR  color_reg#
;*
;*           parm1 = color reg#  0-255
;*****************************************************************

C65__screenclear
                jsr     getbyt                          ; get color register # (range 0-255)?????
;limit to range allowed by current screen def?
                stx     GKI__parm1
                jsr     CheckGraphicMode
                jmp     ($800c)                         ; bra screenclear


;*****************************************************************
;* SCREEN SET  specify draw & view screens
;*
;*  Syntax : SCREEN SET  [DrawScreen] [,ViewScreen]
;*
;*           parm1 = draw screen # 0-3, 255=don't change    [910711]
;*           parm2 = view screen # 0-3, 255=text
;*****************************************************************

ScreenSet
                jsr     chrget                          ; advance past token

C65__screen
; beq snerr  ;missing args??      [911017]
                ldx     #255                            ; [911028]
                cmp     #','
                beq     l267_1                          ; options byte only

                jsr     getbyt                          ; get draw screen# in .x
; cpx #4   ;       [910711]
; bcs 20$   ;  out of range error???? (255=leave alone)  [910930]
l267_1          stx     GKI__parm1

                ldx     $1f69                           ; current viewscreen     [911017]
                jsr     optbyt                          ; eat a comma, get view screen# in .x
; cpx #4   ;
;20$ bcs fcerr  ;  out of range error???? (255=text)   [910930]
                stx     GKI__parm2

                jsr     ($800e)                         ; kg65.screen
                bcs     NoGraphicArea
                rts


;*****************************************************************
;* SCREEN DEF  define a graphic screen
;*
;*  Syntax : SCREEN DEF  screen#, width, height, depth
;*
;*           parm1 = screen#           0-3    [910711]
;*           parm2 = width             0=320, 1=640, 2=1280
;*           parm3 = height            0=200, 1=400
;*           parm4 = depth             1-8 bitplanes (2-256 colors)
;*****************************************************************

ScreenDef
                jsr     chrget                          ; advance past token

C65__screendef
                jsr     getbyt                          ; get screen number
                cpx     #4                              ; range 0-3   [910711]
                bcs     l268_1
                stx     GKI__parm1                      ; screen#

                jsr     combyt                          ; get width
                cpx     #3                              ; range 0-2 ???? 1280 mode ????
                bcs     l268_1
                stx     GKI__parm2                      ; width

                jsr     combyt                          ; get height
                cpx     #2                              ; range 0-1
                bcs     l268_1
                stx     GKI__parm3                      ; height

                jsr     combyt                          ; get depth (# bitplanes)
                dex                                     ; convert 1-8 to 0-7
                cpx     #8                              ; range 0-7
l268_1          +lbcs   fcerr                           ; illegal quantity error
                stx     GKI__parm4                      ; depth

                jmp     ($8006)                         ; bra screendef


;*****************************************************************
;* SCREEN OPEN  open a graphic screen for viewing or drawing
;*
;*  Syntax : SCREEN OPEN screen#
;*
;*           parm1 = screen#      0-3    [910711]
;*****************************************************************


ScreenOpen
                jsr     chrget                          ; advance past Open token

C65__screenopen
                jsr     getbyt                          ; get screen# in .x
                cpx     #4                              ; range 0-3   [910711]
                +lbcs   fcerr                           ; branch if out of range

                stx     GKI__parm1
                jmp     ($8008)                         ; screenopen    [910826]

; bcs NoGraphicArea ; bad ???? let user catch via RGRAPHIC
; rts


;*****************************************************************
;* SCREEN CLOSE  close a graphic screen
;*
;*  Syntax : SCREEN CLOSE screen#
;*
;*           parm1 = screen#  0-3    [910711]
;*****************************************************************


ScreenClose
                jsr     chrget                          ; advance past Close token

C65__screenclose
                jsr     getbyt                          ; get screen#
                cpx     #4                              ; range 0-3   [910711]
                +lbcs   fcerr                           ; branch if out of range
                stx     GKI__parm1

                jmp     ($800a)                         ; bra screenclose


; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
