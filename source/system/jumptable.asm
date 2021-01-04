; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      jumptable.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



                * = $7f00


; Format Conversions     [6]

                +lbra   ayint                           ; convert floating point to integer
                +lbra   givayf                          ; convert integer to floating point
                +lbra   fout                            ; convert floating point to PETSCII string
                +lbra   val_1                           ; convert PETSCII string to floating point
                +lbra   getadr                          ; convert floating point to an address
                +lbra   floatc                          ; convert address to floating point

; Math Functions     [24]

                +lbra   fsub                            ; MEM - FACC
                +lbra   fsubt                           ; ARG - FACC
                +lbra   fadd                            ; MEM + FACC
                +lbra   faddt_c65                       ; ARG - FACC      [910402]
                +lbra   fmult                           ; MEM * FACC
                +lbra   fmultt_c65                      ; ARG * FACC      [910402]
                +lbra   fdiv                            ; MEM / FACC
                +lbra   fdivt_c65                       ; ARG / FACC      [910402]
                +lbra   log                             ; compute natural log of FACC
                +lbra   int                             ; perform BASIC INT() on FACC
                +lbra   sqr                             ; compute square root of FACC
                +lbra   negop                           ; negate FACC
                +lbra   fpwr                            ; raise ARG to the MEM power
                +lbra   fpwrt                           ; raise ARG to the FACC power
                +lbra   exp                             ; compute EXP of FACC
                +lbra   cos                             ; compute COS of FACC
                +lbra   sin                             ; compute SIN of FACC
                +lbra   tan                             ; compute TAN of FACC
                +lbra   atn                             ; compute ATN of FACC
                +lbra   round                           ; round FACC
                +lbra   abs                             ; absolute value of FACC
                +lbra   sign                            ; test sign of FACC
                +lbra   fcomp                           ; compare FACC with MEM
                +lbra   rnd_0                           ; generate random floating point number

; Movement      [22]

                +lbra   conupk                          ; move RAM MEM to ARG
                +lbra   romupk                          ; move ROM MEM to ARG
                +lbra   movfrm                          ; move RAM MEM to FACC
                +lbra   movfm                           ; move ROM MEM to FACC
                +lbra   movmf                           ; move FACC to MEM
                +lbra   movfa                           ; move ARG to FACC
                +lbra   movaf                           ; move FACC to ARG

; bra optab ;????not executable
; bra drawln
; bra gplot
; bra cirsub
                +lbra   run
                +lbra   runc
                +lbra   clearc                          ; [910410]
                +lbra   new
                +lbra   link_program
                +lbra   crunch
                +lbra   FindLine
                +lbra   newstt
                +lbra   eval
                +lbra   frmevl
                +lbra   run_a_program
                +lbra   setexc
                +lbra   linget
                +lbra   garba2
                +lbra   execute_a_line

; Temporaries for C65 development (???? used by graphics) [12]

                +lbra   chrget
                +lbra   chrgot
                +lbra   chkcom
                +lbra   frmnum
                +lbra   getadr
                +lbra   getnum
                +lbra   getbyt
                +lbra   plsv

                +lbra   lda_far_ram0                    ; lda (.x),y from BASIC text bank [910716]
                +lbra   lda_far_ram1                    ; lda (.x),y from BASIC variable bank [910716]
                +lbra   sta_far_ram0                    ; sta (.x),y to   BASIC text bank [910716]
                +lbra   sta_far_ram1                    ; sta (.x),y to   BASIC variable bank [910716]


; Graphic Kernel Call. (Temporary for C65 development ????)
;
;  syntax:  GRAPHIC command# [,args]
;
; Basically this is a modified C64-type SYS command, minus the address.
; In the final C65 system, this will represent the ML interface, not the
; BASIC 10.0 interface which is implemented here in the development system.


graphic
                cmp     #clr_token                      ; GRAPHIC CLR (graphic system initialize)
                bne     l317_1                          ; no
                jsr     chrget                          ; yes advance past token
                jmp     ($8000)                         ; go initialize graphic kernel

l317_1
; tax
; bmi snerr  ;Syntax error if any other secondary token
;
;
                jmp     (graphic_vector)                ; Else, call the Graphics Kernel's Parser...
;
;
graphic_kernel                                          ; ...via indirect
                jmp     ($8002)


; C65 Graphic Kernel Jump Table      [910826]
;
; 8000 init   ;sets up graphic vars
; 8002 parser   ;GRAPHIC ML Parser????
;
; 8004 kg65.start-1  ;0 commands
; 8006 kg65.screendef-1 ;1
; 8008 kg65.screenopen-1 ;2
; 800a kg65.screenclose-1 ;3
; 800c kg65.screenclear-1 ;4
; 800e kg65.screen-1  ;5
; 8010 kg65.setpen-1  ;6
; 8012 kg65.setpalette-1 ;7
; 8014 kg65.setdmode-1  ;8
; 8016 kg65.setdpat-1  ;9
; 8018 kg65.line-1  ;10
; 801a kg65.box-1  ;11
; 801c kg65.circle-1  ;12
; 801e kg65.polygon-1  ;13
; 8020 kg65.ellipse-1  ;14
; 8022 kg65.viewpclr-1  ;15
; 8024 kg65.copy-1  ;16
; 8026 kg65.cut-1  ;17
; 8028 kg65.paste-1  ;18
; 802a kg65.load-1  ;19
; 802c kg65.char-1  ;20
; 802e kg65.paint-1  ;21
; 8030 kg65.viewpdef-1  ;22
; 8032 kg65.f.pixel-1  ;23
; 8034 kg65.f.rpalette-1 ;24
; 8036 kg65.f.index2color-1 ;25
; 8038 kg65.f.rgraphic  ;26






; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
