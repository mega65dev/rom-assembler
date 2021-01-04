; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      sprites.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************

Sprite_CLR
                jsr     chkeos                          ; eat CLR token, check eos   [910717] new
Sprite_CLR_1
                php
                sei
                lda     #0
                sta     vic+21                          ; Turn off all sprites
                sta     vic+23                          ; Unexpand them     [910828]
                sta     vic+27                          ; Sprite priority
                sta     vic+28                          ; Hires sprites
                sta     vic+29

                ldx     #init_as_0                      ; Init sprite tables
l316_1          sta     sprite_data,x
                dex
                bpl     l316_1

                lda     #sprite_base/64+7               ; Set up sprite pointers
                ldy     #7
l316_2          bbr7    _mode,l316_3
                sta     sprite_ptrs_40,y                ; 40 col screen
                bra     l316_4
l316_3          sta     sprite_ptrs_80,y                ; 80 col screen
l316_4          dec
                dey
                bpl     l316_2

                plp
; rts

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
