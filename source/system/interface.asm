; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      interface.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************


ready_1
                lda     #%10000000
                jsr     _setmsg                         ; turn Kernel messages on
                lda     #%11000000
                trb     runmod                          ; turn run modes off, leave trace mode on????

ready_2
                bbs4    runmod,l24_1                    ; print appropriate system prompt
                jsr     _primm                          ; Program mode: print 'ready.'
                !text cr,"READY.",cr,0
                bra     main

l24_1           jsr     _primm                          ; Edit mode: print 'ok.'
                !text cr,"OK.",cr,0


main            jmp     (imain)                         ; MAIN INPUT LOOP

nmain           ldx     #$ff                            ; set direct mode flag
                stx     curlin+1
                jsr     InputLine                       ; get a line of input & buffer it


; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
