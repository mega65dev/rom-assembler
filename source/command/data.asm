; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      data.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************




data
                jsr     datan                           ; skip to end of statement- offset in .y
addon           tya
                clc
                adc     txtptr                          ; add offset to end to txtptr
                sta     txtptr
                bcc     remrts
                inc     txtptr+1
remrts          rts                                     ; NEWSTT rts addr is still there



rem             jsr     remn                            ; skip rest of statement
                bra     addon                           ; will always branch


datan           ldx     #':'                            ; DATA terminates on ":" and null
                !text $2c

remn            ldx     #0                              ; REM terminates on null only
                stx     charac                          ; preserve terminator
                ldy     #0                              ; this makes charac=0 after swap
                sty     endchr

l43_1           lda     endchr
                ldx     charac
                sta     charac
                stx     endchr
l43_2           jsr     indtxt
                beq     remrts                          ; null always terminates
                cmp     endchr                          ; is it some another terminator?
                beq     remrts                          ; yes, it's finished
                iny                                     ; progress to next character
                cmp     #'"'                            ; is it a quote?
                bne     l43_2                           ; no, just continue
                beq     l43_1                           ; yes, time to change

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
