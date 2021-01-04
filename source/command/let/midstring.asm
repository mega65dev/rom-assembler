; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      midstring.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



; Alternate use of the MID$ function, as the target of an assignment.
;
; MID$(string_var,starting_position [,length]) = string_expression

midd2
midwrk          =midd2-1

                jsr     chkopn                          ; check for '('
                jsr     ptrget                          ; get pointer to descriptor of string-var
                sta     forpnt                          ; store for later use
                sty     forpnt+1
                jsr     chkstr                          ; check if string

                jsr     combyt                          ; look for comma, followed by 1 byte starting address
                dex                                     ; adjust starting addr
                stx     hulp                            ; store    " "

                cmp     #')'                            ; finished?
                beq     l66_1                           ; branch if so (use default length)
                jsr     combyt                          ; ..else get length
                !text $2c

l66_1           ldx     #$ff                            ; default length
                stx     z_p_temp_1
                jsr     chkcls                          ; look for ')'
                lda     #equal_token                    ; look for '='
                jsr     synchr
                jsr     frmevl                          ; bring on the source!
                jsr     chkstr                          ; nothing funny

                ldy     #2                              ; get string descriptors
l66_2           lda     #forpnt                         ; target
                jsr     lda_far_ram1                    ; lda (forpnt),y
                sta     str1,y
                jsr     indfmo                          ; source
                sta     str2,y
                dey
                bpl     l66_2

; Test for target string in text was removed-  all strings are copied to
; string RAM when they are created.

                sec                                     ; adjust pointer to source string so that the same
                lda     str2+1                          ; ..index can load & save
                sbc     hulp
                sta     str2+1
                bcs     l66_3
                dec     str2+2

l66_3           lda     z_p_temp_1                      ; get specified length (or default)
                cmp     str2                            ; compare with length of source
                bcc     l66_4                           ; ok if less,
                lda     str2                            ; ..else use length of source
l66_4           tax
                beq     l66_7                           ; done if length=0
                clc
                adc     hulp                            ; add length to starting posn.
                +lbcs   fcerr                           ; illegal quantity error if > 256
                cmp     str1
                bcc     l66_5
                +lbne   fcerr                           ; ...or if > target length

l66_5           ldy     hulp                            ; get adjusted starting address
l66_6           phx
                ldx     #str1+1
                lda     #str2+1
                jsr     lda_far_ram1                    ; fetch from string bank
                jsr     sta_far_ram1                    ; this is what it's all about
                iny
                plx
                dex
                bne     l66_6                           ; keep going for specified length

l66_7           +lbra   frefac                          ; free up temp. string, rts

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
