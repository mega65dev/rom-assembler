; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      copy.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************

; COPY cdddfn=sdsfn

dcopy           jsr     dospar                          ; parse the line
                and     #$30
                cmp     #$30                            ; check required parameters
                bne     l231_1                          ; branch if single drive copy
                lda     parsts                          ; else check for dual drive params
                and     #$c7
                beq     l231_2
                and     #3                              ; special check for 2nd filename   [910717]
                cmp     #3
                beq     l231_1                          ; branch if given
                lda     #'*'
                sta     dosdid                          ; else supply "*" for him, just like 'name2'
                lda     #1
                ldx     #<dosdid
                ldy     #>dosdid
                sta     dosf2l
                stx     dosf2a
                sty     dosf2a+1
                lda     #2                              ; and set filename2 flag
                tsb     parsts                          ; set flag in status
l231_1          lda     parsts
                jsr     chk4
; lda parsts
l231_2          ldy     #fcopy                          ; tabld offset
                lda     #8                              ; length
                jsr     trans                           ; send command
                +lbra   print_dos_error                 ; if any




; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
