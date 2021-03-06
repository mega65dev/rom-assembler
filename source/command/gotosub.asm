; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      gotosub.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



; GOSUB-  Push text pointer, line #, & gosub token on stack:
;
;  (bottom) highest memory
;===========================================================
;  txtptr+1 address of next statement
;  txtptr
;  ========
;  curlin+1 current line number
;  curlin
;  ========
;  'gosub' token <== (tos) top of stack pointer
;===========================================================
;  (top of stack) lowest memory


gosub           bbs4    runmod,edit_err                 ; [910620]
                jsr     gosub_sub
                jsr     chrgot                          ; get character and set carry for linget
                jsr     goto
                +lbra   newstt


goto            bbs4    runmod,edit_err                 ; [910620]
                jsr     linget                          ; pick up the line number in LINNUM
                lda     endchr                          ; test if linget found any number
                +lbeq   snerr                           ; no number error

goto_1          jsr     remn                            ; jump to end of line (entry for interrupt code)
                sec
                lda     curlin
                sbc     linnum
                lda     curlin+1
                sbc     linnum+1
                bcs     luk4it
                tya
                sec
                adc     txtptr
                ldx     txtptr+1
                bcc     lukall
                inx
                bra     lukall                          ; always goes


luk4it          lda     txttab
                ldx     txttab+1

lukall          jsr     FindLink                        ; (a,x) are all set up
                +lbcc   userr                           ; undefined statement error
                lda     lowtr
                sbc     #1
                sta     txtptr
                lda     lowtr+1
                sbc     #0
                sta     txtptr+1
                bbr7    runmod,setexc                   ; branch if in direct mode
                rts



gosub_sub
                lda     #lengos                         ; free up necessary space on stack
                jsr     getstk                          ; make sure there is room
                ldy     #lengos-1
                lda     txtptr+1                        ; push on the text pointer
                sta     (tos),y                         ; (common area)
                dey
                lda     txtptr
                sta     (tos),y                         ; (common area)
                dey
                lda     curlin+1                        ; push on the current line number
                sta     (tos),y                         ; (common area)
                dey
                lda     curlin
                sta     (tos),y                         ; (common area)
                dey
                lda     #gosub_token                    ; (a) was smashed by GETSTK
                sta     (tos),y                         ; (common area)
                rts


edit_err
                ldx     #edit_mode_error                ; [910620]
                +lbra   error

;.end



go_without_to
                jsr     chrget                          ; what is next character?
                cmp     #to_token                       ; ..is it GO TO?
                bne     l71_1
                jsr     chrget                          ; ..yes, set up for goto
                bra     goto                            ; ..bye!

l71_1           jsr     getbyt                          ; is it GO 64?
                cpx     #64
                +lbne   snerr                           ; ...no, error

; The user wants to go to C64 mode.

l71_2           jsr     are_you_sure
                bne     cont_rts                        ; must have had second thoughts. never mind
; jsr put_io_in_map
                jmp     _go_64


;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
