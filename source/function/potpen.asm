; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      potpen.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************


;***********************************************************
; POT(n)  --  Read paddles
;
;    n = 1 : paddle-1 - X-position
;  2 : paddle-1 - Y-position
;  3 : paddle-2 - X-position
;  4 : paddle-2 - Y-position
;
;     result >= 256 --  trigger set
;***********************************************************

pot             jsr     chkcls                          ; look for closing paren
                jsr     conint                          ; get 1-byte arg in .x
                dex
                cpx     #4
                +lbcs   fcerr                           ; value error

; jsr put_io_in_map
                txa                                     ; convert arg (0-3) into paddle enables
                lsr                                     ; .c= X/Y   .a= port 1/2
                tax
                lda     sbits+6,x
                tax                                     ; (CIA paddle port, $40/$80)
                lda     #0
                rol
                tay                                     ; (SID x/y offset,  $00/$01)

                stx     pot_temp_1                      ; save which port
                php                                     ; save IRQ enable while we
                sei                                     ; disable IRQ to inhibit keyboard scan
                lda     d1pra
                pha                                     ; save kybd output lines
                stx     d1pra                           ; turn on correct paddle

                jsr     go_slow
                ldx     #0
l145_1          inx                                     ; delay to let pot be read by SID
                bne     l145_1

l145_2          lda     sid1+25,y                       ; read pot
                cmp     sid1+25,y                       ; debounce
                bne     l145_2
                sta     pot_temp_2                      ; save pot value
                jsr     go_fast

                ldx     #0                              ; set index to d1pra
                bit     pot_temp_1                      ; test if pot-0,1 or pot-2,3
                bmi     l145_3                          ; skip if pot 2,3
                inx                                     ; index to d1prb
l145_3          lda     #04                             ; use joy line-2
                dey                                     ; test if pot-x or pot-y
                bmi     l145_4                          ; skip if pot-x
                asl                                     ; use joy line-3
l145_4          ldy     #$ff
                sty     d1pra                           ; disable keybd inputs
                iny                                     ; set to zero for no trigger
                and     d1pra,x                         ; test if trigger set
                bne     l145_5                          ; skip if not trigger
                iny                                     ; return value >255 for trigger
l145_5          pla
                sta     d1pra                           ; restore keybd lines
                tya
                ldy     pot_temp_2                      ; restore pot value
                plp                                     ; restore status
                +lbra   nosflt                          ; output 2-byte result


;*************************************************************
;  LPEN(n)  --  Read light pen
;
; n = 0 x position
;     1 y position
;*************************************************************

lpen            jsr     chkcls                          ; look for closing parens
                jsr     conint                          ; get 1 byte arg in .X
; dex   ;convert [1-2] to [0-1]
                cpx     #2
                +lbcs   fcerr                           ; bad value

                lda     #0
                sei
                ldy     lightpen_xpos,x                 ; get latched light pen value (a=msb, y=lsb)
                sta     lightpen_xpos,x                 ; reset to zero (????preserve last latched position)
                cli
                cpx     #0
                bne     l146_1                          ; done if y position
                tya
                asl                                     ; else multiply *2 to get correct x position
                tay                                     ; lsb
                lda     #0
                rol                                     ; msb
l146_1          +lbra   nosflt                          ; float it (y,a)


;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
