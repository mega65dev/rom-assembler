

; Print Using - Formatted print routine
;
; Entered by cmd, print, or print#
; Syntax:  PRINT USING"****";a;b;c

pudefs          !text " ,.$"                            ; default:  fill, comma, dec pnt, money symbol

using           ldx #$ff
                stx endfd
                jsr chrget
                jsr frmevl                              ; evaluate format string
                jsr chkstr                              ; must be string type...

                lda facmo                               ; save temp descriptor
                pha
                lda facmo+1
                pha

                ldy #2                                  ; move (facmo),1&2 to form,form+1
l198_1          jsr indfmo
                dey
                sta form,y
                bne l198_1

                jsr indfmo                              ; get length
                sta lfor
                tay
                beq l198_3                              ; syntax error if length is zero

l198_2          dey
                jsr indfrm
                cmp #'#'                                ; at least one # in format?
                beq l198_4                              ; yes...
                tya                                     ; no...end of format
                bne l198_2                              ; no...
l198_3          +lbra snerr                             ; yes...syntax error


l198_4          lda #';'                                ; '
eex2            jsr synchr                              ; check character
                sty z_p_temp_1                          ; clear flag for anaf
                sty bnr                                 ; set pointer to begin of no
                jsr frmevl                              ; evaluate expression
                bbr7 valtyp,conv                        ; branch if numeric

                jsr ini                                 ; init counters and flags
                jsr anaf                                ; analyze format
                ldx chsn                                ; > or = in format field
                beq prcha                               ; branch if not
                ldx #0
                sec
                lda cform
                sbc hulp                                ; .a=room left in field
                bcc prcha                               ; branch if no room left
                ldx #'='
                cpx chsn                                ; = in field
                bne l199_1                              ; branch if not
                lsr                                     ; .a=.a/2
                adc #0                                  ; add 1 if odd

l199_1          tax                                     ; store no of blanks in x
prcha           ldy #0
chx             txa
                beq cpef                                ; branch if no blanks
                dex

oblk            lda #' '                                ; output a blank
                bra outc                                ; always


cpef            cpy hulp                                ; end of string reached?
                bcs oblk                                ; output blank if yes
                jsr indin1_ram1                         ; lda (index),y
                iny

outc            jsr cdout                               ; output character
                bne chx                                 ; branch if not ready
                bra reay



conv            jsr fout                                ; convert mfp to decimal

                ldy #$ff                                ; build descriptor for fout string
l200_1          iny                                     ; how big IS it?
                lda fbuffr,y
                bne l200_1
                tya
                jsr strspa                              ; jsr getspa,stx dsctmp+1,sty dsctmp+2,sta dsctmp,rts

                phx
                ldy #0
                ldx #dsctmp+1
l200_2          lda fbuffr,y
                beq l200_3
                jsr sta_far_ram1                        ; sta (dsctmp+1),y
                iny
                bne l200_2

l200_3          plx
                jsr putnew
                jsr ini                                 ; init counters and flags
                jsr fform                               ; output one formatted number

reay            jsr chrgot                              ; get old character
                cmp #','                                ; comma?
                beq eex2                                ; continue print use if yes
                sec
                ror z_p_temp_1                          ; set flag for anaf
                jsr anaf                                ; print rest of format
                ply                                     ; restore descriptor
                pla
                jsr fretmp
                jsr chrgot
                cmp #';'                                ; semi-colon?
                +lbne crdo                              ; end of print using
                jmp chrget                              ; branch if yes
