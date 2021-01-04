
; Test: IF new line is longer than the line it replaces,
;  THEN IF there isn't enough room in memory to add this new line,
;   THEN out-of-memory error
;
; Before this fix, the old line was deleted BEFORE testing if the new line fit.
;
; N.B.: I am assuming that lines cannot be greater than 255 chars, as is the
; case where the line was entered "normally", that is, using LINGET.  The only
; consequence of this assumption is that lines > 255 will fall prey to the
; pre-fix problem mentioned above.

                 ldy #0
                 jsr indlow                               ; get lsb of the next line's starting address
                 sec
                 sbc lowtr                                ; subtract lsb of this line's starting address
                 sec                                      ; ignore borrow (gives abs. value)
                 sbc #4                                   ; allow for link & line number
                 sbc count                                ; compare with new length
                 bcs l25_5                                ; new line is shorter, no problem
                 neg                                      ; convert to positive delta

                 ldy text_top+1                           ; get msb of end of text (.c=0)
                 adc text_top                             ; add our calculated delta to end of text
                 bcc l25_4
                 iny
l25_4            cpy max_mem_0+1
                 bcc l25_5                                ; result is less than top-of-memory: ok
                 +lbne omerr                              ; msb >  top, overflow
                 cmp max_mem_0                            ; msb's the same, test lsb's
                 +lbcs omerr                              ; lsb >= top, overflow

; Using DMA device to move text downwards (to delete or replace a line)...
;
; lowtr     = destination
; (lowtr)    = pointer to source (via link bytes of line to be removed)
; text_top-(lowtr) = number of bytes to move (text_top points to old top of text)
; new text_top     = text_top -( (lowtr)-lowtr )

l25_5            lda lowtr                                ; set up DMA destination
                 sta dma1_dest_lo
                 lda lowtr+1
                 sta dma1_dest_hi
                 ldy #0
                 jsr indlow                               ; set up DMA source (& delta)
                 sta dma1_src_lo
                 sec
                 sbc lowtr
                 sta index1                               ; (delta lo)
                 iny
                 jsr indlow
                 sta dma1_src_hi
                 sbc lowtr+1
                 sta index1+1                             ; (delta hi)
                 sec
                 lda text_top                             ; set up DMA count
                 sbc dma1_src_lo
                 sta dma1_cnt_lo
                 lda text_top+1
                 sbc dma1_src_hi
                 sta dma1_cnt_hi

                 lda text_bank
; and #%00001111  ;      [910102]
; and #%01111111  ;      [910520] F018A
                 sta dma1_src_bank
                 sta dma1_dest_bank

                 lda #0
                 sta dma1_cmd                             ; dma command (copy, source=start)
                 sta dma1_subcmd                          ; [910520] F018A
                 sta dma_ctlr+2                           ; dma_list bank

                 ldx #>dma1_cmd                           ; dma_list
                 lda #<dma1_cmd
                 stx dma_ctlr+1                           ; dma_list hi
                 sta dma_ctlr                             ; dma_list lo & trigger

                 sec                                      ; calculate & set new text_top
                 lda text_top
                 sbc index1
                 sta text_top                             ; lo
                 lda text_top+1
                 sbc index1+1
                 sta text_top+1                           ; hi
;fall into routine to insert new line (if any)


nodel            jsr init_stack                           ; 'clearc' removed since text changes don't require trashing variables
                 jsr link_program                         ; fix links
                 jsr error_clear                          ; clear HELP/error flag, assuming he fixed whatever caused current error, if any

                 ldy #0
                 lda (txtptr),y                           ; delete line? ("common")
                 +lbeq main                               ; yes

l26_1            clc                                      ; no...something to insert
                 ldy text_top+1
                 lda text_top
                 sty hightr+1                             ; top of block to move (old text_top)
                 sta hightr
                 adc count                                ; number of characters in line to be inserted
                 bcc l26_2
                 iny
l26_2            clc
                 adc #4                                   ; plus link and line #
                 bcc l26_3                                ; gives us destination of move (new text_top)
                 iny

l26_3            sta highds                               ; destination of top
                 sty highds+1
                 cpy max_mem_0+1                          ; make sure new top doesn't crash into top of available ram
                 bcc l26_4                                ; ok
                 +lbne omerr                              ; out of memory, don't insert
                 cmp max_mem_0
                 +lbcs omerr                              ; out of memory, don't insert

l26_4            sta text_top                             ; set new top of text
                 sty text_top+1
                 sec                                      ; compute number of things to move up
                 lda hightr
                 sbc lowtr                                ; (old top) - (adr where new line goes)
                 tay                                      ; lowtr was setup previously by FindLine call
                 lda hightr+1
                 sbc lowtr+1
                 tax

; Using DMA device to copy data upwards...
;
; (hightr)   = source  (old top)
; (highds)   = destination (new top)
; .y, .x     = number of bytes to move
; (lowtr)    = where to insert new line (starting with link bytes)

                 dew hightr                               ; (text_top-1) points to actual last byte
                 dew highds

; lda dma_ctlr+3  ;dma controller version    [910520] F018A
; and #1
; beq l26_5   ; F018    removed [910808] F018B
                 lda #%00110000                           ; F018A, B
l26_5            sta dma1_cmd                             ; command=copy, source=endpt   [910102]
                 sty dma1_cnt_lo                          ; count
                 stx dma1_cnt_hi
                 tya
                 ora dma1_cnt_hi
                 beq l26_7                                ; special case= nothing to move???? should not happen

                 lda hightr
                 ldy hightr+1
                 sta dma1_src_lo                          ; source
                 sty dma1_src_hi
                 lda highds
                 ldy highds+1
                 sta dma1_dest_lo                         ; destination
                 sty dma1_dest_hi
                 lda text_bank                            ; [910520] F018A
; ldx dma1_cmd  ;version?    removed [910808] F018B
; bne l26_6   ; F018A
; and #%00001111  ;      [910102]
; ora #%01000000  ;(copy source=endpoint)    [910102]
l26_6            sta dma1_src_bank                        ; banks
                 sta dma1_dest_bank
                 lda #0
                 sta dma1_subcmd                          ; [910520] F018A
                 sta dma_ctlr+2                           ; dma_list bank
                 ldx #>dma1_cmd                           ; dma_list
                 lda #<dma1_cmd
                 stx dma_ctlr+1                           ; dma_list hi
                 sta dma_ctlr                             ; dma_list lo & trigger

; Make links non-null to fool 'chead'

l26_7            ldy #0
                 lda #1
                 ldx #lowtr
                 jsr sta_far_ram0                         ; sta (lowtr),y  y=0 (bleed-thru)
                 iny
                 jsr sta_far_ram0                         ; sta (lowtr),y  y=1 (bleed-thru)

; Put line number in text

                 iny
                 lda linnum
                 jsr sta_far_ram0                         ; sta (lowtr),y  y=2 (bleed-thru)
                 iny
                 lda linnum+1
                 jsr sta_far_ram0                         ; sta (lowtr),y  y=3 (bleed-thru)

; Advance 'lowtr' to start of line (past link bytes & line #)

                 clc
                 lda lowtr
                 adc #4
                 sta lowtr
                 bcc l26_8
                 inc lowtr+1


; Block move line to text

l26_8            ldy count                                ; use dma ???? [910925]
                 dey

l26_9            lda (txtptr),y                           ; (from common area)
                 jsr sta_far_ram0                         ; sta (lowtr),y   (bleed-thru)
                 dey
                 cpy #$ff
                 bne l26_9

; beq l26_9   ;special case= nothing to move???? should not happen
; lda #0   ; F018A, B
; sta dma1_cmd  ;command=copy, source=start
; sty dma1_cnt_lo  ;count
; sta dma1_cnt_hi
;
; lda txtptr
; ldy txtptr+1
; sta dma1_src_lo  ;source
; sty dma1_src_hi
; lda lowtr
; ldy lowtr+1
; sta dma1_dest_lo ;destination
; sty dma1_dest_hi
; lda text_bank  ;banks
; sta dma1_dest_bank
; lda #sys_bank  ;????
; sta dma1_src_bank
; sta dma1_subcmd  ;      [910520] F018A
; sta dma_ctlr+2  ;dma_list bank
; ldx #>dma1_cmd  ;dma_list
; lda #<dma1_cmd
; stx dma_ctlr+1  ;dma_list hi
; sta dma_ctlr  ;dma_list lo & trigger
;l26_9
                 jsr link_program
                 jsr reset_txtptr                         ; set up txtptr (was jsr runc)

; Test if AUTO in effect

                 lda autinc                               ; if in auto mode, increment val <> 0
                 ora autinc+1
                 beq l26_12                               ; not in

                 lda linnum                               ; yes, construct new line number
                 clc
                 adc autinc
                 sta facho+1
                 lda linnum+1
                 adc autinc+1
                 bcs l26_12                               ; no auto if wrapped
                 cmp #$fa                                 ; test if # >= 64000
                 bcs l26_12                               ; no auto if so.
                 sta facho
                 ldx #$90
                 sec
                 jsr floatc                               ; float it
                 jsr fout                                 ; make it into a string

                 sei                                      ; [910710]
                 ldx #0                                   ; move string into kbd buffer
l26_10           lda fbuffr+1,x                           ; copy number formed into buffer, ignoring leading space
                 beq l26_11                               ; a null marks end
                 sta _keyd,x
                 inx
                 bne l26_10                               ; always

l26_11           lda #29                                  ; cursor right
                 sta _keyd,x
                 inx
                 stx _ndx
                 cli                                      ; [910710]

l26_12           +lbra main
