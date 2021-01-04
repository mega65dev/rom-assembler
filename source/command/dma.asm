


; DMA - Set up for DMA operation (FETCH/STASH/SWAP)
;
;  Syntax:  DMA  command,length,source(l/h/b),destination(l/h/b)[,subcmd,mod(l/h)] [,...]


dma                                                     ; params are not longer optional-  [910520] F018A
                jsr getbyt                              ; get command
l64_1           bcc l64_2
                txa                                     ; [910102]
                and #%00000100                          ;
                +lbne fcerr                             ; (disallow chained DMA lists)
                stx dma2_cmd

l64_2           jsr comwrd                              ; get length
; bcc l64_3
                sty dma2_cnt_lo
                sta dma2_cnt_hi

l64_3           jsr comwrd                              ; get source address & bank
; bcc l64_4
                sty dma2_src_lo
                sta dma2_src_hi
l64_4           jsr combyt
; bcc l64_5
                stx dma2_src_bank

l64_5           jsr comwrd                              ; get destination address & bank
; bcc l64_6
                sty dma2_dest_lo
                sta dma2_dest_hi
l64_6           jsr combyt
; bcc l64_7
                stx dma2_dest_bank

l64_7           jsr optzer                              ; get subcmd, default=0    [910520] F018A
; bcc l64_8
                stx dma2_subcmd

l64_8           jsr optzer                              ; get mod lo/hi, default=0   [910102]
; bcc l64_9
                stx dma2_mod_lo
l64_9           jsr optzer
; bcc l64_10
                stx dma2_mod_hi

l64_10          ldy #0                                  ; dma_list (bank 0)
                ldx #>dma2_cmd
                lda #<dma2_cmd
                sty dma_ctlr+2                          ; dma_list bank
                stx dma_ctlr+1                          ; dma_list hi
                sta dma_ctlr                            ; dma_list lo & trigger
l64_11          bit dma_ctlr+3                          ; check status (in case IRQ enabled)  [910103]
                bmi l64_11                              ; busy

                jsr chrgot                              ; eol?
                beq l64_12                              ; yes
                jsr optbyt                              ; no- continue after getting comma & next cmd byte
                bra l64_1

l64_12          rts

;.end