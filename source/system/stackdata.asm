
                * = $00ff

lofbuf          !fill 1
fbuffr          !fill 16                                ; MathPack builds numbers here, USING, RENUMBER

;  Kernel MAP configurations & DMA lists

                !fill 16+36                             ; (4 configs + 3 DMA lists)

;  BASIC DMA lists  (2 @ 12bytes each = 24 bytes)

dma1_cmd        !fill 1                                 ; This list is used by BASIC OS
dma1_cnt_lo     !fill 1
dma1_cnt_hi     !fill 1
dma1_src_lo     !fill 1
dma1_src_hi     !fill 1
dma1_src_bank   !fill 1
dma1_dest_lo    !fill 1
dma1_dest_hi    !fill 1
dma1_dest_bank  !fill 1
dma1_subcmd     !fill 1                                 ; (from here on not supported until F018A) [910520] F018A
dma1_mod_lo     !fill 1
dma1_mod_hi     !fill 1

dma2_cmd        !fill 1                                 ; This list is used by DMA command & Graphics
dma2_cnt_lo     !fill 1
dma2_cnt_hi     !fill 1
dma2_src_lo     !fill 1
dma2_src_hi     !fill 1
dma2_src_bank   !fill 1
dma2_dest_lo    !fill 1
dma2_dest_hi    !fill 1
dma2_dest_bank  !fill 1
dma2_subcmd     !fill 1                                 ; (from here on not supported until F018A) [910520] F018A
dma2_mod_lo     !fill 1
dma2_mod_hi     !fill 1

                !fill 1                                 ; Kernel's dma_byte

sysstk                                                  ; bottom of system stack
stkend          = $1fb                                  ; top of system stack
