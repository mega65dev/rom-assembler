; RECORD- relative record access

record          lda #'#'
                jsr synchr                              ; syntax error if not 'record#'

                jsr getbyt                              ; get lfn in x
                cpx #0
                +lbeq fcerr                             ; cannot be zero
                stx dosla                               ; save logical address

                jsr comwrd                              ; check for comma, get record number in 'poker'

                ldx #1                                  ; set up to get starting byte # - default is 1
                jsr optbyt
                stx dosrcl                              ; save byte position (pos)    [911024]
                txa                                     ; cpx #0
                +lbeq fcerr                             ; if out of range
                inx                                     ; cpx #$ff
                +lbeq fcerr                             ; if out of range

                lda dosla                               ; get logical address
; jsr put_io_in_map
                jsr _lkupla                             ; logical to physical map
                bcs l229_1                              ; if file not found (not open)    [910404]
                sty dossa_temp                          ; save secondary address

                stx dosfa                               ; set up device number for trans routine
                lda #0
                sta dosla                               ; set up logical address for trans routine
                lda #$6f
                sta dossa                               ; and secondary address, too!

                ldy #frec                               ; set pointer
                lda #4                                  ; process five bytes
                jsr trans                               ; send command
                +lbra print_dos_error                   ; if any

l229_1          ldx #errfno                             ; file not found err (file not open)   [910404]
                +lbra error
