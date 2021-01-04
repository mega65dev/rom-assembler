

;*****************************************************************
;* DPAT   set draw pattern
;*
;*  Syntax : DPAT  type [, # bytes, byte1, byte2, byte3, byte4]
;*
;*           parm1 = type        0-63  <<< only 0-4 implemented [911003]
;*           parm2 = # bytes     1-4
;*           parm3 = byte1       0-255
;*           parm4 = byte2       0-255
;*           parm5 = byte3       0-255
;*           parm6 = byte4       0-255
;*****************************************************************

C65__setdpat
                jsr getbyt                              ; get pattern type
                cpx #4+1                                ; 63+1       [911028]
l270_1          +lbcs fcerr                             ; if out of range
                stx GKI__parm1
                txa
                bne l270_2                              ; if parm1 is 0 then get extra stuff

                jsr combyt                              ; get number of bytes
                cpx #5
                bcs l270_1                              ; too many bytes
                stx GKI__parm2
                stx z_p_temp_1                          ; save for count

                jsr combyt                              ; get byte 1
                stx GKI__parm3
                dec z_p_temp_1
                beq l270_2
                +lbmi fcerr                             ; too few bytes

                jsr combyt                              ; get byte 2
                stx GKI__parm4
                dec z_p_temp_1
                beq l270_2

                jsr combyt                              ; get byte 3
                stx GKI__parm5
                dec z_p_temp_1
                beq l270_2

                jsr combyt                              ; get byte 4
                stx GKI__parm6

l270_2          jmp ($8016)                             ; bra setdpat
