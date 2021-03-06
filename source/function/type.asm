; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      type.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************

; TYPE  types a given disk (SEQ) file to output channel
;

type            ldz     #0

open_SEQ_file
                phz                                     ; save EDIT load flag    [910620]
                lda     #$e6                            ; parse:  filename [,U#] [,D#]
                jsr     dosprs                          ; (like dopen:      0 0 0 *  * 0 0 1 )
                jsr     chk1                            ; check parameters
                jsr     find_la                         ; find an available LA
                jsr     find_sa                         ; find an available SA
                ldy     #fopnseq
                ldx     #6
                jsr     open_file                       ; open the file
                +lbcs   list_err                        ; exit if error
                plz                                     ; [910620]
                beq     l215_1
                rts                                     ; or exit if called by EDIT load routine

l215_1          jsr     _stop                           ; check stop key
                beq     l215_6                          ; exit if down
                ldx     dosla
                jsr     _chkin                          ; get input channel
                bcs     l215_6                          ; exit if bad??
                ldx     #0
l215_2          cpx     #255                            ; check buffer (buflen????)
; bcs 99$   ; 'too long' error
                beq     l215_3                          ; allow long lines   [910620]
                jsr     _basin                          ; read file data
                sta     dosstr,x                        ; buffer it
                inx                                     ; bump buffer pointer
                tay                                     ; save char
                jsr     _readst                         ; check channel status
                bne     l215_3                          ; exit if eof or error
                cpy     #cr
                bne     l215_2                          ; loop until eol

l215_3          php                                     ; save input channel status (beq=eol, bne=eof/err)
                stx     t4                              ; save character count
                jsr     dcato                           ; get output channel
                ldx     #0
l215_4          cpx     t4                              ; check buffer
                bcs     l215_5                          ; end of buffered data
                lda     dosstr,x                        ; output data
                jsr     _bsout
                inx                                     ; bump buffer pointer
                bne     l215_4                          ; loop until end of buffer

l215_5          jsr     _clrch
                plp                                     ; check input status
                beq     l215_1                          ; loop until eof or bad status

l215_6          +lbra   list_exit                       ; release channel, close file, return to main

;99$ jsr _clrch  ;non-I/O trouble   removed [910620]
; lda dosla  ; shut down disk & report BASIC error
; clc
; jsr _close
; bra errlen  ;buffer overflow: report 'string too long'


; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
