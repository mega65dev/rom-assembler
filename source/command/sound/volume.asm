; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      volume.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************


;***************************************************************
;*
;*  VOLUME - set volume of SID chips
;*
;* Syntax : VOLUME [right] [,left]
;*
;* Where  : vol in 0..15
;*
;***************************************************************

volume          +lbeq   snerr                           ; stereo parameters    [910612]
                cmp     #','
                beq     l114_1                          ; left volume only
; jsr getbyt  ;right volume in .x
; cpx #16
; bcs fcerr  ;too large
                jsr     getnyb                          ; [910930]
                stx     z_p_temp_1                      ; a temp (sorry fred)

; The way this is done must work with 'PLAY' without too much conflict.
; So, along with setting the SID 'volume' reg, we'll also set up PLAY's
; record of current volume as well.

                lda     filters1+3
                and     #$f0
                ora     z_p_temp_1
                sta     filters1+3

; lda filters1+4  ;???? why     [910612]
; and #$f0
; ora z_p_temp_1
; sta filters1+4

; jsr put_io_in_map
; jsr go_slow  ;      [910716] 4567R7A
                sta     sid1+24
; jsr go_fast  ;      [910716] 4567R7A
                jsr     chrgot
                beq     volrts

l114_1          jsr     optbyt                          ; get optional left parameter   [910612]
                +lbcc   snerr                           ; comma but no value not given??
                jsr     chknyb                          ; [910930]
; cpx #16
; bcs fcerr  ;too large
                stx     z_p_temp_1                      ; a temp (sorry fred)

                lda     filters2+3
                and     #$f0
                ora     z_p_temp_1
                sta     filters2+3

; lda filters2+4  ;???? why     [910612]
; and #$f0
; ora z_p_temp_1
; sta filters2+4

; jsr put_io_in_map
; jsr go_slow  ;      [910716] 4567R7A
                sta     sid2+24
; jsr go_fast  ;      [910716] 4567R7A
                rts

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
