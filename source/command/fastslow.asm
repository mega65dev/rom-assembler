


;***********************************************************************
;
;    FAST - put the system in FAST (4 MHz?) mode.
;
;***********************************************************************

fast
                 +lbne snerr                              ; no args      [910410]
; jsr put_io_in_map
                 lda #%01000000
                 tsb vic+49                               ; set FAST (4MHz?) mode
                 rts



;***********************************************************************
;
;    SLOW - put the system in SLOW (1 MHz) mode.
;
;***********************************************************************

slow
                 +lbne snerr                              ; no args      [910410]
; jsr put_io_in_map
                 lda #%01000000
                 trb vic+49
                 rts

;.end



; These routines check for certain VALTYP.   (c) is not preserved.



frmnum           jsr frmevl

chknum           clc
                 !text $89

chkstr           sec

chkval
; bbs7 valtyp,docstr ;cannot do this- return status +/-/= needed!
                 bit valtyp
                 bmi docstr
                 bcs chkerr
chkok            rts

docstr           bcs chkok

chkerr           ldx #errtm
                 !text $2c

sterr            ldx #errst
                 +lbra error

;.end
