; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      bank.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************


;************************************************************************
;*  Set Memory Bank for PEEK,POKE,WAIT,BLOAD,BSAVE and SYS,BOOT Commands
;*
;*  Syntax: BANK DATA  n  for PEEK,POKE,WAIT,BLOAD,BSAVE
;*  BANK SYS  [a,x,y,z] for SYS,BOOT Commands
;*
;* where   n=  %11111111  to access I/O area (System MAP)
;*      %0xxxxxxx to use physical bank n
;*
;* or      a,x,y,z  describe precise configuration for MAPper
;*    if omitted, the System MAP is to be used.
;*
;* The DATA option is to access data,  i.e., LDA/STA_far
;* The SYS  option is to execute code, i.e., JMP/JSR_far
;*
;*  Idea: BANK SCREEN n  when REC is finalized????
;************************************************************************

bank            jsr     getbyt                          ; get bank number in .x
                stx     current_bank
                rts


;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
