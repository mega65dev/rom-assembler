; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      header.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************


; HEADER nddn [,id]  (alias: FORMAT)

header          jsr     dospar                          ; parse the line
                jsr     chk1                            ; check parameter errors
                and     #$01
                cmp     #$01
                +lbne   snerr                           ; if required parameters not present

                jsr     _clall                          ; close all files
                jsr     are_you_sure                    ; confirm if in direct mode
                bne     header_rts                      ; exit if 'no' response
                ldy     #fhed                           ; tabld index
                lda     #4                              ; length
                ldx     dosdid                          ; check for diskid
                beq     l226_1
                lda     #6                              ; length with id

l226_1          jsr     trans                           ; build and send command
;fall into 'print_dos_error'


; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
