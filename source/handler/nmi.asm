


basic_nmi                                                 ; removed [910826]
; lda nmi_wrap_flag ;filter out wrapped NMI calls   [910523] audio
; beq 1$   ; it's ok
; rts   ; exit- we're already handling one interrupt
;
;1$ inc nmi_wrap_flag ;shut the door to NMI
;
;basic_nmi_end
; dec nmi_wrap_flag ;open the door to NMI
                 rts




;.end