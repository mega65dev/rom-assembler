


;*******************************************************************************
;*
;* SLEEP Command - Postpone all activity for a specified number of seconds
;*
;* Syntax:  SLEEP n
;*
;* Where n is the number of seconds to remain inactive,
;* expressed as a positive value < 65536.
;*
;*******************************************************************************

sleep           jsr getwrd                              ; get argument in (y,a)

; Multiply # of seconds to sleep by 60.  This will be the number of 'jiffies'
; to hibernate.  Store this value in 3 consecutive locations the kernel will
; decrement as a 24-bit binary value, and wait for an underflow.
;
; ldx #0   ;THIS CODE REPLACED    [910730]
; php
; sei   ;silence, please!
; sty _sleep_counter
; sta _sleep_counter+1
; stx _sleep_counter+2 ;sleep_counter = n
;
; jsr sleep_times_2 ;sleep_counter = 2n
; jsr add_xay_to_sleep ;sleep_counter = 3n
; jsr sleep_times_4 ;sleep_counter = 12n
;
; ldy _sleep_counter
; lda _sleep_counter+1
; ldx _sleep_counter+2 ;(xay) = 12n
;
; jsr sleep_times_4 ;sleep_counter = 48n
; jsr add_xay_to_sleep ;sleep_counter = 60n !!!!!
;
; plp
;
;1$ jsr is_stop_key_down
; ldx _sleep_counter+2
; inx   ;underflow?
; bne 1$   ;no, loop
; rts
;
;
;sleep_times_4
; jsr sleep_times_2
;sleep_times_2
; asl _sleep_counter
; rol _sleep_counter+1
; rol _sleep_counter+2
; rts
;
;add_xay_to_sleep
; pha
; tya
; adc _sleep_counter
; sta _sleep_counter
; pla
; adc _sleep_counter+1
; sta _sleep_counter+1
; txa
; adc _sleep_counter+2
; sta _sleep_counter+2
; rts


; SLEEP is now based upon the system hardware TOD clock (same one used by TI$).  This
; makes it accurate, something it was not when it was based upon the frame rate.

                sty time                                ; Number of seconds to "sleep"   [910730] new
                sta time+1

l136_1          jsr _ReadTime                           ; Get current time
                stz time+2                              ; tenths
                sta time+3                              ; seconds

l136_2          jsr is_stop_key_down                    ; Allow user to abort
                jsr _ReadTime                           ; Wait for seconds to increment
                cmp time+3
                beq l136_2
                sta time+3

l136_3          jsr _ReadTime                           ; Wait for tenths to increment
                cpz time+2
                bne l136_3

                dew time                                ; Decrement sleep period 1 second
                bne l136_2                              ; Loop until sleep period over

                rts

;.end