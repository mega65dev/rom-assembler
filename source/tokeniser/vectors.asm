; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      vectors.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************


stmdsp
                !word end-1
                !word for-1
                !word next-1
                !word data-1
                !word inputn-1
                !word input-1
                !word dim-1
                !word read-1
                !word let-1
                !word goto-1
                !word run-1
                !word if-1
                !word restor-1
                !word gosub-1
                !word return-1
                !word rem-1
                !word stop-1
                !word ongoto-1
                !word wait-1
                !word load-1
                !word save-1
                !word verify-1
                !word def-1
                !word poke-1
                !word printn-1
                !word print-1
                !word cont-1
                !word list-1
                !word clear-1
                !word cmd-1
                !word sys-1
                !word open-1
                !word close-1
                !word get-1
                !word new-1

                !word else-1
                !word resume-1
                !word trap-1
                !word tron-1
                !word troff-1
                !word sound-1
                !word volume-1
                !word auto-1
                !word puctrl-1
                !word graphic-1

                !word C65__paint-1
                !word C65__char-1
                !word C65__box-1
                !word C65__circle-1
                !word C65__paste-1                      ; gshape
                !word C65__cut-1                        ; sshape
                !word C65__line-1                       ; draw

                !word bad_command-1                     ; escape - SYSTEM - unimplemented command
; .word  locate-1

                !word color-1
                !word scnclr-1

                !word bad_command-1                     ; escape - SYSTEM - unimplemented command
; .word  scale-1

                !word help-1
                !word do-1
                !word loop-1
                !word exit-1
                !word directory-1
                !word dsave-1
                !word dload-1
                !word header-1
                !word scratch-1
                !word collect-1
                !word dcopy-1
                !word rename-1
                !word backup-1
                !word delete-1
                !word renumber-1
                !word key-1
                !word _monitor-1
                !word bank-1                            ; escape
                !word filter-1                          ; escape
                !word play-1                            ; escape
                !word tempo-1                           ; escape

                !word movspr-1                          ; escape
                !word sprite-1                          ; escape
                !word sprcolor-1                        ; escape

                !word rreg-1                            ; escape
                !word envelope-1                        ; escape
                !word sleep-1                           ; escape
                !word directory-1                       ; escape
                !word dopen-1                           ; escape
                !word append-1                          ; escape
                !word dclose-1                          ; escape
                !word bsave-1                           ; escape
                !word bload-1                           ; escape
                !word record-1                          ; escape
                !word concat-1                          ; escape
                !word dverify-1                         ; escape
                !word dclear-1                          ; escape

                !word sprsav-1                          ; escape
                !word collision-1                       ; escape

                !word data-1                            ; escape - BEGIN
                !word data-1                            ; escape - BEND
                !word window-1                          ; escape
                !word boot-1                            ; escape

                !word bad_command-1
; .word  set_width-1 ;escape - WIDTH

                !word bad_command-1
; .word  sprdef-1  ;escape - Sprite Definition mode

                !word bad_command-1                     ; escape - QUIT - unimplemented command
                !word dma-1                             ; escape
                !word 0                                 ; placeholder to skip over the space character
                !word dma-1                             ; escape
                !word 0                                 ; placeholder to skip over the quote character
                !word dma-1                             ; escape
                !word bad_command-1                     ; escape - OFF - unimplemented command
                !word fast-1                            ; escape
                !word slow-1                            ; escape
                !word type-1                            ; escape (C65: type SEQ file)
                !word bverify-1                         ; escape (C65: verify BINary file)
                !word snerr-1                           ; escape (C65: kludge- dirECTORY)
                !word scratch-1                         ; escape (C65: erase alias for scratch)
                !word find-1                            ; escape (C65: find BASIC text)
                !word change-1                          ; escape (C65: change BASIC text)

                !word C65__set-1                        ; escape (C65: multi-purpose command)
                !word Screen-1                          ; escape (C65: SCREEN)
                !word C65__polygon-1                    ; escape (C65: POLYGON)
                !word C65__ellipse-1                    ; escape (C65: ELLIPSE)
                !word C65__Viewport-1                   ; escape (C65: VIEWPORT)
                !word C65__copy-1                       ; escape (C65: GCOPY)
                !word C65__setpen-1                     ; escape (C65: PEN)
                !word C65__setpalette-1                 ; escape (C65: PALETTE)
                !word C65__setdmode-1                   ; escape (C65: DMODE)
                !word C65__setdpat-1                    ; escape (C65: DPAT)
                !word header-1                          ; format alias for header command [911017]
                !word genlock-1                         ; [910108]

stmdsp2
                !word foreground-1                      ; this is the 128th command!  [910109]
                !word 0                                 ; placeholder to skip over the colon character
                !word background-1
                !word border-1
                !word highlight-1
                !word mouse-1                           ; [910122]
                !word rmouse-1                          ; [910123]
                !word disk-1                            ; [910123]
                !word cursor-1                          ; [910228]
                !word rcursor-1                         ; [910228]
                !word loadiff-1                         ; [910402]
                !word saveiff-1                         ; [910930]
                !word edit-1                            ; [910620]


fundsp
                !word sgn
                !word int
                !word abs
                !word usrpok
                !word fre
                !word pos
                !word sqr
                !word rnd
                !word log
                !word exp
                !word cos
                !word sin
                !word tan
                !word atn
                !word peek
                !word len
                !word strd
                !word val
                !word asc
                !word chrd
                !word leftd
                !word rightd
                !word midd
                !word rgraphic                          ; [910701]
                !word rcolor                            ; [910701]
                !word 0                                 ; placeholder for escape function token
                !word joy
                !word rpen                              ; was rdot     [910820]
                !word dcml                              ; dec
                !word hexd
                !word errd
                !word pot                               ; escape
                !word bump                              ; escape
                !word lpen                              ; escape
                !word rsppos                            ; escape
                !word rsprite                           ; escape
                !word rspcolor                          ; escape
                !word xor                               ; escape
                !word rwindow                           ; escape
                !word pointer                           ; escape
                !word mod                               ; escape c65     [910402]
                !word pixel                             ; escape c65     [910820]
                !word rpalette                          ; escape c65     [910820]


optab           !text 121
                !word faddt-1
                !text 121
                !word fsubt-1
                !text 123
                !word fmultt-1
                !text 123
                !word fdivt-1
                !text 127
                !word fpwrt-1
                !text 80
                !word andop-1
                !text 70
                !word orop-1
negtab          !text 125
                !word negop-1
                !text 90
                !word notop-1
ptdorl          !text 100
                !word dorel-1

;.end


; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
