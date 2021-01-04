


;************************************************************************************
; SPRITE CLR
; SPRITE {LOAD|SAVE} "filename" [,Ddrive] [,Udevice]
; SPRITE sprite [,enable [,color [,priority [,xexp [,yexp [,resolution] ]]]]]
;
; where: sprite  :== sprite number (1-8)
;  enable  :== enable  (0=off, 1=on)
;  color  :== color  (0-15)
;  priority :== sprite/bgnd  (0=sprite, 1=bgnd)
;  xexp  :== expand x direction (0=no, 1=yes)
;  yexp  :== expand y direction (0=no, 1=yes)
;  resolution :== resolution  (0=hires, 1=multicolor)
;************************************************************************************

sprite           cmp #clr_token                           ; SPRITE CLR: init environment   [910717]
                 +lbeq Sprite_CLR                         ; yes
                 cmp #save_token                          ; SPRITE SAVE: save sprite data   [911001]
                 beq Sprite_Save                          ; yes
                 cmp #load_token                          ; SPRITE LOAD: load sprite data   [911001]
                 beq Sprite_Load                          ; yes

                 jsr get_sprite_number                    ; get sprite number in z_p_temp_1
                 jsr optbyt                               ; look for (optional) enable
                 bcc l285_1                               ; none here, don't change
                 ldy #21
                 jsr sprbit                               ; set/clear sprite bit

l285_1           jsr optbyt                               ; get (optional) color
                 bcc l285_2                               ; branch if no arg
                 jsr chknyb                               ; [910109]
                 txa
                 ldx z_p_temp_1                           ; get back sprite number
; jsr put_io_in_map
                 sta vic+39,x

l285_2           jsr optbyt                               ; look for (optional) priority
                 bcc l285_3
                 ldy #27
                 jsr sprbit

l285_3           jsr optbyt                               ; look for (optional) x expansion
                 bcc l285_4
                 ldy #29
                 jsr sprbit

l285_4           jsr optbyt                               ; look for (optional) y expansion
                 bcc l285_5
                 ldy #23
                 jsr sprbit

l285_5           jsr optbyt                               ; look for (optional) resolution
                 bcc l285_6
                 ldy #28
                 jsr sprbit

l285_6           rts


Sprite_Save                                               ; Just like Key_Save     [911001]
                 jsr GetSaveChannel
                 lda #highds                              ; set starting & ending addresses
                 ldy #>sprite_base                        ; start address & pointer to it
                 ldx #<sprite_base
                 sty highds+1
                 stx highds
                 iny                                      ; end address = start address + 512 + 1
                 iny
                 inx
                 +lbra savenb                             ; [910925]



Sprite_Load
                 jsr GetLoadChannel                       ; get a channel      [911001]
                 ldy #>sprite_base
                 lda #<sprite_base
                 jsr LoadBlock                            ; load first block
                 inc highds+1
                 jsr LoadBlockNext                        ; load second block
                 +lbra list_err                           ; release channel, close file, return to main


;  Set or clear a bit in a VIC register
;
; .X = 1 to set, 0 to clear
; .Y = register in VIC to operate opon

sprbit           txa
                 lsr                                      ; put lsb in .C (0 clear, 1 set sprite bit)
                 +lbne fcerr                              ; only 0 or 1, please.
; jsr put_io_in_map
                 ldx z_p_temp_1                           ; get sprite number
                 lda sbits,x
                 ora vic,y
                 bcs l286_1
                 eor sbits,x
l286_1           sta vic,y
                 rts


get_sprite_number
                 jsr getbyt
; dex        [910221]
                 cpx #8
                 +lbcs fcerr
                 stx z_p_temp_1
                 rts

;.end