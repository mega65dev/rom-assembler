


;****************************************************************
; Move Sprite.  Position sprite and optionally animate it.
;
;   MOVSPR n, [ p | x#y ]   or   MOVSPR n, p1 TO p2, speed
;
;  n = Sprite number (0-7)
; p = (x,y) coordinate.
;  Relative and angular distances  are relative to
;  current sprite position and scaled if scaling is on.
; x#y = Constant movement at an angle-x with speed-y.
;****************************************************************

movspr          lda #0                                  ; flag 'movspr' initial coord   [910808]
                sta op                                  ; (0=movspr, $80=movspr_to, $7f=mouse)
                jsr get_sprite_number                   ; get sprite #
                jsr sprcor                              ; get first coordinate (y,a)
movspr_1                                                ; entry to eval destination coordinate  [910808]
                bit numcnt                              ; test coordinate type
                +lbvs snerr                             ; syntax error
                sty xdest                               ; save coordinate value
                sty xdest+2
                sta xdest+1
                sta xdest+3

                jsr sprcor                              ; get second coordinate (y,a)
                bit numcnt                              ; test coordinate type & dispatch accordingly
                bvc movspr_normal                       ; normal coordinates
                bmi movspr_angle                        ; angular coordinates

                bit op                                  ; angle#speed, test if allowed
                +lbmi snerr                             ; ng- movspr_to call
                phy                                     ; ok- save speed value
                ldy #xdest-vwork
                jsr getang                              ; get angle of movement
                ldx z_p_temp_1                          ; get sprite number
                ldy sproff,x                            ; get offset to speed data
                lda #0
                sta sprite_data,y                       ; turn off sprite speed
                iny

                ldx #3
l287_1          lsr sinval,x
                dex
                ror sinval,x
                dex
                bpl l287_1

l287_2          sei
                inx                                     ; x=0
                lda angsgn,x                            ; move angle data to speed data
                iny
                sta sprite_data,y
                cpx #4
                bne l287_2

                lda #0                                  ; clear speed angle counts
l287_3          iny
                sta sprite_data,y
                dex
                bne l287_3

                pla                                     ; restore speed value
                and #$3f                                ; limit range (0-63) ????  [910806]
                sta sprite_data-10,y                    ; start sprite movement
                cli
                rts

movspr_angle
; jsr swapxy  ;swap y and a (eventually) : y ==> x
; tay   ;        a ==> y
; txa   ;        x ==> a
                pha
                tya
                ply

                jsr gtang1                              ; get angle values
; ldx #xdest-vwork
; jsr scalxy  ;scale lengths
                ldx #xdest-vwork
                clc

l288_1          jsr angmlt                              ; multiply lengths*angles for x and y
                sta vwork,x
                tya
                sta vwork+1,x
                inx
                inx
                cpx #ydest-vwork
                beq l288_1                              ; loop to do y-position

                ror numcnt                              ; shift in carry to set msb
                bra movspr_position                     ; go place sprite


movspr_normal                                           ; [910122]
                sty xdest+2                             ; save second coordinate (y,a)
                sta xdest+3
; ldx #xdest-vwork
; jsr scalxy  ;scale the coordinates


movspr_position
                sei                                     ; [910123]
                lda z_p_temp_1                          ; get sprite number
                tax                                     ; use as an index
                asl
                tay                                     ; get sprite-number * 2 as another index

                bbr7 op,l289_1
                rts                                     ; >>>exit here if movspr_to call   [910808]

l289_1          lda xdest+2                             ; get y-coordinate
                asl numcnt                              ; test if relative
                bcc l289_3                              ; skip if absolute
                clc
                bpl l289_2                              ; skip if normal coordinates
                eor #$ff
                sec                                     ; invert to subtract if angular
l289_2          adc vic+1,y                             ; add to current sprite y-value  ???vic_save

l289_3          sta vic+1,y                             ; save new sprite y-position  ???vic_save
                lda xdest                               ; get low byte of x-coordinate
                asl numcnt                              ; test if relative
                bpl l289_5                              ; skip if absolute
                clc
                adc vic,y                               ; add current sprite x-position  ???vic_save
                sta vic,y                               ; save sprite x-position   ???vic_save
                bcs l289_4                              ; skip if carry
                inc xdest+1                             ; invert lsb

l289_4          lda vic+16                              ; get x-position msb bits  ???vic_save
                bra l289_6                              ; test if need to invert msb bit

l289_5          sta vic,y                               ; save new sprite x-position  ???vic_save
                lda vic+16                              ; ???vic_save
                ora sbits,x                             ; set x-position msb bit

l289_6          lsr xdest+1                             ; match to lsb of x-coordinate high byte
                bcs l289_7                              ; skip if should be set
                eor sbits,x                             ; reset bit

l289_7          sta vic+16                              ; save position msb bits   ???vic_save
; cli
;1l289_1 rts   ; mouse or movspr_to


movspr_to                                               ; setup for moving sprite to a particular position
;we have already positioned the sprite onscreen
                jsr chrgot                              ; reget terminating character
                cmp #to_token
                beq l290_1                              ; not our call
                cli
                rts

l290_1          smb7 op                                 ; it's for us- let everybody else know we're in charge
                jsr chrget                              ; move to next non-space character
                clc
                jsr sprcor_1                            ; go get & evaluate destination coordinate
                jsr movspr_1                            ; returns with sprite# in .x, VIC sprite index in .y,
;P1 in VIC sprite regs, and P2 in x,ydest
                asl numcnt                              ; Y: handle specific coordinate types
                bcc l290_3                              ; skip if absolute
                clc
                lda xdest+2                             ; get y-coordinate
                bpl l290_2                              ; skip if normal coordinates
                eor #$ff
                sec                                     ; invert to subtract if angular
l290_2          adc vic+1,y                             ; add to current sprite y-value ???vic_save
                sta xdest+2                             ; save sprite destination y-position

l290_3          asl numcnt                              ; X: handle specific coordinate types
                bpl l290_4                              ; skip if absolute
                clc
                lda xdest                               ; get low byte of x-coordinate
                adc vic,y                               ; add current sprite x-position  ???vic_save
                sta xdest                               ; save sprite destination x-position
                bcc l290_4
                inc xdest+1

l290_4          phy
                jsr combyt                              ; get speed parameter
                txa
                and #$3f                                ; limit range (0-63) ????
                ora #$80
                sta xcnt                                ; save in temp.
                ply

                lda vic,y                               ; copy current sprite pos'n to line vars
                sta xpos                                ; in preparation for line calculations
                lda vic+1,y
                sta ypos
                lda #0
                sta xpos+1
                sta ypos+1
                tya
                lsr
                tay
                lda sbits,y
                and vic+16
                beq l290_5
                inc xpos+1
l290_5

;******************************************************************
;  MOVSPR n, p1 TO p2 - move a sprite along line from p1 to p2
;
; The following is performed now:
;
;           absx    = abs(destx-posx) : absy = abs(desty-posy)
;           sgnx    = sgn(destx-posx) : sgny = sgn(desty-posy)
;                     ( sgn=(1,0,-1) if (+,0,-) )
;           greatr  = index to the greatr of absx,absy
;           lesser  = index to the smaller of absx,absy
;
;           fct1    = 2*min(absx,absy)
;           fct2    = fct1 - 2*max(absx,absy)
;           error   = fct1 - max(absx,absy)
;
; The following is performed during IRQ:
;
;           for i:= 1 to max(absx,absy) do begin
;                 movspr n, posx, posy
;                 if error > 0 then begin
;                      pos(lesser):= pos(lesser) + sgn(lesser)
;                      error:= error + fct2
;                      end
;                      else error:= error + fct1
;                 pos(greatr):= pos(greatr) + sgn(greatr)
;           end;
;
; (modification of C128 Bresenham DrawLn algorithm 910808 F.Bowen)
;******************************************************************

movspr_line
                ldx #ypos-vwork
                ldy #ydest-vwork
l291_1          lda #0
                sta xsgn,x                              ; init direction pointers
                sta xsgn+1,x
                jsr abstwo                              ; get absolute value of coordinate differences
                bpl l291_2                              ; and determine direction
                dec xsgn,x                              ; negative direction
                dec xsgn+1,x
                bra l291_4

l291_2          cmp #0
                bne l291_3
                cpy #0
                beq l291_4                              ; zero direction
l291_3          inc xsgn,x                              ; positive direction
l291_4          sta xabs,x
                asl
                sta fct,x                               ; fct(x,y) = 2*abs(x,y)
                tya
                sta xabs+1,x
                rol
                sta fct+1,x
                dex
                dex
                ldy #xdest-vwork                        ; loop to do in x-direction
                cpx #xpos-vwork
                beq l291_1

                ldx #yabs-savram                        ; determine max(xabs,yabs)
                ldy #xabs-savram
                jsr subtwo_savram
                lda #0
                rol
                rol                                     ; a = c * 2
                sta lesser                              ; index to smaller delta
                eor #2
                sta greatr                              ; index to greater delta

                clc
                lda #fct-savram
                adc lesser
                pha
                tay
                eor #2
                tax
                jsr subtwo_savram                       ; fct(greatr) = fct(lesser)-fct(greatr)
                sta savram,x
                sty savram+1,x

                ply                                     ; fct(lesser)
                clc
                lda #xabs-savram
                adc greatr
                tax
                jsr subtwo_savram                       ; error = fct(lesser) - abs(greatr)
                sta errval
                sty errval+1

; At this point, we've positioned the sprite at the start position, and have
; calculated everything we need to move it along a line towards the destination
; position.  All that's left is to copy the working vars into the sprite_data
; tables where the IRQ routine can find & diddle with our data.
;
;    move ang/dist move line
;  offset= 0 b7=0+speed b7=1+speed
;   1 counter  counter lo
;   2 angle sign         hi
;   3,4 delta-X  dir+min/max
;   5,6 delta-Y  fct1
;   7,8 total-X  fct2
;   9,10 total-Y  error

                ldy z_p_temp_1                          ; sprite #
                ldx sproff,y                            ; sprite IRQ table offset

                lda xcnt                                ; set speed factor
                sta sprite_data,x
                ldy greatr
                lda xabs,y                              ; set counter = max(xyabs)
                sta sprite_data+1,x
                lda xabs+1,y
                sta sprite_data+2,x
                lda xsgn,y                              ; set dir(max) and max
                ora xsgn+1,y
                and #3
                lsr
                ror
                ora greatr
                ror
                sta sprite_data+4,x
                ldy lesser
                lda xsgn,y                              ; set dir(min) and min
                ora xsgn+1,y
                and #3
                lsr
                ror
                ora lesser
                ror
                sta sprite_data+3,x
                ldy #0                                  ; set f1, f2, and e
l291_5          lda fct,y
                sta sprite_data+5,x
                inx
                iny
                cpy #6
                bcc l291_5

                cli
                rts                                     ; done!
