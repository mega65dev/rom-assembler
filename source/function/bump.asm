


;******************************************************************
;* BUMP - read sprite collision
;*
;* Syntax : BUMP (argument)
;*
;* Where  : argument = [1..2]
;*   1 : sprite/sprite collision
;*   2 : sprite/background collision
;******************************************************************

bump            jsr chkcls
                jsr conint                              ; get arg in .X
                dex                                     ; adjust [1..2] to [0..1]
                cpx #2
                +lbcs fcerr                             ; value error

                sei
                ldy collisions,x                        ; get recorded collisions
                lda #0                                  ; reset them
                sta collisions,x
                cli
                +lbra sngflt                            ; float 1 byte arg in .Y

;.end



; GRAPHIC3.SRC
;****************************************************************
;  getang  -  set cosine & sine values
;             results in sinval & cosval based as a fraction
;             - over 65536
;             angsgn = angle phase (0-3)
;    on input vwork+y = 2 byte angle
;***************************************************************

getang
                jsr settwo                              ; move angle value into y/a

gtang1          ldx #0                                  ; init count of phase

l305_1          inx
                sec
                sbc #90                                 ; subtract 90 until less than 0
                bcs l305_1
                dey
                bpl l305_1
                stx angsgn                              ; save phase (here it is 1-4)
                pha
                adc #90                                 ; make positive
                jsr l305_2                              ; do division by 10
                pla                                     ; get 2's comp of angle
                clc
                eor #$ff
                adc #1                                  ; make positive
                dec angsgn                              ; correct phase

l305_2          ldx #$ff
l305_3          inx                                     ; do division by 10
                sec
                sbc #10
                bcs l305_3
                adc #10                                 ; make positive
                sta vtemp1                              ; save remainder
                txa
                asl                                     ; get quotient*2 as index
                tax
                lda angval+1,x                          ; get low byte base
                ldy angval,x                            ; get high byte value

l305_4          clc
                dec vtemp1
                bmi l305_5                              ; done - remainder = 0
                adc incval+1,x                          ; add low byte increment
                pha
                tya
                adc incval,x                            ; add high byte increment
                tay
                pla
                bcc l305_4                              ; ...always

l305_5          pha                                     ; save low byte of result
                ldx #0                                  ; point to sinval
                lda angsgn
                lsr
                bcs l305_6                              ; skip if sine value
                ldx #2                                  ; point to cosval

l305_6          pla
                sta sinval,x                            ; save low byte result
                tya
                sta sinval+1,x                          ; save high byte result
                rts


;*************************************************************
;  angmlt  -  multiple 2-byte integer times angle
;       carry set/reset = cosine/sine
;
;       vwork+x = 2-byte integer
;       result left in y/a
;*************************************************************

angmlt
                ldy #sinval-vwork                       ; get offset to angle value
                bcc l306_1                              ; get cosine/sine offset
                ldy #cosval-vwork

l306_1          lda angsgn
                adc #2                                  ; correct phase for cosine to look as sine
                lsr
                lsr
                php                                     ; save if carry - means negative angle value
                jsr settwo                              ; get angle fraction in y/a
                cpy #$ff                                ; test if value should be 1
                bcc l306_2                              ; skip if not
                txa
                tay                                     ; get offset to integer
                jsr settwo                              ; just get integer - multiplied by 1
                bcs l306_3

l306_2          jsr twobyt                              ; multiply integer times angle value
l306_3          plp                                     ; get sign of angle
                bcc invert                              ; invert result if negative,do rts
                rts


;*************************************************************
;  angdst  -  set up values for distance * angles
;       vwork+x = x & y distances
;       a = angles : ang1,ang2,ang3,ang4,0,0,0,0
;       get  xdist1 = xdist1 * angle-1
;     ydist1 = ydist1 * angle-2
;     xdist2 = xdist2 * angle-3
;     ydist2 = ydist2 * angle-4
;*************************************************************
;
;angdst
; sta angcnt      ;save angles
; ldx #xdist1-vwork
;angd10
; asl angcnt
; jsr angmlt      ;multiply angle * distance
; sta vwork,x
; tya  ;save results
; sta vwork+1,x
; inx  ;point to next distance
; inx
; cpx #disend-vwork
; bcc angd10 ;loop 4 times
;angd20 rts

;.end

; GRAPHIC8.SRC
;****************************************************************
;  docolr  --  set up color for 8x8 charcater cell
;   x = row number  --  y = column number
;****************************************************************
;
;docolr lda _ldtb2,x      ;put address of video ram into grapnt
; sta grapnt
; lda graphic_ldtb1,x ;point to bit mapped color area
; sta grapnt+1
;
; lda colsel  ;get current color source selected
;
; bne l306_1   ;branch if NOT background
; lda fg_bg
; bit _graphm  ;test if mode = hires
; bpl 25$   ;if so, go set up byte
; rts   ;else exit
;
;l306_1 cmp #2
; bne l306_3   ;branch if NOT multi-color 1
;
;l306_2 lda fg_mc1  ;get correct packed colors for multicolor mode.
;25$ and #$0f
; sta z_p_temp_1
; lda (grapnt),y
; and #$f0
; ora z_p_temp_1
; sta (grapnt),y
; rts
;
;l306_3 bcs 40$   ;branch if multicolor 2
;
; lda fg_bg  ;here for foreground. get packed colors.
; and #$f0
; sta z_p_temp_1
; lda (grapnt),y  ;do foreground
; and #$0f
; ora z_p_temp_1
; sta (grapnt),y
; rts
;
;40$ lda grapnt+1  ;do multicolor 2
; and #3
; ora #>color_ram_hi ;set up to point to high color area
; sta grapnt+1
;
; lda #0   ;put i/o in map
; sta mmu_config_reg
;
; sei
; lda _6510_data_reg
; pha
; and #%11111110  ;point cpu at correct nybble bank
; sta _6510_data_reg
; lda multicolor_2
; sta (grapnt),y
; pla
; sta _6510_data_reg
; cli
; rts
;
;
;
;graphic_ldtb1   ;_ldtb1 adjusted for an org at color_ram_lo
;99$=color_ram_lo
;1$=color_ram_lo+40*1
;2$=color_ram_lo+40*2
;3$=color_ram_lo+40*3
;4$=color_ram_lo+40*4
;5$=color_ram_lo+40*5
;6$=color_ram_lo+40*6
;7$=color_ram_lo+40*7
;8$=color_ram_lo+40*8
;9$=color_ram_lo+40*9
;l306_1=color_ram_lo+40*10
;11$=color_ram_lo+40*11
;12$=color_ram_lo+40*12
;13$=color_ram_lo+40*13
;14$=color_ram_lo+40*14
;15$=color_ram_lo+40*15
;16$=color_ram_lo+40*16
;17$=color_ram_lo+40*17
;18$=color_ram_lo+40*18
;19$=color_ram_lo+40*19
;l306_2=color_ram_lo+40*20
;21$=color_ram_lo+40*21
;22$=color_ram_lo+40*22
;23$=color_ram_lo+40*23
;24$=color_ram_lo+40*24
;
; .byte >99$,>1$,>2$,>3$,>4$,>5$,>6$,>7$,>8$,>9$,>l306_1
; .byte >11$,>12$,>13$,>14$,>15$,>16$,>17$,>18$,>19$
; .byte >l306_2,>21$,>22$,>23$,>24$


;******************************************************************
;  getpos - get address in graphic bit map into grapnt
;      x = bit offset into byte specified (0-7)
;      y = offset to byte within 8x8 character cell
;      a = bit mask to the bit (or bits if multicolor mode)
;******************************************************************
;
;getpos jsr divpos      ;get xpos/ypos to column/row position
; bcs grprts      ;abort if position too large
;
;getps1 tya  ;get addr for row (X) and col (Y) in grapnt
; clc
; adc _ldtb2,x ;add column position to low byte offset
; sta grapnt
; lda _ldtb1,x ;get high byte screen address
; adc #0  ;add any carry
; asl grapnt
; rol a
; asl grapnt ;mult by 8 to get offset into 8k area
; rol a
; asl grapnt
; rol a
; sta grapnt+1
;
; lda ypos
; and #07
; tay  ;get byte offset into 8x8 char cell
; lda xpos
; bit _graphm
; php
; bpl grpos3 ;skip if not multicolor mode
; asl a  ;shift x-pos for multicolor mode
;
;grpos3 and #07
; tax
; lda rbits,x ;get bit mask
; plp
; bpl grprts ;done if not multicolor mode
; inx
; ora rbits,x ;mask for 2 bits if multicolor mode
;grprts rts
;
;rbits .byte   $80,$40,$20,$10,$08,$04,$02,$01


;**************************************************************
;  divpos  --  convert xpos to column number
;  convert ypos to row number
;  return carry set if either above limits
;**************************************************************
;
;divpos lda xpos+1
; lsr a
; bne l306_2       ;out of bounds if greater than 1
; lda xpos
; ror a
; lsr a  ;get column position = xpos/8
; bit _graphm
; bmi l306_1  ;skip if multicolor mode
; lsr a  ;divide by 8 if a hires or text mode
;l306_1 tay
; cpy #llen
; bcs l306_2  ;error exit if out of bounds
; lda ypos+1
; bne l306_2  ;out of bounds error if not = 0
; lda ypos
; lsr a
; lsr a  ;get row number = ypos/8
; lsr a
; tax
; cmp #nlines ;compare to max number of rows
; rts  ;carry clr if okay
;l306_2 sec
; rts


;***************************************************************
;   SCALXY  - Scale the x & y coordinates found in vwork+x
;***************************************************************
;
;scalxy lda scalem
; beq sclrts      ;do nothing if scaling off
;
; lda scale_x
; ldy scale_x+1
; jsr doscal      ;scale in the x-direction
;
; lda scale_y
; ldy scale_y+1 ;scale in the y direction
;
;doscal jsr twobyt ;multiply * coordinate
; sta vwork,x
; tya
; inx  ;store back into original position
; sta vwork,x
; inx
;sclrts
; rts

;.end

;GRAPHICS9.SRC
;***************************************************************
;   DOTWO  - Add      two 2-byte values if carry clear
;  Subtract two 2-byte values if carry set
;***************************************************************

dotwo2
                bcc addtw2                              ; go do addition
                bcs subtw2                              ; go do subtraction
dotwo
                bcs subtwo                              ; go do subtraction

;***************************************************************
;  ADDTWO  - Add vwork+y and vwork+x  Result in y/a
;***************************************************************

addtwo
                jsr settwo                              ; put vwrok+y into y/a

addtw2                                                  ; enter here to add y/a to vwork+x
                clc
                adc vwork,x
                pha
                tya
                adc vwork+1,x
                tay
                pla
                rts


;****************************************************************
;  SUBTWO  - Subtract vwork+y - vwork+x Result in y/a
;****************************************************************

subtwo
                jsr settwo                              ; move vwork+y into y/a

subtw2                                                  ; enter here with 1st value in y/a
                sec
                sbc vwork,x
                sta tempf1
                tya
                sbc vwork+1,x
                tay
                php
                lda tempf1
                plp
                rts


subtwo_savram
                lda savram,y                            ; load value into y,a
                pha
                lda savram+1,y
                tay
                pla
                sec
                sbc savram,x
                sta tempf1
                tya
                sbc savram+1,x
                tay
                php
                lda tempf1
                plp
                rts


;************************************************************
;  SETTWO  - Move value in vwork+y into y/a
;************************************************************

settwo
                lda vwork,y
                pha
                lda vwork+1,y
                tay
                pla
                rts

;******************************************************************
;  ABSTWO  - Get absolute value of vwork+y - vwork+x
;  Result in y/a  -  carry === vwork+y >= vwork+x
;******************************************************************

abstwo                                                  ; movspr_to [910809]
                jsr subtwo                              ; subtract vwork+y - vwork+x
abstw2                                                  ; entrance with vwork+y in y/a
                bpl absrts                              ; done if result is positive
invert          php
                clc
                eor #$ff                                ; invert low byte result and add 1
                adc #1
                pha
                tya
                eor #$ff                                ; invert high byte result
                adc #0                                  ; add back any carry
                tay
                pla
                plp
absrts          rts


;****************************************************************
;  TWOBYT  - Multiply 2 byte fraction in y/a times 2 bytes
;  Integer found in vwork+x-reg.  Result = y/a
;****************************************************************

twobyt
                sty vtemp1                              ; save fraction
                sta vtemp2
                lda vwork,x
                ldy vwork+1,x
                php                                     ; save sign of integer
                jsr abstw2                              ; absolute value
                sta vwork,x
                tya
                sta vwork+1,x
                lda #0
                sta vtemp3                              ; initialize result to zero

                ldy #16                                 ; initialize count
l307_1          lsr vtemp1
                ror vtemp2
                bcc l307_2                              ; skip if no bit set
                clc
                adc vwork,x                             ; add integer low byte
                pha
                lda vtemp3
                adc vwork+1,x                           ; add integer high byte to total
                sta vtemp3
                pla

l307_2          lsr vtemp3                              ; divide by 2
                ror
                dey
                bne l307_1                              ; loop 16 times - test all bits in 2 bytes

                adc #0                                  ; add back round factor
                ldy vtemp3
                bcc l307_3
                iny
l307_3          plp                                     ; pop sign
                bra abstw2                              ; return with signed product in y/a


;******************************************************************
;  dstpos  -  move xdest/ydest to xpos/ypos
;******************************************************************
;
;dstpos
; ldy #0
; jsr dstmov
; ldy #2
;dstmov
; lda xdest,y
; sta xpos,y
; lda xdest+1,y
; sta xpos+1,y
; rts

;.end

;GRAPHICS10.SRC
;************************************************************
;   incolr  --  get color selection parameter into colsel
;************************************************************
;
;incolr
; ldx #1   ;get an optional 1 byte val, def=fg(1)
; jsr chrgot
;incol1
; beq incol2       ;eol, use default
; cmp #','
; beq incol2       ;just ',', use default
; jsr getbyt
; cpx #4   ;must be 0-3
; bcs illval       ;..else illegal value
; cpx #2
; bit _graphm       ;if hires, must be 0 or 1
; bmi incol2
; bcs illval
;incol2
; stx colsel
; rts
;
;illval
; jmp fcerr  ;illegal value



;******************************************************************
;  INCORD  ---  Get X,Y coordinate from input stream into vwork+x
;
;  Coordinate may have any of the forms:
;    x,y  = absolute xpos & absolute ypos
; +/-x,y  = relative xpos & absolute ypos
;    x,+/-y = absolute xpos & relative ypos
; +/-x,+/-y = relative xpos & relative ypos
;    x;y  = x-distance at an angle y
;
;  Relative and angle distances are relative to current x,ypos.
;  Values are scaled to current mode parameters if required.
;******************************************************************


incor2                                                  ; enter here for optional argument
                jsr chrgot                              ; end of line?
                beq l308_1                              ; yes, use defaults
                jsr chkcom
                cmp #','                                ; is there really an arg?
                bne incord                              ; yes, let'er rip

l308_1          ldy #0                                  ; set default pos = current pos
l308_2          lda xpos,y
                sta vwork,x
                inx
                iny
                cpy #4
                bcc l308_2
                rts


;incor3    ;enter here for non-optional arg preceded by a comma
; jsr chkcom
incord
                stx vtemp4                              ; save offset to destination
                jsr cordsb                              ; get 2-byte x-parameter
                jsr chrgot
                cmp #','
                beq docord                              ; skip ahead if have comma

                cmp #';'                                ; check for semi-colon
                +lbne snerr                             ; missing angle param- show syntax message
                jsr chrget       ;skip over '           ; '
                jsr getwrd                              ; get 2-byte angle in a,y
                sta z_p_temp_1                          ; swap a,y
                tya
                ldy z_p_temp_1
                jsr gtang1                              ; get sine & cosine values for the angle
                ldx vtemp4
                lda vwork,x
                sta vwork+2,x                           ; move length to y-parameter
                lda vwork+1,x
                sta vwork+3,x
; jsr scalxy       ;scale the values
                lda #$0e
                sta vtemp5
                clc
                ldx vtemp4

l309_1          jsr angmlt                              ; multiply length * angle
                sta vwork,x                             ; save angle result
                tya
                sta vwork+1,x
                ldy #xpos-vwork
                lsr vtemp5
                bcc l309_2
                ldy #ypos-vwork

l309_2          jsr dotwo                               ; add/subtract value to current position
                sta vwork,x
                tya                                     ; save result in destination
                sta vwork+1,x
                inx
                inx
                lsr vtemp5
                bne l309_1                              ; do y-coordinate
                clc
                rts


docord          jsr chrget                              ; skip over comma
                inc vtemp4                              ; point to y-destination
                inc vtemp4
                jsr cordsb                              ; get y-paramter
; ldx vtemp4
; dex
; dex
; jsr scalxy       ;scale the values
                ldy #ypos-vwork
                ldx vtemp4
                inx
                inx

docor1          dex
                dex
                lsr vtemp5
                bcc docor2                              ; skip if not relative
                jsr addtwo                              ; add to current position
                sta vwork,x
                tya
                sta vwork+1,x

docor2          ldy #xpos-vwork
                cpx vtemp4
                beq docor1                              ; loop to do x-coordinate
                clc
                rts

;
; CORDSB -- Get the next 2-byte parameter
;

cordsb          jsr chrgot                              ; read character
                cmp #plus_token                         ; check if relative - plus sign
                beq l310_1                              ; skip if yes
                cmp #minus_token
                beq l310_1                              ; skip if relative - minus sign
                clc                                     ; .c=1 if relative coord, .c=0 if absolute
l310_1          rol vtemp5                              ; save coord type for later
                jsr frmnum
                jsr getsad                              ; get signed 2 byte coordinate (y,a), do rts
                ldx vtemp4
                sta vwork+1,x                           ; save 2-byte parameter
                tya
                sta vwork,x
                rts

;.end

;GRAPHICS11.SRC

;  ANGVAL  -- Table of angle values on 10 degree boundaries
;  Values based as fraction of 65536

angval
                !text $00,$00                           ; sine 00 degrees -  .0000
                !text $2c,$71                           ; sine 10 degrees -  .1736
                !text $57,$8d                           ; sine 20 degrees -  .3420
                !text $80,$00                           ; sine 30 degrees -  .5000
                !text $a4,$8f                           ; sine 40 degrees -  .6428
                !text $c4,$19                           ; sine 50 degrees -  .7660
                !text $dd,$b2                           ; sine 60 degrees -  .8660
                !text $f0,$90                           ; sine 70 degrees -  .9397
                !text $fc,$1c                           ; sine 80 degrees -  .9848
                !text $ff,$ff                           ; sine 90 degrees - 1.0000

;  INCVAL  -- Table of incremental values between 10 degrees
;  Values based on fraction of 65536

incval
                !text $04,$72                           ; 01 - 09 degrees -  .01739
                !text $04,$50                           ; 11 - 19 degrees -  .01692
                !text $04,$0b                           ; 21 - 29 degrees -  .01592
                !text $03,$a8                           ; 31 - 39 degrees -  .01443
                !text $03,$28                           ; 41 - 49 degrees -  .01252
                !text $02,$90                           ; 51 - 59 degrees -  .01023
                !text $01,$e3                           ; 61 - 69 degrees -  .00762
                !text $01,$28                           ; 71 - 79 degrees -  .00477
                !text $00,$63                           ; 81 - 89 degrees -  .00179

;.end
