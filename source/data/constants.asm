; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      constants.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************
; General assignments and equates

doslfn          = 0                                     ; DOS' private logical file number
dosffn          = 8                                     ; DOS' default drive number

strsiz          = 3                                     ; string descriptor size in temps. and arrays
lengos          = 5                                     ; length of a GOSUB entry on the runtime stack
lenfor          = 18                                    ; length of a FOR entry in the runtime stack

; maxchr = 80  ;misc. command equates
; llen  = 40
; nlines = 25
column_width    = 10                                    ; print comma spacing

beats_pal       = 1200                                  ; whole note (4/4 time = 2sec.)    [910724]
beats_ntsc      = 1440                                  ;

lf              = $0a                                   ; line feed
cr              = $0d                                   ; carriage return
esc             = $1b                                   ; escape
pi              = $ff

basbgn          = $2000                                 ; bottom of BASIC text bank
baswrk          = basbgn+1                              ; where BASIC text starts
varbgn          = $2000                                 ; bottom of BASIC data bank (C65: DOS RAM below $12000)
bank_0_top      = $8000                                 ; top of BASIC text bank ($FF00)    [910528]
bank_1_top      = $8000                                 ; top of BASIC data bank ($f800, VIC attributes)  [910528]

; graphic_base = $2000
; color_ram_lo = $1c00
; color_ram_hi = $d800

vic             = $d000                                 ; Video controller

fdc             = $d080                                 ; Built-in floppy disk controller

sid1            = $d400                                 ; Audio processors (right)
sid2            = $d420                                 ; (left)

d1_6526         = $dc00                                 ; Ports, peripheral control
d1pra           = d1_6526
d2_6526         = $dd00
d2pra           = d2_6526

dma_ctlr        = $d700                                 ; DMA Controller


;  BASIC base page storage

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
