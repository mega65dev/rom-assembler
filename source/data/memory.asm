

                * = $0200

buflen          = 161                                   ; input buffer size (2 80-column lines + 1)
buf             !fill buflen                            ; BASIC/Monitor line input buffer
buf_txtptr      = buf-1


                * = $02c0
; BASIC RAM code  (RAM code not needed- following moved to ROM)
;
; chrget *=*+42  ;get byte from text bank after incrementing TXTPTR
; chrgot = chrget+2 ;get byte from text bank at TXTPTR
; qnum  = chrget+27 ;evaluate byte as a number & set flags accordingly

adray1          !fill 2                                 ; ptr to routine:  convert float -> integer ???? why keep
adray2          !fill 2                                 ; ptr to routine:  convert integer -> float ???? why keep
zero            !fill 3                                 ; numeric constant for BASIC, downloaded from ROM

errnum          !fill 1                                 ; used by error trapping routine-last error number
errlin          !fill 2                                 ; line # of last error ($FFFF if no error)
trapno          !fill 2                                 ; line to go to on error ($FFxx if none set)
tmptrp          !fill 1                                 ; hold trap # temporary
errtxt          !fill 2                                 ; pointer to statement causing last error
max_mem_0       !fill 2                                 ; highest address available to BASIC in RAM 0 (text bank)

current_bank    !fill 1                                 ; context for PEEK,POKE,BOOT,SYS,WAIT,BLOAD/SAVE set by BANK.
fin_bank        !fill 1                                 ; bank pointer for string->number conversion routine FIN
tmpdes          !fill 4                                 ; pointers to temporary descriptors for INSTR
bits            !fill 1                                 ; flag for math bit/byte shifter
highlight_color !fill 1                                 ; color for highlighting text
highlight_save  !fill 1                                 ; saves normal color during highlighting, msb=flag
find_count      !fill 1                                 ; count for LIST to highlight FIND text

; Interrupt stuff

irq_wrap_flag   !fill 1                                 ; used by BASIC_IRQ to block all but one IRQ call
intval          !fill 1                                 ; BASIC interrupts enabled (via collision command)
int_trip_flag   !fill 3                                 ; flags which interrupts occurred
int_adr_lo      !fill 3                                 ; where to go for each type of collision (line number)
int_adr_hi      !fill 3                                 ;
collisions      !fill 2                                 ; sprite collisions, s/s and s/bgnd, recorded during IRQ
lightpen_xpos   !fill 1                                 ; lightpen position recorded during IRQ
lightpen_ypos   !fill 1

; dejavu *=*+1  ;'cold' or 'warm' reset status (must be in page 5!)????

; nmi_wrap_flag *=*+1  ;used by BASIC_NMI to block all but one NMI call [910523]
;    ; (removed)      [910826]
;(leaving 12 bytes)

; BASIC indirect vectors

                * = $02f7

usrpok          !fill 3                                 ; USR vector (must be set by application)

vectors_begin
iAutoScroll     !fill 2                                 ; AutoScroll vector
esc_fn_vec      !fill 2                                 ; Escape Function vector
graphic_vector  !fill 2                                 ; Graphic Kernel vector (was 'bnkvec')
ierror          !fill 2                                 ; indirect error (output error in .x)
imain           !fill 2                                 ; indirect main (system direct loop)
icrnch          !fill 2                                 ; indirect crunch (tokenization routine)
iqplop          !fill 2                                 ; indirect list (char list)
igone           !fill 2                                 ; indirect gone (char dispatch)
ieval           !fill 2                                 ; indirect eval (symbol evaluation)
iesclk          !fill 2                                 ; escape token crunch
iescpr          !fill 2                                 ; escape token list
iescex          !fill 2                                 ; escape token execute
itime           !fill 2                                 ; 60Hz interrupt vector (before jiffy)
cinv            !fill 2                                 ; IRQ RAM vector
cbinv           !fill 2                                 ; BRK RAM vector

; Remainder of this area reserved for Kernel indirects & Kernel RAM code


                * = $0400                               ; BASIC's run-time stack (2 pages)
stktop                                                  ; (also used by BOOT SYS and Monitor)
stkbot          = $05ff


                * = $0600                               ; Sprite definitions (2 pages, must be below $1000)
sprite_base


                * = $0800
screen_start                                            ; Text display screen
                * = *+2000

sprite_ptrs_40  = screen_start+$3f8
sprite_ptrs_80  = screen_start+$7f8


                * = $1170                               ; previous to this used by Kernel

oldlin          !fill 2                                 ; BASIC storage
oldtxt          !fill 2                                 ; BASIC storage
rndx            !fill 5                                 ; Floating Point representation of last random #


; Yet more temporaries shared by various routines

window_temp                                             ; window  (4 bytes)
t3                                                      ; dcat  (1 byte)
renum_tmp_1                                             ; renumber (2 bytes)
tmptxt          !fill 2                                 ; do/loop (2 bytes)

t4                                                      ; dcat  (2 bytes)
renum_tmp_2                                             ; renumber (2 bytes)
tmplin          !fill 2                                 ; do/loop (2 bytes)


;  BASIC/DOS interface vars  (20 bytes)

dosofl          !fill 2                                 ; BLOAD/BSAVE starting addr
dosofh          !fill 2                                 ; BSAVE ending addr
dosla           !fill 1                                 ; DOS logical addr
dosfa           !fill 1                                 ; DOS physical addr
dossa           !fill 1                                 ; DOS secondary addr

xcnt            !fill 1                                 ; DOS loop counter------ this area zeroed-out each DOS call -----
dosf1l          !fill 1                                 ; DOS filename 1 len
dosds1          !fill 1                                 ; DOS disk drive 1
dosf2l          !fill 1                                 ; DOS filename 2 len
dosds2          !fill 1                                 ; DOS disk drive 2
dosf2a          !fill 2                                 ; DOS filename 2 addr
dosrcl          !fill 1                                 ; DOS record length
dosbnk          !fill 1                                 ; DOS load/save bank
dosdid          !fill 2                                 ; DOS ID identifier
dosflags        !fill 1                                 ; DOS flags  7:ID,  6:recover
dossa_temp      !fill 1                                 ; temp storage for file's sa during RECORD command
dosspc          = *-xcnt                                ; space used by DOS routines-------------------------------------

savram          !fill 67                                ; buffer used by MOVSPR, SPRDEF, SAVSPR, and DOS parser

xabs            = savram                                ; movspr_line calculations   [910809]
yabs            = savram+2
xsgn            = savram+4
ysgn            = savram+6
fct             = savram+8
errval          = savram+12


; PRINT USING definitions & storage  (24 bytes)

puchrs                                                  ; Declarations for PRINT USING...
pufill          !fill 1                                 ; print using fill symbol
pucoma          !fill 1                                 ; print using comma symbol
pudot           !fill 1                                 ; print using decimal point symbol
pumony          !fill 1                                 ; print using monetary symbol

bnr             !fill 1                                 ; pointer to begin #
enr             !fill 1                                 ; pointer to end #
dolr            !fill 1                                 ; dollar flag
flag            !fill 1                                 ; comma flag (also used by PLAY)????
swe             !fill 1                                 ; counter
usgn            !fill 1                                 ; sign exponent
uexp            !fill 1                                 ; pointer to exponent
vn              !fill 1                                 ; # of digits before decimal point
chsn            !fill 1                                 ; justify flag
vf              !fill 1                                 ; # of positions before decimal point (field)
nf              !fill 1                                 ; # of positions after decimal point (field)
posp            !fill 1                                 ; +/- flag (field)
fesp            !fill 1                                 ; exponent flag (field)
etof            !fill 1                                 ; switch
cform           !fill 1                                 ; char counter (field)
sno             !fill 1                                 ; sign no
blfd            !fill 1                                 ; blank/star flag
begfd           !fill 1                                 ; pointer to begin of field
lfor            !fill 1                                 ; length of format
endfd           !fill 1                                 ; pointer to end of field


;  * = $1200 ;BASIC Graphic, Sprite, Music, & Sound storage

;  The following 24 bytes are multiply defined...
;
; params = *

;  Circle drawing variables  (multiply defined).
;
; xcircl *=*+2  ;circle center, x coordinate
; ycircl *=*+2  ;circle center, y coordinate
; xradus *=*+2  ;x radius
; yradus *=*+2  ;y radius
; rotang *=*+4  ;rotation angle
; angbeg *=*+2  ;arc angle start
; angend *=*+2  ;arc angle end
; xrcos  *=*+2  ;x radius * cos(rotation angle)
; yrsin  *=*+2  ;y radius * sin(rotation angle)
; xrsin  *=*+2  ;x radius * sin(rotation angle)
; yrcos  *=*+2  ;y radius * cos(rotation angle)

; parend = *

;  Box drawing variables  (multiply defined).
;
;  *=params
; xcord1 *=*+2  ;point 1 x-coord.
; ycord1 *=*+2  ;point 1 y-coord.
; boxang *=*+2  ;rotation angle
; xcount *=*+2
; ycount *=*+2
; bxleng *=*+2  ;length of a side
; xcord2 *=*+2
; ycord2 *=*+2


;  Shape variables  (multiply defined).
;
;  *=params
;  *=*+1  ;placeholder
; keylen *=*+1
; keynxt *=*+1
; strsz  *=*+1  ;string len
; gettyp *=*+1  ;replace shape mode
; strptr *=*+1  ;string pos'n counter
; oldbyt *=*+1  ;old bit map byte
; newbyt *=*+1  ;new string or bit map byte
;  *=*+1  ;placeholder
; xsize  *=*+2  ;shape column length
; ysize  *=*+2  ;shape row length
; xsave  *=*+2  ;temp for column length
; stradr *=*+2  ;save shape string descriptor
; bitidx *=*+1  ;bit index into byte

;  General use parameters  (multiply defined).
;
;  *=params
; xcentr *=*+2
; ycentr *=*+2
; xdist1 *=*+2
; ydist1 *=*+2
; xdist2 *=*+2
; ydist2 *=*+2
; disend
;  *=*+2  ;placeholder
; colcnt *=*+1  ;char's col. counter
; rowcnt *=*+1
; strcnt *=*+1


;  General  graphic & sound  buffers & assignments
;
;  * = parend

;  General graphic storage (used by C128-type sprite routines in C65)

vwork                                                   ; graphics & sprite vars
xpos            !fill 2                                 ; current x position
ypos            !fill 2                                 ; current y position
xdest           !fill 2                                 ; x-coordinate destination
ydest           !fill 2                                 ; y-coordinate destination

numcnt          !fill 1                                 ; temp, usually coordinate type
vtemp1          !fill 1                                 ; used by sprite math stuff ????was base page
vtemp2          !fill 1                                 ; ????was base page
vtemp3          !fill 1                                 ; misc. graphic temp storage
vtemp4          !fill 1
vtemp5          !fill 1

; mvdflg *=*+1  ;flag if 10k hires allocated ???? this stuff was base page
; colsel *=*+1  ;current color selected
; multicolor_1 *=*+1
; multicolor_2 *=*+1
; foreground *=*+1
; scalem *=*+1  ;scale mode flag
; scale_x *=*+2  ;scale factor in x
; scale_y *=*+2  ;scale factor in y
; stopnb *=*+1  ;stop paint if not background/not same color
; fg_bg  *=*+1  ;packed foreground/background color nybbles
; fg_mc1 *=*+1  ;packed foreground/multicolor 1 color nybbles

; bitcnt *=*+1  ;temp for gshape
; width  *=*+1  ;double width flag
; filflg *=*+1  ;box fill flag
; circle_segment *=*+1  ;degrees per circle segment
; bitmsk *=*+1  ;temp for bit mask

; character_rom *=*+1  ;high byte of address of char rom for 'char' command
; upper_lower *=*+1  ;pointer to upper/lower case for char command
; upper_graphic *=*+1  ;   "       upper/graphic

;  DrawLine stuff
;
; xabs  *=*+2  ;16 bytes
; yabs  *=*+2
; xsgn  *=*+2
; ysgn  *=*+2
; fct  *=*+4
; errval *=*+2
; lesser *=*+1
; greatr *=*+1

;  Angle stuff (used by sprites)

angsgn          !fill 1                                 ; sign of angle
sinval          !fill 2                                 ; sine of value of angle
cosval          !fill 2                                 ; cosine of value of angle
; angcnt *=*+2  ;temps for angle distance routines


; Sprite stuff

savsiz          !fill 4                                 ; temp work locations for SSHAPE, SPRSAV, MOVSPR_TO
lesser
sprtmp_1        !fill 1                                 ; temp for SPRSAV
greatr
sprtmp_2        !fill 1

sprite_data     !fill 88                                ; speed/direction tables for 8 sprites, 11 bytes each
;   move ang/dist move line
; offset= 0 b7=0+speed b7=1+speed
;  1 counter  counter lo
;  2 angle sign         hi
;  3,4 delta-X  dir+min/max
;  5,6 delta-Y  fct1
;  7,8 total-X  fct2
;  9,10 total-Y  error

init_as_0       = *-sprite_data-1

; vic_save *=*+21  ;copy of VIC reg's, used to update chip during retrace

; defmod *=*+1  ;for SPRDEF
; lincnt *=*+1  ; "
; sprite_number *=*+1  ; "


; Music stuff driving stereo SIDs, 3 voices each

voices          !fill 12                                ; Voice counters (activity flags)  [910612] stereo
waveform        !fill 6                                 ; Waveforms for each voice   [910612] stereo

voice           !fill 1                                 ; Play note parameters
octave          !fill 1
sharp           !fill 1
dnote           !fill 1
tempo_rate      !fill 1                                 ; duration of whole note 4/4 time = 24/rate
pitch           !fill 2
ntime           !fill 2

filters1        !fill 4                                 ; Volume & Filter parameters   [910612] was 5
filters2        !fill 4                                 ; [910612] stereo
fltsav          !fill 4                                 ; temps
fltflg          !fill 1                                 ; temp

tonnum          !fill 1                                 ; Tune Envelope stuff
tonval          !fill 3

atktab          !fill 10                                ; Tune Envelopes
sustab          !fill 10
wavtab          !fill 10
pulslw          !fill 10
pulshi          !fill 10

parcnt          !fill 1                                 ; temp: envelope
nibble          !fill 1                                 ; temp: envelope, filter


; SOUND command stuff

sound_voice     !fill 1
sound_time_lo   !fill 3+3                               ; [910612] stereo
sound_time_hi   !fill 3+3                               ; [910612] stereo
sound_max_lo    !fill 3+3                               ; [910612] stereo
sound_max_hi    !fill 3+3                               ; [910612] stereo
sound_min_lo    !fill 3+3                               ; [910612] stereo
sound_min_hi    !fill 3+3                               ; [910612] stereo
sound_direction !fill 3+3                               ; [910612] stereo
sound_step_lo   !fill 3+3                               ; [910612] stereo
sound_step_hi   !fill 3+3                               ; [910612] stereo
sound_freq_lo   !fill 3+3                               ; [910612] stereo
sound_freq_hi   !fill 3+3                               ; [910612] stereo

;above must end before $1300
                * = $1160
;below must end before $1170

temp_time_lo    !fill 1
temp_time_hi    !fill 1
temp_max_lo     !fill 1
temp_max_hi     !fill 1
temp_min_lo     !fill 1
temp_min_hi     !fill 1
temp_direction  !fill 1
temp_step_lo    !fill 1
temp_step_hi    !fill 1
temp_freq_lo    !fill 1
temp_freq_hi    !fill 1
temp_pulse_lo   !fill 1
temp_pulse_hi   !fill 1
temp_waveform   !fill 1

pot_temp_1      !fill 1                                 ; temporaries for 'POT' function
pot_temp_2      !fill 1


                * = $1300

dosstr          !fill 256                               ; DOS input/output string buffer


                * = $1f00                               ; Graphics Kernel Interface

GKI__parm1      !fill 1                                 ; ml interface parm values
GKI__parm2      !fill 1
GKI__parm3      !fill 1
GKI__parm4      !fill 1
GKI__parm5      !fill 1
GKI__parm6      !fill 1
GKI__parm7      !fill 1
GKI__parm8      !fill 1
GKI__parm9      !fill 1
GKI__parm10     !fill 1
GKI__parm11     !fill 1
GKI__parm12     !fill 1
GKI__parm13     !fill 1
GKI__parm14     !fill 1
GKI__parm15     !fill 1
GKI__parm16     !fill 1
GKI__parm17     !fill 1

GKI__subparm1   !fill 1                                 ; subroutine parm values
GKI__subparm2   !fill 1
GKI__subparm3   !fill 1
GKI__subparm4   !fill 1
GKI__subparm5   !fill 1

GKI__temp1      !fill 1                                 ; local variables within subroutines
GKI__temp2      !fill 1
GKI__temp3      !fill 1
GKI__temp4      !fill 1
GKI__temp5      !fill 1
GKI__temp6      !fill 1
GKI__temp7      !fill 1
GKI__temp8      !fill 1
GKI__temp9      !fill 1
GKI__temp10     !fill 1
GKI__temp11     !fill 1
GKI__temp12     !fill 1
GKI__temp13     !fill 1
GKI__temp14     !fill 1
GKI__temp15     !fill 1
GKI__temp16     !fill 1
GKI__temp17     !fill 1

;.end

