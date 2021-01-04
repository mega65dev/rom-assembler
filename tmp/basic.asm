;[[system.header]]



;  ***************************************************************************
;  *                               //                                        *
;  *              CCCCCCC         //    6666666     555555555                *
;  *             CCC   CCC       //    666   666    555                      *
;  *            CCC             //    666           555                      *
;  *            CCC            //     666 6666      55555555                 *
;  *            CCC           //      6666   666          555                *
;  *            CCC          //       666     666          555               *
;  *             CCC   CCC  //         666   666    555   555                *
;  *              CCCCCCC  //           6666666      5555555                 *
;  *                      //                                                 *
;  *                                                                         *
;  *          BBBBBBBBB      AAAA      SSSSSSSS   III    CCCCCCC             *
;  *          BBB    BBB   AAA  AAA   SSS    SSS  III   CCC   CCC            *
;  *          BBB    BBB  AAA    AAA  SSS         III  CCC                   *
;  *          BBBBBBBBB   AAAAAAAAAA   SSSSSSSS   III  CCC                   *
;  *          BBB    BBB  AAA    AAA         SSS  III  CCC                   *
;  *          BBB    BBB  AAA    AAA  SSS    SSS  III   CCC   CCC            *
;  *          BBBBBBBBB   AAA    AAA   SSSSSSSS   III    CCCCCCC             *
;  *                                                                         *
;  *                       V E R S I O N   1 0 . 0                           *
;  *              *
;  *        Copyright (C)1991  by   Commodore Business Machines, Inc.        *
;  *              *
;  *       All  Rights  Reserved        *
;  *              *
;  ***************************************************************************

;   ROM VERSION  911115  (ver 0.9B)

; ******************************************************************
; *                                                                *
; * This listing contains confidential and proprietary information *
; * of CBM, Inc.  The reproduction, dissemination or disclosure to *
; * others without express written permission is prohibited.  This *
; * software is for use in prototype Commodore C/65 systems only.  *
; *                                                                *
; *  The information in this document will change without notice.  *
; *                                                                *
; *  No  responsibility  is  assumed  for the reliability of this  *
; *                          software.                             *
; *                                                                *
; ******************************************************************



; This version written and assembled by Fred Bowen using BSO format.

; Adapted from the following C128 files, ROM part numbers 318018-04, 3180194-04:
;
; disclaim  resume   hexfunc
; declare   doloop   rgr
; entries   key   rclr
; header   paint   joy
; init   box   penpot
; indjumps  sshape   pointer
; crunch   gshape   rsprite
; tokens1   circle   rspcolor
; tokens2   draw   bump
; disptable  char   rsppos
; errmsgs   locate   xor
; errprint  scale   rwindow
; execute   color   rnd
; functions  scnclr   code12
; code0   graphic   stringfns
; rtstack   bank   code17
; findline  sleep   code18
; lineget   wait   code19
; list   sprite   code21
; newclr   movspr   code22
; return   play   code23
; remdata   filter   code24
; if   envelope  code26
; ongoto   collision  grbcol
; let   sprcolor  trig
; print   width   using
; input   volume   instring
; next   sound   graphic3
; dim   window   rdot
; sys   boot   graphic7
; trontroff  sprdef   graphic8
; rreg   sprsav   graphic9
; midequal  fast   graphic10
; auto   slow   graphic11
; help   checkval  sethires
; gosubgoto  formeval  clrhires
; go   variables  dos1
; continue  getpointr  dos2
; run   array   dos3
; restore   patcheslo  dos4
; renumber  fre   overflow
; for   val   irq
; delete   dec   stash
; pudef   peekpoke  fetch
; trap   errfunc   swap
; patcheshi  jumptable  def
; strings








;[[data.constants]]
; General assignments and equates

doslfn           = 0                                      ; DOS' private logical file number
dosffn           = 8                                      ; DOS' default drive number

strsiz           = 3                                      ; string descriptor size in temps. and arrays
lengos           = 5                                      ; length of a GOSUB entry on the runtime stack
lenfor           = 18                                     ; length of a FOR entry in the runtime stack

; maxchr = 80  ;misc. command equates
; llen  = 40
; nlines = 25
column_width     = 10                                     ; print comma spacing

beats_pal        = 1200                                   ; whole note (4/4 time = 2sec.)    [910724]
beats_ntsc       = 1440                                   ;

lf               = $0a                                    ; line feed
cr               = $0d                                    ; carriage return
esc              = $1b                                    ; escape
pi               = $ff

basbgn           = $2000                                  ; bottom of BASIC text bank
baswrk           = basbgn+1                               ; where BASIC text starts
varbgn           = $2000                                  ; bottom of BASIC data bank (C65: DOS RAM below $12000)
bank_0_top       = $8000                                  ; top of BASIC text bank ($FF00)    [910528]
bank_1_top       = $8000                                  ; top of BASIC data bank ($f800, VIC attributes)  [910528]

; graphic_base = $2000
; color_ram_lo = $1c00
; color_ram_hi = $d800

vic              = $d000                                  ; Video controller

fdc              = $d080                                  ; Built-in floppy disk controller

sid1             = $d400                                  ; Audio processors (right)
sid2             = $d420                                  ; (left)

d1_6526          = $dc00                                  ; Ports, peripheral control
d1pra            = d1_6526
d2_6526          = $dd00
d2pra            = d2_6526

dma_ctlr         = $d700                                  ; DMA Controller


;  BASIC base page storage
;[[data.zeropage]]

                 * = $0000

                 !fill 2                                  ; '4510' registers (not used in C65 mode)
srchtk           !fill 1                                  ; token 'search' looks for (run-time stack) / SYS 'bank#'

                 * = $000a                                ; skip over SYS address, status, a/x/y/z registers

integr                                                    ; used by math routines (this & following location)
charac           !fill 1
endchr           !fill 1
verck            !fill 1                                  ; LOAD/VERIFY flag
count            !fill 1                                  ; temp used all over
dimflg           !fill 1                                  ; DIM flag used by variable search
valtyp           !fill 1                                  ; 0=numeric, $FF=string
intflg           !fill 1                                  ; b7: (0=float,1=integer), b6: (1=get flag)
garbfl                                                    ; garbage collection temporary
dores            !fill 1                                  ; b7: P1LINE quote flag
subflg           !fill 1                                  ; b7: subscript flag (set to disallow subscripts() & integers%)
input_flag       !fill 1                                  ; READ($98), GET($40), or INPUT($00)
domask
tansgn           !fill 1
channl           !fill 1                                  ; active I/O channel
poker                                                     ; temp used all over
linnum           !fill 2                                  ; line number

temppt           !fill 1                                  ; pointer to next temporary descriptor in tempst
lastpt           !fill 2                                  ; pointer to last used temporary string
tempst           !fill 9                                  ; temporary descriptor pointers (3 at 3 bytes each)

index
index1           !fill 2
index2           !fill 2

multiplicand                                              ; 2 bytes wide, for unsigned integer multiply
resho            !fill 1
resmoh           !fill 1
product                                                   ; 3 bytes wide, for unsigned integer multiply
addend
resmo            !fill 1
reslo            !fill 1
                 !fill 1
txttab           !fill 2                                  ; where BASIC program begins   (text_bank)
vartab           !fill 2                                  ; where variable descriptors begin  (var_bank)
arytab           !fill 2                                  ; where array table begins   (var_bank)
strend           !fill 2                                  ; where arrays table ends   (var_bank)
fretop           !fill 2                                  ; bottom of string storage   (var_bank)
frespc           !fill 2                                  ; where temporary strings begin   (var_bank)
max_mem_1        !fill 2                                  ; highest address available to BASIC in RAM 1 (var_bank)
curlin           !fill 2
txtptr           !fill 2                                  ; pointer to BASIC text used by CHRGET, etc.
form                                                      ; used by print using
fndpnt           !fill 2                                  ; pointer to item found by search
datlin           !fill 2
datptr           !fill 2
inpptr           !fill 2
varnam           !fill 2
fdecpt
varpnt           !fill 2
lstpnt
andmsk
forpnt           !fill 2
eormsk           =forpnt+1
vartxt
opptr            !fill 2
opmask           !fill 1
grbpnt
tempf3
defpnt           !fill 2
dscpnt           !fill 2
token_saver                                               ; temp used by P1LINE/HELPSB (was spare????) [910628]
trmpos           !fill 1                                  ; temp used by SPC(), TAB()   [910628]

helper           !fill 1                                  ; P1LINE flag b7: HELP vs. LIST
;  b6: memory vs. file
;  b5: FIND/CHANGE
;  b4: highlight tokens
;  b3: highlight REM
;  b1: LINGET flag for AUTOSCROLL
;  b0: token in progress

jmper            !fill 1                                  ; 3 locations used by Function handler
                 !fill 1                                  ;
oldov            !fill 1                                  ;

tempf1           !fill 1                                  ; used by math routines
ptarg1           =tempf1                                  ; multiply defined for INSTR thru FACexp
ptarg2           =tempf1+2                                ; (also used by Monitor Utility, thru lowtr)
str1             =tempf1+4
str2             =tempf1+7
positn           =tempf1+10
match            =tempf1+11

arypnt
highds           !fill 2
hightr           !fill 2

tempf2           !fill 1                                  ; used by math routines
deccnt           !fill 2
tenexp           = deccnt+1
grbtop
dptflg
lowtr            !fill 1
expsgn           !fill 1

fac                                                       ; Floating point accumulator (primary) FAC1
dsctmp
facexp           !fill 1
facho            !fill 1
facmoh           !fill 1
indice
facmo            !fill 1
faclo            !fill 1
facsgn           !fill 1
degree
sgnflg           !fill 1

argexp           !fill 1                                  ; Floating point accumulator (secondary) FAC2
argho            !fill 1
argmoh           !fill 1
argmo            !fill 1
arglo            !fill 1
argsgn           !fill 1

strng1
arisgn           !fill 1
facov            !fill 1

strng2
polypt
curtol
fbufpt           !fill 2

autinc           !fill 2                                  ; incremental value for AUTO (0=off)

z_p_temp_1       !fill 1                                  ; USING's leading zero counter
;GET, RENUMBER, KEY temporary
;MOVSPR, SPRITE, PLAY, VOL temporary
;MID$= temporary

hulp                                                      ; counter
keysiz           !fill 1

syntmp           !fill 1                                  ; used as temp all over the place
dsdesc           !fill 3                                  ; descriptor for DS$
tos              !fill 2                                  ; top of run time stack
runmod           !fill 1                                  ; flags run/direct(b7), load(b6), trace(b5), edit(b4) modes
; autoboot wedge (b0)
point                                                     ; USING's pointer to decimal point, 2 bytes used by AutoScroll
parsts           !fill 1                                  ; DOS parser status word
parstx           !fill 1                                  ; DOS parser status extensions

oldstk           !fill 1                                  ; BASIC saves uP stack pointer here

text_top         !fill 2                                  ; top of BASIC text pointer  (in text_bank)
text_bank        !fill 1                                  ; where BASIC text lives   (RAM0 default)
var_bank         !fill 1                                  ; where BASIC vars live   (RAM1 default)
sys_bank         = 0                                      ; where system space is  ???? (RAM0, make this a var?)

sid_speed_flag   !fill 1                                  ; saves system speed during SID ops (used during IRQ)

time                                                      ; temporaries for TI, TI$, SLEEP (4 bytes)
grapnt                                                    ; used by SPRSAV, RMOUSE, RCOLOR
op
column           !fill 1                                  ; temporaries for FIND/CHANGE, [L]INPUT, [L]READ, CURSOR
srow
fstr1            !fill 3                                  ;
fstr2            !fill 3                                  ;


;[[system.stackdata]]

                 * = $00ff

lofbuf           !fill 1
fbuffr           !fill 16                                 ; MathPack builds numbers here, USING, RENUMBER

;  Kernel MAP configurations & DMA lists

                 !fill 16+36                              ; (4 configs + 3 DMA lists)

;  BASIC DMA lists  (2 @ 12bytes each = 24 bytes)

dma1_cmd         !fill 1                                  ; This list is used by BASIC OS
dma1_cnt_lo      !fill 1
dma1_cnt_hi      !fill 1
dma1_src_lo      !fill 1
dma1_src_hi      !fill 1
dma1_src_bank    !fill 1
dma1_dest_lo     !fill 1
dma1_dest_hi     !fill 1
dma1_dest_bank   !fill 1
dma1_subcmd      !fill 1                                  ; (from here on not supported until F018A) [910520] F018A
dma1_mod_lo      !fill 1
dma1_mod_hi      !fill 1

dma2_cmd         !fill 1                                  ; This list is used by DMA command & Graphics
dma2_cnt_lo      !fill 1
dma2_cnt_hi      !fill 1
dma2_src_lo      !fill 1
dma2_src_hi      !fill 1
dma2_src_bank    !fill 1
dma2_dest_lo     !fill 1
dma2_dest_hi     !fill 1
dma2_dest_bank   !fill 1
dma2_subcmd      !fill 1                                  ; (from here on not supported until F018A) [910520] F018A
dma2_mod_lo      !fill 1
dma2_mod_hi      !fill 1

                 !fill 1                                  ; Kernel's dma_byte

sysstk                                                    ; bottom of system stack
stkend           = $1fb                                   ; top of system stack

;[[data.memory]]


                 * = $0200

buflen           = 161                                    ; input buffer size (2 80-column lines + 1)
buf              !fill buflen                             ; BASIC/Monitor line input buffer
buf_txtptr       = buf-1


                 * = $02c0
; BASIC RAM code  (RAM code not needed- following moved to ROM)
;
; chrget *=*+42  ;get byte from text bank after incrementing TXTPTR
; chrgot = chrget+2 ;get byte from text bank at TXTPTR
; qnum  = chrget+27 ;evaluate byte as a number & set flags accordingly

adray1           !fill 2                                  ; ptr to routine:  convert float -> integer ???? why keep
adray2           !fill 2                                  ; ptr to routine:  convert integer -> float ???? why keep
zero             !fill 3                                  ; numeric constant for BASIC, downloaded from ROM

errnum           !fill 1                                  ; used by error trapping routine-last error number
errlin           !fill 2                                  ; line # of last error ($FFFF if no error)
trapno           !fill 2                                  ; line to go to on error ($FFxx if none set)
tmptrp           !fill 1                                  ; hold trap # temporary
errtxt           !fill 2                                  ; pointer to statement causing last error
max_mem_0        !fill 2                                  ; highest address available to BASIC in RAM 0 (text bank)

current_bank     !fill 1                                  ; context for PEEK,POKE,BOOT,SYS,WAIT,BLOAD/SAVE set by BANK.
fin_bank         !fill 1                                  ; bank pointer for string->number conversion routine FIN
tmpdes           !fill 4                                  ; pointers to temporary descriptors for INSTR
bits             !fill 1                                  ; flag for math bit/byte shifter
highlight_color  !fill 1                                  ; color for highlighting text
highlight_save   !fill 1                                  ; saves normal color during highlighting, msb=flag
find_count       !fill 1                                  ; count for LIST to highlight FIND text

; Interrupt stuff

irq_wrap_flag    !fill 1                                  ; used by BASIC_IRQ to block all but one IRQ call
intval           !fill 1                                  ; BASIC interrupts enabled (via collision command)
int_trip_flag    !fill 3                                  ; flags which interrupts occurred
int_adr_lo       !fill 3                                  ; where to go for each type of collision (line number)
int_adr_hi       !fill 3                                  ;
collisions       !fill 2                                  ; sprite collisions, s/s and s/bgnd, recorded during IRQ
lightpen_xpos    !fill 1                                  ; lightpen position recorded during IRQ
lightpen_ypos    !fill 1

; dejavu *=*+1  ;'cold' or 'warm' reset status (must be in page 5!)????

; nmi_wrap_flag *=*+1  ;used by BASIC_NMI to block all but one NMI call [910523]
;    ; (removed)      [910826]
;(leaving 12 bytes)

; BASIC indirect vectors

                 * = $02f7

usrpok           !fill 3                                  ; USR vector (must be set by application)

vectors_begin
iAutoScroll      !fill 2                                  ; AutoScroll vector
esc_fn_vec       !fill 2                                  ; Escape Function vector
graphic_vector   !fill 2                                  ; Graphic Kernel vector (was 'bnkvec')
ierror           !fill 2                                  ; indirect error (output error in .x)
imain            !fill 2                                  ; indirect main (system direct loop)
icrnch           !fill 2                                  ; indirect crunch (tokenization routine)
iqplop           !fill 2                                  ; indirect list (char list)
igone            !fill 2                                  ; indirect gone (char dispatch)
ieval            !fill 2                                  ; indirect eval (symbol evaluation)
iesclk           !fill 2                                  ; escape token crunch
iescpr           !fill 2                                  ; escape token list
iescex           !fill 2                                  ; escape token execute
itime            !fill 2                                  ; 60Hz interrupt vector (before jiffy)
cinv             !fill 2                                  ; IRQ RAM vector
cbinv            !fill 2                                  ; BRK RAM vector

; Remainder of this area reserved for Kernel indirects & Kernel RAM code


                 * = $0400                                ; BASIC's run-time stack (2 pages)
stktop                                                    ; (also used by BOOT SYS and Monitor)
stkbot           = $05ff


                 * = $0600                                ; Sprite definitions (2 pages, must be below $1000)
sprite_base


                 * = $0800
screen_start                                              ; Text display screen
                 * = *+2000

sprite_ptrs_40   = screen_start+$3f8
sprite_ptrs_80   = screen_start+$7f8


                 * = $1170                                ; previous to this used by Kernel

oldlin           !fill 2                                  ; BASIC storage
oldtxt           !fill 2                                  ; BASIC storage
rndx             !fill 5                                  ; Floating Point representation of last random #


; Yet more temporaries shared by various routines

window_temp                                               ; window  (4 bytes)
t3                                                        ; dcat  (1 byte)
renum_tmp_1                                               ; renumber (2 bytes)
tmptxt           !fill 2                                  ; do/loop (2 bytes)

t4                                                        ; dcat  (2 bytes)
renum_tmp_2                                               ; renumber (2 bytes)
tmplin           !fill 2                                  ; do/loop (2 bytes)


;  BASIC/DOS interface vars  (20 bytes)

dosofl           !fill 2                                  ; BLOAD/BSAVE starting addr
dosofh           !fill 2                                  ; BSAVE ending addr
dosla            !fill 1                                  ; DOS logical addr
dosfa            !fill 1                                  ; DOS physical addr
dossa            !fill 1                                  ; DOS secondary addr

xcnt             !fill 1                                  ; DOS loop counter------ this area zeroed-out each DOS call -----
dosf1l           !fill 1                                  ; DOS filename 1 len
dosds1           !fill 1                                  ; DOS disk drive 1
dosf2l           !fill 1                                  ; DOS filename 2 len
dosds2           !fill 1                                  ; DOS disk drive 2
dosf2a           !fill 2                                  ; DOS filename 2 addr
dosrcl           !fill 1                                  ; DOS record length
dosbnk           !fill 1                                  ; DOS load/save bank
dosdid           !fill 2                                  ; DOS ID identifier
dosflags         !fill 1                                  ; DOS flags  7:ID,  6:recover
dossa_temp       !fill 1                                  ; temp storage for file's sa during RECORD command
dosspc           = *-xcnt                                 ; space used by DOS routines-------------------------------------

savram           !fill 67                                 ; buffer used by MOVSPR, SPRDEF, SAVSPR, and DOS parser

xabs             = savram                                 ; movspr_line calculations   [910809]
yabs             = savram+2
xsgn             = savram+4
ysgn             = savram+6
fct              = savram+8
errval           = savram+12


; PRINT USING definitions & storage  (24 bytes)

puchrs                                                    ; Declarations for PRINT USING...
pufill           !fill 1                                  ; print using fill symbol
pucoma           !fill 1                                  ; print using comma symbol
pudot            !fill 1                                  ; print using decimal point symbol
pumony           !fill 1                                  ; print using monetary symbol

bnr              !fill 1                                  ; pointer to begin #
enr              !fill 1                                  ; pointer to end #
dolr             !fill 1                                  ; dollar flag
flag             !fill 1                                  ; comma flag (also used by PLAY)????
swe              !fill 1                                  ; counter
usgn             !fill 1                                  ; sign exponent
uexp             !fill 1                                  ; pointer to exponent
vn               !fill 1                                  ; # of digits before decimal point
chsn             !fill 1                                  ; justify flag
vf               !fill 1                                  ; # of positions before decimal point (field)
nf               !fill 1                                  ; # of positions after decimal point (field)
posp             !fill 1                                  ; +/- flag (field)
fesp             !fill 1                                  ; exponent flag (field)
etof             !fill 1                                  ; switch
cform            !fill 1                                  ; char counter (field)
sno              !fill 1                                  ; sign no
blfd             !fill 1                                  ; blank/star flag
begfd            !fill 1                                  ; pointer to begin of field
lfor             !fill 1                                  ; length of format
endfd            !fill 1                                  ; pointer to end of field


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

vwork                                                     ; graphics & sprite vars
xpos             !fill 2                                  ; current x position
ypos             !fill 2                                  ; current y position
xdest            !fill 2                                  ; x-coordinate destination
ydest            !fill 2                                  ; y-coordinate destination

numcnt           !fill 1                                  ; temp, usually coordinate type
vtemp1           !fill 1                                  ; used by sprite math stuff ????was base page
vtemp2           !fill 1                                  ; ????was base page
vtemp3           !fill 1                                  ; misc. graphic temp storage
vtemp4           !fill 1
vtemp5           !fill 1

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

angsgn           !fill 1                                  ; sign of angle
sinval           !fill 2                                  ; sine of value of angle
cosval           !fill 2                                  ; cosine of value of angle
; angcnt *=*+2  ;temps for angle distance routines


; Sprite stuff

savsiz           !fill 4                                  ; temp work locations for SSHAPE, SPRSAV, MOVSPR_TO
lesser
sprtmp_1         !fill 1                                  ; temp for SPRSAV
greatr
sprtmp_2         !fill 1

sprite_data      !fill 88                                 ; speed/direction tables for 8 sprites, 11 bytes each
;   move ang/dist move line
; offset= 0 b7=0+speed b7=1+speed
;  1 counter  counter lo
;  2 angle sign         hi
;  3,4 delta-X  dir+min/max
;  5,6 delta-Y  fct1
;  7,8 total-X  fct2
;  9,10 total-Y  error

init_as_0        = *-sprite_data-1

; vic_save *=*+21  ;copy of VIC reg's, used to update chip during retrace

; defmod *=*+1  ;for SPRDEF
; lincnt *=*+1  ; "
; sprite_number *=*+1  ; "


; Music stuff driving stereo SIDs, 3 voices each

voices           !fill 12                                 ; Voice counters (activity flags)  [910612] stereo
waveform         !fill 6                                  ; Waveforms for each voice   [910612] stereo

voice            !fill 1                                  ; Play note parameters
octave           !fill 1
sharp            !fill 1
dnote            !fill 1
tempo_rate       !fill 1                                  ; duration of whole note 4/4 time = 24/rate
pitch            !fill 2
ntime            !fill 2

filters1         !fill 4                                  ; Volume & Filter parameters   [910612] was 5
filters2         !fill 4                                  ; [910612] stereo
fltsav           !fill 4                                  ; temps
fltflg           !fill 1                                  ; temp

tonnum           !fill 1                                  ; Tune Envelope stuff
tonval           !fill 3

atktab           !fill 10                                 ; Tune Envelopes
sustab           !fill 10
wavtab           !fill 10
pulslw           !fill 10
pulshi           !fill 10

parcnt           !fill 1                                  ; temp: envelope
nibble           !fill 1                                  ; temp: envelope, filter


; SOUND command stuff

sound_voice      !fill 1
sound_time_lo    !fill 3+3                                ; [910612] stereo
sound_time_hi    !fill 3+3                                ; [910612] stereo
sound_max_lo     !fill 3+3                                ; [910612] stereo
sound_max_hi     !fill 3+3                                ; [910612] stereo
sound_min_lo     !fill 3+3                                ; [910612] stereo
sound_min_hi     !fill 3+3                                ; [910612] stereo
sound_direction  !fill 3+3                                ; [910612] stereo
sound_step_lo    !fill 3+3                                ; [910612] stereo
sound_step_hi    !fill 3+3                                ; [910612] stereo
sound_freq_lo    !fill 3+3                                ; [910612] stereo
sound_freq_hi    !fill 3+3                                ; [910612] stereo

;above must end before $1300
                 * = $1160
;below must end before $1170

temp_time_lo     !fill 1
temp_time_hi     !fill 1
temp_max_lo      !fill 1
temp_max_hi      !fill 1
temp_min_lo      !fill 1
temp_min_hi      !fill 1
temp_direction   !fill 1
temp_step_lo     !fill 1
temp_step_hi     !fill 1
temp_freq_lo     !fill 1
temp_freq_hi     !fill 1
temp_pulse_lo    !fill 1
temp_pulse_hi    !fill 1
temp_waveform    !fill 1

pot_temp_1       !fill 1                                  ; temporaries for 'POT' function
pot_temp_2       !fill 1


                 * = $1300

dosstr           !fill 256                                ; DOS input/output string buffer


                 * = $1f00                                ; Graphics Kernel Interface

GKI__parm1       !fill 1                                  ; ml interface parm values
GKI__parm2       !fill 1
GKI__parm3       !fill 1
GKI__parm4       !fill 1
GKI__parm5       !fill 1
GKI__parm6       !fill 1
GKI__parm7       !fill 1
GKI__parm8       !fill 1
GKI__parm9       !fill 1
GKI__parm10      !fill 1
GKI__parm11      !fill 1
GKI__parm12      !fill 1
GKI__parm13      !fill 1
GKI__parm14      !fill 1
GKI__parm15      !fill 1
GKI__parm16      !fill 1
GKI__parm17      !fill 1

GKI__subparm1    !fill 1                                  ; subroutine parm values
GKI__subparm2    !fill 1
GKI__subparm3    !fill 1
GKI__subparm4    !fill 1
GKI__subparm5    !fill 1

GKI__temp1       !fill 1                                  ; local variables within subroutines
GKI__temp2       !fill 1
GKI__temp3       !fill 1
GKI__temp4       !fill 1
GKI__temp5       !fill 1
GKI__temp6       !fill 1
GKI__temp7       !fill 1
GKI__temp8       !fill 1
GKI__temp9       !fill 1
GKI__temp10      !fill 1
GKI__temp11      !fill 1
GKI__temp12      !fill 1
GKI__temp13      !fill 1
GKI__temp14      !fill 1
GKI__temp15      !fill 1
GKI__temp16      !fill 1
GKI__temp17      !fill 1

;.end


;[[data.kernal]]
; Addresses of OS parameters referenced by BASIC:

_6510_data_reg   = $01
_bank            = $02                                    ; reg's for Kernel xxx_FAR routines (used by SYS)
_pchi            = $03
_pclo            = $04
_s_reg           = $05
_a_reg           = $06
_x_reg           = $07
_y_reg           = $08
_z_reg           = $09

_vicIRQ          = $a0                                    ; VIC IRQ flag register at time of IRQ
_starting_addr   = $ac                                    ; address BLOAD loaded to
_sa              = $b9                                    ; I/O channel secondary address
_fa              = $ba                                    ; I/O channel device number
_ndx             = $d0                                    ; number of characters in keyboard buffer
_kyndx           = $d1                                    ; fkey active flag
_mode            = $d7                                    ; 40/80 mode
_graphm          = $d8                                    ; graphic mode switch (multi/hires/split)
_pnt             = $e0                                    ; Editor screen address at cursor

_screen_bottom   = $e4                                    ; these describe the current window
_screen_top      = $e5
_screen_left     = $e6
_screen_right    = $e7

_color           = $f1                                    ; text color      [910722]
_autoinsert      = $f6                                    ; enable/disable auto insert mode
_locks           = $f7                                    ; Editor keyboard locks     [910722]

_keyd            = $02b0                                  ; keyboard buffer     [910710]
;_split = $0a34  ;line to start split at

number_fkeys     = 16                                     ; max of 14 prog. fn. keys
_pky_lengths     = $1000                                  ; table of prog. fn. key sizes
_pky_buffer      = _pky_lengths+number_fkeys              ; actual buffer

_restart_vector  = $1100                                  ; Kernel restart vector
_pal_ntsc        = $1103                                  ; PAL=$ff, NTSC=$00 indicator    [910107]
_init_status     = $1104                                  ; msb set tells Kernel to let BASIC have IRQs
_default_drive   = $1106                                  ; system default disk drive
_expansion       = $1107                                  ; expansion RAM (# banks????)    [910107]
_sleep_counter   = $110c                                  ; binary frame counter maintained by Kernel  [910730]
_mouse_enable    = $1135                                  ; port# used by mouse (b7=port2, b6=port1, or both) [910107]
_mouse_pointer   = $1136                                  ; sprite pointer (sprite*2) by Kernel mouse driver "
_mouse_top       = $113b                                  ; margins for mouse pointer    "
_mouse_bottom    = $113c                                  ; "
_mouse_left      = $113d                                  ; "
_mouse_right     = $113e                                  ; "

; Addresses of I/O areas referenced by BASIC:

_red             = $d100                                  ; VIC palette (I/O block)
_green           = $d200
_blue            = $d300

; Addresses of Kernel entry points referenced by BASIC:

_print           = $e00c
_mouse           = $e01b                                  ; [910122]
_set_window      = $e02d
_palette_init    = $e027
_cursor          = $e030                                  ; [910228]
;_ldtb2 = $e033
;_ldtb1 = $e04c

_close_all       = $ff50                                  ; close all channels assigned to device .a
_go_64           = $ff53                                  ; C64 mode
_monitor         = $ff56                                  ; ML Monitor
_bootsys         = $ff59                                  ; Boot alternate OS     [910110]
_phoenix         = $ff5c                                  ; jump to 'post-BASIC initialize' routine
_lkupla          = $ff5f                                  ; find an available Logical Address
_lkupsa          = $ff62                                  ; find an available Secondary Address
_swapper         = $ff65                                  ; switch 80/40 column
_doakey          = $ff68                                  ; add/remove a definition from the p.f. key table
_setbank         = $ff6b                                  ; set bank for load/save/verify/open
_jsr_far         = $ff6e                                  ; call a subroutine in any bank
_jmp_far         = $ff71                                  ; jump to code in any bank
_lda_far         = $ff74                                  ; write a byte to any bank
_sta_far         = $ff77                                  ; read a byte from any bank
_cmp_far         = $ff7a                                  ; compare a byte to any bank
_primm           = $ff7d                                  ; print immediate

_setmsg          = $ff90
_readst          = $ffb7
_setlfs          = $ffba
_setnam          = $ffbd
_open            = $ffc0
_close           = $ffc3
_chkin           = $ffc6
_chkout          = $ffc9
_clrch           = $ffcc
_basin           = $ffcf
_bsout           = $ffd2
_loadsp          = $ffd5
_savesp          = $ffd8
_SetTime         = $ffdb
_ReadTime        = $ffde
_stop            = $ffe1
_getin           = $ffe4
_clall           = $ffe7
_screen_org      = $ffed
_plot            = $fff0

;.end



;[[initialise.initialise]]
; ***************************************************************************************************************
; ***************************************************************************************************************
;
;      Name:       patch.asm
;      Purpose:    Fixes
;      Created:    4th January 2020
;      Author:     Paul Robson (paul@robsons.org.uk)
;
; ***************************************************************************************************************
; ***************************************************************************************************************

; ***************************************************************************************************************
;
;				At present ACME does not support BRA opcode $83. BRL replaces this.
;
; ***************************************************************************************************************

!macro lbra addr {
	!byte $83
	!word (addr-*-1) & $FFFF
}

!macro lbcc addr {
	!byte $93
	!word (addr-*-1) & $FFFF
}

!macro lbcs addr {
	!byte $B3
	!word (addr-*-1) & $FFFF
}

!macro lbne addr {
	!byte $D3
	!word (addr-*-1) & $FFFF
}

!macro lbeq addr {
	!byte $F3
	!word (addr-*-1) & $FFFF
}

!macro lbpl addr {
	!byte $13
	!word (addr-*-1) & $FFFF
}

!macro lbmi addr {
	!byte $33
	!word (addr-*-1) & $FFFF
}

!macro lbvs addr {
	!byte $73
	!word (addr-*-1) & $FFFF
}

!macro lbvc addr {
	!byte $53
	!word (addr-*-1) & $FFFF
}
                 * = $2000

basic
                 jmp hard_reset
                 jmp soft_reset
                 jmp basic_irq
                 jmp basic_nmi                            ; (removed)    [910523] audio

soft_reset                                                ; warm start BASIC...
                 jsr release_channels                     ; restore default terminal I/O channels
                 lda #doslfn                              ; restore reserved disk channel
                 sec                                      ; not a real close
                 jsr _close
                 jsr Clear_DS                             ; zap DS$ just in case
; (might have been in Monitor or building DS$)
                 jsr init_sound_sprites                   ; init interrupt & dma stuff   [910523]
                 jsr init_stack                           ; restore stack
                 lda #1
                 tsb _init_status                         ; tell Kernel to give BASIC a call at IRQ time
                 bra go_ready                             ; enable IRQ, print READY, and go MAIN


hard_reset
                 jsr init_vectors                         ; init vectors
                 jsr init_storage                         ; init variables, voices,  & download RAM code
                 jsr signon_message                       ; print initialization message

                 lda #0                                   ; init bank pointers   [900509]
                 sta text_bank
                 sta helper                               ; reset all LIST flags
                 lda #1
                 sta var_bank
                 lda #2
                 sta highlight_color                      ; set highlight color (2=red)
                 ldx #<basic+3
                 stx _restart_vector                      ; point system restart vector at warm start entry
                 jsr init_stack                           ; initialize system stack pointer
                 lda #1
                 tsb _init_status                         ; tell Kernel to give BASIC a call at IRQ time
                 jsr ($8000)                              ; initialize graphics
                 jsr _phoenix                             ; call cartridges, check out expansion card
                 jsr autobootCSG                          ; attempt to boot program from disk

go_ready
                 cli                                      ; enable IRQ
                 +lbra ready


init_storage
                 lda #76                                  ; 'jmp' opcode
                 sta jmper
                 sta usrpok

                 lda #<errguf                             ; init USR vector to 'undef'd function'  [910226] FAB
                 ldy #>errguf
                 sta usrpok+1
                 sty usrpok+2

                 lda #<flpint                             ; ???? why keep
                 ldy #>flpint
                 sta adray1
                 sty adray1+1

                 lda #<givayf                             ; ???? why keep
                 ldy #>givayf
                 sta adray2
                 sty adray2+1

; Download CHRGET (and INDSUB code????) to RAM
;
; ldx #endmov-chrget_pattern
;1$ lda chrget_pattern-1,x
; sta chrget-1,x
; dex
; bne 1$

                 ldx #0
                 stx zero                                 ; zero constant
                 stx zero+1
                 stx zero+2
                 stx bits                                 ; reset bit/byte shifter
                 stx channl                               ; default channels
                 stx runmod                               ; direct mode
                 stx lastpt+1
                 stx autinc                               ; turn off auto increment
                 stx autinc+1
                 stx rndx                                 ; zero-ing MSB will guarantee a legal value
; stx dosfa  ;zero device number     [910429]

                 stx intval                               ; reset all BASIC IRQ stuff
                 stx int_trip_flag                        ; (BASIC IRQ enabled in init_voices)
                 stx int_trip_flag+1
                 stx int_trip_flag+2
                 stx lightpen_xpos
                 stx lightpen_ypos

; stx mvdflg  ;flag '8k graphics screen not allocated'
; stx width  ;init to single-width lines
; stx scalem  ;turn off scaleing
; stx filflg

; inx   ;.x=1 ???? why init stack with $0101 ????
; stx buf-3
; stx buf-4

; ldy #88   ;zero out sprite information area
;2$ sta sprite_data,y ;???? this is done later at init_as_0
; dey
; bpl 2$

; ldx #13
; stx foreground  ;init bit map's fg color to light green
; ldx #1
; stx multicolor_1 ;init mc1 to white
; ldx #2
; stx multicolor_2 ;init mc2 to red
; jsr set_packed_color ;set up packed fg/bg and fg/mc1 bytes

                 ldx _default_drive
                 stx dosfa                                ; init device number to system default   [910429]

                 ldx #$80                                 ; bank 0 with I/O????
                 stx current_bank                         ; set default bank for PEEK,POKE,BOOT,SYS,WAIT,BLOAD/SAVE

                 ldx #tempst
                 stx temppt                               ; init temp descriptor pointer

                 ldx #<baswrk                             ; set up bottom of bank 0 (text area)
                 ldy #>baswrk
                 stx txttab
                 sty txttab+1

                 lda #<varbgn                             ; set up bottom of bank 1 (storage area)
                 ldy #>varbgn
                 sta vartab
                 sty vartab+1

                 lda #<bank_0_top                         ; set up top of bank 0
                 ldy #>bank_0_top
                 sta max_mem_0
                 sty max_mem_0+1

                 lda #<bank_1_top                         ; set up  top of bank 1
                 ldy #>bank_1_top
                 sta max_mem_1
                 sty max_mem_1+1

                 lda #0                                   ; init text input buffer  (these are for autoboot)
                 sta buf
                 dec
                 sta curlin+1                             ; init line pointer
                 ldx #<buf_txtptr                         ; init txtptr
                 ldy #>buf_txtptr
                 stx txtptr
                 sty txtptr+1

; Set up sprite pointers

                 lda #sprite_base/64+7
                 ldy #7
l1_1             bbr7 _mode,l1_2
                 sta sprite_ptrs_40,y                     ; 40 col screen
                 bra l1_3
l1_2             sta sprite_ptrs_80,y                     ; 80 col screen
l1_3             dec
                 dey
                 bpl l1_1

; Zero out sprite movement stuff and some VIC stuff too

                 lda #0
                 ldx #init_as_0
l1_4             sta sprite_data,x
                 dex
                 bpl l1_4

                 jsr init_sound_sprites                   ; init misc. interrupt & dma stuff

; lda #$d0  ;initialize pointers to character ROM
; sta upper_graphic
; lda #$d8
; sta upper_lower

                 +lbra init_text                          ; go to 'new'


init_sound_sprites                                          ; [910523]
;; init_voices   ;Initialize music stuff
; bit _pal_ntsc  ;determine if PAL or NTSC system  [910724]
; bmi 1$   ;...branch if PAL
; lda #<beats_ntsc/4 ;set beat to quarter note (4/4 time = .5 sec)
; ldy #>beats_ntsc/4
; bra 2$
;1$ lda #<beats_pal/4
; ldy #>beats_pal/4
;2$ sta ntime
; sty ntime+1
;
; lda #4   ;set default octave
; sta octave
;
; lda #12   ;set default tempo    [910220]
; sta tempo_rate  ; 12 makes whole note in 4/4 time last 2 seconds
;----
;; jsr go_slow  ;      [910716] 4567R7A
; lda #0   ;make sure all gates are off
; sta sid1+4
; sta sid1+11
; sta sid1+18
; sta sid2+4
; sta sid2+11
; sta sid2+18
; sta filters1+2  ;set filters off, volume to max????  [910612]
; sta filters2+2
;
; lda #8
; sta sid1+24
; sta sid2+24
; sta filters1+3
; sta filters2+3  ;      [910612]
;; sta filters+4  ;why?      [910612]
;; jsr go_fast  ;      [910716] 4567R7A
;----
; ldy #29   ;initialize music tables
;10$ lda atkmus,y
; sta atktab,y
; dey
; bpl 10$
;
; ldx #9   ;initialize pulse widths
;20$ lda pwhmus,x
; sta pulshi,x
; dex
; bpl 20$
;
; stx sound_time_hi ;turn all SOUND counters off (.X = $ff)
; stx sound_time_hi+1
; stx sound_time_hi+2
; stx sound_time_hi+3 ;stereo SIDs     [910612]
; stx sound_time_hi+4
; stx sound_time_hi+5
; stx voices+1  ;turn all PLAY counters off
; stx voices+3
; stx voices+5
; stx voices+7  ;stereo SIDs     [910612]
; stx voices+9
; stx voices+11
;
; ldy #6-1  ;set default envelope (piano) for all voices (6)
; sty voice
;30$ ldx #0
; jsr set_envelope_1
; dec voice
; bpl 30$
; inc voice  ;set default voice (0)
;-----
                 jsr Sound_CLR_1                          ; [910724]

                 lda #%11100111                           ; [910626]
                 trb helper                               ; reset LIST/HELP/FIND flags
                 tsb highlight_save                       ; mark saved color as invalid

                 lda #0                                   ; [910523] F018A
                 ldx #12+12-1                             ; init DMA lists
l2_1             sta dma1_cmd,x
                 dex
                 bpl l2_1

; stop_sprites   ;Stop all moving sprites (a=0)   [910523]
                 ldy #7                                   ; for sprites 0...7
l2_2             ldx sproff,y                             ; get table offset
                 sta sprite_data,x                        ; reset speed for this sprite
                 dey
                 bpl l2_2                                 ; loop until done

                 sta vic+21                               ; Turn off all sprites    [910717]

                 sta irq_wrap_flag                        ; enable BASIC IRQ handler
; sta nmi_wrap_flag ;enable BASIC NMI handler   [910523]
                 rts                                      ; (removed)    [910826]


signon_message
l3_1             jsr _primm
                 !text 147,18,028,"                     ",146,169
                 !text 5,9,"       THE COMMODORE C65 DEVELOPMENT SYSTEM",cr
                 !text 18,150,"                  ",146,169,cr
                 !text 18,158,"               ",146,169
                 !text 5,9,"   COPYRIGHT  1991  COMMODORE ELECTRONICS, LTD.",cr
                 !text 18,030,"            ",146,169
                 !text 5,9,9,9,"    COPYRIGHT  1977  MICROSOFT",cr
                 !text 18,154,"          ",146,169,cr
                 !text 18,156,"        ",146,169
                 !text 5,9,9," BASIC 10.0   V0.9B.911119    ALL RIGHTS RESERVED",cr,0

                 rts


init_vectors
                 ldx #l4_3-l4_2-1
l4_1             lda l4_2,x
                 sta vectors_begin,x
                 dex
                 bpl l4_1

                 rts


l4_2             !word AutoScroll                         ; autoscroll vector
                 !word n_esc_fn_vec                       ; escape function vector
                 !word graphic_kernel                     ; graphic extension vector
                 !word nerror,nmain,ncrnch,nqplop,ngone,neval ; traditional vectors
                 !word nesclk,nescpr,nescex               ; escape command vectors
l4_3


;; CHRGET/CHRGOT code.  It is downloaded to RAM.
;;
;chrget_pattern
; inw txtptr ;CHRGET entry
; phz  ;CHRGOT entry (chrget+2)
; phx
; lda #0
; ldx #$f0
; ldy #0
; ldz #$f0
; map
; ldy #0
; lda (txtptr),y
; phy
; pha
; jsr _restore_sys
; nop  ;unmap
; pla
; ply
; plx
; plz
;
; cmp #':' ;QNUM entry (chrget+27)
; bcs l4_2
; cmp #' '
; beq chrget_pattern
; sec
; sbc #'0'
; sec
; sbc #$d0
;l4_2 rts  ;(42 bytes to here)
;
;
;
;; Constants which must be moved to RAM
;
; .byte   0,0,0 ;zero, of course!
;
;endmov   ;(45 bytes to here)


; CHRGET/CHRGOT code.
;

chrget           inw txtptr                               ; get next character from text
chrgot           ldy #0                                   ; re-get current character from text
                 jsr indtxt                               ; lda (txtptr),y from RAM0
qnum             cmp #' '
                 beq chrget                               ; skip spaces
chrtst           cmp #':'                                 ; [910513]
                 bcs l5_1                                 ; eol
                 sec
                 sbc #'0'                                 ; alpha or numeric?
                 sec
                 sbc #$d0
l5_1             rts


;.end
;[[system.indirection]]

; C65 BASIC Indirect Load Subroutines


inddef
                 lda #defpnt
                 bra lda_far_ram1

indfrm
                 lda #form
                 bra lda_far_ram1

inddpt
                 lda #dscpnt
                 bra lda_far_ram1

;indhtr
; lda #hightr
; bra lda_far_ram0

indhtr_ram1
                 lda #hightr
                 bra lda_far_ram1

indfmo
                 lda #facmo
                 bra lda_far_ram1

indlow
                 lda #lowtr
                 bra lda_far_ram0

indst1
                 lda #strng1
                 bra lda_far_ram0

indst1_ram1
                 lda #strng1
                 bra lda_far_ram1

indgrb
                 lda #grbpnt
                 bra lda_far_ram1

indlow_ram1
                 lda #lowtr
                 bra lda_far_ram1

indin1
                 lda #index1
                 bra lda_far_ram0

;indin2
; lda #index2
; bra lda_far_ram0

indtxt
                 lda #txtptr
; bra lda_far_ram0


; C65 BASIC Indirect Load Subroutines

lda_far_ram0
                 phz                                      ; save registers
                 phx
                 tax                                      ; pointer
                 ldz text_bank                            ; RAM0
                 jsr _lda_far                             ; LDA (.x),Y from bank .z
                 plx
                 plz
                 and #$ff                                 ; set processor status per byte fetched
                 rts



indin1_ram1
                 lda #index1
; bra lda_far_ram1

lda_far_ram1
                 php                                      ; save .c
                 phz                                      ; save registers
                 phx
                 tax                                      ; pointer
                 ldz var_bank                             ; RAM1
                 lda 1,x                                  ; check to see if pointer points to "common"
                 cmp #$20
                 bcs l6_1                                 ; branch if not
                 ldz text_bank                            ; else select RAM0

l6_1             jsr _lda_far                             ; LDA (.x),Y from bank .z
                 plx
                 plz
                 plp                                      ; restore .c
                 and #$ff                                 ; set processor status per byte fetched
                 rts


; C65 BASIC Indirect Save Subroutines

sta_far_ram1
                 php                                      ; save registers
                 phz
                 pha
                 ldz var_bank                             ; RAM1
                 lda 1,x                                  ; check to see if pointer points to "common"
                 cmp #$20
                 bcs l7_1                                 ; branch if not
                 ldz text_bank                            ; else select RAM0

l7_1             pla
                 jsr _sta_far                             ; STA (.x),Y to bank .z
                 plz
                 plp
                 rts


sta_far_in1                                               ; [910624]
                 ldx #index1
                 bra sta_far_ram0

sta_far_txt
                 ldx #txtptr

sta_far_ram0
                 php                                      ; save registers
                 phz
                 ldz text_bank                            ; RAM0
                 jsr _sta_far                             ; STA (.x),Y to bank .z
                 plz
                 plp
                 rts


indcmp_in1                                                ; [910620]
                 ldx #index1
                 ldz text_bank                            ; RAM0
                 jmp _cmp_far                             ; STA (.x),Y to bank .z

;.end



;[[system.tokeniser]]
;        CRUNCH
;
;  Entry:  TXTPTR points to start of text to crunch
;  Exit:   TXTPTR points to start of crunched text
;
;  Calls:  CHRGET
;          CHRGOT
;          RESER
;          KLOOP
;          REM
;          DATA
;
;  CRUNCH collapses all reserved words into tokens.  It removes all graphic
;  characters (characters with msb set) not in quoted strings, DATA or REM
;  statements.
;
;  An escape token is implemented as follows:
;
; As each character on a line of text to be crunched is scanned, an
; indirect jump is performed.  Anyone wishing to scan for their own
; commands should grab off this vector, saving the return vector.
; On entry, if the carry flag is set, it is still up for grabs.
; The current text pointer is at TXTPTR.  If the escape routine
; recognizes the command, it should:
;
;  ) put the length of the reserved word in .y
;  ) put the desired 'second' token in .a
;  ) clear the carry flag
;  ) put type of token in x: 0==>command, ff==>function
;
; If it is not your command, leave .a and the carry flag intact.
; NOTE:  The reserved word must be >= 2 characters long.  Exit through
; the old vector (for daisy chaining).  If the carry flag is clear on
; entry it means someone else before you recognized this command.  In
; this case, just pass control through the old vector.


crunch           jmp (icrnch)


ncrnch           phw txtptr                               ; save old text pointer

crun05           jsr chrgot
                 bra crun20

crun10           jsr chrget


crun20           bcc crun10                               ; don't crunch numbers
                 jmp (iesclk)                             ; give others a chance at this.  (carry is set)

nesclk
                 +lbcc l8_12                              ; carry clear if someone wanted it
                 cmp #0                                   ; end of line?
                 beq l8_10                                ; yes
                 cmp #':'                                 ; multi-stmt char?
                 beq crun10                               ; yes
                 cmp #'?'                                 ; print ('?') abreviation?
                 bne l8_1                                 ; no
                 lda #print_token                         ; yes- substitute print token
                 bra l8_8

l8_1             cmp #$80                                 ; graphics?
                 bcc l8_2                                 ; no
                 cmp #pi                                  ; pi? (special case)
                 beq crun10                               ; yes, leave alone
                 ldy #1
                 jsr kloop                                ; crunch out graphics
                 bra crun05


l8_2             cmp #'"'                                 ; quote string?
                 bne l8_4                                 ; no- try escape token

l8_3             jsr chrget
                 cmp #0                                   ; end of line?
                 beq l8_10                                ; yes
                 cmp #'"'                                 ; close quote?
                 beq crun10                               ; yes
                 bra l8_3                                 ; no, continue skipping characters


; Crunch escape token

l8_4             lda #>esc_command_list                   ; look for token in escape-command list
                 ldy #<esc_command_list
                 jsr reser
                 bcc l8_5                                 ; not found
                 lda #first_esc_command_token+$80-1       ; set up for common escape routine
                 ldx #0                                   ; ..flag 'cmd' type escape
                 bra l8_11                                ; ..and go to it.

l8_5             lda #>esc_function_list                  ; look for token in escape-function list
                 ldy #<esc_function_list
                 jsr reser
                 bcc l8_6                                 ; not found
                 lda #first_esc_function_token+$80-1      ; set up for common escape routine
                 ldx #$ff                                 ; ..flag 'function' type escape
                 bra l8_11                                ; ..and go to it

l8_6             lda #>keyword_list                       ; look for token in normal list
                 ldy #<keyword_list
                 jsr reser
                 bcc crun10                               ; not found
                 cpy #0                                   ; anything to move?
                 beq l8_7                                 ; no
                 jsr kloop                                ; crunch it out
l8_7             lda count

l8_8             ldy #0
                 jsr sta_far_txt                          ; put token into text  (bleed-thru)
                 cmp #rem_token
                 beq l8_9
                 cmp #data_token
                 bne crun10
                 jsr chrget
                 jsr data
                 +lbra crun05

l8_9             jsr chrget
                 jsr rem


;  No other statements can follow a REM

l8_10            ldx txtptr
                 pla
                 sta txtptr+1
                 pla
                 sta txtptr
                 sec                                      ; compute length of line
                 txa
                 sbc txtptr
                 tay
                 iny
                 rts


; Crunch out old text, install an escape token

l8_11            adc count                                ; make pointer into a token
l8_12            pha                                      ; save second token
                 dey                                      ; waste (# of chars) - 1
                 jsr kloop

; See if this is function (x=ff) or command (x=0)

                 lda #esc_command_token                   ; assume command
                 inx
                 bne l8_13                                ; branch if command
                 lda #esc_function_token                  ; ..else get correct token

l8_13            ldy #0
                 jsr sta_far_txt                          ; install escape token... (bleed-thru)
                 iny
                 pla
                 jsr sta_far_txt                          ; ..and second token  (bleed-thru)
                 jsr chrget                               ; skip over token,
                 +lbra crun10                             ; ..and continue with line.


;      KLOOP
;
;  Crunch loop.  Moves offset .y characters from txtptr to end of line.
;  .x is preserved

kloop            clc                                      ; compute source address
                 tya
                 adc txtptr
                 sta index1
                 lda txtptr+1
                 adc #0
                 sta index1+1
                 ldy #$ff

l9_1             iny
                 lda (index1),y                           ; move source..  ????assumes text in common area
                 sta (txtptr),y                           ; to destination offset ????assumes text in common area
                 bne l9_1                                 ; not end of line
                 rts


;      RESER
;
;  Search reserved word list for a match
;
;  Entry:  (txtptr) is first char of word to match
;    (y,a) is start of table to check
;
;  Exit:   .y  length of word matched
;    .c  success/fail (set/clear) flag
;    count token value

reser            sta index1+1
                 sty index1
                 ldy #0
                 sty count
                 dey
l10_1            iny
l10_2            lda (txtptr),y                           ; assumes common memory
                 bmi l10_7                                ; abrieviation    [900510]
                 sec
                 sbc (index1),y                           ; does letter match? (ind.ok)
                 beq l10_1                                ; yes...continue
                 cmp #$80                                 ; end of word?
                 beq l10_6                                ; yes...c set...done


;  find next word

l10_3            lda (index1),y                           ; ind.ok
                 bmi l10_4                                ; found end of current
                 iny
                 bne l10_3
l10_4            iny                                      ; start of next
                 inc count                                ; value of token
                 clc
                 tya
                 adc index1
                 sta index1
                 bcc l10_5
                 inc index1+1
l10_5            clc
                 ldy #0
                 lda (index1),y                           ; end of list? ind.ok
                 bne l10_2                                ; no


;  yes...carry clear...fail

l10_6            ora count                                ; .a=$80 if match
                 sta count                                ; token is formed
                 rts


; special case- last character is shifted (necessary for 'diR' compatibility)

l10_7            sec                                      ; allow last chr to be shifted   [900510]
                 sbc (index1),y                           ; does letter match? (ind.ok)
                 beq l10_8                                ; yes- end of word
                 cmp #$80                                 ; end of word?
                 beq l10_6                                ; yes
                 bne l10_3                                ; no- next word

l10_8            lda #$80                                 ; last chr is shifted & so is end of current word
                 bra l10_6

;.end



;[[tokeniser.keywords]]

keyword_list
                 !text "EN",'D'+$80                       ; $80
                 !text "FO",'R'+$80                       ; $81
                 !text "NEX",'T'+$80                      ; $82
                 !text "DAT",'A'+$80                      ; $83
                 !text "INPUT",'#'+$80                    ; $84
                 !text "INPU",'T'+$80                     ; $85
                 !text "DI",'M'+$80                       ; $86
                 !text "REA",'D'+$80                      ; $87
                 !text "LE",'T'+$80                       ; $88
                 !text "GOT",'O'+$80                      ; $89
                 !text "RU",'N'+$80                       ; $8A
                 !text "I",'F'+$80                        ; $8B
                 !text "RESTOR",'E'+$80                   ; $8C
                 !text "GOSU",'B'+$80                     ; $8D
                 !text "RETUR",'N'+$80                    ; $8E
                 !text "RE",'M'+$80                       ; $8F
                 !text "STO",'P'+$80                      ; $90
                 !text "O",'N'+$80                        ; $91
                 !text "WAI",'T'+$80                      ; $92
                 !text "LOA",'D'+$80                      ; $93
                 !text "SAV",'E'+$80                      ; $94
                 !text "VERIF",'Y'+$80                    ; $95
                 !text "DE",'F'+$80                       ; $96
                 !text "POK",'E'+$80                      ; $97
                 !text "PRINT",'#'+$80                    ; $98
                 !text "PRIN",'T'+$80                     ; $99
                 !text "CON",'T'+$80                      ; $9A
                 !text "LIS",'T'+$80                      ; $9B
                 !text "CL",'R'+$80                       ; $9C
                 !text "CM",'D'+$80                       ; $9D
                 !text "SY",'S'+$80                       ; $9E
                 !text "OPE",'N'+$80                      ; $9F
                 !text "CLOS",'E'+$80                     ; $A0
                 !text "GE",'T'+$80                       ; $A1
                 !text "NE",'W'+$80                       ; $A2
                 !text "TAB",'('+$80                      ; $A3
                 !text "T",'O'+$80                        ; $A4
                 !text "F",'N'+$80                        ; $A5
                 !text "SPC",'('+$80                      ; $A6
                 !text "THE",'N'+$80                      ; $A7
                 !text "NO",'T'+$80                       ; $A8
                 !text "STE",'P'+$80                      ; $A9
                 !text '+'+$80                            ; $AA operators
                 !text '-'+$80                            ; $AB
                 !text '*'+$80                            ; $AC
                 !text '/'+$80                            ; $AD
                 !text '^'+$80                            ; $AE
                 !text "AN",'D'+$80                       ; $AF
                 !text "O",'R'+$80                        ; $B0
                 !text '>'+$80                            ; $B1
                 !text '='+$80                            ; $B2
                 !text '<'+$80                            ; $B3
                 !text "SG",'N'+$80                       ; $B4 first numeric function
                 !text "IN",'T'+$80                       ; $B5
                 !text "AB",'S'+$80                       ; $B6
                 !text "US",'R'+$80                       ; $B7
                 !text "FR",'E'+$80                       ; $B8
                 !text "PO",'S'+$80                       ; $B9
                 !text "SQ",'R'+$80                       ; $BA
                 !text "RN",'D'+$80                       ; $BB
                 !text "LO",'G'+$80                       ; $BC
                 !text "EX",'P'+$80                       ; $BD
                 !text "CO",'S'+$80                       ; $BE
                 !text "SI",'N'+$80                       ; $BF
                 !text "TA",'N'+$80                       ; $C0
                 !text "AT",'N'+$80                       ; $C1
                 !text "PEE",'K'+$80                      ; $C2
                 !text "LE",'N'+$80                       ; $C3
                 !text "STR",'$'+$80                      ; $C4
                 !text "VA",'L'+$80                       ; $C5
                 !text "AS",'C'+$80                       ; $C6 last numeric function
                 !text "CHR",'$'+$80                      ; $C7 last single-arg function
                 !text "LEFT",'$'+$80                     ; $C8
                 !text "RIGHT",'$'+$80                    ; $C9
                 !text "MID",'$'+$80                      ; $CA
                 !text "G",'O'+$80                        ; $CB
; beginning of new C128 keywords------------
                 !text "RGRAPHI",'C'+$80                  ; $CC was 'rgr'   [910701]
                 !text "RCOLO",'R'+$80                    ; $CD was 'rclr'   [910701]
                 !text $80                                ; $CE null to skip over escape_function token
                 !text "JO",'Y'+$80                       ; $CF
                 !text "RPE",'N'+$80                      ; $D0 (was rdot in c128)
                 !text "DE",'C'+$80                       ; $D1
                 !text "HEX",'$'+$80                      ; $D2
                 !text "ERR",'$'+$80                      ; $D3
                 !text "INST",'R'+$80                     ; $D4 last function

                 !text "ELS",'E'+$80                      ; $D5
                 !text "RESUM",'E'+$80                    ; $D6
                 !text "TRA",'P'+$80                      ; $D7
                 !text "TRO",'N'+$80                      ; $D8
                 !text "TROF",'F'+$80                     ; $D9
                 !text "SOUN",'D'+$80                     ; $DA
                 !text "VO",'L'+$80                       ; $DB
                 !text "AUT",'O'+$80                      ; $DC
                 !text "PUDE",'F'+$80                     ; $DD
                 !text "GRAPHI",'C'+$80                   ; $DE
                 !text "PAIN",'T'+$80                     ; $DF
                 !text "CHA",'R'+$80                      ; $E0
                 !text "BO",'X'+$80                       ; $E1
                 !text "CIRCL",'E'+$80                    ; $E2
                 !text "PAST",'E'+$80                     ; $E3 (was gshape in C128)
                 !text "CU",'T'+$80                       ; $E4 (was sshape in C128)
                 !text "LIN",'E'+$80                      ; $E5 (was draw in C128)
                 !text "LOCAT",'E'+$80                    ; $E6
                 !text "COLO",'R'+$80                     ; $E7
                 !text "SCNCL",'R'+$80                    ; $E8
                 !text "SCAL",'E'+$80                     ; $E9
                 !text "HEL",'P'+$80                      ; $EA
                 !text "D",'O'+$80                        ; $EB
                 !text "LOO",'P'+$80                      ; $EC
                 !text "EXI",'T'+$80                      ; $ED
                 !text "DI",'R'+$80                       ; $EE
                 !text "DSAV",'E'+$80                     ; $EF
                 !text "DLOA",'D'+$80                     ; $F0
                 !text "HEADE",'R'+$80                    ; $F1
                 !text "SCRATC",'H'+$80                   ; $F2
                 !text "COLLEC",'T'+$80                   ; $F3
                 !text "COP",'Y'+$80                      ; $F4
                 !text "RENAM",'E'+$80                    ; $F5
                 !text "BACKU",'P'+$80                    ; $F6
                 !text "DELET",'E'+$80                    ; $F7
                 !text "RENUMBE",'R'+$80                  ; $F8
                 !text "KE",'Y'+$80                       ; $F9
                 !text "MONITO",'R'+$80                   ; $FA
                 !text "USIN",'G'+$80                     ; $FB
                 !text "UNTI",'L'+$80                     ; $FC
                 !text "WHIL",'E'+$80                     ; $FD
                 !text 0                                  ; $FE skip over the escape_command token

;.end




;[[tokeniser.keyword.esc]]
; Escape Command Tokens

esc_command_list
                 !text "BAN",'K'+$80                      ; $02: set bank number
                 !text "FILTE",'R'+$80                    ; $03: set up filter
                 !text "PLA",'Y'+$80                      ; $04: play a tune
                 !text "TEMP",'O'+$80                     ; $05: set rate for playing
                 !text "MOVSP",'R'+$80                    ; $06: sprite position/movement
                 !text "SPRIT",'E'+$80                    ; $07: turn on/set up sprite
                 !text "SPRCOLO",'R'+$80                  ; $08: set sprite multicolor registers
                 !text "RRE",'G'+$80                      ; $09: retreive register values after 'SYS'
                 !text "ENVELOP",'E'+$80                  ; $0A: set up SID envelopes
                 !text "SLEE",'P'+$80                     ; $0B: delay
                 !text "CATALO",'G'+$80                   ; $0C: disk directory
                 !text "DOPE",'N'+$80                     ; $0D: open a disk file
                 !text "APPEN",'D'+$80                    ; $0E: open a disk file for appending
                 !text "DCLOS",'E'+$80                    ; $0F: close a file opened w/ DOPEN
                 !text "BSAV",'E'+$80                     ; $10: binary (non-program) save
                 !text "BLOA",'D'+$80                     ; $11: binary load
                 !text "RECOR",'D'+$80                    ; $12:
                 !text "CONCA",'T'+$80                    ; $13: concatenate 2 files
                 !text "DVERIF",'Y'+$80                   ; $14: verify a saved program
                 !text "DCLEA",'R'+$80                    ; $15: re-initialize a drive
                 !text "SPRSA",'V'+$80                    ; $16: sprite/string to sprite/string
                 !text "COLLISIO",'N'+$80                 ; $17: set traps for sprite & light pen collisions
                 !text "BEGI",'N'+$80                     ; $18: mark start of a b-block
                 !text "BEN",'D'+$80                      ; $19: ..and its end, too!
                 !text "WINDO",'W'+$80                    ; $1A: set screen window
                 !text "BOO",'T'+$80                      ; $1B: load&run ML or autoboot a disk
                 !text "WIDT",'H'+$80                     ; $1C: single/double width drawing
                 !text "SPRDE",'F'+$80                    ; $1D: define a sprite
                 !text "QUI",'T'+$80                      ; $1E: (UNIMPLEMENTED)
                 !text "DM",'A'+$80                       ; $1F: access memory
                 !text ' '+$80                            ; $20: POISON - space character
                 !text "DM",'A'+$80                       ; $21: access memory
                 !text ' '+$80                            ; $22: POISON - quote character
                 !text "DM",'A'+$80                       ; $23: access memory
                 !text "OF",'F'+$80                       ; $24: KEY OFF
                 !text "FAS",'T'+$80                      ; $25: go to 2 MHz. mode
                 !text "SLO",'W'+$80                      ; $26: go to 1 MHz. mode
                 !text "TYP",'E'+$80                      ; $27: type SEQ file
                 !text "BVERIF",'Y'+$80                   ; $28: verify a saved program
                 !text "ECTOR",'Y'+$80                    ; $29: dirECTORY
                 !text "ERAS",'E'+$80                     ; $2A: alias for scratch
                 !text "FIN",'D'+$80                      ; $2B: find string
                 !text "CHANG",'E'+$80                    ; $2C: change string
                 !text "SE",'T'+$80                       ; $2D:
                 !text "SCREE",'N'+$80                    ; $2E:
                 !text "POLYGO",'N'+$80                   ; $2F:
                 !text "ELLIPS",'E'+$80                   ; $30:
                 !text "VIEWPOR",'T'+$80                  ; $31:
                 !text "GCOP",'Y'+$80                     ; $32:
                 !text "PE",'N'+$80                       ; $33:
                 !text "PALETT",'E'+$80                   ; $34:
                 !text "DMOD",'E'+$80                     ; $35:
                 !text "DPA",'T'+$80                      ; $36:
                 !text "FORMA",'T'+$80                    ; $37: alias for HEADER command  [911017]
                 !text "GENLOC",'K'+$80                   ; $38:     [910108]
                 !text "FOREGROUN",'D'+$80                ; $39:     [910109]
                 !text ' '+$80                            ; $3A: POISON - colon character  "
                 !text "BACKGROUN",'D'+$80                ; $3B:     "
                 !text "BORDE",'R'+$80                    ; $3C:     "
                 !text "HIGHLIGH",'T'+$80                 ; $3D:     "
                 !text "MOUS",'E'+$80                     ; $3E:     [910122]
                 !text "RMOUS",'E'+$80                    ; $3F: return coordinates of mouse [910123]
                 !text "DIS",'K'+$80                      ; $40:     [910123]
                 !text "CURSO",'R'+$80                    ; $41:     [910228]
                 !text "RCURSO",'R'+$80                   ; $42: return cursor position  [910228]
                 !text "LOADIF",'F'+$80                   ; $43: load IFF picture from disk [910402]
                 !text "SAVEIF",'F'+$80                   ; $44: save IFF picture to   disk [910402]
                 !text "EDI",'T'+$80                      ; $45: Edit mode on/off   [910620]

                 !text 0                                  ; End marker
;(don't forget to change last_command_token!)

; Escape Function Tokens

esc_function_list
                 !text "PO",'T'+$80                       ; $02: return paddle value
                 !text "BUM",'P'+$80                      ; $03: read sprite collision
                 !text "LPE",'N'+$80                      ; $04: read light pen value
                 !text "RSPPO",'S'+$80                    ; $05: read sprite position
                 !text "RSPRIT",'E'+$80                   ; $06: read sprite value
                 !text "RSPCOLO",'R'+$80                  ; $07: read sprite multicolor value
                 !text "XO",'R'+$80                       ; $08: exclusive or
                 !text "RWINDO",'W'+$80                   ; $09: read window size
                 !text "POINTE",'R'+$80                   ; $0a: return address of descriptor
                 !text "MO",'D'+$80                       ; $0b: modulus    [910402]
                 !text "PIXE",'L'+$80                     ; $0c: return BP data at pixel  [910820]
                 !text "RPALETT",'E'+$80                  ; $0d: return RGB component of color [910820]
                 !text 0

;.end
;[[tokeniser.vectors]]


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
                 !word C65__paste-1                       ; gshape
                 !word C65__cut-1                         ; sshape
                 !word C65__line-1                        ; draw

                 !word bad_command-1                      ; escape - SYSTEM - unimplemented command
; .word  locate-1

                 !word color-1
                 !word scnclr-1

                 !word bad_command-1                      ; escape - SYSTEM - unimplemented command
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
                 !word bank-1                             ; escape
                 !word filter-1                           ; escape
                 !word play-1                             ; escape
                 !word tempo-1                            ; escape

                 !word movspr-1                           ; escape
                 !word sprite-1                           ; escape
                 !word sprcolor-1                         ; escape

                 !word rreg-1                             ; escape
                 !word envelope-1                         ; escape
                 !word sleep-1                            ; escape
                 !word directory-1                        ; escape
                 !word dopen-1                            ; escape
                 !word append-1                           ; escape
                 !word dclose-1                           ; escape
                 !word bsave-1                            ; escape
                 !word bload-1                            ; escape
                 !word record-1                           ; escape
                 !word concat-1                           ; escape
                 !word dverify-1                          ; escape
                 !word dclear-1                           ; escape

                 !word sprsav-1                           ; escape
                 !word collision-1                        ; escape

                 !word data-1                             ; escape - BEGIN
                 !word data-1                             ; escape - BEND
                 !word window-1                           ; escape
                 !word boot-1                             ; escape

                 !word bad_command-1
; .word  set_width-1 ;escape - WIDTH

                 !word bad_command-1
; .word  sprdef-1  ;escape - Sprite Definition mode

                 !word bad_command-1                      ; escape - QUIT - unimplemented command
                 !word dma-1                              ; escape
                 !word 0                                  ; placeholder to skip over the space character
                 !word dma-1                              ; escape
                 !word 0                                  ; placeholder to skip over the quote character
                 !word dma-1                              ; escape
                 !word bad_command-1                      ; escape - OFF - unimplemented command
                 !word fast-1                             ; escape
                 !word slow-1                             ; escape
                 !word type-1                             ; escape (C65: type SEQ file)
                 !word bverify-1                          ; escape (C65: verify BINary file)
                 !word snerr-1                            ; escape (C65: kludge- dirECTORY)
                 !word scratch-1                          ; escape (C65: erase alias for scratch)
                 !word find-1                             ; escape (C65: find BASIC text)
                 !word change-1                           ; escape (C65: change BASIC text)

                 !word C65__set-1                         ; escape (C65: multi-purpose command)
                 !word Screen-1                           ; escape (C65: SCREEN)
                 !word C65__polygon-1                     ; escape (C65: POLYGON)
                 !word C65__ellipse-1                     ; escape (C65: ELLIPSE)
                 !word C65__Viewport-1                    ; escape (C65: VIEWPORT)
                 !word C65__copy-1                        ; escape (C65: GCOPY)
                 !word C65__setpen-1                      ; escape (C65: PEN)
                 !word C65__setpalette-1                  ; escape (C65: PALETTE)
                 !word C65__setdmode-1                    ; escape (C65: DMODE)
                 !word C65__setdpat-1                     ; escape (C65: DPAT)
                 !word header-1                           ; format alias for header command [911017]
                 !word genlock-1                          ; [910108]

stmdsp2
                 !word foreground-1                       ; this is the 128th command!  [910109]
                 !word 0                                  ; placeholder to skip over the colon character
                 !word background-1
                 !word border-1
                 !word highlight-1
                 !word mouse-1                            ; [910122]
                 !word rmouse-1                           ; [910123]
                 !word disk-1                             ; [910123]
                 !word cursor-1                           ; [910228]
                 !word rcursor-1                          ; [910228]
                 !word loadiff-1                          ; [910402]
                 !word saveiff-1                          ; [910930]
                 !word edit-1                             ; [910620]


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
                 !word rgraphic                           ; [910701]
                 !word rcolor                             ; [910701]
                 !word 0                                  ; placeholder for escape function token
                 !word joy
                 !word rpen                               ; was rdot     [910820]
                 !word dcml                               ; dec
                 !word hexd
                 !word errd
                 !word pot                                ; escape
                 !word bump                               ; escape
                 !word lpen                               ; escape
                 !word rsppos                             ; escape
                 !word rsprite                            ; escape
                 !word rspcolor                           ; escape
                 !word xor                                ; escape
                 !word rwindow                            ; escape
                 !word pointer                            ; escape
                 !word mod                                ; escape c65     [910402]
                 !word pixel                              ; escape c65     [910820]
                 !word rpalette                           ; escape c65     [910820]


optab            !text 121
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
negtab           !text 125
                 !word negop-1
                 !text 90
                 !word notop-1
ptdorl           !text 100
                 !word dorel-1

;.end

;[[tokeniser.const]]


end_token        = $80                                    ; v2 commands
for_token        = $81
next_token       = $82
data_token       = $83
input_token      = $84
goto_token       = $89
run_token        = $8a
restore_token    = $8c
gosub_token      = $8d
rem_token        = $8f
on_token         = $91
load_token       = $93
save_token       = $94
verify_token     = $95
def_token        = $96
print_token      = $99
clr_token        = $9c
sys_token        = $9e
open_token       = $9f
close_token      = $a0
new_token        = $a2
tab_token        = $a3
to_token         = $a4
fn_token         = $a5
spc_token        = $a6
then_token       = $a7
not_token        = $a8
step_token       = $a9
plus_token       = $aa                                    ; operators
minus_token      = $ab
greater_token    = $b1
equal_token      = $b2
less_token       = $b3
first_function_token = $b4                                    ; v2 functions
left_token       = $c8
mid_token        = $ca
go_token         = $cb                                    ; kludges
rgraphic_token   = $cc                                    ; first new v7 token
esc_function_token = $ce
err_token        = $d3
instr_token      = $d4
last_function_token = $d4
else_token       = $d5
resume_token     = $d6
trap_token       = $d7
color_token      = $e7
do_token         = $eb
loop_token       = $ec
key_token        = $f9
monitor_token    = $fa
using_token      = $fb
until_token      = $fc
while_token      = $fd
esc_command_token = $fe

first_esc_command_token = $02
collision_token  = $17
begin_token      = $18
bend_token       = $19
off_token        = $24
ectory_token     = $29
set_token        = $2d
pic_token        = $37
disk_token       = $40
last_esc_command_token = $45                                    ; <<<< last_command_token

first_esc_function_token = $02
pointer_token    = $0a
last_esc_function_token = $0d                                    ; [910820]


;[[error.messages]]


ok_error_message
                 !text "O",'K'+$80                        ; 0 for ERR$ [910911]
error_message_list
                 !text "TOO MANY FILE",'S'+$80            ; 1
                 !text "FILE OPE",'N'+$80                 ; 2
                 !text "FILE NOT OPE",'N'+$80             ; 3
                 !text "FILE NOT FOUN",'D'+$80            ; 4
                 !text "DEVICE NOT PRESEN",'T'+$80        ; 5
                 !text "NOT INPUT FIL",'E'+$80            ; 6
                 !text "NOT OUTPUT FIL",'E'+$80           ; 7
                 !text "MISSING FILE NAM",'E'+$80         ; 8
                 !text "ILLEGAL DEVICE NUMBE",'R'+$80     ; 9
                 !text "NEXT WITHOUT FO",'R'+$80          ; 10
                 !text "SYNTA",'X'+$80                    ; 11
                 !text "RETURN WITHOUT GOSU",'B'+$80      ; 12
                 !text "OUT OF DAT",'A'+$80               ; 13
                 !text "ILLEGAL QUANTIT",'Y'+$80          ; 14
                 !text "OVERFLO",'W'+$80                  ; 15
                 !text "OUT OF MEMOR",'Y'+$80             ; 16
                 !text "UNDEF",$27,"D STATEMEN",'T'+$80   ; 17
                 !text "BAD SUBSCRIP",'T'+$80             ; 18
                 !text "REDIM",$27,"D ARRA",'Y'+$80       ; 19
                 !text "DIVISION BY ZER",'O'+$80          ; 20
                 !text "ILLEGAL DIREC",'T'+$80            ; 21
                 !text "TYPE MISMATC",'H'+$80             ; 22
                 !text "STRING TOO LON",'G'+$80           ; 23
                 !text "FILE DAT",'A'+$80                 ; 24
                 !text "FORMULA TOO COMPLE",'X'+$80       ; 25
                 !text "CAN",$27,"T CONTINU",'E'+$80      ; 26
                 !text "UNDEF'D FUNCTIO",'N'+$80          ; 27
                 !text "VERIF",'Y'+$80                    ; 28
                 !text "LOA",'D'+$80                      ; 29
                 !text "BREA",'K'+$80                     ; 30 ???? null & space [910925]
                 !text "CAN'T RESUM",'E'+$80              ; 31
                 !text "LOOP NOT FOUN",'D'+$80            ; 32
                 !text "LOOP WITHOUT D",'O'+$80           ; 33
                 !text "DIRECT MODE ONL",'Y'+$80          ; 34
; .byte 'NO GRAPHICS ARE','A'+$80   ;35
                 !text "SCREEN NOT OPE",'N'+$80           ; 35    [911001]
                 !text "BAD DIS",'K'+$80                  ; 36 ???? used for failed bootsys
                 !text "BEND NOT FOUN",'D'+$80            ; 37
                 !text "LINE NUMBER TOO LARG",'E'+$80     ; 38
                 !text "UNRESOLVED REFERENC",'E'+$80      ; 39
                 !text "UNIMPLEMENTED COMMAN",'D'+$80     ; 40
                 !text "FILE REA",'D'+$80                 ; 41
                 !text "EDIT MOD",'E'+$80                 ; 42    [910620]



;[[error.constants]]

errtmf           = 1
errfno           = 3
errfnf           = 4
err_missing_fname = 8
err_illegal_device = 9
errnf            = 10
errsn            = 11
errrg            = 12
errod            = 13
errfc            = 14
errov            = 15
errom            = 16
errus            = 17
errbs            = 18
errdd            = 19
errdvo           = 20
errid            = 21
errtm            = 22
errls            = 23
errbd            = 24
errst            = 25
errcn            = 26
erruf            = 27
ervfy            = 28
erload           = 29
erbrk            = 30
errcr            = 31
errlnf           = 32
errlwd           = 33
erroid           = 34
errng            = 35
errbdk           = 36
err_no_bend      = 37
err_too_large    = 38
err_ref          = 39
err_bad_command  = 40
err_file_read    = 41
edit_mode_error  = 42                                     ; [910620]
last_error_message = 42                                     ; # of last error msg

;.end

;[[error.message]]


; Routine to translate error message # in .a
; into address of string containing message in index2

erstup           tax                                      ; error set up
                 ldy #0                                   ; start with address of first error message
                 lda #<error_message_list
                 sta index2
                 lda #>error_message_list
                 sta index2+1

l11_1            dex
                 bmi l11_3                                ; finished when .x decrements out

l11_2            lda (index2),y                           ; look at msg, and find end (msb set) (ind.ok)
                 inw index2
                 and #$ff                                 ; was msb set?
                 bpl l11_2                                ; no, not end of message
                 bra l11_1                                ; yes, tick off another msg

l11_3            rts

;.end
;[[system.dispatcher]]



; Here for new statement. Character -> by txtptr is ':' or eol. The adr of
; this loc is left on the stack when a statement is executed so that it can
; merely do a rts when it is done.
; Get char, exit via xeqcm3, and return to newstt.

xeqcm            jmp (igone)

; Check if there is an interrupt from VIC that needs to be serviced

ngone            bbr7 runmod,l12_3                        ; get off here if we are in direct mode
                 lda intval                               ; check if there is an interrupt already in progress
                 bmi l12_3                                ; yes, don't go any further

                 ldx #2                                   ; check for 3 types of interrupts: s/s, s/b, & lp
l12_1            lda int_trip_flag,x
                 beq l12_2                                ; this wasn't set, go check next

                 lda #0
                 sta int_trip_flag,x                      ; reset this flag to show 'serviced'
                 lda int_adr_lo,x                         ; install the trap address as linnum
                 sta linnum
                 lda int_adr_hi,x
                 sta linnum+1
                 phx                                      ; save counter & text pointer
                 phw txtptr
                 lda #$80                                 ; flag 'no other interrupt traps, please'
                 tsb intval

                 jsr chrget                               ; skip over 2nd byte of line number
                 jsr gosub_sub                            ; fake a 'gosub' from here, so trap rx can do a RETURN
                 jsr goto_1
                 jsr newstt

                 lda #$80
                 trb intval
                 pla
                 sta txtptr+1
                 pla
                 sta txtptr
                 plx

l12_2            dex
                 bpl l12_1


l12_3            jsr chrget                               ; get statement type
xeqdir           jsr xeqcm3

newstt           jsr is_stop_key_down
                 bbr7 runmod,l13_1                        ; branch if direct mode

; In run mode...save txtptr for CONTinue command

                 jsr tto                                  ; transfer txtptr to oldtxt
                 tsx
                 stx oldstk

l13_1            ldy #0
                 jsr indtxt                               ; end of the line?
                 +lbne morsts                             ; no...out of statement

l13_2            bit runmod                               ; in direct mode?
                 +lbpl ready                              ; yes, go to ready
                 ldy #2
                 jsr indtxt                               ; end of text?
                 +lbeq ready                              ; yes...finished
                 iny                                      ; y=3
                 jsr indtxt                               ; extract line# lo byte
                 sta curlin
                 iny
                 jsr indtxt                               ; extract line # hi byte
                 sta curlin+1
                 tya                                      ; y=4
                 clc
                 adc txtptr                               ; point @ character before line start
                 sta txtptr
                 bcc l13_3
                 inc txtptr+1
l13_3            +lbra xeqcm                              ; execute new line



tto              lda txtptr
                 ldy txtptr+1
                 sta oldtxt
                 sty oldtxt+1
xeqrts           rts


; Set up for command processing and set processor address on stack.
; Exit via jmp to CHRGET

xeqcm3           beq xeqrts                               ; nothing here...null statement
                 bbr5 runmod,xeqcm2                       ; trcflg. branch if trace not enabled
                 bbr7 runmod,xeqcm2                       ; branch if direct mode- can't trace

                 pha                                      ; save token
                 lda #'['                                 ; print '[line-number]'
                 jsr outch                                ; outdo
                 jsr curprt                               ; print curlin
                 lda #']'
                 jsr outch                                ; outdo
                 pla                                      ; restore token


xeqcm2           cmp #esc_command_token                   ; special case: escape token
                 beq xeqesc
                 cmp #go_token                            ; special case: go to
                 +lbeq go_without_to
                 cmp #mid_token                           ; special case: mid$()=
                 beq xeqmid

; Command can be in the range END...NEW (old BASIC) & ELSE...MONITOR
; (new extensions).  Although there is a gap between these two blocks,
; it will be quickest & easiest to collapse them into one continuous block.

                 cmp #monitor_token+1
                 bcs snerr1
                 cmp #new_token+1
                 bcc xeqcm4                               ; no need to collapse
                 cmp #else_token
                 bcc snerr1
                 sbc #else_token-new_token-1

xeqcm4           sec                                      ; convert adjusted token into an index into a jump table.
                 sbc #end_token
                 +lbcc let                                ; it wasn't a token after all!  assume an assignment

xeqcm5           asl                                      ; *2 to convert into word pointer
                 tay
                 bcs l14_1                                ; dispatch table 1 or 2?     [901212]
                 lda stmdsp+1,y                           ; one
                 pha
                 lda stmdsp,y
                 bra l14_2

l14_1            lda stmdsp2+1,y                          ; two      [901212]
                 pha
                 lda stmdsp2,y

l14_2            pha
                 jmp chrget                               ; execution will commence after chrget's RTS



xeqmid                                                    ; handle special case of MID$= (what we call a kludge)
                 lda #>midwrk                             ; midd2-1
                 pha
                 lda #<midwrk
                 pha
xeqchr
                 jmp chrget




xeqesc                                                    ; execute escape token
                 jsr chrget                               ; let's have us a look at the second char
                 beq snerr1                               ; oops, there wasn't any!
                 cmp #first_esc_command_token             ; is it one of our esc tokens?
                 bcc l15_1                                ; no, foreign.
                 cmp #last_esc_command_token+1
                 bcs l15_1                                ; foreign

; It's one of our own.  Convert to index into command dispatch table

                 adc #monitor_token-else_token+new_token-end_token-first_esc_command_token+2
                 bra xeqcm5                               ; always

l15_1            sec                                      ; set up flag for a trip into the users code
                 jmp (iescex)

nescex           bcc xeqchr                               ; jmp chrget

snerr1           +lbra snerr

morsts           cmp #':'
                 +lbeq xeqcm                              ; if ':', continue statement
                 bra snerr1


;[[command.set1]]
; STOP, STOP KEY, and END handlers
;

is_stop_key_down
                 jsr _stop                                ; test stop key
                 bne do_rts                               ; not down, exit

; ldy trapno+1  ;test if trap on????   removed [910925]
; iny
; beq stop_1  ;no, do a normal stop


break_exit                                                ; STOP KEY:     [910104]
l16_1            jsr _stop                                ; wait for the user to release the key
                 beq l16_1
                 ldx #erbrk                               ; take the vector thru error to ready
                 +lbra error



stop             bcs stopc                                ; STOP: .c=1

end              clc                                      ; END: .c=0
stopc            +lbne snerr                              ; error if args present   [910410]

stop_1           bbr7 runmod,l17_1                        ; branch if direct mode
                 jsr tto                                  ; transfer txtptr to oldtxt
                 lda curlin
                 ldy curlin+1
                 sta oldlin
                 sty oldlin+1
l17_1            pla                                      ; .diris
                 pla
                 +lbcc ready                              ; say 'ready' if END, say 'break' if STOP


break            jsr release_channels                     ; make sure we're in text mode????  [910909]
                 jsr RestoreTextScreen
                 jsr highlight_text                       ; ????      [910624]
                 jsr _primm
                 !text cr,"BREAK",0
                 +lbra errfin                             ; exit via 'in line #'

do_rts           rts

;.end
;[[function.dispatch]]



; At this point, eval has determined that the token in a has to be a
; function.  It must therefor be in the range SGN...MID$ (old BASIC),
; or RGR...INSTR (new extensions).  We will collapse these two disjoint
; blocks into one continuous range.
;
; On entry, we can assume the token is >= 'sgn'

isfun            cmp #esc_function_token                  ; is this an escape function?
                 beq do_esc_fn                            ; yes
                 cmp #last_function_token+1
                 bcs snerr1                               ; no- must be syntax error
                 cmp #mid_token+1
                 bcc l18_1                                ; no need to adjust
                 sbc #rgraphic_token-mid_token-1

l18_1            pha                                      ; save token
                 tax
                 jsr chrget                               ; set up for synchk.
                 cpx #instr_token-1                       ; look for (adjusted) instr token
                 beq l18_2                                ; yes
                 cpx #rgraphic_token-1                    ; look for rgraphic which now takes 2 args [910801]
                 +lbeq rgraphic                           ; yes

                 cpx #mid_token+1
                 bcs oknorm                               ; LEFT$,RIGHT$,MID$ require multiple args
                 cpx #left_token                          ; is it past last single-arg function?
                 bcc oknorm                               ; no, must be normal function


; Most functions take a single argument.  The return address of these functions
; is CHKNUM, which ascertains that VALTYP=0 (numeric).  Normal functions which
; return string results (eg. CHR$) must pop off that return address and return
; directly to FRMEVL.
;
; The so called "funny" functions can take more than one argument, the first
; of which must be string and the second of which must be a number between 0
; and 255.  The closed parenthesis must be checked and return is directly to
; FRMEVL with the text pointer pointing beyond the ")".  The pointer to the
; description of the string argument is stored on the stack underneath the
; value of the integer argument.

l18_2            jsr chkopn                               ; check for an open parenthesis
                 jsr frmevl                               ; eat open paren and first argument
                 jsr chkcom                               ; two args so comma must delimit
                 jsr chkstr                               ; make sure first was string

                 pla                                      ; check token
                 cmp #instr_token-1                       ; special case: INSTR() bails out here
                 +lbeq instr
                 ldx facmo+1                              ; push address of string arg1
                 phx
                 ldx facmo
                 phx
                 pha                                      ; push token
                 jsr getbyt                               ; get arg2
                 pla                                      ; retrieve token
                 phx                                      ; push value of arg2
                 bra fingo                                ; go set up to evaluate fn



oknorm
                 jsr parchk                               ; check for open parens, evaluate argument
                 pla                                      ; restore token

fingo
                 sec                                      ; convert token to index into jump table
                 sbc #first_function_token
                 asl
                 tay
                 lda fundsp+1,y
                 sta jmper+2
                 lda fundsp,y
                 sta jmper+1
                 jsr jmper                                ; dispatch
;string functions remove this ret addr
                 +lbra chknum                             ; check for "numeric-ness" and return


; Escape Function handler

do_esc_fn
                 jsr chrget                               ; get second token
                 +lbeq snerr                              ; error if no second token
                 cmp #pointer_token
                 beq l19_1                                ; skip pre-parse if 'POINTER()'
                 pha
                 jsr chrget                               ; should be '('
                 jsr chkopn
                 jsr frmevl                               ; evaluate first argument
                 pla
l19_1            cmp #first_esc_function_token            ; see if this esc fn is one of ours
                 bcc foreign_esc_fn                       ; nope.
                 cmp #last_esc_function_token+1
                 bcs foreign_esc_fn                       ; nope

; Convert to index into the function dispatch table

                 adc #last_function_token-first_esc_function_token-1
                 bra fingo                                ; always


foreign_esc_fn
                 sec                                      ; flag 'up for grabs'
                 jsr go_foreign_esc_fn
n_esc_fn_vec
                 +lbcs snerr                              ; it's unwanted. off to the refuse pile
                 +lbra chknum

go_foreign_esc_fn
                 jmp (esc_fn_vec)


orop             ldy #255                                 ; must always complement
                 !text $2c

andop            ldy #0
                 sty count                                ; operator
                 jsr ayint                                ; (facmo&lo)=int value and check size
                 lda facmo                                ; use Demorgan's Law on high
                 eor count
                 sta integr
                 lda faclo                                ; and low
                 eor count
                 sta integr+1
                 jsr movfa
                 jsr ayint                                ; (facmo&lo)=int of arg
                 lda faclo
                 eor count
                 and integr+1
                 eor count                                ; finish out Demorgan
                 tay                                      ; save high
                 lda facmo
                 eor count
                 and integr
                 eor count
                 +lbra givayf                             ; float (a,y) and return to user


;[[operator.relational]]

; Time to perform a relational operator.
; (domask) contains the bits as to which relational operator it was.
; Carry bit on = string compare.


dorel            jsr chkval                               ; check for match
                 bcs strcmp                               ; is it a string?
                 lda argsgn                               ; pack argument for fcomp
                 ora #$7f
                 and argho
                 sta argho
                 lda #<argexp
                 ldy #>argexp
                 jsr fcomp
                 tax
                 bra qcomp


strcmp           lda #0
                 sta valtyp
                 dec opmask
                 jsr frefac                               ; free the faclo string
                 sta dsctmp                               ; save it for later
                 stx dsctmp+1
                 sty dsctmp+2
                 lda argmo                                ; get pointer to other string
                 ldy argmo+1
                 jsr fretmp                               ; frees first desc pointer
                 stx argmo
                 sty argmo+1
                 tax                                      ; copy count into x
                 sec
                 sbc dsctmp                               ; which is greater. if 0, all set up
                 beq stasgn                               ; just put sign of difference away
                 lda #1
                 bcc stasgn                               ; sign is positive
                 ldx dsctmp                               ; length of fac is shorter
                 lda #$ff                                 ; get a minus one for negatives
stasgn           sta facsgn                               ; keep for later
                 ldy #255                                 ; set pointer to first string. (arg)
                 inx                                      ; to loop properly
nxtcmp           iny
                 dex                                      ; any characters left to compare?
                 bne getcmp                               ; not done yet
                 ldx facsgn                               ; use sign of length difference
;since all characters are the same
qcomp            bmi docmp                                ; c is always set then
                 clc
                 bra docmp                                ; always branch


getcmp           lda #argmo
                 jsr lda_far_ram1                         ; lda (argmo),y from RAM1
                 pha
                 lda #dsctmp+1
                 jsr lda_far_ram1                         ; lda (dsctmp+1),y from RAM1
                 sta syntmp
                 pla
                 cmp syntmp
                 beq nxtcmp
                 ldx #$ff
                 bcs docmp
                 ldx #1



docmp
                 inx                                      ; -1 to 1, 0 to 2, 1 to 4
                 txa
                 rol
                 and domask
                 beq l20_1
                 lda #$ff                                 ; map 0 to 0, map all others to -1
l20_1            +lbra float                              ; float the one-byte result into FAC


;.end

;[[system.readyerror]]


bad_command
                 ldx #err_bad_command                     ; unimplemented command
                 !text $2c

userr            ldx #errus                               ; undefined statement
                 !text $2c

omerr            ldx #errom                               ; out of memory
                 !text $2c

doverr           ldx #errdvo                              ; division by zero
                 !text $2c

overr            ldx #errov                               ; overflow
                 !text $2c

snerr            ldx #errsn                               ; syntax error
                 !text $2c

ready            ldx #$80                                 ; no error

error            jmp (ierror)

nerror           txa
                 +lbmi ready_1                            ; ...branch if no error (from 'ready')
                 stx errnum                               ; save error # for 'er'
                 bbr7 runmod,errisd                       ; branch if direct mode- always display error

                 ldy #1                                   ; copy curlin to errlin, oldtxt to errtxt
l21_1            lda curlin,y
                 sta errlin,y                             ; line# where error occurred
                 lda oldtxt,y
                 sta errtxt,y                             ; statement where error occured
                 dey
                 bpl l21_1
                 inc errtxt                               ; point to a token, not ':' for HELP
                 bne l21_2
                 inc errtxt+1

l21_2            ldy trapno+1                             ; is trap set?
                 cpy #$ff
                 beq errisd                               ; no
                 sty linnum+1
                 sty tmptrp                               ; save until a resume is executed
                 ldy trapno
                 sty linnum

                 ldx #$ff
                 stx trapno+1                             ; flag no more traps
                 ldx #tempst                              ; clear any accumulated string temps
                 stx temppt
                 ldx oldstk
                 txs
                 jsr luk4it
                 +lbra newstt


;[[error.handler]]
errisd           dex
                 txa
                 jsr erstup                               ; set up address of error msg in .a in index2

                 bbs7 runmod,l22_1                        ; reset error line if direct mode error
                 lda #$ff
                 sta errlin                               ;
                 sta errlin+1

l22_1            jsr release_channels                     ; restore output to screen    [910909]
                 jsr RestoreTextScreen                    ; make sure we're in text mode    [910404]
                 jsr init_stack

l22_2            jsr crdo                                 ; Print error message- start a new line with '?'
                 jsr highlight_text                       ; use highlight color????    [910624]
                 jsr outqst
                 ldy #0
l22_3            lda (index2),y                           ; Read error msg from ROM  (ind.ok????)
                 pha
                 and #$7f
                 jsr outch                                ; Print it
                 iny
                 pla
                 bpl l22_3
                 ldx errnum                               ; retrieve error #     [910925]
                 cpx #erbrk
                 beq errfin                               ; skip 'error' crap if 'break'
                 jsr _primm
                 !text " ERROR",0

errfin           ldy curlin+1                             ; direct mode?
                 iny
                 beq l23_1                                ; yes...no line #
                 jsr inprt
l23_1            jsr highlight_done                       ; restore normal text color????    [910624]

;[[system.interface]]


ready_1
                 lda #%10000000
                 jsr _setmsg                              ; turn Kernel messages on
                 lda #%11000000
                 trb runmod                               ; turn run modes off, leave trace mode on????

ready_2
                 bbs4 runmod,l24_1                        ; print appropriate system prompt
                 jsr _primm                               ; Program mode: print 'ready.'
                 !text cr,"READY.",cr,0
                 bra main

l24_1            jsr _primm                               ; Edit mode: print 'ok.'
                 !text cr,"OK.",cr,0


main             jmp (imain)                              ; MAIN INPUT LOOP

nmain            ldx #$ff                                 ; set direct mode flag
                 stx curlin+1
                 jsr InputLine                            ; get a line of input & buffer it

;[[system.execute]]

execute_a_line                                            ; EXECUTE PLAIN TEXT IN BUFFER
                 stx txtptr                               ; init buffer pointer
                 sty txtptr+1
                 jsr chrget                               ; get first character of null-terminated string
                 tax
                 beq main                                 ; got null input
                 bcc l25_1                                ; got line number
                 jsr crunch                               ; got text- tokenize buffer,
                 jsr chrgot                               ; get first command (token),
                 +lbra xeqdir                             ; and execute it


;ADD or DELETE NEW LINE
l25_1            jsr linget                               ; evaluate line number, put into into linnum
                 bbr4 runmod,l25_2
                 jsr edit_crunch                          ; if edit mode, find end of input   [910620]
                 bra l25_3

l25_2            jsr crunch                               ; tokenize rest of input if not edit mode
l25_3            sty count                                ; save length
                 jsr FindLine                             ; locate line in program
                 +lbcc nodel                              ; not found, go insert line into program
; else delete current line and insert this one
;[[edit.shift]]

; Test: IF new line is longer than the line it replaces,
;  THEN IF there isn't enough room in memory to add this new line,
;   THEN out-of-memory error
;
; Before this fix, the old line was deleted BEFORE testing if the new line fit.
;
; N.B.: I am assuming that lines cannot be greater than 255 chars, as is the
; case where the line was entered "normally", that is, using LINGET.  The only
; consequence of this assumption is that lines > 255 will fall prey to the
; pre-fix problem mentioned above.

                 ldy #0
                 jsr indlow                               ; get lsb of the next line's starting address
                 sec
                 sbc lowtr                                ; subtract lsb of this line's starting address
                 sec                                      ; ignore borrow (gives abs. value)
                 sbc #4                                   ; allow for link & line number
                 sbc count                                ; compare with new length
                 bcs l25_5                                ; new line is shorter, no problem
                 neg                                      ; convert to positive delta

                 ldy text_top+1                           ; get msb of end of text (.c=0)
                 adc text_top                             ; add our calculated delta to end of text
                 bcc l25_4
                 iny
l25_4            cpy max_mem_0+1
                 bcc l25_5                                ; result is less than top-of-memory: ok
                 +lbne omerr                              ; msb >  top, overflow
                 cmp max_mem_0                            ; msb's the same, test lsb's
                 +lbcs omerr                              ; lsb >= top, overflow

; Using DMA device to move text downwards (to delete or replace a line)...
;
; lowtr     = destination
; (lowtr)    = pointer to source (via link bytes of line to be removed)
; text_top-(lowtr) = number of bytes to move (text_top points to old top of text)
; new text_top     = text_top -( (lowtr)-lowtr )

l25_5            lda lowtr                                ; set up DMA destination
                 sta dma1_dest_lo
                 lda lowtr+1
                 sta dma1_dest_hi
                 ldy #0
                 jsr indlow                               ; set up DMA source (& delta)
                 sta dma1_src_lo
                 sec
                 sbc lowtr
                 sta index1                               ; (delta lo)
                 iny
                 jsr indlow
                 sta dma1_src_hi
                 sbc lowtr+1
                 sta index1+1                             ; (delta hi)
                 sec
                 lda text_top                             ; set up DMA count
                 sbc dma1_src_lo
                 sta dma1_cnt_lo
                 lda text_top+1
                 sbc dma1_src_hi
                 sta dma1_cnt_hi

                 lda text_bank
; and #%00001111  ;      [910102]
; and #%01111111  ;      [910520] F018A
                 sta dma1_src_bank
                 sta dma1_dest_bank

                 lda #0
                 sta dma1_cmd                             ; dma command (copy, source=start)
                 sta dma1_subcmd                          ; [910520] F018A
                 sta dma_ctlr+2                           ; dma_list bank

                 ldx #>dma1_cmd                           ; dma_list
                 lda #<dma1_cmd
                 stx dma_ctlr+1                           ; dma_list hi
                 sta dma_ctlr                             ; dma_list lo & trigger

                 sec                                      ; calculate & set new text_top
                 lda text_top
                 sbc index1
                 sta text_top                             ; lo
                 lda text_top+1
                 sbc index1+1
                 sta text_top+1                           ; hi
;fall into routine to insert new line (if any)


nodel            jsr init_stack                           ; 'clearc' removed since text changes don't require trashing variables
                 jsr link_program                         ; fix links
                 jsr error_clear                          ; clear HELP/error flag, assuming he fixed whatever caused current error, if any

                 ldy #0
                 lda (txtptr),y                           ; delete line? ("common")
                 +lbeq main                               ; yes

l26_1            clc                                      ; no...something to insert
                 ldy text_top+1
                 lda text_top
                 sty hightr+1                             ; top of block to move (old text_top)
                 sta hightr
                 adc count                                ; number of characters in line to be inserted
                 bcc l26_2
                 iny
l26_2            clc
                 adc #4                                   ; plus link and line #
                 bcc l26_3                                ; gives us destination of move (new text_top)
                 iny

l26_3            sta highds                               ; destination of top
                 sty highds+1
                 cpy max_mem_0+1                          ; make sure new top doesn't crash into top of available ram
                 bcc l26_4                                ; ok
                 +lbne omerr                              ; out of memory, don't insert
                 cmp max_mem_0
                 +lbcs omerr                              ; out of memory, don't insert

l26_4            sta text_top                             ; set new top of text
                 sty text_top+1
                 sec                                      ; compute number of things to move up
                 lda hightr
                 sbc lowtr                                ; (old top) - (adr where new line goes)
                 tay                                      ; lowtr was setup previously by FindLine call
                 lda hightr+1
                 sbc lowtr+1
                 tax

; Using DMA device to copy data upwards...
;
; (hightr)   = source  (old top)
; (highds)   = destination (new top)
; .y, .x     = number of bytes to move
; (lowtr)    = where to insert new line (starting with link bytes)

                 dew hightr                               ; (text_top-1) points to actual last byte
                 dew highds

; lda dma_ctlr+3  ;dma controller version    [910520] F018A
; and #1
; beq l26_5   ; F018    removed [910808] F018B
                 lda #%00110000                           ; F018A, B
l26_5            sta dma1_cmd                             ; command=copy, source=endpt   [910102]
                 sty dma1_cnt_lo                          ; count
                 stx dma1_cnt_hi
                 tya
                 ora dma1_cnt_hi
                 beq l26_7                                ; special case= nothing to move???? should not happen

                 lda hightr
                 ldy hightr+1
                 sta dma1_src_lo                          ; source
                 sty dma1_src_hi
                 lda highds
                 ldy highds+1
                 sta dma1_dest_lo                         ; destination
                 sty dma1_dest_hi
                 lda text_bank                            ; [910520] F018A
; ldx dma1_cmd  ;version?    removed [910808] F018B
; bne l26_6   ; F018A
; and #%00001111  ;      [910102]
; ora #%01000000  ;(copy source=endpoint)    [910102]
l26_6            sta dma1_src_bank                        ; banks
                 sta dma1_dest_bank
                 lda #0
                 sta dma1_subcmd                          ; [910520] F018A
                 sta dma_ctlr+2                           ; dma_list bank
                 ldx #>dma1_cmd                           ; dma_list
                 lda #<dma1_cmd
                 stx dma_ctlr+1                           ; dma_list hi
                 sta dma_ctlr                             ; dma_list lo & trigger

; Make links non-null to fool 'chead'

l26_7            ldy #0
                 lda #1
                 ldx #lowtr
                 jsr sta_far_ram0                         ; sta (lowtr),y  y=0 (bleed-thru)
                 iny
                 jsr sta_far_ram0                         ; sta (lowtr),y  y=1 (bleed-thru)

; Put line number in text

                 iny
                 lda linnum
                 jsr sta_far_ram0                         ; sta (lowtr),y  y=2 (bleed-thru)
                 iny
                 lda linnum+1
                 jsr sta_far_ram0                         ; sta (lowtr),y  y=3 (bleed-thru)

; Advance 'lowtr' to start of line (past link bytes & line #)

                 clc
                 lda lowtr
                 adc #4
                 sta lowtr
                 bcc l26_8
                 inc lowtr+1


; Block move line to text

l26_8            ldy count                                ; use dma ???? [910925]
                 dey

l26_9            lda (txtptr),y                           ; (from common area)
                 jsr sta_far_ram0                         ; sta (lowtr),y   (bleed-thru)
                 dey
                 cpy #$ff
                 bne l26_9

; beq l26_9   ;special case= nothing to move???? should not happen
; lda #0   ; F018A, B
; sta dma1_cmd  ;command=copy, source=start
; sty dma1_cnt_lo  ;count
; sta dma1_cnt_hi
;
; lda txtptr
; ldy txtptr+1
; sta dma1_src_lo  ;source
; sty dma1_src_hi
; lda lowtr
; ldy lowtr+1
; sta dma1_dest_lo ;destination
; sty dma1_dest_hi
; lda text_bank  ;banks
; sta dma1_dest_bank
; lda #sys_bank  ;????
; sta dma1_src_bank
; sta dma1_subcmd  ;      [910520] F018A
; sta dma_ctlr+2  ;dma_list bank
; ldx #>dma1_cmd  ;dma_list
; lda #<dma1_cmd
; stx dma_ctlr+1  ;dma_list hi
; sta dma_ctlr  ;dma_list lo & trigger
;l26_9
                 jsr link_program
                 jsr reset_txtptr                         ; set up txtptr (was jsr runc)

; Test if AUTO in effect

                 lda autinc                               ; if in auto mode, increment val <> 0
                 ora autinc+1
                 beq l26_12                               ; not in

                 lda linnum                               ; yes, construct new line number
                 clc
                 adc autinc
                 sta facho+1
                 lda linnum+1
                 adc autinc+1
                 bcs l26_12                               ; no auto if wrapped
                 cmp #$fa                                 ; test if # >= 64000
                 bcs l26_12                               ; no auto if so.
                 sta facho
                 ldx #$90
                 sec
                 jsr floatc                               ; float it
                 jsr fout                                 ; make it into a string

                 sei                                      ; [910710]
                 ldx #0                                   ; move string into kbd buffer
l26_10           lda fbuffr+1,x                           ; copy number formed into buffer, ignoring leading space
                 beq l26_11                               ; a null marks end
                 sta _keyd,x
                 inx
                 bne l26_10                               ; always

l26_11           lda #29                                  ; cursor right
                 sta _keyd,x
                 inx
                 stx _ndx
                 cli                                      ; [910710]

l26_12           +lbra main

;[[system.linkprogram]]


link_program
                 lda txttab
                 ldy txttab+1
                 sta index
                 sty index+1
                 clc

chead            ldy #0
                 jsr indin1                               ; lda (index),y .. check for null link
                 bne l27_1
                 iny
                 jsr indin1                               ; lda (index),y
                 beq lnkrts

l27_1            ldy #3                                   ; [900524]
l27_2            iny                                      ; ???? very expensive loop ????
                 cpy #254
                 bcs link_error                           ; failsafe- program is mangled  [910103]
                 jsr indin1                               ; lda (index),y
                 bne l27_2
                 iny
                 tya
                 adc index
                 pha
                 ldy #0
                 ldx #index
                 jsr sta_far_ram0                         ; sta (index),y   (bleed-thru)
                 tya
                 adc index+1
                 iny
                 jsr sta_far_ram0                         ; sta (index),y   (bleed-thru)
                 plx
                 stx index
                 sta index+1
                 bra chead                                ; always


link_error                                                ; [910103]
                 jsr highlight_text                       ; [911119]
                 jsr _primm
                 !text cr,"?PROGRAM MANGLED",cr,0
                 jsr highlight_done                       ; [911119]


lnkrts           rts

;[[command.input.handler]]


; Function to get a line one character at a time from the input
; channel and build it in the input buffer.
;

PromptedInput                                             ; qinlin.
                 lda channl                               ; entry for things line INPUT, wanting a prompt
                 bne InputLine                            ; prompt only if terminal
                 jsr outqst                               ; yes- print '? '
                 jsr realsp


InputLine                                                 ; inlin.
                 ldx #0                                   ; read & buffer data until 'return' or buffer full
l28_1            jsr inchr                                ; get a character
                 cmp #0
                 beq l28_2
                 cmp #cr                                  ; a carriage return?
                 beq l28_2                                ; yes...done build

                 sta buf,x                                ; no...buffer it
                 inx
                 cpx #buflen                              ; buffer full?
                 bcc l28_1                                ; no...continue
                 +lbra errlen                             ; yes...string too long error


l28_2            lda #0                                   ; fininl.  terminate input with a null
                 sta buf,x
                 ldx #<buf_txtptr                         ; set up pointer to start of buffer-1 (for chrget)
                 ldy #>buf_txtptr
                 lda channl                               ; print 'return' only if terminal
                 +lbeq crdo
                 rts

;.end

;[[system.stack]]



; Find a specific token in the run-time stack. token to be found is in srchtk.
;
; If called by 'for' or 'next', scan entries in stack, looking for a specific
; 'for-variable' (in (forpnt)).  If found, (fndpnt) will point to it, and z is
; set.  Otherwise, (fndpnt) will point to either:
;  1) the non-for token
;  2) bottom-of-stack
;
; Special case: 'next' with no argument will match first 'for' entry on stack
; found, if any.  This case is signaled by a (forpnt) with a msb of $ff (an
; impossible value).
;
; All other calls to search will result in either:
;  1) (success) z = 1, (fndpnt) = address
;  2) (failure) z = 0


; Set up temporary pointer with current top of stack

search           sta srchtk                               ; save token to search for
                 jsr movtos                               ; tos => fndpnt


; Test if pointer is at bottom of stack.  If so, the item was not found.

l29_1            lda fndpnt
                 cmp #<stkbot
                 bne l29_2                                ; (fndpnt) <> bottom, ok
                 lda fndpnt+1                             ; lsb's the same, test msb's
                 cmp #>stkbot
                 beq l29_6                                ; stack empty, rts

l29_2            ldy #0
                 lda srchtk                               ; what are we looking for?
                 cmp #for_token                           ; 'for' tokens are special cases
                 bne l29_4

; Looking for a 'for' token.  If next token examined is not a 'for' token,
; return with z = 0.  Otherwise, check the pointer to its 'for' variable.
; If the variable pointer = (forpnt) or if (forpnt) = $FFxx, return with z=1.
; Otherwise, set up x with length of a 'for' entry, and use the usual
; mechanisim for examining the next entry.

                 cmp (fndpnt),y                           ; indirect ok- looking at runtime stack????
                 bne l29_7                                ; not 'for', do rts with z = 0
                 ldy #2                                   ; point to msb of 'for' variable
                 lda forpnt+1
                 cmp #$ff
                 beq l29_7                                ; do rts with z = 1
                 cmp (fndpnt),y
                 bne l29_3                                ; not right variable, keep looking.
                 dey
                 lda forpnt                               ; test lsb
                 cmp (fndpnt),y
                 beq l29_7                                ; a hit! rts with z = 1

l29_3            ldx #lenfor
                 bra l29_5                                ; keep looking

l29_4            lda (fndpnt),y
                 cmp srchtk                               ; is this the correct type of entry?
                 beq l29_7                                ; rts with z = 1

; The entry on top of the run-time stack is not the entry we are looking for.
; Find out what is there, and advance temp. pointer past it.

                 ldx #lenfor                              ; is it a 'for' entry?
                 cmp #for_token
                 beq l29_5
                 ldx #5                                   ; must be gosub or do by default

l29_5            txa
                 clc
                 adc fndpnt
                 sta fndpnt
                 bcc l29_1
                 inc fndpnt+1
                 bra l29_1                                ; always

l29_6            ldy #1                                   ; clear z flag
l29_7            rts


; GETSTK
;
; Add (.A) elements to top of run-time stack.  Error if result exceeds tos.

getstk           eor #$ff                                 ; make value 2's comp.
                 sec
                 adc tos
                 sta tos
                 ldy tos+1
                 bcs l30_1
                 dey
l30_1            sty tos+1
                 cpy #>stktop
                 +lbcc omerr
                 bne l30_2
                 cmp tos
                 +lbcc omerr
l30_2            rts


; (a,y) is a certain address.  REASON makes sure it is less than (fretop).

reason           cpy fretop+1
                 bcc l31_4
                 bne l31_1                                ; go garbage collect
                 cmp fretop
                 bcc l31_4

l31_1            pha
                 ldx #9                                   ; if tempf2 has zero in between
                 tya

l31_2            pha
                 lda highds-1,x                           ; save highds on stack
                 dex
                 bpl l31_2                                ; put 8 of them on stack
                 jsr garba2                               ; go garbage collect
                 ldx #$f7

l31_3            pla
                 sta highds+9,x                           ; restore after garbage collect
                 inx
                 bmi l31_3
                 ply
                 pla                                      ; restore .a and .y
                 cpy fretop+1                             ; compare highs
                 bcc l31_4
                 +lbne omerr                              ; higher is bad
                 cmp fretop                               ; compare the lows
                 +lbcs omerr
l31_4            rts



;  Utilities involved in the operation of the BASIC run-time stack.


; Move top-of-stack pointer to (fndpnt)

movtos           lda tos
                 sta fndpnt
                 lda tos+1
                 sta fndpnt+1
                 rts



; move (fndpnt) to (tos)

movfnd           lda fndpnt
                 sta tos
                 lda fndpnt+1
                 sta tos+1
                 rts

; Reduce size of run-time stack by (y).  No error checking performed!

rlsstk           tya
                 clc
                 adc tos
                 sta tos
                 bcc l32_1
                 inc tos+1
l32_1            rts

;.end
;[[system.linesearch]]




; FindLine
; Searches the program text for the line whose number is passed in "linnum".
; There are two possible returns:
;
; 1) carry set.
;  Line found.  (lowtr) points to the link bytes of line sought.
;
; 2) carry clear.
;  Line not found.  (lowtr) points to the link bytes of the next
;  line greater than the one sought.

FindLine
                 lda txttab                               ; init pointer to beginning of program
                 ldx txttab+1

FindLink
                 sta lowtr                                ; current position in program
                 stx lowtr+1
                 ldy #1
                 jsr indlow                               ; end of program (null link)?
                 beq l33_3                                ; yes, exit with .c=0 (not found)
                 iny
                 iny
                 jsr indlow                               ; get line number of this line (high byte first)
; sta syntmp
; lda linnum+1 ;is this the line we're looking for?
; cmp syntmp
; bcc l33_4  ; no- too high, so the line does not exist, exit
; beq l33_1
; dey  ; no- too low, so get link to next line
; bra l33_2
                 cmp linnum+1                             ; is this the line we're looking for?   [910925]
                 beq l33_1                                ; maybe
                 bcs l33_3                                ; no- too high, so the line does not exist, exit with .c=0
                 dey                                      ; no- too low, so get link to next line
                 bra l33_2

l33_1            dey                                      ; maybe- have to check low byte
                 jsr indlow
; sta syntmp
; lda linnum
; cmp syntmp
; bcc l33_4  ; no- too high, exit
; beq l33_4  ; yes- got it, exit
                 cmp linnum                               ; is this the line we're looking for?   [910925]
                 beq l33_4                                ; yes- got it, exit with .c=1
                 bcs l33_3                                ; no- too high, so the line does not exist, exit with .c=0

l33_2            dey                                      ; get link to next line
                 jsr indlow
                 tax
                 dey
                 jsr indlow
                 bra FindLink                             ; continue looking


l33_3            clc                                      ; exit, line not found (.c=0)
l33_4            rts                                      ; exit, line found (.c=1)

;.end
;[[command.line.get]]




; LINGET  Reads a line # from the current txtptr position
;   and stores it in linnum  (valid range is 0-63999).
;
;   On exit txtptr is pointing to the terminating char
;   which is in .a with condition codes set.
;   Endchr will be =0 if no digit input, else >0.  Use it
;   to distinguish between line # 0 & null input.

linget           ldx #0                                   ; enter with CHRGET flags set
                 stx endchr                               ; flags line # input
                 stx linnum                               ; init line # to 0
                 stx linnum+1

l34_1            bcs l34_5                                ; it's not a digit, do rts
l34_2            inc endchr                               ; indicate line # input
                 sbc #$2f                                 ; '0'-1 since .c=0
                 sta charac                               ; save for later
                 lda linnum+1
                 sta index
                 cmp #25                                  ; line number will be < 64000?
                 bcc l34_3                                ; yes, continue
                 bbs1 helper,l34_5                        ; no, if called by AutoScroll it's okay
                 +lbra snerr                              ; else syntax error

l34_3            lda linnum
                 asl                                      ; multiply by 10
                 rol index
                 asl
                 rol index
                 adc linnum
                 sta linnum
                 lda index
                 adc linnum+1
                 sta linnum+1
                 asl linnum
                 rol linnum+1
                 lda linnum
                 adc charac                               ; add in digit
                 sta linnum
                 bcc l34_4
                 inc linnum+1
l34_4
; jsr chrget  ;ALLOW SPACES to terminate number  [910620]
; bra l34_1
                 inw txtptr                               ; get next character from text
                 ldy #0                                   ; re-get current character from text
                 jsr indtxt                               ; lda (txtptr),y from RAM0
                 cmp #' '                                 ; space=eol    [910708]
                 beq l34_6
                 cmp #':'                                 ;
                 bcs l34_5                                ; eol
                 sec
                 sbc #'0'                                 ; alpha or numeric?
                 sec
                 sbc #$d0
                 bcc l34_2                                ; numeric
l34_5            rts                                      ; exit

l34_6            jsr chargt                               ; terminating character is a space, eat it just this once
                 +lbra chrtst                             ; return with flags set appropriately (esp. for 'range')

;.end

;[[command.list]]



list             rmb7 helper                              ; clear 'help' flag for p1line

; Determine which form of LIST we have...

                 +lbeq list_memory                        ; branch if terminator (no parameter)
                 +lbcc list_memory                        ; branch if a number (assume range parameter)
                 cmp #minus_token
                 +lbeq list_memory                        ; branch if a dash (assume range parameter)


; LIST command is of the form  LIST filename [,U#] [,D#]

list_file
                 lda #$e6                                 ; parse:  filename [,U#] [,D#]
                 jsr dosprs                               ; (like dopen:  0 0 0 *  * 0 0 1 )
                 jsr chk1                                 ; check parameters
                 lda #0
                 sta dossa                                ; setup as dload would (0 = load channel)
                 jsr find_la                              ; find an available la to use (cannot use reserved one)
                 ldy #fopn
                 ldx #4
                 jsr open_file                            ; open the file
                 bcs list_err                             ; exit if error

                 ldx dosla
                 jsr _chkin                               ; get input channel
                 bcs list_err                             ; exit if bad??
                 jsr _basin                               ; waste 'load address'
                 jsr _basin

l35_1            jsr _basin                               ; get link bytes
                 sta dosstr
                 jsr _basin
                 sta dosstr+1
                 ora dosstr
                 beq list_exit                            ; done if null pointer
                 jsr _readst
                 bne list_exit                            ; done if eof or bad status
; ???? assumes serial bus
                 lda #>dosstr                             ; point p1line's pointer at our line buffer
                 ldx #<dosstr
                 sta lowtr+1
                 stx lowtr

                 ldx #2
                 jsr _basin                               ; read line into buffer
                 sta dosstr,x
                 inx
                 jsr _basin                               ; 2-byte line #
                 sta dosstr,x
                 inx
l35_2            cpx #255                                 ; check buffer (buflen????)
                 +lbcs errlen                             ; 'too long' error
                 jsr _basin
                 sta dosstr,x
                 inx
                 tay                                      ; save char
                 jsr _readst                              ; check channel status (serial bus????)
                 bne list_exit                            ; exit if eof or error
                 jsr _stop
                 beq list_exit                            ; exit if stop key down
                 tya
                 bne l35_2                                ; loop until eol

                 jsr dcato                                ; get output channel
                 jsr crdo                                 ; start new line
                 ldx dosstr+2                             ; get line #
                 lda dosstr+3
                 jsr p1line                               ; print line #, space, and the line of code
                 jsr _clrch
                 ldx dosla
                 jsr _chkin                               ; get input channel
                 bcc l35_1                                ; [900730]

list_exit
                 jsr dcato                                ; flush last line with a <cr>
                 jsr crdo                                 ; flush current line
                 clc                                      ; no errors    [910404]
list_err
                 php                                      ; save error status   [910404]
                 pha
                 jsr release_channels                     ; release cmd channel, restore terminal
                 lda dosla
; bra close_out  ;    removed [900725]
                 clc                                      ; a real close   new [910404]
                 jsr _close
                 pla                                      ; pop error status, if any
                 plp
                 +lbra exit_disk_op


; LIST command is of the form  LIST [range]

list_memory
                 jsr range                                ; set up line range

l36_1            ldy #1
                 jsr indlow                               ; get ms byte of line to list's pointer
                 bne l36_2                                ; ok if not zero, but..
                 dey
                 jsr indlow
                 +lbeq crdo                               ; ..if ls byte is also zero, we're done

l36_2            jsr is_stop_key_down
                 jsr crdo                                 ; new line
                 ldy #2
                 jsr indlow                               ; get ms byte of line number
                 tax
                 iny
                 jsr indlow                               ; get ls byte

                 cmp linnum+1                             ; test if we are past the last line requested
                 bne l36_3
                 cpx linnum
                 beq l36_4
l36_3            +lbcs crdo                               ; next line is > last line requested, exit
l36_4            jsr p1line                               ; print line #, space, and the line of code
                 ldy #0                                   ; move 'pointer to next line' into (lowtr)
                 jsr indlow
                 tax
                 iny
                 jsr indlow
                 stx lowtr
                 sta lowtr+1
                 bra l36_1

;[[command.list.basic]]


;******************************************************
; P1LINE Print 1 line of BASIC text
;
; Entry: (a,x) contains line number low,high
;  (lowtr) points to beginning of line
;
; next-line   line-num  BASIC text......  null
; lo    hi    lo    hi  byte byte...byte   00
;        ^           ^     ^
;    (lowtr)        .A    .X
;******************************************************

p1line           bbr4 runmod,l37_1                        ; [910620]
                 +lbra edit_p1line                        ; handle things differently for plain text

l37_1            ldy #3
                 sty lstpnt
                 sty dores                                ; reset quote-switch
                 jsr linprt                               ; print line number
                 lda #' '                                 ; print a space

p1l010           ldy lstpnt
                 and #$7f

p1l015           cmp #':'                                 ; end-of-stmt?     [900516]
                 bne l38_1                                ; no
                 bbr7 helper,l38_1                        ; yes, but skip e-o-s check if not HELP...
                 bbs7 dores,l38_1                         ; or ':' is inside quotes
                 jsr highlight_done                       ; yes, restore normal text color
                 lda #':'

l38_1            jsr outch                                ; outdo
                 cmp #'"'                                 ; if quote character, toggle quote-switch
                 bne l38_2
                 lda dores
                 eor #$ff
                 sta dores

l38_2            iny                                      ; point to next character (should never wrap)
                 bbs0 helper,l38_3                        ; branch if highlighting tokens
                 bbs5 helper,l38_3                        ; branch if called by FIND/CHANGE
                 bbr7 helper,l38_4                        ; branch if called by LIST or HELP satisfied
l38_3            jsr helpsb

l38_4            jsr indlow
                 +lbeq highlight_done                     ; finished when trailing null is found
                 jmp (iqplop)                             ; usually points to nqplop


nqplop                                                    ; <<<<<<< vector entry
                 bpl p1l015                               ; not a token, just print character
                 bbs7 dores,p1l015                        ; branch if inside quotes, print chr as is

;  At this point, we know we're talking token.  Scan the token text
;  list until the correct text is found, and print that text.

                 sta token_saver                          ; save token for REM check   [910626]
                 cmp #esc_command_token                   ; is this an escape token?
                 beq print_esc_cmd                        ; yes- escape command
                 cmp #esc_function_token
                 beq print_esc_fn                         ; yes- escape function
                 cmp #pi
                 beq p1l015                               ; no- pi is >$80, but should be printed 'as is'
                 tax
                 sty lstpnt                               ; no- use the token as index into ROM keyword list
                 lda #>keyword_list
                 ldy #<keyword_list

; Scan list pointed to by (y,a) for token in (x), and print token's text

p1l026           sta index1+1                             ; index1 points to token text list in ROM
                 sty index1
                 ldy #0                                   ; begin scanning lists for this token's text
                 dex
                 bpl p1l070                               ; what luck! it's the first one

l39_1            inw index1                               ; scan text until next command found
                 lda (index1),y                           ; ind.ok (ROM)
                 bpl l39_1                                ; loop until terminal char (msb=1)
                 dex                                      ; is next text the one we want?
                 bmi l39_1                                ; no, keep scanning
                 inw index1                               ; yes, point to first character

                 bbr3 helper,p1l070                       ; found text for this token, is it REM?  [910626]
                 lda token_saver                          ; [910628]
                 cmp #rem_token
                 beq p1l071                               ; yes, and REM highlighting is enabled

p1l070                                                    ; found text for this token
                 bbr4 helper,p1l072                       ; branch if not highlighting tokens
                 lda (index1),y                           ; peek at first character
                 bmi p1l010                               ; branch if operator (1-byte, msb=1)
                 smb0 helper                              ; else begin highlight
p1l071           jsr highlight_text

p1l072           lda (index1),y                           ; get char from ROM table
                 bmi p1l010                               ; msb=1=last char this token, contine line
                 jsr outch                                ; else print it
                 iny
                 bra p1l072


; Print Escape Command

print_esc_cmd
                 tax                                      ; save type (cmd) in case it is a foreign esc token
                 iny
                 jsr indlow                               ; look at second token
                 +lbeq p1l015                             ; none?  print funny character
                 sty lstpnt
                 cmp #first_esc_command_token             ; is this one of ours?
                 bcc print_foreign_esc                    ; nope
                 cmp #last_esc_command_token+1
                 bcs print_foreign_esc                    ; nope
                 adc #$80-first_esc_command_token         ; yes- make a pointer p1l will be proud of
                 tax
                 ldy #<esc_command_list
                 lda #>esc_command_list
                 bra p1l026                               ; go scan list and print it



; Print Escape Function

print_esc_fn
                 tax                                      ; save type (function) in case it's a foreign esc token
                 iny
                 jsr indlow                               ; look at second token
                 +lbeq p1l015                             ; none?  print funny character
                 sty lstpnt
                 cmp #first_esc_function_token            ; is this one of ours?
                 bcc print_foreign_esc                    ; nope
                 cmp #last_esc_function_token+1
                 bcs print_foreign_esc                    ; nope
                 adc #$80-first_esc_function_token        ; yes- make a pointer p1l will be proud of
                 tax
                 ldy #<esc_function_list
                 lda #>esc_function_list
                 bra p1l026                               ; go scan list and print it


; The token to be printed is an escape token which is NOT recognized by BASIC.
; We will jump through the indirect chain and see if anyone claims this token.
;
; At this point:
; .C = 1 to signal 'unclaimed'
; .X = type (0==>command, ff==>function)
; .A = second token character
;
; If anyone claims this token, they should:
;
; > Clear .C to flag 'taken'
; > Point (INDEX1) at the string to be printed (with msb of last char set)
; > Note: string to print MUST be in RAM-0!

print_foreign_esc
                 cpx #esc_command_token
                 bne l40_1
                 ldx #0
                 !text $2c

l40_1            ldx #$ff
                 sec
                 jmp (iescpr)

nescpr           +lbcs p1l015                             ; no takers, print a funny graphic character
                 ldy #0
                 bra p1l070


;.end
;[[command.newclr]]



;
; The NEW command clears the program text as well as variable space.
;

new              beq init_text                            ; Erase program in memory
                 cmp #restore_token                       ; Restore an erased program?    [910103]
                 +lbne snerr                              ; no- syntax error    [910410]
                 jsr chkeos                               ; yes- eat token, error if not eos  [910429]
                 lda txttab                               ; "seed" first link to fool 'chead'
                 ldx txttab+1
                 sta index
                 stx index+1
                 lda #0
                 ldy #1
                 ldx #index
                 jsr sta_far_ram0                         ; clear msb  (bleed-thru)
                 dey
                 inc
                 jsr sta_far_ram0                         ; set lsb   (bleed-thru)
                 +lbra renumber                           ; make renumber check it for us (not 100%) & relink


init_text
                 lda txttab                               ; find the bottom of basic text
                 ldx txttab+1
                 sta index
                 stx index+1
                 dew index                                ; (the absolute bottom)

                 lda #0
                 tay
                 ldx #index
                 jsr sta_far_ram0                         ; clear bottom     (bleed-thru)
                 iny
                 jsr sta_far_ram0                         ; clear first link bytes    (bleed-thru)
                 iny
                 jsr sta_far_ram0                         ; (bleed-thru)
                 clc
                 lda txttab
                 adc #2
                 sta text_top                             ; set up (text_top), the end of text
                 lda txttab+1
                 adc #0
                 sta text_top+1

                 rmb5 runmod                              ; trcflg. reset trace flag


runc             jsr reset_txtptr                         ; load (txtptr) with (txttab)-1
                 bra clearc                               ; "CLR" to clear vars    [910410]


; CLeaR Routines
;

; Special forms of CLR command:
;
; CLR ERR$ Clears program error status, useful in TRAP handlers which
;   have resolved an error & wish to RESUME with a clean status.
;
; CLR DS$  Clears the currently buffered DS,DS$ messages.  The next
;   use of DS or DS$ will make BASIC re a new message from DOS.

clear            beq clearc                               ; branch if no args    [910410]

                 cmp #err_token                           ; CLR ERR$
                 bne l41_1                                ; no
                 jsr chkeos                               ; yes- eat token & error if not eos
                 +lbra error_clear                        ; and go clear ERR$

l41_1            cmp #'D'                                 ; CLR DS$     [910717]
                 bne l41_2                                ; no- error
                 jsr chrget
                 cmp #'S'
                 bne l41_2
                 jsr chrget
                 cmp #'$'
l41_2            +lbne snerr                              ; no- error
                 jsr chkeos
                 +lbra Clear_DS                           ; yes- clear current DS$


; Clearc is a subroutine which initializes the variable and array space by
; resetting STREND (the end of array storage).  It falls into INIT_STACK,
; which resets the stack.

clearc           jsr _clall                               ; close all files
                 ldy #0
                 sty dsdesc                               ; flag 'no DS$ string'
                 dey                                      ; (y=$ff)
                 sty trapno+1                             ; flag no current trap line
                 sty errlin                               ; reset last error pointers
                 sty errlin+1
                 sty errnum

                 lda max_mem_1                            ; clear string space
                 ldy max_mem_1+1
                 sta fretop
                 sty fretop+1

                 lda #<stkbot                             ; empty run-time stack
                 ldy #>stkbot
                 sta tos
                 sty tos+1

                 lda vartab
                 ldy vartab+1
                 sta arytab                               ; this will delete all variables,
                 sty arytab+1
                 sta strend                               ; ..and arrays
                 sty strend+1

                 ldx #pumony-puchrs                       ; reset print using chars
l42_1            lda pudefs,x
                 sta puchrs,x
                 dex
                 bpl l42_1

fload            jsr restore__1                           ; reset pointer for DATA statements


;[[stack.init]]

; INIT_STACK Routine (formerly STKINI)
;
;   Init_Stack resets the stack pointer.  String temporaries are freed up,
;   SUBFLG is reset, continuing is prohibited.

init_stack
                 ply                                      ; pop return address
                 pla
                 ldx #stkend-257                          ; reset system stack pointer
                 txs
                 pha                                      ; push return address
                 phy
                 ldx #tempst                              ; reset string temporaries
                 stx temppt
                 lda #0
                 sta subflg                               ; allow subscripted & integer vars
                 sta oldtxt+1                             ; disallow continuing
                 sta bits                                 ; reset math bit/byte flag

stkrts           rts



reset_txtptr
                 clc                                      ; load (txtptr) with (txttab)-1
                 lda txttab
                 adc #$ff
                 sta txtptr
                 lda txttab+1
                 adc #$ff
                 sta txtptr+1                             ; set up text pointers
                 rts

;.end
;[[command.return]]



;*********************************************************************
; RETURN Routine
;
; Restores the line number and text pointer from the stack, and
; eliminates all the FOR entries in front of the GOSUB entry.
;
;*********************************************************************

return

; Ok, pay attention: we got here by a pseudo-jsr which left a return to NEWSTT
; on the stack for us to return to.  There is also a return to NEWSTT left on
; the stack from the GOSUB we are returning from.  This is true UNLESS we got
; here on a sprite collision, in which case we still have the NEWSUB return
; recently left by our current call, but the second return goes back to the
; trapping mechanism.  The bottom line is: we have an extra return address on
; the stack, which we have to get rid of before leaving.

                 pla                                      ; mea culpa, mea culpa, mea culpa
                 pla
                 lda #gosub_token
                 jsr search                               ; look for GOSUB on runtime stack
                 beq ret010                               ; found
                 ldx #errrg                               ; else error
                 +lbra error

ret010           jsr movfnd                               ; (fndpnt) => (tos)
                 ldy #lengos
                 jsr rlsstk                               ; effectivly pop GOSUB off run-time stack
; dey
; lda (fndpnt),y
; sta txtptr+1
; dey
; lda (fndpnt),y
; sta txtptr
; dey
; lda (fndpnt),y
                 jsr retpat                               ; 01/18/84 patch: correct RETURN to GOSUB from direct mode
; lda (fndpnt),y
; sta curlin ;jump to DATA to waste rest of stmt (in case of ON..GOSUB)
                 bra data

;.end
;[[command.data]]




data
                 jsr datan                                ; skip to end of statement- offset in .y
addon            tya
                 clc
                 adc txtptr                               ; add offset to end to txtptr
                 sta txtptr
                 bcc remrts
                 inc txtptr+1
remrts           rts                                      ; NEWSTT rts addr is still there



rem              jsr remn                                 ; skip rest of statement
                 bra addon                                ; will always branch


datan            ldx #':'                                 ; DATA terminates on ":" and null
                 !text $2c

remn             ldx #0                                   ; REM terminates on null only
                 stx charac                               ; preserve terminator
                 ldy #0                                   ; this makes charac=0 after swap
                 sty endchr

l43_1            lda endchr
                 ldx charac
                 sta charac
                 stx endchr
l43_2            jsr indtxt
                 beq remrts                               ; null always terminates
                 cmp endchr                               ; is it some another terminator?
                 beq remrts                               ; yes, it's finished
                 iny                                      ; progress to next character
                 cmp #'"'                                 ; is it a quote?
                 bne l43_2                                ; no, just continue
                 beq l43_1                                ; yes, time to change

;.end
;[[command.ifthenelse]]



;****************************************************************
;*
;* IF Statment
;*
;* IF exp {GOTO line#  | THEN {line# | statements | b-block} }
;*  [:ELSE {line# | statements | b-block} ]
;*
;* B-block
;*
;* BEGIN : [statement(s) on one or more lines] : BEND
;*
;****************************************************************

if               jsr frmevl                               ; evaluate the conditional expression
                 jsr chrgot                               ; re-get current character
                 cmp #goto_token                          ; is terminating character a GOTO?
                 beq l44_1                                ; yes
                 lda #then_token                          ; no, it must be THEN
                 jsr synchr

l44_1            lda facexp                               ; test truth value of argument
                 bne if_true                              ; branch if true

if_false
                 jsr chrgot                               ; is there a b-block?
                 cmp #esc_command_token
                 bne l45_1                                ; no, must be an escape command
                 iny                                      ; might be, look at escape token
                 jsr indtxt
                 cmp #begin_token
                 bne l45_1                                ; branch if not
                 jsr find_bend                            ; skip to end of b-block

l45_1            jsr data                                 ; may be 'else' clause. first skip over 'then' clause..
                 ldy #0
                 jsr indtxt                               ; ..and see if end of stmt or end of line
                 beq rem                                  ; end of line, no 'else'. go to next line
                 jsr chrget                               ; another statement on this line.. is it 'else'?
                 cmp #else_token
                 bne l45_1                                ; no, keep looking on this line
                 jsr chrget                               ; yes! skip over token and execute clause (below)

if_true          jsr chrgot
                 beq l46_2                                ; branch if end of statement
                 bcs l46_1                                ; branch if not a number
                 +lbra goto                               ; here if of the form 'THEN line#'

l46_1            cmp #esc_command_token                   ; is this the beginning of a b-block?
                 bne l46_2                                ; no, must be an escape command
                 iny                                      ; might be, look at escape token
                 jsr indtxt
                 cmp #begin_token
                 bne l46_2
                 jsr chrget                               ; skip over 'BEGIN' if so...
                 jsr chrget                               ; ..and the second token, as well.

l46_2            jsr chrgot                               ; get back original character, & set up flags
                 +lbra xeqcm3                             ; ..and go execute whatever it is


find_bend                                                 ; ... subroutine to find end of current b-block
                 jsr chrget
                 bne l47_3

; End of statement.. set up next

l47_1            cmp #':'                                 ; is this EOL?
                 beq find_bend                            ; no, keep looking

l47_2            bbr7 runmod,l47_7                        ; EOL: branch if direct mode, 'block terminator not found' error

                 ldy #2
                 jsr indtxt                               ; end of text?
                 beq l47_7                                ; yes, msb of next stmt pointer = 0. error

                 iny
                 jsr indtxt
                 sta curlin                               ; set up next line of text
                 iny
                 jsr indtxt
                 sta curlin+1
                 tya
                 clc
                 adc txtptr
                 sta txtptr
                 bcc find_bend
                 inc txtptr+1
                 bra find_bend                            ; always

l47_3            cmp #'"'
                 bne l47_4
                 jsr un_quote                             ; look for terminating quote, or EOL
                 beq l47_1                                ; EOL or ':' after closing quote
                 bne find_bend                            ; ..else normal char, keep looking

l47_4            cmp #rem_token                           ; REM?
                 bne l47_5                                ; no
                 jsr rem                                  ; yes, trash this line
                 bra l47_2                                ; and go test for end of text

l47_5            cmp #esc_command_token                   ; is this a BEND?
                 bne find_bend                            ; can't be, has to be an escape

                 jsr chrget                               ; skip over esc token
                 cmp #bend_token
                 beq l47_6                                ; this is what we came for, bye!

                 cmp #begin_token                         ; not a BEND. is it a BEGIN?
                 bne find_bend                            ; it's just a normal, stick-in-the-mud char. keep looking.

                 jsr find_bend                            ; oh-oh, recursion. Dr. Ja-Ja warned me about this.
                 bra find_bend

l47_6            rts

l47_7            ldx #err_no_bend
                 +lbra error

un_quote                                                  ; txtptr points to a '"'. look for closing '"', or EOL
                 ldy #0
l48_1            inw txtptr
                 jsr indtxt
                 beq l48_2                                ; EOL, get out here with .z set and a '00' in .a
                 cmp #'"'
                 bne l48_1                                ; keep looking until quote
                 jmp chrget                               ; got closing quote, get byte after quote, set flags

l48_2            rts



else             cmp #esc_command_token                   ; is this of the form "ELSE b-block"?
                 bne l49_1                                ; no, must be an escape command
                 iny                                      ; might be, look at escape token
                 jsr indtxt
                 cmp #begin_token
                 bne l49_1                                ; no, justa plain-old "ELSE statement"
                 jsr find_bend                            ; yes, it is a b-block. skip over the b-block.
l49_1            +lbra rem


;.end
;[[command.on]]



;*********************************************************
;* ON expression {GOTO | GOSUB} line_number
;*********************************************************
ongoto
                 jsr getbyt                               ; get & save GOTO/GOSUB
                 pha
                 cmp #goto_token                          ; GOTO?
                 beq l50_1                                ; yes
                 cmp #gosub_token                         ; GOSUB?
                 +lbne snerr                              ; no, syntax error

l50_1            dec faclo
                 bne l50_2                                ; skip another line number
                 pla                                      ; get dispatch character
                 +lbra xeqcm2

l50_2            jsr chrget                               ; advance and set codes
                 jsr linget                               ; read next line
                 cmp #','                                 ; is it a "comma"?
                 beq l50_1
                 pla                                      ; remove stack entry (token)
                 rts                                      ; either end of line or syntax error

;.end
;[[command.let]]



;****************************************************************
;*
;*  [LET] variable = expression
;*
;****************************************************************

let              jsr ptrget                               ; get pntr to variable into "varpnt"
                 sta forpnt                               ; preserve pointer
                 sty forpnt+1
                 lda #equal_token
                 jsr synchr                               ; "=" is necessary

                 lda intflg                               ; save type for later
                 pha
                 lda valtyp                               ; retain the variable's value type too
                 pha

                 jsr frmevl                               ; get value of formula into FAC
                 pla
                 rol                                      ; carry set for string, off for numeric
                 jsr chkval                               ; make sure VALTYP matches carry
;and set zero flag for numeric
                 bne copstr                               ; if numeric, copy it
                 pla                                      ; get number type

qintgr           bpl copflt                               ; store a floating point number
                 jsr round                                ; round integer
                 jsr ayint                                ; make two-byte number
                 ldy #0
                 lda facmo                                ; get high
                 phx
                 ldx #forpnt
                 jsr sta_far_ram1 ;sta (forpnt),y         ; store it
                 iny
                 lda faclo                                ; get low
                 jsr sta_far_ram1                         ; sta (forpnt),y
                 plx
                 rts



copflt           ldx forpnt
                 ldy forpnt+1
                 +lbra movmf_ram1                         ; put number @forpnt in var bank



copstr           pla                                      ; if string, no INTFLG

inpcom           ldy forpnt+1                             ; TI$?
                 cpy #>zero                               ; (only TI$ can be this on assign)
                 +lbeq Set_TI_String                      ; yes
                 bra getspt                               ; no


dskx1            pla
                 iny

dskx2            cmp fretop+1
                 bcc l51_2
                 bne l51_1
                 dey
                 jsr indfmo
                 cmp fretop
                 bcc l51_2

l51_1            ldy faclo                                ; qvaria
                 cpy vartab+1                             ; if (vartab) > (facmo), don't copy
                 bcc l51_2
                 bne copy                                 ; it is less
                 lda facmo
                 cmp vartab                               ; compare low orders
                 bcs copy

l51_2            lda facmo                                ; dntcpy
                 ldy facmo+1
                 bra copyc


getspt           ldy #2                                   ; get pntr to descriptor
                 jsr indfmo
                 cmp dsdesc+2                             ; check for DS$ hi
                 bne dskx2                                ; nope
                 pha
                 dey
                 jsr indfmo
                 cmp dsdesc+1                             ; check for DS$ lo
                 bne dskx1                                ; nope
                 lda dsdesc                               ; check if len=0
                 beq dskx1                                ; yup
                 pla                                      ; fall through to copy


copy             ldy #0
                 jsr indfmo
                 jsr strini                               ; get room to copy string into
                 lda dscpnt                               ; get pointer to old descriptor, so
                 ldy dscpnt+1
                 sta strng1                               ; movins can find string
                 sty strng1+1
                 jsr movins                               ; copy it

                 lda strng1                               ; fix to free get strings
                 ldy strng1+1
                 jsr fretms                               ; free the string, if it is a temp

                 lda #<dsctmp
                 ldy #>dsctmp

copyc            sta dscpnt
                 sty dscpnt+1
                 sta index                                ; index points to new descriptor
                 sty index+1
                 jsr fretms


;   Fix the strings by flagging the old string as garbage and the new
;   string by pointing it to its new descriptor.

                 jsr stradj                               ; set up new string
                 bcc l52_1                                ; leave it alone
                 ldy #0
                 lda forpnt                               ; put in backwards link
                 phx
                 ldx #index
                 jsr sta_far_ram1
                 iny
                 lda forpnt+1
                 jsr sta_far_ram1
                 plx

l52_1            lda forpnt                               ; fix old string
                 sta index
                 lda forpnt+1
                 sta index+1
                 jsr stradj                               ; point to old string
                 bcc l52_2                                ; in text do not fix
                 dey                                      ; restore y
                 phx
                 ldx #index
                 lda #$ff                                 ; garbage flag
                 jsr sta_far_ram1
                 dey
                 pla                                      ; (was txa)
                 pha
                 jsr sta_far_ram1                         ; store length
                 plx

l52_2            ldy #2                                   ; set the descriptor
                 phx
                 ldx #forpnt
l52_3            lda #dscpnt
                 jsr lda_far_ram1                         ; lda (dscpnt),y from RAM1
                 jsr sta_far_ram1                         ; sta (forpnt),y to   RAM1
                 dey
                 bpl l52_3
                 plx
                 rts


;   STRADJ takes the pointer index which points to a descriptor and
;   indexes to the desciptor's string data.  If the string is not in
;   string space (no action to take) we return with carry clear, else
;   we return with the pointer set to the link bytes in the string, the
;   length in .a and the carry set.

stradj           ldy #0
                 jsr indin1_ram1                          ; push length on stack
                 pha
                 beq l53_5                                ; if length=0 do nothing
                 iny
                 jsr indin1_ram1                          ; get low byte (into .x)
                 tax
                 iny
                 jsr indin1_ram1                          ; get high byte
                 cmp max_mem_1+1
                 bcc l53_1                                ; ok
                 bne l53_5                                ; if above top of memory
                 cpx max_mem_1                            ; msb the same, test lsb
                 bcs l53_5                                ; if above top of memory

l53_1            cmp fretop+1
                 bcc l53_5                                ; if below fretop
                 bne l53_2
                 cpx fretop
                 bcc l53_5                                ; if below fretop

l53_2            cmp dsdesc+2
                 bne l53_3                                ; fix
                 cpx dsdesc+1
                 beq l53_5

l53_3            stx index                                ; ok set pointer
                 sta index+1
                 pla                                      ; get back length
                 tax                                      ; into x also
                 clc
                 adc index
                 sta index
                 bcc l53_4
                 inc index+1
l53_4            sec                                      ; carry set
                 rts

l53_5            pla                                      ; clean up stack
                 clc
                 rts

;.end
;[[command.printcmd]]



;***********************************************************
;*
;* PRINT   PRINT#   CMD
;*
;**********************************************************

printn           jsr cmd                                  ; docmd
                 +lbra release_channels                   ; restore terminal


cmd              jsr getbyt
                 beq l54_1
                 lda #','                                 ; comma?
                 jsr synchr

l54_1            php                                      ; save stat (beq=eof)
                 pha                                      ; save char
                 stx channl                               ; channel to output on
                 jsr coout
                 pla                                      ; get char back
                 plp                                      ; get stat back
                 bra print


strdon           jsr strprt

newchr           jsr chrgot                               ; reget last character

print            beq crdo                                 ; terminator only, so print crlf
                 cmp #using_token
                 +lbeq using



printc           beq prtrts  ;here after seeing TAB(x) or "," or " ; " in which case
;a terminator does not mean a crlf but just RTS
                 cmp #tab_token                           ; TAB function?
                 beq taber                                ; yes (c=1)
                 cmp #spc_token                           ; space function?
                 clc                                      ; clear carry
                 beq taber                                ; yes (c=0)
                 cmp #','                                 ; comma?
                 beq comprt                               ; yes
                 cmp #';'                                 ; a semicolon?
                 beq notabr                               ; yes

                 jsr frmevl                               ; evaluate the formula
                 bbs7 valtyp,strdon                       ; branch if a string
                 jsr fout
                 jsr strlit                               ; build descriptor
                 jsr strprt                               ; print the number
                 jsr outspc                               ; print a space
                 bra newchr                               ; always goes



crdo             lda #cr
                 jsr outch                                ; outdo

crfin            bbr7 channl,prtrts
                 lda #lf
                 jsr outch                                ; outdo
; eor #$ff  ;????

prtrts           rts



comprt           sec
                 jsr _plot                                ; get tab position in x
                 tya
                 sec
morco1           sbc #column_width
                 bcs morco1
                 eor #$ff
                 adc #1
                 bne aspac

taber            php                                      ; remember if SPC(c=0) or TAB(c=1) function
                 sec
                 jsr _plot                                ; read tab position
                 sty trmpos
                 jsr gtbytc                               ; get value into accx
                 cmp #')'
                 +lbne snerr
                 plp
                 bcc xspac
                 txa
                 sbc trmpos
                 bcc notabr                               ; negative, don't print any
aspac            tax
xspac            inx
xspac2           dex
                 bne xspac1


notabr           jsr chrget                               ; reget last character
                 bra printc                               ; don't call crdo


xspac1           jsr outspc
                 bne xspac2


; STROUT Routine
;
; Print the string pointed to by .x.  It must end with a null byte.

strout           jsr strlit                               ; get a string literal

strprt           jsr frefac                               ; return temp pointer
                 tax                                      ; put count into counter
                 ldy #0
                 inx                                      ; move one ahead
strpr2           dex
                 beq prtrts                               ; all done
                 jsr indin1_ram1                          ; lda (index),y
                 jsr outch                                ; outdo
                 iny
                 cmp #cr
                 bne strpr2
                 jsr crfin                                ; type rest of carriage return
                 bra strpr2                               ; and on and on

outspc           lda channl                               ; if terminal print skip chr., else print space
                 bne realsp
                 lda #29                                  ; CBM cursor right (non-destructive skip char)
                 !text $2c

realsp           lda #' '                                 ; space
                 !text $2c

outqst           lda #'?'

;outdo
                 jmp outch                                ; output char in .a
; and #$ff ;????
; rts

;.end

;[[command.inputs]]


get              jsr errdir                               ; direct mode illegal
                 sta z_p_temp_1                           ; flag to distinguish between GET and GETKEY

                 cmp #'#'                                 ; is it GET# ?
                 beq getn                                 ; yes
                 cmp #key_token                           ; is it GETKEY ?
                 bne gettty                               ; no, must be plain GET
                 jsr chrget                               ; yes, skip over KEY token
                 bra gettty


getn             jsr chrget                               ; GET# move up to next byte
                 jsr getbyt                               ; get channel into x
                 lda #','                                 ; comma?
                 jsr synchr
                 stx channl
                 jsr coin                                 ; chkin


gettty                                                    ; GET
                 ldx #<buf+1                              ; point to 0
                 ldy #>buf
                 lda #0                                   ; to stuff and to point
                 sta buf+1                                ; zero it
                 lda #$40                                 ; turn on v-bit
                 jsr inpco1                               ; do the get
                 ldx channl
                 bne release_channels                     ; restore terminal channels
                 rts


linputn                                                   ; input line from channel into a string var
                 jsr chrget                               ; (eat input# token)
                 smb7 op
                 !text $2c

inputn           rmb7 op                                  ; flag INPUT# vs. LINPUT#
                 jsr getbyt                               ; get channel number
                 lda #','                                 ; a comma?
                 jsr synchr
                 stx channl
                 jsr coin                                 ; chkin
                 jsr notqti                               ; do input to variables


release_channels                                          ; iodone, iorele.
                 jsr _clrch                               ; clear I/O channels
; ldx #0   ;restore normal terminal channels
                 sta channl                               ; (was stx)     [910909]
                 rts


linput                                                    ; input line from console into a string var
                 jsr chrget                               ; (eat input token)
                 smb7 op
                 !text $2c

input            rmb7 op                                  ; flag INPUT vs. LINPUT
                 cmp #'"'                                 ; a quote?
                 bne notqti                               ; no message
                 jsr strtxt                               ; literalize the string in text

                 jsr chrgot                               ; looking for prompt string terminator  [910219]
                 cmp #','
                 bne l55_1
                 sta buf_txtptr                           ; is comma- supress '?' after prompt  [910219]
                 jsr chrget                               ; eat comma
                 jsr strprt                               ; print prompt
                 jsr errdir                               ; error if direct mode
                 jsr InputLine                            ; get first item
                 bra getagn1                              ; see if there's more to do

l55_1            lda #';'                                 ; must end in semicolon
                 jsr synchr
                 jsr strprt                               ; print prompt

notqti           jsr errdir                               ; use common routine since def direct
                 lda #','                                 ; get comma
                 sta buf_txtptr                           ; (data reader expects buffer to start with terminator)

getagn           jsr PromptedInput                        ; type "?" and input a line of text
getagn1          lda channl
                 beq l56_1
                 jsr _readst                              ; get status byte
; and #2   ; (assumes serial bus????)  [910618] eoi ok
                 and #%10000111                           ; serial: err if dnp, r/w timeout errors
                 beq l56_1                                ; a-ok rs232: err if brk, ovr, frm, par errors
                 jsr release_channels                     ; bad, close channel
                 +lbra data                               ; skip rest of input

l56_1            lda buf                                  ; bufful. get anything?
                 bne inpcon                               ; yes- process input
; lda channl  ;didn't get anything.  is this keyboard? [901212]
; bne getagn  ; no- keep looking for data ????
                 jsr datan                                ; skip to end of statement
                 +lbra addon


read             rmb7 op                                  ; flag READ vs. LREAD    [910102]
                 ldx datptr                               ; get last data location
                 ldy datptr+1
                 lda #$98                                 ; initiator= read
                 !text $2c

inpcon           lda #0                                   ; initiator= input
inpco1           sta input_flag                           ; $98=read, $40=get, $00=input

; In the processing of DATA and READ statements, one pointer points to the data
; (i.e., the numbers being fetched) and another points to the list of variables.
;
; The pointer into the data always starts pointing to a terminator- a ",", ":", or EOL.
; At this point TXTPTR points to list of variables and (x,y) points to data or input line.

                 stx inpptr                               ; pointer to data
                 sty inpptr+1

inloop           jsr ptrget                               ; get a pointer to the variable
                 sta forpnt                               ; store its address
                 sty forpnt+1

                 ldx #1
l57_1            lda txtptr,x                             ; move variable list pointer to 'vartxt'
                 sta vartxt,x
                 lda inpptr,x                             ; move data line pointer to 'txtptr'
                 sta txtptr,x
                 dex
                 bpl l57_1

                 jsr chrgot                               ; get first data byte
                 bne datbk1                               ; not null, so we got something
                 bit input_flag                           ; READ($98), GET($40), or INPUT($00)?
                 bvc qdata                                ; branch if READ or INPUT
                 lda z_p_temp_1                           ; GET or GETKEY?
                 cmp #key_token
                 bne l57_3                                ; branch if GET

l57_2            jsr cgetl                                ; GETKEY
                 tax                                      ; test if null
                 beq l57_2                                ; it is null, keep scanning
                 bne l57_4                                ; got a key, go put it in var

l57_3            jsr cgetl                                ; get a key if pressed, otherwise gets a zero
l57_4            sta buf
                 ldx #<buf_txtptr
                 ldy #>buf_txtptr
                 bra datbk


qdata            +lbmi datlop                             ; branch if READ
                 lda channl                               ; else it's INPUT
                 bne l58_1
                 jsr outqst                               ; console input, so display '? ' prompt

l58_1            jsr PromptedInput                        ; get another line

datbk            stx txtptr                               ; set for CHRGET
                 sty txtptr+1

datbk1           bbr7 op,l59_1                            ; no chrgot if LINPUT (want leading spaces) [910513]
                 jsr chargt
                 jsr chrtst
                 bra l59_2

l59_1            jsr chrget                               ; get next data byte
l59_2            bbr7 valtyp,l59_8                        ; get value type, input a number if numeric
                 bbr6 input_flag,l59_4                    ; branch if not get, set quote
                 inx
                 stx txtptr
l59_3            lda #0                                   ; [901212]
                 sta charac
                 bra l59_5

l59_4            bbs7 op,l59_3                            ; no terminators if LINPUT or LREAD  [901212]
                 sta charac                               ; setqut.  assume quoted string
                 cmp #'"'                                 ; terminators ok?
                 beq l59_6                                ; yes (sets .c)
                 lda #':'                                 ; set terminators to ":" and...
                 sta charac
                 lda #','                                 ; ...comma

l59_5            clc                                      ; resetc
l59_6            sta endchr                               ; nowget
                 lda txtptr
                 ldy txtptr+1
                 adc #0                                   ; .c is set properly above
                 bcc l59_7
                 iny
l59_7            jsr strlt2                               ; make a string descriptor for the value & copy if needed
                 jsr st2txt                               ; copy strng2 to txtptr (st-2-txt... get it?)
                 jsr inpcom                               ; do assignment
                 bra l59_9

l59_8            bbs7 op,l59_10                           ; error if LINPUT (string input only)  [901212]
                 ldx #0                                   ; numins. flag 'text bank' (0)
                 jsr fin
                 lda intflg                               ; set codes on flags
                 jsr qintgr                               ; go decide on float

l59_9            jsr chrgot                               ; strdn2. read last character
                 beq trmok                                ; ":" or EOL is ok
                 cmp #','                                 ; a comma?
                 beq trmok

                 lda input_flag                           ; is this get, read, or input?
                 beq l59_11                               ; input
                 bmi l59_10                               ; read
                 ldx channl                               ; get. if not kbd, go use 'bad file data error'
                 bne l59_12

l59_10           ldx #errtm                               ; tmerr. 'get from kbd' or 'read' saw a bad type
                 bra l59_13                               ; always

l59_11           lda channl
                 beq l59_14                               ; do again if keybd input
l59_12           ldx #errbd                               ; input saw bad file data
l59_13           +lbra error


l59_14           jsr highlight_text                       ; [911119]
                 jsr _primm
                 !text "?REDO FROM START",cr,0
                 jsr highlight_done                       ; [911119]

ott              lda oldtxt
                 ldy oldtxt+1
                 sta txtptr                               ; put user back to beginning of input
                 sty txtptr+1
                 rts



trmok            ldx #1
l60_1            lda txtptr,x
                 sta inpptr,x                             ; save for more reads
                 lda vartxt,x
                 sta txtptr,x                             ; point to variable list
                 dex
                 bpl l60_1

                 jsr chrgot                               ; look at last vartab character
                 beq l60_2                                ; that's the end of the list
                 jsr chkcom                               ; not end. check for comma
                 +lbra inloop

l60_2            lda inpptr                               ; put away a new data pntr name
                 ldy inpptr+1
                 bbr7 input_flag,l60_3
                 sta datptr
                 sty datptr+1
                 rts

l60_3            ldy #0                                   ; last data chr could have been ',' or ':' but should be null
                 lda #inpptr
                 jsr lda_far_ram0
                 beq l60_4                                ; it is null
                 lda channl                               ; if not terminal, no type
                 bne l60_4

                 jsr highlight_text                       ; [911119]
                 jsr _primm
                 !text "?EXTRA IGNORED", cr,0
                 jsr highlight_done                       ; [911119]

l60_4            rts                                      ; do next statement


; DATLOP Routine Subroutine to find data.
;
; The search is made by using the execution code for data to skip over
; statements, the start word of each statement is compared with "data_token".
; Each new line number is stored in "datlin" so that if any error occurs while
; reading data the error message can give the line number of the bad data.

datlop           jsr datan                                ; skip some text
                 iny
                 tax                                      ; end of line?
                 bne l61_1                                ; no
                 ldx #errod                               ; yes, "no data" error
                 iny
                 jsr indtxt
                 +lbeq error

                 iny
                 jsr indtxt                               ; get high byte of line number
                 sta datlin
                 iny
                 jsr indtxt                               ; get low byte
                 iny
                 sta datlin+1

l61_1            jsr addon                                ; nowlin.  txtptr+.y
                 jsr chrgot                               ; span blanks
                 tax                                      ; used later
                 cpx #data_token                          ; is it a DATA statement?
                 bne datlop                               ; not quite right, keep looking
                 +lbra datbk1                             ; this is the one


;.end
;[[command.next]]



; Next routine
;
; 'FOR' entry on the stack has the following format:
;
; Low address
;  token (for_token) 1 byte
;  a pointer to the loop variable 2 bytes
;  the step 5 bytes
;  a byte reflecting the sign of the incr. 2 bytes
;  the upper value (packed) 5 bytes
;  the line number of the FOR statement 2 bytes
;  a text pointer into the FOR statement 2 bytes
; High address
;
; (total 16 bytes)

next             bne l62_2                                ; hop if 'next' variable given
                 ldy #$ff                                 ; flag no specific 'for' variable
                 bra l62_3                                ; always

l62_1            ldy #lenfor                              ; done, clean up stack
                 jsr rlsstk                               ; release (y) items from stack
                 jsr chrgot
                 cmp #','                                 ; ie., NEXT j,k
                 bne l62_7
                 jsr chrget

l62_2            jsr ptrget                               ; get pointer to variable in (a,y)
                 sta forpnt

l62_3            sty forpnt+1
                 lda #for_token
                 jsr search                               ; look for FOR entry in run-time stack
                 beq l62_4                                ; branch if found
                 ldx #errnf                               ; otherwise 'error, not found'
                 +lbra error


; Set up to move STEP value to FAC

l62_4            jsr movfnd                               ; (fndpnt) => (tos)
                 lda fndpnt
                 clc
                 adc #3                                   ; offset to step value
                 ldy fndpnt+1
                 bcc l62_5
                 iny

l62_5            jsr movfm                                ; actually "move from ROM", but sys stack is in "common"
                 ldy #8                                   ; MOVFM doesn't move sign.  Get it
                 lda (fndpnt),y
                 sta facsgn

; Get pointer to FOR variable

                 ldy #1
                 lda (fndpnt),y                           ; get lsb
                 pha
                 tax
                 iny
                 lda (fndpnt),y                           ; get msb
                 pha
                 tay                                      ; msb in y
                 txa                                      ; lsb in a
                 jsr fadd                                 ; add STEP value to FOR variable (fadd gets from bank 1)
                 ply                                      ; msb in y
                 plx                                      ; lsb in x
                 jsr movmf_ram1                           ; put result back into FOR variable in var bank

; Make (a,y) point at TO value in stack

                 lda fndpnt
                 clc
                 adc #9
                 ldy fndpnt+1
                 bcc l62_6
                 iny

; Test if loop done

l62_6
; sta sw_rom_ram0 ;????
                 jsr fcomp                                ; compare FAC to value pointed to by (a,y)
                 ldy #8
                 sec
                 sbc (fndpnt),y                           ; (common area????)
                 beq l62_1                                ; branch taken if done

                 ldy #17                                  ; not done, set pointers to re-execute loop
                 lda (fndpnt),y                           ; (common area????)
                 sta txtptr
                 dey
                 lda (fndpnt),y
                 sta txtptr+1
                 dey
                 lda (fndpnt),y
                 sta curlin+1
                 dey
                 lda (fndpnt),y
                 sta curlin
l62_7            rts

;.end
;[[command.dim]]



; The DIMension code sets DIMFLG and then falls into the variable search
; routine, which looks at DIMFLG at 3 different points:
;
; 1) If an entry is found, DIMFLG being on indicates a
;    doubly-defined variable.
; 2) When a new entry is being built, DIMFLG being on indicates
;    the indices should be used for the size of each index.
;    Otherwise the default of ten is used.
; 3) When the build entry code finishes, indexing will be done
;    only if DIMFLG is off.


dim3             jsr chkcom                               ; must be a comma

dim              tax                                      ; make .x non-zero (.a must be non-zero to work correctly)
                 jsr ptrgt1
                 jsr chrgot                               ; get last character
                 bne dim3
                 rts

;.end
;[[command.sys]]



sys              jsr getwrd                               ; convert arg to integer value
                 lda linnum                               ; set up arg's for call to 'long jsr'
                 sta _pclo
                 lda linnum+1
                 sta _pchi
                 lda current_bank
                 sta _bank

                 jsr optbyt                               ; (optional) .A reg arg
                 bcc l63_1
                 stx _a_reg

l63_1            jsr optbyt                               ; (optional) .X reg arg
                 bcc l63_2
                 stx _x_reg

l63_2            jsr optbyt                               ; (optional) .Y reg arg
                 bcc l63_4
                 stx _y_reg

l63_3            jsr optbyt                               ; (optional) .Z reg arg
                 bcc l63_4
                 stx _z_reg

l63_4            jsr optbyt                               ; (optional) .S reg arg
                 bcc l63_5
                 stx _s_reg

l63_5            jmp _jsr_far                             ; far, far away
;If returns, Kernel will update _reg's for us

;.end
;[[command.dma]]



; DMA - Set up for DMA operation (FETCH/STASH/SWAP)
;
;  Syntax:  DMA  command,length,source(l/h/b),destination(l/h/b)[,subcmd,mod(l/h)] [,...]


dma                                                       ; params are not longer optional-  [910520] F018A
                 jsr getbyt                               ; get command
l64_1            bcc l64_2
                 txa                                      ; [910102]
                 and #%00000100                           ;
                 +lbne fcerr                              ; (disallow chained DMA lists)
                 stx dma2_cmd

l64_2            jsr comwrd                               ; get length
; bcc l64_3
                 sty dma2_cnt_lo
                 sta dma2_cnt_hi

l64_3            jsr comwrd                               ; get source address & bank
; bcc l64_4
                 sty dma2_src_lo
                 sta dma2_src_hi
l64_4            jsr combyt
; bcc l64_5
                 stx dma2_src_bank

l64_5            jsr comwrd                               ; get destination address & bank
; bcc l64_6
                 sty dma2_dest_lo
                 sta dma2_dest_hi
l64_6            jsr combyt
; bcc l64_7
                 stx dma2_dest_bank

l64_7            jsr optzer                               ; get subcmd, default=0    [910520] F018A
; bcc l64_8
                 stx dma2_subcmd

l64_8            jsr optzer                               ; get mod lo/hi, default=0   [910102]
; bcc l64_9
                 stx dma2_mod_lo
l64_9            jsr optzer
; bcc l64_10
                 stx dma2_mod_hi

l64_10           ldy #0                                   ; dma_list (bank 0)
                 ldx #>dma2_cmd
                 lda #<dma2_cmd
                 sty dma_ctlr+2                           ; dma_list bank
                 stx dma_ctlr+1                           ; dma_list hi
                 sta dma_ctlr                             ; dma_list lo & trigger
l64_11           bit dma_ctlr+3                           ; check status (in case IRQ enabled)  [910103]
                 bmi l64_11                               ; busy

                 jsr chrgot                               ; eol?
                 beq l64_12                               ; yes
                 jsr optbyt                               ; no- continue after getting comma & next cmd byte
                 bra l64_1

l64_12           rts

;.end
;[[command.trace]]




tron                                                      ; trace mode on
                 smb5 runmod                              ; trcflg
                 rts


troff                                                     ; trace mode off
                 rmb5 runmod                              ; trcflg
                 rts


;.end
;[[command.sys.returnreg]]



; RREG - Return values of 6502 registers following a SYS call.
;
; Syntax : RREG [.A variable [,[.X[...Z] variable] [,[.S variable] ]]]

rreg             lda #0
                 sta count

l65_1            jsr chrgot
                 beq l65_4                                ; reached end of statement- done
                 cmp #','                                 ; skip this arg?
                 beq l65_3                                ; branch if so
                 jsr ptrget                               ; get pointer to target variable
                 sta forpnt                               ; a little bit of set up so we can share LET code
                 sty forpnt+1
                 lda valtyp                               ; what kind of variable name did ptrget find?
                 +lbne chkerr                             ; type mismatch error if string

                 ldy count                                ; which register's value are we looking for?
                 lda _a_reg,y                             ; .A, .X, .Y, & .Z are contiguious
                 cpy #4
                 bne l65_2
                 lda _s_reg                               ; but .S isn't

l65_2            tay                                      ; low byte in .Y
                 lda #0                                   ; high byte of zero
                 jsr givayf                               ; go float it
                 lda intflg                               ; set conditions for type of var (int/float)
                 jsr qintgr                               ; ..and use part of LET to do the work

l65_3            inc count                                ; 5 registers to do
                 lda count
                 cmp #5
                 bcs l65_4
                 jsr chrgot                               ; was this e-o-statement?
                 beq l65_4
                 jsr chrget                               ; not e-o-s, skip over comma,
                 bne l65_1                                ; ..and go do next

l65_4            rts

;.end
;[[command.midstring]]



; Alternate use of the MID$ function, as the target of an assignment.
;
; MID$(string_var,starting_position [,length]) = string_expression

midd2
midwrk           =midd2-1

                 jsr chkopn                               ; check for '('
                 jsr ptrget                               ; get pointer to descriptor of string-var
                 sta forpnt                               ; store for later use
                 sty forpnt+1
                 jsr chkstr                               ; check if string

                 jsr combyt                               ; look for comma, followed by 1 byte starting address
                 dex                                      ; adjust starting addr
                 stx hulp                                 ; store    " "

                 cmp #')'                                 ; finished?
                 beq l66_1                                ; branch if so (use default length)
                 jsr combyt                               ; ..else get length
                 !text $2c

l66_1            ldx #$ff                                 ; default length
                 stx z_p_temp_1
                 jsr chkcls                               ; look for ')'
                 lda #equal_token                         ; look for '='
                 jsr synchr
                 jsr frmevl                               ; bring on the source!
                 jsr chkstr                               ; nothing funny

                 ldy #2                                   ; get string descriptors
l66_2            lda #forpnt                              ; target
                 jsr lda_far_ram1                         ; lda (forpnt),y
                 sta str1,y
                 jsr indfmo                               ; source
                 sta str2,y
                 dey
                 bpl l66_2

; Test for target string in text was removed-  all strings are copied to
; string RAM when they are created.

                 sec                                      ; adjust pointer to source string so that the same
                 lda str2+1                               ; ..index can load & save
                 sbc hulp
                 sta str2+1
                 bcs l66_3
                 dec str2+2

l66_3            lda z_p_temp_1                           ; get specified length (or default)
                 cmp str2                                 ; compare with length of source
                 bcc l66_4                                ; ok if less,
                 lda str2                                 ; ..else use length of source
l66_4            tax
                 beq l66_7                                ; done if length=0
                 clc
                 adc hulp                                 ; add length to starting posn.
                 +lbcs fcerr                              ; illegal quantity error if > 256
                 cmp str1
                 bcc l66_5
                 +lbne fcerr                              ; ...or if > target length

l66_5            ldy hulp                                 ; get adjusted starting address
l66_6            phx
                 ldx #str1+1
                 lda #str2+1
                 jsr lda_far_ram1                         ; fetch from string bank
                 jsr sta_far_ram1                         ; this is what it's all about
                 iny
                 plx
                 dex
                 bne l66_6                                ; keep going for specified length

l66_7            +lbra frefac                             ; free up temp. string, rts

;.end
;[[command.auto]]



; AUTO Increment
;   Syntax :    auto {line-number} (line-number = 0 means turn off)

auto
                 jsr errind
                 jsr linget
                 lda linnum
                 sta autinc
                 lda linnum+1
                 sta autinc+1
                 rts

;.end



help             ldx errnum                               ; check for error status
                 inx
                 beq l67_1                                ; exit if there is no current error
                 lda errlin
                 ldy errlin+1
                 sta linnum
                 sty linnum+1
                 jsr FindLine                             ; find the beginning of line with error
                 bcc l67_1                                ; exit if line not found?

                 jsr crdo                                 ; begin a new line
                 ldx linnum
                 lda linnum+1
                 ldz helper
                 rmb4 helper                              ; temporarily disable token highlighting
                 smb7 helper                              ; set 'help' flag for P1LINE
                 jsr p1line                               ; display line & highlight error
                 stz helper
l67_1            rmb7 helper                              ; reset 'help' flag
                 +lbra crdo                               ; and return to caller



helpsb                                                    ; logic to highlight error or find string
                 bbs4 helper,highlight_done               ; branch if highlighting tokens
                 bbs5 helper,l68_3                        ; branch if FIND

                 ldx lowtr+1                              ; has P1LINE reached code in error?
                 tya
                 clc
                 adc lowtr                                ; add character pointer to line pointer...
                 bcc l68_1
                 inx
l68_1            cpx errtxt+1                             ; and compare to error pointer
                 bne l68_2                                ; not there
                 cmp errtxt
                 bcs highlight_text                       ; we're there- begin highlighting
l68_2            rts


l68_3            cpy fndpnt                               ; at first character of find string?
                 bcc l68_5                                ; before it
                 lda find_count
                 beq l68_5                                ; past it
                 bmi l68_6                                ; at last character
                 cmp fstr1+2
                 bcc l68_4                                ; in middle of string
                 jsr highlight_text                       ; at first char- start highlight
l68_4            dec find_count                           ; one less character to highlight
                 beq l68_4                                ; special case-
;make it negative for next time around
l68_5            rts

l68_6            inc find_count                           ; make it zero


highlight_done                                            ; nasty kludge to colorize error or found text
                 lda highlight_save
                 bmi l69_1                                ; (unless it's already normal)
                 sta _color                               ; restore normal color
                 ora #$80
                 sta highlight_save                       ; mark highlight_save invalid
                 rmb7 helper                              ; remove HELP flag
                 rmb1 helper                              ; remove token flag
l69_1            rts


highlight_text                                            ; nasty kludge to colorize error or found text
                 bit highlight_save
                 bpl l70_1                                ; (unless it's already highlighted)
                 lda _color                               ; save current (normal) color
                 sta highlight_save                       ; msb=0 to mark highlight_save valid
                 lda highlight_color
                 sta _color                               ; change color to highlight
l70_1            rts

;.end
;[[command.gotosub]]



; GOSUB-  Push text pointer, line #, & gosub token on stack:
;
;  (bottom) highest memory
;===========================================================
;  txtptr+1 address of next statement
;  txtptr
;  ========
;  curlin+1 current line number
;  curlin
;  ========
;  'gosub' token <== (tos) top of stack pointer
;===========================================================
;  (top of stack) lowest memory


gosub            bbs4 runmod,edit_err                     ; [910620]
                 jsr gosub_sub
                 jsr chrgot                               ; get character and set carry for linget
                 jsr goto
                 +lbra newstt


goto             bbs4 runmod,edit_err                     ; [910620]
                 jsr linget                               ; pick up the line number in LINNUM
                 lda endchr                               ; test if linget found any number
                 +lbeq snerr                              ; no number error

goto_1           jsr remn                                 ; jump to end of line (entry for interrupt code)
                 sec
                 lda curlin
                 sbc linnum
                 lda curlin+1
                 sbc linnum+1
                 bcs luk4it
                 tya
                 sec
                 adc txtptr
                 ldx txtptr+1
                 bcc lukall
                 inx
                 bra lukall                               ; always goes


luk4it           lda txttab
                 ldx txttab+1

lukall           jsr FindLink                             ; (a,x) are all set up
                 +lbcc userr                              ; undefined statement error
                 lda lowtr
                 sbc #1
                 sta txtptr
                 lda lowtr+1
                 sbc #0
                 sta txtptr+1
                 bbr7 runmod,setexc                       ; branch if in direct mode
                 rts



gosub_sub
                 lda #lengos                              ; free up necessary space on stack
                 jsr getstk                               ; make sure there is room
                 ldy #lengos-1
                 lda txtptr+1                             ; push on the text pointer
                 sta (tos),y                              ; (common area)
                 dey
                 lda txtptr
                 sta (tos),y                              ; (common area)
                 dey
                 lda curlin+1                             ; push on the current line number
                 sta (tos),y                              ; (common area)
                 dey
                 lda curlin
                 sta (tos),y                              ; (common area)
                 dey
                 lda #gosub_token                         ; (a) was smashed by GETSTK
                 sta (tos),y                              ; (common area)
                 rts


edit_err
                 ldx #edit_mode_error                     ; [910620]
                 +lbra error

;.end



go_without_to
                 jsr chrget                               ; what is next character?
                 cmp #to_token                            ; ..is it GO TO?
                 bne l71_1
                 jsr chrget                               ; ..yes, set up for goto
                 bra goto                                 ; ..bye!

l71_1            jsr getbyt                               ; is it GO 64?
                 cpx #64
                 +lbne snerr                              ; ...no, error

; The user wants to go to C64 mode.

l71_2            jsr are_you_sure
                 bne cont_rts                             ; must have had second thoughts. never mind
; jsr put_io_in_map
                 jmp _go_64


;.end
;[[command.continue]]


;**********************************************************
;*
;* CONTINUE Execution after STOP/END
;*
;**********************************************************

cont             bne cont_rts                             ; make sure there is a terminator
                 bbs4 runmod,edit_err                     ; [910620]
                 bbs7 runmod,cont_rts                     ; if in run-mode just rts

                 ldx #errcn                               ; continue error.
                 ldy oldtxt+1                             ; a stored txtptr of zero set up by INIT_STACK
                 +lbeq error                              ; indicates there is nothing to continue

                 lda oldtxt                               ; STOP, END, typing crlf to INPUT, and STOP key
                 sta txtptr
                 sty txtptr+1
                 lda oldlin
                 ldy oldlin+1
                 sta curlin
                 sty curlin+1

setexc           smb7 runmod                              ; set up run mode
                 lda #0
                 sta autinc                               ; turn auto increment off
                 sta autinc+1
                 sta intval                               ; enable & reset collision-trapping mechanism
                 sta _autoinsert                          ; disable auto-insert mode ?????

                 ldx #2                                   ; turn off all interrupt trip flags
l72_1            sta int_trip_flag,x
                 dex
                 bpl l72_1

                 jsr _setmsg                              ; turn kernel messages off & rts

cont_rts
                 rts

;.end
;[[command.run]]


;***********************************************************
;*
;* RUN Command
;*
;* RUN [line_number]
;* RUN filename [[ON] Ddrive_number[,Uunit_number]]
;*
;* Entry:  RUN_A_PROGRAM sets up, links, and executes
;*  a program previously loaded into RAM.
;*
;***********************************************************

run              bbs4 runmod,edit_err                     ; [910620]
                 beq run__10                              ; branch if no arguments
                 bcc run__20                              ; branch if number (i.e., RUN line_number)


; Here if of the form "RUN file_name"

                 smb6 runmod                              ; set flag for load not to go to ready
                 jsr dload                                ; use DLOAD's parser, and load the program
                 +lbcs erexit                             ; if problem loading   [900801]

run_a_program
                 jsr crdo                                 ; [911010]
                 jsr fix_links                            ; re-link the program
                 jsr setexc                               ; set various run modes
                 jsr runc
                 +lbra newstt                             ; start executing


; Here if of the form "RUN"

run__10          jsr setexc                               ; set various run codes
                 +lbra runc                               ; ..and start executing


; Here if of the form "RUN line_number"

run__20          jsr clearc                               ; first trash all variables
                 jsr chrgot
                 jsr goto                                 ; set up to execute from new line number
                 jsr setexc                               ; ..and do a little housekeeping,
                 +lbra newstt                             ; ..otherwise it's business as usual

;.end
;[[command.restore]]


;*********************************************************************
;*
;* RESTORE Command
;*
;* Reset pointers to next DATA statement.  Allows optional argument
;* specifying a specific line number, otherwise the default is the
;* beginning of text area.
;*
;*********************************************************************

restor
                 beq restore__1                           ; branch if no argument...use default
                 jsr getwrd                               ; get 2 byte argument (???? no check for real number means a var legal)
                 sty linnum
                 sta linnum+1
                 jsr FindLine                             ; get pointer to specified line
                 +lbcc userr                              ; error if not found

                 lda lowtr                                ; decrement 2 byte pointer, and save it
                 ldy lowtr+1
                 bra restore__2                           ; always


restore__1                                                ; entry from FLOAD
                 sec
                 lda txttab
                 ldy txttab+1

restore__2
                 sbc #1
                 bcs l73_1
                 dey
l73_1            sta datptr
                 sty datptr+1
                 rts

;.end
;[[command.renumber]]


;***********************************************************************
;
; RENUMBER Command
;
; Syntax:  RENUMBER [n1 [,[n2] ,n3]]
;
;  n1 = new start line number, default 10
;  n2 = line increment, default 10
;  n3 = start line, default first
;
; - Syntax error may occur for missing commas or bad line numbers.
; - Illegal quantity error for line increment of 0 or for bad range.
; - Overflow error if increment wraps line number during renumber,
;  line number too large error if renumbering would force line
;  numbers greater than 63999.
; - Out of memory error if the renumbered program would be too large.
; - Unresolved reference error if an imbedded line number references
;  a line which does not exist.
;
; Otherwise returns to "ready" mode upon completion.
;
;***********************************************************************


; Before any data is changed in any way, two preliminary passes are
; made to insure no errors would occur during the actual renumbering
; process (as detailed below).
;
; Pass 1 makes sure that the renumbered program would have no line
; numbers greater than 63999 (nothing is actually renumbered; the
; statement table is not modified).
;
; Pass 2 checks if the renumbered program would be too long and also
; checks for non-existant line number destinations.
;
; Pass 3 examines the entire statement table first for imbedded line
; numbers (branches) to fix. This is done by looking for keywords (GOTO,
; GOSUB, THEN, RUN) which are usually followed by line numbers. The old
; line number is mapped to a new value and the string representing the
; new branch label replaces the original text.
;
; Pass 4 then replaces the statement number bytes by their final values.
; and the table is relinked.


testwd
                 !text goto_token,run_token,gosub_token,then_token
                 !text restore_token,resume_token,trap_token,else_token

renumber
                 jsr errind                               ; allowed only in direct mode

; Set up default values for n1, n2, and n3

                 lda #0                                   ; line #10...
                 ldx #10
                 stx renum_tmp_1                          ; default renum origin (n1)
                 sta renum_tmp_1+1
                 stx renum_tmp_2                          ; default increment (n2)
                 sta renum_tmp_2+1
                 sta hightr                               ; default start line # (n3)
                 sta hightr+1

                 jsr chrgot                               ; any parameters?
                 beq ren_pass_1                           ; no...


; Check for new starting line number (n1)

                 jsr linget                               ; check for a number
                 lda endchr                               ; was there one?
                 beq renum_10                             ; no...use default
                 lda linnum
                 ldx linnum+1
                 sta renum_tmp_1
                 stx renum_tmp_1+1

; Check for new increment

renum_10
                 jsr optwrd                               ; an increment given?
                 bcc renum_30                             ; no...use default

                 sty renum_tmp_2
                 sta renum_tmp_2+1
                 ora renum_tmp_2                          ; increment must be >0
                 +lbeq fcerr                              ; illegal quantity error

; Check for starting line number

renum_30
                 jsr optwrd                               ; starting line number given?
                 bcc ren_pass_1                           ; no...

                 sty hightr
                 sty linnum
                 sta hightr+1
                 sta linnum+1
                 jsr FindLine                             ; test for illegal renumber range
                 lda lowtr                                ; (n1 must be >= n3)
                 ldx lowtr+1
                 sta highds                               ; pointer to first statement to renumber
                 stx highds+1
                 lda renum_tmp_1
                 ldx renum_tmp_1+1
                 sta linnum
                 stx linnum+1
                 jsr FindLine                             ; lowtr = ptr to 1st stmt to be overlapped
                 sec
                 lda lowtr                                ; can't be smaller
                 sbc highds
                 lda lowtr+1
                 sbc highds+1
                 +lbcc fcerr                              ; bad...


;***********************************************************************
;**************  R E N U M B E R    P A S S    O N E  ******************
;***********************************************************************

; Pass 1 makes sure that the renumbered program will not have any line numbers
; greater than 63999 (however, nothing is actually renumbered in this pass).

ren_pass_1
                 jsr tto                                  ; save txtptr for restoration when done
                 jsr n1_reset                             ; put n1 in FAC, reset txtptr
                 jsr chargt                               ; skip low link
                 iny                                      ; (.y=1)
                 jsr indtxt                               ; skip high link
                 beq ren_pass_2                           ; end of program => begin pass 2 (assumes txttab > 0)

r_pass1_10
                 iny                                      ; (.y=2)
                 jsr indtxt                               ; line number low
                 sec
                 sbc hightr                               ; in line range which is to be renumbered?
                 iny                                      ; (.y=3)
                 jsr indtxt                               ; line number high
                 sbc hightr+1
                 bcs r_pass1_20                           ; yes => fake renumbering
                 jsr set_next                             ; goto next line
                 bne r_pass1_10                           ; if z=0 then not end-of-text => keep going
                 beq ren_pass_2                           ; else end

r_pass1_20
                 jsr set_next                             ; goto next line
                 beq ren_pass_2                           ; if z=1 then end-of-text => exit
                 jsr new_num                              ; create next line number
                 bcs r_pass1_30                           ; if c=1 then it wrapped => error
                 cmp #>63999                              ; can't have lines > 63999
                 bcc r_pass1_20                           ; if c=0 then ok

r_pass1_30                                                ; renumbering will generate an illegal line #
                 ldx #err_too_large                       ; 'line number too large' error
                 +lbra error

set_next
                 ldy #0                                   ; set for next BASIC line
                 jsr indtxt                               ; low link
                 tax
                 iny                                      ; (.y=1)
                 jsr indtxt                               ; high link
                 beq set_end                              ; if z=1 then end of program => exit
                 stx txtptr
                 sta txtptr+1
set_end          rts


;***********************************************************************
;**************  R E N U M B E R    P A S S    T W O  ******************
;***********************************************************************

; Pass 2 checks if the renumbered program will be too long and also
; checks for non-existant line number destinations.

ren_pass_2
                 bbr4 runmod,l74_1                        ; skip pass two and three if plain text (edit mode) [910620]
                 jsr n1_reset                             ; yes- just setup up starting line # and reset txtptr
                 bra ren_pass_4                           ; then renumber just the text's line numbers

l74_1            lda #$01                                 ; set flag for 'pass 2'
                 sta z_p_temp_1
                 lda text_top                             ; copy top-of-text pointer for later use
                 ldx text_top+1                           ; (we don't want to change original here)
                 sta fndpnt
                 stx fndpnt+1
                 jsr imbed_lines                          ; search for imbedded lines (but don't change)



;***********************************************************************
;************  R E N U M B E R    P A S S    T H R E E  ****************
;***********************************************************************

; Pass 3 actually renumbers the imbedded destination line numbers
; which follow goto, gosub, trap, etc.

ren_pass_3
                 dec z_p_temp_1                           ; z_p_temp_1 = 0 (for pass 3)
                 jsr imbed_lines                          ; search for and update imbedded line #'s


;***********************************************************************
;*************  R E N U M B E R    P A S S    F O U R  *****************
;***********************************************************************

; Pass 4 actually renumbers the program line numbers & exits

ren_pass_4
                 jsr chargt_x2                            ; skip link
                 beq renumber_exit                        ; null link=> end-of-text, exit (assumes txttab > 0)
                 jsr chargt                               ; not null...
                 sta linnum                               ; if line# >= start#, replace with facho
                 iny
                 jsr indtxt
                 sec
                 sbc hightr+1
                 bcc r_pass4_20                           ; no, let alone
                 bne r_pass4_10                           ; yes, replace
                 lda linnum
                 sbc hightr
                 bcc r_pass4_20                           ; no, let alone

r_pass4_10
                 lda facho
; phx
                 jsr sta_far_txt                          ; sta (txtptr),y  hi  (bleed-thru)
                 dey
                 lda facho+1
                 jsr sta_far_txt                          ; sta (txtptr),y  lo (bleed-thru)
; plx
                 jsr chargt                               ; skip past 2nd byte of line#
                 jsr line_inc                             ; incr line# and scan to eol
                 bra ren_pass_4                           ; always...

r_pass4_20
                 jsr chargt                               ; skip past line#
                 jsr scan_thru                            ; scan to eol
                 bra ren_pass_4                           ; always...


renumber_exit
                 jsr fix_links                            ; patch things up: relink & set eot

direct_mode_exit
                 jsr ott                                  ; restore txtptr for next command in buffer
                 lda #0                                   ; but disallow continuing
                 sta oldtxt+1
                 rts


;***********************************************************************
;*************  R E N U M B E R   S U B R O U T I N E S  ***************
;***********************************************************************

; Look for imbedded line #'s (after GOTO, GOSUB, etc.)
; but only change them in pass 3 (ie. z_p_temp_1 = 0)

imbed_lines
                 jsr reset_txtptr                         ; start at first line: load (txtptr) with (txttab)-1

next_line
                 jsr chargt_x2                            ; skip link (assumes txttab > 0)
                 +lbeq n1_reset                           ; null link: put current line # in fac, reset txtptr, exit
                 jsr chargt                               ; line number
                 sta forpnt                               ; save in case there is an error
                 jsr chargt
                 sta forpnt+1

next_char
                 jsr chargt                               ; first character in the line

chk_quote
                 cmp #'"'                                 ; opening double quote?
                 bne not_quote                            ; no...
l75_1            jsr chargt                               ; scan line
                 beq next_line                            ; end...
                 cmp #'"'                                 ; close double quote
                 bne l75_1                                ; no... continue
                 bra next_char                            ; yes... resume renumber

not_quote
                 tax                                      ; end of line?
                 beq next_line                            ; yes...
                 bpl next_char                            ; not a token...

                 ldx #8                                   ; check special token list
l76_1            cmp testwd-1,x
                 beq iline_10                             ; a match...
                 dex
                 bne l76_1                                ; continue until zero

                 cmp #go_token                            ; wasn't in the token list. check for 'go to'
                 bne chk_escape                           ; not 'go', go check for 'collision' *c128 fix*
hop_1            jsr chrget                               ; got a 'go', look for 'to'
                 beq next_line                            ; end of line, abort
                 cmp #to_token
                 beq iline_10                             ; got it! go to fix number routine
                 bra next_char                            ; no 'to', keep looking

; Look for 'COLLISION'.  This is an escape command. *c128 fix* ?????????

chk_escape
                 cmp #esc_command_token
                 bne next_char
                 jsr chrget
                 beq hop_1                                ; end of line ,abort
                 cmp #collision_token
                 bne next_char
l77_1            jsr chrget                               ; got it! skip over first argument
                 beq hop_1                                ; end of line, abort
                 cmp #','
                 bne l77_1                                ; not there yet


iline_10
                 lda txtptr                               ; save current txtptr
                 sta oldlin
                 lda txtptr+1
                 sta oldlin+1
                 jsr chrget
                 bcs chk_quote                            ; not a #...
                 jsr linget                               ; get line # from text
                 jsr form_line                            ; replace if this line # > n3
                 lda oldlin                               ; restore old txtptr
                 sta txtptr
                 lda oldlin+1
                 sta txtptr+1

                 jsr chrget                               ; skip over leading spaces
                 dew txtptr                               ; then backup (txtptr) by 1
                 ldx #$ff
                 lda z_p_temp_1                           ; if this is pass2 then don't actually change
                 beq p3code                               ; if z=1 then pass3 => ok to change
                 jsr p2code                               ; renumber 'pass two': trial run to see if enough room
                 jsr chrgot                               ; re-get last character from BASIC text & rts

iline_20
                 cmp #','                                 ; comma from 'on'?
                 beq iline_10                             ; it is...
                 bra chk_quote                            ; no...


;*********** This part of imbed_lines executed in pass 2 only **********

p2code                                                    ; updates text_top without actually changing lines
                 inx
                 lda fbuffr+1,x                           ; get character from number
                 beq l78_3                                ; end of number
                 jsr chrget                               ; get digit from old number
                 bcc p2code                               ; digit...move on

l78_1            inw fndpnt
                 sec                                      ; have we run out of memory (theoretically)?
                 lda fndpnt                               ; (compare with limit-of-memory pointer)
                 sbc max_mem_0
                 lda fndpnt+1
                 sbc max_mem_0+1
                 +lbcs omerr                              ; yes- out of memory error
                 inx                                      ; no - next...
                 lda fbuffr+1,x
                 bne l78_1
l78_2            rts                                      ; no more

l78_3            jsr chrget
                 bcs l78_2                                ; old stuff after # is other char
                 dew fndpnt                               ; digit...move down
                 bra l78_3                                ; still digits...


;*********** This part of imbed_lines executed in pass 3 only **********

p3code
                 inx
                 lda fbuffr+1,x                           ; get character from number
                 beq l79_3                                ; end of number

                 pha                                      ; save digit from new number
                 jsr chargt                               ; get digit from old number
                 cmp #':'                                 ; command terminator or letter?
                 bcs l79_1
                 cmp #' '                                 ; space? (fix for goto10 :rem)
                 beq l79_1
                 sec
                 sbc #'0'                                 ; number?
                 sec
                 sbc #$d0
                 bcc l79_2                                ; digit...move on

l79_1            jsr move_init                            ; other char...move up
                 jsr moveup
                 inw text_top

l79_2            pla
                 phx
                 ldy #0
                 jsr sta_far_txt                          ; put new digit in new number (bleed-thru)
                 plx
                 bra p3code


l79_3            jsr chrget
                 bcs iline_20                             ; old stuff after # is other char

l79_4            jsr move_init                            ; digit...move down
                 jsr movedown
                 dew text_top
                 jsr chrgot
                 bcc l79_4                                ; still digits...

                 bra iline_20                             ; branch always


;*************************** FORM_LINE *********************************

; Remaps the destination line if it is greater than n3

form_line
                 jsr n1_reset
find_it
                 jsr chargt_x2                            ; new line, skip over link
                 bne l80_1                                ; if we get to end-of-text without finding the
                 ldx #err_ref                             ; line # then 'unresolved reference' error
                 lda forpnt
                 sta curlin                               ; fake error routine into saying 'in line xxxxx'
                 lda forpnt+1
                 sta curlin+1
                 +lbra error

l80_1            jsr chargt                               ; get line number low
                 sta highds                               ; highds = current line# in loop
                 cmp linnum
                 bne l80_4
                 jsr chargt                               ; get line number high
                 sta highds+1
                 cmp linnum+1
                 bne l80_5
                 sec                                      ; if linnum < start#, no remapping
                 sbc hightr+1
                 bcc l80_2
                 bne l80_3
                 lda linnum
                 sbc hightr
                 bcs l80_3

l80_2            lda linnum                               ; use same line#
                 sta facho+1
                 lda linnum+1
                 sta facho

l80_3            ldx #$90                                 ; make replacement string
                 sec
                 jsr floatc
                 +lbra fout


l80_4            jsr chargt
                 sta highds+1                             ; (** 01/27/84 fix)

l80_5            jsr line_add                             ; scan to end of line
                 bra find_it                              ; always


;*************************** N1_RESET **********************************

; Copies n1 (new renumber origin) into facho & sets (txtptr) = (txttab)-1

n1_reset
                 lda renum_tmp_1
                 sta facho+1
                 lda renum_tmp_1+1
                 sta facho
                 +lbra reset_txtptr


;*************************** LINE_ADD **********************************

; Adds n2 (new line increment) to line number stored in facho if the
; current line number (highds) >= n3 (line to start renumbering with).
; The line is then scanned.

line_add
                 lda highds                               ; if line# >= start# then incr new#
                 sec
                 sbc hightr
                 lda highds+1
                 sbc hightr+1
                 bcc scan_thru

line_inc
                 jsr new_num

scan_thru
                 jsr chargt                               ; scan to end of line
                 bne scan_thru
                 rts


;**************************** NEW_NUM **********************************

; Adds n2 (the new line increment) to the line number stored in facho.

new_num
                 lda facho+1                              ; increment new line#
                 clc
                 adc renum_tmp_2
                 sta facho+1
                 lda facho
                 adc renum_tmp_2+1
                 sta facho
                 rts


;********************** CHARGT & CHARGT_X2 *****************************

; Chargt simulates chrget but doesn't ignore spaces & carry has no
; significance.  Chargt_x2 executes chargt twice.
; Used by Renumber, Find/Change, etc.

chargt_x2
                 inw txtptr                               ; jsr chargt
chargt
                 ldy #0                                   ; increment txtptr
                 inw txtptr
                 +lbra indtxt


;***********************************************************************
;************************* MEMORY MOVE ROUTINES ************************
;***********************************************************************

;****************************** MOVEINIT *******************************

; Setup for Renumber memory move.

move_init
                 lda txtptr                               ; index1 = txtptr
                 sta index1
                 lda txtptr+1
                 sta index1+1

                 lda text_top                             ; index2 = text_top
                 sta index2
                 lda text_top+1
                 sta index2+1

                 lda #1                                   ; move 1 character
                 sta count                                ; lo
                 dec
                 sta argmo                                ; hi

                 rts


;****************************** MOVEDOWN *******************************

; Move block of BASIC text from INDEX1+COUNT to INDEX2 down to INDEX1.
; Used by commands Renumber, Find/Change.

movedown
                 sec                                      ; set up DMA list:   [900524]
                 lda index2
                 sbc index1
                 sta dma1_cnt_lo                          ; cnt = index2-index1-count
                 lda index2+1
                 sbc index1+1
                 sta dma1_cnt_hi
                 sec
                 lda dma1_cnt_lo
                 sbc count                                ; lo
                 sta dma1_cnt_lo
                 lda dma1_cnt_hi
                 sbc argmo                                ; hi
                 sta dma1_cnt_hi

                 clc
                 lda index1
                 sta dma1_dest_lo                         ; dest = index1
                 adc count
                 sta dma1_src_lo                          ; src = index1+count
                 lda index1+1
                 sta dma1_dest_hi
                 adc argmo
                 sta dma1_src_hi

                 lda text_bank                            ; bank = BASIC text bank
; and #%00001111  ;      [910520] F018A
                 sta dma1_src_bank
                 sta dma1_dest_bank

execute_DMA1                                              ; [910620] Edit
                 lda #0
                 sta dma1_cmd                             ; command = copy from startpoint
                 sta dma1_subcmd                          ; [910520] F018A

                 sta dma_ctlr+2                           ; dma_list bank
                 lda #>dma1_cmd
                 sta dma_ctlr+1                           ; dma_list hi
                 lda #<dma1_cmd
                 sta dma_ctlr                             ; dma_list lo & trigger
                 rts


;******************************* MOVEUP ********************************

; Move block of BASIC text from INDEX1 to INDEX2 up to INDEX2+COUNT.
; Used by commands Renumber, Find/Change.

moveup
                 sec                                      ; set up DMA list:   [900524]
                 lda index2
                 sbc index1
                 sta dma1_cnt_lo                          ; cnt = index2-index1
                 lda index2+1
                 sbc index1+1
                 sta dma1_cnt_hi

                 dew index2                               ; (index2 = text_top = end+1)
                 clc
                 lda index2
                 sta dma1_src_lo                          ; src = index2
                 adc count
                 sta dma1_dest_lo                         ; dest = index2+count
                 lda index2+1
                 sta dma1_src_hi
                 adc argmo
                 sta dma1_dest_hi

; lda dma_ctlr+3  ;dma controller version    [910520] F018A
; and #1
; beq l81_1   ; F018    removed [910808] F018B
                 lda #%00110000                           ; F018A,B
l81_1            sta dma1_cmd                             ; command=copy, source=start   [910102]
; php
                 lda text_bank                            ; bank = BASIC text bank   [910520] F018A
; plp   ;version?    removed [910808] F018B
; bne l81_2   ; F018A
; and #%00001111  ; F018     [910102]
; ora #%01000000  ;(copy source=endpoint)    [910102]
l81_2            sta dma1_src_bank                        ; banks
                 sta dma1_dest_bank

                 lda #0                                   ; [910219]
; sta dma1_cmd  ; command = copy, source=endpoint
                 sta dma1_subcmd                          ; [910520] F018A
; dec a   ;      [910219]
                 sta dma_ctlr+2                           ; dma_list bank
                 ldy #>dma1_cmd                           ; dma_list
                 lda #<dma1_cmd
                 sty dma_ctlr+1                           ; dma_list hi
                 sta dma_ctlr                             ; dma_list lo & trigger
                 rts

;.end

;[[command.for]]

; FOR
;
; Push the following information on the run-time stack:
;
; (bottom)   highest memory
; =========================
;  txtptr    address of next statement
;  txtptr+1
;  ========
;  curlin+1  current line number
;  curlin
;  ========
;  to lo
;  to mo
;  to moh    'to' value
;  to ho
;  to exp
;  ========
;  step sign
;  step lo
;  step mo
;  step moh  'step' value
;  step ho
;  step exp
;  ========
;  forpnt+1  'for' variable pointer
;  forpnt
;  ========
;  'for' token       <== (tos) top of stack pointer
; ============================
; (top of stack)  lowest memory


for              lda #$80
                 sta subflg                               ; no arrays(), no integers%
                 jsr let                                  ; get & set FOR variables
                 lda #for_token                           ; set up for call to see if
                 jsr search                               ; ..this 'for' variable is unique
                 beq l82_1                                ; branch if not

; If the variable is not unique, (fndpnt) will point to last occurance
; in stack, and we will reset the stack to that point.  Otherwise we
; will adjust the pointer by 'lenfor' and start from that point.

                 lda #lenfor
                 jsr getstk                               ; updates stack pointer, error if overflow
                 jsr movtos                               ; (tos) => (fndpnt)

l82_1            jsr movfnd                               ; (fndpnt) => (tos)   (redundant for new entries)
                 jsr datan                                ; find address of next statement
                 tya                                      ; offset from (txtptr) in y
                 ldy #lenfor-1

                 clc                                      ; Push address of next statement on stack
                 adc txtptr
                 sta (tos),y                              ; (common area)
                 lda txtptr+1
                 adc #0
                 dey
                 sta (tos),y                              ; (common area)

                 lda curlin+1                             ; Push current line number on stack
                 dey
                 sta (tos),y                              ; (common area)
                 lda curlin
                 dey
                 sta (tos),y                              ; (common area)

                 lda #to_token                            ; Look for TO, must appear
                 jsr synchr
                 jsr chknum                               ; get TO value
                 jsr frmnum
                 lda facsgn
                 ora #$7f
                 and facho
                 sta facho

                 ldx #4
                 ldy #lenfor-5
l82_2            lda facexp,x                             ; Push faclo,mo,moh,ho,exp
                 sta (tos),y                              ; (common area)
                 dex
                 dey
                 bpl l82_2

                 lda #<fone                               ; Push STEP value
                 ldy #>fone                               ; (point to default 'one' in ROM)
                 jsr movfm
                 jsr chrgot
                 cmp #step_token
                 bne l82_3                                ; branch if no step given
                 jsr chrget
                 jsr frmnum

l82_3            jsr sign
                 pha                                      ; save sign for a moment
                 jsr round
                 pla

                 ldy #lenfor-10
                 ldx #5
l82_4            sta (tos),y                              ; (common area)
                 lda facexp-1,x
                 dey
                 dex
                 bpl l82_4

                 lda forpnt+1                             ; Finally push pointer to 'for' variable, & 'for' token
                 sta (tos),y                              ; (common area)
                 lda forpnt
                 dey
                 sta (tos),y                              ; (common area)
                 lda #for_token
                 dey
                 sta (tos),y                              ; (common area)
                 rts

;.end
;[[command.delete]]



; Delete a range of source   -or-   Delete a disk file
;
; Syntax: DELETE from# - to# (same range parameters as LIST)
;  DELETE "filename" (same parameters as SCRATCH)

; Determine which form of DELETE we have...

delete           bcc delete_line                          ; branch if a number (assume range parameter)
                 cmp #minus_token
                 beq delete_line                          ; branch if a dash (assume range parameter)
                 +lbra scratch                            ; branch if string (assume filename or U#)

delete_line
                 jsr errind                               ; direct mode only command
                 jsr chrgot                               ; requires line# or range, no default
                 +lbeq snerr                              ; error, none given

                 jsr range                                ; parse range, find starting line, ptr to ending line
                 lda lowtr
                 ldx lowtr+1
                 sta index1                               ; (destination)
                 stx index1+1

                 jsr FindLine                             ; find ending line
                 bcc l83_2                                ; branch if not found
                 ldy #1
                 jsr indlow                               ; if eot, use this ptr.  else, need ptr to next
                 dey
                 tax                                      ; save it in case of swap
                 bne l83_1                                ; branch if not eot (end-of-text)
                 jsr indlow
                 beq l83_2                                ; branch if eot (null link bytes)

l83_1            jsr indlow
                 sta lowtr                                ; (source)
                 stx lowtr+1

l83_2            lda lowtr                                ; check that start <= end
                 sec
                 sbc index1                               ; calculate delta
                 sta count                                ; (count)
                 lda lowtr+1                              ; (does not catch case where
                 sbc index1+1                             ; start>end when end=start+1,
                 sta argmo                                ; but it does no harm)
                 ora count
                 beq fix_links                            ; all done- nothing to move!?
                 +lbcc snerr                              ; error- bad range (start > end)

                 lda text_top                             ; setup for common DMA move routine: [900530]
                 ldx text_top+1
                 sta index2                               ; index2 = top
                 stx index2+1                             ; index1 = destination
; count  = delta

                 jsr movedown                             ; delete the text, then relink & exit



fix_links                                                 ; <<<<<<<<<<<<<<<<<<<<<<<<<<< entry from renumber

                 jsr link_program                         ; relink program
                 lda index1
                 ldx index1+1
                 clc
                 adc #2
                 bcc l84_1
                 inx
l84_1            sta text_top                             ; set eot pointer
                 stx text_top+1
                 rts                                      ; C128-04 fix: was 'jmp ready' (FAB)


;********************************
;*
;*    Input Range Parameters
;*
;********************************

range            beq l85_1                                ; a terminator from chrgot?
                 bcc l85_1                                ; a number?
                 cmp #minus_token                         ; a dash?
                 bne l85_4   ;if it's not a dash, error (C128-03 fix ; FAB)
                 ldy #1
                 jsr indtxt                               ; let's peek, and see what follows the dash!
                 beq l85_4                                ; uh-oh! it's of the form 'delete -' - error
                 cmp #':'                                 ; the other terminator
                 beq l85_4                                ; ..still bad
                 sec                                      ; set up for linget

l85_1            jsr linget                               ; get first #
                 jsr FindLine                             ; find it & set ptrs
                 jsr chrgot                               ; get last char
                 beq l85_2                                ; skip done
                 cmp #minus_token                         ; a dash?
                 bne l85_4                                ; no- syntax error
                 jsr chrget                               ; yes- skip dash
                 jsr linget                               ; get second #
                 bne l85_4                                ; error- wasn't a number

l85_2            lda endchr                               ; was a # input?
                 bne l85_3                                ; yes
                 lda #$ff                                 ; no - make max
                 sta linnum
                 sta linnum+1
l85_3            rts


l85_4            +lbra snerr                              ; syntax error

;.end
;[[command.findchange]]



; FIND   "string"                    [,line_range]
; CHANGE "oldstring" TO "newstring"  [,line_range]
;
; where <"> delimiter can be any character, but only
; double-quotes will prevent tokenization of strings.
;
; N.B.: I am assuming that lines cannot be greater than 255 chars, as is
; the case where the line was entered "normally", that is, using LINGET.

find
                 rmb7 op                                  ; FIND flag
                 !text $2c

change
                 smb7 op                                  ; CHANGE flag
                 rmb6 op                                  ; reset change-all mode
                 jsr errind                               ; report error if not in direct mode

                 jsr chrgot                               ; get delimeter
                 ldx #0                                   ; evaluate string args
                 jsr delimit_string                       ; string1
                 lda fstr1+2
                 +lbeq fcerr                              ; error if string1 null
                 bbr7 op,l86_1                            ; branch if no string2
                 jsr chrget                               ; pick up required 'to' token
                 cmp #to_token
                 +lbne snerr                              ; error if missing
                 jsr chrget
                 +lbeq snerr                              ; error if eol
                 ldx #3
                 jsr delimit_string                       ; string2

l86_1            jsr chrget                               ; line number range given?
                 beq l86_2                                ; no, eol
                 jsr chkcom                               ; yes, pick up required comma
l86_2            jsr range                                ; set up line number range (lowtr,linnum)
                 jsr tto                                  ; save txtptr for restoration when done
                 rmb7 helper                              ; clear 'help' flag for 'p1line'
                 lda helper
                 pha
                 rmb4 helper                              ; temporarily disable token highlighting
                 smb5 helper                              ; set   'find' flag for 'p1line'
                 bra find_loop_1                          ; begin


find_loop
                 ldy #0                                   ; move to next line (copy link bytes to lowtr)
                 jsr indlow
                 tax
                 iny
                 jsr indlow
                 stx lowtr
                 sta lowtr+1

find_loop_1
                 ldy #1
                 jsr indlow                               ; check link
                 bne l87_1                                ; not null- continue
                 dey
                 jsr indlow
                 +lbeq find_exit                          ; null- exit

l87_1            ldy #2
                 jsr indlow                               ; check line number
                 tax
                 iny
                 jsr indlow
                 cmp linnum+1
                 bne l87_2
                 cpx linnum
                 beq l87_3                                ; line is <= last line requested, continue
l87_2            +lbcs find_exit                          ; line is >  last line requested, exit

l87_3            ldx #3                                   ; set initial position - 1 (past link & line#)
                 stx fndpnt


find_loop_2
                 jsr _stop                                ; check stop key
                 +lbeq find_break                         ; exit if down

                 ldx fndpnt                               ; duh, where are we?
                 clc
                 txa                                      ; program:
                 adc lowtr                                ; txtptr = line start + position in line
                 sta txtptr
                 lda #0
                 adc lowtr+1
                 sta txtptr+1                             ; search string:
                 ldz #0                                   ; at the beginning

l88_1            jsr chargt                               ; get next character from text
                 beq find_loop                            ; eol (no match this line)
                 inx                                      ; bump pointer to next character
                 cmp (fstr1),z                            ; character match?  ind okay- buffer
                 bne l88_1                                ; no
                 stx fndpnt                               ; yes- save next position

l88_2            inz                                      ; bump position in search string
                 cpz fstr1+2                              ; string match?
                 bcs print_line                           ; yes
                 jsr chargt
                 beq find_loop                            ; no- eol
                 cmp (fstr1),z                            ; ind okay- buffer
                 bne find_loop_2                          ; no- rewind to beginning of search string
                 beq l88_2                                ; maybe- still more chars to compare


; Print the line of text at LOWTR, highlighting the section of code
; beginning at LOWTR+FNDPNT and running for FIND_COUNT characters.

print_line
                 jsr crdo                                 ; get a new display line
                 lda fstr1+2                              ; length of string to highlight
                 sta find_count
                 ldy #2
                 jsr indlow                               ; get ms byte of line number
                 tax
                 iny
                 jsr indlow                               ; get ls byte
                 jsr p1line                               ; print #, space, and the line of code
                 bbr7 op,find_loop_2                      ; Find op? branch if so and continue search


; Change operation
; Query the user and replace string1 with string2 if he wants to.
; Options are  'Y' (yes),  '*' (do all),  'CR' (quit),  anything else means no.

change_line
                 bbs6 op,l89_1                            ; branch if change-all mode set
                 jsr _primm                               ; prompt & get response
                 !text cr," CHANGE? ",0
                 jsr response_get
                 cmp #'Y'
                 beq l89_1                                ; yes, change it
                 cmp #cr
                 +lbeq find_exit                          ; cr only, abort entire operation
                 cmp #'*'
                 bne find_loop_2                          ; *, change all.  else don't change
                 smb6 op

; Replace string1 with string2.  Requires moving text up/down beginning at
; LOWTR+FNDPNT+(LEN(string1)-LEN(string2)) through TEXT_TOP and copying
; string1 into text beginning at LOWTR+FNDPNT for LEN(string2) characters.

l89_1            lda text_top                             ; setup upper address of text to move (index2)
                 sta index2
                 lda text_top+1                           ; TEXT_TOP
                 sta index2+1

                 clc                                      ; setup lower address of text to move (index1)
                 lda fndpnt
                 adc lowtr
                 sta index1                               ; LOWTR+FNDPNT
                 lda #0
                 sta argmo                                ; count hi
                 adc lowtr+1
                 sta index1+1

                 sec                                      ; calc number of chars to insert/delete
                 lda fstr1+2                              ; LEN(string1)-LEN(string2)
                 sbc fstr2+2
                 beq l89_6                                ; branch if string1 = string2 (no move)
                 bpl l89_4                                ; branch if string1 > string2 (delete)
; else      string1 < string2 (insert)

                 neg                                      ; Move memory up to make room for larger string2
                 sta count
                 ldy #0                                   ; first check for line too long
                 jsr indlow
                 adc count
                 taz
                 iny
                 jsr indlow                               ; (link+#chr)-line_sa must be <256
                 adc #0
                 tay
                 sec
                 tza
                 sbc lowtr
                 tya
                 sbc lowtr+1
                 +lbne errlen                             ; error, line > 255 characters

                 clc                                      ; now check for sufficient memory
                 ldy text_top+1
                 lda count
                 adc text_top
                 bcc l89_2
                 iny
l89_2            cpy max_mem_0+1
                 bcc l89_3                                ; result is less than top-of-memory: ok
                 +lbne omerr                              ; msb >  top, overflow
                 cmp max_mem_0                            ; msb's the same, test lsb's
                 +lbcs omerr                              ; lsb >= top, overflow
l89_3            sta text_top
                 sty text_top+1                           ; set new top of text pointer
                 jsr moveup                               ; make room
                 bra l89_6                                ; go copy string2 into area

l89_4            sta count                                ; Move memory down for smaller string2
                 ldy text_top+1
                 lda text_top
                 sec
                 sbc count
                 bcs l89_5
                 dey
l89_5            sta text_top
                 sty text_top+1                           ; set new top of text pointer
                 jsr movedown                             ; squish out excess space

l89_6            lda fstr2+2                              ; Copy string2 into text
                 beq l89_8                                ; branch if null, nothing to copy
                 sta find_count                           ; how many characters to copy
                 ldx #lowtr
                 ldy fndpnt                               ; index into text
                 ldz #0                                   ; index into string2
l89_7            lda (fstr2),z                            ; ind okay- buffer
                 jsr sta_far_ram0                         ; do the copy
                 iny
                 inz
                 dec find_count
                 bne l89_7

l89_8            jsr link_program                         ; relink program
                 clc
                 lda fndpnt                               ; place find position after new text
                 adc fstr2+2
                 dec
                 sta fndpnt
                 +lbra find_loop_2                        ; and resume searching


find_exit
                 jsr crdo                                 ; normal exit
                 pla
                 sta helper                               ; restore token highlight status
                 rmb5 helper                              ; remove 'find' flag
                 +lbra direct_mode_exit                   ; done



find_omerr                                                ; out of memory
                 ldx #errom
                 !text $2c
find_errlen                                               ; string too long
                 ldx #errls
                 sec
                 !text $89
find_break                                                ; stop key break
                 clc
                 pla
                 sta helper                               ; restore token highlight status
                 rmb5 helper                              ; remove 'find' flag
                 +lbcc break_exit                         ; [910925]
                 +lbra error


delimit_string                                            ; command is in buffer, .x = ptr to strptr
                 sta match                                ; delimiter character
                 lda txtptr                               ; point to first character in string
                 inc                                      ; (never wraps- string in input buffer)
                 sta fstr1,x                              ; set pointer to string data
                 lda txtptr+1
                 sta fstr1+1,x
                 lda #$ff                                 ; set string length
                 sta fstr1+2,x

l90_1            inc fstr1+2,x
                 jsr chargt                               ; build string
                 +lbeq snerr                              ; error if eol encountered inside string
                 cmp match
                 bne l90_1                                ; continue until matching delimiter found
                 rts

;.end



puctrl           jsr frmstr                               ; do frmevl,frestr. return with a=len, index=~string
                 tay
                 dey
                 cpy #4
                 +lbcs fcerr                              ; len > 4 is illegal value error

l91_1            jsr indin1_ram1                          ; lda (index),y
                 sta puchrs,y
                 dey
                 bpl l91_1
                 rts

;.end

;[[command.trap]]


trap
; jsr errdir ;why not????      [910925]
                 jsr chrgot                               ; if no #, means 'turn off trap'
                 beq l92_1
                 jsr getwrd
                 sty trapno
                 !text $2c

l92_1            lda #$ff                                 ; flag no trap
                 sta trapno+1
                 rts

;.end
;[[command.resume]]




; RESUME command
;
; Used to resume execution following a TRAPped error.
;
; Syntax: RESUME [line_number | NEXT]
;
; Can take the following forms:
;
; RESUME   :resume executing at the statement which caused
;     the error.
; RESUME NEXT  :resume execution at the statement FOLLOWING
;     the statement which caused the error.
; RESUME line_number :resume at the specified line number.


resume           jsr errdir                               ; no direct mode
                 ldx errlin+1                             ; is there an error to resume from?
                 inx
                 beq rescnt                               ; can't resume!
                 jsr chrgot                               ; look for arguments
                 beq resswp                               ; no arg's...restart err'd line
                 bcc l93_3                                ; numeric argument
                 cmp #next_token                          ; only other choice is 'next'
                 +lbne snerr                              ; if not, syntax error

                 jsr resswp                               ; resume execution with next stm't
                 ldy #0
                 jsr indtxt
                 bne l93_2                                ; must be a ':'
                 iny                                      ; must be a null,get next line
                 jsr indtxt                               ; make sure its not end-of-text
                 bne l93_1
                 iny
                 jsr indtxt
                 +lbeq ready                              ; 2 nulls, eot. bye!

l93_1            ldy #3                                   ; new line, update pointers
                 jsr indtxt
                 sta curlin
                 iny
                 jsr indtxt
                 sta curlin+1
                 tya
                 clc
                 adc txtptr
                 sta txtptr
                 bcc l93_2
                 inc txtptr+1
l93_2            jsr chrget                               ; skip over this character, into body of statement
                 +lbra data                               ; advance until null or ':', then rts


l93_3            jsr getwrd                               ; resnum. numeric argument
                 sta linnum+1
                 jsr resend
                 +lbra luk4it


resswp           lda errtxt                               ; backup one so chrget will work
                 bne l94_1
                 dec errtxt+1
l94_1            dec errtxt

                 ldx #1
l94_2            lda errlin,x                             ; restore line#
                 sta curlin,x
                 lda errtxt,x                             ; restore text pointer to statement
                 sta txtptr,x
                 dex
                 bpl l94_2


resend           ldx tmptrp                               ; restore trap line to allow traps again
                 stx trapno+1
error_clear
                 ldx #$ff
                 stx errnum                               ; reset error status- he's saying he's fixed it
                 stx errlin
                 stx errlin+1                             ; flag 'no further resumes until next error'
                 rts


rescnt           ldx #errcr
                 +lbra error

;.end

;[[command.loops]]


do               ldy #1
l95_1            lda txtptr,y                             ; save current pointers for stack entry
                 sta tmptxt,y
                 lda curlin,y
                 sta tmplin,y
                 dey
                 bpl l95_1

                 jsr chrgot                               ; look for 'while' or 'until'
                 beq doyes                                ; unconditional
                 cmp #until_token
                 beq do10
                 cmp #while_token
                 bne snrjmp


;  Here for WHILE

                 jsr frmjmp
                 lda facexp
                 bne doyes                                ; conditional evaluated true

dono             jsr chrgot
                 bra fnd010                               ; advance to end of block, do rts


;  Here for UNTIL

do10             jsr frmjmp
                 lda facexp
                 bne dono

doyes            lda #5                                   ; 'do' needs 5 bytes on the run-time stack
                 jsr getstk
                 ldy #4                                   ; ..now stuff those 5 bytes!
                 lda tmptxt+1
                 sta (tos),y                              ; (common area)
                 dey
                 lda tmptxt
                 sta (tos),y                              ; (common area)
                 dey
                 lda tmplin+1
                 sta (tos),y                              ; (common area)
                 dey
                 lda tmplin
                 sta (tos),y                              ; (common area)
                 dey
                 lda #do_token
                 sta (tos),y                              ; (common area)
                 rts


;  Here for EXIT

exit             jsr popdgo                               ; pop do entry off stack
                 jsr chrgot
                 beq fnd010
snrjmp           +lbra snerr



;  Find end of current block

fndend           jsr chrget

fnd010           beq l96_2                                ; end of statement
                 cmp #loop_token
                 +lbeq data                               ; a hit!  read to end of statement, rts
                 cmp #'"'                                 ; quote
                 beq l96_1
                 cmp #do_token
                 bne fndend                               ; keep looking
                 jsr fndend                               ; recursivly
                 bra dono                                 ; do a chrgot, go to fnd010


l96_1            jsr un_quote                             ; look for terminating quote, or end of statement
                 bne fndend                               ; character after quote wasn't terminator, keep going

l96_2            cmp #':'                                 ; end of line or end of stmt?
                 beq fndend                               ; just stmt, keep going
                 bbr7 runmod,fnderr                       ; if direct mode, not found error
                 ldy #2
                 jsr indtxt                               ; end of text?
                 beq fnderr                               ; 'fraid so
                 iny                                      ; y=3
                 jsr indtxt                               ; update pointers
                 sta curlin
                 iny
                 jsr indtxt
                 sta curlin+1
                 tya
                 clc
                 adc txtptr
                 sta txtptr
                 bcc fndend
                 inc txtptr+1
                 bra fndend


loop             beq popngo                               ; no conditionals, just do it
                 cmp #while_token
                 beq loop10
                 cmp #until_token
                 bne snrjmp

;  Here for UNTIL

                 jsr frmjmp
                 lda facexp
                 beq popngo                               ; false, do it again!

popdgo           lda #do_token                            ; pop, but don't go
                 jsr search
                 bne poperr                               ; branch if not found
                 jsr movfnd
                 ldy #5
                 +lbra rlsstk


fnderr
                 lda tmplin                               ; loop not found error: must make curlin match oldtxt
                 ldx tmplin+1
                 sta curlin
                 stx curlin+1

                 ldx #errlnf
                 !text $2c
poperr
                 ldx #errlwd                              ; loop without do
                 +lbra error



;  Here for WHILE

loop10
                 jsr frmjmp
                 beq popdgo                               ; false, exit
popngo
                 bsr popdgo
; dey
; lda (fndpnt),y ;restore pointers
; sta txtptr+1
; dey
; lda (fndpnt),y
; sta txtptr
; dey
; lda (fndpnt),y
                 jsr retpat                               ; (** 01/18/84 fixes 'loop' to a direct mode 'do')
; lda (fndpnt),y
; sta curlin
                 +lbra do

frmjmp
                 jsr chrget
                 +lbra frmevl

;.end
;[[command.key]]



;**************************************************************
;
;   KEY  Programmable Key Functions    [900725]
;
;**************************************************************

key              beq Key_List                             ; KEY ? yes- no args

l97_1            ldx _kyndx                               ; is function key buffered?
                 bne l97_1                                ; yes- hang until IRQ finishes processing it ????

                 cmp #on_token                            ; KEY ON ?
                 bne l97_2
                 rmb5 _locks                              ; yes- reset Editor's lock bit
                 bra l97_4                                ; exit

l97_2            cmp #load_token                          ; KEY LOAD <filename>[,D#,U#]
                 +lbeq Key_load

                 cmp #save_token                          ; KEY SAVE <filename>[,D#,U#]
                 +lbeq Key_Save

                 cmp #restore_token                       ; KEY RESTORE ?      [910925]
                 bne l97_3                                ; no
                 rmb5 _locks                              ; yes- reset Editor's lock bit (enable keys)
                 jsr key_restore                          ; init key definitions
                 bra l97_4                                ; exit

l97_3            cmp #esc_command_token                   ; KEY OFF ?
                 +lbne Key_Change                         ; no- must be new key definition
                 jsr chrget
                 cmp #off_token
                 +lbne snerr                              ; no- bad syntax
                 smb5 _locks                              ; yes- set Editor's lock bit
l97_4            +lbra chrget                             ; exit


;**************************************************************
;
;   Key_List  List all function key definitions
;
;**************************************************************

Key_List
                 ldx #0                                   ; display all key definitions
                 ldy #0

lstky1           inx                                      ; get key number = 1-16
                 lda _pky_lengths-1,x                     ; get key size
                 beq lstest                               ; skip if key not defined
                 sta keysiz                               ; save size
                 stx z_p_temp_1                           ; save key number

                 phy
                 ldx #3
l98_1            lda preamb,x                             ; print key preamble:
                 jsr _bsout
                 dex                                      ; 'KEY '
                 bpl l98_1
                 ldx z_p_temp_1                           ; key number
                 lda #0
                 jsr linprt
                 lda #','
                 jsr _bsout                               ; comma

                 ply
                 ldx #7                                   ; (length of 'keydat' string)
lsloop           lda _pky_buffer,y                        ; print key definition
                 iny
                 pha                                      ; save character
                 phx                                      ; save position in output string

                 ldx #4                                   ; check for special (non-printable) characters
l99_1            cmp keychr-1,x
                 beq list_special                         ; yes, display it as 'CHR$(...)'
                 dex
                 bne l99_1

                 plx                                      ; restore position
                 cpx #8
                 bcc l99_2                                ; 1st time thru- display leading quote
                 bne l99_3                                ; previous was a character- no additions needed
                 lda #'+'                                 ; add since previous was quote or return
                 jsr _bsout
l99_2            lda #'"'
                 jsr _bsout                               ; add leading quote
l99_3            pla                                      ; restore character
                 jsr _bsout                               ; display it
                 ldx #9                                   ; mark normal character

lstnd            dec keysiz
                 bne lsloop                               ; loop to end of definition
                 cpx #9
                 bcc l100_1                               ; skip if previous not normal character
                 lda #'"'
                 jsr _bsout                               ; add ending quote

l100_1           lda #$8d
                 jsr _bsout                               ; add ending return (shifted)

                 ldx z_p_temp_1                           ; key number
lstest           cpx #number_fkeys
                 bne lstky1                               ; ...loop until done all keys
                 rts


list_special
                 plx                                      ; restore .x
l101_1           lda keydat-3,x                           ; display something like  ' "+CHR$( '
                 jsr _bsout
                 dex
                 cpx #3
                 bcs l101_1
                 pla                                      ; restore character
                 jsr prtdec                               ; display decimal value of chr in .a
                 lda #')'                                 ; finish off with closing paren.
                 jsr _bsout
                 ldx #8                                   ; mark end of special
                 bra lstnd                                ; ..always


preamb           !text " YEK"                             ; key preamble

keydat           !text "($RHC+",$22                       ; chr$( string

keychr           !text cr,$8d,$22,esc                     ; special KEY chars- return, sft-return, quote, esc


;************************************************************************
;
;   Key_Change  Add, Delete or Change function key definition
;
;************************************************************************

Key_Change
                 jsr getbyt                               ; get key number (1-16)
                 stx z_p_temp_1                           ; save key number     [910925]
                 dex
                 cpx #number_fkeys
                 +lbcs fcerr                              ; exit - key number invalid

; stx z_p_temp_1 ;save key number
                 jsr chkcom                               ; look for comma
                 jsr frmstr                               ; do frmevl, frestr. returns len in .a, addr in 'index'
                 tay                                      ; set up for call to do-a-key
                 lda #1                                   ; tell do-a-key that string is in bank 1
                 sta index+2
                 lda #index                               ; now .A points to (adr lo, adr hi, bank #)
                 ldx z_p_temp_1
; inx         [910925]
key_restore
; jsr put_io_in_map
                 jsr _doakey                              ; re-define the key
                 +lbcs omerr                              ; bad return (.c=1)
                 rts                                      ; ok return  (.c=0)


;************************************************************************
;   Key_Load  Load function key definitions (from disk)   [900725]
;************************************************************************

Key_load
                 jsr GetLoadChannel                       ; get a channel      [911001]
                 ldy #>_pky_lengths
                 lda #<_pky_lengths
                 jsr LoadBlock                            ; load it
                 +lbra list_err                           ; release channel, close file, return to main


GetLoadChannel                                            ; Used by KeyLoad and SpriteLoad    [911001]
                 jsr chrget                               ; eat LOAD token
                 lda #$e6                                 ; parse:  filename [,U#] [,D#]
                 jsr dosprs                               ; (like dopen:  0 0 0 *  * 0 0 1 )
                 jsr chk1                                 ; check parameters
                 lda #0
                 sta dossa                                ; setup as dload would (0 = load channel)
                 jsr find_la                              ; find an unused LA to use (cannot use reserved one)
                 ldy #fopn
                 ldx #4
                 jsr open_file                            ; open the file
                 bcs LoadEOF                              ; exit if problem
                 ldx dosla
                 jsr _chkin                               ; get input channel
                 bcs LoadEOF                              ; exit if bad??
                 jsr _basin                               ; waste dummy load address
                 jsr _basin
                 jsr _readst                              ; prevent corruption if there's a problem
                 bne LoadERR                              ; exit if problem
                 clc
                 rts


LoadBlock
                 sta highds                               ; where to put data
                 sty highds+1
LoadBlockNext
                 ldy #0
l102_1           jsr _basin                               ; read definitions
                 sta (highds),y
                 jsr _readst                              ; check channel status
                 bne LoadEOF                              ; exit if eof or error
                 jsr _stop
                 beq LoadEOF                              ; exit if stop key down
                 iny
                 bne l102_1                               ; continue up to 1 page maximum
                 clc                                      ; indicate "more"
                 rts


LoadERR
                 ldx #erload                              ; Load Error
                 sec
                 !text $89

LoadEOF
                 clc
                 +lbra list_err                           ; release channel, close file, return to main


;************************************************************************
;   Key_Save  Save function key definitions (from disk)   [900725]
;************************************************************************

Key_Save
                 jsr GetSaveChannel                       ; [910930]
                 lda #highds                              ; set starting & ending addresses
                 ldy #>_pky_lengths                       ; start address & pointer to it
                 ldx #<_pky_lengths
                 sty highds+1
                 stx highds
                 iny                                      ; end address = start address + 256 + 1
                 inx
                 +lbra savenb                             ; [910925]
; jsr _savesp  ;save it
;; clc   ; return no error  ????why not  [910404]
; bra exit_disk_op ; but if direct mode print DOS error  [910404]



GetSaveChannel                                            ; Used by KeySave and SpriteSave    [910930]
                 jsr chrget                               ; eat SAVE token
                 lda #$66                                 ; parse:  filename [,U#] [,D#]
                 jsr dosprs                               ; parse options
                 jsr chk2                                 ; check required parameters

                 ldy #fopn                                ; DOS table offset
                 lda #4                                   ; length
                 jsr sendp
                 lda #sys_bank                            ; set banks ???? buffer in system bank ????
                 ldx #sys_bank
                 jmp _setbank

;.end

;[[command.bank]]


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

bank             jsr getbyt                               ; get bank number in .x
                 stx current_bank
                 rts


;.end
;[[command.play]]



; C65 Music Interpreter
;
; Syntax : PLAY "music_string"
;
; Where : music_string is a string of characters composed of:
;
; A..G   : notes
; W,H,Q,I,S : set note lengths to whole,half,quarter,eighth,sixteenth
; U   : set volume level   (0-9)
; O   : set octave    (0-6)
; T   : set current envelope  (0-9)
; V   : select voice to play  (1-6: 1-3 right, 4-6 left)
; X   : filter    (0-1: 0=off, 1=on)
; M   : measure
; R   : rest
; .   : dot
; #   : sharp
; $   : flat


play             jsr frmstr                               ; frmevl,frestr,return w/ .A=len, (index)=>string
; sta sw_rom_ram0  ;????
                 sta z_p_temp_1                           ; save number of characters
                 jsr clear_play_flags                     ; set 'dot' and 'sharp' to 0. return with Acc=0
                 sta hulp                                 ; zero counter

l103_1           ldy hulp
                 cpy z_p_temp_1
                 beq play_rts                             ; done!
                 jsr indin1_ram1
                 jsr play_one_character
                 inc hulp
                 bne l103_1                               ; always
play_rts
                 rts


play_one_character
                 cmp #' '                                 ; spaces are a 'no-op'
                 beq play_rts

l104_1           cmp #'A'                                 ; note name a-g?
                 bcc l104_2
                 cmp #'H'
                 +lbcc play_note                          ; yes...play it

l104_2           ldx #4                                   ; test for notes,'w,h,q,i,s'
l104_3           cmp notes,x
                 +lbeq set_note_length
                 dex
                 bpl l104_3

                 cmp #'R'                                 ; rest?
                 +lbeq play_rest
                 cmp #'.'                                 ; dotted note?
                 +lbeq play_dot

                 ldx #5                                   ; test for v,o,t,x,u,m commands
l104_4           cmp mutabl,x
                 +lbeq play_command
                 dex
                 bpl l104_4                               ; test all 5 characters in table

                 cmp #'#'                                 ; sharp?
                 +lbeq play_sharp
                 cmp #'$'                                 ; flat?
                 +lbeq play_flat


; Must be a digit here for Octave, Voice, envelope (T), filter (X), or volume (U)

                 sec
                 sbc #'0'                                 ; mask nybble
                 cmp #10                                  ; must be in range 0..9
                 +lbcs play_bad_value

                 asl flag                                 ; octave, voice, envelope, filter, or volume?
                 bcs set_voice
                 asl flag                                 ; octave, envelope, filter, or volume?
                 bcs set_octave
                 asl flag                                 ; envelope, filter, or volume?
                 bcs set_envelope
                 asl flag                                 ; filter or volume?
                 bcc set_volume

set_filter
                 jsr wait_for_all_quiet                   ; [910722]
                 cmp #2
                 +lbcs play_bad_value                     ; value too large
                 lsr                                      ; .c=on/off
                 ldy voice                                ; 0-5
                 ldx filter_offset,y                      ; 0 0 0 4 4 4
                 lda filters1+2,x                         ; get current filter data for this SID  [910612]
                 ora vbits,y                              ; update filter voice bit
                 bcs l105_1                               ; branch to turn filter on
                 eor vbits,y                              ; else, turn filter off   [910612]

l105_1           sta filters1+2,x
; lda filters1+3,x ;why????     [910612]
; sta filters1+4,x ;save new filter-type/volume

; jsr put_io_in_map
                 lda SID_offset,y                         ; get hardware offset for current voice
                 and #$f0                                 ; $00 or $20
                 tay
; jsr go_slow  ;      [910716] 4567R7A
                 ldz #3
l105_2           lda filters1,x                           ; update the hardware
                 sta sid1+21,y
                 inx
                 iny
                 dez
                 bpl l105_2
; jsr go_fast  ;      [910716] 4567R7A
                 bra clear_flag                           ; always


set_voice
                 dec
                 cmp #6                                   ; stereo SIDs: 0-2=right, 3-5=left  [910612]
                 +lbcs play_bad_value
                 sta voice                                ; 0-5
                 bra clear_flag                           ; always


set_octave
                 cmp #7
                 +lbcs play_bad_value                     ; too big octave
                 sta octave                               ; set octave
                 bra clear_flag                           ; always


set_envelope
                 jsr wait_for_quiet                       ; [910626]
                 tax
set_envelope_1                                            ; entry for initialization code
; jsr put_io_in_map
                 ldy voice
                 lda wavtab,x
                 sta waveform,y                           ; set waveform
                 lda SID_offset,y                         ; get hardware offset for this voice
                 tay
; jsr go_slow  ;      [910716] 4567R7A
                 lda atktab,x
                 sta sid1+5,y                             ; set attack/decay
                 lda sustab,x
                 sta sid1+6,y                             ; set sustain/release
                 lda pulslw,x
                 sta sid1+2,y                             ; set pulse width - low byte
                 lda pulshi,x
                 sta sid1+3,y                             ; set pulse width - high byte
; jsr go_fast  ;      [910716] 4567R7A
                 bra clear_flag


set_volume
                 jsr wait_for_all_quiet                   ; [910626]
                 tax
                 ldy voice                                ; [910612]
                 lda filter_offset,y                      ; get filter offset for this voice
                 tay
                 lda filters1+3,y                         ; get mode/volume for this SID
                 and #$f0                                 ; mask out old volume
                 ora voltab,x                             ; add new volume
                 sta filters1+3,y                         ; save for filter change
; lda filters1+4,y ;get current filter-type/volume ????why  [910612]
; and #$f0
; ora voltab,x
                 tax
                 ldy voice
                 lda SID_offset,y                         ; get hardware offset for current voice
                 and #$f0                                 ; $00 or $20
                 tay
; jsr go_slow  ;      [910716] 4567R7A
                 stx sid1+24,y                            ; set new volume
; jsr go_fast  ;      [910716] 4567R7A
;fall into clear_flag

clear_flag
                 lda #0
                 sta flag
                 rts


go_fast
                 lda sid_speed_flag
                 tsb vic+49
                 rts


go_slow
                 pha
                 lda #$40
                 and vic+49
                 trb vic+49
                 sta sid_speed_flag
                 pla
                 rts


wait_for_quiet                                            ; Wait for current voice to be quiet  [910626]
                 ldy voice
                 ldx times2,y                             ; voice*2
l106_1           bit voices+1,x                           ; test if voice is active   [910617]
                 bpl l106_1                               ; loop until inactive (IRQ)
                 rts


wait_for_all_quiet                                          ; Wait for all voices on this SID to be quiet [910626]
                 ldy #3
                 ldx voice
                 cpx #3                                   ; determine left/right SID
                 bcs l107_1
                 ldy #0
l107_1           ldz #3                                   ; for each of 3 voices
l107_2           ldx times2,y
l107_3           bit voices+1,x                           ; wait for voice to be inactive (IRQ)
                 bpl l107_3
                 iny                                      ; next voice
                 dez
                 bne l107_2                               ; until done 3 voices
                 rts


play_bad_value
                 jsr clear_flag
                 +lbra fcerr                              ; illegal quantity

play_dot
                 sta dnote
                 rts



set_note_length
; ldy #<beats  ;found note (.x), divide beats accordingly
; sty ntime
; ldy #>beats
; sty ntime+1

                 bit _pal_ntsc                            ; determine if PAL or NTSC system  [910724]
                 bmi l108_1                               ; ...branch if PAL
                 ldz #<beats_ntsc                         ; (whole note 4/4 time = 2 sec)
                 ldy #>beats_ntsc
                 bra l108_2
l108_1           ldz #<beats_pal
                 ldy #>beats_pal
l108_2           stz ntime
                 sty ntime+1

l108_3           dex
                 bmi l108_4                               ; finished dividing, exit
                 lsr ntime+1
                 ror ntime
                 bra l108_3

l108_4           rts


play_note
                 sec
                 sbc #'A'
                 tax
                 lda scalen,x                             ; note #0-11
                 tax
                 lda #6
                 sec
                 sbc octave
                 tay
                 txa
                 clc
                 adc sharp
                 bpl l109_1                               ; added sharp or nat
                 lda #11                                  ; underflow
                 iny                                      ; bump octave down
l109_1           cmp #12                                  ; overflow?
                 bcc l109_2                               ; no...
                 lda #0
                 dey                                      ; bump octave up
l109_2           tax
                 lda scalel,x
                 sta pitch

                 bit _pal_ntsc                            ; determine if PAL or NTSC system
                 bmi l109_3                               ; ...branch if PAL
                 lda scaleh,x                             ; continue as before patch
                 bra l109_4

l109_3           lda scalelp,x                            ; load from PAL tables
                 sta pitch
                 lda scalehp,x

l109_4           dey
                 bmi play_note_1                          ; go play note
                 lsr
                 ror pitch
                 bra l109_4


play_command
                 cmp #'M'                                 ; measure?
                 beq l110_1

                 lda rbits,x                              ; all others, set flag for next number
                 sta flag
                 rts

; Wait for msb of all 3 voice counters to underflow

;l110_1 ldy #5
;l110_2 lda voices,y
; bpl l110_2
; dey
; dey
; bpl l110_2
; rts

l110_1           ldy #5                                   ; [910626]
l110_2           ldx times2,y
l110_3           bit voices+1,x                           ; wait for voice to be inactive (IRQ)
                 bpl l110_3
                 dey                                      ; next voice
                 bpl l110_2                               ; until done 6 voices
                 rts



play_sharp
                 lda #1
                 !text $2c
play_flat
                 lda #$ff
                 sta sharp
                 rts


play_note_1                                               ; play a note
                 sta pitch+1
                 lda #0                                   ; flag 'not rest'
                 !text $2c                                ; hop
play_rest
                 lda #$ff                                 ; flag 'rest'
                 pha                                      ; save flag
                 ldx voice
                 ldy times2,x                             ; y=x*2
l111_1           lda voices+1,y                           ; test if there is a note playing
                 bpl l111_1                               ; and loop if so

                 sei
                 lda ntime                                ; load counter for current length
                 sta voices,y
                 lda ntime+1
                 sta voices+1,y
                 lda dnote                                ; test if this is a dotted note
                 beq l111_2                               ; no
                 lda ntime+1
                 lsr                                      ; duration is 1.5 x current length
                 pha
                 lda ntime
                 ror
                 clc
                 adc voices,y
                 sta voices,y
                 pla
                 adc voices+1,y
                 sta voices+1,y

l111_2           pla                                      ; test if this is a rest
                 bmi l111_3                               ; and branch if so- clear play flags and exit [910722]

; jsr put_io_in_map
; jsr go_slow  ;      [910716] 4567R7A
                 ldy SID_offset,x                         ; get offset to voice hardware
                 lda pitch
                 sta sid1,y
                 lda pitch+1
                 sta sid1+1,y
                 lda #$08                                 ; reset this voice
                 sta sid1+4,y
                 lda waveform,x                           ; and finally, turn on gate
                 sta sid1+4,y
; jsr go_fast  ;      [910716] 4567R7A
l111_3           cli


clear_play_flags
                 lda #0
                 sta sharp                                ; clear flags
                 sta dnote
                 cli
                 rts


tempo            jsr getbyt                               ; duration of whole note 4/4 time = 24/rate
                 txa
                 +lbeq fcerr                              ; can't be zero- illegal quantity error
                 stx tempo_rate
                 rts


times2           !text 0,2,4,6,8,10                       ; [910612] stereo

notes            !text "WHQIS"                            ; sixteenth,eigth,quarter,half,and whole notes

mutabl           !text "VOTXUM"                           ; voice,octave,envelope,filter,volume,& measure

scalen           !text 9,11,0,2,4,5,7                     ; a,b,c,d,e,f,g

scalel           !text $0f,$0c,$46,$bf,$7d,$83            ; c,c#,d,d#,e,f,f#,g,g#,a,a#,b (NTSC, octave 6)
                 !text $d6,$7a,$73,$c8,$7c,$97            ; [910729]

scaleh           !text $43,$47,$4b,$4f,$54,$59            ; c,c#,d,d#,e,f,f#,g,g#,a,a#,b (NTSC, octave 6)
                 !text $5e,$64,$6a,$70,$77,$7e            ; [910729]

scalelp          !text $87,$8b,$cc,$4e,$14,$24            ; c,c#,d,d#,e,f,f#,g,g#,a,a#,b (PAL,  octave 6)
                 !text $80,$2d,$32,$91,$52,$7a            ; [910729]

scalehp          !text $43,$47,$4b,$50,$55,$5a            ; c,c#,d,d#,e,f,f#,g,g#,a,a#,b (PAL,  octave 6)
                 !text $5f,$65,$6b,$71,$78,$7f            ; [910729]

;  Music envelope tables, default values downloaded to RAM:
;
; 0: piano   1: accordion    2: calliope  3: drum     4: flute
; 5: guitar  6: harpsichord  7: organ     8: trumpet  9: xylophone

;  Attack/decay rates

atkmus           !text $09,$c0,$00,$05,$94,$09,$09,$09,$89,$09

;  Sustain/release rates

susmus           !text $00,$c0,$f0,$50,$40,$21,$00,$90,$41,$00

;  Waveform table

wavmus           !text $41,$21,$11,$81,$11,$21,$41,$41,$41,$11

;  Pulse width hi table

pwhmus           !text $06,$00,$00,$00,$00,$00,$02,$08,$02,$00

;  Offset tables

SID_offset
                 !text $00,$07,$0e,$20,$27,$2e            ; [910612] stereo
filter_offset
                 !text 0,0,0,4,4,4

;  Volume levels

voltab           !text 0,1,3,5,7,8,10,12,14,15

;.end
;[[command.filter]]



;******************************************************************
;
;  FILTER  sid, freq, lp, bp, hp, res   --  set values for filter
;
;     sid =  right (1), left (2)
;          freq =  filter frequency (0-1023)
;            lp =  low pass filter on (1) or off (0)
;            bp =  band pass filter on (1) or off (0)
;            hp =  high pass filter on (1) or off (0)
;           res =  resonance (0-15)
;
;******************************************************************

filter           jsr getbyt                               ; get left/right SID    [910612]
                 dex
                 cpx #2
                 +lbcs fcerr
                 lda filter_offset+2,x                    ; get filter offset for specified SID
                 sta z_p_temp_1
                 tax

                 ldy #0
l112_1           lda filters1,x                           ; save current voice's filter params
                 sta fltsav,y
                 inx
                 iny
                 cpy #4
                 bcc l112_1

                 jsr optwrd                               ; get filter frequency
                 bcc l112_2                               ; skip if no value given
                 cmp #8                                   ; test m.s. byte
                 +lbcs fcerr                              ; error if > 2047
                 sty fltsav                               ; save lower byte

; Idea: shift lower 3 bits of upper byte into lower byte, forming bits 10-3

                 sty fltsav+1
                 lsr
                 ror fltsav+1
                 lsr
                 ror fltsav+1                             ; save upper 7 bits (10-3)
                 lsr
                 ror fltsav+1

l112_2           lda #$10                                 ; start at type=LP
                 sta fltflg
                 lda fltsav

l112_3           jsr optbyt                               ; get filter types (LP,BP,HP)
                 bcc l112_6                               ; skip if no value input
                 cpx #1                                   ; (set .c: 0=0, 1=1)
                 bcc l112_4
                 beq l112_4
                 +lbra fcerr                              ; error if >1

l112_4           lda fltsav+3                             ; get filter flags byte
                 ora fltflg                               ; set filter on
                 bcs l112_5                               ; skip if it should be on
                 eor fltflg                               ; turn filter off
l112_5           sta fltsav+3                             ; save value

l112_6           asl fltflg                               ; shift for next filter
                 bpl l112_3                               ; loop 3 times

                 jsr optbyt                               ; get resonance value
                 bcc l112_7                               ; skip if no value given
; cpx #16
; bcs fcerr  ;error if >15
                 jsr chknyb                               ; [910930]
                 txa
                 asl                                      ; shift to upper nibble
                 asl
                 asl
                 asl
                 sta nibble
                 lda fltsav+2                             ; get current value
                 and #$0f                                 ; mask it out
                 ora nibble                               ; add new value
                 sta fltsav+2                             ; save it

l112_7           ldx z_p_temp_1                           ; hardware offset for this voice's filter [910612]
                 ldy #0
l112_8           lda fltsav,y                             ; copy new filter params to hardware
                 sta filters1,x
                 inx
                 iny
                 cpy #4
                 bcc l112_8
                 rts

;.end

;[[command.envelope]]


;****************************************************************
;
;  ENVELOPE n, attack, decay, sustain, release, waveform, pulse width
;        set music envelope
;                n = envelope number (0-9)
;            wave  =   0 : triangle waveform
;                      1 : sawtooth waveform
;                      2 : pulse waveform
;                      3 : noise waveform
;                      4 : ring modulation
;            pulse = pulse width if pulse waveform is selected (0-4095)
;
;******************************************************************

envelope
                 jsr getbyt                               ; get envelope number
                 cpx #10
                 +lbcs fcerr                              ; exit - invalid tone number
                 stx tonnum                               ; save number
                 lda atktab,x                             ; get attack/decay rates
                 sta tonval
                 lda sustab,x                             ; get sustain/release rates
                 sta tonval+1
                 lda wavtab,x                             ; get waveform and filter
                 sta tonval+2

                 ldx #0
l113_1           stx parcnt
                 jsr optbyt                               ; get parameter - attack or sustain
                 bcc l113_2                               ; skip if no input
                 txa
                 asl
                 asl                                      ; shift to upper nibble
                 asl
                 asl
                 sta nibble                               ; save it
                 ldx parcnt
                 lda tonval,x                             ; get current value
                 and #$0f                                 ; mask it out
                 ora nibble                               ; add new value
                 sta tonval,x                             ; save it

l113_2           jsr optbyt                               ; get decay or release rate
                 bcc l113_3                               ; skip if no input
                 txa
                 and #$0f                                 ; use only lower nibble
                 sta nibble                               ; save it
                 ldx parcnt
                 lda tonval,x                             ; get current value
                 and #$f0                                 ; mask it out
                 ora nibble                               ; add new value
                 sta tonval,x                             ; save it

l113_3           ldx parcnt
                 inx
                 cpx #1
                 beq l113_1                               ; loop to do sustain/release rates
                 jsr optbyt                               ; get waveform
                 bcc l113_5                               ; skip if no value
                 lda #$15                                 ; assume ring modulation
                 cpx #4
                 beq l113_4                               ; skip if correct
                 +lbcs fcerr                              ; error if >4
                 lda sbits+4,x                            ; get waveform bit
                 ora #1                                   ; set gate bit

l113_4           sta tonval+2                             ; save waveform

l113_5           jsr optwrd                               ; is there a pulse width arg?
                 bcc l113_6                               ; nope, done

                 tax                                      ; save msb
                 lda tonval+2                             ; get waveform
                 and #$40
                 beq l113_6                               ; skip if not pulse waveform
                 txa
                 ldx tonnum                               ; get envelope number
                 sta pulshi,x                             ; save high byte of pulse width
                 tya
                 sta pulslw,x                             ; save low byte

l113_6           ldx tonnum
                 lda tonval                               ; set inputted values
                 sta atktab,x
                 lda tonval+1
                 sta sustab,x
                 lda tonval+2
                 sta wavtab,x
volrts
                 rts

;.end

;[[command.volume]]


;***************************************************************
;*
;*  VOLUME - set volume of SID chips
;*
;* Syntax : VOLUME [right] [,left]
;*
;* Where  : vol in 0..15
;*
;***************************************************************

volume           +lbeq snerr                              ; stereo parameters    [910612]
                 cmp #','
                 beq l114_1                               ; left volume only
; jsr getbyt  ;right volume in .x
; cpx #16
; bcs fcerr  ;too large
                 jsr getnyb                               ; [910930]
                 stx z_p_temp_1                           ; a temp (sorry fred)

; The way this is done must work with 'PLAY' without too much conflict.
; So, along with setting the SID 'volume' reg, we'll also set up PLAY's
; record of current volume as well.

                 lda filters1+3
                 and #$f0
                 ora z_p_temp_1
                 sta filters1+3

; lda filters1+4  ;???? why     [910612]
; and #$f0
; ora z_p_temp_1
; sta filters1+4

; jsr put_io_in_map
; jsr go_slow  ;      [910716] 4567R7A
                 sta sid1+24
; jsr go_fast  ;      [910716] 4567R7A
                 jsr chrgot
                 beq volrts

l114_1           jsr optbyt                               ; get optional left parameter   [910612]
                 +lbcc snerr                              ; comma but no value not given??
                 jsr chknyb                               ; [910930]
; cpx #16
; bcs fcerr  ;too large
                 stx z_p_temp_1                           ; a temp (sorry fred)

                 lda filters2+3
                 and #$f0
                 ora z_p_temp_1
                 sta filters2+3

; lda filters2+4  ;???? why     [910612]
; and #$f0
; ora z_p_temp_1
; sta filters2+4

; jsr put_io_in_map
; jsr go_slow  ;      [910716] 4567R7A
                 sta sid2+24
; jsr go_fast  ;      [910716] 4567R7A
                 rts

;.end
;[[command.sound]]



;*****************************************************************************
;*
;*  SOUND - Produce sound effects
;*
;* Syntax : SOUND v, f, d [,[dir] [,[m] [,[s] [,[w] [,p] ]]]]
;*
;* Where : v   = voice    (1..6)
;*  f   = frequency    (0..65535)
;*  d   = duration    (0..32767 jiffys)
;*  dir = step direction  (0(up) ,1(down) or 2(oscillate)) default=0
;*  m   = minimum frequency  (if sweep is used) (0..65535) default=0
;*  s   = step value for effects  (0..32767) default=0
;*  w   = waveform  (0=triangle,1=saw,2=square,3=noise) default=2
;*  p   = pulse width  (0..4095) default=2048 (50% duty cycle)
;*
;*****************************************************************************

sound            cmp #clr_token                           ; SOUND CLR: init sound/music environment [910717]
                 +lbeq Sound_CLR                          ; yes

                 jsr getbyt                               ; get voice number in .X
                 dex                                      ; adjust 1..3 to 0..2
                 cpx #6                                   ; [910612]
l115_1           +lbcs fcerr                              ; illegal value

l115_2           stx sound_voice

; Get frequency

                 jsr comwrd                               ; eat comma, get frequency in y,a
                 sty temp_max_lo                          ; save our copy of max, also set up as current
                 sta temp_max_hi
                 sty temp_freq_lo
                 sta temp_freq_hi

; Get duration

                 jsr comwrd                               ; eat comma, get number of jiffys to play
                 cmp #$80
                 bcs l115_1
                 sty temp_time_lo
                 sta temp_time_hi

; Get sweep direction

                 jsr optzer                               ; get optional sweep (default = 0, up)
                 cpx #3
                 bcs l115_1
                 txa
                 sta temp_direction
                 and #1                                   ; set .Z if sweep up or oscillate
                 php                                      ; save .Z for step (below)

; Get minimum frequency value (sweep lo)

                 jsr optwrd
                 sty temp_min_lo
                 sta temp_min_hi

; Get step value for sweep

                 jsr optwrd                               ; get optional step, default is zero
                 plp                                      ; get flags from direction
                 beq l115_3                               ; branch if 'up' or oscillate
                 pha                                      ; if 'down', make step 2's complement
                 tya
                 eor #$ff
                 clc
                 adc #1
                 tay
                 pla
                 eor #$ff
                 adc #0
l115_3           sta temp_step_hi
                 tya
                 sta temp_step_lo

; Get waveform

                 ldx #2                                   ; get waveform. default is square (2)
                 jsr optbyt
                 cpx #4
                 bcs l115_1                               ; illegal value
                 lda sbits+4,x                            ; get bit pattern for selected waveform
                 ora #1                                   ; add in the gate bit
                 sta temp_waveform

; Get pulse width

                 jsr optwrd                               ; get optional pulse width in y,a
                 bcs l115_4
                 lda #8                                   ; no arg's given, use default pulse width
                 ldy #0
l115_4           cmp #16
                 bcs l115_1
                 sty temp_pulse_lo
                 sta temp_pulse_hi

; All arg's in, time to get to work

                 lda temp_time_lo
                 ora temp_time_hi
                 beq l115_9                               ; special case: time=0 means 'kill it NOW'

; Wait for all current uses of this voice to time out

                 ldx sound_voice                          ; first test 'PLAY'
                 txa                                      ; make an index into PLAY's tables
                 asl
                 tay
l115_5           lda voices+1,y
                 bpl l115_5

l115_6           lda sound_time_hi,x                      ; now test 'SOUND'
                 bpl l115_6

; All clear, now set up for current effect

                 ldy #0                                   ; download max freq l&h, min freq l&h,
l115_7           lda temp_max_lo,y                        ; ..sweep direction, step value l&h, & freq l&h
                 sta sound_max_lo,x
                 inx
                 inx
                 inx
                 inx                                      ; [910612] stereo
                 inx
                 inx
                 iny
                 cpy #9
                 bcc l115_7

; Now set up SID

                 ldx sound_voice
                 ldy SID_offset,x                         ; get index to SID voices
; jsr put_io_in_map
; jsr go_slow  ;      [910716] 4567R7A

                 lda #$08                                 ; turn off SID gate
                 sta sid1+4,y

                 lda #0                                   ; set up attack & decay,
                 sta sid1+5,y
                 lda #$f0                                 ; ..and sustain & release
                 sta sid1+6,y

                 ldx #0                                   ; set up freq (l&h), pulse width (l&h), & waveform
l115_8           lda temp_freq_lo,x
                 sta sid1,y
                 iny
                 inx
                 cpx #5
                 bne l115_8
; jsr go_fast  ;      [910716] 4567R7A

; Now set up time to play

l115_9           ldx sound_voice
                 ldy temp_time_lo
                 lda temp_time_hi

                 sei
                 sta sound_time_hi,x
                 tya
                 sta sound_time_lo,x
                 cli

                 rts

;.end
;[[command.window]]



;****************************************************************
;*
;*  WINDOW Command
;*
;*  Syntax : WINDOW upper-left col, upper-left row,
;*      lower-left col, lower-right row [,clear]
;*
;*  Where  :  0 <= row <= 24
;*       0 <= column <= (80/40)
;*       clear : 0 (no) or 1 (yes)
;*
;****************************************************************

window           jsr getbyt                               ; get u.l. col
                 cpx #80
                 bbr7 _mode,l116_1
                 cpx #40
l116_1           bcs l116_4
                 stx window_temp

                 jsr combyt                               ; get u.l. row
                 cpx #25
                 bcs l116_4
                 stx window_temp+1

                 jsr combyt                               ; get l.r. column
                 cpx #80
                 bbr7 _mode,l116_2
                 cpx #40
l116_2           bcs l116_4
                 stx window_temp+2
                 cpx window_temp                          ; can't be less than u.l. column
                 bcc l116_4

                 jsr combyt                               ; get l.r. row
                 cpx #25
                 bcs l116_4
                 stx window_temp+3
                 cpx window_temp+1                        ; can't be less than u.l. row
                 bcc l116_4

                 jsr optzer                               ; get optional clear flag
                 cpx #2
                 bcs l116_4
                 phx

                 ldx window_temp
                 lda window_temp+1
                 clc
                 jsr _set_window

                 ldx window_temp+2
                 lda window_temp+3
                 sec
                 jsr _set_window

                 ldx #19                                  ; assume 'home', not 'cls'
                 pla
                 beq l116_3
                 ldx #147
l116_3           txa
                 jmp _bsout

l116_4           +lbra fcerr                              ; illegal value error

;.end
;[[command.fastslow]]



;***********************************************************************
;
;    FAST - put the system in FAST (4 MHz?) mode.
;
;***********************************************************************

fast
                 +lbne snerr                              ; no args      [910410]
; jsr put_io_in_map
                 lda #%01000000
                 tsb vic+49                               ; set FAST (4MHz?) mode
                 rts



;***********************************************************************
;
;    SLOW - put the system in SLOW (1 MHz) mode.
;
;***********************************************************************

slow
                 +lbne snerr                              ; no args      [910410]
; jsr put_io_in_map
                 lda #%01000000
                 trb vic+49
                 rts

;.end



; These routines check for certain VALTYP.   (c) is not preserved.



frmnum           jsr frmevl

chknum           clc
                 !text $89

chkstr           sec

chkval
; bbs7 valtyp,docstr ;cannot do this- return status +/-/= needed!
                 bit valtyp
                 bmi docstr
                 bcs chkerr
chkok            rts

docstr           bcs chkok

chkerr           ldx #errtm
                 !text $2c

sterr            ldx #errst
                 +lbra error

;.end

;[[system.evaluate]]


; Formula Evaluator Routine
;
; The formula evaluator starts with (txtptr) pointing to the first character
; in the formula.  At the end (txtptr) points to the terminator.
; The result is left in the FAC.  On return (a) does not reflect the terminator.
;
; The formula evaluator uses the operator (optab) to determine precedence and
; dispatch addresses for each operator.
; A temporary result on the stack has the following format:
;
;     * The address of the operator routine.
;     * The floating point temporary result.
;     * The precedence of the operator.


frmevl           dew txtptr                               ; txtptr points to 1st char. in formula
                 ldx #0                                   ; dummy precedence = 0
                 !text $89

lpoper           pha                                      ; save precedence
                 phx
                 tsx                                      ; confirm enough system stack available (recursive calls)
                 cpx #<sysstk+44                          ; bottom of stack + room for error handling
                 bcc sterr                                ; formula too complex
                 jsr eval
                 lda #0
                 sta opmask

tstop            jsr chrgot                               ; last char
loprel           sec                                      ; prepare to subtract
                 sbc #greater_token                       ; is current character a relation?
                 bcc endrel                               ; no, relations all through
                 cmp #less_token-greater_token+1
                 bcs endrel                               ; really relational?  no, just big
                 cmp #1                                   ; reset carry for zero only
                 rol                                      ; 0 to 1, 1 to 2, 2 to 4
                 eor #1
                 eor opmask                               ; bring in the old bits
                 cmp opmask                               ; make sure that the new mask is bigger
                 +lbcc snerr                              ; syntax error, because two of the same
                 sta opmask                               ; save mask
                 jsr chrget
                 bra loprel                               ; get the next candidate


endrel           ldx opmask                               ; were there any?
                 bne finrel                               ; yes, handle as special op
                 +lbcs qop                                ; not an operator
                 adc #greater_token-plus_token
                 +lbcc qop                                ; not an operator
                 adc valtyp                               ; (c)=1
                 +lbeq cat                                ; only if (a)=0 and VALTYP=$FF (a string)

                 adc #$ff                                 ; get back original (a)
                 sta index1
                 asl                                      ; multiply by two
                 adc index1                               ; by three
                 tay                                      ; set up for later

qprec            pla                                      ; get previous precedence
                 cmp optab,y                              ; is old precedence greater or equal?
                 bcs qchnum                               ; yes, go operate
                 jsr chknum                               ; can't be string here

doprec           pha                                      ; save old precedence

negprc           jsr dopre1                               ; save a return for op
                 pla                                      ; pull off previous precedence
                 ldy opptr                                ; get pointer to op
                 bpl qprec1                               ; that's a real operator
                 tax                                      ; done?
                 beq qopgo                                ; done!
                 bra pulstk


finrel           lsr valtyp                               ; get value type into (c)
                 txa
                 rol                                      ; put VALTYP into low order bit of mask
                 dew txtptr                               ; decrement text pointer
                 ldy #ptdorl-optab                        ; make (y) point at operator entry
                 sta opmask                               ; save the operation mask
                 bra qprec                                ; branch always


qprec1                                                    ; note b7(VALTYP)=0 so CHKNUM call is ok
                 cmp optab,y                              ; last precedence is greater?
                 bcs pulstk                               ; yes, go operate
                 bcc doprec                               ; no, save argument and get other operand


dopre1           lda optab+2,y
                 pha                                      ; disp addr goes on stack
                 lda optab+1,y
                 pha
                 jsr pushf1                               ; save FAC on stack unpacked, precedence in (x)
                 lda opmask                               ; (a) may be mask for rel
                 bra lpoper


pushf1                                                    ; save FAC on stack unpacked
                 pla                                      ; first grab return address off stack
                 sta index1
                 pla
                 sta index1+1
                 inw index1

                 ldx optab,y                              ; precedence
                 ldy facsgn
                 phy
                 jsr round                                ; put rounded FAC on stack
                 lda faclo
                 pha
                 lda facmo
                 pha
                 lda facmoh
                 pha
                 lda facho
                 pha
                 lda facexp
                 pha
                 jmp (index1)                             ; return


pullf1                                                    ; retrieve FAC from stack unpacked  [910402]
                 pla                                      ; first grab return address off stack
                 sta index1
                 pla
                 sta index1+1
                 inw index1

                 lda #0                                   ; it's been rounded
                 sta facov
                 pla
                 sta facexp
                 pla
                 sta facho
                 pla
                 sta facmoh
                 pla
                 sta facmo
                 pla
                 sta faclo
                 pla
                 sta facsgn
                 jmp (index1)                             ; return


qop              ldy #255
                 pla                                      ; get high precedence of last op
qopgo            beq qoprts                               ; done!

qchnum           cmp #100                                 ; relational operator?
                 beq unpstk                               ; yes, don't check operand
                 jsr chknum                               ; must be number

unpstk           sty opptr                                ; save operator's pointer for next time
pulstk           pla                                      ; get mask for rel op if it is one
                 lsr                                      ; setup .c for dorel's chkval
                 sta domask                               ; save for "docmp"
                 pla                                      ; unpack stack into arg
                 sta argexp
                 pla
                 sta argho
                 pla
                 sta argmoh
                 pla
                 sta argmo
                 pla
                 sta arglo
                 pla
                 sta argsgn
                 eor facsgn                               ; get probable result sign
                 sta arisgn                               ; sign used by add, sub, mul, div

qoprts           lda facexp                               ; get it and set codes
                 rts                                      ; return

eval             jmp (ieval)

neval            lda #0                                   ; assume numeric
                 sta valtyp

eval0            jsr chrget                               ; get a character
                 bcs eval2
eval1            ldx #0                                   ; flag 'bank 0' (text bank)
                 +lbra fin                                ; it is a number

eval2            jsr isletc                               ; variable name?
                 bcs is_variable                          ; yes.
                 cmp #pi                                  ; pi?
                 bne qdot
                 lda #<pival
                 ldy #>pival
                 jsr movfm                                ; put value in for p1.
                 jmp chrget


qdot             cmp #'.'                                 ; constant?
                 beq eval1
                 cmp #minus_token                         ; negation?
                 beq domin                                ; yes.
                 cmp #plus_token
                 beq eval0
                 cmp #'"'                                 ; string?
                 bne eval3

strtxt           lda txtptr
                 ldy txtptr+1
                 adc #0                                   ; c=1
                 bcc strtx2
                 iny
strtx2           jsr strlit                               ; process string

st2txt           ldx strng2
                 ldy strng2+1
                 stx txtptr
                 sty txtptr+1
                 rts


eval3            cmp #not_token                           ; not?
                 bne eval4
                 ldy #24
                 bne gonprc                               ; branch always


notop            jsr ayint                                ; integerize
                 lda faclo                                ; get argument
                 eor #$ff
                 tay
                 lda facmo
                 eor #$ff

givayf           jsr stoint                               ; integer to float routine
                 +lbra floats


eval4            cmp #fn_token                            ; user defined function?
                 +lbeq fndoer                             ; yes
                 cmp #first_function_token                ; function name?
                 +lbcs isfun                              ; yes
; (functions are the highest numbered
; tokens so no need to check further)

parchk           jsr chkopn                               ; only thing left is formula in parens
                 jsr frmevl                               ; a formula in parens

chkcls           lda #')'                                 ; close paren?
                 !text $2c

chkopn           lda #'('                                 ; open paren?
                 !text $2c

chkcom           lda #','                                 ; comma?


; SYNCHR looks at the current character to make sure it is the specific
; thing loaded into (a) just before the call to SYNCHR.  If not, it calls
; the "syntax error" routine.  Otherwise it gobbles the next char and returns.
;
; (a)=new char and TXTPTR is advanced by CHRGET.


synchr           ldy #0
                 sta syntmp
                 jsr indtxt
                 cmp syntmp
                 +lbne snerr
                 jmp chrget                               ; ok



domin
l117_1           =negtab-optab                            ; negoff
                 ldy #l117_1                              ; precedence below '-'

gonprc           pla                                      ; get rid of rts addr.
                 pla
                 +lbra negprc                             ; do negation

;.end



is_variable
                 jsr ptrget                               ; parse variable name, put name in varnam

isvret           sta facmo                                ; save pointer to variable
                 sty facmo+1
                 ldx varnam
                 ldy varnam+1
                 lda valtyp
                 beq is_numeric                           ; branch if numeric

                 lda #0
                 sta facov
                 cpx #'T'                                 ; TI$ is a special case. look for it
                 bne isvds                                ; no- go test for DS$
                 cpy #'I'+$80                             ; shifted I?
                 bne ds_rts                               ; no- and it's not DS$ either

; Variable name is TI$.  To see if this is 'the' TI$ and not an
; array TI$(), test to see if it has a pointer to the zero in "ROM".
; If it is an array item, its pointer will be to a real value, or
; a real zero.  If it isn't an array item, its pointer will point
; to a dummy zero in "ROM".

                 lda facmo+1
                 cmp #>zero
                 bne ds_rts                               ; not TI$, not DS$
                 lda facmo
                 cmp #<zero
                 bne ds_rts
                 +lbra Get_TI_String                      ; the one and only TI$


isvds            cpx #'D'                                 ; is this DS$?
                 bne ds_rts                               ; no
                 cpy #'S'+$80                             ; shifted S?
                 bne ds_rts                               ; no

                 jsr Check_DS                             ; yes- check DS$ allocation,
;  and get it if not in memory
                 ldy #$ff
l118_1           iny                                      ; copy DS$ to a temp.
                 lda #dsdesc+1                            ; must first determine how big it is
                 jsr lda_far_ram1                         ; lda (dsdesc+1),y
                 bne l118_1                               ; loop until terminating null found

                 tya                                      ; length of string required
                 jsr strini                               ; get temp. string space & descriptor
                 tay
                 beq l118_3                               ; (don't bother copying if length is 0)

                 phx

                 ldx #dsctmp+1                            ; ???? was ldx #frespc
l118_2           lda #dsdesc+1                            ; copy DS$ into temp
                 dey
                 jsr lda_far_ram1                         ; lda (dsdesc+1),y
                 jsr sta_far_ram1                         ; sta (dsctmp+1),y
                 tya
                 bne l118_2
                 plx
                 lda dsdesc                               ; a=length     [901014] FAB
                 jsr mvdone                               ; ???? (does nothing on C128 - bug or oversight?)

l118_3           +lbra putnew

ds_rts           rts


is_numeric
                 bbr7 intflg,is_floating                  ; branch if not an integer
                 ldy #0
                 jsr indfmo                               ; fetch high
                 tax
                 iny
                 jsr indfmo                               ; fetch low
                 tay                                      ; put low in y
                 txa                                      ; get high in a
                 +lbra givayf                             ; float and return


; Screen out TI, ST, ER, and EL, and assign values to them.  First test
; if the pointer points to "ROM" zero.  If not, it can't be any of the above.

is_floating
                 lda facmo+1
                 cmp #>zero
                 bne gomovf                               ; not TI, etc.
                 lda facmo
                 cmp #<zero
                 bne gomovf                               ; not TI, etc.


; The pointer does point to the ROM zero.  Now it is necessary to
; examine the actual variable name case by case.

                 cpx #'T'                                 ; TI?
                 bne qstatv                               ; no
                 cpy #'I'
                 bne gomovf                               ; no, and it can't be ST either
                 +lbeq Get_TI


qstatv           cpx #'S'                                 ; ST?
                 bne qdsav                                ; no, go test DS
                 cpy #'T'
                 bne gomovf
                 jsr _readst                              ; (???? system bank for rs232 st)
                 +lbra float


qdsav            cpx #'D'                                 ; DS?
                 bne qerlin                               ; no, go test ER & EL
                 cpy #'S'
                 bne gomovf

; Get disk status - make the first two characters of DS$ string into a number.

                 jsr Check_DS                             ; get a DS$ string if one doesn't exist already
                 ldy #0
                 lda #dsdesc+1
                 jsr lda_far_ram1                         ; lda (dsdesc+1),y
                 and #$0f
                 asl
                 sta garbfl
                 asl
                 asl
                 adc garbfl
                 sta garbfl
                 iny
                 lda #dsdesc+1
                 jsr lda_far_ram1                         ; lda (dsdesc+1),y
                 and #$0f
                 adc garbfl
                 +lbra float


qerlin           cpx #'E'                                 ; ER or EL?
                 bne gomovf
                 cpy #'R'
                 beq qnumer
                 cpy #'L'
                 bne gomovf

                 lda errlin+1                             ; want EL (last error line #)
                 ldy errlin
                 +lbra nosflt

qnumer           lda errnum                               ; want ER (number of last error)
                 +lbra float


gomovf           lda facmo
                 ldy facmo+1

movfrm           sta index1                               ; move value from RAM
                 sty index1+1

                 ldy #0
                 jsr indin1_ram1
                 sta facexp
                 sty facov

                 iny                                      ; (1)
                 jsr indin1_ram1
                 sta facsgn
                 ora #$80
                 sta facho

                 iny                                      ; (2)
                 jsr indin1_ram1
                 sta facmoh

                 iny                                      ; (3)
                 jsr indin1_ram1
                 sta facmo

                 iny                                      ; (4)
                 jsr indin1_ram1
                 sta faclo
                 rts

;.end



;  Read the variable name at the current text position and put a pointer
;  to its value in VARPNT.   TXTPTR points to the terminating character.
;  Note that evaluating subscripts in a variable name can cause recursive
;  calls to PTRGET, so all values must be stored on the stack.

ptrget           ldx #0
                 jsr chrgot
ptrgt1           stx dimflg                               ; store flag away
ptrgt2           sta varnam
                 jsr chrgot                               ; get current character
                 jsr isletc                               ; check for a letter
                 +lbcc snerr                              ; not a letter

                 ldx #0                                   ; assume no second character
                 stx valtyp                               ; default is numeric
                 stx intflg                               ; assume floating
                 jsr chrget                               ; get following character
                 bcc l119_1                               ; branch if numeric
                 jsr isletc                               ; is it alpha?
                 bcc l119_3                               ; no, no second character. branch
l119_1           tax                                      ; issec. save second character of name

l119_2           jsr chrget                               ; skip over remainder of name. we only care about 2 chars.
                 bcc l119_2                               ; ..eat numbers,
                 jsr isletc
                 bcs l119_2                               ; ..and alphas, too!

l119_3           cmp #'$'                                 ; nosec. is this a string?
                 bne l119_4                               ; if not, VALTYP = 0
                 lda #$ff
                 sta valtyp                               ; ..else, flag 'string'
                 bra l119_5

l119_4           cmp #'%'                                 ; notstr. isn't string. is it integer?
                 bne l119_6                               ; branch if not
                 lda subflg
; bne snerr ; syntax error if integers disabled
                 +lbne chkerr                             ; integers disallowed- type mismatch error  [910114]
                 lda #$80                                 ; flag integer by turning on both high bits
                 sta intflg
                 tsb varnam

l119_5           txa                                      ; turnon. turn on msb of second character
                 ora #$80
                 tax
                 jsr chrget                               ; get character after $ or %

l119_6           stx varnam+1                             ; strnam. store away second character
                 sec
                 ora subflg                               ; add flag whether to allow arrays
                 sbc #'('
                 +lbeq is_array                           ; note: won't match if SUBFLG set

                 ldy #0
                 sty subflg                               ; allow subscripts again
                 lda vartab                               ; place to start search
                 ldx vartab+1

l119_7           stx lowtr+1                              ; stxfnd.
l119_8           sta lowtr
                 cpx arytab+1                             ; at end of table yet?
                 bne l119_9
                 cmp arytab
                 beq notfns                               ; yes, we couldn't find it

l119_9           jsr indlow_ram1                          ; lda (lowtr),y
                 cmp varnam                               ; compare high orders
                 bne l119_10
                 iny
                 jsr indlow_ram1
                 cmp varnam+1                             ; and the low part?
                 +lbeq finptr                             ; !!that's it!!

                 dey
l119_10          clc
                 lda lowtr
                 adc #7                                   ; makes no difference among types
                 bcc l119_8
                 inx
                 bra l119_7                               ; branch always




; Test for a letter: (c)=0 not a letter
;   (c)=1 a letter

isletc           cmp #'A'
                 bcc l120_1                               ; if less than "a", return
                 sbc #'Z'+1                               ; $5a + 1
                 sec
                 sbc #$a5                                 ; reset carry if (a) .gt. "z".
l120_1           rts


notfns           tsx                                      ; check who's calling????
                 lda $102,x                               ; sniff processor stack
                 cmp #>pointer_ret
                 beq ldzr                                 ; special case if called by pointer function

l121_1           = isvret-1
                 cmp #>l121_1                             ; is eval calling????
                 bne notevl                               ; no, carry on

ldzr             lda #<zero                               ; set up pointer to simulated zero
                 ldy #>zero
                 rts                                      ; for strings or numeric


qst001           cpy #'I'+$80                             ; we know first is T, is second <shift>I (TI$)?
                 beq ldzr
                 cpy #'I'                                 ; or I (TI)?
                 bne varok
                 beq gobadv


qst004           cpy #'S'+$80                             ; check for DS$
                 beq gobadv
                 cpy #'S'                                 ; check for DS
                 bne varok
                 beq gobadv


qst002           cpy #'T'                                 ; check for ST
                 bne varok
                 beq gobadv


qst003           cpy #'R'                                 ; check for ER
                 beq gobadv
                 cpy #'L'                                 ; check for EL
                 bne varok


gobadv           +lbra snerr



notevl           lda varnam
                 ldy varnam+1
                 cmp #'T'                                 ; screen out TI
                 beq qst001
                 cmp #'S'                                 ; ...and ST
                 beq qst002
                 cmp #'E'                                 ; ...and ER and EL
                 beq qst003
                 cmp #'D'                                 ; ...and DS
                 beq qst004


varok            lda arytab
                 ldy arytab+1
                 sta lowtr
                 sty lowtr+1
                 lda strend
                 ldy strend+1
                 sta hightr
                 sty hightr+1
                 clc
                 adc #7
                 bcc l122_1                               ; not even
                 iny

l122_1           sta highds
                 sty highds+1
                 jsr bltu
                 lda highds
                 ldy highds+1
                 iny
                 sta arytab
                 sty arytab+1


; Scan thru array entries for string arrays.  If any are found it will be
; necessary to adjust the back-links of the strings in that array, since
; the array descriptor block itself was moved.

                 sta arypnt                               ; set pointer to arrays
                 sty arypnt+1

aryva2           lda arypnt
                 ldx arypnt+1

aryva3           cpx strend+1                             ; end of arrays?
                 bne aryvgo
                 cmp strend
                 beq arydon                               ; ...finished


aryvgo           sta index1
                 stx index1+1
                 ldy #0
                 jsr indin1_ram1                          ; look at array name
                 tax
                 iny
                 jsr indin1_ram1                          ; name 2nd char
                 php                                      ; save status reg
                 iny
                 jsr indin1_ram1                          ; point to offset to next array
                 adc arypnt
                 sta arypnt                               ; save start of next array in arypnt
                 iny
                 jsr indin1_ram1
                 adc arypnt+1
                 sta arypnt+1
                 plp                                      ; restore status
                 bpl aryva2                               ; not a string type
                 txa
                 bmi aryva2                               ; not a string array
                 iny                                      ; ok we have a string array
                 jsr indin1_ram1                          ; get number of dimensions
                 ldy #0
                 asl                                      ; move index to ptr to 1st string (add 2*number of dims + 5)
                 adc #5
                 adc index1
                 sta index1
                 bcc aryget
                 inc index1+1

aryget           ldx index1+1
                 cpx arypnt+1                             ; done with this array?
                 bne l123_1
                 cmp arypnt
                 beq aryva3                               ; yes

l123_1           ldy #0                                   ; process string pointer
                 jsr indin1_ram1                          ; get length of string
                 beq dvarts                               ; skip if null string
                 sta syntmp
                 iny
                 jsr indin1_ram1                          ; get lo byte of string ptr
                 clc
                 adc syntmp                               ; and add string length
                 sta hightr
                 iny
                 jsr indin1_ram1                          ; get hi byte of string ptr
                 adc #0                                   ; adjust high byte
                 sta hightr+1

; Fix backwards pointer by adding move length to it.

                 phx
                 ldx #hightr
                 ldy #0
                 jsr indhtr_ram1                          ; lda (hightr),y
                 adc #7                                   ; carry clear (careful!)
                 jsr sta_far_ram1                         ; sta (hightr),y
                 iny
                 jsr indhtr_ram1                          ; lda (hightr),y
                 adc #0
                 jsr sta_far_ram1                         ; sta (hightr),y
                 plx                                      ; done with this string

; Fix the next string in the array

dvarts           lda #strsiz
                 clc
                 adc index1
                 sta index1
                 bcc aryget
                 inc index1+1
                 bra aryget                               ; branch always


arydon           phx
                 ldx #lowtr
                 ldy #0
                 lda varnam
                 jsr sta_far_ram1                         ; sta (lowtr),y
                 iny                                      ; .y=1
                 lda varnam+1
                 jsr sta_far_ram1                         ; sta (lowtr),y
                 lda #0
l124_1           iny
                 jsr sta_far_ram1                         ; sta (lowtr),y
                 cpy #6
                 bne l124_1
                 plx

finptr           lda lowtr
                 clc
                 adc #2
                 ldy lowtr+1
                 bcc l125_1
                 iny
l125_1           sta varpnt
                 sty varpnt+1
                 rts




bltu             jsr reason
                 sta strend
                 sty strend+1
                 sec
                 lda hightr
                 sbc lowtr
                 sta index
                 tay
                 lda hightr+1
                 sbc lowtr+1
                 tax
                 inx
                 tya
                 beq decblt
                 lda hightr
                 sec
                 sbc index
                 sta hightr
                 bcs l126_1
                 dec hightr+1
                 sec
l126_1           lda highds
                 sbc index
                 sta highds
                 bcs moren1
                 dec highds+1
                 bcc moren1

bltlp            jsr indhtr_ram1                          ; lda (hightr),y
                 phx
                 ldx #highds
                 jsr sta_far_ram1                         ; sta (highds),y
                 plx

moren1           dey
                 bne bltlp
                 jsr indhtr_ram1                          ; lda (hightr),y
                 phx
                 ldx #highds
                 jsr sta_far_ram1                         ; sta (highds),y
                 plx

decblt           dec hightr+1
                 dec highds+1
                 dex
                 bne moren1
                 rts

;.end
;[[system.arrays]]



; The format of arrays in core:
;
; Descriptor: low  byte = first character
;   high byte = second character (msb is string flag)
; Length of array in memory in bytes (includes everything).
; Number of dimensions.
; For each dimension starting with the first a list (2 bytes each)
; of the max indice+1.
; The values.


is_array
                 lda dimflg
                 ora intflg
                 pha                                      ; save DIMFLG for recursion
                 lda valtyp
                 pha                                      ; save VALTYP for recursion
                 ldy #0                                   ; set number of dimensions to zero

l127_1           phy                                      ; save number of dims
                 lda varnam+1
                 pha
                 lda varnam
                 pha                                      ; save looks
                 jsr intidx                               ; evaluate indice into facmo&lo
                 pla
                 sta varnam
                 pla
                 sta varnam+1                             ; get back all...we're home
                 ply                                      ; (# of units)
                 tsx
                 lda 258,x
                 pha                                      ; push DIMFLG and VALTYP further
                 lda 257,x
                 pha
                 lda indice                               ; put indice onto stack
                 sta 258,x                                ; under DIMFLG and VALTYP
                 lda indice+1
                 sta 257,x
                 iny                                      ; y counts # of subscripts
                 sty count                                ; protect y from chrget
                 jsr chrgot                               ; get terminating character
                 ldy count
                 cmp #','                                 ; more subscripts?
                 beq l127_1                               ; yes


                 jsr chkcls                               ; must be closed paren
                 pla
                 sta valtyp                               ; get VALTYP and
                 pla
                 sta intflg
                 and #$7f
                 sta dimflg                               ; DIMFLG off stack
                 ldx arytab                               ; place to start search
                 lda arytab+1


l127_2           stx lowtr
                 sta lowtr+1
                 cmp strend+1                             ; end of arrays?
                 bne l127_3
                 cpx strend
                 beq notfdd                               ; a fine thing! no array!

l127_3           ldy #0
                 jsr indlow_ram1                          ; get high of name from array bank (ram1)
                 iny
                 cmp varnam                               ; compare high orders.
                 bne l127_4                               ; no way is it this. get the bite outta here
                 jsr indlow_ram1
                 cmp varnam+1                             ; low orders?
                 beq gotary                               ; well here it is

l127_4           iny
                 jsr indlow_ram1                          ; get length
                 clc
                 adc lowtr
                 tax
                 iny
                 jsr indlow_ram1
                 adc lowtr+1
                 bcc l127_2                               ; always branches


bserr            ldx #errbs                               ; get bad sub error number
                 !text $2c

fcerr            ldx #errfc                               ; too big. Illegal Quantity error
                 +lbra error



gotary           ldx #errdd                               ; perhaps a "re-dimension" error
                 lda dimflg                               ; test the DIMFLG
                 +lbne error
                 jsr fmaptr
                 ldy #4
                 jsr indlow_ram1
                 sta syntmp
                 lda count                                ; get number of dims input.
                 cmp syntmp                               ; # of dims the same?
                 bne bserr                                ; same so get definition.
                 +lbra getdef


; Come here when variable is not found in the array table to build an entry.
;
; Put down the descriptor.
; Setup number of dimensions.
; Make sure there is room for the new entry.
; Remember VARPNT.
; Tally=4.
; Skip two locs for later fill in of size.
; LOOP: Get an indice.
;  Put down number+1 and increment VARPTR.
;  Tally=tally*number+1
;  Decrement number of dims.
;  Bne LOOP
; Call REASON with (a,b) reflecting last loc of variable.
; Update STREND
; Zero all.
; Make tally include maxdims and descriptor.
; Put down tally
; If called by dimension, return.
;  Else index into the variable as if it were found on the initial search.

notfdd
                 jsr fmaptr                               ; form ARYPNT
                 jsr reason
                 ldy #0
                 sty curtol+1
                 ldx #5
                 lda varnam
                 php
                 phx
                 ldx #lowtr                               ; point to string/array bank
                 jsr sta_far_ram1                         ; sta (lowtr),y
                 plx
                 plp
                 bpl l128_1
                 dex

l128_1           iny                                      ; notflt.
                 lda varnam+1
                 php
                 phx
                 ldx #lowtr                               ; point to string/array bank
                 jsr sta_far_ram1                         ; sta (lowtr),y
                 plx
                 plp
                 bpl l128_2
                 dex
                 dex

l128_2           stx curtol
                 lda count                                ; save number of dimensions
                 iny
                 iny
                 iny
                 ldx #lowtr                               ; point to string/array bank
                 jsr sta_far_ram1                         ; sta (lowtr),y

l128_3           ldx #11                                  ; loppta. default size
                 lda #0
                 bbr6 dimflg,l128_4                       ; not in a dim statement
                 pla                                      ; get low order of indice
                 clc
                 adc #1
                 tax
                 pla                                      ; get high order of indice
                 adc #0

l128_4           iny                                      ; notdim.
                 phx
                 ldx #lowtr
                 jsr sta_far_ram1 ;sta (lowtr),y          ; store high part of indice
                 plx
                 iny
                 txa
                 phx
                 ldx #lowtr
                 jsr sta_far_ram1 ;sta (lowtr),y          ; store low part of indice
                 plx
                 jsr umult                                ; (a,x)+(curtol)*(lowtr,y)
                 stx curtol                               ; save new tally
                 sta curtol+1
                 ldy index
                 dec count                                ; any more indices left?
                 bne l128_3                               ; yes
                 adc arypnt+1
                 +lbcs omerr                              ; overflow
                 sta arypnt+1                             ; compute where to zero
                 tay
                 txa
                 adc arypnt
                 bcc l128_5
                 iny
                 +lbeq omerr

l128_5           jsr reason                               ; grease.  get room
                 sta strend
                 sty strend+1                             ; new end of storage
                 lda #0                                   ; storing (a) is faster than clear
                 inc curtol+1
                 ldy curtol
                 beq l128_7

l128_6           dey                                      ; zero out new entry
                 php
                 phx
                 ldx #arypnt
                 jsr sta_far_ram1                         ; sta (arypnt),y
                 plx
                 plp
                 bne l128_6                               ; no. continue

l128_7           dec arypnt+1                             ; deccur.
                 dec curtol+1
                 bne l128_6                               ; do another block
                 inc arypnt+1                             ; bump back up. will use later
                 sec
                 lda strend                               ; restore (a)
                 sbc lowtr                                ; determine length
                 ldy #2
                 phx
                 ldx #lowtr
                 jsr sta_far_ram1 ;sta (lowtr),y          ; low
                 lda strend+1
                 iny
                 sbc lowtr+1
                 jsr sta_far_ram1 ;sta (lowtr),y          ; high
                 plx
                 lda dimflg                               ; quit here if this is a DIM statement
                 bne dimrts                               ; bye!
                 iny


; At this point (LOWTR,y) points beyond the size to the number of dimensions.
; Strategy:
;  NUMDIM = number of dimensions
;  curtol = 0
;  INLPNM: Get a new indice
;   Make sure indice is not too big
;   Multiply CURTOL by CURMAX
;   Add indice to CURTOL
;   NUMDIM=NUMDIM-1
;   bne INLPNM
;  Use (CURTOL)*4 as offset


getdef           jsr indlow_ram1                          ; get # of dim's from string bank
                 sta count                                ; save a counter
                 lda #0                                   ; zero (curtol)
                 sta curtol

inlpnm           sta curtol+1
                 plx                                      ; get low indice
                 stx indice
                 iny
                 jsr indlow_ram1
                 sta syntmp
                 pla                                      ; and the high part
                 sta indice+1
                 cmp syntmp                               ; compare with max indice
                 bcc inlpn2
                 bne bserr7                               ; if greater, "bad subscript" error
                 iny
                 jsr indlow_ram1
                 sta syntmp
                 cpx syntmp
                 bcc inlpn1

bserr7           +lbra bserr


inlpn2           iny
inlpn1           lda curtol+1                             ; don't multiply if curtol=0
                 ora curtol
                 clc                                      ; prepare to get indice back
                 beq l129_1                               ; get high part of indice back
                 jsr umult                                ; multiply (curtol) by (5&6,lowtr)
                 txa
                 adc indice                               ; add in (indice)
                 tax
                 tya
                 ldy index1

l129_1           adc indice+1
                 stx curtol
                 dec count                                ; any more?
                 bne inlpnm                               ; yes
                 sta curtol+1
                 ldx #5
                 lda varnam
                 bpl l129_2
                 dex
l129_2           lda varnam+1
                 bpl l129_3
                 dex
                 dex
l129_3           stx addend
                 lda #0
                 jsr umultd                               ; on rts, a & y = hi. x = lo.
                 txa
                 adc arypnt
                 sta varpnt
                 tya
                 adc arypnt+1
                 sta varpnt+1
                 tay
                 lda varpnt
dimrts           rts


; Integer arithmetic routines.
;
; Two byte unsigned integer multiply.
; This is for multiply dimensioned arrays.
; (a,b)=(curtol)*(5&6,x).

umult
                 sty index
                 jsr indlow_ram1
                 sta addend                               ; low, then high
                 dey
                 jsr indlow_ram1                          ; put (5&6,lowtr) in faster memory

umultd           sta addend+1
                 lda #16
                 sta deccnt
                 ldx #0                                   ; clear the accs
                 ldy #0                                   ; result initially zero

umultc           txa
                 asl                                      ; multiply by two
                 tax
                 tya
                 rol
                 tay
                 +lbcs omerr                              ; to much!
                 asl curtol
                 rol curtol+1
                 bcc umlcnt                               ; nothing in this position to multiply
                 clc
                 txa
                 adc addend
                 tax
                 tya
                 adc addend+1
                 tay
                 +lbcs omerr                              ; man, just too much!

umlcnt           dec deccnt                               ; done?
                 bne umultc                               ; keep it up
                 rts                                      ; yes, all done


fmaptr           lda count
                 asl
                 adc #5                                   ; point to entries. ((c) cleared by asl)
                 adc lowtr
                 ldy lowtr+1
                 bcc l130_1
                 iny
l130_1           sta arypnt
                 sty arypnt+1
                 rts

;.end
;[[command.time]]



; TI$="hh:mm:ss.t" Allows optional colons to delimit parameters and
;   allows input to be abbrieviated (eg., TI$="h:mm" or
;   even TI$=""), defaulting to "00" for unspecified
;   parameters.  24-hour clock (00:00:00.0 to 23:59:59.9).
;   901010 F.Bowen

Set_TI_String
                 jsr frefac                               ; we won't need it
                 sta count                                ; save length

                 ldy #0                                   ; our pointer into TI$ assignment
                 sty time                                 ; default time to zero, in case it's not fully specified
                 sty time+1
                 sty time+2
                 sty time+3

                 ldx #3                                   ; parameter pointer (3=hr,2=min,1=sec,0=tenths)
l131_1           jsr GetTimeDigit                         ; get first digit, convert to BCD
                 bcs l131_2                               ; colon or eos
                 sta time,x
                 jsr GetTimeDigit                         ; get second digit, convert to BCD
                 bcs l131_2                               ; colon or eos

                 asl time,x                               ; move first digit to msd
                 asl time,x
                 asl time,x
                 asl time,x
                 ora time,x                               ; combine with second digit
                 sta time,x                               ; now we have a time element of packed BCD

l131_2           lda time,x
                 cmp MaxTimeValues,x                      ; check for parameter too big
                 +lbcs fcerr                              ; hr>23, min>59, sec>59, tenths>9

                 dex                                      ; check if done
                 bmi l131_4                               ; yes- all parameters accounted for
                 cpy count
                 bcs l131_5                               ; yes- end of string

                 jsr indin1_ram1                          ; check for optional colon (or period)   [910103]
                 cmp #':'
                 beq l131_3
                 cmp #'.'
                 bne l131_1                               ; not there
l131_3           iny                                      ; it's there- skip over it

                 bra l131_1                               ; loop until done


l131_4           cpy count                                ; done
                 +lbcc errlen                             ; error if string too long

l131_5           ldz time                                 ; tenths  0-9
                 lda time+1                               ; seconds 0-59
                 ldx time+2                               ; minutes 0-59
                 ldy time+3                               ; hours  0-23
                 jmp _SetTime                             ; Go set time & exit


; Get an ASCII digit, make sure it's in range 0-9 or a colon.
; if no digit to get, default to '0'
;
; exit with .c=0 if okay  (.A contains BCD)
;    .c=1 if colon or eos (.A invalid)

GetTimeDigit
                 lda #0                                   ; default to '0'
                 cpy count
                 bcs l132_1                               ; exit if at end of string (carry set)

                 jsr indin1_ram1                          ; else get a character from string
                 iny                                      ; point to next character
                 cmp #'.'                                 ; [910103]
                 beq l132_1                               ; terminator (period) (carry set)
                 cmp #'0'                                 ; check character, only 0-9 allowed
                 +lbcc fcerr                              ; too small
                 cmp #':'
                 bcc l132_1                               ; just right  (carry clear)
                 +lbne fcerr                              ; too big
; falls through if colon (carry set)

l132_1           and #$0f                                 ; make BCD
                 rts



MaxTimeValues
                 !text $10,$60,$60,$24                    ; t,s,m,h in packed BCD


; x$=TI$  Return a string of the form "hh:mm:ss.t", including colons.

Get_TI_String
                 jsr ReadSystemClock                      ; get time as packed BCD

                 lda #10                                  ; get string space for 10 characters
                 jsr strspa
                 tay                                      ; length
                 dey                                      ; index

                 lda time                                 ; build TI$ string in 'fbuffr'
                 ora #'0'                                 ; (build string backwards, from last chr to first)
                 ldx #dsctmp+1
                 jsr sta_far_ram1                         ; put tenths (special case- only 1 digit)
                 dey
                 lda #'.'
                 jsr sta_far_ram1                         ; put period (special case)   [910103]
                 dey
                 ldx #1
                 bra l133_2

l133_1           phx                                      ; element pointer (1=secs, 2=mins, 3=hrs)
                 ldx #dsctmp+1
                 lda #':'
                 jsr sta_far_ram1                         ; put colon
                 dey
                 plx

l133_2           lda time,x
                 taz
                 and #$0f                                 ; do lsd first, since we're working backwards
                 ora #'0'
                 phx
                 ldx #dsctmp+1
                 jsr sta_far_ram1                         ; put lsd
                 dey
                 tza                                      ; then do msd
                 lsr
                 lsr
                 lsr
                 lsr
                 ora #'0'
                 jsr sta_far_ram1                         ; put msd
                 plx
                 inx                                      ; next packed element
                 dey
                 bpl l133_1                               ; loop until done

                 lda #10                                  ; length
                 jsr mvdone                               ; update frespc ????
                 +lbra putnew                             ; make descriptor in dsctmp real


;[[system.time]]
; TI. Convert 24-hour TOD into tenths of seconds.  901010 F.Bowen

Get_TI
                 jsr ReadSystemClock                      ; glance at the clock, get time as h:m:s:t
                 stz faclo                                ; init accumulator with tenths (0-9, so nothing to convert)
                 ldz #0
                 stz facmo
                 stz facmoh

                 ldx #3                                   ; convert time (BCD) to tenths of seconds (binary) since midnight
l134_1           jsr TimeMultiply
                 clc
                 adc faclo
                 sta faclo
                 lda product+1
                 adc facmo
                 sta facmo
                 lda product+2
                 adc facmoh
                 sta facmoh                               ; (can't overflow since 23:59:59:9 -> 863999 ($0D2EFF)
                 dex
                 bne l134_1                               ; next factor

                 lda #0                                   ; float value in FAC
                 sta facho                                ; zero msb, facov, facsgn
                 ldx #160                                 ; set facov for time
                 sec                                      ; normal fac
                 +lbra floatb                             ; do it



ReadSystemClock
                 jsr _ReadTime                            ; get packed BCD, y=hrs, x=min, a=sec, z=tenths
; (assumes system clock was set properly!)
                 stz time                                 ; tenths  0-9
                 sta time+1                               ; seconds  0-59
                 stx time+2                               ; minutes  0-59
                 sty time+3                               ; hours  0-59
                 rts


; Unsigned Integer Multiply: Time * Factor  -> Tenths_of_Seconds
;     A   *  (B,C)  ->      (D,E,F)

TimeMultiply
                 lda time,x                               ; convert packed BCD to binary
                 and #$0f
                 sta facho
                 lda time,x                               ; 10x = 8x + 2x
                 and #$f0
                 lsr                                      ; msd x 8
                 sta time,x
                 lsr
                 lsr                                      ; msd x 2
                 clc
                 adc facho                                ; lsd
                 adc time,x
                 sta time,x                               ; can't overflow ($99->153)

                 txa                                      ; make a word pointer from byte pointer
                 asl
                 tay
                 lda TimeFactor-2,y                       ; multiplicand = TimeFactor,y  (2 bytes)
                 sta multiplicand                         ; multiplier = Time,x x (1 byte)
                 lda TimeFactor-1,y                       ; -----------
                 sta multiplicand+1
                 lda #0                                   ; product lo   (3 bytes)
                 sta product+1                            ; mid
                 sta product+2                            ; hi

                 ldy #16                                  ; 16-bit multiplicand
l135_1           asl
                 row product+1
                 row multiplicand                         ; multiplier * multiplicand -> product
                 bcc l135_2
                 clc
                 adc time,x
                 bcc l135_2
                 inw product+1                            ; (does no error check, since using time factors
l135_2           dey                                      ; in ROM and maximum time multiplier of 59 there
                 bne l135_1                               ; is no danger of overflow)

; sta product
                 rts                                      ; (.X is preserved)


TimeFactor
                 !word 10                                 ; tenths per second  (max    59*10 =    590 ($0024E)
                 !word 600                                ; per minute  (max   59*600 =  35400 ($08A48)
                 !word 36000                              ; per hour    (max 23*36000 = 828000 ($CA260)

;[[command.sleep]]



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

sleep            jsr getwrd                               ; get argument in (y,a)

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

                 sty time                                 ; Number of seconds to "sleep"   [910730] new
                 sta time+1

l136_1           jsr _ReadTime                            ; Get current time
                 stz time+2                               ; tenths
                 sta time+3                               ; seconds

l136_2           jsr is_stop_key_down                     ; Allow user to abort
                 jsr _ReadTime                            ; Wait for seconds to increment
                 cmp time+3
                 beq l136_2
                 sta time+3

l136_3           jsr _ReadTime                            ; Wait for tenths to increment
                 cpz time+2
                 bne l136_3

                 dew time                                 ; Decrement sleep period 1 second
                 bne l136_2                               ; Loop until sleep period over

                 rts

;.end
;[[command.wait]]



; WAIT<location>,<mask1>[,<mask2>] statement waits until the contents of
; <location> is nonzero when XORed with mask2 and then ANDed with mask1.
; If mask2 is not present, it is assumed to be zero.

wait             jsr getnum                               ; get required mask1
                 stx andmsk
                 ldx #0
                 jsr chrgot
                 beq l137_1
                 jsr combyt                               ; get optional mask2
l137_1           stx eormsk

                 phz
                 ldz current_bank                         ; set up bank number for fetch
                 ldx #poker                               ; ..and address
                 ldy #0                                   ; ..and index

l137_2           bit current_bank
                 bmi l137_3                               ; NOMAP?
                 jsr _lda_far                             ; lda (poker),y
                 !text $2c

l137_3           lda (poker),y
                 eor eormsk
                 and andmsk
                 beq l137_2
                 plz
                 rts                                      ; got a nonzero

;.end
;[[function.fre]]



;*****************************************************************************
; FRE(n) Function
;
; Where: n=0 returns amount of free RAM in bank 0. This is the area
;  between top of text (TEXT_TOP) and top of RAM (MAX_MEM_0).
;
;  n=1 returns amount of free ram in bank 1. This is the area
;  between top of arrays (STREND) and bottom of strings (FRETOP).
;
;  n=2 returns the amount (???? presence) of expansion RAM.
;
;*****************************************************************************

fre              jsr conint                               ; get integer argument in .x
                 cpx #1                                   ; which bank?
                 beq l138_1                               ; go do bank one
                 cpx #2                                   ; go do expansion banks   [910107]
                 beq l138_2                               ; else it must be bank zero
                 +lbcs fcerr                              ; any other is unpleasant to talk about

                 sec                                      ; FRE(text_bank)
                 lda max_mem_0
                 sbc text_top
                 tay                                      ; set up result for nosflt
                 lda max_mem_0+1
                 sbc text_top+1
                 bra l138_3                               ; assumes text_top < max_mem


l138_1           jsr garba2                               ; FRE(var_bank) do garbage collect first
                 sec
                 lda fretop
                 sbc strend
                 tay
                 lda fretop+1
                 sbc strend+1
                 bra l138_3

l138_2           ldy _expansion                           ; FRE(expansion banks)    [910107]
                 lda #0

l138_3           +lbra nosflt                             ; go float the number (y,a)=(lo,hi)

;.end
;[[function.val]]



; The VAL function takes a string and turns it into a number by interpreting
; the PETSCII digits etc.  Except for the problem that a terminator must be
; supplied by replacing the character beyond the string, VAL is merely a call
; to floating point input (FIN).

val              jsr len1                                 ; get length
                 +lbeq zerofc                             ; return 0 if len=0

; Use text to fp number code by faking a new text poiner

val_1            clc                                      ; ///jump table entry.  convert PETSCII to floating point
                 adc index1
                 sta strng2                               ; add length to index1 and put in strng2
                 lda index1+1
                 adc #0
                 sta strng2+1

                 ldy #0
                 lda #strng2
                 jsr lda_far_ram1                         ; replace character after string with $00 (fake EOL)
                 pha                                      ; save old character
                 tya                                      ; (.A=0)
                 ldx #strng2
                 jsr sta_far_ram1 ;sta (strng2),y         ; ..and put in null
                 jsr fin_chrget_2                         ; get character pointed to and set flags.(sorta like chrgot)
                 ldx #1                                   ; flag 'bank 1'
                 jsr fin                                  ; go do evaluation
                 pla                                      ; get saved character
                 phx
                 ldx #strng2
                 ldy #0
                 jsr sta_far_ram1 ;sta (strng2),y         ; restore zeroed-out character
                 plx
                 rts

;.end
;[[function.dec]]



; DEC convert a hex string representing a 2-byte integer into decimal.

dcml             jsr len1                                 ; find length of string
                 sta index2                               ; len ret. in a
                 ldy #0
                 sty index2+1                             ; zero char counter
                 sty strng2+1                             ; zero out value
                 sty strng2

l139_1           cpy index2                               ; evaluated all characters?
                 beq l139_4                               ; branch if so
                 jsr indin1_ram1                          ; get next character from string
                 iny
                 cmp #' '                                 ; ignore spaces
                 beq l139_1
                 inc index2+1
                 ldx index2+1
                 cpx #5
                 bcs decbad                               ; can't have more than 4 characters

                 cmp #'0'
                 bcc decbad                               ; bad if < 0
                 cmp #':'                                 ; '9'+1
                 bcc l139_2                               ; ok if  = 0-9
                 cmp #'A'
                 bcc decbad                               ; bad if > 9  and < A
                 cmp #'G'
                 bcs decbad                               ; bad if > F

                 sbc #7                                   ; adjust if A-F  (.c is clr)
l139_2           sbc #$2f                                 ; adjust to $00..$0f (.c is set)
                 asl                                      ; shift low nibble to high
                 asl
                 asl
                 asl

                 ldx #4                                   ; mult. old val. by 16, add new
l139_3           asl
                 rol strng2
                 rol strng2+1
                 dex
                 bne l139_3
                 bra l139_1

l139_4           ldy strng2                               ; get lsb of value,
                 lda strng2+1                             ; & msb,
                 +lbra nosflt                             ; go float 2 byte unsigned integer


decbad
                 +lbra fcerr                              ; illegal qty error

;.end

;[[command.peekpoke]]



peek             phw poker                                ; ..also happens to be LINNUM!   [910911]
                 jsr chknum
                 jsr getadr
                 ldy #0                                   ; index
                 bit current_bank
                 bmi l140_1                               ; NOMAP?

                 phz
                 ldz current_bank                         ; set up bank number for Kernel's fetch
                 ldx #poker                               ; ..and address
                 jsr _lda_far                             ; lda (poker),y
                 plz
                 !text $2c

l140_1           lda (poker),y
                 tay                                      ; get byte into .y
                 pla
                 sta poker+1                              ; restore linnum
                 pla
                 sta poker
                 +lbra sngflt                             ; float it


poke             jsr getnum
l141_1           txa                                      ; set up value to store for Kernel 'stash' routine
                 ldy #0                                   ; ..and index
                 sei                                      ; to allow poking IRQ vector, etc.  [910612]
                 bit current_bank
                 bmi l141_2                               ; (anything >1Meg means NOMAP)

                 phz
                 ldx #poker                               ; ..and address
                 ldz current_bank                         ; ..finally, get the bank number
                 jsr _sta_far                             ; sta (poker),y
                 plz
                 !text $2c

l141_2           sta (poker),y                            ; NoMap

l141_3           jsr chrgot                               ; eol?
                 beq l141_4                               ; yes
                 inw poker                                ; no- increment address
; lda poker  ; check for segment wrap (FFFF->0000) [910911]
; ora poker+1
                 +lbeq omerr                              ; [910916]
                 jsr optbyt                               ; & get next [,byte]
                 bcs l141_1

l141_4           cli                                      ; [910612]
                 rts


;.end

;[[function.errstr]]


errd             jsr sign                                 ; get sign
                 bmi l142_1                               ; (allow err$(er) when er=-1)
                 jsr conint                               ; get integer arg in x
                 dex
                 txa                                      ; error # (0 to max-1)
                 cmp #last_error_message                  ; check range
                 bcc l142_2                               ; ok
                 ldx #0                                   ; too high, return null
                 !text $2c

l142_1           ldx #2                                   ; no error, return "ok"    [910911]
                 lda #<ok_error_message
                 ldy #>ok_error_message
                 sta index2
                 sty index2+1
                 bra l142_5                               ; pass it

l142_2           jsr erstup                               ; look up the error, set up a pointer to it
                 ldy #$ff                                 ; determine how long it is
                 ldx #0
l142_3           inx                                      ; count printing characters
l142_4           iny
                 lda (index2),y                           ; (rom: ind.ok)
                 bmi l142_5                               ; msb set means last
                 cmp #' '
                 bcc l142_4                               ; don't count non-printers
                 bra l142_3                               ; count all others

l142_5           txa                                      ; message length
                 jsr strspa                               ; get space
                 tax
                 beq l142_7                               ; null

; sta sw_rom_ram1  ;set up string bank????
                 ldx #0
                 ldy #$ff
l142_6           iny                                      ; copy message into memory
                 lda (index2),y                           ; (rom: ind.ok)
                 cmp #' '
                 bcc l142_6                               ; skip non-printers

                 pha
                 and #$7f
                 phy                                      ; swap x&y
                 phx
                 ply
                 ldx #dsctmp+1
                 jsr sta_far_ram1                         ; sta (dsctmp+1),y to RAM1
                 phy                                      ; swap x&y
                 plx
                 ply
                 inx
                 pla                                      ; test if msb was set
                 bpl l142_6

l142_7           +lbra chrd1                              ; pla,pla,jmp putnew


;.end
;[[function.hexstr]]



hexd             jsr chknum
                 phw poker                                ; save linnum    [910911]
                 jsr getadr                               ; 2 byte val in (poker)
                 lda #4
                 jsr strspa
                 ldy #0
                 lda poker+1
                 jsr hexit
                 lda poker
                 jsr hexit
                 pla                                      ; restore linnum
                 sta poker+1
                 pla
                 sta poker
                 +lbra chrd1                              ; pla,pla,jmp putnew

hexit            pha
                 lsr
                 lsr
                 lsr
                 lsr
                 jsr dohex
                 pla

dohex            and #$0f
                 cmp #$0a
                 bcc l143_1
                 adc #6
l143_1           adc #'0'
                 phx
                 ldx #dsctmp+1
                 jsr sta_far_ram1                         ; sta (dsctmp+1),y
                 plx
                 iny
                 rts

;.end
;[[function.joy]]



;*************************************************************
; JOY (n)  -- Return joystick status
;
; where: n =  1  return position of joystick-1
;       2  return position of joystick-2
;
; result:      0  no direction, no button
;       1-8    direction (see below), no button
;       128 no direction, button
;       129-136 direction & button  128 + [1...8]
;
; button--->  128        1
;       8     2
; stick--->  7           3
;       6     4
;          5
;
;*************************************************************

joy              jsr conint                               ; get 1 byte arg in x
                 dex
                 cpx #2                                   ; make sure arg. is valid
                 +lbcs fcerr                              ; >1, error

                 txa
                 eor #1                                   ; invert to match legends on case
                 tax
                 php                                      ; save status

; jsr put_io_in_map
                 sei                                      ; disable IRQ to inhibit kybd
                 lda d1pra
                 pha                                      ; save kybd output lines
                 ldy #$ff
                 sty d1pra                                ; set to not read any kybd inputs

l144_1           lda d1pra,x                              ; read joystick values
                 cmp d1pra,x                              ; debounce
                 bne l144_1

                 tax                                      ; save joystick values
                 pla
                 sta d1pra                                ; reset kybd output lines
                 txa                                      ; restore joystick values
                 plp                                      ; restore status
                 and #$0f                                 ; test which direction
                 tay
                 lda joytab-5,y                           ; get direction indicator
                 tay                                      ; save direction : 0-8
                 txa                                      ; restore joystick value
                 and #$10                                 ; test if button triggered
                 bne l144_2                               ; skip if not
                 tya
                 ora #$80                                 ; show trigger depressed
                 tay
l144_2           +lbra sngflt                             ; float 1 byte arg in y.

joytab           !text 4,2,3,0,6,8,7,0,5,1,0

;.end

;[[function.potpen]]


;***********************************************************
; POT(n)  --  Read paddles
;
;    n = 1 : paddle-1 - X-position
;  2 : paddle-1 - Y-position
;  3 : paddle-2 - X-position
;  4 : paddle-2 - Y-position
;
;     result >= 256 --  trigger set
;***********************************************************

pot              jsr chkcls                               ; look for closing paren
                 jsr conint                               ; get 1-byte arg in .x
                 dex
                 cpx #4
                 +lbcs fcerr                              ; value error

; jsr put_io_in_map
                 txa                                      ; convert arg (0-3) into paddle enables
                 lsr                                      ; .c= X/Y   .a= port 1/2
                 tax
                 lda sbits+6,x
                 tax                                      ; (CIA paddle port, $40/$80)
                 lda #0
                 rol
                 tay                                      ; (SID x/y offset,  $00/$01)

                 stx pot_temp_1                           ; save which port
                 php                                      ; save IRQ enable while we
                 sei                                      ; disable IRQ to inhibit keyboard scan
                 lda d1pra
                 pha                                      ; save kybd output lines
                 stx d1pra                                ; turn on correct paddle

                 jsr go_slow
                 ldx #0
l145_1           inx                                      ; delay to let pot be read by SID
                 bne l145_1

l145_2           lda sid1+25,y                            ; read pot
                 cmp sid1+25,y                            ; debounce
                 bne l145_2
                 sta pot_temp_2                           ; save pot value
                 jsr go_fast

                 ldx #0                                   ; set index to d1pra
                 bit pot_temp_1                           ; test if pot-0,1 or pot-2,3
                 bmi l145_3                               ; skip if pot 2,3
                 inx                                      ; index to d1prb
l145_3           lda #04                                  ; use joy line-2
                 dey                                      ; test if pot-x or pot-y
                 bmi l145_4                               ; skip if pot-x
                 asl                                      ; use joy line-3
l145_4           ldy #$ff
                 sty d1pra                                ; disable keybd inputs
                 iny                                      ; set to zero for no trigger
                 and d1pra,x                              ; test if trigger set
                 bne l145_5                               ; skip if not trigger
                 iny                                      ; return value >255 for trigger
l145_5           pla
                 sta d1pra                                ; restore keybd lines
                 tya
                 ldy pot_temp_2                           ; restore pot value
                 plp                                      ; restore status
                 +lbra nosflt                             ; output 2-byte result


;*************************************************************
;  LPEN(n)  --  Read light pen
;
; n = 0 x position
;     1 y position
;*************************************************************

lpen             jsr chkcls                               ; look for closing parens
                 jsr conint                               ; get 1 byte arg in .X
; dex   ;convert [1-2] to [0-1]
                 cpx #2
                 +lbcs fcerr                              ; bad value

                 lda #0
                 sei
                 ldy lightpen_xpos,x                      ; get latched light pen value (a=msb, y=lsb)
                 sta lightpen_xpos,x                      ; reset to zero (????preserve last latched position)
                 cli
                 cpx #0
                 bne l146_1                               ; done if y position
                 tya
                 asl                                      ; else multiply *2 to get correct x position
                 tay                                      ; lsb
                 lda #0
                 rol                                      ; msb
l146_1           +lbra nosflt                             ; float it (y,a)


;.end
;[[function.pointer]]



;******************************************************************
;
; POINTER(var_name) - Return address of descriptor for var_name
;
;******************************************************************

pointer          jsr chrget                               ; skip over escape token
                 jsr chkopn                               ; test for open paren
                 jsr isletc                               ; test if character follows parens
                 +lbcc snerr                              ; ...syntax error if not.
                 jsr ptrget                               ; look for this varname in table

pointer_ret      =*-1
                 tax
                 phy
                 jsr chkcls                               ; look for closing paren
                 txa
                 tay
                 pla
                 cmp #>zero                               ; is this a dummy pointer?
                 bne l147_1
                 lda #0                                   ; if so, return 0
                 tay
l147_1           +lbra nosflt

;.end
;[[operator.xor]]



;**************************************************************
;*
;*   XOR - Exclusive-or two 16 bit arguments
;*
;* Syntax : XOR (arg1, arg2)
;*
;**************************************************************

xor              phw poker                                ; protect the poker value (could be in use)  [910911]
                 jsr chknum
                 jsr getadr                               ; get first arg
                 pha                                      ; save MSB
                 phy                                      ; save LSB

                 jsr comwrd                               ; check for comma, get word
                 jsr chkcls                               ; check for closing parens

                 pla
                 eor poker                                ; xor LSB (comwrd left a copy of its arg in POKER)
                 tay
                 pla
                 eor poker+1                              ; ..and MSB
                 jsr nosflt                               ; ..and go float 'em

                 pla
                 sta poker+1
                 pla
                 sta poker
                 rts

;.end
;[[operator.mod]]



;**************************************************************
;*
;* MOD  -  Modulus of a number
;*
;* Syntax : MOD (number, range)
;*      910402 FAB
;**************************************************************

; Calculate   MOD = NUMBER-RANGE*INT(NUMBER/RANGE)

mod              jsr chknum                               ; 1st arg in FAC1 (number)
                 jsr pushf1                               ; save two copies of it for later
                 jsr pushf1
                 jsr chkcom                               ; check for comma
                 jsr frmnum                               ; 2nd arg in FAC1 (range)
                 jsr chkcls                               ; check for closing paren

                 jsr movaf                                ; save range in FAC2
                 jsr pullf1                               ; get back number in FAC1
                 ldx #5                                   ; swap FAC1 and FAC2
l148_1           lda facexp,x
                 ldy argexp,x
                 sta argexp,x
                 sty facexp,x
                 dex
                 bpl l148_1
                 jsr pushf1                               ; save one copy of range for later

                 jsr fdivt_c65                            ; number/range
                 jsr int                                  ; INT(number/range)
                 jsr movaf                                ; round & move to FAC2
                 jsr pullf1                               ; retrieve arg2 (range)
                 jsr fmultt_c65                           ; range*INT(number/range)
                 jsr negop                                ; -range*INT(number/range)
                 jsr movaf                                ; move to FAC2
                 jsr pullf1                               ; retrieve arg1 (number)
                 +lbra faddt_c65                          ; number-range*INT(number/range)


;.end

;[[function.rwindow]]


;******************************************************************************
;
; RWINDOW  - Returns information about the current console display environment.
;
;   Syntax : RWINDOW (n)
;
;   Where: n=0 : number of lines in the current window
;   =1 : number of rows in the current window
;   =2 : returns either 40 or 80, depending on the
;   current console device
;
;******************************************************************************

rwindow          jsr chkcls
                 jsr conint
                 cpx #2
                 beq l149_2                               ; return current console
                 +lbcs fcerr

                 cpx #0
                 bne l149_1

                 lda _screen_bottom
                 sec
                 sbc _screen_top
                 bra l149_3                               ; always

l149_1           lda _screen_right
                 sec
                 sbc _screen_left
                 bra l149_3                               ; always


l149_2           lda #80                                  ; assume 80 col
                 bbr7 _mode,l149_3
                 lsr
l149_3           tay
                 +lbra sngflt                             ; float 1 byte arg in .Y

;.end
;[[function.rnd]]



;    Random Number Function  RND(x)
;
;  x=0 ==> generate a random number based on hardware clock & noisy POT lines
;  x<0 ==> seed a reproducable, pseudo-random number generator
;  x>0 ==> generate a reproducable pseudo-random # based upon seed value above


rnd              jsr sign                                 ; get sign into .a

rnd_0            bmi l150_2                               ; /// entry from jump table
                 bne l150_1


; Get value from hardware

                 jsr go_slow                              ; Use CIA#1 timer B & SID#2 pot X & Y for seeds  [910314]
                 lda sid2+25                              ; go slow to read POT-X
                 asl
                 asl
                 asl
                 asl
                 ora sid2+26                              ; and POT-Y
                 eor vic+18                               ; ???? should be okay- we're in Slow mode
                 sta facmoh
                 jsr go_fast                              ; restore speed
                 lda d1pra+6                              ; timer B is free-running
                 sta facmo
                 lda d1pra+7
                 sta faclo
                 eor facho
                 adc facmoh
                 eor facmo
                 adc faclo
                 sta facho
                 bra l150_3


l150_1           lda #<rndx                               ; get last one into FAC
                 ldy #>rndx
                 jsr movfm
                 lda #<rmulc
                 ldy #>rmulc                              ; FAC was zero.  restore last one
                 jsr rommlt                               ; multiply by random constant
                 lda #<raddc
                 ldy #>raddc
                 jsr romadd                               ; add random constant

l150_2           ldx faclo
                 lda facho
                 sta faclo
                 stx facho                                ; reverse hi and lo
                 ldx facmoh
                 lda facmo
                 sta facmoh
                 stx facmo

l150_3           lda #0                                   ; strnex.  make number positive
                 sta facsgn
                 lda facexp                               ; put exp where it will
                 sta facov                                ; be shifted in by normal
                 lda #$80
                 sta facexp                               ; make result between 0 and 1
                 jsr normal                               ; normalize
                 ldx #<rndx
                 ldy #>rndx
                 +lbra movmf                              ; put new one into memory

rmulc            !text 152,53,68,122,0
raddc            !text 104,40,177,70,0

;.end

;[[math.ext1]]


n32768           !text $90,$80,0,0,0


flpint           jsr ayint
                 lda facmo
                 ldy  faclo
                 rts


intidx           jsr chrget
                 jsr frmevl                               ; get a number


posint           jsr chknum
                 lda facsgn
                 bmi nonono                               ; if negative, blow him out


ayint            lda facexp
                 cmp #$90                                 ; FAC > 32767?
                 bcc qintgo
                 lda #<n32768                             ; get address of -32768
                 ldy #>n32768
                 jsr fcomp                                ; see if FAC=((x))

nonono           +lbne fcerr                              ; no, FAC is too big
qintgo           +lbra qint                               ; go shove it


; Float an unsigned double byte integer
; Entry:  MSB in (a), LSB in (y)

nosflt           jsr stoint
                 sec                                      ; sign is positive
                 +lbra floatc



pos              sec
                 jsr _plot                                ; get tab pos in .y

sngflt           lda #0
                 +lbra givayf                             ; float it



stoint           ldx #0                                   ; move int to fac & compute proper exponents
                 stx valtyp
                 sta facho
                 sty facho+1
                 ldx #$90
storts           rts



; See if we are in direct mode, and complain if so.

errdir           bbs7 runmod,storts                       ; goto error if not in run mode

                 ldx #errid                               ; input direct error code
                 !text $2c

errguf           ldx #erruf
                 +lbra error


errind           bbr7 runmod,storts                       ; goto error if not in direct mode
                 ldx #erroid
                 +lbra error

;.end

;[[function.userdef]]


; User Defined Function Code
;
; Note only single arguments are allowed to functions, and functions must
; be of the single line form:
;
;  DEF FNA(x)=x~2 + x-2
;
; No strings may be involved with these functions.
;
; Idea: create a simple variable entry whose first character has the MSB set.
; The value will be:
;
;  A text pointer to the formula
;  A pointer to the argument variable

def              jsr getfnm                               ; get a pointer to the function
                 jsr errdir
                 jsr chkopn                               ; must have a (
                 lda #$80
                 sta subflg                               ; prohibit subscripted & integer variables
                 jsr ptrget                               ; get pointer to argument
                 jsr chknum                               ; is it a number?
                 jsr chkcls                               ; must have )
                 lda #equal_token                         ; followed by =
                 jsr synchr
                 pha
                 lda varpnt+1
                 pha
                 lda varpnt
                 pha
                 lda txtptr+1
                 pha
                 lda txtptr
                 pha
                 jsr data
                 bra deffin


; Subroutine to get a pointer to a function name

getfnm           lda #fn_token                            ; must start with fn
                 jsr synchr
                 ora #$80                                 ; put function bit on
                 sta subflg                               ; (disallows array & integer variables)
                 jsr ptrgt2                               ; get pointer to function or create anew
                 sta defpnt
                 sty defpnt+1
                 +lbra chknum                             ; make sure it's not a string, and return


fndoer           jsr getfnm                               ; get the function's name
                 lda defpnt+1
                 pha
                 lda defpnt
                 pha
                 jsr parchk                               ; evaluate parameter
                 jsr chknum
                 pla
                 sta defpnt
                 pla
                 sta defpnt+1
                 ldy #2
                 jsr inddef                               ; get pointer to the variable
                 sta varpnt                               ; save variable pointer
                 tax
                 iny
                 jsr inddef
                 beq errguf
                 sta varpnt+1
                 iny                                      ; since def uses only 4


defstf           lda #varpnt
                 jsr lda_far_ram1
                 pha                                      ; push it all on the stack, since we might be recursing
                 dey
                 bpl defstf
                 ldy varpnt+1

                 jsr movmf_ram1                           ; put current FAC into our argument variable
                 lda txtptr+1                             ; save variable pointer
                 pha
                 lda txtptr
                 pha
                 jsr inddef                               ; get pointer to function
                 sta txtptr
                 iny
                 jsr inddef
                 sta txtptr+1
                 lda varpnt+1                             ; save variable pointer
                 pha
                 lda varpnt
                 pha
                 jsr frmnum                               ; evaluate variable, and check numeric
                 pla
                 sta defpnt
                 pla
                 sta defpnt+1
                 jsr chrgot
                 +lbne snerr                              ; it didn't terminate, syntax error

                 pla                                      ; restore text pointer
                 sta txtptr
                 pla
                 sta txtptr+1

deffin           ldy #0
l151_1           pla                                      ; get old arg value off stack,
                 phx
                 ldx #defpnt
                 jsr sta_far_ram1 ;sta (defpnt),y         ; and put it back in variable
                 plx
                 iny
                 cpy #5
                 bne l151_1
                 rts

;.end
;[[function.stringmisc]]




; The STR$() function takes a number and gives a string with
; the characters the output of the number would have given.

strd             jsr chknum                               ; arg has to be numeric
                 ldy #0
                 jsr foutc                                ; do its output
                 pla
                 pla

timstr           lda #<lofbuf
                 ldy #>lofbuf
                 +lbra strlit


; CHR$() creates a string which contains as its only character the PETSCII
; equivalent of the integer argument (#) which must be < 256.

chrd             jsr conint                               ; get integer in range
                 phx
                 lda #1                                   ; one-character string
                 jsr strspa                               ; get space for string
                 ldy #0
                 pla
; phx   ;set up string bank
                 ldx #dsctmp+1
                 jsr sta_far_ram1                         ; sta (dsctmp+1),y
; plx

chrd1            pla                                      ; get rid of "chknum" return address
                 pla
                 +lbra putnew                             ; setup FAC to point to desc


; The following is the LEFT$($,#) function.  It takes the leftmost # characters
; of the string.  If # > len of the string, it returns the whole string.

leftd            jsr pream                                ; test parameters
                 pha                                      ; # arg
                 jsr inddpt                               ; string len
                 sta syntmp
                 pla
                 cmp syntmp
                 tya                                      ; that's all there is to LEFT$

rleft            bcc l152_1
                 jsr inddpt
                 tax                                      ; put length into x
                 tya                                      ; zero (a), the offset
l152_1           pha                                      ; save offset
rleft2           txa
rleft3           pha                                      ; save length
                 jsr strspa                               ; get space
                 lda dscpnt
                 ldy dscpnt+1
                 jsr fretmp
                 ply
                 pla
                 clc
                 adc index                                ; compute where to copy
                 sta index
                 bcc l153_1
                 inc index+1
l153_1           tya
                 jsr movdo                                ; go move it
                 +lbra putnew



rightd           jsr pream
                 pha
                 jsr inddpt
                 sta syntmp
                 pla
                 clc                                      ; (length des'd)-(length)-1
                 sbc syntmp
                 eor #$ff                                 ; negate
                 bra rleft


; MID$($,#) returns string with chars from # position onward. If # > LEN($)
; then return null string.  MID($,#,#) returns string with characters from
; # position for #2 characters.  If #2 goes past end of string return as much
; as possible.

midd             lda #255                                 ; default
                 sta faclo                                ; save for later compare
                 jsr chrgot                               ; get current character
                 cmp #')'                                 ; is it a right paren )?
                 beq l154_1                               ; no third paren.
; jsr chkcom  ;must have comma
; jsr getbyt  ;get the length into "faclo"
                 jsr combyt                               ; [910820]

l154_1           jsr pream                                ; check it out
                 +lbeq fcerr                              ; illegal qty error
                 dex                                      ; compute offset
                 phx
                 phx                                      ; preserve a while (2 copies)
                 ldx #0
                 jsr inddpt                               ; get length of what's left
                 sta syntmp
                 pla
                 clc
                 sbc syntmp
                 bcs rleft2                               ; give null string
                 eor #$ff                                 ; in sub c was 0 so just complement
                 cmp faclo                                ; greater than what's desired
                 bcc rleft3                               ; no, just copy that much
                 lda faclo                                ; get length of what's desired
                 bcs rleft3                               ; copy it




; Common routine used by RIGHT$, LEFT$, MID$, for parameter chk and setup.

pream            jsr chkcls                               ; param list should end
                 ply
                 pla
                 sta jmper+1                              ; get return address
                 pla                                      ; get rid of fingo's jsr ret addr
                 pla
                 plx                                      ; get length
                 pla
                 sta dscpnt
                 pla
                 sta dscpnt+1
                 lda jmper+1
                 pha
                 phy
                 ldy #0
                 txa
                 rts



; The function LEN$() returns the length of the string passed as an argument.

len              bsr len1
                 +lbra sngflt

len1             jsr frestr                               ; free up string
                 ldx #0
                 stx valtyp                               ; force numeric
                 tay                                      ; set condition codes
                 rts                                      ; done





; The following is the ASC$() function.  It returns an integer which is the
; decimal equivalent of the PETSCII string argument.

asc              jsr len1
                 beq l155_1                               ; it was null (zero length)
                 ldy #0
                 jsr indin1_ram1                          ; get 1st character
                 tay
l155_1           +lbra sngflt

;.end





; STRINI gets string space for the creation of a string and creates
; a descriptor for it in DSCTMP.

strini
                 ldx facmo                                ; get facmo to store in dscpnt
                 ldy facmo+1
                 stx dscpnt                               ; retain the descriptor pointer
                 sty dscpnt+1

strspa           jsr getspa                               ; get string space
                 stx dsctmp+1                             ; save location
                 sty dsctmp+2
                 sta dsctmp                               ; save length
                 rts                                      ; done


; STRLT2 takes the string literal whose first character is pointed to by
; (xreg)+1 and builds a descriptor for it.  The descriptor is initially
; built in DSCTMP, but PUTNEW transfers it into a temporary and leaves a
; pointer to the temporary in FACMO & FACLO.  The characters other than the
; zero that terminates the string should be set up in CHARAC and ENDCHR.
; If the terminator is a quote, the quote is skipped over.  Leading quotes
; should be skipped before call.  On return, the character after the string
; literal is pointed to by (strng2).


strlit           ldx #'"'                                 ; assume string ends on quote
                 stx charac
                 stx endchr

strlt2           sta strng1                               ; save pointer to string
                 sty strng1+1
                 sta dsctmp+1                             ; in case no strcpy
                 sty dsctmp+2

                 ldy #255                                 ; initialize character count
strget           iny
                 jsr indst1                               ; get character
                 beq l156_2                               ; if zero
                 cmp charac                               ; this terminator?
                 beq l156_1                               ; yes
                 cmp endchr
                 bne strget                               ; look further

l156_1           cmp #'"'                                 ; strfin.  quote?
                 beq l156_3

l156_2           clc
l156_3           sty dsctmp                               ; no, back up. retain count
                 tya
                 adc strng1                               ; wishing to set (txtptr)
                 sta strng2
                 ldx strng1+1
                 bcc l156_4
                 inx
l156_4           stx strng2+1
                 tya


strlit_1                                                  ; //// entry from SPRSAV
                 jsr strini
                 tay
                 beq putnew                               ; length=0, don't bother copying
                 pha                                      ; save length
                 phx
                 ldx #frespc
l157_1           dey
                 jsr indst1                               ; lda (strng1),y in bank 0
                 jsr sta_far_ram1                         ; sta (frespc),y in bank 1
                 tya
                 bne l157_1
                 plx
                 pla                                      ; restore length
                 jsr mvdone                               ; finish up by updating frespc


; Some string function is returning a result in DSCTMP.  Set up a temp
; descriptor with DSCTMP in it.  Put a pointer to the descriptor in FACMO&LO
; and flag the result as a string type.

putnew           ldx temppt                               ; pointer to first free temp
                 cpx #tempst+strsiz+strsiz+strsiz
                 +lbeq sterr                              ; string temporary error

                 lda dsctmp                               ; length
                 sta 0,x
                 lda dsctmp+1                             ; pointer to string lo
                 sta 1,x
                 lda dsctmp+2                             ; hi
                 sta 2,x

                 ldy #0                                   ; pointer to temp. descriptor
                 stx facmo                                ; lo
                 sty facmo+1                              ; hi
                 sty facov
                 dey                                      ; ($ff)
                 sty valtyp                               ; type is string
                 stx lastpt                               ; set pointer to last-used temp

                 inx
                 inx
                 inx                                      ; point further
                 stx temppt                               ; save pointer to next temp, if any
                 rts                                      ; all done


; The following routine concatenates two strings.  At this point, the FAC
; contains the first one and (txtptr) points to the + sign.

cat              lda faclo                                ; push high order onto stack
                 pha
                 lda facmo                                ; and the low
                 pha
                 jsr eval                                 ; can come back here since operator is known
                 jsr chkstr                               ; must be string
                 pla
                 sta strng1                               ; get high order of old descriptor
                 pla
                 sta strng1+1
                 ldy #0
                 jsr indst1_ram1                          ; get length of old string
                 sta syntmp
                 jsr indfmo
                 clc
                 adc syntmp
                 +lbcs errlen                             ; result >255, error "long string"

                 jsr strini                               ; sizeok.  initialize string
                 jsr movins                               ; move it
                 lda dscpnt                               ; get pointer to second
                 ldy dscpnt+1
                 jsr fretmp                               ; free it
                 jsr movdo                                ; move second string
                 lda strng1
                 ldy strng1+1
                 jsr fretmp
                 jsr putnew
                 +lbra tstop                              ; "cat" reenters frmevl from tstop


movins           ldy #0                                   ; get address of string
                 jsr indst1_ram1
                 pha
                 iny
                 jsr indst1_ram1
                 tax
                 iny
                 jsr indst1_ram1
                 tay
                 pla

movstr           stx index                                ; adr in (x,y), len in a
                 sty index+1

movdo            tay
                 beq mvdone

                 pha
                 phx
                 ldx #frespc
l158_1           dey
                 jsr indin1_ram1
                 jsr sta_far_ram1                         ; sta (frespc),y
                 tya
                 bne l158_1
                 plx
                 pla

mvdone           clc                                      ; update frespc pointer
                 adc frespc
                 sta frespc
                 bcc l159_1
                 inc frespc+1
l159_1           rts


;[[string.manager]]
; FRETMP is passed a string descriptor pntr in (a,y).  A check is made to see
; if the string descriptor points to the last temporary descriptor allocated by
; putnew.  If so, the temporary is freed up by the updating of (temppt).  If a
; string is freed up, a further check sees if it was the last one created and if
; so, (fretop) is updated to reflect the fact that the space is no longer in use.
; The address of the actual string is returned in (x,y) and its length in (a).

frmstr           jsr frmevl

frestr           jsr chkstr                               ; make sure it's a string
frefac           lda facmo                                ; free up string pointed to by FAC
                 ldy facmo+1
fretmp           sta index                                ; get length for later
                 sty index+1
                 jsr fretms                               ; check desc. if last
                 bne l160_3                               ; one then scratch it
                 jsr stradj                               ; index points to link
                 bcc l160_3                               ; literal no fix

                 phx                                      ; .x=length
                 dey                                      ; .y=1
                 ldx #index
                 lda #$ff                                 ; flag string as garbage
                 jsr sta_far_ram1                         ; sta (index),y
                 pla
                 pha                                      ; get length, but leave copy on stack
                 dey
                 ldx #index
                 jsr sta_far_ram1 ;sta (index),y          ; put in length

                 eor #$ff                                 ; put index back
                 sec                                      ; to first byte
                 adc index
                 ldy index+1
                 bcs l160_1
                 dey
l160_1           sta index
                 sty index+1

                 tax                                      ; lo into x
                 pla                                      ; pull length from stack
                 cpy fretop+1                             ; = to fretop?
                 bne frerts
                 cpx fretop
                 bne frerts


; The string was the last one put into string space.  Save garbage
; collection some time by freeing up. (length + 2)

                 pha                                      ; save length on stack
                 sec                                      ; plus one
                 adc fretop
                 sta fretop
                 bcc l160_2
                 inc fretop+1
l160_2           inw fretop                               ; + one more
                 pla                                      ; pull length off stack
                 rts


l160_3           ldy #0                                   ; set up x,y,a and index
                 jsr indin1_ram1                          ; length
                 pha                                      ; on stack
                 iny
                 jsr indin1_ram1                          ; pointer lo
                 tax
                 iny
                 jsr indin1_ram1                          ; pointer hi
                 tay
                 stx index
                 sty index+1
                 pla                                      ; get back length
                 rts



fretms           cpy lastpt+1                             ; last entry to temp?
                 bne frerts
                 cmp lastpt
                 bne frerts
                 sta temppt
                 sbc #strsiz                              ; point to lst one
                 sta lastpt                               ; update temp pointer
                 ldy #0                                   ; also clears zflg so we do rest of fretmp
frerts           rts                                      ; all done

;.end
;[[string.garbage]]



;  Get space for a string, perhaps forcing garbage collection.
;
;  Entry:  a = # of chars
;  Exit:   (x,y) pointer to space, otherwise
;          blows off to 'out of string space' error
;          (also preserves .a and sets frespc= y,x = -> at space.)


getspa           lsr garbfl                               ; signal no garbage collection yet

tryag2           tax                                      ; save in x also
                 beq getrts                               ; length of 0 no go...
                 pha                                      ; save a (length) on stack
                 lda fretop                               ; lo byte
                 sec                                      ; for subtract
                 sbc #2                                   ; minus 2 (link bytes)
                 ldy fretop+1
                 bcs l161_1
                 dey
l161_1           sta index1                               ; save for later
                 sty index1+1
                 txa
                 eor #$ff
                 sec
                 adc index1
                 bcs l161_2
                 dey
l161_2           cpy strend+1
                 bcc garbag
                 bne strfre
                 cmp strend
                 bcc garbag                               ; clean up


strfre           sta frespc
                 sty frespc+1
                 ldy #1                                   ; flag string as garbage
                 lda #$ff
                 phx                                      ; set up string bank
                 ldx #index1
                 jsr sta_far_ram1 ;sta (index1),y         ; flag
                 plx
                 dey
                 pla                                      ; length
                 phx                                      ; set up string bank
                 ldx #index1
                 jsr sta_far_ram1 ;sta (index1),y         ; length
                 plx
                 ldx frespc
                 ldy frespc+1
                 stx fretop
                 sty fretop+1                             ; save new (fretop)
getrts           rts


garbag           lda garbfl
                 +lbmi omerr                              ; if out of memory
                 jsr garba2
                 sec
                 ror garbfl
                 pla                                      ; get back string length
                 bra tryag2                               ; always branches



; Routine looks for and squashes out any unused string space it finds, thus
; returning the space for future use by the string routines.  GARBA2 is called
; only when BASIC needs space or the FRE() function is used.


garba2           ldx temppt                               ; ptr to temp. strings
l162_1           cpx #tempst                              ; any out there?
                 beq l162_2                               ; none
                 jsr slr1                                 ; setup ptr (tempf2) to temp. string's bkptr
                 beq l162_1                               ; (skip if null string!)
                 txa                                      ; .x = lsb of ptr to descriptor
                 phx                                      ; set up string bank
                 ldx #tempf2
                 ldy #0
                 jsr sta_far_ram1 ;(tempf2),y             ; place backpointer on string to temp. descr
                 tya                                      ; .a = msb of ptr (0)
                 iny
                 jsr sta_far_ram1                         ; (tempf2),y
                 plx
                 bra l162_1                               ; always


l162_2           ldy #0                                   ; set up flag
                 sty highds
                 ldx max_mem_1
                 ldy max_mem_1+1
                 stx grbtop                               ; set both pointers
                 stx grbpnt
                 stx frespc
                 sty grbtop+1
                 sty grbpnt+1
                 sty frespc+1
                 txa


; do while (grbpnt <= fretop)

gloop            jsr chkgrb                               ; check garbage string
                 bne l163_2                               ; if not garbage

l163_1           dey                                      ; back up to length
                 jsr indgrb
                 jsr movpnt                               ; move grbpnt to next
                 sec
                 ror highds                               ; indicate garbage string found
                 bra gloop                                ; always

l163_2           bit highds
                 bpl l163_6                               ; if garbage string not found
                 ldx #0
                 stx highds                               ; clear indicator

                 lda #2                                   ; skip pointers past

; Move a string over garbage

l163_3           phx
                 ldx #grbtop
                 ldy #1                                   ; move the link bytes
                 jsr indgrb
                 jsr sta_far_ram1                         ; sta (grbtop),y
                 dey
                 jsr indgrb
                 jsr sta_far_ram1                         ; sta (grbtop),y
                 plx

                 jsr indin1_ram1
                 tax
                 jsr movtop                               ; move top pointer
                 sta frespc                               ; save in frespc
                 sty frespc+1
                 txa
                 jsr movpnt                               ; move grbpnt
                 txa                                      ; put length-1 in .y
                 tay

l163_4           dey
                 jsr indgrb
                 phx
                 ldx #grbtop
                 jsr sta_far_ram1                         ; sta (grbtop),y
                 plx
                 dex
                 bne l163_4

                 ldy #2                                   ; fix the descriptor
                 phx
                 ldx #index1
l163_5           lda grbtop-1,y
                 jsr sta_far_ram1                         ; sta (index1),y
                 dey
                 bne l163_5
                 plx

                 lda grbpnt                               ; check pointer
                 ldy grbpnt+1
                 jsr chkgrb                               ; check garbage string
                 beq l163_1                               ; if garbage found
                 bne l163_3                               ; always

l163_6           ldy #0                                   ; skip over good strings
                 jsr indin1_ram1
                 tax
                 jsr movtop
                 sta frespc
                 sty frespc+1
                 txa
                 jsr movpnt
                 bra gloop

;[[string.garbage.utils]]

; Subroutines used for garbage collection.
;
;  Compare for (y,a) = fretop.
; Entry  (y,a) = address of current string descriptor.
; Exits to caller if (y,a) = fretop, else z flag set if garbage string.
;      z flag clear if not garbage string.
; In either case pointers are setup for next loop and string movement.
; If carry clear (y,a) <= fretop


chkgrb           cpy fretop+1                             ; end of strings?
                 bcc l164_5
                 bne l164_1                               ; if not equal
                 cmp fretop
                 beq l164_5
                 bcc l164_5

l164_1           bit highds                               ; check flag
                 bmi l164_2                               ; if empty string found
                 lda #2                                   ; skip pointers past
                 jsr movtop                               ; move top pointer

l164_2           lda #2                                   ; skip pointers past
                 jsr movpnt                               ; move pointers
                 ldy #1
                 jsr indgrb                               ; garbage?
                 cmp #$ff
                 beq l164_4                               ; yes

l164_3           jsr indgrb                               ; to link bytes
                 sta index1,y
                 dey
                 bpl l164_3                               ; if two bytes not moved
l164_4           rts


l164_5           ldx temppt                               ; ptr to temp. strings

l164_6           cpx #tempst                              ; any out there?
                 beq l164_7                               ; no
                 jsr slr1                                 ; setup ptr (tempf2) to temp. string's bkptr.
                 beq l164_6                               ; (skip if null string!)
                 phx
                 ldx #tempf2
                 ldy #0                                   ; .a = string length
                 jsr sta_far_ram1 ;sta (tempf2),y         ; remove backpointer built at garba2
                 iny
                 lda #$ff
                 jsr sta_far_ram1 ;sta (tempf2),y         ; and mark as garbage
                 plx
                 bra l164_6                               ; always

l164_7           pla                                      ; throw away return address
                 pla
                 lda frespc                               ; fix fretop and frespc
                 ldy frespc+1
                 sta fretop
                 sty fretop+1
                 rts


movpnt           eor #$ff                                 ; comp and add
                 sec
                 adc grbpnt
                 ldy grbpnt+1
                 bcs l165_1
                 dey
l165_1           sta grbpnt
                 sty grbpnt+1
                 rts



movtop           eor #$ff                                 ; comp and add
                 sec
                 adc grbtop
                 ldy grbtop+1
                 bcs l166_1
                 dey
l166_1           sta grbtop
                 sty grbtop+1
                 rts



slr1             dex                                      ; .x = ptr to temp. string descriptor
                 lda 0,x                                  ; msb of ptr to string
                 sta tempf2+1
                 dex
                 lda 0,x                                  ; lsb of ptr to string
                 sta tempf2
                 dex
                 lda 0,x                                  ; string length
                 pha                                      ; save for later test
                 clc
                 adc tempf2                               ; want ptr to string's backpointer
                 sta tempf2
                 bcc l167_1
                 inc tempf2+1
l167_1           pla   ;.a=len & set z flag               ; .x=next desc. ptr
                 rts

;.end
;[[math.ext2]]



gtbytc           jsr chrget

getbyt           jsr frmnum                               ; read formula into FAC

conint           jsr posint                               ; convert the FAC to a single byte int
                 ldx facmo
                 +lbne fcerr                              ; result must be <= 255
                 ldx faclo
                 jmp chrgot                               ; set condition codes on terminator


getnum                                                    ; get 2-byte value in y,a: check for a comma, get 1 byte val in x
                 jsr frmnum                               ; get address
                 jsr getadr                               ; get that location

combyt                                                    ; check for a comma, get a 1 byte value in x
                 jsr chkcom                               ; check for comma
                 bra getbyt                               ; get something to store and return


comwrd           jsr chkcom

getwrd           jsr frmnum                               ; get an unsigned 2-byte value in y,a

getadr           lda facsgn                               ; for this entry, value can't be < 0
                 +lbmi fcerr                              ; function call error

getsad                                                    ; get a signed 2-byte value in (y,a), ///entry from sprcor
                 lda facexp                               ; examine exponent
                 cmp #145
                 +lbcs fcerr                              ; function call error
                 jsr qint                                 ; integerize it
                 lda facmo
                 ldy facmo+1
                 sty poker
                 sta poker+1
                 rts                                      ; it's all done

;.end
;[[math.ext3]]



; Floating Point Math Package configuration:
;
; Throughout the math package the floating point format is as follows:
;
; the sign of the first bit of the mantissa.
; the mantissa is 24 bits long.
; the binary point is to the left of the msb.
; number = mantissa * 2 ~ exponent.
; the mantissa is positive with a 1 assumed to be where the sign bit is.
; the sign of the exponent is the first bit of the exponent.
; the exponent is stored in excess $80, i.e., with a bias of +$80.
; so, the exponent is a signed 8 bit number with $80 added to it.
; an exponent of zero means the number is zero.
; the other bytes may not be assumed to be zero.
; to keep the same number in the fac while shifting,
; to shift right, exp:=exp+1.
; to shift left,  exp:=exp-1.
;
; In memory the number looks like this:
; the exponent as a signed number +$80.
; the sign bit in 7, bits 2-8 of mantissa are bits 6-0.
;  remember bit 1 of mantissa is always a one.
; bits 9-16 of the mantissa.
; bits 17-24 of the mantisa.
;
; Arithmetic routine calling conventions:
;
;   For one-argument functions:
; the argument is in the fac.
; the result is left in the fac.
;   For two-argument operations:
; the first argument is in arg (argexp,ho,mo,lo and argsgn).
;       the second argument is in the fac.
; the result is left in the fac.
;
; The "t" entry points to the two argument operations have both arguments setup
; in the respective registers. Before calling arg may have been popped off the
; stack and into arg, for example. The other entry point assumes (xreg) points
; to the argument somewhere in memory. it is unpacked into arg by "conupk".
;
; On the stack, the sgn is pushed on first, the lo,mo,ho, and finally exp.
; Note all things are kept unpacked in arg, fac and on the stack.
;
; It is only when something is stored away that it is packed to four bytes,
; the unpacked format has a sn byte reflecting the sign of the ho turned on.
; The exp is the same as stored format. This is done for speed of operation.


fsub             jsr conupk

fsubt            lda facsgn
                 eor #$ff                                 ; complement it
                 sta facsgn
                 eor argsgn                               ; complement arisgn
                 sta arisgn
                 lda facexp                               ; set codes on facexp
                 bra faddt                                ; (y)=argexp

fadd5            jsr shiftr                               ; do a long shift
                 bcc fadd4                                ; continue with addition

fadd             jsr conupk
faddt            +lbeq movfa                              ; if fac=0, result is in arg
                 ldx facov
                 stx oldov
                 ldx #argexp                              ; default is shift argument
                 lda argexp                               ; if arg=0, fac is result

faddc            tay                                      ; also copy (a) into (y)
                 +lbeq zerrts                             ; return
                 sec
                 sbc facexp
                 beq fadd4                                ; no shifting
                 bcc fadda                                ; branch if argexp < facexp
                 sty facexp                               ; resulting exponent
                 ldy argsgn                               ; since arg is bigger, its
                 sty facsgn                               ; sign is sign of result
                 eor #$ff                                 ; shift a negative number of palces
                 adc #0                                   ; complete negation, w/ c=1
                 ldy #0                                   ; zero oldov
                 sty oldov
                 ldx #fac                                 ; shift the FAC instead
                 bra fadd1

fadda            ldy #0
                 sty facov

fadd1            cmp #$f9                                 ; for speed and necessity.  gets most likely case to
;SHIFTR fastest and allows shifting of neg nums by QUINT
                 bmi fadd5                                ; shift big
                 tay
                 lda facov                                ; set facov
                 lsr 1,x                                  ; gets 0 in the MSB
                 jsr rolshf                               ; do the rolling

fadd4            bbr7 arisgn,fadd2                        ; get resulting sign and if positive, add. carry is clear
                 ldy #facexp
                 cpx #argexp                              ; fac is bigger
                 beq l168_1
                 ldy #argexp                              ; arg is bigger

l168_1           sec                                      ; subit.
                 eor #$ff
                 adc oldov
                 sta facov
                 lda 4,y
                 sbc 4,x
                 sta faclo
                 lda 3,y
                 sbc 3,x
                 sta facmo
                 lda 2,y
                 sbc 2,x
                 sta facmoh
                 lda 1,y
                 sbc 1,x
                 sta facho

fadflt           bcs normal                               ; here if signs differ. if carry, FAC is set ok
                 jsr negfac                               ; negate (FAC)

normal           ldy #0
                 tya
                 clc

l169_1           ldx facho
                 bne norm1
                 ldx facho+1                              ; shift 8 bits at a time for speed
                 stx facho
                 ldx facmoh+1
                 stx facmoh
                 ldx facmo+1
                 stx facmo
                 ldx facov
                 stx faclo
                 sty facov
                 adc #8
                 cmp #32
                 bne l169_1

zerofc           lda #0                                   ; not needed by NORMAL, but by others
zerof1           sta facexp                               ; number must be zero
zeroml           sta facsgn                               ; make sign positive
zerrts           rts                                      ; all done


fadd2            adc oldov
                 sta facov
                 lda faclo
                 adc arglo
                 sta faclo
                 lda facmo
                 adc argmo
                 sta facmo
                 lda facmoh
                 adc argmoh
                 sta facmoh
                 lda facho
                 adc argho
                 sta facho
                 bra squeez                               ; go round if signs same


norm2            adc #1                                   ; decrement shift counter
                 asl facov                                ; shift all left one bit
                 rol faclo
                 rol facmo
                 rol facmoh
                 rol facho

norm1            bpl norm2                                ; if msb=0 shift again
                 sec
                 sbc facexp
                 bcs zerofc
                 eor #$ff
                 adc #1                                   ; complement
                 sta facexp

squeez           bcc rndrts                               ; bits to shift?
rndshf           inc facexp
                 +lbeq overr
                 ror facho
                 ror facmoh
                 ror facmo
                 ror faclo
                 ror facov
rndrts           rts                                      ; all done adding


negfac           lda facsgn
                 eor #$ff                                 ; complement FAC entirely
                 sta facsgn

negfch           lda facho
                 eor #$ff                                 ; complement just the number
                 sta facho
                 lda facmoh
                 eor #$ff
                 sta facmoh
                 lda facmo
                 eor #$ff
                 sta facmo
                 lda faclo
                 eor #$ff
                 sta faclo
                 lda facov
                 eor #$ff
                 sta facov
                 inc facov
                 bne incfrt

incfac           inc faclo
                 bne incfrt
                 inc facmo
                 bne incfrt                               ; if no carry, return
                 inc facmoh
                 bne incfrt
                 inc facho                                ; carry complement
incfrt           rts


; SHIFTR shifts (x+1:x+3) (-a) bits right.  Shifts bits to start with
; if possible.

mulshf           ldx #resho-1                             ; entry point for multiplier
shftr2           ldy 4,x                                  ; shift bits first
                 sty facov
                 ldy 3,x
                 sty 4,x
                 ldy 2,x                                  ; get mo
                 sty 3,x                                  ; store lo
                 ldy 1,x                                  ; get ho
                 sty 2,x                                  ; store mo
                 ldy bits
                 sty 1,x                                  ; store ho

shiftr           adc #8
                 bmi shftr2
                 beq shftr2
                 sbc #8                                   ; c can be either 1,0 and it works!
                 tay
                 lda facov
                 bcs shftrt                               ; equiv to beq here

shftr3           asl 1,x
                 bcc l170_1
                 inc 1,x
l170_1           ror 1,x
                 ror 1,x                                  ; yes, two of them

rolshf           ror 2,x
                 ror 3,x
                 ror 4,x                                  ; one more time
                 ror
                 iny
                 bne shftr3                               ; $$$ (most expensive!!!)

shftrt           clc                                      ; clear output of FACOV
                 rts

;.end
;[[math.const]]



; Constants used by LOG, EXP, TRIG, and others.

fr4              !text 127,0,0,0,0                        ; 1/4
neghlf           !text 128,128,0,0,0                      ; -0.5
fhalf            !text 128,0,0,0,0                        ; 0.5
tenc             !text 132,32,0,0,0                       ; 10.0
pival            !text 130,73,15,218,161                  ; pi
pi2              !text 129,73,15,218,162                  ; pi/2
twopi            !text 131,73,15,218,162                  ; pi*2

n0999            !text $9b,$3e,$bc,$1f,$fd
n9999            !text $9e,$6e,$6b,$27,$fd
nmil             !text $9e,$6e,$6b,$28,$00

foutbl                                                    ; powers of 10
                 !text 250,10,31,0                        ; -100,000,000
                 !text 0,152,150,128                      ; 10,000,000
                 !text 255,240,189,192                    ; -1,000,000
                 !text 0,1,134,160                        ; 100,000
                 !text 255,255,216,240                    ; -10,000
                 !text 0,0,3,232                          ; 1,000
                 !text 255,255,255,156                    ; -100
                 !text 0,0,0,10                           ; 10
                 !text 255,255,255,255                    ; -1
fdcend

; .byte @377,@337,@012,@200 ;-2,160,000 for time converter removed [901014]
; .byte @000,@003,@113,@300 ;   216,000
; .byte @377,@377,@163,@140 ;   -36,000
; .byte @000,@000,@016,@020 ;     3,600
; .byte @377,@377,@375,@250 ;      -600
; .byte @000,@000,@000,@074 ;        60
;timend

logcn2           !text 3                                  ; degree-1
                 !text 127,94,86,203,121                  ; 0.43425594188
                 !text 128,19,155,11,100                  ; 0.57658454134
                 !text 128,118,56,147,22                  ; 0.96180075921
                 !text 130,56,170,59,32                   ; 2.8853900728

expcon           !text 7                                  ; degree-1
                 !text 113,52,88,62,86                    ; 0.000021498763697
                 !text 116,22,126,179,27                  ; 0.00014352314036
                 !text 119,47,238,227,133                 ; 0.0013422634824
                 !text 122,29,132,28,42                   ; 0.0096140170199
                 !text 124,99,89,88,10                    ; 0.055505126860
                 !text 126,117,253,231,198                ; 0.24022638462
                 !text 128,49,114,24,16                   ; 0.69314718600
fone             !text 129,0,0,0,0                        ; 1.0

logeb2           !text 129,56,170,59,41                   ; log(e) base 2
sqr05            !text 128,53,4,243,52                    ; 0.707106781 sqr(0.5)
sqr20            !text 129,53,4,243,52                    ; 1.41421356 sqr(2.0)
log2             !text 128,49,114,23,248                  ; 0.693147181 ln(2)


sincon           !text 5                                  ; degree-1 trig
                 !text 132,230,26,45,27
                 !text 134,40,7,251,248
                 !text 135,153,104,137,1
                 !text 135,35,53,223,225
                 !text 134,165,93,231,40
                 !text 131,73,15,218,162

atncon           !text 11                                 ; degree-1
                 !text 118,179,131,189,211
                 !text 121,30,244,166,245
                 !text 123,131,252,176,16
                 !text 124,12,31,103,202
                 !text 124,222,83,203,193
                 !text 125,20,100,112,76
                 !text 125,183,234,81,122
                 !text 125,99,48,136,126
                 !text 126,146,68,153,58
                 !text 126,76,204,145,199
                 !text 127,170,170,170,19
                 !text 129,0,0,0,0

;[[math.log]]

; Natural Log Function
;
; Calculation is by   LN(f*2^n) = (n+LOG2(f))*LN(2)
; An approximation polynomial is used to calculate LOG2(f).


log              jsr sign                                 ; is it positive?
                 +lbeq fcerr                              ; can't tolerate neg or zero

                 lda facexp                               ; get exponent into (a)
                 sbc #$7f                                 ; remove bias (carry is off)
                 pha                                      ; save exponent a while
                 lda #$80
                 sta facexp                               ; result is FAC in range (0.5,1)
                 lda #<sqr05                              ; get pointer to sqr(0.5)
                 ldy #>sqr05
                 jsr romadd
                 lda #<sqr20
                 ldy #>sqr20
                 jsr romdiv
                 lda #<fone
                 ldy #>fone
                 jsr romsub
                 lda #<logcn2
                 ldy #>logcn2
                 jsr polyx                                ; evaluate approximation polynomial
                 lda #<neghlf                             ; add in last constant
                 ldy #>neghlf
                 jsr romadd
                 pla                                      ; get exponent back
                 jsr finlog
                 lda #<log2                               ; multiply result by ln(2)
                 ldy #>log2


rommlt           jsr romupk
                 bra fmultt                               ; multiply together


faddh            lda #<fhalf
                 ldy #>fhalf

romadd           jsr romupk
                 +lbra faddt


romsub           jsr romupk
                 +lbra fsubt


romdiv           jsr romupk
                 +lbra fdivt

;[[math.multiply]]

; Multiplication        FAC = ARG*FAC

fmultt_c65                                                ; [910402]
                 lda argsgn
                 eor facsgn
                 sta arisgn                               ; resultant sign
                 ldx facexp                               ; set signs on thing to multiply
                 bra fmultt                               ; go multiply

fmult            jsr conupk                               ; unpack the constant into arg for use

fmultt           beq multrt                               ; if FAC=0, return.  FAC is set
                 jsr muldiv                               ; fix up the exponents
                 lda #0                                   ; to clear result
                 sta resho
                 sta resmoh
                 sta resmo
                 sta reslo
                 lda facov
                 jsr mltpl1                               ; *** THIS fixes the DBL-0 bug without causing other grief!  C128-04 FAB
                 lda faclo                                ; multiply arg by faclo
                 jsr mltply
                 lda facmo                                ; multiply arg by facmo
                 jsr mltply
                 lda facmoh
                 jsr mltpl1                               ; *** THIS fixes the DBL-0 bug without causing other grief!  C128-04 FAB
                 lda facho                                ; multiply arg by facho
                 jsr mltpl1
                 +lbra movfr                              ; move result into FAC


mltply           +lbeq mulshf                             ; normalize result and return. shift result right 1 byte.  exits with .c=0
mltpl1           lsr
                 ora #$80                                 ; will flag end of shifting

l171_1           tay
                 bcc l171_2                               ; if mult bit=0, just shift
                 clc
                 lda reslo
                 adc arglo
                 sta reslo
                 lda resmo
                 adc argmo
                 sta resmo
                 lda resmoh
                 adc argmoh
                 sta resmoh
                 lda resho
                 adc argho
                 sta resho

l171_2           ror resho
                 ror resmoh
                 ror resmo
                 ror reslo
                 ror facov                                ; save for rounding
                 tya
                 lsr                                      ; clear msb so we get a closer to 0
                 bne l171_1                               ; slow as a turtle

multrt           rts


;[[math.unpack]]
; Unpack a ROM constant into the FAC

romupk           sta index1
                 sty index1+1
                 ldy #4
                 lda (index1),y                           ; it's in ROM, so ok to use ind
                 sta arglo
                 dey
                 lda (index1),y
                 sta argmo
                 dey
                 lda (index1),y
                 sta argmoh
                 dey
                 lda (index1),y
                 sta argsgn
                 eor facsgn
                 sta arisgn
                 lda argsgn
                 ora #$80
                 sta argho
                 dey
                 lda (index1),y
                 sta argexp
                 lda facexp                               ; sets code of facexp
                 rts


; Unpack a RAM constant into the FAC

conupk           sta index1
                 sty index1+1

; lda mmu_config_reg
; pha   ;preserve caller's memory config????

                 ldy #4
                 jsr indin1_ram1
                 sta arglo
                 dey
                 jsr indin1_ram1
                 sta argmo
                 dey
                 jsr indin1_ram1
                 sta argmoh
                 dey
                 jsr indin1_ram1
                 sta argsgn
                 eor facsgn
                 sta arisgn
                 lda argsgn
                 ora #$80
                 sta argho
                 dey
                 jsr indin1_ram1
                 sta argexp

; pla
; sta mmu_config_reg ;restore caller's memory config????

                 lda facexp                               ; set codes of facexp
                 rts


; Check special cases and add exponents for FMULT, FDIV

muldiv
                 lda argexp                               ; exp of arg=0?
mldexp           beq zeremv                               ; so we get zero exponent
                 clc
                 adc facexp                               ; result is in (a)
                 bcc l172_1                               ; find (c) xor (n)
                 +lbmi overr                              ; overflow if bits match
                 clc
                 !text $2c

l172_1           bpl zeremv                               ; underflow
                 adc #$80                                 ; add bias
                 sta facexp
                 +lbeq zeroml                             ; zero the rest of it
                 lda arisgn
                 sta facsgn                               ; arisgn is result's sign
                 rts                                      ; done


mldvex           lda facsgn                               ; get sign
                 eor #$ff                                 ; complement it
                 +lbmi overr

zeremv           pla                                      ; get addr off stack
                 pla
                 +lbra zerofc                             ; underflow


; Multiply FAC by 10

mul10            jsr movaf                                ; copy FAC into ARG
                 tax
                 beq mul10r                               ; if (FAC)=0, got answer
                 clc
                 adc #2                                   ; augment exp by 2
                 +lbcs overr                              ; overflow

finml6           ldx #0
                 stx arisgn                               ; signs are same
                 jsr faddc                                ; add together
                 inc facexp                               ; multiply by two
                 +lbeq overr                              ; overflow

mul10r           rts


div10            jsr movaf                                ; move FAC to ARG
                 lda #<tenc
                 ldy #>tenc                               ; point to constant of 10.0
                 ldx #0                                   ; signs are both positive

fdivf            stx arisgn
                 jsr movfm                                ; put it into FAC
                 bra fdivt

fdivt_c65                                                 ; [910402]
                 lda argsgn
                 eor facsgn
                 sta arisgn                               ; resultant sign
                 ldx facexp                               ; set signs on thing to divide
                 bra fdivt                                ; go divide

fdiv             jsr conupk                               ; unpack constant
fdivt            +lbeq doverr                             ; can't divide by zero
                 jsr round                                ; take FACOV into account in FAC
                 lda #0                                   ; negate facexp
                 sec
                 sbc facexp
                 sta facexp
                 jsr muldiv                               ; fix up exponents
                 inc facexp                               ; scale it right
                 +lbeq overr                              ; overflow
                 ldx #$fc                                 ; set up procedure
                 lda #1


divide                                                    ; this is the best code in the whole pile
                 ldy argho                                ; see what relation holds
                 cpy facho
                 bne savquo                               ; (c)=0,1. n(c=0)=0.
                 ldy argmoh
                 cpy facmoh
                 bne savquo
                 ldy argmo
                 cpy facmo
                 bne savquo
                 ldy arglo
                 cpy faclo

savquo           php
                 rol                                      ; save result
                 bcc qshft                                ; if not done, continue
                 inx
                 sta reslo,x
                 beq ld100
                 bpl divnrm                               ; note this req 1 no ram then access
                 lda #1

qshft            plp                                      ; return condition codes
                 bcs divsub                               ; FAC <= ARG

shfarg           asl arglo                                ; shift ARG one place left
                 rol argmo
                 rol argmoh
                 rol argho
                 bcs savquo                               ; save a result of one for this position and divide
                 bmi divide                               ; if msb on, go decide whether to sub
                 bpl savquo


divsub           tay                                      ; notice c must be on here
                 lda arglo
                 sbc faclo
                 sta arglo
                 lda argmo
                 sbc facmo
                 sta argmo
                 lda argmoh
                 sbc facmoh
                 sta argmoh
                 lda argho
                 sbc facho
                 sta argho
                 tya
                 bra shfarg



ld100            lda #$40                                 ; only want two more bits
                 bra qshft                                ; always branches



divnrm           asl                                      ; get last two bits into MSB and B6
                 asl
                 asl
                 asl
                 asl
                 asl
                 sta facov
                 plp



movfr            lda resho                                ; move result to FAC
                 sta facho
                 lda resmoh
                 sta facmoh
                 lda resmo
                 sta facmo
                 lda reslo                                ; move lo and sign
                 sta faclo
                 +lbra normal                             ; all done



movfm            sta index1                               ; move memory into FAC from ROM (unpacked)
                 sty index1+1
                 ldy #4
                 lda (index1),y
                 sta faclo
                 dey
                 lda (index1),y
                 sta facmo
                 dey
                 lda (index1),y
                 sta facmoh
                 dey
                 lda (index1),y
                 sta facsgn
                 ora #$80
                 sta facho
                 dey
                 lda (index1),y
                 sta facexp
                 sty facov
                 rts


; Move number from FAC to memory

mov2f            ldx #tempf2                              ; move from FAC to temp FAC2
                 !text $2c

mov1f            ldx #tempf1                              ; move from FAC to temp FAC1

                 ldy #0
movmf            jsr round
                 stx index1
                 sty index1+1
                 ldy #4
                 lda faclo
                 sta (index),y                            ; BasePage
                 dey
                 lda facmo
                 sta (index),y                            ; BasePage
                 dey
                 lda facmoh
                 sta (index),y                            ; BasePage
                 dey
                 lda facsgn                               ; include sign in ho
                 ora #$7f
                 and facho
                 sta (index),y                            ; BasePage
                 dey
                 lda facexp
                 sta (index),y                            ; BasePage
                 sty facov                                ; zero it since rounded
                 rts                                      ; (y)=0


movmf_ram1
                 jsr round
                 stx index1
                 sty index1+1
                 phx
                 ldx #index
                 ldy #4
                 lda faclo
                 jsr sta_far_ram1                         ; sta (index),y
                 dey
                 lda facmo
                 jsr sta_far_ram1                         ; sta (index),y
                 dey
                 lda facmoh
                 jsr sta_far_ram1                         ; sta (index),y
                 dey
                 lda facsgn                               ; include sign in ho
                 ora #$7f
                 and facho
                 jsr sta_far_ram1                         ; sta (index),y
                 dey
                 lda facexp
                 jsr sta_far_ram1                         ; sta (index),y
                 sty facov                                ; zero it since rounded
                 plx
                 rts                                      ; (y)=0


movfa            lda argsgn

movfa1           sta facsgn

                 ldx #5
l173_1           lda argexp-1,x
                 sta facexp-1,x
                 dex
                 bne l173_1
                 stx facov
                 rts


movaf            jsr round

movef            ldx #6
l174_1           lda facexp-1,x
                 sta argexp-1,x
                 dex
                 bne l174_1
                 stx facov                                ; zero it since rounded
movrts           rts



round            lda facexp                               ; zero?
                 beq movrts                               ; yes, done rounding
                 asl facov                                ; round?
                 bcc movrts                               ; no, msb off

incrnd           jsr incfac                               ; yes, add one to lsb(FAC) /// entry from EXP
;note .c=1 since incfac doesn't touch .c
                 +lbeq rndshf                             ; carry:   squeeze msb in and rts
                 rts                                      ; no carry: rts now



; Put sign in FAC into (a).

sign             lda facexp
                 beq signrt                               ; if number is zero, so is result

fcsign           lda facsgn
fcomps           rol
                 lda #$ff                                 ; assume negative
                 bcs signrt
                 lda #1                                   ; get +1
signrt           rts



; SGN function

sgn              jsr sign

;float the signed integer in accb
float            sta facho                                ; put (accb) in high order
                 lda #0
                 sta facho+1
                 ldx #$88                                 ; get the exponent
;float the signed number in FAC


floats           lda facho
                 eor #$ff
                 rol                                      ; get comp of sign in carry
floatc           lda #0                                   ; zero (a) but not carry
                 sta faclo
                 sta facmo

floatb           stx facexp
                 sta facov
                 sta facsgn
                 +lbra fadflt




; Absolute value of FAC

abs              lsr facsgn
                 rts



; Compare two numbers:
;
; a=1  if  ARG < FAC
; a=0  if  ARG = FAC
; a=-1 if  ARG > FAC

fcomp            sta index2
                 sty index2+1
                 ldy #0
                 lda (index2),y                           ; has argexp
                 iny                                      ; bump pointer up
                 tax                                      ; save a in x and reset codes
                 beq sign
                 lda (index2),y
                 eor facsgn                               ; signs the same
                 bmi fcsign                               ; signs differ so result is
                 cpx facexp                               ; sign of FAC again
                 bne l175_1

                 lda (index2),y
                 ora #$80
                 cmp facho
                 bne l175_1
                 iny
                 lda (index2),y
                 cmp facmoh
                 bne l175_1
                 iny
                 lda (index2),y
                 cmp facmo
                 bne l175_1
                 iny
                 lda #$7f
                 cmp facov
                 lda (index2),y
                 sbc faclo                                ; get zero if equal
                 beq qintrt                               ; rts

l175_1           lda facsgn
                 bcc l175_2
                 eor #$ff
l175_2           bra fcomps                               ; a part of sign sets up (a)

;.end
;[[math.ext4]]



; Quick Greatest Integer Function
;
; Leaves INT(FAC) in FACHO&MO&LO signed
; Assumes FAC < 2~23 =8388608

qint             lda facexp
                 beq clrfac                               ; if zero, got it
                 sec
                 sbc #$a0                                 ; get number of places to shift

                 bbr7 facsgn,l176_1

                 tax
                 lda #$ff
                 sta bits                                 ; put $ff in when shftr shifts bytes
                 jsr negfch                               ; truly negate quantity in FAC
                 txa

l176_1           ldx #fac
                 cmp #$f9
                 bpl qint1                                ; if number of places > 7 shift 1 place at a time
                 jsr shiftr                               ; start shifting bytes, then bits
                 sty bits                                 ; zero bits since adder wants zero
qintrt           rts


qint1            tay                                      ; put count in counter
                 lda facsgn
                 and #$80                                 ; get sign bit
                 lsr facho                                ; save first shifted byte
                 ora facho
                 sta facho
                 jsr rolshf                               ; shift the rest
                 sty bits                                 ; zero (bits)
                 rts



; Greatest Integer Function

int              lda facexp
                 cmp #$a0
                 bcs intrts                               ; forget it
                 jsr round                                ; round FAC per FACOV (fixes the  INT(.9+.1) -> 0  Microsoft bug.  FAB)
                 jsr qint                                 ; INT(FAC)
                 sty facov                                ; clr overflow byte
                 lda facsgn
                 sty facsgn                               ; make FAC look positive
                 eor #$80                                 ; get complement of sign in carry
                 rol
                 lda #$a0                                 ; @230+8
                 sta facexp
                 lda faclo
                 sta integr
                 +lbra fadflt


clrfac           sta facho                                ; make it really zero
                 sta facmoh
                 sta facmo
                 sta faclo
                 tay
intrts           rts

;[[math.fpin]]


; Floating Point Input Routine.
;
; Number input is left in FAC.  At entry (TXTPTR) points to the first character
; in a text buffer.  The first character is also in (a).  FIN packs the digits
; into the FAC as an integer and keeps track of where the decimal point is.
; (DPTFLG) tells whether a dp has been seen.  (DECCNT) is the number of digits
; after the dp.  At the end (DECCNT) and the exponent are used to determine how
; many times to multiply or divide by ten to get the correct number.


fin              stx fin_bank                             ; save bank number where string is stored

                 ldy #0                                   ; zero facsgn, sgnflg
                 ldx #$0a                                 ; zero exp and ho (and moh)
l177_1           sty deccnt,x                             ; zero mo and lo
                 dex                                      ; zero tenexp and expsgn
                 bpl l177_1                               ; zero deccnt, dptflg

                 bcc findgq                               ; flags still set from chrget
                 cmp #'-'                                 ; a negative sign?
                 bne qplus                                ; no, try plus sign
                 stx sgnflg                               ; it's negative. (x=@377)
                 bra finc                                 ; always branches


qplus            cmp #'+'                                 ; plus sign?
                 bne fin1                                 ; yes, skip it

finc             jsr fin_chrget

findgq           bcc findig

fin1             cmp #'.'                                 ; the dp?
                 beq findp                                ; no kidding
                 cmp #'E'                                 ; exponent follows
                 bne fine                                 ; no

                 jsr fin_chrget                           ; yes, get another, to check sign of exponent
                 bcc fnedg1                               ; is it a digit. (easier than backing up pointer)
                 cmp #minus_token                         ; minus?
                 beq finec1                               ; negate
                 cmp #'-'                                 ; minus sign?
                 beq finec1
                 cmp #plus_token                          ; plus?
                 beq finec
                 cmp #'+'                                 ; plus sign?
                 beq finec
                 bra finec2

finec1           ror expsgn                               ; turn it on

finec            jsr fin_chrget                           ; get another

fnedg1           bcc finedg                               ; it is a digit
finec2           bbr7 expsgn,fine
                 lda #0
                 sec
                 sbc tenexp
                 bra fine1

findp            ror dptflg
                 bbr6 dptflg,finc

fine             lda tenexp
fine1            sec
                 sbc deccnt                               ; get number of places to shift
                 sta tenexp
                 beq finqng                               ; negate?
                 bpl finmul                               ; positive, so multiply

findiv           jsr div10
                 inc tenexp                               ; done?
                 bne findiv                               ; no
                 bra finqng                               ; yes


finmul           jsr mul10
                 dec tenexp                               ; done?
                 bne finmul                               ; no
finqng           lda sgnflg
                 +lbmi negop                              ; if negative, negate and return
                 rts                                      ; if positive, return



findig           pha
                 bbr7 dptflg,l178_1
                 inc deccnt
l178_1           jsr mul10
                 pla                                      ; get it back
                 sec
                 sbc #'0'
                 jsr finlog                               ; add it in
                 bra finc



finlog           pha
                 jsr movaf                                ; save it for later
                 pla
                 jsr float                                ; float the value in (a)

faddt_c65                                                 ; [910402]
                 lda argsgn
                 eor facsgn
                 sta arisgn                               ; resultant sign
                 ldx facexp                               ; set signs on thing to add
                 +lbra faddt                              ; add together and return


; Pack in the next digit of the exponent.
; Multiply the old exp by 10 and add in the next digit.
; (note: does not check for exp overflow)

finedg           lda tenexp                               ; get exp so far
                 cmp #10                                  ; will result be >= 100?
                 bcc l179_1
                 lda #100
                 bbs7 expsgn,l179_4                       ; if neg exp, no chk for overr
                 +lbra overr

l179_1           asl                                      ; max is 120
                 asl                                      ; mult by 2 twice
                 clc                                      ; possible shift out of high
                 adc tenexp                               ; like multiplying by five
                 asl                                      ; and now by ten
                 clc
                 ldy #0
                 sta syntmp

                 lda fin_bank                             ; text or string bank?
                 bne l179_2
                 jsr indtxt                               ; text
                 bra l179_3
l179_2           jsr indin1_ram1                          ; string

l179_3           adc syntmp
                 sec
                 sbc #'0'
l179_4           sta tenexp                               ; save result
                 +lbra finec


; Get a character from either text or string area, and set the flags
; in the manner performed by CHRGET.

fin_chrget
                 lda fin_bank                             ; text or string bank?
                 +lbeq chrget                             ; get byte from text bank via normal CHRGET mechanism

fin_chrget_1                                              ; get byte from string bank via modified CHRGET mechanism
                 inw index1
fin_chrget_2
                 ldy #0
                 jsr indin1_ram1
                 cmp #':'
                 bcs l180_1
                 cmp #' '
                 beq fin_chrget_1                         ; skip over spaces
                 sec
                 sbc #'0'                                 ; set up .c as CHRGET would
                 sec
                 sbc #$d0
l180_1           rts

;.end
;[[math.ext5]]



inprt            jsr _primm
                 !text " IN ",0

curprt           lda curlin+1
                 ldx curlin

linprt           sta facho
                 stx facho+1
                 ldx #$90                                 ; exponent of 16
                 sec                                      ; number is positive
                 jsr floatc
                 jsr foutc
                 +lbra strout                             ; print and return


fout             ldy #1
foutc            lda #' '                                 ; if positive, print space
                 bbr7 facsgn,l181_1
                 lda #'-'                                 ; if neg
l181_1           sta fbuffr-1,y                           ; store the character
                 sta facsgn                               ; make FAC pos for QINT
                 sty fbufpt                               ; save for later
                 iny
                 lda #'0'                                 ; get zero to type if FAC=0
                 ldx facexp
                 +lbeq fout19

                 lda #0
                 cpx #$80                                 ; is number < 1?
                 beq l181_2                               ; no
                 bcs l181_3

l181_2           lda #<nmil                               ; mult by 10~6
                 ldy #>nmil
                 jsr rommlt
                 lda #$f7
l181_3           sta deccnt                               ; save count or zero it

l181_4           lda #<n9999
                 ldy #>n9999
                 jsr fcomp                                ; is number > 999999.499 or 999999999.5?
                 beq l181_9                               ; go to biggies
                 bpl l181_7                               ; yes, make it smaller

l181_5           lda #<n0999
                 ldy #>n0999
                 jsr fcomp                                ; is number > 99999.9499 or 99999999.90625?
                 beq l181_6
                 bpl l181_8                               ; yes. done multiplying

l181_6           jsr mul10                                ; make it bigger
                 dec deccnt
                 bne l181_5                               ; see if that does it (this always goes)

l181_7           jsr div10                                ; make it smaller
                 inc deccnt
                 bne l181_4                               ; see if that does it (this always goes)

l181_8           jsr faddh                                ; add a half to round up


l181_9           jsr qint                                 ; biggies.
                 ldx #1                                   ; decimal point count
                 lda deccnt
                 clc
                 adc #$0a                                 ; should number be printed in E notation?  (ie, is number .lt. .01?)
                 bmi l181_10                              ; yes
                 cmp #$0b                                 ; is it > 999999 or 9999999999?
                 bcs l181_11                              ; yes, use E notation
                 adc #$ff                                 ; number of places before decimal point
                 tax                                      ; put into accx
                 lda #2                                   ; no E notation
l181_10          sec

l181_11          sbc #2                                   ; effectively add 5 to orig exp
                 sta tenexp                               ; that is the exponent to print
                 stx deccnt                               ; number of decimal places
                 txa
                 beq l181_12
                 bpl l181_14                              ; some places before dec pnt

l181_12          ldy fbufpt                               ; get pointer to output
                 lda #'.'                                 ; put in "."
                 iny
                 sta fbuffr-1,y
                 txa
                 beq l181_13
                 lda #'0'                                 ; get the ensuing zero
                 iny
                 sta fbuffr-1,y

l181_13          sty fbufpt                               ; save it for later

l181_14          ldy #0

foutim           ldx #$80                                 ; first pass through, accb has msb set
fout2            lda faclo
                 clc
                 adc foutbl+3,y
                 sta faclo
                 lda facmo
                 adc foutbl+2,y
                 sta facmo
                 lda facmoh
                 adc foutbl+1,y
                 sta facmoh
                 lda facho
                 adc foutbl,y
                 sta facho
                 inx                                      ; it was done yet another time
                 bcs l182_1
                 bpl fout2
                 bmi l182_2

l182_1           bmi fout2
l182_2           txa
                 bcc l182_3                               ; can use (a) as is
                 eor #$ff                                 ; find 11.(a)
                 adc #10                                  ; c is still on to complete negation, and will always be on after

l182_3           adc #'0'-1                               ; get a character to print
                 iny
                 iny
                 iny
                 iny
                 sty fdecpt
                 ldy fbufpt
                 iny                                      ; point to place to store output
                 tax
                 and #$7f                                 ; get rid of msb
                 sta fbuffr-1,y
                 dec deccnt
                 bne l182_4                               ; not time for dp yet
                 lda #'.'
                 iny
                 sta fbuffr-1,y                           ; store dp

l182_4           sty fbufpt                               ; store pointer for later
                 ldy fdecpt
                 txa                                      ; complement accb
                 eor #$ff                                 ; complement acca
                 and #$80                                 ; save only msb
                 tax
                 cpy #fdcend-foutbl
; beq l182_5  ;for time converter ????   removed [901014]
; cpy #timend-foutbl
                 bne fout2                                ; continue with output

l182_5           ldy fbufpt                               ; get back output pointer
l182_6           lda fbuffr-1,y                           ; remove trailing blanks
                 dey
                 cmp #'0'
                 beq l182_6
                 cmp #'.'
                 beq l182_7                               ; ran into dp,  stop
                 iny                                      ; something else, save it

l182_7           lda #'+'
                 ldx tenexp
                 beq fout17                               ; no exponent to output
                 bpl l182_8
                 lda #0
                 sec
                 sbc tenexp
                 tax
                 lda #'-'                                 ; exponent is negative

l182_8           sta fbuffr+1,y                           ; store sign of exponent
                 lda #'E'
                 sta fbuffr,y                             ; store the 'E' character
                 txa

                 ldx #'0'-1
                 sec
l182_9           inx                                      ; move closer to output value
                 sbc #10                                  ; subtract 10
                 bcs l182_9                               ; not negative yet

                 adc #'9'+1                               ; get second output character
                 sta fbuffr+3,y                           ; store high digit
                 txa
                 sta fbuffr+2,y                           ; store low digit
                 lda #0                                   ; put in terminator
                 sta fbuffr+4,y
                 bra fout20                               ; return


fout19           sta fbuffr-1,y                           ; store the character
fout17           lda #0                                   ; store the terminator
                 sta fbuffr,y

fout20           lda #<fbuffr
                 ldy #>fbuffr
                 rts                                      ; all done


; Exponentiation and Square Root Functions.
;
; square root function - sqr(a)
; use sqr(x) = x^.5

sqr              jsr movaf                                ; move FAC into ARG
                 lda #<fhalf
                 ldy #>fhalf

fpwr             jsr movfm                                ; put memory into FAC    ARG^MEM


; Last thing fetched is facexp into accx.
;
; Exponentiation --- x^y.
; n.b. 0^0=1
; First check if y=0, and if so the result is one.
; Next  check if x=0, and if so the result is zero.
; Then  check if x>0:
; if not check that y is an integer.
; if so negate x, so that lg doesn't give fcerr.
; If x is negative and y is odd, negate the result returned by exp.
; To compute the result use x^y = EXP((y*LOG(x))


fpwrt            beq exp                                  ; if FAC=0, just exponentiate that  ARG^FAC
                 lda argexp                               ; is x=0?
                 +lbeq zerof1                             ; zero FAC

                 ldx #<tempf3                             ; save it for later in a temp
                 ldy #>tempf3
                 jsr movmf                                ; FAC->MEM

                 lda argsgn                               ; note y=0 already. that's good, in case no one calls int.
                 bpl l183_1                               ; no problems if x>0
                 jsr int                                  ; integerize the FAC
                 lda #<tempf3                             ; get addr of comperand
                 ldy #>tempf3
                 jsr fcomp                                ; equal?
                 bne l183_1                               ; leave x neg. log will blow him out
;a=-1 and y is irrelavant
                 tya                                      ; negative x. make positive
                 ldy integr                               ; get evenness

l183_1           jsr movfa1                               ; alternate entry point.    ARG->FAC
                 phy                                      ; save evenness for later
                 jsr log                                  ; find log
                 lda #<tempf3                             ; multiply FAC times LOG(x)
                 ldy #>tempf3
                 jsr fmult
                 jsr exp                                  ; exponentiate the FAC
                 pla
                 lsr                                      ; is it even?
                 bcc negrts                               ; yes. or x>0
;negate the number in FAC


negop                                                     ; /// entry point
                 lda facexp
                 beq negrts
                 lda facsgn
                 eor #$ff
                 sta facsgn
negrts           rts

;.end
;[[math.ext6]]



; Exponentation Function
;
; First save the original argument and multiply the FAC by LOG2(e).  The
; result is used to determine if overflow will occur since
;
;  EXP(x) = 2^(x*LOG2(e))
;
; where
;  LOG2(e) = LOG(e), base 2
;
; Then save the integer part of this to scale the answer at the end, since
; 2^y=2^INT(y)*2^(y-INT(y)) and 2^INT(y) are easy to compute.  Now compute
;
;  2^(x*LOG2(e)-INT(x*LOG2(e))
; by
;  p(LOG(2)*(INT(x*LOG2(e))+1)-x
;
; where p is an approximation polynomial. The result is then scaled by the
; power of two previously saved.  Re: Taylor expansion.


exp              lda #<logeb2                             ; multiply by LOG(e) base 2
                 ldy #>logeb2
                 jsr rommlt                               ; LOGEB2->ARG, FAC=FAC*ARG
                 lda facov
                 adc #$50                                 ; ????
                 bcc l184_1
                 jsr incrnd

l184_1           sta oldov
                 jsr movef                                ; to save in ARG without round.  ARG=FAC, facov=0)
                 lda facexp
                 cmp #$88                                 ; if ABS(FAC) >= 128, too big
                 bcc l184_3

l184_2           jsr mldvex                               ; overflow or overflow
l184_3           jsr int                                  ; FAC=INT(FAC), uses facov
                 lda integr                               ; get low part
                 clc
                 adc #$81
                 beq l184_2                               ; overflow or overflow!!

                 sec
                 sbc #1                                   ; subtract it
                 pha                                      ; save a while

                 ldx #5                                   ; swap FAC and ARG
l184_4           lda argexp,x
                 ldy facexp,x
                 sta facexp,x
                 sty argexp,x
                 dex
                 bpl l184_4

                 lda oldov
                 sta facov
                 jsr fsubt                                ; FAC=ARG-FAC
                 jsr negop                                ; negate FAC
                 lda #<expcon
                 ldy #>expcon
                 jsr poly
                 lda #0
                 sta arisgn                               ; multiply by positive 1.0

                 pla                                      ; recall scale factor
                 jsr mldexp                               ; modify facexp and check for overflow
                 rts                                      ; (has to do jsr due to pla's in muldiv)

;[[math.polyeval]]


; Polynomial Evaluator and the Random Number Generator.
;
; Evaluate  p(x^2)*x
; The pointer to degree is in (a,y) and the constants follow the degree.
; For x=FAC, compute  c0*x + c1*x^3 + c2*x^5 + c3*x^7 +...+ c(n)*x^(2*n+1)


polyx            sta polypt                               ; retain polynomial pointer for later
                 sty polypt+1
                 jsr mov1f                                ; save FAC in factmp (y=0 upon return)
                 lda #tempf1
                 jsr fmult                                ; compute x^2.
                 jsr poly1                                ; compute p(x^2).
                 lda #<tempf1
                 ldy #>tempf1
                 +lbra fmult                              ; multiply by FAC again


; Polynomial Evaluator
;
; Pointer to degree is in (a,y).
; Compute:  c0+c1*x+c2*x^2+c3*x^3+c4*x^4...+c(n-1)*x^(n-1)+c(n)*x^n
;  which is roughly (LOG(2)^n)/LOG(EXP(1))/n!


poly             sta polypt
                 sty polypt+1

poly1            jsr mov2f                                ; save FAC (rounds, .y=0)
                 lda (polypt),y
                 sta degree
                 inw polypt
                 lda polypt
                 ldy polypt+1

l185_1           jsr rommlt
                 lda polypt                               ; get current pointer
                 ldy polypt+1
                 clc
                 adc #5
                 bcc l185_2
                 iny
l185_2           sta polypt
                 sty polypt+1
                 jsr romadd                               ; add in constant
                 lda #<tempf2                             ; multiply the original FAC
                 ldy #>tempf2
                 dec degree                               ; done?
                 bne l185_1
                 rts                                      ; yes

;.end
;[[math.ext7]]



; Sine, Cosine, and Tangent Functions.



; Cosine function cos(x)=sin(x+pi/2)


cos              lda #<pi2                                ; pointer to pi/2
                 ldy #>pi2
                 jsr romadd                               ; add it in.  fall into sine



; Sine function
;
; Use identities to get FAC in quadrants I or IV.  The FAC is divided by 2*pi
; and the integer part is ignored because sin(x+2*pi)=sin(x).  Then the
; argument can be compared with pi/2 by comparing the result of the division
; with pi/2(2*pi)=1/4.  Identities are then used to get the result in quadrants
; I or IV.  An approximation polynomial is then used to compute sin(x).


sin              jsr movaf
                 lda #<twopi                              ; get pointer to divisor
                 ldy #>twopi
                 ldx argsgn                               ; get sign of result
                 jsr fdivf
                 jsr movaf                                ; get result into ARG
                 jsr int                                  ; integerize FAC
                 lda #0
                 sta arisgn                               ; always have the same sign
                 jsr fsubt                                ; keep only the fractional part
                 lda #<fr4                                ; get pointer to 1/4
                 ldy #>fr4
                 jsr romsub
                 lda facsgn                               ; save sign for later
                 pha
                 bpl sin1                                 ; first quadrant
                 jsr faddh                                ; add 1/2 to FAC
                 lda facsgn                               ; sign is negative?
                 bmi sin2
                 lda tansgn                               ; quads II and III come here
                 eor #$ff
                 sta tansgn

sin1             jsr negop                                ; if positive, negate it

sin2             lda #<fr4                                ; pointer to 1/4
                 ldy #>fr4
                 jsr romadd                               ; add it in
                 pla                                      ; get original quadrant
                 bpl l186_1
                 jsr negop                                ; if negative, negate result

l186_1           lda #<sincon
                 ldy #>sincon
                 +lbra polyx                              ; do approximation polyomial



; Tangent function


tan              jsr mov1f                                ; move FAC into temporary
                 lda #0
                 sta tansgn                               ; remember whether to negate
                 jsr sin                                  ; compute the sin
                 ldx #<tempf3
                 ldy #>tempf3
                 jsr movmf                                ; put sign into other temp
                 lda #<tempf1
                 ldy #>tempf1
                 jsr movfm                                ; put this memory location into FAC
                 lda #0
                 sta facsgn                               ; start off positive
                 lda tansgn
                 jsr l187_1                               ; compute cosine
                 lda #<tempf3
                 ldy #>tempf3                             ; address of sine value
; bra fdiv ;divide sine by cosine and return
                 jsr conupk                               ; unpack constant    [910226] FAB
                 +lbeq overr                              ; overflow error     "
                 +lbra fdivt                              ; "

l187_1           pha                                      ; cosc.
                 bra sin1


; Arctangent function
;
; Use identities to get arg between 0 and 1 and then use an approximation
; polynomial to compute arctan(x).


atn              lda facsgn                               ; what is sign?
                 pha                                      ; save for later
                 bpl l188_1
                 jsr negop                                ; if negative, negate FAC
;use arctan(x)=-arctan(-x)
l188_1           lda facexp
                 pha                                      ; save this too for later
                 cmp #$81                                 ; see if FAC >= 1.0
                 bcc l188_2                               ; it is less than 1
                 lda #<fone                               ; get pntr to 1.0
                 ldy #>fone
                 jsr romdiv                               ; compute reciprocal
;use aectan(x)=pi/2-arctan(1/x)
l188_2           lda #<atncon                             ; pointer to arctan constants
                 ldy #>atncon
                 jsr polyx
                 pla
                 cmp #$81                                 ; was original argument < 1?
                 bcc l188_3                               ; yes
                 lda #<pi2
                 ldy #>pi2
                 jsr romsub                               ; subtract arctan from pi/2

l188_3           pla                                      ; was original aurgument positive?
                 bpl l188_4                               ; yes
                 +lbra negop                              ; if negative, negate result

l188_4           rts                                      ; all done

;.end
;[[system.boot]]



;****************************************************************************
; BOOT  Boot has three modes of operation...
;
;   1. *B*LOAD a given binary file and SYS to its load address.
;   2. *D*LOAD a BASIC file named AUTOBOOT.C65* and RUN it.
;   3. BOOT SYS loads the home sector to $0400 and JMPs to it.
;
; For modes 1 & 2, syntax is the same as BLOAD.  Differentiate
; between the two modes via the presence of a filename.  All other
; parameters, such as drive and device numbers, are utilized in the
; normal manner.  For mode 3 there are no options.  It's intended
; to boot a new OS.  BASIC is turned off if it's successful.  If it
; fails, the run time stack & sprites might be corrupted.
;****************************************************************************

boot             cmp #sys_token                           ; BOOTSYS?      [910111]
                 bne l189_1                               ; no
                 jsr chrget                               ; yes- eat token
                 jsr _bootsys                             ; attempt to boot a new OS
                 bcc l189_4                               ; returned to us after successful install
                 ldx #errbdk                              ; bootsys failed, report 'bad disk'????
                 +lbra error

l189_1           bbr4 runmod,l189_2                       ; Error if in Edit mode     [910620]
                 +lbra edit_err

l189_2           lda #0                                   ; BOOT "filename"     [910417]
                 sta verck                                ; want 'load', not 'verify'
                 lda #$e6                                 ; set up parameters for DOS parser like BLOAD
                 ldx #$fc
                 jsr dosprx                               ; parse the command
                 bbr0 parsts,l189_5                       ; was there a filename?  branch if not
                 jsr bload_boot                           ; yes- bload it
                 +lbcs erexit                             ; load error

; ldx current_bank ;assume no B(ank) arg was given    [910114]
; bbr0 parstx,l189_3  ; correct, use current setup
                 ldx dosbnk                               ; else use given bank number
l189_3           stx _bank
                 lda _starting_addr                       ; set up address BLOAD loaded to
                 sta _pclo
                 lda _starting_addr+1
                 sta _pchi
                 jsr _jsr_far                             ; call it
l189_4           rts

l189_5           ldy #$ff
l189_6           iny                                      ; Copy default filename from ROM into buffer
                 lda autoboot_filename,y
                 sta savram,y
                 bne l189_6                               ; null terminated

                 sty dosf1l                               ; length not counting terminator
                 smb6 runmod                              ; set flag for load not to go to ready
                 jsr dload_boot                           ; Load it
                 +lbcs erexit                             ; error if problems
                 +lbra run_a_program                      ; else go run it


; AUTOBOOT_CSG Runs a system diagnostic if PB0 is low after initialization.
;  Diagnostic is copied to RAM-0 from ROM-2 and jumped to.

autobootCSG                                               ; Run ROMed diagnostic if PB0 low   [911105]
                 lda $dd01
                 lsr
                 bcs autoboot                             ; no, try to boot from disk

                 sei                                      ; prevent IRQ from wacking code DL'd to $1xxx  [911106]
                 ldx #12-1
l190_1           lda l190_2,x                             ; prep DMA list
                 sta dma1_cmd,x
                 dex
                 bpl l190_1

                 lda #0
                 ldx #>dma1_cmd                           ; copy program from ROM to RAM
                 ldy #<dma1_cmd
                 sta dma_ctlr+2                           ; dma_list bank
                 stx dma_ctlr+1                           ; dma_list hi
                 sty dma_ctlr                             ; dma_list lo & trigger

; jmp run_a_program ;if 'program' was BASIC
; lda #0   ;else set up arg's for call to 'long jmp'  [911105]
                 sta _bank
                 sta _pclo
                 lda #$10
                 sta _pchi
                 jmp _jmp_far                             ; jump to code, no return.  NOTE: this *MAPs* RAM-0 into context!

; move from $024001 to $002001, $3FFF bytes  BASIC program
;l190_2 .byte $00,$ff,$3f,$01,$40,$02,$01,$20,$00,$00,$00,$00

; move from $024000 to $1000, $4000 bytes   Diagnostic  [911105]
l190_2           !text $00,$00,$40,$00,$40,$02,$00,$10,$00,0,0,0


; AUTOBOOT Attempts to RUN a disk program after cold startup.  The
;  program must be a BASIC program called "AUTOBOOT.C65*"

autoboot
                 lda #0                                   ; Select internal drive
                 sta fdc
l191_1           bit fdc+2                                ; busywait
                 bmi l191_1
                 lda fdc+3                                ; See if a diskette is present
                 and #$08
                 beq l191_3                               ; exit with no action taken if not

                 lda #$e6                                 ; set up parameters for DOS parser like BLOAD
                 ldx #$fc
                 jsr dosprx                               ; let the parser init DOS stuff

                 ldy #$ff
l191_2           iny                                      ; Copy filename from ROM into buffer
                 lda autoboot_filename,y
                 sta savram,y
                 bne l191_2                               ; null terminated
                 sty dosf1l                               ; length not counting terminator

                 lda #%01000001                           ; set flag for load indicating autoboot
                 sta runmod                               ; set flag for load not to go to ready
                 jsr dload_boot                           ; skip parser & load it

                 lda #0                                   ; clear autoboot flags
                 sta runmod
                 phx                                      ; save end address
                 phy
                 jsr _readst                              ; get status report, but check it later
                 pha
                 jsr Suck_DS                              ; clear any DOS errors (to kill error LED)
                 pla                                      ; now check I/O status
                 ply
                 plx
                 and #$bf                                 ; EOI is okay
                 bne l191_3                               ; outside problems
                 bcs l191_3                               ; inside problems

                 stx text_top                             ; success- set end address & run it
                 sty text_top+1
                 cli
                 +lbra run_a_program

l191_3           rts                                      ; failure- go_ready


autoboot_filename
                 !text "AUTOBOOT.C65*",0




erexit           tax                                      ; set termination flags
                 +lbne error                              ; normal error
                 +lbra break_exit                         ; user break



outch            jsr _bsout
                 bcs erexit
                 rts



inchr            jsr _basin
                 bcs erexit
                 rts


coout
; jsr put_io_in_map
                 jsr _chkout
                 jsr dschk                                ; see if device # >=4, and clear DS if so
                 bcs erexit                               ; take error exit of there was one
                 rts


coin
; jsr put_io_in_map
                 jsr _chkin
                 jsr dschk                                ; see if device # >=4, and clear DS if so
                 bcs erexit
                 rts

cgetl
; jsr put_io_in_map
                 jsr _getin
                 +lbcs break_exit                         ; 'stop' key was pressed
                 rts


save             jsr plsv                                 ; parse parameters, dschk


savenp                                                    ; Save Program (from DSave)
                 ldx text_top                             ; ending address
                 ldy text_top+1
                 lda #<txttab                             ; pointer to start address


savenb                                                    ; Save Binary (from BSave & KEY SAVE)
; jsr put_io_in_map
                 jsr _savesp                              ; save it

; Any changes to the following code must be duplicated at:
;  bload
;  load (load_file)

exit_disk_op
exit_disk_operation
                 php                                      ; preserve kernel error status (.c)
                 pha                                      ; preserve kernel error # (.a)
                 jsr print_dos_error                      ; print DOS error msg if any only in direct mode
                 pla
                 plp
                 bcc l192_3                               ; branch if no error (rts)
                 bbs7 runmod,l192_2                       ; branch if run mode (erexit)
                 cmp #errfnf                              ; is it 'file not found' catch-all?
                 bne l192_1                               ; no  (erexit)
                 sta errnum                               ; yes- save error # for 'er'
                 ora #$80                                 ; but no errdis
l192_1           sec
l192_2           bcs erexit                               ; exit if kernel problem (rts)
l192_3           rts


verify           lda #1                                   ; verify flag
                 !text $2c                                ; skip two bytes

load             lda #0                                   ; load flag
                 sta verck
l193_1           bbr4 runmod,l193_2                       ; Error if in Edit mode     [910620]
                 +lbra edit_err
l193_2           jsr plsv                                 ; parse parameters, dschk

cld10                                                     ; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< entry from dload
; jsr put_io_in_map
                 lda verck
                 ldx txttab                               ; .x and .y have alt...
                 ldy txttab+1                             ; ...load address

; Any changes to the following code must be duplicated at:
;  bload
;  save (exit_disk_op)

load_file
                 jsr _loadsp                              ; load it
                 bbs0 runmod,cld20                        ; skip error checks if autoboot (rts)
                 phx                                      ; save end address
                 phy
                 php                                      ; save kernel load status (.c)
                 pha                                      ; save kernel error # (.a)
                 jsr _readst                              ; save I/O status byte
                 sta parsts
                 jsr print_dos_error                      ; report error msg if any only in direct mode
                 pla                                      ; restore error stuff
                 plp
                 bcc l194_3                               ; branch if no error (rts)
                 bbs7 runmod,l194_2                       ; branch if run mode (erexit)
                 cmp #errfnf                              ; is it 'file not found' catch-all?
                 bne l194_1                               ; no  (erexit)
                 sta errnum                               ; yes- save error # for 'er'
                 ora #$80                                 ; but no errdis
l194_1           sec
l194_2           +lbcs erexit                             ; exit if kernel problem
l194_3           ply                                      ; restore end address
                 plx
                 lda verck
                 beq cld50                                ; was load

; Finish verify

verify_check
                 ldx #ervfy                               ; assume error
; jsr _readst  ;read status
                 bbs4 parsts,cld55                        ; branch if 'verify' error
                 bbs7 runmod,cld20                        ; branch if not direct mode
verify_ok
                 jsr _primm
                 !text cr,"OK", cr,0
cld20            rts



; Finish load

cld50
; jsr _readst  ;check I/O status
                 lda parsts
                 and #%10111111                           ; EOI is okay, so mask it
                 beq cld60                                ; good- finish load operation

load_error
                 ldx #erload
cld55            +lbra error


cld60            stx text_top
                 sty text_top+1                           ; end load address

                 bbs7 runmod,cld70                        ; branch if not direct mode
                 bbs6 runmod,cld20                        ; special "RUN file_name" flag...get out here (rts)

                 jsr link_program                         ; relink
                 jsr runc                                 ; clear vars
                 +lbra ready_2                            ; print 'ready' & return to main


; Program load

cld70            jsr reset_txtptr
                 jsr link_program
                 +lbra fload


open             jsr paoc                                 ; parse statement
                 jsr _open                                ; open it
                 bra close_out_1



close            jsr paoc                                 ; parse statement
; jsr put_io_in_map
                 lda andmsk                               ; get la


close_out                                                 ; enter with .a=LA   [900725]
                 clc                                      ; flag a real close
                 jsr _close                               ; close it

close_out_1
                 php
                 pha
                 lda _fa                                  ; special error checking if disk op
                 cmp #8
                 bcc l195_1
                 pla
                 plp
                 +lbra exit_disk_operation                ; disk

l195_1           pla                                      ; something else
                 plp
                 +lbcs erexit
                 rts


; Parse LOAD, SAVE, & VERIFY commands

plsv
                 lda #0                                   ; set default filename (none)
                 jsr _setnam
                 ldx _default_drive                       ; set default device # (dosffn)
                 ldy #0                                   ; command 0
                 jsr _setlfs
                 lda text_bank                            ; all loads to   bank 0 ???? set default memory banks
                 ldx var_bank                             ; all names from bank 1 ????   [910620]
                 jsr _setbank

                 jsr paoc20                               ; by-pass junk
                 jsr paoc15                               ; get/set file name
                 jsr paoc20                               ; by-pass junk
                 jsr plsv7                                ; get ',fa'
                 ldy #0                                   ; command 0
                 stx andmsk
                 jsr _setlfs
                 jsr paoc20                               ; by-pass junk
                 jsr plsv7                                ; get ',sa'
                 txa                                      ; new command
                 tay
                 ldx andmsk                               ; device #
                 jsr _setlfs
                 bra dschk                                ; make dosfa current   [900801]



; Look for comma followed by byte

plsv7            jsr paoc30
                 +lbra getbyt



; Skip return if next char is end

paoc20           jsr chrgot
                 bne paocx
                 pla
                 pla
paocx            rts



; Check for comma and good stuff

paoc30           jsr chkcom                               ; check comma

paoc32           jsr chrgot                               ; get current character
                 bne paocx                                ; is okay
                 +lbra snerr                              ; bad...end of line


; Parse OPEN/CLOSE

paoc             lda #sys_bank                            ; ????      [910620]
                 ldx var_bank                             ;
                 jsr _setbank                             ; filename bank     (string bank)????
                 jsr _setnam                              ; default file name (null)
                 jsr paoc32                               ; must get something
                 jsr getbyt                               ; get la
                 stx andmsk
                 txa
                 ldx _default_drive                       ; default device
                 ldy #0                                   ; default command
                 jsr _setlfs                              ; store it
                 jsr paoc20                               ; skip junk
                 jsr plsv7
                 stx eormsk
                 ldy #0                                   ; default sa (command)
                 lda andmsk                               ; get la
                 cpx #3
                 bcc l196_1
                 dey                                      ; if sa not given and fa=serial bus, default to $ff
l196_1           jsr _setlfs                              ; store them
                 jsr paoc20                               ; skip junk
                 jsr plsv7                                ; get sa
                 txa
                 tay
                 ldx eormsk
                 lda andmsk
                 jsr _setlfs                              ; set up real everything
                 jsr paoc20
                 jsr paoc30

paoc15           jsr frmstr                               ; do frmevl, frestr. return with len in a, index =~string
                 jsr getspa                               ; ????fixes old PET bug- load"string",val(chr$(56)) [910917]
; ldx index1
; ldy index1+1
                 jsr _setnam                              ; bank always set at plsv
;fall into dschk     [900801]



dschk            php                                      ; check if current device >=8, and clear DS if so
                 pha
                 lda _fa
                 cmp #1
                 bne l197_1
                 lda _default_drive
                 sta _fa
l197_1           cmp #8                                   ; ????     [900807]
                 bcc l197_2
                 sta dosfa                                ; also make last DOS device = current device
                 jsr Clear_DS
l197_2           pla
                 plp
                 rts


;k_readst
; jsr put_io_in_map
; jmp _readst



;k_setlfs
; jsr put_io_in_map
; jmp _setlfs



;k_setnam
; jsr put_io_in_map
; jmp _setnam



;k_basin
; jsr put_io_in_map
; jmp _basin


;k_bsout
; jsr put_io_in_map
; jmp _bsout


;k_clrch
; jsr put_io_in_map
; jmp _clrch



;k_close
; jsr put_io_in_map
; jmp _close



;k_clall
; jsr put_io_in_map
; jmp _clall



;k_primm
; jsr put_io_in_map
; jmp _primm


;k_setbank
; jsr put_io_in_map
; jmp _setbank
; rts


;k_plot
; sta sw_rom_ram0  ;????
; jmp _plot


;k_stop
; jsr put_io_in_map
; jmp _stop

;.end
;[[command.print.using]]


; Print Using - Formatted print routine
;
; Entered by cmd, print, or print#
; Syntax:  PRINT USING"****";a;b;c

pudefs           !text " ,.$"                             ; default:  fill, comma, dec pnt, money symbol

using            ldx #$ff
                 stx endfd
                 jsr chrget
                 jsr frmevl                               ; evaluate format string
                 jsr chkstr                               ; must be string type...

                 lda facmo                                ; save temp descriptor
                 pha
                 lda facmo+1
                 pha

                 ldy #2                                   ; move (facmo),1&2 to form,form+1
l198_1           jsr indfmo
                 dey
                 sta form,y
                 bne l198_1

                 jsr indfmo                               ; get length
                 sta lfor
                 tay
                 beq l198_3                               ; syntax error if length is zero

l198_2           dey
                 jsr indfrm
                 cmp #'#'                                 ; at least one # in format?
                 beq l198_4                               ; yes...
                 tya                                      ; no...end of format
                 bne l198_2                               ; no...
l198_3           +lbra snerr                              ; yes...syntax error


l198_4           lda #';'                                 ; '
eex2             jsr synchr                               ; check character
                 sty z_p_temp_1                           ; clear flag for anaf
                 sty bnr                                  ; set pointer to begin of no
                 jsr frmevl                               ; evaluate expression
                 bbr7 valtyp,conv                         ; branch if numeric

                 jsr ini                                  ; init counters and flags
                 jsr anaf                                 ; analyze format
                 ldx chsn                                 ; > or = in format field
                 beq prcha                                ; branch if not
                 ldx #0
                 sec
                 lda cform
                 sbc hulp                                 ; .a=room left in field
                 bcc prcha                                ; branch if no room left
                 ldx #'='
                 cpx chsn                                 ; = in field
                 bne l199_1                               ; branch if not
                 lsr                                      ; .a=.a/2
                 adc #0                                   ; add 1 if odd

l199_1           tax                                      ; store no of blanks in x
prcha            ldy #0
chx              txa
                 beq cpef                                 ; branch if no blanks
                 dex

oblk             lda #' '                                 ; output a blank
                 bra outc                                 ; always


cpef             cpy hulp                                 ; end of string reached?
                 bcs oblk                                 ; output blank if yes
                 jsr indin1_ram1                          ; lda (index),y
                 iny

outc             jsr cdout                                ; output character
                 bne chx                                  ; branch if not ready
                 bra reay



conv             jsr fout                                 ; convert mfp to decimal

                 ldy #$ff                                 ; build descriptor for fout string
l200_1           iny                                      ; how big IS it?
                 lda fbuffr,y
                 bne l200_1
                 tya
                 jsr strspa                               ; jsr getspa,stx dsctmp+1,sty dsctmp+2,sta dsctmp,rts

                 phx
                 ldy #0
                 ldx #dsctmp+1
l200_2           lda fbuffr,y
                 beq l200_3
                 jsr sta_far_ram1                         ; sta (dsctmp+1),y
                 iny
                 bne l200_2

l200_3           plx
                 jsr putnew
                 jsr ini                                  ; init counters and flags
                 jsr fform                                ; output one formatted number

reay             jsr chrgot                               ; get old character
                 cmp #','                                 ; comma?
                 beq eex2                                 ; continue print use if yes
                 sec
                 ror z_p_temp_1                           ; set flag for anaf
                 jsr anaf                                 ; print rest of format
                 ply                                      ; restore descriptor
                 pla
                 jsr fretmp
                 jsr chrgot
                 cmp #';'                                 ; semi-colon?
                 +lbne crdo                               ; end of print using
                 jmp chrget                               ; branch if yes

;[[command.fform]]


;  FFORM - output a number to format
;
;  Number is in fbuffr,  format checked by anaf

fform
; sta sw_rom_ram0 ;????
                 lda pufill
                 sta blfd                                 ; set working fill char
                 lda #$ff
ana              sta sno                                  ; save blank or '-' in sno
                 bra insy


stp              stx point                                ; set point pointer
insy             cpy hulp                                 ; end of no reached?
                 beq eoa                                  ; branch if yes
                 lda fbuffr,y                             ; get character of no
                 iny
                 cmp #' '                                 ; blank?
                 beq insy                                 ; yes...span
                 cmp #'-'                                 ; sign no negative
                 beq ana                                  ; yes...
                 cmp #'.'                                 ; decimal point?
                 beq stp                                  ; yes...
                 cmp #'E'                                 ; is char 'e'?
                 beq lsg                                  ; yes...
                 sta fbuffr,x                             ; move number
                 stx enr                                  ; update end-no pointer
                 inx
                 bit point                                ; point pointer set?
                 bpl insy                                 ; yes...
                 inc vn                                   ; count digits before point
                 bra insy


lsg              lda fbuffr,y
                 cmp #'-'                                 ; sign of exponent negative
                 bne l201_1                               ; no...
                 ror usgn                                 ; make sign negative
l201_1           iny
                 sty uexp                                 ; set exponent pointer

eoa              lda point                                ; decimal found?
                 bpl l202_1                               ; yes...
                 stx point                                ; no...add point

l202_1           jsr anaf                                 ; analyze format
                 lda vf
                 cmp #$ff
                 beq l202_3                               ; field overflow
                 lda fesp                                 ; exponent in field
                 beq cff                                  ; convert to f format if not
                 lda uexp                                 ; exponent in number?
                 bne l202_2                               ; yes...
                 ldx enr
                 jsr et2                                  ; add exponent to number
                 dec fbuffr+2,x
                 inx
                 stx uexp
                 jsr alg                                  ; delete leading zeros
                 beq hup                                  ; all zero

l202_2           ldy posp                                 ; + or - in format?
                 bne sswe                                 ; yes...
                 ldy sno                                  ; +?
                 bmi sswe                                 ; yes...
                 lda vf

l202_3           beq errf                                 ; no room for sign
                 dec vf                                   ; reserve room
                 bne l202_4
                 lda nf                                   ; one #?
                 beq errf                                 ; yes...error

l202_4           inc swe

sswe             jsr shpn                                 ; shift decimal point
                 jsr uround                               ; round number
                 jsr shpn                                 ; shift again if necessary

hup              +lbra chout                              ; output number



cff              ldy uexp                                 ; exponent in no?
                 beq l203_2                               ; no...
                 sta hulp                                 ; delete exponent
                 sec                                      ; adjust decimal point
                 ror etof                                 ; set e-to-f flag
                 ldy point
                 lda usgn                                 ; exec nos3 or nos4
                 bpl l203_1                               ; depends on sign of exp
                 jsr nos3
                 bra l203_3

l203_1           jsr nos4

l203_2           ldy point                                ; at start of no?
                 beq l203_3                               ; yes...
                 jsr cho                                  ; no = 0 ?
                 beq l203_4                               ; yes...no round

l203_3           jsr uround
                 bra l203_5

l203_4           dec vn                                   ; adjust...no was 0
l203_5           sec
                 lda vf
                 sbc vn
                 bcc errf                                 ; no fit...error
                 sta swe                                  ; save difference
                 ldy posp                                 ; + or -?
                 bne ahp                                  ; yes...
                 ldy sno                                  ; get sign
                 bmi ahp                                  ; positive...
                 tay
                 beq errf                                 ; no room for sign
                 dey
                 bne ldvn                                 ; swe<>1
                 lda nf
                 ora vn                                   ; both zero?
                 bne hup                                  ; no...


errf             lda #'*'                                 ; format error
stout            jsr cdout                                ; fill field with stars
                 bne stout
                 rts


ahp              tay                                      ; output no if swe=0
                 beq hup
ldvn             lda vn
                 bne hup                                  ; vn<>0
                 dec swe                                  ; add extra 0
                 inc z_p_temp_1                           ; before decimal point
                 bra hup


; Using- shift decimal point

shpn             sec
                 lda vf
                 sbc vn
                 beq rdy                                  ; format o.k
                 ldy point
                 bcc pntl                                 ; vf<vn
                 sta hulp

incy             cpy enr                                  ; end of no reached?
                 beq ity
                 bcs nos1                                 ; yes...

ity              iny
nos1             inc vn
nos4             jsr eado                                 ; adjust exponent
                 dec hulp                                 ; ready?
                 bne incy                                 ; no...
                 beq poit

pntl             eor #$ff
                 adc #1
                 sta hulp                                 ; =vn-vf

decy             cpy bnr                                  ; begin of no?
                 beq inz1                                 ; yes...
                 dey
                 dec vn
                 bra inz2


inz1             inc z_p_temp_1                           ; add leading zeros
inz2             lda #$80
nos3             jsr eadj                                 ; adjust exponent
                 dec hulp                                 ; ready?
                 bne decy                                 ; no...

poit             sty point                                ; decimal point pointer
rdy              rts


; Using- adjust exponent

sexp             bne retrn                                ; no over/underflow
                 eor #$09                                 ; .a is 0 or 9
                 sta fbuffr,x                             ; digit back in exp
                 dex                                      ; = 0 or 9
                 cpx uexp
                 rts


eado             lda #0
eadj             ldx uexp
                 inx
                 bit etof                                 ; e-to-f flag on?
                 bmi l204_2                               ; yes...
                 eor usgn
                 beq l204_2                               ; ++ or --

l204_1           jsr tag3                                 ; inc exp, overflow?
                 jsr sexp                                 ; digit 0 if yes
                 bcs l204_1                               ; try second digit
                 +lbra overr                              ; exp>99

l204_2           lda fbuffr,x
                 dec fbuffr,x                             ; decrement exp
                 cmp #'0'                                 ; underflow on digit?
                 jsr sexp                                 ; set digit=9 if yes...
                 bcs l204_2                               ; try 2nd digit
                 bit etof                                 ; flag off?
                 bpl et3                                  ; yes...
                 sty point                                ; decimal point pointer

retrn            pla
                 pla
                 rts                                      ; return to fform/main


et3              lda usgn
                 eor #$80                                 ; reverse sign exp
et2              sta usgn
                 lda #'0'
                 sta fbuffr+1,x                           ; exp<0 here
                 lda #'1'
                 sta fbuffr+2,x
                 rts


tag3             lda fbuffr,x                             ; get digit of exp
                 inc fbuffr,x                             ; increment digit
                 cmp #'9'                                 ; overflow
                 rts                                      ; return .z set


; Using- ansub: load format field char in .a

ansub            clc
                 iny                                      ; begin format?
                 beq l205_1                               ; yes...
                 cpy lfor                                 ; end?
                 bcc l205_2                               ; no...

l205_1           ldy z_p_temp_1                           ; <>0?
                 bne retrn                                ; yes...

l205_2           jsr indfrm
                 inc cform                                ; pointer to field
                 rts


;  Using- ini: init counters and flags

ini              jsr frefac                               ; free temp descriptor
                 sta hulp                                 ; length string
                 ldx #$0a                                 ; printed in hulp
                 lda #0

l206_1           sta swe,x                                ; init working registers
                 dex
                 bpl l206_1
                 stx flag                                 ; comma flag =ff
                 stx point                                ; point pointer=ff
                 stx dolr                                 ; dollar flag=ff
                 tax                                      ; x=y=0
                 tay
                 rts


; Using- round number

uround           clc
                 lda point
                 adc nf                                   ; overflow?
                 bcs rrts                                 ; yes...
                 sec
                 sbc z_p_temp_1                           ; underflow?
                 bcc rrts                                 ; yes...
                 cmp enr                                  ; anything to round?
                 beq l207_1                               ; yes...
                 bcs rrts                                 ; no...

l207_1           cmp bnr                                  ; again...
                 bcc rrts                                 ; no...
                 tax
                 lda fbuffr,x                             ; get digit
                 cmp #'5'                                 ; <5 ?
                 bcc rrts                                 ; yes...no round

l207_2           cpx bnr                                  ; begin of no reached?
                 beq l207_3                               ; yes..add 1
                 dex
                 jsr tag3                                 ; increment digit
                 stx enr                                  ; new end of no pointer
                 beq l207_2                               ; branch on overflow
                 rts

l207_3           lda #'1'
                 sta fbuffr,x
                 inx
                 stx point
                 dec z_p_temp_1                           ; # of 0's before '.'
                 bpl rrts                                 ; no underflow
                 inc z_p_temp_1                           ; underflow...back to 0
                 inc vn
rrts             rts


; Using- alg: delete leading zeros of no

alg              ldy point                                ; start with a .?
                 beq szer                                 ; yes...

cho              ldy bnr
cmo              lda fbuffr,y                             ; start with a 0?
                 cmp #'0'
                 rts


nbr              inc point                                ; adjust point
                 jsr eado                                 ; and exponent
                 inc bnr                                  ; and pointer to begin of no
                 cpy enr                                  ; end of number?
                 beq rrts                                 ; yes...
                 iny

szer             jsr cmo                                  ; zero in no?
                 beq nbr                                  ; yes...
                 rts                                      ; no...


; Using- chout: print number

chout            lda dolr                                 ; dollar flag set?
                 bmi l208_1                               ; no...
                 inc z_p_temp_1                           ; make room for $

l208_1           ldx bnr                                  ; start of #
                 dex
                 ldy begfd                                ; begin of field

afrm             jsr indfrm
                 iny
                 cmp #','                                 ; comma?
                 bne punt                                 ; no...
                 bit flag                                 ; comma flag on?
                 bmi bout                                 ; yes, do a fill char
; sta sw_rom_ram0 ;????
                 lda pucoma
                 bra out                                  ; no,...output a comma char


bout             lda blfd
                 bra out


punt             cmp #'.'                                 ; decimal point?
                 bne afplus
; sta sw_rom_ram0 ;????
                 lda pudot                                ; yes...
                 bra out


afplus           cmp #'+'                                 ; plus?
                 beq ispl                                 ; yes...
                 cmp #'-'                                 ; minus?
                 beq ispl1                                ; yes...
                 cmp #'^'                                 ; up arrow?
                 bne pndd                                 ; no...
                 lda #'E'
                 jsr cdout
                 ldy uexp
                 jsr cmo                                  ; first dig of exp zero?
                 bne l209_1                               ; no...
                 iny
                 jsr cmo                                  ; second digit?
                 beq l209_2                               ; yes

l209_1           lda #'-'
                 bit usgn
                 bmi l209_3

l209_2           lda #'+'
l209_3           jsr cdout                                ; output sign exp
                 ldx uexp
                 lda fbuffr,x
                 jsr cdout                                ; output first dig exp
                 ldy endfd
                 bra telx

ispl1            lda sno                                  ; positive?
                 bmi bout                                 ; yes...out blank or *
ispl             lda sno                                  ; output sgn
                 bra out


zerot            lda z_p_temp_1                           ; # of zeros
                 bne zerot1
                 cpx enr                                  ; end of # reached?
                 beq zout                                 ; yes...output zero

telx             inx
                 lda fbuffr,x                             ; get digit
                 !text $2c                                ; skip

zout             lda #'0'                                 ; output zero

outs             lsr flag                                 ; clear comma flag

out              jsr cdout                                ; output character
                 +lbeq rrts
                 bra afrm                                 ; not ready...


zerot1           dec z_p_temp_1                           ; count leading zeros
                 lda dolr                                 ; $ flag set?
                 bmi zout                                 ; no...output zero

                 jsr indfrm                               ; take a peek at the next character in the format string
                 cmp #','                                 ; if it's a comma, we got problems
                 bne l210_1                               ; ...branch if no comma & resume normal processing

                 lda blfd                                 ; here's the "$,999.99" bug fix:
                 jsr cdout                                ; print a 'fill' character instead of the '$'
                 iny                                      ; and increment format string pointer past comma

l210_1           sec                                      ; resume normal processing
                 ror dolr                                 ; clear the dollar flag & go on to print '$'
; sta sw_rom_ram0 ;????
                 lda pumony
                 bra outs                                 ; output money symbol


pndd             lda swe                                  ; # of blanks
                 beq zerot
                 dec swe                                  ; count !

l211_1           +lbne bout                               ; out blank or *
                 lda posp                                 ; + or - in field?
                 bmi l211_1                               ; yes...out blank or *

l211_2           jsr indfrm
                 cmp #','                                 ; comma?
                 bne ispl1                                ; no...out sign
                 lda blfd                                 ; yes...
                 jsr cdout                                ; out blank or *
                 iny
                 bra l211_2



cdout            jsr outch ;outdo                         ; char out
                 dec cform                                ; count it
                 rts


; Using- anaf: analyze format

anaf             ldy endfd                                ; format pointer
gfor             jsr ansub
                 jsr comp                                 ; check special chars
                 bne pchar                                ; no match...
                 sty begfd                                ; save beginning of field
                 bcc ffoun                                ; # was found
                 tax                                      ; save char

sfur             jsr ansub                                ; get next format char
                 bcs l212_1                               ; stop on wrap-around
                 jsr com1                                 ; compare specials
                 beq foun1                                ; found some...

l212_1           ldy begfd
                 txa
pchar            jsr outch ;outdo                         ; out character
                 bra gfor


foun1            bcs sfur                                 ; no #...keep looking
                 ldy begfd
ffoun            ldx z_p_temp_1
                 bne trts
                 stx cform
                 dey

hyo2             dec cform                                ; correct counter
hyo              jsr ansub                                ; get next format char
                 bcs efo                                  ; end of format
                 cmp #','                                 ; comma?
                 beq hyo                                  ; yes...ignore it
                 jsr isp                                  ; + or - in format?
                 bcc hyo2                                 ; yes...
                 cmp #'.'                                 ; decimal point?
                 bne avf1                                 ; no...
                 inx
                 cpx #2                                   ; more than 1 decimal?
                 bcc hyo                                  ; no...
ero              +lbra snerr                              ; yes...syntax error


avf1             jsr com2                                 ; =, >, or # in field
                 bne llar                                 ; no...
                 bcc hyo1                                 ; was #
                 sta chsn                                 ; was > or =

hyo1             inc vf,x
                 bra hyo

llar             cmp #'$'                                 ; dollar?
                 bne expo                                 ; no...
                 bit dolr                                 ; test flag
                 bpl hyo1                                 ; no...
                 clc
                 ror dolr                                 ; set flag
                 dec vf
                 bra hyo1

expo             cmp #'^'                                 ; up arrow?
                 bne isp                                  ; no...
                 ldx #$02
l213_1           jsr ansub                                ; must be 4 up arrows
                 bcs ero
                 cmp #'^'                                 ; up arrow?
                 bne ero
                 dex
                 bpl l213_1
                 inc fesp                                 ; set exp flag
                 jsr ansub                                ; next format char
                 bcs efo                                  ; end of format

isp              cmp #'+'                                 ; plus?
                 bne chom                                 ; no...
                 lda sno                                  ; sign neg?
                 bpl spos                                 ; yes...
                 lda #'+'
                 sta sno

spos             lda posp                                 ; + or - already?
                 bne ero
                 ror posp                                 ; make posp neg/clc
                 sty endfd                                ; end field pointer
                 inc cform                                ; correct counter
trts             rts


chom             cmp #'-'                                 ; minus?
                 beq spos                                 ; yes...
                 sec
efo              sty endfd                                ; end field pointer
                 dec endfd
                 rts


; Using- comp: compare .a with symbols

comp             cmp #'+'
                 beq rt
                 cmp #'-'
                 beq rt

com1             cmp #'.'
                 beq rt

com2             cmp #'='
                 beq rt
                 cmp #'>'
                 beq rt
                 cmp #'#'
                 bne rt
                 clc
rt               rts

;.end


;[[function.instr]]

;  INSTRing - Find position of str1 in str2 at or after position n
;
; Usage: INSTR(a$,b${,n})

instr            lda facmo                                ; save pointer to temporary descriptors
                 sta tmpdes
                 lda facmo+1
                 sta tmpdes+1

                 jsr frmevl                               ; get next arg
                 jsr chkstr                               ; must be string
                 lda facmo
                 sta tmpdes+2
                 lda facmo+1
                 sta tmpdes+3

                 ldx #1                                   ; default starting position
                 stx faclo
                 jsr chrgot
                 cmp #')'                                 ; any length argument?
                 beq l214_1                               ; branch if not
                 jsr combyt                               ; else go get a one byte argument

l214_1           jsr chkcls                               ; look for )
                 ldx faclo
                 +lbeq fcerr                              ; starting position can't be 0
                 dex
                 stx positn

                 ldx #3                                   ; copy 'pointers to temp descriptors' to zero page
l214_2           lda tmpdes,x
                 sta ptarg1,x
                 dex
                 bpl l214_2

                 ldy #2                                   ; now get the descriptors
l214_3           lda #ptarg1
                 jsr lda_far_ram1                         ; lda (ptarg1),y
                 sta str1,y
                 lda #ptarg2
                 jsr lda_far_ram1                         ; lda (ptarg2),y
                 sta str2,y
                 dey
                 bpl l214_3

                 lda str2                                 ; check if string 2 is null
                 beq l214_8                               ; if so, return 0

l214_4           lda #0
                 sta match
                 clc
                 lda str2                                 ; length of string 2
                 adc positn
                 bcs l214_8                               ; too long, not found
                 cmp str1                                 ; see if > length of string 1
                 bcc l214_5                               ; < len string 1
                 bne l214_8                               ; must be >, not found

l214_5           ldy match
                 cpy str2                                 ; if match len = str len, then found
                 beq l214_7
                 tya
                 clc
                 adc positn                               ; compare str1(s+p+m) with str2(m)
                 tay
                 lda #str1+1
                 jsr lda_far_ram1                         ; lda (str1+1),y
                 sta syntmp
                 ldy match
                 lda #str2+1
                 jsr lda_far_ram1                         ; lda (str2+1),y
                 cmp syntmp
                 beq l214_6
                 inc positn                               ; not the same, start over from next positn
                 bra l214_4                               ; always

l214_6           inc match                                ; count characters that match
                 bra l214_5                               ; always


l214_7           inc positn                               ; found
                 lda positn
                 !text $2c

l214_8           lda #0                                   ; not found
; sta sw_rom_ram0 ;????
                 pha
                 lda tmpdes+2                             ; free temp descriptors
                 ldy tmpdes+3
                 jsr fretmp
; sta sw_rom_ram0 ;????
                 lda tmpdes
                 ldy tmpdes+1
                 jsr fretmp
                 ply
                 +lbra sngflt                             ; float 1 byte in .y

;.end



;[[function.type]]

; TYPE  types a given disk (SEQ) file to output channel
;

type             ldz #0

open_SEQ_file
                 phz                                      ; save EDIT load flag    [910620]
                 lda #$e6                                 ; parse:  filename [,U#] [,D#]
                 jsr dosprs                               ; (like dopen:      0 0 0 *  * 0 0 1 )
                 jsr chk1                                 ; check parameters
                 jsr find_la                              ; find an available LA
                 jsr find_sa                              ; find an available SA
                 ldy #fopnseq
                 ldx #6
                 jsr open_file                            ; open the file
                 +lbcs list_err                           ; exit if error
                 plz                                      ; [910620]
                 beq l215_1
                 rts                                      ; or exit if called by EDIT load routine

l215_1           jsr _stop                                ; check stop key
                 beq l215_6                               ; exit if down
                 ldx dosla
                 jsr _chkin                               ; get input channel
                 bcs l215_6                               ; exit if bad??
                 ldx #0
l215_2           cpx #255                                 ; check buffer (buflen????)
; bcs 99$   ; 'too long' error
                 beq l215_3                               ; allow long lines   [910620]
                 jsr _basin                               ; read file data
                 sta dosstr,x                             ; buffer it
                 inx                                      ; bump buffer pointer
                 tay                                      ; save char
                 jsr _readst                              ; check channel status
                 bne l215_3                               ; exit if eof or error
                 cpy #cr
                 bne l215_2                               ; loop until eol

l215_3           php                                      ; save input channel status (beq=eol, bne=eof/err)
                 stx t4                                   ; save character count
                 jsr dcato                                ; get output channel
                 ldx #0
l215_4           cpx t4                                   ; check buffer
                 bcs l215_5                               ; end of buffered data
                 lda dosstr,x                             ; output data
                 jsr _bsout
                 inx                                      ; bump buffer pointer
                 bne l215_4                               ; loop until end of buffer

l215_5           jsr _clrch
                 plp                                      ; check input status
                 beq l215_1                               ; loop until eof or bad status

l215_6           +lbra list_exit                          ; release channel, close file, return to main

;99$ jsr _clrch  ;non-I/O trouble   removed [910620]
; lda dosla  ; shut down disk & report BASIC error
; clc
; jsr _close
; bra errlen  ;buffer overflow: report 'string too long'

;[[command.disk]]



; DISK "command_string" [,U#] [,D#]     new [910123]

disk
                 lda #$f6                                 ; parse:  command_string [,U#]
                 jsr dosprs
                 jsr chk1                                 ; check parameters
                 lda #doslfn                              ; la (reserved la)
                 sta dosla
                 lda #$6f
                 sta dossa                                ; sa (command channel)
                 ldy #fdisk
                 ldx #2                                   ; add "/" [911108]
                 jsr open_file                            ; open command channel & send command string
                 php                                      ; save error status    [910404]
                 pha
                 lda #doslfn                              ; close it
                 sec                                      ; not a real close
                 jsr _close                               ; close it
                 pla                                      ; [910404]
                 plp
                 +lbra exit_disk_op                       ; common error check & exit path ????


;.end
;[[dos.setup]]



; These routines take tokens and values after the following BASIC keywords:
;
;  DOPEN,   DCLOSE,  APPEND,  CONCAT, RECORD
;  DLOAD,   DSAVE,   DVERIFY, BLOAD,  BSAVE
;  FORMAT,  COLLECT, BACKUP,  COPY
;  CATALOG, RENAME,  SCRATCH, DCLEAR
;
; It then parses the following line and finds syntax errors, checks for values
; out of range, and sets variables in the zero-page to be passed to the disk
; message generator.


directory                                                 ; display disk directory (catalog)
                 jsr chrgot                               ; get current chr
                 cmp #esc_command_token                   ; eat dirECTORY kludge if it's there
                 bne l216_1
                 jsr chrget                               ; (esc token + another)
                 cmp #ectory_token
                 +lbne snerr
                 jsr chrget                               ; yes- get next good char

l216_1           jsr dospar                               ; parse the line
                 lda parsts                               ; check options
                 and #$e6
                 +lbne snerr

                 ldy #fdir                                ; table offset for directory
                 bit dosflags                             ; want recoverable files? [901024]
                 bvc l216_2                               ; no
                 ldy #fdirr                               ; yes
l216_2           ldx #1                                   ; just $
                 lda parsts                               ; check for default
                 and #$11                                 ; no drive?
                 beq l216_4
                 lsr
                 bcc l216_3                               ; just drive
                 inx                                      ; drive and filename
                 inx
l216_3           inx
l216_4           txa                                      ; a now has length
                 jsr sendp                                ; build

                 ldx #sys_bank                            ; set banks????  fname in system space, bank0 [910620]
                 txa                                      ; (load bank is don't care- we're not actually loading)
                 jsr _setbank

                 ldy #$60                                 ; sa, load floppy
                 ldx dosfa
                 lda #doslfn                              ; lfn
                 jsr _setlfs                              ; set file parameters
                 jsr _open                                ; open it...
                 bcc l216_5                               ; ...ok
                 pha
                 jsr dcat11                               ; ...error, shut down and report
                 plx
                 sec
                 +lbra error

l216_5           lda channl                               ; determine DIR vs LDIR
                 bne ldir                                 ; if output channel not default (screen)
; use LDIR

; Get length in blocks

dir              ldx #doslfn
                 jsr _chkin
                 bcs dcat11                               ; if problem??
                 jsr _readst                              ; check status
                 bne dcat11                               ; exit if bad status
                 jsr crdo                                 ; output new line
                 ldy #3                                   ; loop counter

dcat3            sty t3                                   ; save counter

l217_1           jsr _basin                               ; get char
                 sta t4
                 jsr _basin                               ; get char
                 sta t4+1
                 jsr _readst                              ; check status
                 bne dcat11                               ; exit if eof or bad status
                 dec t3
                 bne l217_1                               ; if not done

; Output blocks number

                 ldx t4
                 lda t4+1
                 jsr linprt                               ; output number
                 lda #' '
                 jsr _bsout                               ; output a space

; Get name & output it

dcat4            jsr _readst                              ; get status
                 bne dcat10                               ; if bad status
                 jsr _basin                               ; get char
                 beq dcat5                                ; if eol
                 jsr _bsout                               ; echo char
                 bra dcat4                                ; continue to process name until eol or err

; Here on end of name

dcat5            jsr crdo                                 ; output new line
                 jsr _stop                                ; check stop key
                 beq dcat11                               ; exit if stop request

; Process next

                 ldy #2                                   ; perform 2 times
                 bra dcat3


; Exit directory

dcat10           jsr crdo                                 ; flush current line
dcat11           jsr release_channels                     ; release cmd channel, restore terminal
                 lda #doslfn
                 +lbra close_out                          ; [900725]

;[[command.ldir]]

; LDIR  same as DIR, except it buffers each line to reduce
;       talker/listener turnaround time.  Even so, it is still
; unacceptably slow for normal screen output, which is
; why it was split out from the original DIRECTORY routine.
;

; Read block count

ldir
                 lda #$c0                                 ; serial bus kludge for open4,4:cmd4:dir ????
                 and $d609
                 trb $d609                                ; disable fast serial bus
                 sta sid_speed_flag                       ; but save enables so we can restore them

                 ldy #3                                   ; loop counter (3=skip fake load adr & link bytes)
l218_1           sty t3                                   ; save counter
                 ldx #doslfn
                 jsr _chkin
                 bcs ldir_end                             ; problem??

l218_2           jsr _readst                              ; check status
                 bne ldir_end                             ; exit if bad status
                 jsr _basin                               ; get block count
                 sta dosstr                               ; buffer it
                 jsr _basin
                 sta dosstr+1
                 dec t3
                 bne l218_2                               ; continue eating bytes until we have block count

; Read filename

                 ldx #1                                   ; buffer index-1
l218_3           inx
                 jsr _readst                              ; check status
                 bne ldir_end                             ; exit if eof or bad status
                 jsr _basin                               ; buffer next character
                 sta dosstr,x
                 bne l218_3                               ; loop until eol (null terminator)

; Print one line of directory

                 jsr dcato                                ; get output channel
                 ldx dosstr
                 lda dosstr+1
                 jsr linprt                               ; print blocks

                 lda #' '
                 jsr _bsout                               ; print space

                 ldx #2
l218_4           lda dosstr,x
                 beq l218_5
                 jsr _bsout                               ; print filename (null terminated)
                 inx
                 bne l218_4

l218_5           jsr crdo                                 ; print return
                 jsr _clrch
                 jsr _stop                                ; check stop key
                 beq ldir_end                             ; exit if stop request

; Continue with next line

                 ldy #2                                   ; set to skip fake link bytes
                 bra l218_1                               ; loop


ldir_end
                 lda sid_speed_flag                       ; serial bus kludge for open4,4:cmd4:dir ????
                 tsb $d609                                ; restore fast serial bus enables
                 bra dcat11



dcato            jsr _clrch
                 ldx channl                               ; restore output channel
                 beq l219_1                               ; branch if screen (default output)
                 jmp _chkout                              ; else get output channel

l219_1           rts


;[[function.dopen]]
; DOPEN dfn(,t(,r))

dopen            lda #$22                                 ; set error flag
                 jsr dosprs                               ; parse the line
                 jsr chk6                                 ; check required parameters
                 jsr find_sa                              ; find unused secondary address
                 ldy #fopn                                ; fcb format pointer
                 ldx #4                                   ; normal length
                 bbr6 parsts,open_it                      ; relative record? branch if not relative
                 ldx #8                                   ; random access length
                 bra open_it                              ; [910925]

;l220_1 jsr open_file  ;open it
; bra exit_disk_op ;report any DOS errors, & return to main [910404]



; APPEND

append           lda #$e2                                 ; set error flags
                 jsr dosprs                               ; parse the line
                 jsr chk6                                 ; check required parameters
                 jsr find_sa                              ; find secondary address
                 ldy #fapn                                ; tabld index
                 ldx #5                                   ; length
open_it
                 jsr open_file                            ; open it
                 +lbra exit_disk_op                       ; report any DOS errors, & return to main [910404]



open_file                                                 ; dop2.
                 txa                                      ; set length into a
                 jsr sendp
                 jsr _clrch
                 ldx #sys_bank                            ; fname is in system space, bank0  [910620]
                 txa                                      ; (load bank not req'd)????
                 jsr _setbank
                 jmp _open


; Find an available secondary address

find_sa
                 ldy #$61                                 ; 2-14 possible

l220_1           iny
                 cpy #$6f
                 beq too_many_files                       ; if none available error
                 jsr _lkupsa                              ; kernel will lookup this sa in its tables
                 bcc l220_1                               ; if used keep looking
                 sty dossa                                ; save secondary address
                 rts                                      ; return .y = sa



; Find an available logical address

find_la
                 lda #0                                   ; 1-127 possible

l221_1           inc
                 bmi too_many_files                       ; if none available error
                 jsr _lkupla                              ; kernel will lookup this la in its tables
                 bcc l221_1                               ; if used keep looking
                 sta dosla                                ; save logical address
                 rts                                      ; return .a = la




too_many_files
                 ldx #errtmf                              ; too many files open
                 +lbra error


; Close disk file

dclose           lda #$f3                                 ; set error flags
                 jsr dosprs                               ; parse the line
                 jsr Clear_DS
                 bbr2 parsts,dclall                       ; any la given?  branch if not
                 lda dosla
                 +lbra close_out

dclall           lda dosfa                                ; get disk #
; jsr put_io_in_map
                 jmp _close_all                           ; close all channels



; DSAVE dfn

dsave            bbr4 runmod,l222_1                       ; PROGRAM or EDIT mode?    [910620]
                 +lbra edit_save                          ; edit

l222_1           lda #$66                                 ; set error flags
                 jsr dosprs                               ; parse the line
                 jsr chk2                                 ; check required parameters
                 ldy #fopn                                ; table offset
                 lda #4                                   ; ..length,
                 jsr sendp

                 lda text_bank                            ; default to text bank set up banks???? [910620]
                 ldx #sys_bank                            ; fname is in system space, bank0
                 jsr _setbank
                 +lbra savenp


; DVERIFY

dverify          lda #1                                   ; flag 'verify'
                 !text $2c



; DLOAD dfn

dload            lda #0
                 sta verck                                ; set load flag (for verify check later)

                 bbr4 runmod,l223_1                       ; PROGRAM or EDIT mode?    [910620]
                 +lbra edit_load                          ; edit

l223_1           lda #$e6                                 ; set error flags
                 jsr dosprs                               ; parse the line
                 jsr chk2                                 ; check required parameters


dload_boot                                                ; <<<<<<<<<<<<<<<<<< entry for BOOT'AUTOBOOT.C65'
                 lda #0
                 sta dossa                                ; set relocate flag
                 ldy #fopn                                ; table offset
                 lda #4                                   ; ..length
                 jsr sendp

                 lda text_bank                            ; set up banks ???? want text_bank ????  [910620]
                 ldx #sys_bank                            ; fname is in system space, bank0
                 jsr _setbank

                 +lbra cld10                              ; finish load, using 'LOAD' code.


; BSAVE

bsave            lda #$66                                 ; std error flag
                 ldx #$f8                                 ; auxiliary error flag (allow bank, start & end address)
                 jsr dosprx                               ; parse options
                 jsr chk2                                 ; check required parameters

                 lda parstx                               ; check for starting & ending addresses
                 and #6
                 cmp #6
                 +lbne snerr                              ; ..if not present, syntax error

                 lda dosofh+1                             ; check that ea>sa
                 cmp dosofl+1
                 +lbcc fcerr                              ; ...error
                 bne l224_1
                 lda dosofh
                 cmp dosofl
                 +lbcc fcerr                              ; ...error
                 +lbeq fcerr

l224_1           ldy #fopn                                ; table offset
                 lda #4                                   ; ..length
                 jsr sendp

                 lda dosbnk                               ; get requested bank
                 ldx #sys_bank                            ; ..and name will be in system bank
                 jsr _setbank                             ; ..and go set up bank

                 ldx dosofl                               ; start addr
                 ldy dosofl+1
                 lda #highds                              ; ..and a pointer to start address
                 stx highds
                 sty highds+1
                 ldx dosofh                               ; end addr
                 ldy dosofh+1
                 +lbra savenb



; DVERIFY

bverify          lda #1                                   ; flag 'verify'
                 !text $2c


;[[command.bload]]

; BLOAD dfn

bload            lda #0                                   ; flag 'load'
                 sta verck                                ; eventually the 'load' routine will look here

                 lda #$e6                                 ; std error flag
                 ldx #$fc                                 ; aux error flag (allow bank & load address)
                 jsr dosprx                               ; parse options

bload_boot                                                ; <<<<<<<<<<<<<<<<<<<<<< entry for BOOT'filename'
                 jsr chk2                                 ; check required parameters
                 ldx dosofl                               ; get starting address high
                 ldy dosofl+1                             ; ..and lo
                 lda #0                                   ; assume x & y not both=ff (means real add., not def)
                 cpx #$ff
                 bne l225_1
                 cpy #$ff
                 bne l225_1
                 lda #$ff                                 ; use defaults
l225_1           sta dossa

                 ldy #fopn                                ; table offset
                 lda #4                                   ; ..length,
                 jsr sendp                                ; ...and go send parameters

                 lda dosbnk
                 ldx #sys_bank
                 jsr _setbank

                 lda verck                                ; flag LOAD or VERIFY
                 ldx dosofl                               ; get starting address high
                 ldy dosofl+1                             ; ..and lo (in case this isn't a 'default' load)
                 jsr _loadsp                              ; load it

; Any changes to the following code must be duplicated at:
;  load (load_file)
;  save (exit_disk_op)

                 php                                      ; save kernel load status (.c)
                 pha                                      ; save kernel error # (.a)
                 jsr _readst                              ; save I/O status byte
                 sta parsts
                 jsr print_dos_error                      ; report DOS problems
                 pla                                      ; restore error stuff
                 plp
                 bcc l225_4                               ; branch if no error (rts)
                 bbs7 runmod,l225_3                       ; branch if run mode (erexit)
                 cmp #errfnf                              ; is it 'file not found' catch-all?
                 bne l225_2                               ; no  (erexit)
                 sta errnum                               ; yes- save error # for 'er'
                 ora #$80                                 ; but no errdis
l225_2           sec
l225_3           +lbcs erexit                             ; exit if kernel problem (rts)

l225_4           lda verck                                ; load or verify operation?
                 +lbne verify_check                       ; verify

; jsr _readst  ;  read status
                 lda parsts                               ; load
                 and #%10111111                           ; EOI is okay, so mask it
                 +lbne load_error                         ; load error
                 clc
                 rts

;[[command.header]]


; HEADER nddn [,id]  (alias: FORMAT)

header           jsr dospar                               ; parse the line
                 jsr chk1                                 ; check parameter errors
                 and #$01
                 cmp #$01
                 +lbne snerr                              ; if required parameters not present

                 jsr _clall                               ; close all files
                 jsr are_you_sure                         ; confirm if in direct mode
                 bne header_rts                           ; exit if 'no' response
                 ldy #fhed                                ; tabld index
                 lda #4                                   ; length
                 ldx dosdid                               ; check for diskid
                 beq l226_1
                 lda #6                                   ; length with id

l226_1           jsr trans                                ; build and send command
;fall into 'print_dos_error'

;[[dos.errors]]

print_dos_error                                           ; [900725]
                 bbs7 runmod,header_rts                   ; branch if not direct mode
                 jsr Check_DS                             ; get current disk error message
                 ldy #0
                 lda #dsdesc+1
                 jsr lda_far_ram1                         ; lda (dsdesc+1),y peek at first character
                 cmp #'2'
                 bcc header_rts                           ; branch if no error occured ('00' or '01')
                 cmp #'7'
                 bne l227_1                               ; [900730]
                 iny
                 lda #dsdesc+1
                 jsr lda_far_ram1                         ; might be '73' powerup message
                 cmp #'3'
                 beq header_rts                           ; yup

; ldx #errbdk  ; bad disk error (carry is set)
; bra error

; Print DOS error message as if it were a BASIC error message   [900910]

l227_1           lda #$ff                                 ; reset error line
                 sta errlin                               ;
                 sta errlin+1
                 jsr _clrch
; inc a   ;a=0 restore output to screen   [910909]
                 sta channl
                 jsr RestoreTextScreen                    ; make sure we're in text mode????  [910404]
                 jsr init_stack                           ; clean up system, string temps, etc.  [910121]

                 jsr highlight_text                       ; [910624]
                 jsr _primm                               ; start a new line with '?DOS: '
                 !text cr,"?DOS: ",0                      ; (to distinguish ?DOS: SYNTAX ERROR from ?SYNTAX ERROR)

                 ldy #3                                   ; print text part of message only
                 lda #dsdesc+1
                 jsr lda_far_ram1                         ; skip err#, comma, & leading space if any
                 cmp #' '
                 bne l227_3
                 iny
l227_2           lda #dsdesc+1
                 jsr lda_far_ram1
                 cmp #','                                 ; finished at comma preceding trk, sector
                 beq l227_4
l227_3           jsr outch
                 iny
                 bpl l227_2                               ; loop always (bpl=failsafe)

l227_4           jsr highlight_done                       ; [910624]
                 jsr crdo
                 +lbra ready                              ; we're in direct mode, error msg has been printed, abort

header_rts
                 clc
                 rts

;[[command.scratch]]

; SCRATCH sdfn  aliases: DELETE, ERASE

scratch          jsr dospar                               ; parse the line
                 jsr chk1
                 jsr are_you_sure                         ; confirm if in direct mode
                 bne l228_4                               ; branch if 'no' response given

                 ldy #fscr                                ; offset
                 lda #4                                   ; length
                 bit dosflags                             ; scratch or recover?
                 bvc l228_1                               ; scratch
                 ldy #frscr                               ; recover
                 lda #6
l228_1           jsr trans                                ; transmit scratch command
                 jsr Read_DS                              ; read error channel & update DS$

                 bbs7 runmod,l228_4                       ; branch if not direct mode
                 jsr crdo                                 ; output cr

                 ldy #0                                   ; display 'files scratched' DOS message
l228_2           lda #dsdesc+1
                 jsr lda_far_ram1                         ; lda (dsdesc+1),y
                 beq l228_3                               ; if end of error message
                 jsr outch                                ; print it
                 iny
                 bpl l228_2                               ; always (bpl=failsafe)

l228_3           jsr crdo                                 ; done

l228_4           rts


;[[command.record]]
; RECORD- relative record access

record           lda #'#'
                 jsr synchr                               ; syntax error if not 'record#'

                 jsr getbyt                               ; get lfn in x
                 cpx #0
                 +lbeq fcerr                              ; cannot be zero
                 stx dosla                                ; save logical address

                 jsr comwrd                               ; check for comma, get record number in 'poker'

                 ldx #1                                   ; set up to get starting byte # - default is 1
                 jsr optbyt
                 stx dosrcl                               ; save byte position (pos)    [911024]
                 txa                                      ; cpx #0
                 +lbeq fcerr                              ; if out of range
                 inx                                      ; cpx #$ff
                 +lbeq fcerr                              ; if out of range

                 lda dosla                                ; get logical address
; jsr put_io_in_map
                 jsr _lkupla                              ; logical to physical map
                 bcs l229_1                               ; if file not found (not open)    [910404]
                 sty dossa_temp                           ; save secondary address

                 stx dosfa                                ; set up device number for trans routine
                 lda #0
                 sta dosla                                ; set up logical address for trans routine
                 lda #$6f
                 sta dossa                                ; and secondary address, too!

                 ldy #frec                                ; set pointer
                 lda #4                                   ; process five bytes
                 jsr trans                                ; send command
                 +lbra print_dos_error                    ; if any

l229_1           ldx #errfno                              ; file not found err (file not open)   [910404]
                 +lbra error

;[[command.dclear]]

; DCLEAR - reinitilaize the drive

dclear           jsr dospar                               ; parse the line
                 ldy #finit                               ; set code
                 lda #2
                 jsr trans                                ; send command
                 jsr print_dos_error                      ; if any
                 +lbra dclall

;[[command.collect]]
; COLLECT v<drive#>

collect          jsr dospar                               ; parse the line
                 jsr chk3                                 ; check optional parameters
                 jsr _clall                               ; close all files
                 ldy #fcoll                               ; tabld offset
                 lda #1                                   ; length
                 bbr4 parsts,l230_1
                 inc                                      ; include drive
l230_1           jsr trans                                ; send command
                 +lbra print_dos_error                    ; if any


;[[command.copy]]

; COPY cdddfn=sdsfn

dcopy            jsr dospar                               ; parse the line
                 and #$30
                 cmp #$30                                 ; check required parameters
                 bne l231_1                               ; branch if single drive copy
                 lda parsts                               ; else check for dual drive params
                 and #$c7
                 beq l231_2
                 and #3                                   ; special check for 2nd filename   [910717]
                 cmp #3
                 beq l231_1                               ; branch if given
                 lda #'*'
                 sta dosdid                               ; else supply "*" for him, just like 'name2'
                 lda #1
                 ldx #<dosdid
                 ldy #>dosdid
                 sta dosf2l
                 stx dosf2a
                 sty dosf2a+1
                 lda #2                                   ; and set filename2 flag
                 tsb parsts                               ; set flag in status
l231_1           lda parsts
                 jsr chk4
; lda parsts
l231_2           ldy #fcopy                               ; tabld offset
                 lda #8                                   ; length
                 jsr trans                                ; send command
                 +lbra print_dos_error                    ; if any



;[[command.concat]]
; CONCAT

concat           jsr dospar                               ; parse the line
                 jsr chk4
                 ldy #fconc                               ; offset
                 lda #12                                  ; length
                 jsr trans                                ; send command
                 +lbra print_dos_error                    ; if any



;[[command.rename]]

; RENAME rdddfn=sdsfn

rename           lda #$e4                                 ; set error flags
                 jsr dosprs                               ; parse the line
                 jsr chk5
                 ldy #fren                                ; offset
                 lda #8                                   ; length
                 jsr trans                                ; send command
                 +lbra print_dos_error                    ; if any



; BACKUP D<destination_drive>=D<source_drive>
;
; where destination|source_drive is [0...9]

backup           lda #$c7                                 ; set error flags
                 jsr dosprs                               ; parse the line
                 and #$30                                 ; required parameters
                 cmp #$30
                 +lbne snerr
                 jsr are_you_sure
                 beq l232_1                               ; if run mode or not 'yes'
                 rts

l232_1           jsr dclall                               ; close disk
                 ldy #fbak
                 lda #4                                   ; length
                 jsr trans                                ; send command
                 +lbra print_dos_error                    ; if any

;[[command.trans]]


; Trans subroutine

trans            jsr sendp                                ; build string to output
                 jsr _clrch
                 ldx #sys_bank                            ; name is in system space, bank0 ????  [910620]
                 txa
                 jsr _setbank
                 jsr _open                                ; send it...
                 php                                      ; save error status (.c)
                 pha                                      ; save error code (if any)
                 lda dosla
                 sec
                 jsr _close                               ; special close...
                 pla                                      ; pop error
                 plp                                      ; pop error status
                 +lbcs erexit                             ; ...branch if there was an error opening
                 rts

;.end
;[[dos.parser]]



; This is the DOS parser routine which looks at lines passed to it and
; verifies that the syntax is proper.  -mgm 7/23/79-
;
; Entry  (dosprs)
; a = parsts bit which must be zero.
;
; Exit  a = parsts as follows
;
;    ---------------------------------
;    | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
;    ---------------------------------
;      ^   ^   ^   ^   ^   ^   ^   ^
;      |   |   |   |   |   |   |   |_____ fn1 set for the first filename
;      |   |   |   |   |   |   |_______ fn2 set for second filename
;      |   |   |   |   |   |_________ la set when #lfn parsed
;      |   |   |   |   |___________ fa set for device number
;      |   |   |   |_____________ d1 set for first disk unit
;      |   |   |_______________ d2 set for second disk unit
;      |   |_________________ dosrcl set for record size
;      |___________________ @ set when @ encountered
;
; The following are the vaild bit patterns for parsts after parsing for the
; various keywords:
;
;       7 6 5 4  3 2 1 0
;
;     header  0 0 0 *  * 0 0 1
;     collect 0 0 0 *  * 0 0 0
;     dclear  0 0 0 *  * 0 0 0
;     backup  0 0 1 1  * 0 0 0
;     copy    0 0 1 1  * 0 0 0
;      or..   0 0 * *  * 0 1 1
;     concat  0 0 * *  * 0 1 1
;     bsave   * 0 0 *  * 0 0 1
;     dsave   * 0 0 *  * 0 0 1
;     bload   0 0 0 *  * 0 0 1
;     dload   0 0 0 *  * 0 0 1
;     dverify 0 0 0 *  * 0 0 1
;     catalog 0 0 0 *  * 0 0 *
;     rename  0 0 0 *  * 0 1 1
;     append  0 0 0 *  * 1 0 1
;     scratch 0 0 0 *  * 0 0 1
;     dopen   * * 0 *  * 1 0 1
;     dclose  0 0 0 0  * * 0 0
;             ^ ^ ^ ^  ^ ^ ^ ^
;             @ l d d  f l f f
;             r r 2 1  a a n n
;             p e          2 1
;             l l
;
;      "0" bits are required to be clear
;      "1" bits are required to be set
;      "*" bits are optional parameters

; Entry (dosprs):
;  parstx shall be set to prevent any auxiliary options to be specified.
;
; Entry (dosprx):
;  .x = parstx bits which must be zero.
;
; Exit:  .x = parstx as follows:
;
;     ---------------------------------
;     | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
;     ---------------------------------
;       ^   ^   ^   ^   ^   ^   ^   ^____ bnk is set for bank option
;       |   |   |   |   |   |   | ______ offl set for 1st address
;       |   |   |   |   |   |_________ offh set for 2nd address
;       |   |   |   |   |___________ unused
;       |   |   |   |_____________ unused
;       |   |   |_______________ unused
;       |   |_________________ unused
;       |___________________ unused
;
; The following are the valid bit patterns for parstx after parsing for the
; various keywords.  These options are allowed for load/save commands only.
;
;    7 6 5 4  3 2 1 0
;
;   bsave   0 0 0 0  0 1 1 *
;   bload   0 0 0 0  0 0 * *
;   bverify 0 0 0 0  0 0 * *
;   [auto]boot 0 0 0 0  0 0 * *
;    ^ ^ ^ ^  ^ ^ ^ ^
;    ? ? ? ?  ? o o b
;        f f n
;        h l k
;
;      "0" bits are required to be clear
;      "1" bits are required to be set
;      "*" bits are optional parameters

dostbl           !word $ffff,$ffff                        ; default start/end address
                 !text doslfn,dosffn,$6f                  ; default la/fa/sa


dospar           lda #0                                   ; DOS Parser

dosprs                                                    ; special error flag entry
                 ldx #$ff                                 ; no aux options!

dosprx                                                    ; spec aux error flag entry
                 pha                                      ; save error flags
                 phx
                 lda #0
                 sta parsts                               ; reset parser status/option words
                 sta parstx

                 ldx #dosspc                              ; clear DOS scratch area   [900522]
l233_1           sta xcnt-1,x
                 dex                                      ; no filenames, null lengths
                 bne l233_1

                 ldx #dossa-dosofl                        ; set some defaults from table
l233_2           lda dostbl,x
                 sta dosofl,x                             ; start/end adr = $FFFF, la/fa/sa
                 dex
                 bpl l233_2

                 ldx _default_drive                       ; set default device   [900522]
                 stx dosfa
                 ldx current_bank                         ; set current bank
                 stx dosbnk

                 jsr chrgot                               ; get next character from command string
                 bne parse1                               ; if eol stick with defaults, else begin parsing


; Done parsing, check for errors, return if everything okay

done             pla                                      ; get aux error flag
                 and parstx                               ; repeated or illegal params?
                 +lbne snerr                              ; yes- report syntax error
                 pla                                      ; get error flags
                 jsr prmrpt
                 lda parsts
                 ldx parstx
                 rts


; Parse given parameters.  what it is  example
;     -------------------- ---------
parse1           cmp #'"'
                 +lbeq name1                              ; explicit filename "file"
                 cmp #'('
                 +lbeq name1                              ; evaluate filename (f$)
                 cmp #'#'
                 beq logadr                               ; logical file number #1
                 cmp #'U'
                 beq unit1                                ; unit number  U8
                 cmp #'D'
                 beq drv1                                 ; drive number  D0
                 cmp #'P'
                 +lbeq doffl                              ; load/save address P1234
                 cmp #'B'
                 beq dbank1                               ; load/save bank   B0
                 cmp #'W'
                 beq reclen                               ; write mode  W
                 cmp #'L'
                 beq reclen                               ; record length  L80
                 cmp #'R'
                 +lbeq recover                            ; recover mode  R
                 cmp #'I'
                 beq ident                                ; ID   Ixx
                 cmp #on_token
; beq on1   ; ON token  ON

                 +lbne snerr                              ; none of these, syntax error


on1              jsr on
                 +lbra del1


unit1            jsr unit                                 ; do unit# parsing
                 +lbra del1                               ; always


dbank1           jsr dbank
                 +lbra del1                               ; always


logadr           lda #4
                 jsr prmrpt                               ; check for repeated parameter
                 jsr gtbytc                               ; getval
                 txa                                      ; cpx #0
                 +lbeq fcerr                              ; if illegal value
                 stx dosla
                 lda #4                                   ; set logical address flag
                 +lbra del1                               ; get next parameter


reclen           tax                                      ; save char
                 lda #$40
                 jsr prmrpt                               ; check for repeated parameter
                 cpx #'W'
                 bne l234_1
                 jsr chrget
                 bra l234_4                               ; set parsts

l234_1           ldx #1                                   ; a kludge to allow  DOPEN#lf,"relfile",L  [911024]
                 jsr chrget
                 beq l234_2                               ; eol? open existing rel file
                 jsr getbyt                               ; get reclen (was getval)
l234_2           stx dosrcl                               ; store parcel
                 txa                                      ; cpx #0
                 beq l234_3                               ; zero illegal dosrcl
                 inx                                      ; cpx #255
l234_3           +lbeq fcerr                              ; illegal dosrcl

l234_4           lda #$40                                 ; set dosrcl flag &
                 +lbra del1


drv1             lda #$10
                 jsr prmrpt                               ; check for repeated parameter
                 jsr gtbytc                               ; getval
                 cpx #10
                 +lbcs fcerr                              ; illegal drv# if >9 [allow 0: to 9: ?????]
                 stx dosds1
                 stx dosds2
                 lda #$10
                 +lbra del1


ident            lda #$80                                 ; set ID flag
                 tsb dosflags
                 +lbne snerr                              ; repeated parameter
                 jsr chrget                               ; get next character
                 cmp #'('                                 ; c65: allow I(ID$) syntax  [900710]
                 bne l235_1
                 jsr frmstr                               ; get ID from var
                 cmp #2
                 +lbcc err_mfn                            ; if length < 2, error
                 ldy #0
                 jsr indin1_ram1                          ; else grab first two characters
                 sta dosdid
                 iny
                 jsr indin1_ram1
                 sta dosdid+1
                 bra delim1                               ; continue

l235_1           sta dosdid                               ; m(txtptr => dosdid
                 jsr chrget
                 sta dosdid+1
                 jsr chrget                               ; continue
                 bra delim2


doffl            lda #$02                                 ; check aux status
                 jsr prxrpt
                 jsr getoff                               ; get offset value
                 sty dosofl
                 sta dosofl+1
                 lda #$02
dlimx1           ora parstx                               ; set aux status bits
                 sta parstx
                 bne delim1                               ; try for next param


doffh            lda #$04
                 jsr prxrpt
                 jsr getoff
                 sty dosofh
                 sta dosofh+1
                 lda #$04
                 bra dlimx1                               ; set aux status


recover          lda #$40
                 tsb dosflags                             ; set 'recover' bit
                 +lbne snerr                              ; if repeated parameter
                 jsr chrget                               ; continue
                 bra delim2


name1            lda #1                                   ; name1 allowed only once
                 jsr newnam                               ; do name parsing
                 sta dosf1l

                 ldy #0
l236_1           jsr indin1_ram1
                 sta savram,y                             ; copy name into buffer
                 iny
                 cpy dosf1l
                 bcc l236_1                               ; ...copy all of it
                 lda #1                                   ; set name1 flag


del1             tsb parsts

delim1           jsr chrgot
delim2           bne nxxx
                 +lbra done                               ; <cr>/<> => done


next6            cmp #on_token
                 +lbeq on1
                 cmp #to_token                            ; "to" token
                 +lbne snerr                              ; syntax error

;  If "to" is not followed by an offset param, then do file2 params.
;  Otherwise, do high offset and continue with file0 options.

                 jsr chrget
                 cmp #'P'
                 bne pars22
                 beq doffh


nxxx             cmp #','
                 bne next6
                 jsr chrget
                 +lbra parse1


parse2           jsr chrget
pars22           cmp #'D'
                 beq l237_1
                 cmp #on_token                            ; "on" token
                 beq on2
                 cmp #'U'
                 beq unit2
                 cmp #'"'
                 beq name2
                 cmp #'('
                 beq name2

l237_1           lda #$20
                 jsr prmrpt                               ; check for repeated parameter
                 jsr gtbytc                               ; getval
                 cpx #10
                 +lbcs fcerr                              ; illegal drive #  [allow 0: to 9: ????]
                 stx dosds2
                 lda #$20
                 bra del2

on2              jsr on
                 bra del2


unit2            jsr unit                                 ; do unit# parsing
                 bra del2                                 ; always

name2            lda #2                                   ; name2 allowed only once
                 jsr newnam
                 sta dosf2l
                 stx dosf2a
                 sty dosf2a+1

                 lda #2                                   ; set filename2 flag &
del2             tsb parsts                               ; set flag in status
                 jsr chrgot
                 +lbeq done                               ; done on <cr>/<>
                 cmp #','
                 beq parse2
                 cmp #on_token                            ; "on" token
                 beq on2
                 cmp #'U'
                 beq unit2
                 +lbra snerr


on               jsr chrget
                 cmp #'B'
                 beq dbank
                 cmp #'U'
                 +lbne snerr


unit             jsr gtbytc                               ; getval
                 cpx #31
                 bcs err_ild                              ; error if >30
                 cpx #1                                   ; drive 1 = use system default drive  [910221]
                 bne l238_1
                 ldx _default_drive
                 bra l238_2
l238_1           cpx #4
                 bcc err_ild                              ; error if <4
l238_2           stx dosfa
                 lda #$08                                 ; set parser's unit flag
                 rts


dbank            lda #$01                                 ; repeated param?
                 jsr prxrpt
                 jsr gtbytc                               ; getval
; cpx #16  ;bank too large?
; bcs fcerr ;illegal qty
                 stx dosbnk
                 lda #$01
                 tsb parstx                               ; set bnk bit in aux status
                 lda #0                                   ; .a=std status wrd, no bits to set
                 rts


newnam
                 pha                                      ; save nam1,2 for subdir check later  [901115]
                 jsr prmrpt                               ; check for repeated parameter
                 jsr frmstr
                 tax                                      ; save length of string
                 beq err_mfn                              ; if length = 0
                 ldy #0
                 jsr indin1_ram1
                 cmp #'@'                                 ; Replace file convention?
                 bne l239_1                               ; no
                 lda #$80                                 ; yes- check for repeated param
                 jsr prmrpt
                 smb7 parsts                              ; set "@" flag
                 dex                                      ; decrement length
                 inw index1                               ; increment past "@"
                 bra lenchk

l239_1           cmp #'/'                                 ; Subdirectory (partition)?   [901115]
                 bne lenchk                               ; no
                 pla                                      ; yes- recall nam1 or nam2
                 tsb dosflags                             ; set appropriate '/' flag (.a=1 or 2)
                 dex                                      ; decrement length
                 inw index1                               ; increment past "@"
                 !text $89


lenchk           pla                                      ; [901115]
                 txa                                      ; Check filename length
                 beq err_mfn                              ; too small, missing filename  [901115]
; cmp #17  ;  ???? (problem if name has ',P')
; bcs errlen ; too long
                 ldx index1
                 ldy index1+1                             ; ok- return pointer to filename
                 rts


err_mfn          ldx #err_missing_fname
                 !text $2c

err_ild          ldx #err_illegal_device
                 !text $2c

errlen           ldx #errls                               ; string or filename too long
                 +lbra error



; Get next value routine (use gtbytc instead!)

;getval jsr chrget ;get nxt chr
; beq snerr ;if end of statement
; bra getbyt ;       [911024]

; bcc getbyt ;can be numeric, go evaluate it  why this crap ????
; jsr chkopn ;or a "("
; jsr getbyt ;anything else is an error
; bra chkcls ;need closing ")"



; Get next 2-byte expression.  Exit: .a,.y (high,low) value

getoff           jsr chrget                               ; get nxt chr
                 +lbeq snerr                              ; if end of statement
                 +lbcc getwrd                             ; can be num. const, go evaluate it
                 jsr chkopn                               ; or a "("
                 jsr getwrd                               ; expr
                 jsr chkcls                               ; need closing ")"
                 ldy poker
                 lda poker+1
                 rts



; Prmrpt checks for a repeated parameter.
;
; Entry: .a contains parsts flag to check


prmrpt           and parsts                               ; compare mask with status
                 +lbne snerr                              ; error if bit previously set
                 rts



; Prxrpt checks for a repeated parameter.
;
; Entry: .a contains parstx flag to check


prxrpt           and parstx                               ; and with parstx
                 +lbne snerr                              ; if bit previously set
                 rts

;.end



; CBM-2001 BASIC-4 disk verb processors.  -- rsr 7-24-79 --
;
; Token Table Definitions

xsca             =$c2                                     ; send dossca
xid              =$d0                                     ; sends disk id
xd1              =$d1                                     ; sends dosds1
xd2              =$d2                                     ; sends dosds2
xrec             =$e0                                     ; sends S for SEQ, or dosrcl
xwrt             =$e1                                     ; sends W or L
xrcl             =$e2                                     ; send low ((poker))
xfat             =$f0                                     ; sends "@" or "/" if specified
xfn1             =$f1                                     ; sends filename1
xfn2             =$f2                                     ; sends filename2


; Tabld - used to build disk command strings

tabld
finit            = $ff                                    ; Dclear
                 !text "I",xd1

fdir             = *-tabld-1                              ; Dir
                 !text "$",xd1,":",xfn1

fdirr            = *-tabld-1                              ; Recover Dir  [901024]
                 !text "_",xd1,":",xfn1                   ; ('_'=CBM backarrow, $5F)

fopn             = *-tabld-1                              ; Dopen
                 !text xfat,xd1,":",xfn1,",",xwrt,",",xrec

fopnseq          = *-tabld-1                              ; Type   [900801]
                 !text xfat,xd1,":",xfn1,",S"

fsavseq          = *-tabld-1                              ; EDIT_SAVE  [910620]
                 !text xfat,xd1,":",xfn1,",S,W"

fconc            = *-tabld-1                              ; Concat
                 !text "C",xd2,":",xfn2,"=",xd2,":",xfn2,","

fapn             = *-tabld-1                              ; Append
                 !text xd1,":",xfn1,",A"

fhed             = *-tabld-1                              ; Header
                 !text "N",xd1,":",xfn1,",",xid

fcoll            = *-tabld-1                              ; Collect
                 !text "V",xd1

fbak             = *-tabld-1                              ; Backup
                 !text "D",xd2,"=",xd1

fcopy            = *-tabld-1                              ; Copy
                 !text "C",xd2,":",xfn2,"=",xd1,":",xfn1

fren             = *-tabld-1                              ; Rename
                 !text "R",xd1,":",xfn2,"=",xd1,":",xfn1

fscr             = *-tabld-1                              ; Scratch
                 !text "S",xd1,":",xfn1

frscr            = *-tabld-1                              ; Recover Del  [901024]
                 !text "F-R",xd1,":",xfn1

fdisk            = *-tabld-1                              ; Disk command  [910123]
                 !text xfat,xfn1                          ; pass '/' [911030]

frec             = *-tabld-1                              ; Record
                 !text "P",xsca,xrcl,xrec


;[[dos.sendparam]]
; Send parameters to device
;
; Entry: .a = number of bytes in format
;  .y = pointer to TABLD entry

sendp            sta xcnt                                 ; save number of string bytes
                 phy
                 jsr Clear_DS                             ; clear old status

                 ldx #0
sdp1             pla
                 dec xcnt
                 bmi tranr
                 tay
                 iny                                      ; move down table
                 phy
                 lda tabld,y                              ; get next entry
                 bpl sdp5                                 ; if not escape code
                 cmp #xsca                                ; if not secondary address
                 beq rsca
                 cmp #xid
                 beq rid                                  ; if disk id
                 cmp #xrcl
                 +lbeq rdcn                               ; if record number
                 cmp #xwrt
                 beq rwrt                                 ; if W or L
                 cmp #xfat
                 beq rfat                                 ; if "@" symbol request
                 cmp #xfn1
                 +lbeq rsfn                               ; if filename 1
                 cmp #xfn2
                 +lbeq rdfn                               ; if filename 2
                 cmp #xrec
                 bne sdp2                                 ; if not record type
                 lda dosrcl                               ; get rec #
                 cmp #1                                   ; kludge to allow DOPEN#lf,"relfile",L  [911024]
                 bne sdp5                                 ; (note RECORD byte 0 = byte 1 anyhow)
                 dec
                 bra sdp5                                 ; always branch

sdp2             cmp #xd1
                 bne sdp3                                 ; if not drive 1
                 lda dosds1
                 bra sdp4                                 ; always branch

sdp3             cmp #xd2
                 bne sdp1                                 ; if not drive 2, continue
                 lda dosds2
sdp4             ora #'0'                                 ; change # to PETSCII

sdp5             sta dosstr,x                             ; else into buffer
                 inx
                 bra sdp1                                 ; always


tranr            txa                                      ; length to a
                 pha
                 ldx #<dosstr                             ; set filename
                 ldy #>dosstr
                 jsr _setnam

                 lda dosla                                ; set channel
                 ldx dosfa
                 ldy dossa
                 jsr _setlfs

                 pla
                 rts




rsca             lda dossa_temp                           ; secondary address (record)
                 bra sdp5                                 ; always


rfat             bbr7 parsts,l240_1                       ; if "@" not encountered
                 lda #'@'
                 bra sdp5                                 ; always

l240_1           lda dosflags
                 lsr
                 bcc sdp1                                 ; if "/" not encountered
                 lda #'/'
                 bra sdp5


; ID subroutine

rid              lda dosdid                               ; include id
                 sta dosstr,x
                 inx
                 lda dosdid+1
                 bra sdp5                                 ; always


rwrt             lda dosrcl                               ; check for L or W
                 beq l241_1                               ; zero then write
                 lda #'L'
                 bra sdp5                                 ; always

l241_1           lda #'S'                                 ; send W,S
                 sta dosrcl
                 lda #'W'
                 bra sdp5                                 ; always



; Move record number

rdcn             lda poker
                 sta dosstr,x
                 lda poker+1
                 inx
                 bra sdp5                                 ; always


; Move file names

rsfn             ldy dosf1l                               ; file name 1: get length
                 beq rdrt0                                ; if null string

                 ldy #0                                   ; move name to dosstr
l242_1           lda savram,y
                 sta dosstr,x
                 inx
                 iny
                 cpy dosf1l
                 bne l242_1                               ; if move not complete
                 bra rdrt1                                ; always


rdfn             lda dosf2a
                 sta index1
                 lda dosf2a+1
                 sta index1+1
                 ldy dosf2l
                 beq rdrt0                                ; if null string

                 ldy #0                                   ; move name to dosstr
l243_1           jsr indin1_ram1
                 sta dosstr,x
                 inx
                 iny
                 cpy dosf2l
                 bne l243_1                               ; if move not complete
                 !text $89                                ; hop

rdrt0            dex                                      ; case cdd=sd
rdrt1            +lbra sdp1                               ; get next symbol


; Syntax checker DOS write

chk1             and #$e6                                 ; for HEADER, DLOAD, SCRATCH, TYPE, LIST
                 +lbne snerr

chk2             lda parsts                               ; for DSAVE
                 and #1
                 cmp #1                                   ; check required parameters
                 +lbne snerr                              ; error if 1 missing
                 lda parsts                               ; reload for return
                 rts


chk3             and #$e7                                 ; for COLLECT
                 +lbne snerr                              ; check optional parameters
                 rts


chk4             and #$c4                                 ; for COPY, CONCAT
                 +lbne snerr                              ; check optional parameters
                 lda parsts
chk5             and #3                                   ; for RENAME
                 cmp #3                                   ; check required parameters
                 +lbne snerr
                 lda parsts                               ; reload for return
                 rts


chk6             and #5                                   ; for APPEND, DOPEN
                 cmp #5                                   ; check required parameters
                 +lbne snerr
                 lda parsts                               ; reload for rts
                 rts

;.end



; Allocate DS$ if nesessary, but use old DS$ string otherwise
; Called by DS$ and DS

Check_DS                                                  ; chkds.
                 lda dsdesc
                 beq Read_DS_1                            ; branch if DS$ is not in memory
                 rts                                      ; else return & use old one


; Allocate DS$ if necessary & Read DOS error channel

Read_DS                                                   ; errchl.
                 lda dsdesc                               ; has DS$ space been allocated?
                 bne Read_DS_2                            ; yes

Read_DS_1
                 lda #40                                  ; no- get 40 char string
                 sta dsdesc
                 jsr getspa                               ; allocate space for DS$
                 stx dsdesc+1                             ; low address of string
                 sty dsdesc+2                             ; high   "    "    "
                 ldx #dsdesc+1                            ; set up string back pointer to dsdesc
                 ldy #40
                 lda #<dsdesc
                 jsr sta_far_ram1                         ; sta (dsdesc+1),y
                 iny
                 lda #>dsdesc
                 jsr sta_far_ram1                         ; sta (dsdesc+1),y

Read_DS_2
                 ldx dosfa                                ; fa
                 cpx #2
                 bcs l244_1                               ; if =0 or 1 use default  [910429]
                 ldx _default_drive                       ; (was dosffn)   [900710]
                 stx dosfa
l244_1           lda #doslfn                              ; la (reserved la)
                 ldy #$6f                                 ; sa (command channel)
                 jsr _setlfs
                 lda #0                                   ; no name (so no setbank)
                 jsr _setnam
                 jsr _open                                ; get command channel
                 ldx #doslfn
                 jsr _chkin
                 bcs l244_4                               ; a problem (file already open??)

                 ldy #$ff
l244_2           iny                                      ; read disk error message
                 jsr _basin
                 cmp #cr
                 beq l244_3                               ; if eol
                 ldx #dsdesc+1
                 jsr sta_far_ram1                         ; sta (dsdesc+1),y copy to DS$
                 cpy #40
                 bcc l244_2                               ; loop unless too long

l244_3           lda #0                                   ; errend.
                 ldx #dsdesc+1                            ; terminate DS$ with a null
                 jsr sta_far_ram1                         ; sta (dsdesc+1),y

                 jsr _clrch                               ; shut down command channel
                 lda #doslfn
                 sec                                      ; not a real close
                 jmp _close                               ; close it and rts

l244_4           pha                                      ; errbad.
                 jsr l244_3
                 jsr Clear_DS                             ; flag 'no DS available'
                 plx                                      ; get error
                 +lbra error


; Clear_DS subroutine - forget current DS$ message, if any
;

Clear_DS                                                  ; oldclr.
                 lda dsdesc                               ; check for allocation
                 beq l245_1                               ; branch if not allocated

                 phy                                      ; mark current DS$ string as garbage
                 phx
; lda #40   ;   standard DS$ allocation
                 tay
                 ldx #dsdesc+1
                 jsr sta_far_ram1                         ; sta (dsdesc+1),y length of garbage
                 iny
                 lda #$ff
                 jsr sta_far_ram1                         ; sta (dsdesc+1),y garbage flagged
                 inc
                 sta dsdesc                               ; (0)    kill DS$
                 plx
                 ply

l245_1           rts


; Read DOS error message, but don't care what it is.  Want to stop disk LED blink.
;

Suck_DS
                 ldx dosfa                                ; fa
                 lda #doslfn                              ; la (reserved la)
                 ldy #$6f                                 ; sa (command channel)
                 jsr _setlfs
                 lda #0                                   ; no name (so no setbank)
                 jsr _setnam
                 jsr _open                                ; get command channel
                 ldx #doslfn
                 jsr _chkin
                 bcs l246_2                               ; skip input if problem

l246_1           jsr _basin                               ; read disk error message
                 cmp #cr
                 bne l246_1                               ; loop until eol

l246_2           jsr _clrch                               ; shut down command channel
                 lda #doslfn
                 sec                                      ; not a real close
                 jmp _close                               ; close it


; R-U-sure subroutine

are_you_sure

                 bbs7 runmod,response_fake                ; branch if not direct mode

                 jsr _primm                               ; else prompt user for y/n answer
                 !text "ARE YOU SURE? ", 0


response_get
                 jsr _clrch                               ; clear channel for basin
                 jsr _basin                               ; next char
                 pha                                      ; save first char of reply

l247_1           cmp #cr                                  ; eat chars until end of line
                 beq l247_2                               ; if cr received, exit
                 jsr _basin
                 bne l247_1                               ; continue to ignore

l247_2           jsr _bsout                               ; new line     [910212] FAB
                 pla
                 cmp #'Y'                                 ; z set means ans=y.....
                 rts


response_fake
                 lda #0                                   ; ...or not in direct mode
                 rts

;.end



;*****************************************************************
;  OPTWRD - get an optional, unsigned 2-byte value in y,a.
;
;      case 1 : pointer at end of line:
;   return a=y=0, clear c to flag 'default'
;      case 2 : pointer is at comma, next non-blank is also a comma:
;   return a=y=0, clear c to flag 'default'
;      case 3 : pointer is at comma, next non-blank is not a comma:
;   get word in y,a, set c to flag 'non-default'
;*****************************************************************

optwrd           jsr chrgot
                 beq l248_1
                 jsr chkcom
                 cmp #','
                 beq l248_1
                 jsr getwrd
                 sec
                 rts

l248_1           lda #0
                 tay

optw99           clc
                 rts


comsad           jsr chkcom                               ; get a comma & signed 2-byte arg in y,a   [910307]
                 +lbra sadwrd



optsad           jsr chrgot                               ; get a comma & optional, signed 2-byte arg in y,a [910307]
                 beq l249_1                               ; eol, therefore this arg is not specified
                 jsr chkcom                               ; eat comma
                 cmp #','                                 ; is next a comma too?
                 beq l249_1                               ; yes, therefore this arg is not specified
                 jsr sadwrd                               ; get signed word
                 sec
                 rts

l249_1           lda #0                                   ; default optional arg to zero
                 tay
                 clc
                 rts


;*****************************************************************
;  OPTBYT - get an optional 1 byte value in x.
;
;  Enter with default value in x.
;
;      case 1 : pointer at end of line:
;   return default x.
;      case 2 : pointer is at comma, next non-blank is also a comma:
;   return default x.
;      case 3 : pointer is at comma, next non-blank is not a comma:
;   get byte in x.
;*****************************************************************

optzer           ldx #0                                   ; optional byte, with default=0

optbyt           jsr chrgot
                 beq optw99                               ; EOL: clc/rts
                 jsr chkcom
                 cmp #','
                 beq optw99                               ; Comma: clc/rts
                 jsr getbyt
                 sec
                 rts


prtdec
                 phy
                 phx
                 tax                                      ; prints decimal value of chr in .a
                 lda #0
                 jsr linprt
                 plx
                 ply
                 rts



retpat                                                    ; f.bowen
                 dey                                      ; [910828]
                 lda (fndpnt),y                           ; restore pointers
                 sta txtptr+1
                 dey
                 lda (fndpnt),y
                 sta txtptr
                 dey
                 lda (fndpnt),y

                 sta curlin+1                             ; fixes a problem when RETURNing to a GOSUB in direct mode
                 dey                                      ; or LOOPing to a DO in direct mode. 'curlin+1' must not be
                 tax                                      ; restored to $ff without also resetting 'runmod'
                 inx
                 bne l250_1                               ; branch if GOSUB or DO was from a program
                 lda #%11000000
                 trb runmod                               ; else force return to direct mode

l250_1           lda (fndpnt),y
                 sta curlin
                 rts


vbits            !text $01,$02,$04,$01,$02,$04            ; for stereo filter, volume bit setting
sbits            !text $01,$02,$04,$08,$10,$20,$40,$80
rbits            !text $80,$40,$20,$10,$08,$04,$02,$01

;.end
;[[handler.irq]]



basic_irq
; lda _vicIRQ  ;a VIC raster interrupt?
; and #%10000001  ; (used to update moving sprites & sound stuff)
; cmp #%10000001
; bne collision_irq ; no, go check other VIC interrupts

                 lda irq_wrap_flag                        ; filter out wrapped IRQ calls (allows interruptable code)
                 beq l251_1                               ; it's ok
                 rts                                      ; exit- we're already handling one interrupt

l251_1           inc irq_wrap_flag                        ; shut the door
                 cli                                      ; but leave the window open


; Test if there was a VIC collision/light pen interrupt

collision_irq
; sei
                 lda _vicIRQ                              ; check VIC IRQ flags
                 and #%00001110                           ; mask all but lp, s/s, and s/bgnd flags
                 beq l252_5                               ; exit if none set
                 trb _vicIRQ                              ; else reset flags we're going to handle
                 lsr                                      ; shift out raster interrupt bit (not used)

; Test for 3 types of collision interrupts : sprite/sprite, sprite/bgnd, & light pen

                 ldy #1                                   ; loop for sprite/bgnd and sprite/sprite collision check
l252_1           lsr
                 bcc l252_4                               ; bit not set ==> not source of interrupt

                 pha
                 lda vic+30,y                             ; accumulate collision data (resets register)
                 ora collisions,y
                 sta collisions,y

                 lda intval                               ; allowable interrupts
                 cpy #0                                   ; examine selected bit
                 beq l252_2
                 lsr
l252_2           lsr
                 bcc l252_3                               ; BASIC doesn't want this interrupt
                 lda #$ff
                 sta int_trip_flag,y                      ; turn on trip flag

l252_3           pla

l252_4           dey
                 bpl l252_1


; Check light pen latch

                 lsr
                 bcc l252_5                               ; LightPen latch not valid

                 ldx vic+49                               ; 4567R7 bug- must read LP_latches in Slow mode????
                 lda #%01000000                           ; [910618]
                 trb vic+49
                 ldy vic+19                               ; save latched x position
                 sty lightpen_xpos
                 ldy vic+20                               ; save latched y position
                 sty lightpen_ypos
                 stx vic+49                               ; restore speed     [910618]

                 lda intval                               ; is BASIC interested in our little find?
                 and #4
                 beq l252_5                               ; no, move on to next IRQ task
                 lda #$ff
                 sta int_trip_flag+2                      ; yes- let BASIC know we caught one

l252_5

; Update moving sprites

movspr_irq
                 lda vic+21                               ; any sprites active?    [910212]
                 +lbeq music_irq                          ; no- skip ahead

                 ldy #7                                   ; check each of 8 sprites
l253_1           lda vic+21                               ; is this sprite is enabled?
                 and sbits,y
                 beq l253_5                               ; sprite not enabled

                 ldx sproff,y                             ; get offset to sprite info from a table
                 lda sprite_data,x                        ; is this sprite moving (speed >0 )?
                 beq l253_5                               ; sprite not moving
                 bpl l253_2                               ; sprite moving, no destination
                 bsr movspr_to_irq                        ; sprite moving to a destination [910809]
                 bra l253_5

l253_2           sta sprite_data+1,x                      ; set counter
l253_3           tya                                      ; convert sprite# to a VIC register pointer
                 asl
                 tay
                 lda sprite_data+2,x                      ; get angle sign
                 dec                                      ; subtract 1 for cosine
                 inx
                 inx
                 iny
                 jsr sprsub                               ; update y position
                 dex
                 dex
                 dey
                 lda sprite_data+2,x
                 jsr sprsub                               ; update x position
                 php
                 tya
                 lsr                                      ; restore index (.Y=sprite pointer)
                 tay
                 plp
                 bcc l253_4                               ; skip if no overflow
                 lda vic+16                               ; get x position msb bits ???vic_save
                 eor sbits,y                              ; invert bit
                 sta vic+16                               ; ???vic_save
l253_4           dec sprite_data+1,x
                 bne l253_3                               ; loop until counter done

l253_5           dey                                      ; check next sprite
                 bpl l253_1                               ; loop until done moving all sprites
                 +lbra music_irq                          ; then continue with next IRQ task

movspr_to_irq                                             ; [910809]
                 phy                                      ; sprite #
                 and #$3f                                 ; speed factor
                 taz
                 tya                                      ; vic sprite index
                 asl
                 tay

l254_1           sec                                      ; for i = 1 to abs(greatr)
                 lda sprite_data+1,x
                 sbc #1
                 sta sprite_data+1,x
                 bcs l254_2
                 lda sprite_data+2,x
                 sbc #0
                 sta sprite_data+2,x
                 bcs l254_2
                 lda #0
                 sta sprite_data,x                        ; done!  sprite is at its destination
                 ply                                      ; remember sprite #
                 rts

l254_2           lda sprite_data+3,x                      ; ptr(lesser)
                 bit sprite_data+10,x
                 bmi l254_3                               ; if e > 0
                 bit sprite_data+3,x                      ; sgn(lesser) (b7=1=neg, b6=1=pos, else 0)
                 jsr drwinc                               ; pos(lesser) = pos(lesser) + sgn(lesser)

                 lda sprite_data+4,x                      ; ptr(greater)
l254_3           lsr                                      ; which f?
                 bcs l254_4
                 lda sprite_data+9,x                      ; e = e + f1
                 adc sprite_data+5,x
                 sta sprite_data+9,x
                 lda sprite_data+10,x
                 adc sprite_data+6,x
                 sta sprite_data+10,x
                 bra l254_5

l254_4           clc
                 lda sprite_data+9,x                      ; e = e + f2
                 adc sprite_data+7,x
                 sta sprite_data+9,x
                 lda sprite_data+10,x
                 adc sprite_data+8,x
                 sta sprite_data+10,x

l254_5           lda sprite_data+4,x                      ; ptr(greater)
                 bit sprite_data+4,x                      ; sgn(greater) (b7=1=neg, b6=1=pos, else 0)
                 jsr drwinc                               ; pos(greater) = pos(greater) + sgn(greater)

                 dez                                      ; count
                 bne l254_1
                 ply                                      ; remember sprite #
                 rts                                      ; done this frame


drwinc           php
                 and #1                                   ; adjust .y for x or y position
                 beq l255_1                               ; 0=x
                 iny                                      ; 1=y
l255_1           plp
                 bmi l255_2                               ; enter with b7=negative, b6=positive, else zero
                 bvc l255_4

                 lda vic,y                                ; positive direction
                 inc
                 sta vic,y
                 bra l255_3

l255_2           lda vic,y                                ; negative direction
                 dec
                 sta vic,y
                 cmp #$ff

l255_3           bne l255_4                               ; no wrap
                 tya
                 bit #1
                 bne l255_4                               ; wrap in y okay
                 lsr
                 tay
                 lda sbits,y                              ; wrap in x- toggle msb
                 eor vic+16
                 sta vic+16
                 tya
                 asl
                 tay

l255_4           tya                                      ; restore y to sprite offset
                 and #$fe
                 tay
                 rts


; Play music, if in progress

music_irq
                 ldx #0
l256_1           ldy voices+1,x
                 bmi l256_2                               ; skip if not active

                 lda voices,x
                 sec
                 sbc tempo_rate                           ; decrement current value by current tempo
                 sta voices,x
                 bcs l256_2
                 tya                                      ; lda voices+1,x
                 sbc #0
                 sta voices+1,x
                 bcs l256_2                               ; ok, no underflow

                 txa
                 lsr                                      ; get offset to waveform
                 tay
                 lda waveform,y                           ; get waveform
                 and #$fe                                 ; mask out gate bit
                 pha
                 lda SID_offset,y                         ; get offset to correct oscillator
                 tay
                 pla
; jsr go_slow  ;      [910716] 4567R7A
                 sta sid1+4,y                             ; turn off sound
; jsr go_fast  ;      [910716] 4567R7A

l256_2           inx
                 inx
                 cpx #6+6                                 ; [910612]
                 bcc l256_1                               ; loop for 6 voices
;then continue with next IRQ task

; Test if SOUND command wants anything

sound_irq
                 ldy #6-1                                 ; test six voices    [910612]
l257_1           lda sound_time_hi,y                      ; active if msb clear
                 bpl l257_3
l257_2           dey
                 bpl l257_1
                 +lbra basic_irq_end

l257_3           clc                                      ; add step to frequency
                 lda sound_freq_lo,y
                 adc sound_step_lo,y
                 sta sound_freq_lo,y
                 lda sound_freq_hi,y
                 adc sound_step_hi,y
                 sta sound_freq_hi,y

                 lda sound_direction,y                    ; test if this is up or down
                 tax
                 and #1
                 beq l257_6                               ; branch if up

; If step direction is down, .C==0 OR freq < min  ==> reset value

                 bcc l257_4                               ; underflow, reset
                 sec
                 lda sound_freq_lo,y
                 sbc sound_min_lo,y
                 lda sound_freq_hi,y
                 sbc sound_min_hi,y
                 bcs l257_9                               ; no borrow, don't reset

l257_4           cpx #2                                   ; is 'cycle' bit set?
                 bcc l257_5                               ; no, keep direction 'down'

                 jsr negate_step                          ; make step 2's comp
                 lda #2                                   ; change direction to 'up'
                 sta sound_direction,y
                 bne l257_8                               ; go reset for 'up'

l257_5           lda sound_max_lo,y                       ; reset to max
                 sta sound_freq_lo,y
                 lda sound_max_hi,y
                 sta sound_freq_hi,y
                 bra l257_9                               ; go update SID frequency

; If step direction is up, overflow (.C==1) OR freq > max ==> reset frequency

l257_6           bcs l257_7                               ; overflow, must reset
                 lda sound_freq_hi,y                      ; 16 bit compare (yech!)
                 cmp sound_max_hi,y
                 bcc l257_9                               ; freq < max, no reset
                 bne l257_7                               ; freq > max, reset
                 lda sound_freq_lo,y                      ; msb's the same, test lsb's
                 cmp sound_max_lo,y
                 bcc l257_9                               ; freq < max, no reset
                 beq l257_9                               ; freq = max, no reset

l257_7           cpx #2                                   ; is this 'cycle'?
                 bcc l257_8                               ; no, go reset for next 'up'

                 jsr negate_step                          ; make step 2's comp
                 lda #3                                   ; change direction to 'down'
                 sta sound_direction,y
                 bne l257_5                               ; go reset for next 'down'

l257_8           lda sound_min_lo,y                       ; set freq to minimum value
                 sta sound_freq_lo,y
                 lda sound_min_hi,y
                 sta sound_freq_hi,y

; Update SID frequency registers

l257_9
; jsr go_slow  ;      [910716] 4567R7A
                 ldx SID_offset,y                         ; get index to SID voices
                 lda sound_freq_lo,y
                 sta sid1,x
                 lda sound_freq_hi,y
                 sta sid1+1,x
; jsr go_fast  ;      [910716] 4567R7A

; Decrement total time - see if it's time to bring down the curtain

                 tya
                 tax
                 lda sound_time_lo,x                      ; 16 bit decrement - not very pretty
                 bne l257_10
                 dec sound_time_hi,x
l257_10          dec sound_time_lo,x

                 lda sound_time_hi,x                      ; underflow?
                 +lbpl l257_2                             ; nope

; Time to turn off this voice

; jsr go_slow  ;      [910716] 4567R7A
                 lda #$08
                 ldx SID_offset,y
                 sta sid1+4,x
; jsr go_fast  ;      [910716] 4567R7A
                 +lbra l257_2


negate_step
                 lda sound_step_lo,y
                 eor #$ff
                 clc
                 adc #1
                 sta sound_step_lo,y
                 lda sound_step_hi,y
                 eor #$ff
                 adc #0
                 sta sound_step_hi,y
                 rts



; Here is where BASIC_IRQ exits

basic_irq_end
                 dec irq_wrap_flag                        ; open the door to IRQ
                 cli
                 rts


; Update sprite position subroutine

sprsub           pha                                      ; save angle phase
                 clc
                 lda sprite_data+3,x                      ; add low bytes
                 adc sprite_data+7,x
                 sta sprite_data+7,x
                 lda sprite_data+4,x                      ; add high bytes
                 adc sprite_data+8,x
                 sta sprite_data+8,x
                 pla                                      ; get angle sign
                 bcc l258_3                               ; skip if no carry - do not update position
                 lsr
                 lsr                                      ; test if positive or negative
                 lda vic,y                                ; ???vic_save
                 bcs l258_1                               ; skip if negative
                 adc #1                                   ; increment position
                 bra l258_2

l258_1           sbc #1                                   ; decrement position
                 cmp #$ff                                 ; set carry if underflow
l258_2           sta vic,y                                ; decrement position  ???vic_save
l258_3           rts

;.end
;[[command.mouse]]



;***********************************************************************
;*   MOUSE  ON  [,[port] [,[sprite] [,[hotspot] [,X/Yposition] ]]]
;*   MOUSE  OFF
;*    where: port     = (1...3) for joyport 1, 2, or either (both)
;*  sprite   = (0...7) sprite pointer
;*  hotspot  = x,y offset in sprite, default 0,0
;*  position = normal, relative, or angluar coordinates
;*
;*      (defaults to sprite 0, port 2, last hotspot & position)
;***********************************************************************

mouse            cmp #on_token                            ; new [910122]
                 beq l259_1
                 jsr chkesc
                 cmp #off_token
                 +lbne snerr

;    The Kernel MOUSE_CMD is called to install or remove mouse driver.
; .a= B7,6 set to install mouse in game port 2 ($80), 1 ($40), or both ($C0)
; .a= 0 to disable mouse driver
; .x= 0-7 physical sprite pointer

                 lda #0                                   ; TURN MOUSE OFF
                 jsr _mouse                               ; do it
                 +lbra chkeos                             ; eat token & exit after checking for eos

;TURN MOUSE ON
l259_1           jsr chrget                               ; eat token
                 ldx #2                                   ; get (optional) port# in .X
                 jsr optbyt                               ; if not present default to port 2
                 cpx #4                                   ;
                 +lbcs fcerr                              ; illegal value
                 phx

                 ldx #0                                   ; get (optional) sprite# in .X
                 jsr optbyt                               ; if not present default to sprite 0
                 cpx #8
                 +lbcs fcerr                              ; illegal value
                 stx z_p_temp_1
                 ldy sproff,x                             ; kill moving sprite
                 lda #0                                   ; get offset to speed data
                 sta sprite_data,y                        ; reset sprite's speed value

                 pla                                      ; setup for Kernel call- get port# into b7,6
                 ror                                      ; .a= port(s), .x=sprite
                 ror
                 ror
                 jsr _mouse                               ; do it (???? do after coord error check)


                 jsr optbyt                               ; get (optional) hotspot, x  new [910307]
                 bcc l259_2                               ; not given
                 cpx #24
                 +lbcs fcerr                              ; out of range (0-23)
                 txa
                 neg
                 tax
                 adc #24
                 sta _mouse_left
                 txa
                 clc
                 adc #87
                 sta _mouse_right

l259_2           jsr optbyt                               ; get (optional) hotspot, y
                 bcc l259_3                               ; not given
                 cpx #21
                 +lbcs fcerr                              ; out of range (0-20)
                 txa
                 neg
                 tax
                 adc #50
                 sta _mouse_top
                 txa
                 clc
                 adc #250
                 sta _mouse_bottom

l259_3           jsr chrgot                               ; get (optional) position coordinate  [910123]
                 beq l259_4                               ; eol, use this sprite's last position
                 jsr sprcor                               ; else get first coordinate
                 bit numcnt                               ; test coordinate type
                 +lbvs snerr                              ; syntax error
                 sty xdest                                ; save coordinate value
                 sty xdest+2
                 sta xdest+1
                 sta xdest+3

                 lda #$7f                                 ; flag 'mouse' for movspr call  [910808]
                 sta op
                 jsr sprcor                               ; get second coordinate
                 bit numcnt                               ; test type of coordinate
                 +lbvc movspr_normal                      ; position sprite, normal coordinates
                 +lbmi movspr_angle                       ; angular coordinates
                 +lbra snerr                              ; else error

l259_4           rts

;.end
;[[function.rmouse]]


;************************************************************************
;*   RMOUSE Returns in variable list current status of mouse *
;*         *
;*   Syntax: RMOUSE [Xposition [,Yposition [, Buttons] ]]  *
;*         *
;*   Where: X,Yposition = current position of mouse pointer sprite *
;*  Button      = current status of mouse buttons  *
;*         *
;*   0   = no button     *
;*   1   = right button    *
;*   128 = left button    *
;*   129 = both buttons    *
;*         *
;* If a mouse is not installed, "-1" is returned for all vars. *
;* If both ports are enabled, buttons from each port are merged. *
;************************************************************************

rmouse           lda #0                                   ; Init
                 sta count                                ; variable count = 0
                 dec
                 ldx #6-1
l260_1           sta grapnt,x                             ; positions/buttons = -1
                 dex
                 bpl l260_1

                 lda _mouse_enable                        ; Is there a mouse in the house?
                 and #%11000000
                 beq l260_5                               ; no, exit
                 pha                                      ; yes, save port assigns for later
                 sei
                 ldy _mouse_pointer                       ; Where is it?  Get pointer to sprite
                 lda vic,y                                ; Get X position    ???vic_save
                 sta grapnt                               ; lsb
                 lda sbits,y
                 and vic+16                               ; msb    ???vic_save
                 beq l260_2
                 lda #1                                   ; convert to 0 or 1
l260_2           sta grapnt+1
                 iny                                      ; Get Y position
                 lda vic,y                                ; lsb    ???vic_save
                 sta grapnt+2
                 lda #0                                   ; msb (fake it)
                 sta grapnt+3

                 sta grapnt+4                             ; Init button status
                 sta grapnt+5
                 ldz d1pra                                ; Set up port & read buttons
                 lda #$ff                                 ; save kybd output lines (IRQ already disabled)
                 sta d1pra                                ; set to not read any kybd inputs

                 ldy #0                                   ; which port?
                 plx                                      ; recall port assignments
l260_3           txa
                 asl                                      ; .c=1 if this one
                 tax
                 bcc l260_4                               ; not this one
                 lda d1pra,y                              ; read it (logical port is opposite physical port)
                 and #%00010001                           ; want left, right buttons only
                 eor #%00010001                           ; (invert, since low means button down)
                 tsb grapnt+4
                 and #%00010000                           ; shift left button to msb
                 beq l260_4
                 smb7 grapnt+4
l260_4           iny                                      ; next port
                 cpy #2
                 bcc l260_3

                 lda #%01111110                           ; clean up
                 trb grapnt+4                             ; fix button register
                 stz d1pra                                ; restore port for Kernel
                 cli

; At this point, we have snapshot the current mouse status.
; Now pass requested info along in a manner very similar to RREG...

l260_5           jsr chrgot                               ; Get a variable name from variable list
                 beq l260_8                               ; eol- exit
                 cmp #','                                 ;
                 beq l260_7                               ; null- skip this arg
                 jsr ptrget                               ; Get pointer to target variable
                 sta forpnt                               ; set up so we can share LET code
                 sty forpnt+1
                 lda valtyp                               ; what kind of variable name did ptrget find?
                 +lbne chkerr                             ; string- type mismatch error

l260_6           ldx count                                ; Make assignment
                 ldy grapnt,x                             ; low byte
                 lda grapnt+1,x                           ; high byte
                 jsr givayf                               ; float it
                 lda intflg                               ; set flags for type of var (int/float)
                 jsr qintgr                               ; use part of LET to do the work

l260_7           inc count                                ; Next assignment
                 inc count
                 ldx count
                 cpx #6                                   ; there are 3 possible
                 bcs l260_8                               ; done all 3, exit
                 jsr chrgot                               ; check terminator
                 beq l260_8                               ; eol- exit
                 jsr chkcom                               ; check delimiter
                 bra l260_5                               ; loop until done

l260_8           rts

;.end
;[[command.cursor]]



;*****************************************************************
;*   CURSOR [ON|OFF,] [column] [,row [,style] ]
;*
;*   where: column,row = x,y logical screen position
;*  style      = flashing (0) or solid (1)
;*  ON,OFF     = to turn the cursor on or off
;*****************************************************************

cursor           cmp #on_token                            ; Check for ON | OFF
                 clc
                 beq l261_3                               ; turn cursor on (.c=0)
                 cmp #esc_command_token
                 bne l261_1                               ; (might be a function)
                 jsr chkesc
                 cmp #off_token                           ; turn cursor off (.c=1)
                 beq l261_3
                 +lbra snerr

l261_1           pha                                      ; Evaluate cursor position parameters
                 sec
                 jsr _plot                                ; get current cursor position & save it
                 stx srow
                 sty column

                 ldx column                               ; get new column, default=current column
                 pla
                 cmp #','
                 beq l261_2                               ; not given, use default
                 jsr getbyt
l261_2           stx column
                 ldx srow                                 ; get new row, default=current row
                 jsr optbyt
; stx srow
                 ldy column
                 clc
                 jsr _plot                                ; set new cursor position
                 +lbcs fcerr                              ; error if bad position

                 jsr optzer                               ; Get new cursor type   ???? assumes screen output
                 bcc l261_4                               ; not given, exit
                 lda #esc
                 jsr _bsout                               ; use escape sequence to set
                 txa
                 and #1
                 eor #1                                   ; [910808]
                 clc
                 adc #'E'                                 ; 0=F=flash, 1=E=solid
                 jmp _bsout                               ; set it and exit

l261_3           jsr _cursor                              ; Turn cursor ON or OFF per .c

                 jsr chrget                               ; eat token, get next character
                 beq l261_4                               ; eol- exit
                 jsr chkcom                               ; else, must be comma
                 bra l261_1                               ; it is- go evaluate position

l261_4           rts                                      ; eol

;[[function.rcursor]]


;************************************************************************
;*   RCURSOR Returns in variable list current cursor position *
;*         *
;*   Syntax: RCURSOR [column [,row] ]    *
;************************************************************************

rcursor          sec                                      ; new [910228]
                 jsr _plot                                ; get current cursor position & save it
                 stx srow
                 sty column

                 ldx #0                                   ; just like RREG and RMOUSE...
                 stx count
l262_1           jsr chrgot                               ; Get a variable name from variable list
                 beq l262_4                               ; eol- exit
                 cmp #','                                 ;
                 beq l262_3                               ; null- skip this arg
                 jsr ptrget                               ; Get pointer to target variable
                 sta forpnt                               ; set up so we can share LET code
                 sty forpnt+1
                 lda valtyp                               ; what kind of variable name did ptrget find?
                 +lbne chkerr                             ; string- type mismatch error

l262_2           ldx count                                ; Make assignment
                 ldy column,x                             ; low byte
                 lda #0                                   ; high byte
                 jsr givayf                               ; float it
                 lda intflg                               ; set flags for type of var (int/float)
                 jsr qintgr                               ; use part of LET to do the work

l262_3           inc count                                ; Next assignment
                 ldx count
                 cpx #2                                   ; there are 2 possible
                 bcs l262_4                               ; done 2, exit
                 jsr chrgot                               ; check terminator
                 beq l262_4                               ; eol- exit
                 jsr chkcom                               ; check delimiter
                 bra l262_1                               ; loop until done

l262_4           rts

;.end



AutoScroll
                 pha                                      ; save character for Editor
                 bbs7 runmod,AutoScrollno                 ; branch if not direct mode
                 ldy channl                               ; is output redirected?
                 bne AutoScrollno                         ; yes- can't do scroll (need to read screen)
                 lda txttab
                 ldx txttab+1                             ; is there a program in memory to scroll?
                 sta txtptr
                 stx txtptr+1
                 iny                                      ; (1)
                 jsr indtxt
                 bne AutoScrollyes                        ; yes- continue
                 bra AutoScrollno                         ; no-  exit

AutoScrollpop
                 pla
                 pla
AutoScrollng
                 ldx point                                ; restore cursor position
                 ldy point+1
                 clc
                 jsr _plot
AutoScrollno
                 rmb1 helper                              ; remove LINGET flag
                 pla                                      ; restore character
                 sec                                      ; return to Editor with no action taken
                 rts

AutoScrollyes
                 ror form                                 ; save .c=direction (character already on stack)
                 sec
                 jsr _plot                                ; get current cursor position & save it
                 stx point
                 sty point+1
                 smb1 helper                              ; set flag for LINGET not to go to error if it has problems
                 bbs7 form,AutoScrolldn                   ; branch according to direction of scroll...


AutoScrollup                                              ; wanting to scroll up
                 sec
                 lda _screen_bottom                       ; put cursor at bottom of screen
                 sbc _screen_top
                 sta form+1                               ; save where it is- we'll be printing line there
                 tax
                 jsr AutoSearch                           ; search for a line number on screen, put it in linnum
                 jsr FindLine                             ; find the line in program
                 bcc l263_1   ;  line not found           ; we have a pointer to the next line
                 ldy #0
                 jsr indlow                               ; find the next line, the one we want to print, via link bytes
                 tax
                 iny
                 jsr indlow
                 stx lowtr                                ; advance pointer to it
                 sta lowtr+1
l263_1           ldx form+1                               ; put cursor back at bottom of screen
                 ldy #0
                 clc
                 jsr _plot
l263_2           jsr crdo                                 ; get a blank line to print on- scroll screen up
                 ldy #1
                 jsr indlow                               ; end of program marker?
                 bne AutoScrollprint                      ; no-  print this line & exit
                 lda txttab                               ; yes- loop to start of program,
                 ldx txttab+1
                 sta lowtr
                 stx lowtr+1
                 jsr crdo                                 ; and add an extra newline
                 bra l263_2


AutoScrolldn                                              ; wanting to scroll down
                 ldx #0                                   ; put cursor at top of screen
                 jsr AutoSearch                           ; search for a line number on screen, put it in linnum
                 ldx #0                                   ; get a blank line to print on
                 ldy #0                                   ; put cursor at top of screen
                 clc
                 jsr _plot
l264_1           jsr _primm                               ; and scroll screen (kill any pending Editor modes, too)
                 !text esc,esc,esc,"W",0
                 jsr FindLine                             ; find the line in program whose number we found on screen
                 lda lowtr                                ; (does not matter if it or next higher line is found)
                 cmp txttab
                 bne l264_2
                 lda lowtr+1
                 cmp txttab+1
                 bne l264_2
                 lda #$ff                                 ; special case- it's the very first line, want to wrap to last line
                 sta linnum+1                             ; fake pointer to the last line,
                 jsr _primm                               ; scroll screen to insert extra space,
                 !text esc,"W",0
                 bra l264_1                               ; and go around again

l264_2           lda txttab                               ; start at beginning of program (txttab) and find the line which points at (lowtr)
                 ldx txttab+1
l264_3           sta index                                ; pointer to link bytes
                 stx index+1
                 ldy #1
                 jsr indin1                               ; get link bytes
                 tax
                 dey
                 jsr indin1
                 cpx lowtr+1                              ; do link bytes point at target line?
                 bne l264_3
                 cmp lowtr
                 bne l264_3                               ; no- use these link bytes to find next line

                 lda index                                ; yes- copy pointer
                 ldx index+1
                 sta lowtr
                 stx lowtr+1
; bra AutoScrollprint ; print the line & exit


AutoScrollprint
                 ldy #2                                   ; get line number to print
                 jsr indlow
                 tax
                 iny
                 jsr indlow
                 jsr p1line                               ; print the number & the line
; bra AutoScrolldone ;Normal exit

AutoScrolldone
                 jsr _primm                               ; kill special Editor modes
                 !text esc,esc,0
                 ldx point                                ; restore cursor position
                 ldy point+1
                 clc
                 jsr _plot
                 rmb1 helper                              ; remove LINGET flag
                 pla                                      ; restore character
                 clc                                      ; return to Editor, with flag we handled character
                 rts

AutoSearch
                 ldy #0                                   ; search for any line number on screen in leftmost column
                 clc
                 jsr _plot                                ; move to beginning of next line
; bcs AutoScrollpop ;  exit if no more lines
                 bcs l265_4                               ; no more lines- fake one   [910716]
                 sec
                 jsr _plot                                ; else check if wrapped line
                 bcs l265_1                               ; it's wrapped- move up one line
                 lda _pnt
                 adc _screen_left                         ; (.c=0)
                 sta txtptr                               ; copy screen address of logical line to txtptr
                 lda _pnt+1
                 adc #0
                 sta txtptr+1
                 ldy #0                                   ; get first character on this line in window
                 lda (txtptr),y
; jsr indtxt  ;    (I did not want to limit search to the first column,
                 cmp #'9'+1                               ; but it was way too slow searching the entire screen)
                 bcs l265_1                               ; it's not a number
                 cmp #'0'
                 bcs l265_3                               ; it's a digit 0-9, continue

l265_1           bbs7 form,l265_2                         ; not on this line- move to next line
                 dex                                      ; move up one line
                 !text $89
l265_2           inx                                      ; move down one line
                 bra AutoSearch                           ; loop until we find a numeric digit or run out of lines

l265_3           clc                                      ; found a digit, get entire number into linnum & rts
                 +lbra linget

l265_4           lda #$ff                                 ; no line found, fake end of program   [910716]
                 sta linnum+1
                 rts




;.end
;[[command.screen]]



                 * = $af00                                ; [911001]

;*****************************************************************
; SCREEN DEF      define a screen
; SCREEN SET  set draw, view screen
; SCREEN CLR  clear a screen
; SCREEN OPEN  open a screen
; SCREEN CLOSE  close a screen
;*****************************************************************

Screen
                 cmp #open_token                          ; else dispatch per secondary token...
                 +lbeq ScreenOpen
                 cmp #close_token
                 +lbeq ScreenClose
                 cmp #def_token
                 beq ScreenDef
                 cmp #clr_token
                 beq ScreenClr

                 jsr chkesc                               ; [910930]
; cmp #esc_command_token
; bne l266_1
; jsr chrget  ; get past escape token
                 cmp #set_token
                 beq ScreenSet
l266_1           +lbra snerr                              ; report syntax error


CheckGraphicMode
                 bit $1f4b                                ; Check draw screen allocation   [910711]
                 bmi NoGraphicArea
                 rts                                      ; ok


NoGraphicArea
                 ldx #errng                               ; bad- no graphic area????
                 +lbra error


RestoreTextScreen                                          ; [910404]
                 lda #$ff                                 ; [910930]
                 sta GKI__parm1                           ; leave drawscreen as is
                 sta GKI__parm2                           ; set viewscreen to text
                 jmp ($800e)                              ; kg65.screen

; lda vic+49  ;Check graphic screen allocation
; and #%00010000
; beq 99$   ; we're in text mode
;; bit $1f43
;; bmi NoGraphicArea
;
; sei
; lda #$80
; bit _mode  ;40/80 mode, 0=80 128=40
; bmi l267_1
;
; tsb vic+49  ; 80
; lda #1
; trb vic+22  ;  fix x-scroll register
; bra 99$
;
;l267_1 trb vic+49  ; 40
; lda #1
; tsb vic+22  ;  fix x-scroll register
;
;99$ cli
;; lda #0
;; sta _graphm  ;text mode????
; rts


;*****************************************************************
; SCNCLR  clear a text or graphic screen
;
;  Syntax : SCNCLR  [ColorReg]
;
; if [ColorReg] not specified, clears text screen
; else clears the graphic screen with given value.
;*****************************************************************

ScreenClr
                 jsr chrget                               ; eat token & fall into SCNCLR

scnclr
                 bne C65__screenclear                     ; have a parameter, go clear graphic screen

                 lda #$93
                 jmp _bsout                               ; no parameter, clear text screen
; rts



;*****************************************************************
;* SCREEN CLR  clear a graphic screen
;*
;*  Syntax : SCREEN CLR  color_reg#
;*
;*           parm1 = color reg#  0-255
;*****************************************************************

C65__screenclear
                 jsr getbyt                               ; get color register # (range 0-255)?????
;limit to range allowed by current screen def?
                 stx GKI__parm1
                 jsr CheckGraphicMode
                 jmp ($800c)                              ; bra screenclear


;*****************************************************************
;* SCREEN SET  specify draw & view screens
;*
;*  Syntax : SCREEN SET  [DrawScreen] [,ViewScreen]
;*
;*           parm1 = draw screen # 0-3, 255=don't change    [910711]
;*           parm2 = view screen # 0-3, 255=text
;*****************************************************************

ScreenSet
                 jsr chrget                               ; advance past token

C65__screen
; beq snerr  ;missing args??      [911017]
                 ldx #255                                 ; [911028]
                 cmp #','
                 beq l267_1                               ; options byte only

                 jsr getbyt                               ; get draw screen# in .x
; cpx #4   ;       [910711]
; bcs 20$   ;  out of range error???? (255=leave alone)  [910930]
l267_1           stx GKI__parm1

                 ldx $1f69                                ; current viewscreen     [911017]
                 jsr optbyt                               ; eat a comma, get view screen# in .x
; cpx #4   ;
;20$ bcs fcerr  ;  out of range error???? (255=text)   [910930]
                 stx GKI__parm2

                 jsr ($800e)                              ; kg65.screen
                 bcs NoGraphicArea
                 rts


;*****************************************************************
;* SCREEN DEF  define a graphic screen
;*
;*  Syntax : SCREEN DEF  screen#, width, height, depth
;*
;*           parm1 = screen#           0-3    [910711]
;*           parm2 = width             0=320, 1=640, 2=1280
;*           parm3 = height            0=200, 1=400
;*           parm4 = depth             1-8 bitplanes (2-256 colors)
;*****************************************************************

ScreenDef
                 jsr chrget                               ; advance past token

C65__screendef
                 jsr getbyt                               ; get screen number
                 cpx #4                                   ; range 0-3   [910711]
                 bcs l268_1
                 stx GKI__parm1                           ; screen#

                 jsr combyt                               ; get width
                 cpx #3                                   ; range 0-2 ???? 1280 mode ????
                 bcs l268_1
                 stx GKI__parm2                           ; width

                 jsr combyt                               ; get height
                 cpx #2                                   ; range 0-1
                 bcs l268_1
                 stx GKI__parm3                           ; height

                 jsr combyt                               ; get depth (# bitplanes)
                 dex                                      ; convert 1-8 to 0-7
                 cpx #8                                   ; range 0-7
l268_1           +lbcs fcerr                              ; illegal quantity error
                 stx GKI__parm4                           ; depth

                 jmp ($8006)                              ; bra screendef


;*****************************************************************
;* SCREEN OPEN  open a graphic screen for viewing or drawing
;*
;*  Syntax : SCREEN OPEN screen#
;*
;*           parm1 = screen#      0-3    [910711]
;*****************************************************************


ScreenOpen
                 jsr chrget                               ; advance past Open token

C65__screenopen
                 jsr getbyt                               ; get screen# in .x
                 cpx #4                                   ; range 0-3   [910711]
                 +lbcs fcerr                              ; branch if out of range

                 stx GKI__parm1
                 jmp ($8008)                              ; screenopen    [910826]

; bcs NoGraphicArea ; bad ???? let user catch via RGRAPHIC
; rts


;*****************************************************************
;* SCREEN CLOSE  close a graphic screen
;*
;*  Syntax : SCREEN CLOSE screen#
;*
;*           parm1 = screen#  0-3    [910711]
;*****************************************************************


ScreenClose
                 jsr chrget                               ; advance past Close token

C65__screenclose
                 jsr getbyt                               ; get screen#
                 cpx #4                                   ; range 0-3   [910711]
                 +lbcs fcerr                              ; branch if out of range
                 stx GKI__parm1

                 jmp ($800a)                              ; bra screenclose

;[[command.pen]]

;*****************************************************************
;* PEN
;*
;*  Syntax : PEN  Pen#, ColorReg
;*
;*           parm1 = pen#  0-2
;*           parm2 = color reg#  0-255
;*****************************************************************

C65__setpen
                 jsr getbyt                               ; get pen#
                 cpx #3                                   ; range 0-2
                 +lbcs fcerr                              ; branch if out of range
                 stx GKI__parm1

                 jsr combyt                               ; get color reg#
;???? error check for max color allowed
; for the current screen.
                 stx GKI__parm2

                 jmp ($8010)                              ; bra setpen

;[[command.dmode]]


;*****************************************************************
;* DMODE   Set Draw Mode
;*
;*  Syntax : DMODE  jam, complement, stencil, style, thickness
;*
;*         parm1 = jam           0-1
;*         parm2 = complement (XOR) 0-1
;*         parm3 = stencil       0-1  <<< not implemented  [911003]
;*         parm4 = style         0-3  <<< only 0-1 implemented [911003]
;*         parm5 = thickness     1-8  <<< not implemented  [911003]
;*******************************************************************

C65__setdmode
                 jsr getbyt                               ; jam mode
                 cpx #2
                 bcs l269_1
                 stx GKI__parm1

                 jsr combyt                               ; complement (xor) mode
                 cpx #2                                   ; (ignores jam mode if set)
                 bcs l269_1
                 stx GKI__parm2

                 jsr combyt                               ; stencil mode (not implemented)
                 cpx #2
                 bcs l269_1
                 stx GKI__parm3

                 jsr combyt                               ; style mode
                 cpx #4                                   ; 0=solid, 1=pattern, 2=tile (not implemented), 3=reserved
                 bcs l269_1
                 stx GKI__parm4

                 jsr combyt                               ; thickness mode (not implemented)
; dex   ; adjust to 0-7     [911003]
                 cpx #8+1
l269_1           +lbcs fcerr                              ; illegal quantity error
                 stx GKI__parm5

                 jmp ($8014)                              ; bra setdmode

;[[command.dpat]]


;*****************************************************************
;* DPAT   set draw pattern
;*
;*  Syntax : DPAT  type [, # bytes, byte1, byte2, byte3, byte4]
;*
;*           parm1 = type        0-63  <<< only 0-4 implemented [911003]
;*           parm2 = # bytes     1-4
;*           parm3 = byte1       0-255
;*           parm4 = byte2       0-255
;*           parm5 = byte3       0-255
;*           parm6 = byte4       0-255
;*****************************************************************

C65__setdpat
                 jsr getbyt                               ; get pattern type
                 cpx #4+1                                 ; 63+1       [911028]
l270_1           +lbcs fcerr                              ; if out of range
                 stx GKI__parm1
                 txa
                 bne l270_2                               ; if parm1 is 0 then get extra stuff

                 jsr combyt                               ; get number of bytes
                 cpx #5
                 bcs l270_1                               ; too many bytes
                 stx GKI__parm2
                 stx z_p_temp_1                           ; save for count

                 jsr combyt                               ; get byte 1
                 stx GKI__parm3
                 dec z_p_temp_1
                 beq l270_2
                 +lbmi fcerr                              ; too few bytes

                 jsr combyt                               ; get byte 2
                 stx GKI__parm4
                 dec z_p_temp_1
                 beq l270_2

                 jsr combyt                               ; get byte 3
                 stx GKI__parm5
                 dec z_p_temp_1
                 beq l270_2

                 jsr combyt                               ; get byte 4
                 stx GKI__parm6

l270_2           jmp ($8016)                              ; bra setdpat

;[[command.palette]]


;*****************************************************************
;* PALETTE   set palette colors
;*
;*  Syntax : PALETTE {screen|COLOR}, color_index, red, green, blue
;*           PALETTE RESTORE
;*
;*           parm1 = screen  0-3     [910711]
;*           parm2 = color_index 0-255
;*           parm3 = red           0-31 (b0-3 red, b4=fgbg)  [910520]
;*           parm4 = green         0-15
;*           parm5 = blue          0-15
;*****************************************************************

C65__setpalette
                 cmp #restore_token                       ; restore palette?
                 bne l271_1                               ; no
                 jsr chrget                               ; yes- advance past Restore token
                 jmp _palette_init

l271_1           cmp #color_token                         ; set physical color register?
                 bne l271_2                               ; no- set logical color register
                 sta GKI__parm1
                 jsr chrget                               ; yes- advance past Color token
                 jsr getbyt
                 bra l271_3

l271_2           jsr getbyt                               ; get screen#
                 cpx #4                                   ; [910711]
                 +lbcs fcerr
                 stx GKI__parm1

                 jsr combyt                               ; get color reg #
l271_3           stx GKI__parm2                           ; (GKI will check for out of range????)

set_palette
                 jsr combyt                               ; get red & fgbg
                 cpx #32                                  ; [910520]
                 +lbcs fcerr
                 stx GKI__parm3

                 jsr getcomnyb                            ; get green
; cpx #16
; bcs 10$
                 stx GKI__parm4

                 jsr getcomnyb                            ; get blue
; cpx #16
; bcs fcerr  ; illegal quantity error
                 stx GKI__parm5

                 lda GKI__parm1                           ; logical or physical color register?
                 bpl l272_1                               ; logical
                 ldx GKI__parm2
                 lda GKI__parm3                           ; physical
                 sta _red,x
                 lda GKI__parm4
                 sta _green,x
                 lda GKI__parm5
                 sta _blue,x
                 bra l272_2

l272_1           jsr ($8012)                              ; go set screen palette
                 +lbcs NoGraphicArea                      ; illegal screen# or color#  [910917]

l272_2           jsr optbyt                               ; get another color reg # ?
                 stx GKI__parm2
                 bcs set_palette                          ; yes- loop
                 rts

;[[command.line]]

;*****************************************************************
;* LINE  draw a dot, a line or a stick shape
;*
;*  Syntax : LINE  x0, y0 [,[x1] [,y1]]...
;*
;* parm1,2 = x0 (lo/hi)
;* parm3,4 = y0
;* parm5,6 = x1 (x1,y1)=(x0,y0) if not specified
;* parm7,8 = y1
;*****************************************************************

C65__line
                 cmp #input_token                         ; special check for 'line input#'  [910103]
                 +lbeq linputn                            ; yes
                 cmp #input_token+1                       ; special check for 'line input'
                 +lbeq linput                             ; yes

                 jsr CheckGraphicMode
                 jsr sadwrd                               ; get x0
                 sty GKI__parm1
                 sta GKI__parm2
                 sty GKI__parm5                           ; [910228]
                 sta GKI__parm6

                 jsr comsad                               ; get y0
                 sty GKI__parm3
                 sta GKI__parm4
                 sty GKI__parm7                           ; [910228]
                 sta GKI__parm8

                 jsr optsad                               ; get x1     [910228]
                 bcc l273_2                               ; use x0
l273_1           sty GKI__parm5
                 sta GKI__parm6

l273_2           jsr optsad                               ; get y1     [910228]
                 bcc l273_3                               ; use y0
                 sty GKI__parm7
                 sta GKI__parm8

l273_3           jsr ($8018)                              ; draw a line from x0,y0 to x1,y1

                 ldx #3
l273_4           lda GKI__parm5,x                         ; copy x1,y1 to x0,y0
                 sta GKI__parm1,x
                 dex
                 bpl l273_4

                 jsr optsad                               ; more?
                 bcs l273_1                               ; yes, continue
                 rts

;[[command.box]]

;*****************************************************************
;* BOX   draw a 4-sided figure
;*
;*  Syntax :  BOX x0,y0, x1,y1, x2,y2, x3,y3 [,solid flag]
;*
;* parm1,2   = x0  (lo/hi)
;* parm3,4   = y0
;* parm5,6   = x1
;* parm7,8   = y1
;* parm9,10  = x2
;* parm11,12 = y2
;* parm13,14 = x3
;* parm15,16 = y3
;* parm17    = solid flag
;*****************************************************************

C65__box
                 jsr CheckGraphicMode
                 jsr sadwrd                               ; get x0
                 sty GKI__parm1
                 sta GKI__parm2

                 jsr comsad                               ; get y0
                 sty GKI__parm3
                 sta GKI__parm4

                 jsr comsad                               ; get x1
                 sty GKI__parm5
                 sta GKI__parm6

                 jsr comsad                               ; get y1
                 sty GKI__parm7
                 sta GKI__parm8

                 jsr comsad                               ; get x2
                 sty GKI__parm9
                 sta GKI__parm10

                 jsr comsad                               ; get y2
                 sty GKI__parm11
                 sta GKI__parm12

                 jsr comsad                               ; get x3
                 sty GKI__parm13
                 sta GKI__parm14

                 jsr comsad                               ; get y3
                 sty GKI__parm15
                 sta GKI__parm16

                 jsr optzer                               ; get solid flag
                 stx GKI__parm17

                 jmp ($801a)                              ; bra box

;[[command.circle]]

;*****************************************************************
;* CIRCLE   draw a Circle
;*
;*  Syntax : CIRCLE  CenterX, CenterY, radius [,solid flag]
;*
;*         parm1 = center x lo
;*         parm2 = center x hi
;*         parm3 = center y lo
;*         parm4 = center y hi
;*         parm5 = radius lo
;*         parm6 = radius hi
;*         parm7 = solid flag 0=no, 1=yes
;*****************************************************************

C65__circle
                 jsr CheckGraphicMode
                 jsr sadwrd                               ; get center x
                 sty GKI__parm1
                 sta GKI__parm2

                 jsr comsad                               ; get center y
                 sty GKI__parm3
                 sta GKI__parm4

                 jsr comsad                               ; get radius
                 sty GKI__parm5
                 sta GKI__parm6

                 jsr optzer                               ; get solid flag
                 cpx #2
                 +lbcs fcerr
                 stx GKI__parm7

                 jmp ($801c)                              ; bra circle


;[[command.ellipse]]


;*****************************************************************
;* ELLIPSE   draw an Ellipse
;*
;*  Syntax : ELLIPSE  CenterX, CenterY, RadiusX,  RadiusY  [,solid flag]
;*
;*         parm1 = center x lo
;*         parm2 = center x hi
;*         parm3 = center y lo
;*         parm4 = center y hi
;*         parm5 = x radius lo
;*         parm6 = x radius hi
;*         parm7 = y radius lo
;*         parm8 = y radius hi
;*         parm9 = solid flag 0-1
;*****************************************************************

C65__ellipse
                 jsr CheckGraphicMode
                 jsr sadwrd                               ; get center x
                 sty GKI__parm1
                 sta GKI__parm2

                 jsr comsad                               ; get center y
                 sty GKI__parm3
                 sta GKI__parm4

                 jsr comsad                               ; get xradius
                 sty GKI__parm5
                 sta GKI__parm6

                 jsr comsad                               ; get yradius
                 sty GKI__parm7
                 sta GKI__parm8

                 jsr optzer                               ; get solid flag
                 cpx #2
                 +lbcs fcerr
                 stx GKI__parm9

                 jmp ($8020)                              ; bra ellipse

;[[command.polygon]]



;*****************************************************************
;* POLYGON   draw a regular n-sided Polygon
;*
;*  POLYGON  X,Y, Xradius, Yradius, sides [,drawsides [,subtend [,angle [,solid] ]]]
;*
;*         parm1 = center x lo
;*         parm2 = center x hi
;*         parm3 = center y lo
;*         parm4 = center y hi
;*         parm5 = xradius lo
;*         parm6 = xradius hi
;*         parm7 = yradius lo
;*         parm8 = yradius hi
;*         parm9 = solid flag 0-1
;*         parm10 = sa lo (starting angle 0-360)
;*         parm11 = sa hi
;*         parm12 = # of sides to draw (1 to 127)
;*         parm13 = # of sides (3 to parm12)
;*         parm14 = subtend flag 0-1
;****************************************************************

C65__polygon                                              ; changed BASIC syntax to something more reasonable [910923] FAB
                 jsr CheckGraphicMode
                 jsr sadwrd                               ; get center x
                 sty GKI__parm1
                 sta GKI__parm2

                 jsr comsad                               ; get center y
                 sty GKI__parm3
                 sta GKI__parm4

                 jsr comwrd                               ; get x radius
                 sty GKI__parm5
                 sta GKI__parm6

                 jsr comwrd                               ; get y radius
                 sty GKI__parm7
                 sta GKI__parm8

                 jsr combyt                               ; get number of sides
                 cpx #3
                 bcc l274_2                               ; too few
                 cpx #128
l274_1           +lbcs fcerr                              ; too many
                 stx GKI__parm13

; ldx GKI__parm13  ;get number of sides to draw (default=#sides)
                 jsr optbyt
                 cpx #1                                   ; must be at least 1 side
l274_2           +lbcc fcerr
                 stx GKI__parm12
                 dex
                 cpx GKI__parm13                          ; draw sides must be <= #sides
                 bcs l274_1

                 jsr optzer                               ; get subtend flag
; cpx #2
; bcs l274_1
                 stx GKI__parm14

                 jsr optwrd                               ; get starting angle (default=0 degrees)
                 sty GKI__parm10                          ; lo
                 sta GKI__parm11                          ; hi

                 jsr optzer                               ; get solid flag
; cpx #2
; bcs l274_1
                 stx GKI__parm9

                 jmp ($801e)                              ; bra polygon

;[[command.set]]



;  SET  A multipurpose command initiator


C65__set
                 cmp #verify_token                        ; SET VERIFY <ON | OFF>  new [910429]
                 +lbeq verify_mode
                 cmp #def_token                           ; SET DEF unit
                 bne l275_1
                 jsr getdisknum_1
                 stx _default_drive
                 stx dosfa                                ; Make last DOS device = current device
                 +lbra Clear_DS



l275_1           jsr chkesc                               ; Must be ESCape token
                 cmp #disk_token                          ; ok so far
                 +lbne bad_command                        ; unknown command




                 jsr getdisknum_1                         ; SET DISK # [<,|TO> #]
                 stx dosfa                                ; got current disk unit #

                 jsr chrgot                               ; check delimiter (comma, 'TO', or eos)
                 +lbeq Clear_DS                           ; eos- just change DOS' current drive [910417]
                 cmp #','                                 ; not eos, must be comma or 'TO'
                 beq l275_2
                 cmp #to_token
                 +lbne snerr

l275_2           jsr getdisknum_1                         ; skip delimiter
                 stx dosds2                               ; got new disk unit #



;  Open disk command channel & pass it 'renumber' command

                 jsr dclall                               ; Close any open files????

                 ldx #6-1
l275_3           lda disk_renum_cmd,x                     ; move command to RAM, setup for open
                 sta savram,x
                 dex
                 bpl l275_3
                 lda dosds2
                 ora #32                                  ; make new # a talk/listen address
                 sta savram+6
                 lda dosds2
                 ora #64
                 sta savram+7

                 lda #8                                   ; command string length
                 jsr SendDiskCmd                          ; Send command
                 lda dosds2
                 sta dosfa                                ; Make last DOS device = current device
                 +lbra close_out_1                        ; common error check & exit path ????


disk_renum_cmd   !text "M-W",119,0,2                      ; Renumber Drive command



;  GetDiskNum - Get a (required) disk number and check it

getdisknum_1
                 jsr chrget                               ; skip current character
getdisknum
                 jsr getbyt                               ; get number in .x
                 cpx #8                                   ; check range (8-30)
                 +lbcc fcerr
                 cpx #31
                 +lbcs fcerr
                 rts                                      ; returns only if okay



;  SendDiskCmd - Send command in SAVRAM to disk, length in .A

SendDiskCmd
; lda #   ; command string length
                 ldx #<savram                             ; address
                 ldy #>savram
                 jsr _setnam
                 ldx #sys_bank                            ; ???? sysbank ????
                 jsr _setbank
                 jsr _clrch                               ; Restore normal channels, establish our's
                 ldx dosfa                                ; fa
                 lda #doslfn                              ; la (reserved la)
                 ldy #$6f                                 ; sa (command channel)
                 jsr _setlfs
                 jsr _open                                ; open channel & send command
                 lda #doslfn                              ; close it already
                 sec                                      ; not a real close
                 jsr _close
                 +lbra Clear_DS                           ; Exit


;  SET VERIFY <ON | OFF> Set DOS verify-after-write mode for 3.5" drives

verify_mode
                 jsr chrget                               ; eat 'verify' token, get next  new [910429]
                 cmp #on_token
                 sec
                 beq l276_1                               ; turn verify on (.c=1)
                 jsr chkesc
                 cmp #off_token                           ; turn cursor off (.c=0)
                 +lbne snerr
                 clc

;  Open disk command channel & pass it 'verify' command

l276_1           php                                      ; Save mode
                 jsr chkeos                               ; eat 'on/off' token, error if not eos

                 ldx #4-1
l276_2           lda verify_cmd,x                         ; move command to RAM, setup for open
                 sta savram,x
                 dex
                 bpl l276_2

                 lda #0                                   ; form on/off flag
                 plp
                 rol
                 ora #$30
                 sta savram+4

                 lda #5                                   ; command string length
                 jsr SendDiskCmd                          ; Send command
                 +lbra close_out_1                        ; common error check & exit path ????


verify_cmd       !text "U0>V"                             ; Verify on/off command

;.end
;[[command.char]]



;*****************************************************************
;* CHAR   draw a character string
;*
;*  Syntax : CHAR column, row, height, width, direction, "string" [,charsetadr [,bank]]
;*
;* parm1  = column#
;* parm2  = row lo
;* parm3  = row hi
;* parm4  = height
;* parm5  = width
;* parm6  = direction
;* parm7  = len of string
;* parm8  = lo addr of string
;* parm9  = hi addr of string
;* parm10 = lo addr of character set $29800 default
;* parm11 = hi addr of character set
;* parm12 = bank of character set   [910912]
;****************************************************************

C65__char
                 jsr CheckGraphicMode
                 jsr getbyt                               ; get column
                 stx GKI__parm1

                 jsr comsad                               ; get row
                 sty GKI__parm2
                 sta GKI__parm3

                 jsr combyt                               ; get height
                 stx GKI__parm4

                 jsr combyt                               ; get width
                 stx GKI__parm5

                 jsr combyt                               ; get direction
                 stx GKI__parm6

                 jsr chkcom
                 jsr frmevl                               ; evaluate the string
                 jsr chkstr                               ; type mismatch error if not string
                 ldy #0
                 jsr indfmo                               ; pointer to string descriptor is left in the fac by frmevl
                 sta GKI__parm7                           ; length  ???? check for null string ????
                 pha
                 iny
                 jsr indfmo
                 sta GKI__parm8                           ; adrlo
                 iny
                 jsr indfmo
                 sta GKI__parm9                           ; adrhi
                 jsr frefac                               ; [910917]
                 pla
                 jsr getspa

                 jsr optwrd                               ; get charset address (????bank)
                 bcs l277_1                               ; given
                 ldy #<$9800                              ; not given- use ROM as default   [910207] FAB
                 lda #>$9800                              ; ???? uc/lc or graphic set ????
l277_1           sty GKI__parm10                          ; lo
                 sta GKI__parm11                          ; hi
                 ldx #2                                   ; default to ROM bank 2    [910912] FAB
                 jsr optbyt
                 stx GKI__parm12

                 lda GKI__parm7                           ; ???? check for null string ????
                 beq l277_2
                 jmp ($802c)                              ; bra kg65.char

l277_2           rts

;[[command.paint]]



;*****************************************************************
;* PAINT   fill a graphic area with color
;*
;*  Syntax : PAINT x, y [,mode [,color]]
;*
;* parm1  = x lo
;* parm2  = x lo
;* parm3  = y lo
;* parm4  = y hi
;* parm5  = mode
;* parm6  = color
;*
;* fill color is pen-A
; mode 0: fill region defined by color at x,y (default) new modes [910916] FAB
; mode 1: fill region using given color as boundary
; mode 2: fill connected region
;****************************************************************

C65__paint                                                ; new [910228] FAB
                 jsr CheckGraphicMode
                 jsr sadwrd                               ; get x
                 sty GKI__parm1
                 sta GKI__parm2

                 jsr comsad                               ; get y
                 sty GKI__parm3
                 sta GKI__parm4

                 ldx #0                                   ; [910916]
                 jsr optbyt                               ; mode, default = 0 (fill region pointed to)
                 cpx #3
                 +lbcs fcerr                              ; (range 0-2)
                 stx GKI__parm5
                 ldx #0
                 jsr optbyt                               ; boundary color, default = 0
                 stx GKI__parm6

l278_1           jsr garba2                               ; create space in var bank for paint stack [910716]
                 lda strend
                 sta GKI__parm7                           ; pass pointer to bottom of bank-1 free space
                 lda strend+1                             ; (top of stack)
                 sta GKI__parm8
                 sec
                 lda fretop                               ; pass pointer to top of free space
                 sbc #3                                   ; (bottom of stack)
                 sta GKI__parm9
                 lda fretop+1
                 sbc #0
                 sta GKI__parm10

                 jsr ($802e)                              ; bra paint
                 bcs l278_2                               ; error- stack overflow or stop key
                 rts

l278_2           cpx #errom
                 +lbeq error                              ; stack overflow, say 'out of memory'
                 +lbra break_exit                         ; user hit stop key

;[[command.loadiff]]



;*****************************************************************
;*   LOADIFF "filename" [,U#] [,D#]
;*
;* ???? requires SCREEN already opened   910402 FAB
;*****************************************************************

loadiff
                 jsr CheckGraphicMode
                 lda #$e6                                 ; parse:  filename [,U#] [,D#]
                 jsr dosprs                               ; (like dopen:  0 0 0 *  * 0 0 1 )
                 jsr chk1                                 ; check parameters
                 lda #0
                 sta dossa                                ; setup as dload would (0 = load channel)
                 jsr find_la                              ; find an available la to use (cannot use reserved one)
                 ldy #fopn
                 ldx #4
                 jsr open_file                            ; open the file
                 bcs l279_1                               ; exit if error

                 ldx dosla
; stx GKI__parm1
                 jsr _chkin                               ; get input channel
l279_1           +lbcs list_err                           ; exit if error

                 jsr ($802a)                              ; Load it

exit_GKI_disk_op
                 php                                      ; preserve completion status
                 pha
                 jsr _clrch
                 lda dosla
                 jsr close_out                            ; close channel

                 jsr is_stop_key_down                     ; weed out BREAK error
                 plx
                 plp
                 +lbcs error                              ; must be I/O or file data error
                 rts                                      ; load was successful

;[[command.saveiff]]

;*****************************************************************
;*
;*   SAVEIFF "[@]filename" [,U#] [,D#]      [910930] FAB
;*
;*****************************************************************

saveiff
                 jsr CheckGraphicMode
                 lda #$66                                 ; set error flags
                 jsr dosprs                               ; parse the line
                 jsr chk2                                 ; check required parameters
                 lda #1
                 sta dossa                                ; setup as dsave would (1 = save channel)
                 jsr find_la                              ; find an available la to use (cannot use reserved one)
                 ldy #fopn
                 ldx #4
                 jsr open_file                            ; open the file
                 bcs l280_1                               ; exit if error

                 ldx dosla
; stx GKI__parm1
                 jsr _chkout                              ; get output channel
l280_1           +lbcs list_err                           ; exit if error

                 jsr ($803a)                              ; Save it
                 bra exit_GKI_disk_op

; php   ;preserve completion status
; pha
; jsr _clrch
; lda dosla
; jsr close_out  ;close channel
;
; jsr is_stop_key_down ; weed out BREAK error
; plx
; plp
; bcs error  ; must be I/O or file data error
; rts   ; load was successful

;.end
;[[command.viewport]]



;*****************************************************************
;*   VIEWPORT <CLR | DEF>  x, y, viewport_width, viewport_height
;*
;* assumes SCREEN already opened   910626 FAB
;*****************************************************************

C65__Viewport
                 pha                                      ; save secondary command
                 jsr chrget                               ; advance past it
                 jsr CheckGraphicMode                     ; make sure a screen is open

                 jsr sadwrd                               ; get x0
                 sty GKI__parm1
                 sta GKI__parm2

                 jsr comsad                               ; get y0
                 sty GKI__parm3
                 sta GKI__parm4

                 jsr comsad                               ; get width (delta-x)
                 sty GKI__parm5
                 sta GKI__parm6

                 jsr comsad                               ; get height (delta-y)
                 sty GKI__parm7
                 sta GKI__parm8

                 pla                                      ; dispatch per secondary token...
                 cmp #clr_token
                 beq l281_1
                 cmp #def_token
                 +lbne snerr                              ; error

                 jmp ($8030)                              ; define viewport & return

l281_1           jmp ($8022)                              ; clear viewport (???? make this a box command)


C65__copy
C65__cut
C65__paste
                 jmp bad_command

;[[command.genlock]]



;*****************************************************************
;* GENLOCK  set/reset genlock mode & color registers
;*
;*  Syntax: GENLOCK <ON[,color#[,...]] | OFF[,color#,R,G,B]>
;*****************************************************************

genlock          sta GKI__parm1                           ; save token as flag for set palette   [910107]
                 cmp #on_token
                 beq l282_4
                 jsr chkesc
                 cmp #off_token
                 +lbne snerr
;TURN GENLOCK OFF
                 lda vic+49                               ; any interlaced bitplanes on?
                 and #%00011001
                 cmp #%00011001
                 beq l282_1                               ; yes, leave interlace mode on

                 lda #%00000001
                 trb vic+49                               ; no, turn interlace off
l282_1           lda #%00000010
                 trb vic+48                               ; reset external sync mode
; beq l282_2   ;       [910114]
; lda vic+63  ;       [910111]
; inc a   ;  adjust vert. position (chip bug ????)
; inc a   ;  (to be handled by a custom C65 genlock board)
; inc a
; sta vic+63

l282_2           jsr chrget                               ; eat token
                 jsr optbyt                               ; get (optional) color reg# in .X
                 stx GKI__parm2                           ; save it
                 +lbcs set_palette                        ; if present, go do it & exit
l282_3           rts                                      ; if not present (eol), exit


l282_4           lda #%00000001                           ; TURN GENLOCK ON
                 tsb vic+49                               ; set interlace mode
                 asl
                 tsb vic+48                               ; set external sync mode
; bne l282_5   ;       [910114]
; lda vic+63  ;       [910111]
; dec a   ;  adjust vert. position (chip bug ????)
; dec a   ;  (to be handled by a custom C65 genlock board)
; dec a
; sta vic+63

l282_5           jsr chrget                               ; eat token
l282_6           jsr optbyt                               ; get (optional) color reg# in .X
                 bcc l282_3                               ; if not present (eol), exit
                 lda #%00010000                           ; if present, set FGBG bit in red palette
                 sta _red,x
                 bra l282_6                               ; loop

;[[command.color]]




;*****************************************************************
;* COLOR       <ON | OFF> Enable|Disable SW & HW color
;* FOREGROUND  color# Set Foreground color (text)
;* HIGHLIGHT   color# Set Highlight color (text)
;* BACKGROUND  color# Set VIC Background color
;* BORDER      color# Set VIC Border color
;*****************************************************************

color            cmp #','                                 ; optional first arg
                 beq l283_3
                 cmp #on_token                            ; SOFTWARE (Editor) color mode
                 beq l283_2
                 jsr chkesc
                 cmp #off_token
l283_1           +lbne snerr

                 ldy #'['                                 ; OFF (color & attributes)
                 !text $2c
l283_2           ldy #']'                                 ; ON
                 lda #esc
                 jsr _bsout                               ; do it
                 tya
                 jsr _bsout
                 jsr chrget                               ; eat token
                 beq l283_6                               ; eol- exit

l283_3           jsr chkcom                               ; else must be comma, eat & get next
; jsr chrgot  ;      [910930]
                 cmp #on_token                            ; HARDWARE (Vic) color mode
                 beq l283_4
                 jsr chkesc
                 cmp #off_token
                 bne l283_1

                 lda #%00000010                           ; OFF (monochrome)
                 tsb vic+49
                 bra l283_5

l283_4           lda #%00000010                           ; ON
                 trb vic+49
l283_5           +lbra chrget                             ; exit after eating last token

l283_6           rts                                      ; exit after encountering eol


foreground
                 jsr getnyb                               ; Set text foreground color
                 stx _color
                 rts



highlight
                 +lbeq snerr                              ; missing args??     [911017]
                 cmp #','
                 beq l284_1                               ; options byte only

                 jsr getbyt                               ; Set text highlight color
                 stx highlight_color

l284_1           jsr optzer                               ; set options:     [911001]
                 bcc l284_2                               ; comma but no value not given??
                 txa
                 and #3                                   ; 0= error msgs only
                 asl                                      ; 1= REMs
                 asl                                      ; 2= tokens
                 asl
                 sta helper
l284_2           rts



background
                 jsr getnyb                               ; Set Vic background color
                 stx vic+33
                 rts



border
                 jsr getnyb                               ; Set Vic border color
                 stx vic+32
                 rts


getcomnyb
                 jsr chkcom                               ; check for comma
getnyb
                 jsr getbyt                               ; Get a nybble, check range (0-15)
chknyb
                 cpx #16
                 +lbcs fcerr
                 rts



chkesc                                                    ; Check for escape token, error if not, else get next token
                 cmp #esc_command_token
                 +lbne snerr
                 jsr chrget
                 +lbeq snerr                              ; eos? report error if so
                 rts



chkeos                                                    ; Check for next byte = end of statement, error if not
                 jsr chrget
                 +lbne snerr                              ; eos? report error if not
                 rts


;.end
;[[command.sprite]]



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
;[[command.movspr]]



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

movspr           lda #0                                   ; flag 'movspr' initial coord   [910808]
                 sta op                                   ; (0=movspr, $80=movspr_to, $7f=mouse)
                 jsr get_sprite_number                    ; get sprite #
                 jsr sprcor                               ; get first coordinate (y,a)
movspr_1                                                  ; entry to eval destination coordinate  [910808]
                 bit numcnt                               ; test coordinate type
                 +lbvs snerr                              ; syntax error
                 sty xdest                                ; save coordinate value
                 sty xdest+2
                 sta xdest+1
                 sta xdest+3

                 jsr sprcor                               ; get second coordinate (y,a)
                 bit numcnt                               ; test coordinate type & dispatch accordingly
                 bvc movspr_normal                        ; normal coordinates
                 bmi movspr_angle                         ; angular coordinates

                 bit op                                   ; angle#speed, test if allowed
                 +lbmi snerr                              ; ng- movspr_to call
                 phy                                      ; ok- save speed value
                 ldy #xdest-vwork
                 jsr getang                               ; get angle of movement
                 ldx z_p_temp_1                           ; get sprite number
                 ldy sproff,x                             ; get offset to speed data
                 lda #0
                 sta sprite_data,y                        ; turn off sprite speed
                 iny

                 ldx #3
l287_1           lsr sinval,x
                 dex
                 ror sinval,x
                 dex
                 bpl l287_1

l287_2           sei
                 inx                                      ; x=0
                 lda angsgn,x                             ; move angle data to speed data
                 iny
                 sta sprite_data,y
                 cpx #4
                 bne l287_2

                 lda #0                                   ; clear speed angle counts
l287_3           iny
                 sta sprite_data,y
                 dex
                 bne l287_3

                 pla                                      ; restore speed value
                 and #$3f                                 ; limit range (0-63) ????  [910806]
                 sta sprite_data-10,y                     ; start sprite movement
                 cli
                 rts

movspr_angle
; jsr swapxy  ;swap y and a (eventually) : y ==> x
; tay   ;        a ==> y
; txa   ;        x ==> a
                 pha
                 tya
                 ply

                 jsr gtang1                               ; get angle values
; ldx #xdest-vwork
; jsr scalxy  ;scale lengths
                 ldx #xdest-vwork
                 clc

l288_1           jsr angmlt                               ; multiply lengths*angles for x and y
                 sta vwork,x
                 tya
                 sta vwork+1,x
                 inx
                 inx
                 cpx #ydest-vwork
                 beq l288_1                               ; loop to do y-position

                 ror numcnt                               ; shift in carry to set msb
                 bra movspr_position                      ; go place sprite


movspr_normal                                             ; [910122]
                 sty xdest+2                              ; save second coordinate (y,a)
                 sta xdest+3
; ldx #xdest-vwork
; jsr scalxy  ;scale the coordinates


movspr_position
                 sei                                      ; [910123]
                 lda z_p_temp_1                           ; get sprite number
                 tax                                      ; use as an index
                 asl
                 tay                                      ; get sprite-number * 2 as another index

                 bbr7 op,l289_1
                 rts                                      ; >>>exit here if movspr_to call   [910808]

l289_1           lda xdest+2                              ; get y-coordinate
                 asl numcnt                               ; test if relative
                 bcc l289_3                               ; skip if absolute
                 clc
                 bpl l289_2                               ; skip if normal coordinates
                 eor #$ff
                 sec                                      ; invert to subtract if angular
l289_2           adc vic+1,y                              ; add to current sprite y-value  ???vic_save

l289_3           sta vic+1,y                              ; save new sprite y-position  ???vic_save
                 lda xdest                                ; get low byte of x-coordinate
                 asl numcnt                               ; test if relative
                 bpl l289_5                               ; skip if absolute
                 clc
                 adc vic,y                                ; add current sprite x-position  ???vic_save
                 sta vic,y                                ; save sprite x-position   ???vic_save
                 bcs l289_4                               ; skip if carry
                 inc xdest+1                              ; invert lsb

l289_4           lda vic+16                               ; get x-position msb bits  ???vic_save
                 bra l289_6                               ; test if need to invert msb bit

l289_5           sta vic,y                                ; save new sprite x-position  ???vic_save
                 lda vic+16                               ; ???vic_save
                 ora sbits,x                              ; set x-position msb bit

l289_6           lsr xdest+1                              ; match to lsb of x-coordinate high byte
                 bcs l289_7                               ; skip if should be set
                 eor sbits,x                              ; reset bit

l289_7           sta vic+16                               ; save position msb bits   ???vic_save
; cli
;1l289_1 rts   ; mouse or movspr_to


movspr_to                                                 ; setup for moving sprite to a particular position
;we have already positioned the sprite onscreen
                 jsr chrgot                               ; reget terminating character
                 cmp #to_token
                 beq l290_1                               ; not our call
                 cli
                 rts

l290_1           smb7 op                                  ; it's for us- let everybody else know we're in charge
                 jsr chrget                               ; move to next non-space character
                 clc
                 jsr sprcor_1                             ; go get & evaluate destination coordinate
                 jsr movspr_1                             ; returns with sprite# in .x, VIC sprite index in .y,
;P1 in VIC sprite regs, and P2 in x,ydest
                 asl numcnt                               ; Y: handle specific coordinate types
                 bcc l290_3                               ; skip if absolute
                 clc
                 lda xdest+2                              ; get y-coordinate
                 bpl l290_2                               ; skip if normal coordinates
                 eor #$ff
                 sec                                      ; invert to subtract if angular
l290_2           adc vic+1,y                              ; add to current sprite y-value ???vic_save
                 sta xdest+2                              ; save sprite destination y-position

l290_3           asl numcnt                               ; X: handle specific coordinate types
                 bpl l290_4                               ; skip if absolute
                 clc
                 lda xdest                                ; get low byte of x-coordinate
                 adc vic,y                                ; add current sprite x-position  ???vic_save
                 sta xdest                                ; save sprite destination x-position
                 bcc l290_4
                 inc xdest+1

l290_4           phy
                 jsr combyt                               ; get speed parameter
                 txa
                 and #$3f                                 ; limit range (0-63) ????
                 ora #$80
                 sta xcnt                                 ; save in temp.
                 ply

                 lda vic,y                                ; copy current sprite pos'n to line vars
                 sta xpos                                 ; in preparation for line calculations
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
l291_1           lda #0
                 sta xsgn,x                               ; init direction pointers
                 sta xsgn+1,x
                 jsr abstwo                               ; get absolute value of coordinate differences
                 bpl l291_2                               ; and determine direction
                 dec xsgn,x                               ; negative direction
                 dec xsgn+1,x
                 bra l291_4

l291_2           cmp #0
                 bne l291_3
                 cpy #0
                 beq l291_4                               ; zero direction
l291_3           inc xsgn,x                               ; positive direction
l291_4           sta xabs,x
                 asl
                 sta fct,x                                ; fct(x,y) = 2*abs(x,y)
                 tya
                 sta xabs+1,x
                 rol
                 sta fct+1,x
                 dex
                 dex
                 ldy #xdest-vwork                         ; loop to do in x-direction
                 cpx #xpos-vwork
                 beq l291_1

                 ldx #yabs-savram                         ; determine max(xabs,yabs)
                 ldy #xabs-savram
                 jsr subtwo_savram
                 lda #0
                 rol
                 rol                                      ; a = c * 2
                 sta lesser                               ; index to smaller delta
                 eor #2
                 sta greatr                               ; index to greater delta

                 clc
                 lda #fct-savram
                 adc lesser
                 pha
                 tay
                 eor #2
                 tax
                 jsr subtwo_savram                        ; fct(greatr) = fct(lesser)-fct(greatr)
                 sta savram,x
                 sty savram+1,x

                 ply                                      ; fct(lesser)
                 clc
                 lda #xabs-savram
                 adc greatr
                 tax
                 jsr subtwo_savram                        ; error = fct(lesser) - abs(greatr)
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

                 ldy z_p_temp_1                           ; sprite #
                 ldx sproff,y                             ; sprite IRQ table offset

                 lda xcnt                                 ; set speed factor
                 sta sprite_data,x
                 ldy greatr
                 lda xabs,y                               ; set counter = max(xyabs)
                 sta sprite_data+1,x
                 lda xabs+1,y
                 sta sprite_data+2,x
                 lda xsgn,y                               ; set dir(max) and max
                 ora xsgn+1,y
                 and #3
                 lsr
                 ror
                 ora greatr
                 ror
                 sta sprite_data+4,x
                 ldy lesser
                 lda xsgn,y                               ; set dir(min) and min
                 ora xsgn+1,y
                 and #3
                 lsr
                 ror
                 ora lesser
                 ror
                 sta sprite_data+3,x
                 ldy #0                                   ; set f1, f2, and e
l291_5           lda fct,y
                 sta sprite_data+5,x
                 inx
                 iny
                 cpy #6
                 bcc l291_5

                 cli
                 rts                                      ; done!

;[[command.sprcor]]


;  SPRCOR  -- Get sprite position coordinate
;

sprcor           jsr chkcom_1                             ; check for a comma
sprcor_1
                 ror numcnt                               ; reset msb if comma else set msb
                 bpl l292_1                               ; skip if got a comma
                 cmp #';'                                 ; test if angular data
                 beq l292_3                               ; skip if yes - 2 msb's = 1 1
                 cmp #'#'                                 ; test if speed type
                 beq l292_2                               ; skip if yes - 2 msb's = 0 1
                 +lbra snerr                              ; syntax error if none of above

l292_1           jsr chrgot                               ; test for relative coordinate
                 cmp #plus_token                          ; test if plus sign
                 beq l292_3                               ; skip if yes - show relative
                 cmp #minus_token                         ; test if minus sign
                 beq l292_3                               ; skip if yes - show relative
l292_2           clc                                      ; reset to show absolute
l292_3           ror numcnt                               ; shift in second flag bit

sadwrd           jsr frmnum                               ; get number     label [910307]
                 +lbra getsad                             ; get signed 2 byte coordinate,do rts


;*************************************************************
; CHKCOM_1  --  Check for a comma
;
;  carry set & eq = end of string
;  carry set & neq = not a comma
;  carry clear = a comma
;*************************************************************

chkcom_1
                 jsr chrgot                               ; get character in input stream
                 beq l293_2                               ; skip if end of string
                 cmp #','                                 ; check if a comma
                 clc
                 beq l293_1                               ; skip if yes
                 sec                                      ; set carry if not
l293_1           php
                 pha
                 jsr chrget                               ; move to next non-space character
                 pla
                 plp
l293_2           rts


sproff           !text 0,11,22,33,44,55,66,77             ; sprite offsets into speed table

;.end
;[[command.sprcolor]]



;**************************************************************
;*
;*   SPRCOLOR - Set sprite multicolor registers
;*
;* syntax : SPRCOLOR [multicolor_1] [,multicolor_2]
;*
;**************************************************************

sprcolor
                 cmp #','                                 ; is there a first arg?
                 beq l294_1                               ; nope, skip to second

                 jsr getnyb                               ; get 1 byte arg in .X, range 0-15
; jsr put_io_in_map
                 stx vic+37

l294_1           jsr optbyt                               ; get (optional) 1 byte arg in .X
                 bcc l294_2
                 jsr chknyb                               ; range 0-15
; jsr put_io_in_map
                 stx vic+38

l294_2           rts

;.end



;***************************************************************
;  SPRSAV ( n1 / s1$ ) , ( n2 / s2$ )
;   - move string(s1) or sprite(n1) to string(s2) or sprite(n2)
;               n1 & n2 = a sprite number (1-8)
;                   s1$ = a string variable or expression
;                   s2$ = a string variable
;***************************************************************

sprsav           jsr savinp                               ; evaluate 1st expression
                 bcs l295_2                               ; skip if source is a string
                 sta forpnt
                 sty forpnt+1                             ; save sprite address
                 ldy #62

l295_1           lda (forpnt),y                           ; move sprite def to save area
                 sta savram,y
                 dey
                 bpl l295_1

                 iny                                      ; (0)
                 sty savram+64                            ; save sprite column length
                 sty savram+66                            ; save sprite row length
                 lda #23
                 sta savram+63
                 lda #20
                 sta savram+65
                 ldx #<savram                             ; set ptr to start of sprite def
                 ldy #>savram
                 stx strng1                               ; **
                 sty strng1+1                             ; **

                 lda #67                                  ; set sprite length including lengths
                 jsr strlit_1                             ; **get string space, copy savram to it
                 jsr desc_free                            ; **free up temp descriptor

l295_2           stx savsiz                               ; save source length
                 sta savsiz+1
                 sty savsiz+2                             ; save source start address

                 jsr chkcom                               ; check for a comma
                 lda txtptr                               ; save basic text pointer
                 sta sprtmp_1
                 lda txtptr+1
                 sta sprtmp_2
                 jsr savinp                               ; get next destination parameter
                 bcs savs50                               ; skip if string

                 sta grapnt
                 sty grapnt+1                             ; save sprite address
                 lda savsiz+1
                 sta forpnt                               ; get source address
                 lda savsiz+2
                 sta forpnt+1
                 ldy #0
l295_3           cpy savsiz                               ; test index vs source length
                 beq l295_4                               ; exit if source depleted
                 lda #forpnt                              ; move source byte to sprite
                 jsr lda_far_ram1                         ; (from ram bank 1)
; sta sw_rom_ram0
                 sta (grapnt),y                           ; (to sprite area in bank 0)????
                 iny
                 cpy #63
                 bne l295_3
l295_4           rts


savs50           lda sprtmp_1                             ; restore basic text pointer
                 sta txtptr
                 lda sprtmp_2
                 sta txtptr+1
                 jsr ptrget                               ; get symbol table descriptor for string dest.
                 sta forpnt
                 sty forpnt+1                             ; save symbol table address
                 lda #<savsiz
                 sta facmo                                ; save descriptor address of source
                 lda #>savsiz
                 sta facmo+1
                 +lbra inpcom                             ; move source to dest, do rts (snerr if not eol)


savinp           jsr frmevl                               ; evaluate expression
                 bbs7 valtyp,desc_free                    ; exit if a string
                 jsr conint                               ; get one byte integer in .X
; dex    ;adjust sprite 1..8 to 0..7  [910220]
                 cpx #8
                 +lbcs fcerr                              ; bad value
                 txa                                      ; move sprite number to .A
                 lsr
                 ror
                 ror                                      ; get sprite address
                 ldy #>sprite_base
                 bcc l296_1
                 iny
l296_1           clc                                      ; flag 'sprite' (as opposed to 'string')
                 rts


desc_free                                                 ; free temporary descriptor, set up pointers to string.
                 lda facmo                                ; get address of temp descriptor
                 ldy facmo+1
                 jsr fretms
                 ldy #0                                   ; get len, addr of string
                 jsr indfmo
                 tax
                 iny
                 jsr indfmo
                 pha
                 iny
                 jsr indfmo
                 tay
                 pla
                 sec                                      ; flag 'string found'
                 rts                                      ; return w/ x=len, (a,y)==> string

;.end
;[[command.collision]]



;*****************************************************************
; COLLISION Command
;
; Syntax:  COLLISION n [,address]
;
; Where:   n= 1 ==> sprite / sprite
;   2 ==> sprite / background
;   3 ==> light pen
;
; Address ==> BASIC line number to trap to on interrupt
;      (no address ==> disable trapping)
;*****************************************************************

collision
                 jsr getbyt                               ; get type in .X
                 dex                                      ; adjust 1..3 to 0..2
                 cpx #3
                 +lbcs fcerr                              ; value error

                 phx                                      ; save collision type
                 jsr optwrd                               ; get address (line number) in .Y,.A (optional)
                 plx
; php   ;save .C (.C == 1 ==> real value)
                 sta int_adr_hi,x                         ; save address given
                 sty int_adr_lo,x

                 lda intval                               ; this records valid interrupts
                 ora sbits,x                              ; set correct bit
; plp
                 bcs l297_1                               ; ..unless this is a 'clear',
                 eor sbits,x                              ; ..in which case we'll reset bit
l297_1           sta intval
                 rts

;.end
;[[function.rcolor]]



;************************************************************************
;  RCOLOR (source)  --  return current color assigned to source
;   0  :  Background color
;   1  :  Foreground color
;   2  :  Highlight color
;   3  :  Border color
;************************************************************************

rcolor           jsr conint                               ; evaluate integer argument, put in .X
; jsr put_io_in_map

                 cpx #4
                 +lbcs fcerr                              ; illegal qty
                 txa
                 asl                                      ; make into word pointer
                 tax
                 lda color_source,x                       ; get address of source
                 sta grapnt
                 lda color_source+1,x
                 sta grapnt+1
                 ldy #0
                 lda (grapnt),y                           ; read source (aways system space or I/O????)
                 and #$0f                                 ; mask unused bits
                 tay
; iny   ; make color match keytops
                 +lbra sngflt                             ; float 1 byte in .Y

color_source
                 !word vic+33,_color,highlight_color,vic+32

;[[function.rgraphic]]



; Return graphic screen status & parameters      [910826]
; RGRAPHIC (screen, param) where param = 0 open (1), closed (0), or invalid (>1)
;            1 width  (0=320, 1=640, 2=1280)
;            2 height (0=200, 1=400)
;            3 depth (1-8 bitplanes)
;            4 bitplanes used  (bitmask)
;            5 bank A blocks used (bitmask)
;            6 bank B blocks used (bitmask)
;            7 drawscreen # (0-3)
;            8 viewscreen # (0-3)
;            9 drawmodes  (bitmask)
;           10 pattern type  (bitmask)
;
; Requires a kludge, because RGR used to be a normal 1-arg function in the C128
; but now it takes two args.

rgraphic
; jsr CheckGraphicMode ;verify screen open
                 pla                                      ; remove token from stack
                 jsr PushParms                            ; preserve Graphics parameters & LINNUM  [910820]

                 jsr chkopn                               ; check for open paren
                 jsr getbyt                               ; get screen # in .X
                 stx GKI__parm1
                 jsr combyt                               ; get param # in .X
                 cpx #10+1                                ; [911028]
                 bcs l298_1                               ; illegal param #
                 phx
                 jsr chkcls                               ; check for closing parens

                 jsr ($8038)                              ; read screen params
l298_1           +lbcs fcerr                              ; bad input????

                 lda GKI__parm2
                 plx                                      ; get back desired param #
                 dex
                 bpl l298_2
                 eor #$80                                 ; make 0=closed, 1=open, >1=invalid
                 lsr
                 lsr
                 bra l298_3                               ; return screen open status

l298_2           dex
                 bpl l298_5
l298_3           lsr
l298_4           lsr
                 lsr
                 lsr
                 and #3
                 bra l298_8                               ; return width, height

l298_5           dex
                 bpl l298_6
                 and #8
                 bra l298_4
l298_6           dex
                 bpl l298_7
                 and #7                                   ; return depth
                 inc                                      ; make depth 1-8
                 bra l298_8

l298_7           lda GKI__parm3,x                         ; return bp bask, banks, etc.
l298_8           tay
                 jsr sngflt                               ; float 1 byte arg in .y

                 jsr PopParms                             ; restore Graphics parameters & LINNUM
                 rts

;[[function.pixel]]

; Return the color of a given X,Y pixel location on the drawscreen  [910801]
;  PIXEL (x,y)

pixel            jsr CheckGraphicMode                     ; verify screen open
                 jsr PushParms                            ; preserve Graphics parameters & LINNUM  [910820]

                 jsr getsad                               ; get x
                 sty GKI__parm1
                 sta GKI__parm2
                 jsr comsad                               ; get y
                 sty GKI__parm3
                 sta GKI__parm4
                 jsr chkcls                               ; check for closing parens

                 jsr ($8032)                              ; get Bitplane data at pixel (x,y), returned in .y
                 jsr sngflt                               ; go float 1 byte arg in .Y

                 jsr PopParms                             ; restore graphics parameters
                 rts

;[[function.rpen]]

; Return the color of a drawscreen's PEN      [910820]
;  RPEN (pen#) where pen# = 0,1,2

rpen             jsr CheckGraphicMode                     ; verify screen open
                 jsr PushParms                            ; preserve Graphics parameters & LINNUM  [910820]

                 jsr conint                               ; get 1 byte arg in .x (old style single arg function)
                 cpx #3
                 bcs l299_1                               ; illegal pen #?
                 stx GKI__parm1

                 jsr ($8036)                              ; convert to logical color# (palette index#)
l299_1           +lbcs fcerr                              ; drawscreen not set or illegal quantity somewhere

                 jsr sngflt                               ; go float 1 byte arg in .Y

                 jsr PopParms                             ; restore graphics parameters
                 rts


;[[function.rpalette]]
; Return the R,G, or B component of a color     [910820]
; RPALETTE (screen#, color#, rgb)

rpalette
                 jsr CheckGraphicMode                     ; verify screen open
                 jsr PushParms                            ; Save graphics parameters

                 jsr conint                               ; get screen# in .x
                 cpx #4
                 bcs l300_1                               ; illegal screen#
                 stx GKI__parm1

                 jsr combyt                               ; get color# in .x ????check for legal color#
                 stx GKI__parm2

                 jsr ($8034)                              ; get RGB components of color# in PARM3,4,5
                 bcs l300_1                               ; something is wrong????

                 jsr combyt                               ; get r,g,b component#
                 cpx #3
l300_1           +lbcs fcerr                              ; illegal value

                 ldy GKI__parm3,x                         ; get r,g,b value
                 jsr sngflt                               ; float 1 byte arg in .y

                 jsr chkcls                               ; check for closing paren
                 jsr PopParms                             ; restore graphics parameters
                 rts


PushParms                                                 ; [910820]
                 ply                                      ; Grab return address
                 plz

                 phw linnum                               ; Save 'poker' value

                 ldx #17-1
l301_1           lda GKI__parm1,x                         ; Save Graphics parameters
                 pha                                      ; [eg: CHAR x,y,1,1,2,str$(PIXEL(x,y))]
                 dex
                 bpl l301_1

                 phz                                      ; Restore return address
                 phy
                 rts


PopParms                                                  ; [910820]
                 ply                                      ; Grab return address
                 plz

                 ldx #0
l302_1           pla                                      ; Restore Graphics parameters
                 sta GKI__parm1,x
                 inx
                 cpx #17
                 bcc l302_1

                 pla                                      ; Restore 'poker' value
                 sta linnum+1
                 pla
                 sta linnum

                 phz                                      ; Restore return address
                 phy
                 rts

;.end
;[[function.rsprite]]



;******************************************************************
;* RSPRITE - Return sprite information
;*
;* Syntax : RSPRITE (sprite_number, argument)
;*
;* Where  : sprite_number = [0..7]
;*   argument = [0..5]
;*   0 : enabled?   (y(1)/n(0))
;*   1 : color?     (0-15)
;*   2 : priority over background? (y(1)/n(0))
;*   3 : expand in x direction? (y(1)/n(0))
;*   4 : expand in Y direction? (y(1)/n(0))
;*   5 : multicolor sprite?  (y(1)/n(0))
;******************************************************************

rsprite          jsr conint                               ; get first arg, sprite #, in .X
; dex  ;adjust [1..8] to [0..7]   [910220]
                 cpx #8  ; (318018-03 mod                 ; fab)
                 bcs l303_1                               ; value error
                 txa
                 pha                                      ; save sprite number

; jsr chkcom ;check for proper delimiter
; jsr getbyt ;do frmevl, get 1 byte arg (arg) in .X
                 jsr combyt                               ; [910820]
                 jsr chkcls                               ; look for closing paren
                 cpx #6
l303_1           +lbcs fcerr                              ; value error

                 ply                                      ; sprite number
; jsr put_io_in_map
                 lda vic+39,y                             ; assume 'color'
                 and #$0f                                 ; range 0-15
; inc a  ;adjust to 'keyboard' colors   [910724]
                 cpx #1
                 beq l303_2                               ; it was color. set up for float

                 lda rspmod,x                             ; get index for this function
                 tax
                 lda sbits,y                              ; get mask for this sprite number
                 and vic,x
                 beq l303_2
                 lda #1                                   ; return all non-zeros as '1'

l303_2           tay
                 +lbra sngflt                             ; go float 1 byte arg in .Y


rspmod           !text 21,39,27,29,23,28                  ; VIC registers associated with arg#

;.end



;******************************************************************
;* RSPCOLOR - return sprite multicolor reg's
;*
;* Syntax : RSPCOLOR (argument)
;*
;* Where  : argument = [1..2]
;*   1 : return multicolor #1
;*   2 : return multicolor #2
;******************************************************************

rspcolor
                 jsr chkcls                               ; check for closing paren
                 jsr conint                               ; get arg in .X
                 dex                                      ; adjust [1..2] to [0..1
                 cpx #2
                 +lbcs fcerr                              ; value error

; jsr put_io_in_map
                 lda vic+37,x
                 and #$0f
                 tay
; iny  ;range 0-15     [910724]
                 +lbra sngflt                             ; float 1 byte arg in .Y

;.end
;[[function.rsppos]]



;******************************************************************
;* RSPPOS - Return sprite location / speed data
;*
;* Syntax:  RSPPOS (sprite_number, argument)
;*
;* Where:   sprite_number = [0..7]
;*    argument = [0..2]
;*   0 : return X position
;*   1 : return Y position
;*   2 : return current speed
;******************************************************************

rsppos           jsr conint                               ; get first arg, sprite #, in .X
; dex  ;adjust [1..8] to [0..7]   [910220]
                 cpx #8  ; (318018-03 mod                 ; fab)
                 bcs l304_1                               ; value error

                 phx                                      ; save sprite number
; jsr chkcom ;check for proper delimiter
; jsr getbyt ;do frmevl, get 1 byte arg (arg) in .X
                 jsr combyt                               ; [910820]
                 jsr chkcls                               ; look for closing paren
                 cpx #3
l304_1           +lbcs fcerr                              ; value error

                 ply                                      ; sprite number
                 cpx #2
                 bne l304_2                               ; branch if x or y position

                 ldx sproff,y                             ; get offset into speed data
                 ldy sprite_data,x                        ; get speed data
                 +lbra sngflt                             ; go float 1 byte arg in .Y

; Get msb of sprite position (in case this is for x position)

l304_2           sei
                 lda sbits,y                              ; get bit mask for this sprite
                 and vic+16                               ; ???vic_save
                 beq l304_3
                 lda #1                                   ; change any non-zero to a '1'
l304_3           pha                                      ; save msb

                 tya                                      ; y = sprite# * 2
                 asl
                 tay
                 txa                                      ; see if this is y position
                 lsr                                      ; .C = 0 for x pos'n, 1 for y pos'n
                 bcc l304_4                               ; branch if x pos'n

                 iny                                      ; adjust pointer to point to y pos'n in register data
                 pla
                 lda #0                                   ; ..and force 'msb' to be zero
                 pha

l304_4           lda vic,y                                ; get correct location lsb   ???vic_save
                 cli
                 tay
                 pla                                      ; ..and get msb,
                 +lbra nosflt                             ; ..and go float 2 byte value in y,a

;.end
;[[function.bump]]



;******************************************************************
;* BUMP - read sprite collision
;*
;* Syntax : BUMP (argument)
;*
;* Where  : argument = [1..2]
;*   1 : sprite/sprite collision
;*   2 : sprite/background collision
;******************************************************************

bump             jsr chkcls
                 jsr conint                               ; get arg in .X
                 dex                                      ; adjust [1..2] to [0..1]
                 cpx #2
                 +lbcs fcerr                              ; value error

                 sei
                 ldy collisions,x                         ; get recorded collisions
                 lda #0                                   ; reset them
                 sta collisions,x
                 cli
                 +lbra sngflt                             ; float 1 byte arg in .Y

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
                 jsr settwo                               ; move angle value into y/a

gtang1           ldx #0                                   ; init count of phase

l305_1           inx
                 sec
                 sbc #90                                  ; subtract 90 until less than 0
                 bcs l305_1
                 dey
                 bpl l305_1
                 stx angsgn                               ; save phase (here it is 1-4)
                 pha
                 adc #90                                  ; make positive
                 jsr l305_2                               ; do division by 10
                 pla                                      ; get 2's comp of angle
                 clc
                 eor #$ff
                 adc #1                                   ; make positive
                 dec angsgn                               ; correct phase

l305_2           ldx #$ff
l305_3           inx                                      ; do division by 10
                 sec
                 sbc #10
                 bcs l305_3
                 adc #10                                  ; make positive
                 sta vtemp1                               ; save remainder
                 txa
                 asl                                      ; get quotient*2 as index
                 tax
                 lda angval+1,x                           ; get low byte base
                 ldy angval,x                             ; get high byte value

l305_4           clc
                 dec vtemp1
                 bmi l305_5                               ; done - remainder = 0
                 adc incval+1,x                           ; add low byte increment
                 pha
                 tya
                 adc incval,x                             ; add high byte increment
                 tay
                 pla
                 bcc l305_4                               ; ...always

l305_5           pha                                      ; save low byte of result
                 ldx #0                                   ; point to sinval
                 lda angsgn
                 lsr
                 bcs l305_6                               ; skip if sine value
                 ldx #2                                   ; point to cosval

l305_6           pla
                 sta sinval,x                             ; save low byte result
                 tya
                 sta sinval+1,x                           ; save high byte result
                 rts


;*************************************************************
;  angmlt  -  multiple 2-byte integer times angle
;       carry set/reset = cosine/sine
;
;       vwork+x = 2-byte integer
;       result left in y/a
;*************************************************************

angmlt
                 ldy #sinval-vwork                        ; get offset to angle value
                 bcc l306_1                               ; get cosine/sine offset
                 ldy #cosval-vwork

l306_1           lda angsgn
                 adc #2                                   ; correct phase for cosine to look as sine
                 lsr
                 lsr
                 php                                      ; save if carry - means negative angle value
                 jsr settwo                               ; get angle fraction in y/a
                 cpy #$ff                                 ; test if value should be 1
                 bcc l306_2                               ; skip if not
                 txa
                 tay                                      ; get offset to integer
                 jsr settwo                               ; just get integer - multiplied by 1
                 bcs l306_3

l306_2           jsr twobyt                               ; multiply integer times angle value
l306_3           plp                                      ; get sign of angle
                 bcc invert                               ; invert result if negative,do rts
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
                 bcc addtw2                               ; go do addition
                 bcs subtw2                               ; go do subtraction
dotwo
                 bcs subtwo                               ; go do subtraction

;***************************************************************
;  ADDTWO  - Add vwork+y and vwork+x  Result in y/a
;***************************************************************

addtwo
                 jsr settwo                               ; put vwrok+y into y/a

addtw2                                                    ; enter here to add y/a to vwork+x
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
                 jsr settwo                               ; move vwork+y into y/a

subtw2                                                    ; enter here with 1st value in y/a
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
                 lda savram,y                             ; load value into y,a
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

abstwo                                                    ; movspr_to [910809]
                 jsr subtwo                               ; subtract vwork+y - vwork+x
abstw2                                                    ; entrance with vwork+y in y/a
                 bpl absrts                               ; done if result is positive
invert           php
                 clc
                 eor #$ff                                 ; invert low byte result and add 1
                 adc #1
                 pha
                 tya
                 eor #$ff                                 ; invert high byte result
                 adc #0                                   ; add back any carry
                 tay
                 pla
                 plp
absrts           rts


;****************************************************************
;  TWOBYT  - Multiply 2 byte fraction in y/a times 2 bytes
;  Integer found in vwork+x-reg.  Result = y/a
;****************************************************************

twobyt
                 sty vtemp1                               ; save fraction
                 sta vtemp2
                 lda vwork,x
                 ldy vwork+1,x
                 php                                      ; save sign of integer
                 jsr abstw2                               ; absolute value
                 sta vwork,x
                 tya
                 sta vwork+1,x
                 lda #0
                 sta vtemp3                               ; initialize result to zero

                 ldy #16                                  ; initialize count
l307_1           lsr vtemp1
                 ror vtemp2
                 bcc l307_2                               ; skip if no bit set
                 clc
                 adc vwork,x                              ; add integer low byte
                 pha
                 lda vtemp3
                 adc vwork+1,x                            ; add integer high byte to total
                 sta vtemp3
                 pla

l307_2           lsr vtemp3                               ; divide by 2
                 ror
                 dey
                 bne l307_1                               ; loop 16 times - test all bits in 2 bytes

                 adc #0                                   ; add back round factor
                 ldy vtemp3
                 bcc l307_3
                 iny
l307_3           plp                                      ; pop sign
                 bra abstw2                               ; return with signed product in y/a


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


incor2                                                    ; enter here for optional argument
                 jsr chrgot                               ; end of line?
                 beq l308_1                               ; yes, use defaults
                 jsr chkcom
                 cmp #','                                 ; is there really an arg?
                 bne incord                               ; yes, let'er rip

l308_1           ldy #0                                   ; set default pos = current pos
l308_2           lda xpos,y
                 sta vwork,x
                 inx
                 iny
                 cpy #4
                 bcc l308_2
                 rts


;incor3    ;enter here for non-optional arg preceded by a comma
; jsr chkcom
incord
                 stx vtemp4                               ; save offset to destination
                 jsr cordsb                               ; get 2-byte x-parameter
                 jsr chrgot
                 cmp #','
                 beq docord                               ; skip ahead if have comma

                 cmp #';'                                 ; check for semi-colon
                 +lbne snerr                              ; missing angle param- show syntax message
                 jsr chrget       ;skip over '            ; '
                 jsr getwrd                               ; get 2-byte angle in a,y
                 sta z_p_temp_1                           ; swap a,y
                 tya
                 ldy z_p_temp_1
                 jsr gtang1                               ; get sine & cosine values for the angle
                 ldx vtemp4
                 lda vwork,x
                 sta vwork+2,x                            ; move length to y-parameter
                 lda vwork+1,x
                 sta vwork+3,x
; jsr scalxy       ;scale the values
                 lda #$0e
                 sta vtemp5
                 clc
                 ldx vtemp4

l309_1           jsr angmlt                               ; multiply length * angle
                 sta vwork,x                              ; save angle result
                 tya
                 sta vwork+1,x
                 ldy #xpos-vwork
                 lsr vtemp5
                 bcc l309_2
                 ldy #ypos-vwork

l309_2           jsr dotwo                                ; add/subtract value to current position
                 sta vwork,x
                 tya                                      ; save result in destination
                 sta vwork+1,x
                 inx
                 inx
                 lsr vtemp5
                 bne l309_1                               ; do y-coordinate
                 clc
                 rts


docord           jsr chrget                               ; skip over comma
                 inc vtemp4                               ; point to y-destination
                 inc vtemp4
                 jsr cordsb                               ; get y-paramter
; ldx vtemp4
; dex
; dex
; jsr scalxy       ;scale the values
                 ldy #ypos-vwork
                 ldx vtemp4
                 inx
                 inx

docor1           dex
                 dex
                 lsr vtemp5
                 bcc docor2                               ; skip if not relative
                 jsr addtwo                               ; add to current position
                 sta vwork,x
                 tya
                 sta vwork+1,x

docor2           ldy #xpos-vwork
                 cpx vtemp4
                 beq docor1                               ; loop to do x-coordinate
                 clc
                 rts

;
; CORDSB -- Get the next 2-byte parameter
;

cordsb           jsr chrgot                               ; read character
                 cmp #plus_token                          ; check if relative - plus sign
                 beq l310_1                               ; skip if yes
                 cmp #minus_token
                 beq l310_1                               ; skip if relative - minus sign
                 clc                                      ; .c=1 if relative coord, .c=0 if absolute
l310_1           rol vtemp5                               ; save coord type for later
                 jsr frmnum
                 jsr getsad                               ; get signed 2 byte coordinate (y,a), do rts
                 ldx vtemp4
                 sta vwork+1,x                            ; save 2-byte parameter
                 tya
                 sta vwork,x
                 rts

;.end

;GRAPHICS11.SRC

;  ANGVAL  -- Table of angle values on 10 degree boundaries
;  Values based as fraction of 65536

angval
                 !text $00,$00                            ; sine 00 degrees -  .0000
                 !text $2c,$71                            ; sine 10 degrees -  .1736
                 !text $57,$8d                            ; sine 20 degrees -  .3420
                 !text $80,$00                            ; sine 30 degrees -  .5000
                 !text $a4,$8f                            ; sine 40 degrees -  .6428
                 !text $c4,$19                            ; sine 50 degrees -  .7660
                 !text $dd,$b2                            ; sine 60 degrees -  .8660
                 !text $f0,$90                            ; sine 70 degrees -  .9397
                 !text $fc,$1c                            ; sine 80 degrees -  .9848
                 !text $ff,$ff                            ; sine 90 degrees - 1.0000

;  INCVAL  -- Table of incremental values between 10 degrees
;  Values based on fraction of 65536

incval
                 !text $04,$72                            ; 01 - 09 degrees -  .01739
                 !text $04,$50                            ; 11 - 19 degrees -  .01692
                 !text $04,$0b                            ; 21 - 29 degrees -  .01592
                 !text $03,$a8                            ; 31 - 39 degrees -  .01443
                 !text $03,$28                            ; 41 - 49 degrees -  .01252
                 !text $02,$90                            ; 51 - 59 degrees -  .01023
                 !text $01,$e3                            ; 61 - 69 degrees -  .00762
                 !text $01,$28                            ; 71 - 79 degrees -  .00477
                 !text $00,$63                            ; 81 - 89 degrees -  .00179

;.end

;[[edit.mode]]


; Edit mode is simply a poor man's word processor.  Text is entered normally
; as if the user were typing in a program, but tokenization is turned off.
; This affects only that text which follows a line number.  CRUNCH and QPLOP
; test for this mode, and deal with the text accordingly.  RUN, GOTO, etc.
; test for this mode and error-out if it's enabled.  LOADing a text file
; will automatically add line numbers, SAVEing a text file will remove them.
;
; Syntax:  EDIT < ON | OFF >
;
; F. Bowen        [910620]

edit             jsr errind                               ; direct mode only command
                 cmp #on_token
                 bne l311_1
                 lda #%00010000                           ; EDIT ON
                 bra l311_3                               ; (this kills trace mode, too)

l311_1           jsr chkesc                               ; [910930]
; cmp #esc_command_token
; bne l311_2
; jsr chrget
                 cmp #off_token
l311_2           +lbne snerr
                 lda #0                                   ; EDIT OFF
l311_3           sta runmod
                 jmp chrget                               ; exit


edit_crunch                                               ; Edit mode only, find end of plain text in input buffer
                 phw txtptr                               ; save current position in input buffer
                 jsr rem                                  ; find the end of the line
                 ldx txtptr
                 pla                                      ; restore buffer pointer
                 sta txtptr+1
                 pla
                 sta txtptr
                 sec                                      ; compute length of line
                 txa
                 sbc txtptr
                 tay
                 iny
                 rts                                      ; done


edit_p1line                                               ; Edit mode only, list a line of plain text
                 jsr linprt                               ; print line number in (a,x)
                 lda #' '                                 ; print a space

                 ldy #3                                   ; start printing at text following line number
l312_1           jsr outch                                ; print character
                 iny
                 bbr5 helper,l312_2                       ; if called from FIND/CHANGE check for highlighting
                 jsr helpsb
l312_2           jsr indlow                               ; get next character
                 bne l312_1                               ; loop until eol
                 rts                                      ; done


;.end

; EDIT LOAD/SAVE  Load or Save a plain text SEQ file in memory

edit_load                                                 ; Called by DLOAD/DVERIFY when in EDIT mode
                 ldz #1
                 jsr open_SEQ_file                        ; Open the file just like TYPE: filename [,U#] [,D#]
                 jsr Check_DS                             ; check current disk error message
                 ldy #0
                 lda #dsdesc+1
                 jsr lda_far_ram1                         ; lda (dsdesc+1),y peek at first character
                 cmp #'2'
                 +lbcs l313_12                            ; exit if error
                 jsr Clear_DS                             ; else zap 'ok' message so user gets fresh one
                 ldx dosla
                 jsr _chkin                               ; get input channel
                 +lbcs l313_12                            ; error

                 bbs0 verck,l313_1
                 jsr _primm
                 !text cr,"LOADING",0
                 bra l313_2
l313_1           jsr _primm
                 !text cr,"VERIFYING",0

l313_2           lda #<1000                               ; default starting line #
                 ldx #>1000
                 sta linnum
                 stx linnum+1

                 lda txttab                               ; load address
                 ldx txttab+1
                 sta index
                 stx index+1

l313_3           ldy #0                                   ; Input one line of text
                 jsr _stop                                ; check stop key
                 beq l313_11                              ; exit if down
                 jsr _readst                              ; check channel status
                 bne l313_11                              ; exit if eof or error

                 bbr0 verck,l313_4
                 ldy #3
                 bra l313_5                               ; skip ahead if verify op

l313_4           lda #1                                   ; install fake line links for this line
                 jsr sta_far_in1
                 iny                                      ; 1
                 jsr sta_far_in1
                 iny                                      ; 2
                 lda linnum                               ; install line number for this line
                 jsr sta_far_in1
                 iny                                      ; 3
                 lda linnum+1
                 jsr sta_far_in1
                 clc
                 lda linnum                               ; generate next line number
                 adc #10
                 sta linnum
                 bcc l313_5
                 inc linnum+1


l313_5           iny                                      ; bump buffer pointer
                 cpy #buflen                              ; check buffer (160 max. input buffer size to edit)
                 beq l313_8                               ; split long lines into two????
                 jsr _basin                               ; read file data
                 beq l313_8                               ; CR or null terminates line
                 cmp #cr
                 beq l313_8
; cmp #$20  ;adjust invisible characters less than space
; bcc l313_6   ; ????make them appear in reverse field, but note
; ora #$80  ; that these lines can't be edited without losing them.

l313_6           bbr0 verck,l313_7
                 jsr indcmp_in1                           ; Compare to memory
                 beq l313_5                               ; ok
                 jsr list_exit
                 ldx #ervfy                               ; verify error
                 +lbra error

l313_7           jsr sta_far_in1                          ; Load into memory
                 bra l313_5                               ; loop until eol or error (kernel returns CR in case of error)

l313_8           bbs0 verck,l313_9
                 lda #0
                 jsr sta_far_in1                          ; terminate line with null (replaces CR)
l313_9           iny
                 tya
                 clc
                 adc index
                 sta index
                 bcc l313_10
                 inc index+1
l313_10          lda index+1
                 cmp max_mem_0+1                          ; out of memory????
                 bcc l313_3                               ; no, continue until eof
                 bsr edit_load_done                       ; yes, patch things up best we can
                 jsr list_exit                            ; close disk
                 +lbra omerr                              ; report error & exit

l313_11          bbs0 verck,l313_12
                 jsr edit_load_done                       ; EOF: terminate memory with a pair of nulls
l313_12          +lbra list_exit                          ; release channel, close file, etc.

; bbr0 verck,40$
; jsr verify_ok  ;if Verify, report 'ok'
;40$ lda #0   ;exit directly to main????
; bra end

edit_load_done
                 lda #0                                   ; EOF: terminate memory with a pair of nulls
                 tay
                 jsr sta_far_in1
                 iny
                 jsr sta_far_in1
                 inw index
                 inw index
                 ldx index                                ; set top
                 ldy index+1
                 stx text_top
                 sty text_top+1
                 +lbra link_program                       ; relink & RTS


edit_save
                 lda #$e6                                 ; parse:  filename [,U#] [,D#]
                 jsr dosprs                               ; (like dopen:      0 0 0 *  * 0 0 1 )
                 jsr chk1                                 ; check parameters
                 jsr find_la                              ; find an available LA
                 jsr find_sa                              ; find an available SA
                 ldy #fsavseq
                 ldx #8
                 jsr open_file                            ; open the file
                 +lbcs list_err                           ; exit if error
                 ldx dosla
                 jsr _chkout                              ; get output channel
                 bcs l314_5                               ; error

                 lda txttab                               ; save address
                 ldx txttab+1
                 sta index
                 stx index+1

l314_1           jsr _stop                                ; check stop key
                 beq l314_5                               ; exit if down
                 jsr _readst                              ; check channel status
                 bne l314_5                               ; exit if eof or error????

                 ldy #3                                   ; save a line, starting past links & line#
l314_2           iny                                      ; bump buffer pointer
; cpy #buflen  ;check buffer (160 max. input buffer size to edit)
; beq ??$   ; split long lines into two????
                 jsr indin1
                 tax                                      ; save character for eol check
                 bne l314_3
                 lda #cr                                  ; eol: substitute CR ???? allow some other terminator
l314_3           jsr _bsout                               ; write file data
                 txa
                 bne l314_2                               ; loop until eol

                 iny                                      ; advance text index to start of next line
                 tya
                 clc
                 adc index
                 sta index
                 bcc l314_4
                 inc index+1

l314_4           ldy #0                                   ; check for EOF: a pair of null links
                 jsr indin1
                 bne l314_1
                 iny
                 jsr indin1
                 bne l314_1                               ; loop until end of text

l314_5           +lbra list_exit                          ; release channel, close file, exit

;.end


Sound_CLR
                 jsr chkeos                               ; eat CLR token, check eos   [910717] new
Sound_CLR_1
                 php
                 sei
; jsr go_slow  ;      [910716] 4567R7A
                 lda #0
                 ldx #24-1
l315_1           sta sid1,x                               ; initialize SID chips
                 sta sid2,x
                 dex
                 bpl l315_1

                 sta filters1+2                           ; set filters off
                 sta filters2+2

                 lda #8                                   ; set default volume
                 sta filters1+3
                 sta filters2+3                           ; [910612]
; sta filters+4  ;why?      [910612]
                 sta sid1+24
                 sta sid2+24

; jsr go_fast  ;      [910716] 4567R7A

                 bit _pal_ntsc                            ; determine if PAL or NTSC system  [910724]
                 bmi l315_2                               ; ...branch if PAL
                 lda #(<beats_ntsc)/4                     ; set beat to quarter note (4/4 time = .5 sec)
                 ldy #>beats_ntsc/4
                 bra l315_3
l315_2           lda #<beats_pal/4
                 ldy #>beats_pal/4
l315_3           sta ntime
                 sty ntime+1

                 lda #4                                   ; set default octave
                 sta octave
                 lda #12                                  ; set default tempo    [910220]
                 sta tempo_rate                           ; 12 makes whole note in 4/4 time last 2 seconds

                 ldy #30-1                                ; initialize music tables
l315_4           lda atkmus,y
                 sta atktab,y
                 dey
                 bpl l315_4

                 ldx #10-1                                ; initialize pulse widths
l315_5           lda pwhmus,x
                 sta pulshi,x
                 dex
                 bpl l315_5

                 txa                                      ; $ff
                 ldx #6-1                                 ; stereo SIDs   (save space) [911119]
                 ldy #1
l315_6           sta sound_time_hi,x                      ; turn all SOUND counters off
                 sta voices,y                             ; turn all PLAY counters off
                 iny
                 iny
                 dex
                 bpl l315_6

                 ldy #6-1                                 ; set default envelope (piano) for all voices (6)
                 sty voice
l315_7           ldx #0
                 jsr set_envelope_1
                 dec voice
                 bpl l315_7
                 inc voice                                ; set default voice (0)

                 plp
                 rts

;[[initialise.sprites]]

Sprite_CLR
                 jsr chkeos                               ; eat CLR token, check eos   [910717] new
Sprite_CLR_1
                 php
                 sei
                 lda #0
                 sta vic+21                               ; Turn off all sprites
                 sta vic+23                               ; Unexpand them     [910828]
                 sta vic+27                               ; Sprite priority
                 sta vic+28                               ; Hires sprites
                 sta vic+29

                 ldx #init_as_0                           ; Init sprite tables
l316_1           sta sprite_data,x
                 dex
                 bpl l316_1

                 lda #sprite_base/64+7                    ; Set up sprite pointers
                 ldy #7
l316_2           bbr7 _mode,l316_3
                 sta sprite_ptrs_40,y                     ; 40 col screen
                 bra l316_4
l316_3           sta sprite_ptrs_80,y                     ; 80 col screen
l316_4           dec
                 dey
                 bpl l316_2

                 plp
; rts

;.end
;[[handler.nmi]]



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
;[[system.jumptable]]



                 * = $7f00


; Format Conversions     [6]

                 +lbra ayint                              ; convert floating point to integer
                 +lbra givayf                             ; convert integer to floating point
                 +lbra fout                               ; convert floating point to PETSCII string
                 +lbra val_1                              ; convert PETSCII string to floating point
                 +lbra getadr                             ; convert floating point to an address
                 +lbra floatc                             ; convert address to floating point

; Math Functions     [24]

                 +lbra fsub                               ; MEM - FACC
                 +lbra fsubt                              ; ARG - FACC
                 +lbra fadd                               ; MEM + FACC
                 +lbra faddt_c65                          ; ARG - FACC      [910402]
                 +lbra fmult                              ; MEM * FACC
                 +lbra fmultt_c65                         ; ARG * FACC      [910402]
                 +lbra fdiv                               ; MEM / FACC
                 +lbra fdivt_c65                          ; ARG / FACC      [910402]
                 +lbra log                                ; compute natural log of FACC
                 +lbra int                                ; perform BASIC INT() on FACC
                 +lbra sqr                                ; compute square root of FACC
                 +lbra negop                              ; negate FACC
                 +lbra fpwr                               ; raise ARG to the MEM power
                 +lbra fpwrt                              ; raise ARG to the FACC power
                 +lbra exp                                ; compute EXP of FACC
                 +lbra cos                                ; compute COS of FACC
                 +lbra sin                                ; compute SIN of FACC
                 +lbra tan                                ; compute TAN of FACC
                 +lbra atn                                ; compute ATN of FACC
                 +lbra round                              ; round FACC
                 +lbra abs                                ; absolute value of FACC
                 +lbra sign                               ; test sign of FACC
                 +lbra fcomp                              ; compare FACC with MEM
                 +lbra rnd_0                              ; generate random floating point number

; Movement      [22]

                 +lbra conupk                             ; move RAM MEM to ARG
                 +lbra romupk                             ; move ROM MEM to ARG
                 +lbra movfrm                             ; move RAM MEM to FACC
                 +lbra movfm                              ; move ROM MEM to FACC
                 +lbra movmf                              ; move FACC to MEM
                 +lbra movfa                              ; move ARG to FACC
                 +lbra movaf                              ; move FACC to ARG

; bra optab ;????not executable
; bra drawln
; bra gplot
; bra cirsub
                 +lbra run
                 +lbra runc
                 +lbra clearc                             ; [910410]
                 +lbra new
                 +lbra link_program
                 +lbra crunch
                 +lbra FindLine
                 +lbra newstt
                 +lbra eval
                 +lbra frmevl
                 +lbra run_a_program
                 +lbra setexc
                 +lbra linget
                 +lbra garba2
                 +lbra execute_a_line

; Temporaries for C65 development (???? used by graphics) [12]

                 +lbra chrget
                 +lbra chrgot
                 +lbra chkcom
                 +lbra frmnum
                 +lbra getadr
                 +lbra getnum
                 +lbra getbyt
                 +lbra plsv

                 +lbra lda_far_ram0                       ; lda (.x),y from BASIC text bank [910716]
                 +lbra lda_far_ram1                       ; lda (.x),y from BASIC variable bank [910716]
                 +lbra sta_far_ram0                       ; sta (.x),y to   BASIC text bank [910716]
                 +lbra sta_far_ram1                       ; sta (.x),y to   BASIC variable bank [910716]


; Graphic Kernel Call. (Temporary for C65 development ????)
;
;  syntax:  GRAPHIC command# [,args]
;
; Basically this is a modified C64-type SYS command, minus the address.
; In the final C65 system, this will represent the ML interface, not the
; BASIC 10.0 interface which is implemented here in the development system.


graphic
                 cmp #clr_token                           ; GRAPHIC CLR (graphic system initialize)
                 bne l317_1                               ; no
                 jsr chrget                               ; yes advance past token
                 jmp ($8000)                              ; go initialize graphic kernel

l317_1
; tax
; bmi snerr  ;Syntax error if any other secondary token
;
;
                 jmp (graphic_vector)                     ; Else, call the Graphics Kernel's Parser...
;
;
graphic_kernel                                            ; ...via indirect
                 jmp ($8002)


; C65 Graphic Kernel Jump Table      [910826]
;
; 8000 init   ;sets up graphic vars
; 8002 parser   ;GRAPHIC ML Parser????
;
; 8004 kg65.start-1  ;0 commands
; 8006 kg65.screendef-1 ;1
; 8008 kg65.screenopen-1 ;2
; 800a kg65.screenclose-1 ;3
; 800c kg65.screenclear-1 ;4
; 800e kg65.screen-1  ;5
; 8010 kg65.setpen-1  ;6
; 8012 kg65.setpalette-1 ;7
; 8014 kg65.setdmode-1  ;8
; 8016 kg65.setdpat-1  ;9
; 8018 kg65.line-1  ;10
; 801a kg65.box-1  ;11
; 801c kg65.circle-1  ;12
; 801e kg65.polygon-1  ;13
; 8020 kg65.ellipse-1  ;14
; 8022 kg65.viewpclr-1  ;15
; 8024 kg65.copy-1  ;16
; 8026 kg65.cut-1  ;17
; 8028 kg65.paste-1  ;18
; 802a kg65.load-1  ;19
; 802c kg65.char-1  ;20
; 802e kg65.paint-1  ;21
; 8030 kg65.viewpdef-1  ;22
; 8032 kg65.f.pixel-1  ;23
; 8034 kg65.f.rpalette-1 ;24
; 8036 kg65.f.index2color-1 ;25
; 8038 kg65.f.rgraphic  ;26





