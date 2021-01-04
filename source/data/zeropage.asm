; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      zeropage.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************

                * = $0000

                !fill 2                                 ; '4510' registers (not used in C65 mode)
srchtk          !fill 1                                 ; token 'search' looks for (run-time stack) / SYS 'bank#'

                * = $000a                               ; skip over SYS address, status, a/x/y/z registers

integr                                                  ; used by math routines (this & following location)
charac          !fill 1
endchr          !fill 1
verck           !fill 1                                 ; LOAD/VERIFY flag
count           !fill 1                                 ; temp used all over
dimflg          !fill 1                                 ; DIM flag used by variable search
valtyp          !fill 1                                 ; 0=numeric, $FF=string
intflg          !fill 1                                 ; b7: (0=float,1=integer), b6: (1=get flag)
garbfl                                                  ; garbage collection temporary
dores           !fill 1                                 ; b7: P1LINE quote flag
subflg          !fill 1                                 ; b7: subscript flag (set to disallow subscripts() & integers%)
input_flag      !fill 1                                 ; READ($98), GET($40), or INPUT($00)
domask
tansgn          !fill 1
channl          !fill 1                                 ; active I/O channel
poker                                                   ; temp used all over
linnum          !fill 2                                 ; line number

temppt          !fill 1                                 ; pointer to next temporary descriptor in tempst
lastpt          !fill 2                                 ; pointer to last used temporary string
tempst          !fill 9                                 ; temporary descriptor pointers (3 at 3 bytes each)

index
index1          !fill 2
index2          !fill 2

multiplicand                                            ; 2 bytes wide, for unsigned integer multiply
resho           !fill 1
resmoh          !fill 1
product                                                 ; 3 bytes wide, for unsigned integer multiply
addend
resmo           !fill 1
reslo           !fill 1
                !fill 1
txttab          !fill 2                                 ; where BASIC program begins   (text_bank)
vartab          !fill 2                                 ; where variable descriptors begin  (var_bank)
arytab          !fill 2                                 ; where array table begins   (var_bank)
strend          !fill 2                                 ; where arrays table ends   (var_bank)
fretop          !fill 2                                 ; bottom of string storage   (var_bank)
frespc          !fill 2                                 ; where temporary strings begin   (var_bank)
max_mem_1       !fill 2                                 ; highest address available to BASIC in RAM 1 (var_bank)
curlin          !fill 2
txtptr          !fill 2                                 ; pointer to BASIC text used by CHRGET, etc.
form                                                    ; used by print using
fndpnt          !fill 2                                 ; pointer to item found by search
datlin          !fill 2
datptr          !fill 2
inpptr          !fill 2
varnam          !fill 2
fdecpt
varpnt          !fill 2
lstpnt
andmsk
forpnt          !fill 2
eormsk          =forpnt+1
vartxt
opptr           !fill 2
opmask          !fill 1
grbpnt
tempf3
defpnt          !fill 2
dscpnt          !fill 2
token_saver                                             ; temp used by P1LINE/HELPSB (was spare????) [910628]
trmpos          !fill 1                                 ; temp used by SPC(), TAB()   [910628]

helper          !fill 1                                 ; P1LINE flag b7: HELP vs. LIST
;  b6: memory vs. file
;  b5: FIND/CHANGE
;  b4: highlight tokens
;  b3: highlight REM
;  b1: LINGET flag for AUTOSCROLL
;  b0: token in progress

jmper           !fill 1                                 ; 3 locations used by Function handler
                !fill 1                                 ;
oldov           !fill 1                                 ;

tempf1          !fill 1                                 ; used by math routines
ptarg1          =tempf1                                 ; multiply defined for INSTR thru FACexp
ptarg2          =tempf1+2                               ; (also used by Monitor Utility, thru lowtr)
str1            =tempf1+4
str2            =tempf1+7
positn          =tempf1+10
match           =tempf1+11

arypnt
highds          !fill 2
hightr          !fill 2

tempf2          !fill 1                                 ; used by math routines
deccnt          !fill 2
tenexp          = deccnt+1
grbtop
dptflg
lowtr           !fill 1
expsgn          !fill 1

fac                                                     ; Floating point accumulator (primary) FAC1
dsctmp
facexp          !fill 1
facho           !fill 1
facmoh          !fill 1
indice
facmo           !fill 1
faclo           !fill 1
facsgn          !fill 1
degree
sgnflg          !fill 1

argexp          !fill 1                                 ; Floating point accumulator (secondary) FAC2
argho           !fill 1
argmoh          !fill 1
argmo           !fill 1
arglo           !fill 1
argsgn          !fill 1

strng1
arisgn          !fill 1
facov           !fill 1

strng2
polypt
curtol
fbufpt          !fill 2

autinc          !fill 2                                 ; incremental value for AUTO (0=off)

z_p_temp_1      !fill 1                                 ; USING's leading zero counter
;GET, RENUMBER, KEY temporary
;MOVSPR, SPRITE, PLAY, VOL temporary
;MID$= temporary

hulp                                                    ; counter
keysiz          !fill 1

syntmp          !fill 1                                 ; used as temp all over the place
dsdesc          !fill 3                                 ; descriptor for DS$
tos             !fill 2                                 ; top of run time stack
runmod          !fill 1                                 ; flags run/direct(b7), load(b6), trace(b5), edit(b4) modes
; autoboot wedge (b0)
point                                                   ; USING's pointer to decimal point, 2 bytes used by AutoScroll
parsts          !fill 1                                 ; DOS parser status word
parstx          !fill 1                                 ; DOS parser status extensions

oldstk          !fill 1                                 ; BASIC saves uP stack pointer here

text_top        !fill 2                                 ; top of BASIC text pointer  (in text_bank)
text_bank       !fill 1                                 ; where BASIC text lives   (RAM0 default)
var_bank        !fill 1                                 ; where BASIC vars live   (RAM1 default)
sys_bank        = 0                                     ; where system space is  ???? (RAM0, make this a var?)

sid_speed_flag  !fill 1                                 ; saves system speed during SID ops (used during IRQ)

time                                                    ; temporaries for TI, TI$, SLEEP (4 bytes)
grapnt                                                  ; used by SPRSAV, RMOUSE, RCOLOR
op
column          !fill 1                                 ; temporaries for FIND/CHANGE, [L]INPUT, [L]READ, CURSOR
srow
fstr1           !fill 3                                 ;
fstr2           !fill 3                                 ;



; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
