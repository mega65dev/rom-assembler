; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      header.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************



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









; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
