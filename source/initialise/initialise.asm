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