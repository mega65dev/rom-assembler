; Addresses of OS parameters referenced by BASIC:

_6510_data_reg  = $01
_bank           = $02                                   ; reg's for Kernel xxx_FAR routines (used by SYS)
_pchi           = $03
_pclo           = $04
_s_reg          = $05
_a_reg          = $06
_x_reg          = $07
_y_reg          = $08
_z_reg          = $09

_vicIRQ         = $a0                                   ; VIC IRQ flag register at time of IRQ
_starting_addr  = $ac                                   ; address BLOAD loaded to
_sa             = $b9                                   ; I/O channel secondary address
_fa             = $ba                                   ; I/O channel device number
_ndx            = $d0                                   ; number of characters in keyboard buffer
_kyndx          = $d1                                   ; fkey active flag
_mode           = $d7                                   ; 40/80 mode
_graphm         = $d8                                   ; graphic mode switch (multi/hires/split)
_pnt            = $e0                                   ; Editor screen address at cursor

_screen_bottom  = $e4                                   ; these describe the current window
_screen_top     = $e5
_screen_left    = $e6
_screen_right   = $e7

_color          = $f1                                   ; text color      [910722]
_autoinsert     = $f6                                   ; enable/disable auto insert mode
_locks          = $f7                                   ; Editor keyboard locks     [910722]

_keyd           = $02b0                                 ; keyboard buffer     [910710]
;_split = $0a34  ;line to start split at

number_fkeys    = 16                                    ; max of 14 prog. fn. keys
_pky_lengths    = $1000                                 ; table of prog. fn. key sizes
_pky_buffer     = _pky_lengths+number_fkeys             ; actual buffer

_restart_vector = $1100                                 ; Kernel restart vector
_pal_ntsc       = $1103                                 ; PAL=$ff, NTSC=$00 indicator    [910107]
_init_status    = $1104                                 ; msb set tells Kernel to let BASIC have IRQs
_default_drive  = $1106                                 ; system default disk drive
_expansion      = $1107                                 ; expansion RAM (# banks????)    [910107]
_sleep_counter  = $110c                                 ; binary frame counter maintained by Kernel  [910730]
_mouse_enable   = $1135                                 ; port# used by mouse (b7=port2, b6=port1, or both) [910107]
_mouse_pointer  = $1136                                 ; sprite pointer (sprite*2) by Kernel mouse driver "
_mouse_top      = $113b                                 ; margins for mouse pointer    "
_mouse_bottom   = $113c                                 ; "
_mouse_left     = $113d                                 ; "
_mouse_right    = $113e                                 ; "

; Addresses of I/O areas referenced by BASIC:

_red            = $d100                                 ; VIC palette (I/O block)
_green          = $d200
_blue           = $d300

; Addresses of Kernel entry points referenced by BASIC:

_print          = $e00c
_mouse          = $e01b                                 ; [910122]
_set_window     = $e02d
_palette_init   = $e027
_cursor         = $e030                                 ; [910228]
;_ldtb2 = $e033
;_ldtb1 = $e04c

_close_all      = $ff50                                 ; close all channels assigned to device .a
_go_64          = $ff53                                 ; C64 mode
_monitor        = $ff56                                 ; ML Monitor
_bootsys        = $ff59                                 ; Boot alternate OS     [910110]
_phoenix        = $ff5c                                 ; jump to 'post-BASIC initialize' routine
_lkupla         = $ff5f                                 ; find an available Logical Address
_lkupsa         = $ff62                                 ; find an available Secondary Address
_swapper        = $ff65                                 ; switch 80/40 column
_doakey         = $ff68                                 ; add/remove a definition from the p.f. key table
_setbank        = $ff6b                                 ; set bank for load/save/verify/open
_jsr_far        = $ff6e                                 ; call a subroutine in any bank
_jmp_far        = $ff71                                 ; jump to code in any bank
_lda_far        = $ff74                                 ; write a byte to any bank
_sta_far        = $ff77                                 ; read a byte from any bank
_cmp_far        = $ff7a                                 ; compare a byte to any bank
_primm          = $ff7d                                 ; print immediate

_setmsg         = $ff90
_readst         = $ffb7
_setlfs         = $ffba
_setnam         = $ffbd
_open           = $ffc0
_close          = $ffc3
_chkin          = $ffc6
_chkout         = $ffc9
_clrch          = $ffcc
_basin          = $ffcf
_bsout          = $ffd2
_loadsp         = $ffd5
_savesp         = $ffd8
_SetTime        = $ffdb
_ReadTime       = $ffde
_stop           = $ffe1
_getin          = $ffe4
_clall          = $ffe7
_screen_org     = $ffed
_plot           = $fff0

;.end


