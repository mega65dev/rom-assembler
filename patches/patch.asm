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
