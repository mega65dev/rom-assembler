; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      esc.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************
; Escape Command Tokens

esc_command_list
                !text "BAN",'K'+$80                     ; $02: set bank number
                !text "FILTE",'R'+$80                   ; $03: set up filter
                !text "PLA",'Y'+$80                     ; $04: play a tune
                !text "TEMP",'O'+$80                    ; $05: set rate for playing
                !text "MOVSP",'R'+$80                   ; $06: sprite position/movement
                !text "SPRIT",'E'+$80                   ; $07: turn on/set up sprite
                !text "SPRCOLO",'R'+$80                 ; $08: set sprite multicolor registers
                !text "RRE",'G'+$80                     ; $09: retreive register values after 'SYS'
                !text "ENVELOP",'E'+$80                 ; $0A: set up SID envelopes
                !text "SLEE",'P'+$80                    ; $0B: delay
                !text "CATALO",'G'+$80                  ; $0C: disk directory
                !text "DOPE",'N'+$80                    ; $0D: open a disk file
                !text "APPEN",'D'+$80                   ; $0E: open a disk file for appending
                !text "DCLOS",'E'+$80                   ; $0F: close a file opened w/ DOPEN
                !text "BSAV",'E'+$80                    ; $10: binary (non-program) save
                !text "BLOA",'D'+$80                    ; $11: binary load
                !text "RECOR",'D'+$80                   ; $12:
                !text "CONCA",'T'+$80                   ; $13: concatenate 2 files
                !text "DVERIF",'Y'+$80                  ; $14: verify a saved program
                !text "DCLEA",'R'+$80                   ; $15: re-initialize a drive
                !text "SPRSA",'V'+$80                   ; $16: sprite/string to sprite/string
                !text "COLLISIO",'N'+$80                ; $17: set traps for sprite & light pen collisions
                !text "BEGI",'N'+$80                    ; $18: mark start of a b-block
                !text "BEN",'D'+$80                     ; $19: ..and its end, too!
                !text "WINDO",'W'+$80                   ; $1A: set screen window
                !text "BOO",'T'+$80                     ; $1B: load&run ML or autoboot a disk
                !text "WIDT",'H'+$80                    ; $1C: single/double width drawing
                !text "SPRDE",'F'+$80                   ; $1D: define a sprite
                !text "QUI",'T'+$80                     ; $1E: (UNIMPLEMENTED)
                !text "DM",'A'+$80                      ; $1F: access memory
                !text ' '+$80                           ; $20: POISON - space character
                !text "DM",'A'+$80                      ; $21: access memory
                !text ' '+$80                           ; $22: POISON - quote character
                !text "DM",'A'+$80                      ; $23: access memory
                !text "OF",'F'+$80                      ; $24: KEY OFF
                !text "FAS",'T'+$80                     ; $25: go to 2 MHz. mode
                !text "SLO",'W'+$80                     ; $26: go to 1 MHz. mode
                !text "TYP",'E'+$80                     ; $27: type SEQ file
                !text "BVERIF",'Y'+$80                  ; $28: verify a saved program
                !text "ECTOR",'Y'+$80                   ; $29: dirECTORY
                !text "ERAS",'E'+$80                    ; $2A: alias for scratch
                !text "FIN",'D'+$80                     ; $2B: find string
                !text "CHANG",'E'+$80                   ; $2C: change string
                !text "SE",'T'+$80                      ; $2D:
                !text "SCREE",'N'+$80                   ; $2E:
                !text "POLYGO",'N'+$80                  ; $2F:
                !text "ELLIPS",'E'+$80                  ; $30:
                !text "VIEWPOR",'T'+$80                 ; $31:
                !text "GCOP",'Y'+$80                    ; $32:
                !text "PE",'N'+$80                      ; $33:
                !text "PALETT",'E'+$80                  ; $34:
                !text "DMOD",'E'+$80                    ; $35:
                !text "DPA",'T'+$80                     ; $36:
                !text "FORMA",'T'+$80                   ; $37: alias for HEADER command  [911017]
                !text "GENLOC",'K'+$80                  ; $38:     [910108]
                !text "FOREGROUN",'D'+$80               ; $39:     [910109]
                !text ' '+$80                           ; $3A: POISON - colon character  "
                !text "BACKGROUN",'D'+$80               ; $3B:     "
                !text "BORDE",'R'+$80                   ; $3C:     "
                !text "HIGHLIGH",'T'+$80                ; $3D:     "
                !text "MOUS",'E'+$80                    ; $3E:     [910122]
                !text "RMOUS",'E'+$80                   ; $3F: return coordinates of mouse [910123]
                !text "DIS",'K'+$80                     ; $40:     [910123]
                !text "CURSO",'R'+$80                   ; $41:     [910228]
                !text "RCURSO",'R'+$80                  ; $42: return cursor position  [910228]
                !text "LOADIF",'F'+$80                  ; $43: load IFF picture from disk [910402]
                !text "SAVEIF",'F'+$80                  ; $44: save IFF picture to   disk [910402]
                !text "EDI",'T'+$80                     ; $45: Edit mode on/off   [910620]

                !text 0                                 ; End marker
;(don't forget to change last_command_token!)

; Escape Function Tokens

esc_function_list
                !text "PO",'T'+$80                      ; $02: return paddle value
                !text "BUM",'P'+$80                     ; $03: read sprite collision
                !text "LPE",'N'+$80                     ; $04: read light pen value
                !text "RSPPO",'S'+$80                   ; $05: read sprite position
                !text "RSPRIT",'E'+$80                  ; $06: read sprite value
                !text "RSPCOLO",'R'+$80                 ; $07: read sprite multicolor value
                !text "XO",'R'+$80                      ; $08: exclusive or
                !text "RWINDO",'W'+$80                  ; $09: read window size
                !text "POINTE",'R'+$80                  ; $0a: return address of descriptor
                !text "MO",'D'+$80                      ; $0b: modulus    [910402]
                !text "PIXE",'L'+$80                    ; $0c: return BP data at pixel  [910820]
                !text "RPALETT",'E'+$80                 ; $0d: return RGB component of color [910820]
                !text 0

;.end

; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
