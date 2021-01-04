; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      keywords.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************

keyword_list
                !text "EN",'D'+$80                      ; $80
                !text "FO",'R'+$80                      ; $81
                !text "NEX",'T'+$80                     ; $82
                !text "DAT",'A'+$80                     ; $83
                !text "INPUT",'#'+$80                   ; $84
                !text "INPU",'T'+$80                    ; $85
                !text "DI",'M'+$80                      ; $86
                !text "REA",'D'+$80                     ; $87
                !text "LE",'T'+$80                      ; $88
                !text "GOT",'O'+$80                     ; $89
                !text "RU",'N'+$80                      ; $8A
                !text "I",'F'+$80                       ; $8B
                !text "RESTOR",'E'+$80                  ; $8C
                !text "GOSU",'B'+$80                    ; $8D
                !text "RETUR",'N'+$80                   ; $8E
                !text "RE",'M'+$80                      ; $8F
                !text "STO",'P'+$80                     ; $90
                !text "O",'N'+$80                       ; $91
                !text "WAI",'T'+$80                     ; $92
                !text "LOA",'D'+$80                     ; $93
                !text "SAV",'E'+$80                     ; $94
                !text "VERIF",'Y'+$80                   ; $95
                !text "DE",'F'+$80                      ; $96
                !text "POK",'E'+$80                     ; $97
                !text "PRINT",'#'+$80                   ; $98
                !text "PRIN",'T'+$80                    ; $99
                !text "CON",'T'+$80                     ; $9A
                !text "LIS",'T'+$80                     ; $9B
                !text "CL",'R'+$80                      ; $9C
                !text "CM",'D'+$80                      ; $9D
                !text "SY",'S'+$80                      ; $9E
                !text "OPE",'N'+$80                     ; $9F
                !text "CLOS",'E'+$80                    ; $A0
                !text "GE",'T'+$80                      ; $A1
                !text "NE",'W'+$80                      ; $A2
                !text "TAB",'('+$80                     ; $A3
                !text "T",'O'+$80                       ; $A4
                !text "F",'N'+$80                       ; $A5
                !text "SPC",'('+$80                     ; $A6
                !text "THE",'N'+$80                     ; $A7
                !text "NO",'T'+$80                      ; $A8
                !text "STE",'P'+$80                     ; $A9
                !text '+'+$80                           ; $AA operators
                !text '-'+$80                           ; $AB
                !text '*'+$80                           ; $AC
                !text '/'+$80                           ; $AD
                !text '^'+$80                           ; $AE
                !text "AN",'D'+$80                      ; $AF
                !text "O",'R'+$80                       ; $B0
                !text '>'+$80                           ; $B1
                !text '='+$80                           ; $B2
                !text '<'+$80                           ; $B3
                !text "SG",'N'+$80                      ; $B4 first numeric function
                !text "IN",'T'+$80                      ; $B5
                !text "AB",'S'+$80                      ; $B6
                !text "US",'R'+$80                      ; $B7
                !text "FR",'E'+$80                      ; $B8
                !text "PO",'S'+$80                      ; $B9
                !text "SQ",'R'+$80                      ; $BA
                !text "RN",'D'+$80                      ; $BB
                !text "LO",'G'+$80                      ; $BC
                !text "EX",'P'+$80                      ; $BD
                !text "CO",'S'+$80                      ; $BE
                !text "SI",'N'+$80                      ; $BF
                !text "TA",'N'+$80                      ; $C0
                !text "AT",'N'+$80                      ; $C1
                !text "PEE",'K'+$80                     ; $C2
                !text "LE",'N'+$80                      ; $C3
                !text "STR",'$'+$80                     ; $C4
                !text "VA",'L'+$80                      ; $C5
                !text "AS",'C'+$80                      ; $C6 last numeric function
                !text "CHR",'$'+$80                     ; $C7 last single-arg function
                !text "LEFT",'$'+$80                    ; $C8
                !text "RIGHT",'$'+$80                   ; $C9
                !text "MID",'$'+$80                     ; $CA
                !text "G",'O'+$80                       ; $CB
; beginning of new C128 keywords------------
                !text "RGRAPHI",'C'+$80                 ; $CC was 'rgr'   [910701]
                !text "RCOLO",'R'+$80                   ; $CD was 'rclr'   [910701]
                !text $80                               ; $CE null to skip over escape_function token
                !text "JO",'Y'+$80                      ; $CF
                !text "RPE",'N'+$80                     ; $D0 (was rdot in c128)
                !text "DE",'C'+$80                      ; $D1
                !text "HEX",'$'+$80                     ; $D2
                !text "ERR",'$'+$80                     ; $D3
                !text "INST",'R'+$80                    ; $D4 last function

                !text "ELS",'E'+$80                     ; $D5
                !text "RESUM",'E'+$80                   ; $D6
                !text "TRA",'P'+$80                     ; $D7
                !text "TRO",'N'+$80                     ; $D8
                !text "TROF",'F'+$80                    ; $D9
                !text "SOUN",'D'+$80                    ; $DA
                !text "VO",'L'+$80                      ; $DB
                !text "AUT",'O'+$80                     ; $DC
                !text "PUDE",'F'+$80                    ; $DD
                !text "GRAPHI",'C'+$80                  ; $DE
                !text "PAIN",'T'+$80                    ; $DF
                !text "CHA",'R'+$80                     ; $E0
                !text "BO",'X'+$80                      ; $E1
                !text "CIRCL",'E'+$80                   ; $E2
                !text "PAST",'E'+$80                    ; $E3 (was gshape in C128)
                !text "CU",'T'+$80                      ; $E4 (was sshape in C128)
                !text "LIN",'E'+$80                     ; $E5 (was draw in C128)
                !text "LOCAT",'E'+$80                   ; $E6
                !text "COLO",'R'+$80                    ; $E7
                !text "SCNCL",'R'+$80                   ; $E8
                !text "SCAL",'E'+$80                    ; $E9
                !text "HEL",'P'+$80                     ; $EA
                !text "D",'O'+$80                       ; $EB
                !text "LOO",'P'+$80                     ; $EC
                !text "EXI",'T'+$80                     ; $ED
                !text "DI",'R'+$80                      ; $EE
                !text "DSAV",'E'+$80                    ; $EF
                !text "DLOA",'D'+$80                    ; $F0
                !text "HEADE",'R'+$80                   ; $F1
                !text "SCRATC",'H'+$80                  ; $F2
                !text "COLLEC",'T'+$80                  ; $F3
                !text "COP",'Y'+$80                     ; $F4
                !text "RENAM",'E'+$80                   ; $F5
                !text "BACKU",'P'+$80                   ; $F6
                !text "DELET",'E'+$80                   ; $F7
                !text "RENUMBE",'R'+$80                 ; $F8
                !text "KE",'Y'+$80                      ; $F9
                !text "MONITO",'R'+$80                  ; $FA
                !text "USIN",'G'+$80                    ; $FB
                !text "UNTI",'L'+$80                    ; $FC
                !text "WHIL",'E'+$80                    ; $FD
                !text 0                                 ; $FE skip over the escape_command token

;.end





; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
