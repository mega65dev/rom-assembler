

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