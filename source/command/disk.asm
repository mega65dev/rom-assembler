


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