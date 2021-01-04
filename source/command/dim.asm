


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


dim3            jsr chkcom                              ; must be a comma

dim             tax                                     ; make .x non-zero (.a must be non-zero to work correctly)
                jsr ptrgt1
                jsr chrgot                              ; get last character
                bne dim3
                rts

;.end