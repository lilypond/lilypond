;; (name . '((downindexstring . upindexstring)
;;           follow-into-staff :: bool
;;           dir-relative-to-stem :: int
;;           force-dir :: int
;;           priority :: int


(set! script-alist
      (append 
      '(("accDiscant" . ((accordion "Discant" "") #f 0 1 0))
	("accDiscantF" . ((accordion "Discant" "F") #f 0 1 0))
	("accDiscantE" . ((accordion "Discant" "E") #f 0 1 0))
	("accDiscantEh" . ((accordion "Discant" "Eh") #f 0 1 0))
	("accDiscantFE" . ((accordion "Discant" "FE") #f 0 1 0))
	("accDiscantFEh" . ((accordion "Discant" "FEh") #f 0 1 0))
	("accDiscantEE" . ((accordion "Discant" "EE") #f 0 1 0))
	("accDiscantFEE" . ((accordion "Discant" "FEE") #f 0 1 0))
	("accDiscantEEE" . ((accordion "Discant" "EEE") #f 0 1 0))
	("accDiscantFEEE" . ((accordion "Discant" "FEEE") #f 0 1 0))
	("accDiscantS" . ((accordion "Discant" "S") #f 0 1 0))
	("accDiscantFS" . ((accordion "Discant" "FS") #f 0 1 0))
	("accDiscantES" . ((accordion "Discant" "ES") #f 0 1 0))
	("accDiscantEhS" . ((accordion "Discant" "EhS") #f 0 1 0))
	("accDiscantFES" . ((accordion "Discant" "FES") #f 0 1 0))
	("accDiscantFEhS" . ((accordion "Discant" "FEhS") #f 0 1 0))
	("accDiscantEES" . ((accordion "Discant" "EES") #f 0 1 0))
	("accDiscantFEES" . ((accordion "Discant" "FEES") #f 0 1 0))
	("accDiscantEEES" . ((accordion "Discant" "EEES") #f 0 1 0))
	("accDiscantFEEES" . ((accordion "Discant" "FEEES") #f 0 1 0))
	("accDiscantSS" . ((accordion "Discant" "SS") #f 0 1 0))
	("accDiscantESS" . ((accordion "Discant" "ESS") #f 0 1 0))
	("accDiscantEESS" . ((accordion "Discant" "EESS") #f 0 1 0))
	("accDiscantEEESS" . ((accordion "Discant" "EEESS") #f 0 1 0))
	("accFreebase" . ((accordion "Freebase" "") #f 0 -1 0))
	("accFreebaseF" . ((accordion "Freebase" "F") #f 0 -1 0))
	("accFreebaseE" . ((accordion "Freebase" "E") #f 0 -1 0))
	("accFreebaseFE" . ((accordion "Freebase" "FE") #f 0 -1 0))
	("accBayanbase" . ((accordion "Bayanbase" "") #f 0 -1 0))
	("accBayanbaseT" . ((accordion "Bayanbase" "T") #f 0 -1 0))
	("accBayanbaseE" . ((accordion "Bayanbase" "E") #f 0 -1 0))
	("accBayanbaseTE" . ((accordion "Bayanbase" "TE") #f 0 -1 0))
	("accBayanbaseEE" . ((accordion "Bayanbase" "EE") #f 0 -1 0))
	("accBayanbaseTEE" . ((accordion "Bayanbase" "TFEE") #f 0 -1 0))
;; I don't know what naming of standard base registers is best?
;; The 'tenor-piano' style of names has been used in some old accordion
;; scores I have seen. But you never name the registers. These four are
;; the symbols that are most used then typesetting music, but in the real
;; world it differs a lot from instrument to instrument what registers
;; are available.
	("accStdbase" . ((accordion "Stdbase" "") #f 0 -1 0))
;; tenor-piano
	("accStdbaseTp" . ((accordion "Stdbase" "FE") #f 0 -1 0))
;; tenor
	("accStdbaseT" . ((accordion "Stdbase" "TFE") #f 0 -1 0))
;; bass-piano
	("accStdbaseBp" . ((accordion "Stdbase" "MES") #f 0 -1 0))
;; master  (changed from accStdbaseM)
	("accStdbaseMa" . ((accordion "Stdbase" "TFMES") #f 0 -1 0))

	("accStdbaseFE" . ((accordion "Stdbase" "FE") #f 0 -1 0))
	("accStdbaseTFE" . ((accordion "Stdbase" "TFE") #f 0 -1 0))
	("accStdbaseMES" . ((accordion "Stdbase" "MES") #f 0 -1 0))
	("accStdbaseTFMES" . ((accordion "Stdbase" "TFMES") #f 0 -1 0))

	("accSB" . ((accordion "SB" "") #f 0 -1 0))
	("accBB" . ((accordion "BB" "") #f 0 -1 0))
	("accOldEE" . ((accordion "OldEE" "") #f 0 -1 0))
	("accOldEES" . ((accordion "OldEES" "") #f 0 -1 0)))
      script-alist)
      )

