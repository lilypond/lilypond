;; (name . '((downindexstring . upindexstring)
;;           follow-into-staff :: bool
;;           dir-relative-to-stem :: int
;;           force-dir :: int
;;           priority :: int

;;TODO?      extra padding :: Real (given in staff spaces)


;; TODO: generate Grob types for this, by prepending appropriate props
;; on top of Script (?)  

(set! default-script-alist
      (append 
      '(("thumb" . ((feta . ("thumb"  . "thumb")) #f 0 1 0))
	("accent" . ((feta . ("sforzato" .  "sforzato")) #f -1 0 0))
	("marcato" . ((feta . ("dmarcato" . "umarcato")) #f -1 0  0))
	("staccatissimo" . ((feta . ("dstaccatissimo" . "ustaccatissimo")) #f  -1 0 0))
	("portato" . ((feta . ("dportato" . "uportato")) #f -1 0 0))
	("fermata" . ((feta . ("dfermata" . "ufermata")) #f 1 0 0))
	("stopped" . ((feta . ("stopped" . "stopped")) #f 0 1 0))
	("staccato" . ((feta . ("staccato" . "staccato")) #t -1 0 -100))
	("tenuto" . ((feta . ("tenuto" . "tenuto")) #t -1 0 0))
	("upbow" . ((feta . ("upbow" . "upbow")) #f 0 1 0))
	("downbow" . ((feta . ("downbow" . "downbow")) #f 0 1 0))
	("lheel" . ((feta . ("upedalheel" . "upedalheel")) #f 0 -1  0))
	("rheel" . ((feta . ("dpedalheel" . "dpedalheel")) #f 0 1 0))
	("ltoe" . ((feta . ("upedaltoe" . "upedaltoe")) #f 0 -1 0))
	("rtoe" . ((feta . ("dpedaltoe" . "dpedaltoe")) #f 0 1 0))
	("turn" . ((feta . ("turn" . "turn")) #f 0 1 0))
	("open" . ((feta . ("open" . "open")) #f 0 1 0))
	("flageolet" . ((feta . ("flageolet" . "flageolet")) #f 0 1 0))
	("reverseturn" . ((feta . ("reverseturn" . "reverseturn")) #f 0 1 0))
	("trill" . ((feta . ("trill" . "trill")) #f 0 1 2000))
	("prall" . ((feta . ("prall" . "prall")) #f 0 1 0))
	("mordent" . ((feta . ("mordent" . "mordent")) #f 0 1 0))
	("prallprall" . ((feta . ("prallprall" . "prallprall")) #f 0 1 0))
	("prallmordent" . ((feta . ("prallmordent" . "prallmordent")) #f 0 1 0))
	("upprall" . ((feta . ("upprall" . "upprall")) #f 0 1 0))
	("downprall" . ((feta . ("downprall" . "downprall")) #f 0 1 0))
	("segno" . ((feta . ("segno" . "segno")) #f 0 1 0))
	("coda" . ((feta . ("coda" . "coda")) #f 0 1 0)))
      default-script-alist)
      )




(set! default-script-alist
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
	("accBayanbaseTEE" . ((accordion "Bayanbase" "TEE") #f 0 -1 0))
	("accStdbase" . ((accordion "Stdbase" "") #f 0 -1 0))
	("accStdbaseFE" . ((accordion "Stdbase" "FE") #f 0 -1 0))
	("accStdbaseTFE" . ((accordion "Stdbase" "TFE") #f 0 -1 0))
	("accStdbaseMES" . ((accordion "Stdbase" "MES") #f 0 -1 0))
	("accStdbaseTFMES" . ((accordion "Stdbase" "TFMES") #f 0 -1 0))

	("accSB" . ((accordion "SB" "") #f 0 -1 0))
	("accBB" . ((accordion "BB" "") #f 0 -1 0))
	("accOldEE" . ((accordion "OldEE" "") #f 0 -1 0))
	("accOldEES" . ((accordion "OldEES" "") #f 0 -1 0)))
      default-script-alist)
      )


