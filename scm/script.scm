;; (name . '((downindexstring . upindexstring)
;;           follow-into-staff :: bool
;;           dir-relative-to-stem :: int
;;           force-dir :: int
;;           priority :: int

;;TODO?      extra padding :: Real (given in staff spaces)

(set! script-alist
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
      script-alist)
      )

