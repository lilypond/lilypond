;; (name . '((downindexstring . upindexstring)
;;           follow-into-staff :: bool
;;           dir-relative-to-stem :: int
;;           force-dir :: int
;;           priority :: int

;;TODO?      extra padding :: Real (given in staff spaces)

(set! script-alist
      (append 
      '(("thumb" . (("thumb"  . "thumb") #f 0 1 0))
	("accent" . (("sforzato" .  "sforzato") #f -1 0 0))
	("marcato" . (("dmarcato" . "umarcato") #f -1 0  0))
	("staccatissimo" . (("dstaccatissimo" . "ustaccatissimo") #f  -1 0 0 0))
	("portato" . (("dportato" . "uportato") #f -1 0 0))
	("fermata" . (("dfermata" . "ufermata") #f 1 0 0))
	("stopped" . (("stopped" . "stopped") #f 0 1 0))
	("staccato" . (("staccato" . "staccato") #t -1 0 -100))
	("tenuto" . (("tenuto" . "tenuto") 1 -1 0 0))
	("upbow" . (("upbow" . "upbow") #f 0 1 0))
	("downbow" . (("downbow" . "downbow") #f 0 1 0))
	("lheel" . (("upedalheel" . "upedalheel") #f 0 -1  0))
	("rheel" . (("dpedalheel" . "dpedalheel") #f 0 1 0))
	("ltoe" . (("upedaltoe" . "upedaltoe") #f 0 -1 0))
	("rtoe" . (("dpedaltoe" . "dpedaltoe") #f 0 1 0))
	("turn" . (("turn" . "turn") #f 0 1 0))
	("open" . (("open" . "open") #f 0 1 0))
	("flageolet" . (("flageolet" . "flageolet")  0 0 1 0))
	("reverseturn" . (("reverseturn" . "reverseturn")   0 0 1 0))
	("trill" . (("trill" . "trill") #f 0 1 2000))
	("prall" . (("prall" . "prall") #f 0 1 0))
	("mordent" . (("mordent" . "mordent") #f 0 1 0))
	("prallprall" . (("prallprall" . "prallprall") #f 0 1 0))
	("prallmordent" . (("prallmordent" . "prallmordent") #f 0 1 0))
	("upprall" . (("upprall" . "upprall") #f 0 1 0))
	("downprall" . (("downprall" . "downprall") #f 0 1 0)))
      script-alist)
      )
