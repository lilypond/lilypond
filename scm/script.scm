
(set! default-script-alist
      (append 
       '(("thumb" .
	  (
	   (script-molecule . (feta . ("thumb"  . "thumb")))
	   (direction . 1)))
	 ("accent" .
	  (
	   (script-molecule . (feta . ("sforzato" .  "sforzato")))
	   (side-relative-direction . -1))
	  )	 
	 ("marcato" .
	  (
	   (script-molecule . (feta . ("dmarcato" . "umarcato")))
	   (side-relative-direction .  -1)))
	 ("staccatissimo" .
	  (
	   (script-molecule . (feta . ("dstaccatissimo" . "ustaccatissimo")))
	   (side-relative-direction .  -1)))
	 
	 ("portato" .
	  ((script-molecule . (feta . ("dportato" . "uportato")))
	   (side-relative-direction . -1)))

	 ("signumcongruentiae" .
	  ((script-molecule . (feta . ("dsignumcongruentiae" . "usignumcongruentiae")))
	   (direction .  1)))
	 ("fermata" .
	  ((script-molecule . (feta . ("dfermata" . "ufermata")))
	   (direction .  1)))
	 ("shortfermata" .
	  ((script-molecule . (feta . ("dshortfermata" . "ushortfermata")))
	   (direction .  1)))
	 ("longfermata" .
	  ((script-molecule . (feta . ("dlongfermata" . "ulongfermata")))
	   (direction .  1)))
	 ("verylongfermata" .
	  ((script-molecule . (feta . ("dverylongfermata" . "uverylongfermata")))
	   (direction .  1)))
	 ("stopped" .
	  ((script-molecule . (feta . ("stopped" . "stopped")))
	   (direction  . 1) ))
	 ("staccato" .
	  ((script-molecule . (feta . ("staccato" . "staccato")))
	   (side-relative-direction .  -1)
	   (follow-into-staff  . #t)
	   (priority . -100)))
	 ("tenuto" .
	  ((script-molecule . (feta . ("tenuto" . "tenuto")))
	   (follow-into-staff . #t)
	   (side-relative-direction . -1)))
	 ("comma" .
	  ((script-molecule . (feta . ("lcomma" . "rcomma")))
	   (follow-into-staff . #t)
	   (direction . 1)))
	 ("varcomma" .
	  ((script-molecule . (feta . ("lvarcomma" . "rvarcomma")))
	   (follow-into-staff . #t)
	   (direction . 1)))
	 ("upbow" .
	  ((script-molecule . (feta . ("upbow" . "upbow")))
	   (direction  . 1) ))
	 ("downbow" .
	  ((script-molecule . (feta . ("downbow" . "downbow")))
	   (direction  . 1) ))
	 ("lheel" .
	  ((script-molecule . (feta . ("upedalheel" . "upedalheel")))
	   (direction .  -1))
	  )
	 ("rheel" .
	  ((script-molecule . (feta . ("dpedalheel" . "dpedalheel")))
	   (direction  . 1) ))
	 ("ltoe" .
	  ((script-molecule . (feta . ("upedaltoe" . "upedaltoe")))
	   (direction  . -1) ))
	 ("rtoe" .
	  ((script-molecule . (feta . ("dpedaltoe" . "dpedaltoe")))
	   (direction  . 1) ))
	 ("turn" .
	  ((script-molecule . (feta . ("turn" . "turn")))
	   (direction  . 1) ))
	 ("open" .
	  ((script-molecule . (feta . ("open" . "open")))
	   (direction  . 1) ))
	 ("flageolet" .
	  ((script-molecule . (feta . ("flageolet" . "flageolet")))
	   (direction  . 1) ))
	 ("reverseturn" .
	  ((script-molecule . (feta . ("reverseturn" . "reverseturn")))
	   (direction  . 1) ))
	 ("trill" .
	  ((script-molecule . (feta . ("trill" . "trill")))
	   (direction . 1)
	   (priority . 2000)))
	 ("prall" .
	  ((script-molecule . (feta . ("prall" . "prall")))
	   (direction  . 1) ))
	 ("mordent" .
	  ((script-molecule . (feta . ("mordent" . "mordent")))
	   (direction  . 1) ))
	 ("prallprall" .
	  ((script-molecule . (feta . ("prallprall" . "prallprall")))
	   (direction  . 1) ))
	 ("prallmordent" .
	  ((script-molecule . (feta . ("prallmordent" . "prallmordent")))
	   (direction  . 1) ))
	 ("upprall" .
	  ((script-molecule . (feta . ("upprall" . "upprall")))
	   (direction  . 1) ))
	 ("downprall" .
	  ((script-molecule . (feta . ("downprall" . "downprall")))
	   (direction  . 1) ))
	 ("upmordent" .
	  ((script-molecule . (feta . ("upmordent" . "upmordent")))
	   (direction  . 1) ))
	 ("downmordent" .
	  ((script-molecule . (feta . ("downmordent" . "downmordent")))
	   (direction  . 1) ))
	 ("lineprall" .
	  ((script-molecule . (feta . ("lineprall" . "lineprall")))
	   (direction  . 1) ))
	 ("pralldown" .
	  ((script-molecule . (feta . ("pralldown" . "pralldown")))
	   (direction  . 1) ))
	 ("prallup" .
	  ((script-molecule . (feta . ("prallup" . "prallup")))
	   (direction  . 1) ))
	 ("segno" .
	  ((script-molecule . (feta . ("segno" . "segno")))
	   (direction  . 1) ))
	 ("coda" .
	  ((script-molecule . (feta . ("coda" . "coda")))
	   (direction  . 1) ))
	 ("varcoda" .
	  ((script-molecule . (feta . ("varcoda" . "varcoda")))
	   (direction  . 1) )))
       default-script-alist)
      )
