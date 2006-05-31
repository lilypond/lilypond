;;;; script.scm -- Script definitions
;;;;
;;;; source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2000--2006 Han-Wen Nienhuys <hanwen@cs.uu.nl>

(set! default-script-alist
      (append 
       '(("thumb" .
	  ((script-stencil . (feta . ("thumb"  . "thumb")))
	   (direction . 1)))
         ("accent" .
	  ((avoid-slur . around)
	   (quantize-position . #t)
	   (script-stencil . (feta . ("sforzato" .  "sforzato")))
	   (side-relative-direction . -1)))
         ("espressivo" .
	  ((avoid-slur . around)
	   (quantize-position . #t)
	   (script-stencil . (feta . ("espr" .  "espr")))
	   (side-relative-direction . -1)))	 
	 ("marcato" .
	  ((script-stencil . (feta . ("dmarcato" . "umarcato")))
;	   (staff-padding . ())
	   (quantize-position . #t)
	   (side-relative-direction .  -1)))
	 ("staccatissimo" .
	  ((avoid-slur . inside) 
	   (script-stencil . (feta . ("dstaccatissimo" . "ustaccatissimo")))
	   (side-relative-direction .  -1)))
	 
	 ("portato" .
	  ((script-stencil . (feta . ("uportato" . "dportato")))
	   (side-relative-direction . -1)))

	 ("accentus" .
	  ((script-stencil . (feta . ("uaccentus" . "uaccentus")))
	   (side-relative-direction .  -1)
	   (quantize-position . #t)
	   (script-priority . -100)
	   (direction  . 1)))
	 ("ictus" .
	  ((script-stencil . (feta . ("ictus" . "ictus")))
	   (side-relative-direction .  -1)
	   (quantize-position . #t)
	   (script-priority . -100)
	   (direction  . -1)))
	 ("semicirculus" .
	  ((script-stencil . (feta . ("dsemicirculus" . "dsemicirculus")))
	   (side-relative-direction .  -1)
	   (quantize-position . #t)
	   (script-priority . -100)
	   (direction  . 1)))
	 ("circulus" .
	  ((script-stencil . (feta . ("circulus" . "circulus")))
	   (side-relative-direction .  -1)
	   (quantize-position . #t)
	   (script-priority . -100)
	   (direction  . 1)))

	 ("signumcongruentiae" .
	  ((script-stencil . (feta . ("dsignumcongruentiae" . "usignumcongruentiae")))
	   (direction .  1)))
	 ("fermata" .
	  ((script-stencil . (feta . ("dfermata" . "ufermata")))
	   (avoid-slur . around)
	   (script-priority . 4000)
	   (direction .  1)))
	 ("shortfermata" .
	  ((script-stencil . (feta . ("dshortfermata" . "ushortfermata")))
	   (direction .  1)))
	 ("longfermata" .
	  ((script-stencil . (feta . ("dlongfermata" . "ulongfermata")))
	   (direction .  1)))
	 ("verylongfermata" .
	  ((script-stencil . (feta . ("dverylongfermata" . "uverylongfermata")))
	   (direction .  1)))
	 ("stopped" .
	  ((script-stencil . (feta . ("stopped" . "stopped")))
	   (direction  . 1)))
	 ("staccato" .
	  ((script-stencil . (feta . ("staccato" . "staccato")))
	   (side-relative-direction .  -1)
	   (quantize-position . #t)
	   (avoid-slur . inside) 
	   (script-priority . -100)))
	 ("tenuto" .
	  ((script-stencil . (feta . ("tenuto" . "tenuto")))
	   (quantize-position . #t)
	   (avoid-slur . inside)
	   (side-relative-direction . -1)))
	 ("comma" .
	  ((script-stencil . (feta . ("lcomma" . "rcomma")))
	   (quantize-position . #t)
	   (direction . 1)))
	 ("varcomma" .
	  ((script-stencil . (feta . ("lvarcomma" . "rvarcomma")))
	   (quantize-position . #t)
	   (direction . 1)))
	 ("upbow" .
	  ((script-stencil . (feta . ("upbow" . "upbow")))
	   (avoid-slur . around)
	   (direction  . 1)))
	 ("downbow" .
	  ((script-stencil . (feta . ("downbow" . "downbow")))
	   (avoid-slur . around)
	   (direction  . 1)))
	 ("lheel" .
	  ((script-stencil . (feta . ("upedalheel" . "upedalheel")))
	   (direction .  -1))
	  )
	 ("rheel" .
	  ((script-stencil . (feta . ("dpedalheel" . "dpedalheel")))
	   (direction  . 1)))
	 ("ltoe" .
	  ((script-stencil . (feta . ("upedaltoe" . "upedaltoe")))
	   (direction  . -1)))
	 ("rtoe" .
	  ((script-stencil . (feta . ("dpedaltoe" . "dpedaltoe")))
	   (direction  . 1)))
	 ("turn" .
	  ((script-stencil . (feta . ("turn" . "turn")))
	   (direction  . 1)))
	 ("open" .
	  ((avoid-slur . outside)
		 (script-stencil . (feta . ("open" . "open")))
	   (direction  . 1)))
	 ("flageolet" .
	  ((script-stencil . (feta . ("flageolet" . "flageolet")))
	   (direction  . 1)))
	 ("reverseturn" .
	  ((script-stencil . (feta . ("reverseturn" . "reverseturn")))
	   (direction  . 1)))
	 ("trill" .
	  ((script-stencil . (feta . ("trill" . "trill")))
	   (direction . 1)
	   (avoid-slur . outside)
	   (script-priority . 2000)))
	 ("prall" .
	  ((script-stencil . (feta . ("prall" . "prall")))
	   (direction  . 1)))
	 ("mordent" .
	  ((script-stencil . (feta . ("mordent" . "mordent")))
	   (direction  . 1)))
	 ("prallprall" .
	  ((script-stencil . (feta . ("prallprall" . "prallprall")))
	   (direction  . 1)))
	 ("prallmordent" .
	  ((script-stencil . (feta . ("prallmordent" . "prallmordent")))
	   (direction  . 1)))
	 ("upprall" .
	  ((script-stencil . (feta . ("upprall" . "upprall")))
	   (direction  . 1)))
	 ("downprall" .
	  ((script-stencil . (feta . ("downprall" . "downprall")))
	   (direction  . 1)))
	 ("upmordent" .
	  ((script-stencil . (feta . ("upmordent" . "upmordent")))
	   (direction  . 1)))
	 ("downmordent" .
	  ((script-stencil . (feta . ("downmordent" . "downmordent")))
	   (direction  . 1)))
	 ("lineprall" .
	  ((script-stencil . (feta . ("lineprall" . "lineprall")))
	   (direction  . 1)))
	 ("pralldown" .
	  ((script-stencil . (feta . ("pralldown" . "pralldown")))
	   (direction  . 1)))
	 ("prallup" .
	  ((script-stencil . (feta . ("prallup" . "prallup")))
	   (direction  . 1)))
	 ("segno" .
	  ((script-stencil . (feta . ("segno" . "segno")))
	   (direction  . 1)))
	 ("coda" .
	  ((script-stencil . (feta . ("coda" . "coda")))
	   (direction  . 1)))
	 ("varcoda" .
	  ((script-stencil . (feta . ("varcoda" . "varcoda")))
	   (direction  . 1))))
       default-script-alist)
      )
