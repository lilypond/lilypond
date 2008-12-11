;;;; script.scm -- Script definitions
;;;;
;;;; source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2000--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>

(define-public default-script-alist
  '(("thumb" .
     ((script-stencil . (feta . ("thumb"  . "thumb")))
      (avoid-slur . inside)
      (padding . 0.20)	   
      (direction . 1)))
    ("accent" .
     ((avoid-slur . around)
      (padding . 0.20)	   
      (quantize-position . #t)
      (script-stencil . (feta . ("sforzato" .  "sforzato")))
      (side-relative-direction . -1)))
    ("espressivo" .
     ((avoid-slur . around)
      (padding . 0.20)	   
      (quantize-position . #t)
      (script-stencil . (feta . ("espr" .  "espr")))
      (side-relative-direction . -1)))	 
    ("marcato" .
     ((script-stencil . (feta . ("dmarcato" . "umarcato")))
      (padding . 0.20)
      (avoid-slur . inside)
					;	   (staff-padding . ())
      (quantize-position . #t)
      (side-relative-direction .  -1)))
    ("staccatissimo" .
     ((avoid-slur . inside) 
      (script-stencil . (feta . ("dstaccatissimo" . "ustaccatissimo")))
      (padding . 0.20)	   
      (side-relative-direction .  -1)))
    
    ("portato" .
     ((script-stencil . (feta . ("uportato" . "dportato")))
      (avoid-slur . around)
      (slur-padding . 0.3)
      (padding . 0.45)
      (side-relative-direction . -1)))

    ("accentus" .
     ((script-stencil . (feta . ("uaccentus" . "uaccentus")))
      (side-relative-direction .  -1)
      (avoid-slur . #f)
      (padding . 0.20)	   
      (quantize-position . #t)
      (script-priority . -100)
      (direction  . 1)))
    ("ictus" .
     ((script-stencil . (feta . ("ictus" . "ictus")))
      (side-relative-direction .  -1)
      (quantize-position . #t)
      (avoid-slur . #f)
      (padding . 0.20)	   
      (script-priority . -100)
      (direction  . -1)))
    ("semicirculus" .
     ((script-stencil . (feta . ("dsemicirculus" . "dsemicirculus")))
      (side-relative-direction .  -1)
      (quantize-position . #t)
      (avoid-slur . #f)
      (padding . 0.20)	   
      (script-priority . -100)
      (direction  . 1)))
    ("circulus" .
     ((script-stencil . (feta . ("circulus" . "circulus")))
      (side-relative-direction .  -1)
      (avoid-slur . #f)
      (padding . 0.20)	   
      (quantize-position . #t)
      (script-priority . -100)
      (direction  . 1)))

    ("signumcongruentiae" .
     ((script-stencil . (feta . ("dsignumcongruentiae" . "usignumcongruentiae")))
      (padding . 0.20)	   
      (avoid-slur . outside)
      (direction .  1)))
    ("fermata" .
     ((script-stencil . (feta . ("dfermata" . "ufermata")))
      (padding . 0.20)	   
      (avoid-slur . around)
      (script-priority . 4000)
      (direction .  1)))
    ("shortfermata" .
     ((script-stencil . (feta . ("dshortfermata" . "ushortfermata")))
      (padding . 0.20)	   
      (avoid-slur . around)
      (direction .  1)))
    ("longfermata" .
     ((script-stencil . (feta . ("dlongfermata" . "ulongfermata")))
      (padding . 0.20)	   
      (avoid-slur . around)
      (direction .  1)))
    ("verylongfermata" .
     ((script-stencil . (feta . ("dverylongfermata" . "uverylongfermata")))
      (padding . 0.20)	   
      (avoid-slur . around)
      (direction .  1)))
    ("stopped" .
     ((script-stencil . (feta . ("stopped" . "stopped")))
      (avoid-slur . inside)
      (padding . 0.20)	   
      (direction  . 1)))
    ("staccato" .
     ((script-stencil . (feta . ("staccato" . "staccato")))
      (side-relative-direction .  -1)
      (quantize-position . #t)
      (avoid-slur . inside) 
      (padding . 0.20)	   
      (script-priority . -100)))
    ("tenuto" .
     ((script-stencil . (feta . ("tenuto" . "tenuto")))
      (quantize-position . #t)
      (avoid-slur . inside)
      (padding . 0.20)	   
      (side-relative-direction . -1)))
    ("comma" .
     ((script-stencil . (feta . ("lcomma" . "rcomma")))
      (quantize-position . #t)
      (padding . 0.20)	   
      (avoid-slur . #f)
      (direction . 1)))
    ("varcomma" .
     ((script-stencil . (feta . ("lvarcomma" . "rvarcomma")))
      (quantize-position . #t)
      (padding . 0.20)	   
      (avoid-slur . #f)
      (direction . 1)))
    ("upbow" .
     ((script-stencil . (feta . ("upbow" . "upbow")))
      (avoid-slur . around)
      (padding . 0.20)	   
      (direction  . 1)))
    ("downbow" .
     ((script-stencil . (feta . ("downbow" . "downbow")))
      (padding . 0.20)	   
      (avoid-slur . around)
      (direction  . 1)))
    ("lheel" .
     ((script-stencil . (feta . ("upedalheel" . "upedalheel")))
      (padding . 0.20)	   
      (avoid-slur . around) ;guessing?
      (direction .  -1))
     )
    ("rheel" .
     ((script-stencil . (feta . ("dpedalheel" . "dpedalheel")))
      (padding . 0.20)	   
      (avoid-slur . around) ;guessing?
      (direction  . 1)))
    ("ltoe" .
     ((script-stencil . (feta . ("upedaltoe" . "upedaltoe")))
      (padding . 0.20)	   
      (avoid-slur . around) ;guessing?
      (direction  . -1)))
    ("rtoe" .
     ((script-stencil . (feta . ("dpedaltoe" . "dpedaltoe")))
      (padding . 0.20)	   
      (avoid-slur . around) ;guessing?
      (direction  . 1)))
    ("turn" .
     ((script-stencil . (feta . ("turn" . "turn")))
      (avoid-slur . inside)
      (padding . 0.20)	   
      (direction  . 1)))
    ("open" .
     ((avoid-slur . outside)
      (padding . 0.20)	   
      (script-stencil . (feta . ("open" . "open")))
      (direction  . 1)))
    ("flageolet" .
     ((script-stencil . (feta . ("flageolet" . "flageolet")))
      (padding . 0.20)	   
      (avoid-slur . around) ;guessing?
      (direction  . 1)))
    ("reverseturn" .
     ((script-stencil . (feta . ("reverseturn" . "reverseturn")))
      (padding . 0.20)	   
      (avoid-slur . inside)
      (direction  . 1)))
    ("trill" .
     ((script-stencil . (feta . ("trill" . "trill")))
      (direction . 1)
      (padding . 0.20)	   
      (avoid-slur . outside)
      (script-priority . 2000)))
    ("prall" .
     ((script-stencil . (feta . ("prall" . "prall")))
      (padding . 0.20)	   
      (avoid-slur . around)
      (direction  . 1)))
    ("mordent" .
     ((script-stencil . (feta . ("mordent" . "mordent")))
      (padding . 0.20)	   
      (avoid-slur . around)
      (direction  . 1)))
    ("prallprall" .
     ((script-stencil . (feta . ("prallprall" . "prallprall")))
      (padding . 0.20)	   
      (avoid-slur . around)
      (direction  . 1)))
    ("prallmordent" .
     ((script-stencil . (feta . ("prallmordent" . "prallmordent")))
      (padding . 0.20)	   
      (avoid-slur . around)
      (direction  . 1)))
    ("upprall" .
     ((script-stencil . (feta . ("upprall" . "upprall")))
      (padding . 0.20)	   
      (avoid-slur . around)
      (direction  . 1)))
    ("downprall" .
     ((script-stencil . (feta . ("downprall" . "downprall")))
      (padding . 0.20)	   
      (avoid-slur . around)
      (direction  . 1)))
    ("upmordent" .
     ((script-stencil . (feta . ("upmordent" . "upmordent")))
      (padding . 0.20)	   
      (avoid-slur . around)
      (direction  . 1)))
    ("downmordent" .
     ((script-stencil . (feta . ("downmordent" . "downmordent")))
      (padding . 0.20)	   
      (avoid-slur . around)
      (direction  . 1)))
    ("lineprall" .
     ((script-stencil . (feta . ("lineprall" . "lineprall")))
      (padding . 0.20)	   
      (avoid-slur . around)
      (direction  . 1)))
    ("pralldown" .
     ((script-stencil . (feta . ("pralldown" . "pralldown")))
      (padding . 0.20)	   
      (avoid-slur . around)
      (direction  . 1)))
    ("prallup" .
     ((script-stencil . (feta . ("prallup" . "prallup")))
      (padding . 0.20)	   
      (avoid-slur . around)
      (direction  . 1)))
    ("segno" .
     ((script-stencil . (feta . ("segno" . "segno")))
      (padding . 0.20)	   
      (avoid-slur . outside)
      (direction  . 1)))
    ("coda" .
     ((script-stencil . (feta . ("coda" . "coda")))
      (padding . 0.20)	   
      (avoid-slur . outside)
      (direction  . 1)))
    ("varcoda" .
     ((script-stencil . (feta . ("varcoda" . "varcoda")))
      (padding . 0.20)	   
      (avoid-slur . outside)
      (direction  . 1)))))
