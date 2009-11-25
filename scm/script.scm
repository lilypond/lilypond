;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2000--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;
;;;; LilyPond is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; LilyPond is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

(define-public default-script-alist
  `(
    ("accent"
     . (
	(avoid-slur . around)
	(padding . 0.20)
	(quantize-position . #t)
	(script-stencil . (feta . ("sforzato" . "sforzato")))
	(side-relative-direction . ,DOWN)))
    ("accentus"
     . (
	(script-stencil . (feta . ("uaccentus" . "uaccentus")))
	(side-relative-direction . ,DOWN)
	(avoid-slur . ignore)
	(padding . 0.20)
	(quantize-position . #t)
	(script-priority . -100)
	(direction . ,UP)))


    ("circulus"
     . (
	(script-stencil . (feta . ("circulus" . "circulus")))
	(side-relative-direction . ,DOWN)
	(avoid-slur . ignore)
	(padding . 0.20)
	(quantize-position . #t)
	(script-priority . -100)
	(direction . ,UP)))
    ("coda"
     . (
	(script-stencil . (feta . ("coda" . "coda")))
	(padding . 0.20)
	(avoid-slur . outside)
	(direction . ,UP)))
    ("comma"
     . (
	(script-stencil . (feta . ("lcomma" . "rcomma")))
	(quantize-position . #t)
	(padding . 0.20)
	(avoid-slur . ignore)
	(direction . ,UP)))


    ("downbow"
     . (
	(script-stencil . (feta . ("downbow" . "downbow")))
	(padding . 0.20)
	(avoid-slur . around)
	(direction . ,UP)))
    ("downmordent"
     . (
	(script-stencil . (feta . ("downmordent" . "downmordent")))
	(padding . 0.20)
	(avoid-slur . around)
	(direction . ,UP)))
    ("downprall"
     . (
	(script-stencil . (feta . ("downprall" . "downprall")))
	(padding . 0.20)
	(avoid-slur . around)
	(direction . ,UP)))


    ("espressivo"
     . (
	(avoid-slur . around)
	(padding . 0.20)
	(quantize-position . #t)
	(script-stencil . (feta . ("espr" .  "espr")))
	(side-relative-direction . ,DOWN)))


    ("fermata"
     . (
	(script-stencil . (feta . ("dfermata" . "ufermata")))
	(padding . 0.20)
	(avoid-slur . around)
	(script-priority . 4000)
	(direction . ,UP)))
    ("flageolet"
     . (
	(script-stencil . (feta . ("flageolet" . "flageolet")))
	(padding . 0.20)
	(avoid-slur . around) ;guessing?
	(direction . ,UP)))


    ("ictus"
     . (
	(script-stencil . (feta . ("ictus" . "ictus")))
	(side-relative-direction . ,DOWN)
	(quantize-position . #t)
	(avoid-slur . ignore)
	(padding . 0.20)
	(script-priority . -100)
	(direction . ,DOWN)))


    ("lheel"
     . (
	(script-stencil . (feta . ("upedalheel" . "upedalheel")))
	(padding . 0.20)
	(avoid-slur . around) ;guessing?
	(direction . ,DOWN)))
    ("lineprall"
     . (
	(script-stencil . (feta . ("lineprall" . "lineprall")))
	(padding . 0.20)
	(avoid-slur . around)
	(direction . ,UP)))
    ("longfermata"
     . (
	(script-stencil . (feta . ("dlongfermata" . "ulongfermata")))
	(padding . 0.20)
	(avoid-slur . around)
	(direction . ,UP)))
    ("ltoe"
     . (
	(script-stencil . (feta . ("upedaltoe" . "upedaltoe")))
	(padding . 0.20)
	(avoid-slur . around) ;guessing?
	(direction . ,DOWN)))


    ("marcato"
     . (
	(script-stencil . (feta . ("dmarcato" . "umarcato")))
	(padding . 0.20)
	(avoid-slur . inside)
        ;;(staff-padding . ())
	(quantize-position . #t)
	(side-relative-direction . ,DOWN)))
    ("mordent"
     . (
	(script-stencil . (feta . ("mordent" . "mordent")))
	(padding . 0.20)
	(avoid-slur . around)
	(direction . ,UP)))


    ("open"
     . (
	(avoid-slur . outside)
	(padding . 0.20)
	(script-stencil . (feta . ("open" . "open")))
	(direction . ,UP)))


    ("portato"
     . (
	(script-stencil . (feta . ("uportato" . "dportato")))
	(avoid-slur . around)
	(slur-padding . 0.3)
	(padding . 0.45)
	(side-relative-direction . ,DOWN)))
    ("prall"
     . (
	(script-stencil . (feta . ("prall" . "prall")))
	(padding . 0.20)
	(avoid-slur . around)
	(direction . ,UP)))
    ("pralldown"
     . (
	(script-stencil . (feta . ("pralldown" . "pralldown")))
	(padding . 0.20)
	(avoid-slur . around)
	(direction . ,UP)))
    ("prallmordent"
     . (
	(script-stencil . (feta . ("prallmordent" . "prallmordent")))
	(padding . 0.20)
	(avoid-slur . around)
	(direction . ,UP)))
    ("prallprall"
     . (
	(script-stencil . (feta . ("prallprall" . "prallprall")))
	(padding . 0.20)
	(avoid-slur . around)
	(direction . ,UP)))
    ("prallup"
     . (
	(script-stencil . (feta . ("prallup" . "prallup")))
	(padding . 0.20)
	(avoid-slur . around)
	(direction . ,UP)))


    ("reverseturn"
     . (
	(script-stencil . (feta . ("reverseturn" . "reverseturn")))
	(padding . 0.20)
	(avoid-slur . inside)
	(direction . ,UP)))
    ("rheel"
     . (
	(script-stencil . (feta . ("dpedalheel" . "dpedalheel")))
	(padding . 0.20)
	(avoid-slur . around) ;guessing?
	(direction . ,UP)))
    ("rtoe"
     . (
	(script-stencil . (feta . ("dpedaltoe" . "dpedaltoe")))
	(padding . 0.20)
	(avoid-slur . around) ;guessing?
	(direction . ,UP)))


    ("segno"
     . (
	(script-stencil . (feta . ("segno" . "segno")))
	(padding . 0.20)
	(avoid-slur . outside)
	(direction . ,UP)))
    ("semicirculus"
     . (
	(script-stencil . (feta . ("dsemicirculus" . "dsemicirculus")))
	(side-relative-direction . ,DOWN)
	(quantize-position . #t)
	(avoid-slur . ignore)
	(padding . 0.20)
	(script-priority . -100)
	(direction . ,UP)))
    ("shortfermata"
     . (
	(script-stencil . (feta . ("dshortfermata" . "ushortfermata")))
	(padding . 0.20)
	(avoid-slur . around)
	(direction . ,UP)))
    ("signumcongruentiae"
     . (
	(script-stencil . (feta . ("dsignumcongruentiae" . "usignumcongruentiae")))
	(padding . 0.20)
	(avoid-slur . outside)
	(direction . ,UP)))
    ("snappizzicato"
     . (
	(script-stencil . (feta . ("snappizzicato" . "snappizzicato")))
	(padding . 0.20)
	(avoid-slur . outside)
	(direction . ,UP)))
    ("staccatissimo"
     . (
	(avoid-slur . inside)
	(script-stencil . (feta . ("dstaccatissimo" . "ustaccatissimo")))
	(padding . 0.20)
	(side-relative-direction . ,DOWN)))
    ("staccato"
     . (
	(script-stencil . (feta . ("staccato" . "staccato")))
	(side-relative-direction . ,DOWN)
	(quantize-position . #t)
	(avoid-slur . inside)
	(toward-stem-shift . 0.5)
	(padding . 0.20)
	(script-priority . -100)))
    ("stopped"
     . (
	(script-stencil . (feta . ("stopped" . "stopped")))
	(avoid-slur . inside)
	(padding . 0.20)
	(direction . ,UP)))


    ("tenuto"
     . (
	(script-stencil . (feta . ("tenuto" . "tenuto")))
	(quantize-position . #t)
	(avoid-slur . inside)
	(padding . 0.20)
	(side-relative-direction . ,DOWN)))
    ("thumb"
     . (
	(script-stencil . (feta . ("thumb"  . "thumb")))
	(avoid-slur . inside)
	(padding . 0.20)
	(direction . ,UP)))
    ("trill"
     . (
	(script-stencil . (feta . ("trill" . "trill")))
	(direction . ,UP)
	(padding . 0.20)
	(avoid-slur . outside)
	(script-priority . 2000)))
    ("turn"
     . (
	(script-stencil . (feta . ("turn" . "turn")))
	(avoid-slur . inside)
	(padding . 0.20)
	(direction . ,UP)))


    ("upbow"
     . (
	(script-stencil . (feta . ("upbow" . "upbow")))
	(avoid-slur . around)
	(padding . 0.20)
	(direction . ,UP)))
    ("upmordent"
     . (
	(script-stencil . (feta . ("upmordent" . "upmordent")))
	(padding . 0.20)
	(avoid-slur . around)
	(direction . ,UP)))
    ("upprall"
     . (
	(script-stencil . (feta . ("upprall" . "upprall")))
	(padding . 0.20)
	(avoid-slur . around)
	(direction . ,UP)))


    ("varcoda"
     . (
	(script-stencil . (feta . ("varcoda" . "varcoda")))
	(padding . 0.20)
	(avoid-slur . outside)
	(direction . ,UP)))
    ("varcomma"
     . (
	(script-stencil . (feta . ("lvarcomma" . "rvarcomma")))
	(quantize-position . #t)
	(padding . 0.20)
	(avoid-slur . ignore)
	(direction . ,UP)))
    ("verylongfermata"
     . (
	(script-stencil . (feta . ("dverylongfermata" . "uverylongfermata")))
	(padding . 0.20)
	(avoid-slur . around)
	(direction . ,UP)))
      ))
