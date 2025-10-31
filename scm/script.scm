;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2000--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
    (accent
     . (
        (avoid-slur . around)
        (padding . 0.20)
        (script-stencil . (feta . ("sforzato" . "sforzato")))
        (side-axis . ,Y)
        (side-relative-direction . ,DOWN)))
    (accentus
     . (
        (script-stencil . (feta . ("uaccentus" . "uaccentus")))
        (side-relative-direction . ,DOWN)
        (avoid-slur . ignore)
        (padding . 0.20)
        (quantize-position . #t)
        (script-priority . -100)
        (side-axis . ,Y)
        (direction . ,UP)))
    (altcomma
     . (
        (script-stencil . (feta . ("laltcomma" . "raltcomma")))
        (quantize-position . #t)
        (padding . 0.20)
        (avoid-slur . ignore)
        (side-axis . ,Y)
        (direction . ,UP)))


    (bachschleifer
     . (
        (script-stencil . (feta . ("bachschleifer" . "bachschleifer")))
        (no-ledgers . #f)
        (padding . 0.8)
        (length-fraction . 0.5)
        (avoid-slur . around)
        (side-axis . ,X)
        (direction . ,LEFT)
        (staff-position . ,(horizontal-script::calc-staff-position -2))))


    (circulus
     . (
        (script-stencil . (feta . ("circulus" . "circulus")))
        (side-relative-direction . ,DOWN)
        (avoid-slur . ignore)
        (padding . 0.20)
        (quantize-position . #t)
        (script-priority . -100)
        (side-axis . ,Y)
        (direction . ,UP)))
    (coda
     . (
        (script-stencil . (feta . ("coda" . "coda")))
        (padding . 0.20)
        (avoid-slur . outside)
        (side-axis . ,Y)
        (direction . ,UP)))
    (comma
     . (
        (script-stencil . (feta . ("lcomma" . "rcomma")))
        (quantize-position . #t)
        (padding . 0.20)
        (avoid-slur . ignore)
        (side-axis . ,Y)
        (direction . ,UP)))


    (downbow
     . (
        (script-stencil . (feta . ("ddownbow" . "udownbow")))
        (padding . 0.20)
        (skyline-horizontal-padding . 0.20)
        (avoid-slur . around)
        (direction . ,UP)
        (side-axis . ,Y)
        (script-priority . 180)))
    (downmordent
     . (
        (script-stencil . (feta . ("downmordent" . "downmordent")))
        (padding . 0.20)
        (avoid-slur . around)
        (side-axis . ,Y)
        (direction . ,UP)))
    (downprall
     . (
        (script-stencil . (feta . ("downprall" . "downprall")))
        (padding . 0.20)
        (avoid-slur . around)
        (side-axis . ,Y)
        (direction . ,UP)))


    (espressivo
     . (
        (avoid-slur . around)
        (padding . 0.20)
        (script-stencil . (feta . ("espr" .  "espr")))
        (side-axis . ,Y)
        (side-relative-direction . ,DOWN)))


    (fermata
     . (
        (script-stencil . (feta . ("dfermata" . "ufermata")))
        (padding . 0.40)
        (avoid-slur . around)
        (outside-staff-priority . 75)
        (script-priority . 175)
        (side-axis . ,Y)
        (direction . ,UP)))
    (flageolet
     . (
        (script-stencil . (feta . ("flageolet" . "flageolet")))
        (padding . 0.20)
        (avoid-slur . around) ;guessing?
        (direction . ,UP)
        (side-axis . ,Y)
        (script-priority . 50)))


    (halfopen
     . (
        (avoid-slur . outside)
        (padding . 0.20)
        (script-stencil . (feta . ("halfopen" . "halfopen")))
        (side-axis . ,Y)
        (direction . ,UP)))
    (halfopenvertical
     . (
        (avoid-slur . outside)
        (padding . 0.20)
        (script-stencil . (feta . ("halfopenvertical" . "halfopenvertical")))
        (side-axis . ,Y)
        (direction . ,UP)))
    (haydnturn
     . (
        (script-stencil . (feta . ("haydnturn" . "haydnturn")))
        (padding . 0.20)
        (avoid-slur . inside)
        (side-axis . ,Y)
        (direction . ,UP)))
    (heel
     . (
        (script-stencil . (feta . ("upedalheel" . "upedalheel")))
        (padding . 0.20)
        (avoid-slur . around) ;guessing?
        (side-axis . ,Y)
        (direction . ,UP)))
    (heelcircle
     . (
        (script-stencil . (feta . ("pedalheelcircle" . "pedalheelcircle")))
        (padding . 0.20)
        (avoid-slur . around) ;guessing?
        (side-axis . ,Y)
        (direction . ,UP)))
    (henzelongfermata
     . (
        (script-stencil . (feta . ("dhenzelongfermata" . "uhenzelongfermata")))
        (padding . 0.40)
        (avoid-slur . around)
        (outside-staff-priority . 75)
        (script-priority . 175)
        (side-axis . ,Y)
        (direction . ,UP)))
    (henzeshortfermata
     . (
        (script-stencil . (feta . ("dhenzeshortfermata" . "uhenzeshortfermata")))
        (padding . 0.40)
        (avoid-slur . around)
        (outside-staff-priority . 75)
        (script-priority . 175)
        (side-axis . ,Y)
        (direction . ,UP)))


    (ictus
     . (
        (script-stencil . (feta . ("ictus" . "ictus")))
        (side-relative-direction . ,DOWN)
        (quantize-position . #t)
        (avoid-slur . ignore)
        (padding . 0.20)
        (script-priority . -100)
        (side-axis . ,Y)
        (direction . ,DOWN)))


    (lheel
     . (
        (script-stencil . (feta . ("upedalheel" . "upedalheel")))
        (padding . 0.20)
        (avoid-slur . around) ;guessing?
        (side-axis . ,Y)
        (direction . ,DOWN)))
    (lineprall
     . (
        (script-stencil . (feta . ("lineprall" . "lineprall")))
        (padding . 0.20)
        (avoid-slur . around)
        (side-axis . ,Y)
        (direction . ,UP)))
    (longfermata
     . (
        (script-stencil . (feta . ("dlongfermata" . "ulongfermata")))
        (padding . 0.40)
        (avoid-slur . around)
        (outside-staff-priority . 75)
        (script-priority . 175)
        (side-axis . ,Y)
        (direction . ,UP)))
    (ltoe
     . (
        (script-stencil . (feta . ("upedaltoe" . "upedaltoe")))
        (padding . 0.20)
        (avoid-slur . around) ;guessing?
        (side-axis . ,Y)
        (direction . ,DOWN)))


    (marcato
     . (
        (script-stencil . (feta . ("dmarcato" . "umarcato")))
        (padding . 0.20)
        (avoid-slur . inside)
        ;;(staff-padding . ())
        (quantize-position . #t)
        (side-axis . ,Y)
        (side-relative-direction . ,DOWN)))
    (mordent
     . (
        (script-stencil . (feta . ("mordent" . "mordent")))
        (padding . 0.20)
        (avoid-slur . around)
        (side-axis . ,Y)
        (direction . ,UP)))


    (open
     . (
        (avoid-slur . outside)
        (padding . 0.20)
        (script-stencil . (feta . ("open" . "open")))
        (side-axis . ,Y)
        (direction . ,UP)))
    (outsidecomma
     . (
        (avoid-slur . around)
        (direction . ,UP)
        (padding . 0.20)
        (side-axis . ,Y)
        (script-stencil . (feta . ("lcomma" . "rcomma")))))


    (portato
     . (
        (script-stencil . (feta . ("uportato" . "dportato")))
        (avoid-slur . around)
        (padding . 0.45)
        (side-axis . ,Y)
        (side-relative-direction . ,DOWN)))
    (prall
     . (
        (script-stencil . (feta . ("prall" . "prall")))
        (padding . 0.20)
        (avoid-slur . around)
        (side-axis . ,Y)
        (direction . ,UP)))
    (pralldown
     . (
        (script-stencil . (feta . ("pralldown" . "pralldown")))
        (padding . 0.20)
        (avoid-slur . around)
        (side-axis . ,Y)
        (direction . ,UP)))
    (prallmordent
     . (
        (script-stencil . (feta . ("prallmordent" . "prallmordent")))
        (padding . 0.20)
        (avoid-slur . around)
        (side-axis . ,Y)
        (direction . ,UP)))
    (prallprall
     . (
        (script-stencil . (feta . ("prallprall" . "prallprall")))
        (padding . 0.20)
        (avoid-slur . around)
        (side-axis . ,Y)
        (direction . ,UP)))
    (prallup
     . (
        (script-stencil . (feta . ("prallup" . "prallup")))
        (padding . 0.20)
        (avoid-slur . around)
        (side-axis . ,Y)
        (direction . ,UP)))


    (reverseturn
     . (
        (script-stencil . (feta . ("reverseturn" . "reverseturn")))
        (padding . 0.20)
        (avoid-slur . inside)
        (side-axis . ,Y)
        (direction . ,UP)))
    (rheel
     . (
        (script-stencil . (feta . ("dpedalheel" . "dpedalheel")))
        (padding . 0.20)
        (avoid-slur . around) ;guessing?
        (side-axis . ,Y)
        (direction . ,UP)))
    (rtoe
     . (
        (script-stencil . (feta . ("dpedaltoe" . "dpedaltoe")))
        (padding . 0.20)
        (avoid-slur . around) ;guessing?
        (side-axis . ,Y)
        (direction . ,UP)))


    (segno
     . (
        (script-stencil . (feta . ("segno" . "segno")))
        (padding . 0.20)
        (avoid-slur . outside)
        (side-axis . ,Y)
        (direction . ,UP)))
    (semicirculus
     . (
        (script-stencil . (feta . ("dsemicirculus" . "dsemicirculus")))
        (side-relative-direction . ,DOWN)
        (quantize-position . #t)
        (avoid-slur . ignore)
        (padding . 0.20)
        (script-priority . -100)
        (side-axis . ,Y)
        (direction . ,UP)))
    (shortfermata
     . (
        (script-stencil . (feta . ("dshortfermata" . "ushortfermata")))
        (padding . 0.40)
        (avoid-slur . around)
        (outside-staff-priority . 75)
        (script-priority . 175)
        (side-axis . ,Y)
        (direction . ,UP)))
    (signumcongruentiae
     . (
        (script-stencil . (feta . ("dsignumcongruentiae" . "usignumcongruentiae")))
        (padding . 0.20)
        (avoid-slur . outside)
        (side-axis . ,Y)
        (direction . ,UP)))
    (slashturn
     . (
        (script-stencil . (feta . ("slashturn" . "slashturn")))
        (padding . 0.20)
        (avoid-slur . inside)
        (side-axis . ,Y)
        (direction . ,UP)))
    (snappizzicato
     . (
        (script-stencil . (feta . ("snappizzicato" . "snappizzicato")))
        (padding . 0.20)
        (avoid-slur . outside)
        (side-axis . ,Y)
        (direction . ,UP)))
    (staccatissimo
     . (
        (avoid-slur . inside)
        (quantize-position . #t)
        (script-stencil . (feta . ("dstaccatissimo" . "ustaccatissimo")))
        (padding . 0.20)
        (skyline-horizontal-padding . 0.10)
        (side-axis . ,Y)
        (side-relative-direction . ,DOWN)
        (toward-stem-shift . 1.0)
        (toward-stem-shift-in-column . 0.0)))
    (staccato
     . (
        (script-stencil . (feta . ("staccato" . "staccato")))
        (side-axis . ,Y)
        (side-relative-direction . ,DOWN)
        (quantize-position . #t)
        (avoid-slur . inside)
        (toward-stem-shift . 1.0)
        (toward-stem-shift-in-column . 0.0)
        (padding . 0.20)
        (skyline-horizontal-padding . 0.10)
        (script-priority . -100)))
    (stopped
     . (
        (script-stencil . (feta . ("stopped" . "stopped")))
        (avoid-slur . inside)
        (padding . 0.20)
        (side-axis . ,Y)
        (direction . ,UP)))


    (tenuto
     . (
        (script-stencil . (feta . ("tenuto" . "tenuto")))
        (quantize-position . #t)
        (avoid-slur . inside)
        (padding . 0.20)
        (script-priority . -50)
        (side-axis . ,Y)
        (side-relative-direction . ,DOWN)))
    (toe
     . (
        (script-stencil . (feta . ("dpedaltoe" . "dpedaltoe")))
        (padding . 0.20)
        (avoid-slur . around) ;guessing?
        (side-axis . ,Y)
        (direction . ,UP)))
    (trill
     . (
        (script-stencil . (feta . ("trill" . "trill")))
        (direction . ,UP)
        (padding . 0.20)
        (avoid-slur . outside)
        (side-axis . ,Y)
        (script-priority . 150)))
    (turn
     . (
        (script-stencil . (feta . ("turn" . "turn")))
        (avoid-slur . inside)
        (padding . 0.20)
        (side-axis . ,Y)
        (direction . ,UP)))


    (upbow
     . (
        (script-stencil . (feta . ("dupbow" . "uupbow")))
        (avoid-slur . around)
        (padding . 0.20)
        (direction . ,UP)
        (side-axis . ,Y)
        (script-priority . 180)))
    (upmordent
     . (
        (script-stencil . (feta . ("upmordent" . "upmordent")))
        (padding . 0.20)
        (avoid-slur . around)
        (side-axis . ,Y)
        (direction . ,UP)))
    (upprall
     . (
        (script-stencil . (feta . ("upprall" . "upprall")))
        (padding . 0.20)
        (avoid-slur . around)
        (side-axis . ,Y)
        (direction . ,UP)))


    (varcoda
     . (
        (script-stencil . (feta . ("varcoda" . "varcoda")))
        (padding . 0.20)
        (avoid-slur . outside)
        (side-axis . ,Y)
        (direction . ,UP)))
    (varcomma
     . (
        (script-stencil . (feta . ("lvarcomma" . "rvarcomma")))
        (quantize-position . #t)
        (padding . 0.20)
        (avoid-slur . ignore)
        (side-axis . ,Y)
        (direction . ,UP)))
    (varheel
     . (
        (script-stencil . (feta . ("dpedalheel" . "dpedalheel")))
        (padding . 0.20)
        (avoid-slur . around) ;guessing?
        (side-axis . ,Y)
        (direction . ,UP)))
    (vartoe
     . (
        (script-stencil . (feta . ("upedaltoe" . "upedaltoe")))
        (padding . 0.20)
        (avoid-slur . around) ;guessing?
        (side-axis . ,Y)
        (direction . ,UP)))
    (verylongfermata
     . (
        (script-stencil . (feta . ("dverylongfermata" . "uverylongfermata")))
        (padding . 0.40)
        (avoid-slur . around)
        (outside-staff-priority . 75)
        (script-priority . 175)
        (side-axis . ,Y)
        (direction . ,UP)))
    (veryshortfermata
     . (
        (script-stencil . (feta . ("dveryshortfermata" . "uveryshortfermata")))
        (padding . 0.40)
        (avoid-slur . around)
        (outside-staff-priority . 75)
        (script-priority . 175)
        (side-axis . ,Y)
        (direction . ,UP)))
    ))


;; The style entries describe the articulation and direction used by `\rtoe`,
;; `\ltoe`, `\rheel`, and `\lheel` (in this order).
(define-public toe-heel-styles
  `((default . ((toe . ,UP)
                (vartoe . ,DOWN)
                (varheel . ,UP)
                (heel . ,DOWN)))
    (standard . ((toe . ,UP)
                 (toe . ,DOWN)
                 (heel . ,UP)
                 (heel . ,DOWN)))
    (reversed . ((vartoe . ,UP)
                 (toe . ,DOWN)
                 (heel . ,UP)
                 (varheel . ,DOWN)))
    (circleheels . ((vartoe . ,UP)
                    (toe . ,DOWN)
                    (heelcircle . ,UP)
                    (heelcircle . ,DOWN)))
    (below . ((toe . ,DOWN)
              (vartoe . ,DOWN)
              (varheel . ,DOWN)
              (heel . ,DOWN)))))

;; Construct an alist with elements of the form `(articulation-name-symbol
;; . glyph-name-string)` for quick access.
(define (make-toe-heel-glyphs style-alist script-alist)
  (let* ((articulation-list (delete-duplicates
                             (flatten-list (map (lambda (style)
                                                  (map (lambda (entry)
                                                         (car entry))
                                                       (cdr style)))
                                                style-alist))))
         (script-entries (map
                          (lambda (articulation)
                            (assoc-get articulation script-alist))
                          articulation-list))
         (glyph-list (map
                      (lambda (entry)
                        ;; We assume that for the glyphs we are interested in,
                        ;; 'up' and 'down' versions are the same.
                        (string-append "scripts."
                                       (caddr (assoc 'script-stencil entry))))
                      script-entries)))
    (fold acons '() articulation-list glyph-list)))

(define-public toe-heel-glyphs (make-toe-heel-glyphs
                                toe-heel-styles default-script-alist))
