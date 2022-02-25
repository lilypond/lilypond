;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2000--2022 Jan Nieuwenhuizen <janneke@gnu.org>
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

(define check-beam-quant
  (lambda (posl posr)
    (lambda (beam posns)
      "Check whether BEAM has POSL and POSR quants.  POSL are (POSITION
. QUANT) pairs, where QUANT is -1 (hang), 0 (center), 1 (sit) or -2/ 2 (inter)

"
      (let* ((thick (ly:grob-property beam 'beam-thickness))
             (layout (ly:grob-layout beam))
             (lthick (ly:output-def-lookup layout 'line-thickness))
             (staff-thick lthick) ; fixme.
             (quant->coord (lambda (p q)
                             (if (= 2 (abs q))
                                 (+ p (/ q 4.0))
                                 (+ p (- (* 0.5 q thick) (* 0.5 q lthick))))))
             (want-l (quant->coord (car posl) (cdr posl)))
             (want-r (quant->coord (car posr) (cdr posr)))
             (almost-equal (lambda (x y) (< (abs (- x y)) 1e-3))))

        (if (or (not (almost-equal want-l (car posns)))
                (not (almost-equal want-r (cdr posns))))
            (begin
              (ly:warning (G_ "Error in beam quanting.  Expected (~S,~S) found ~S.")
                          want-l want-r posns)
              (set! (ly:grob-property beam 'annotation)
                    (format #f "(~S,~S)" want-l want-r))))
        posns))))

(define check-beam-slope-sign
  (lambda (comparison)
    (lambda (beam posns)
      "Check whether the slope of BEAM is correct wrt. COMPARISON."
      (let* ((slope-sign (- (cdr posns) (car posns)))
             (correct (comparison slope-sign 0)))
        (if (not correct)
            (begin
              (ly:warning (G_ "Error in beam quanting.  Expected ~S 0, found ~S.")
                          (procedure-name comparison) slope-sign)
              (set! (ly:grob-property beam 'annotation)
                    (format #f "~S 0" (procedure-name comparison))))
            (set! (ly:grob-property beam 'annotation) ""))
        posns))))


(define-public (check-quant-callbacks l r)
  (lambda (grob)
    ((check-beam-quant l r)
     grob
     (beam::place-broken-parts-individually grob))))


(define-public (check-slope-callbacks comparison)
  (lambda (grob)
    ((check-beam-slope-sign comparison)
     grob
     (beam::place-broken-parts-individually grob))))
