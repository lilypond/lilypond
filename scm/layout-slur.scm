;;;; layout-slur.scm -- Slur scheme stuff
;;;;
;;;; source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2000--2009 Jan Nieuwenhuizen <janneke@gnu.org>
				;
; this is put into the slur-details property of Slur and PhrasingSlur
(define default-slur-details
  '((region-size . 4)
    (head-encompass-penalty . 1000.0)
    (stem-encompass-penalty . 30.0)
    (closeness-factor . 10)
    (edge-attraction-factor . 4)
    (same-slope-penalty . 20)
    (steeper-slope-factor . 50)
    (non-horizontal-penalty . 15)
    (max-slope . 1.1)
    (max-slope-factor . 10)
    (free-head-distance . 0.3)
    (free-slur-distance . 0.8)
    (extra-object-collision-penalty . 50)
    (accidental-collision . 3)
    (extra-encompass-free-distance . 0.3)
    (extra-encompass-collision-distance . 0.8)
    (head-slur-distance-max-ratio . 3)
    (head-slur-distance-factor . 10)
    (absolute-closeness-measure . 0.3)
    (edge-slope-exponent . 1.7)
    ))
