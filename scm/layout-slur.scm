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

;; this is put into the slur-details property of Slur and PhrasingSlur
(define-public default-slur-details
  '((region-size . 4)
    (head-encompass-penalty . 1000.0)
    (stem-encompass-penalty . 30.0)
    (edge-attraction-factor . 4)
    (same-slope-penalty . 20)
    (steeper-slope-factor . 50)
    (non-horizontal-penalty . 15)
    (max-slope . 1.1)
    (max-slope-factor . 10)
    (free-head-distance . 0.3)
    (free-slur-distance . 0.8)
    (gap-to-staffline-inside . 0.2)
    (gap-to-staffline-outside . 0.1)
    (extra-object-collision-penalty . 50)
    (accidental-collision . 3)
    (extra-encompass-free-distance . 0.3)
    (extra-encompass-collision-distance . 0.8)
    (head-slur-distance-max-ratio . 3)
    (head-slur-distance-factor . 10)
    (absolute-closeness-measure . 0.3)
    (edge-slope-exponent . 1.7)
    (close-to-edge-length . 2.5)
    (encompass-object-range-overshoot . 0.5)
    (slur-tie-extrema-min-distance . 0.2)
    (slur-tie-extrema-min-distance-penalty . 2)
    ))
