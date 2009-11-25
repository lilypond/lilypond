;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2000--2009 Jan Nieuwenhuizen <janneke@gnu.org>
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

;;  Determine end moment for auto beaming (or begin moment, but mostly
;;  0== anywhere).  We only consider the current time signature.
;;  In order of decreasing priority:
;;
;;  1. end <type>
;;  2. end   *
;;  3. if 1-2 not specified, begin anywhere, end at beatLength intervals
;;
;;  Rationale:
;;
;;  [user override]
;;  1. override for specific duration type
;;  2. override for all duration types in a time signature.
;;
;;  defined in scm/beam-settings.scm:
;;  1. Default grouping for common time signatures
;;  2. exceptions for specific time signature, for specific duration type


(define-public (default-auto-beam-check context dir test-beam)
  (define (get name default)
    (let ((value (ly:context-property context name)))
      (if (not (null? value)) value default)))

  (define (ending-moments group-list start-beat beat-length)
    (if (null? group-list)
        '()
        (let ((new-start (+ start-beat (car group-list))))
          (cons (ly:moment-mul (ly:make-moment new-start 1) beat-length)
                (ending-moments (cdr group-list) new-start beat-length)))))

  ;; Start of actual auto-beam test routine
  ;;
  ;;
  ;; Don't start auto beams on grace notes
  (if (and (!= (ly:moment-grace-numerator (ly:context-now context)) 0)
           (= dir START))
      #f
      (if (= dir START)
          ;; start anywhere is currently implemented
          #t
          (let* ((beat-length (get 'beatLength (ly:make-moment 1 4)))
                 (measure-length (get 'measureLength (ly:make-moment 1 1)))
                 (time-signature-fraction
                   (get 'timeSignatureFraction '(4 . 4)))
                 (measure-pos (get 'measurePosition ZERO-MOMENT))
                 (settings (get 'beamSettings '()))
                 (function (if (= dir START) 'begin 'end))
                 (type (cons (ly:moment-main-numerator test-beam)
                             (ly:moment-main-denominator test-beam)))
                 (pos (if (>= (ly:moment-main-numerator measure-pos) 0)
                        measure-pos
                        (ly:moment-add measure-length measure-pos)))
                 (type-grouping (ly:beam-grouping
                                  settings
                                  time-signature-fraction
                                  function
                                  type))
                 (default-grouping (ly:beam-grouping
                                     settings
                                     time-signature-fraction
                                     function
                                     '*))
                 (beat-grouping (if (null? type-grouping)
                                  default-grouping
                                  type-grouping))
                 (grouping-moment (if (null? type-grouping)
                                    beat-length
                                    test-beam))
                 (grouping-moments (ending-moments
                                      beat-grouping 0 grouping-moment)))
           (if (null? beat-grouping)
               ;; no rule applies, so end at beatLength
               (= (ly:moment-main-denominator
                   (ly:moment-div pos beat-length)) 1)
               ;; otherwise, end at beginning of measure or
               ;; at specified moment
               (or
                ;; start/end at beginning of measure
                (= (ly:moment-main-numerator pos) 0)
                ;; end if measure-pos matches a specified ending moment
                (member pos grouping-moments)))))))
