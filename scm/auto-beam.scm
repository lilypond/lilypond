;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2000--2012 Jan Nieuwenhuizen <janneke@gnu.org>
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

;;  Determine whether an auto beam should be extended to the right
;;  of the current stem.   In general, we start anywhere except on
;;  the last note of a beat. We end according to the follwing rules,
;;  in order of decreasing priority:
;;
;;  1. end <type>
;;  2. end <greater type>
;;  3. if 1-2 not specified,  end at beatStructure intervals
;;
;;  Rationale:
;;
;;  [user override]
;;  1. override for specific duration type
;;  2. overrides apply to shorter durations
;;
;;  defined in scm/time-signature-settings.scm:
;;  1. Default grouping for common time signatures

(define-public (default-auto-beam-check context dir measure-pos test-beam)
  (define (get name default)
    (let ((value (ly:context-property context name)))
      (if (not (null? value)) value default)))

  (define (beaming<? a b)
    (ly:moment<? (fraction->moment (car a))
                 (fraction->moment (car b))))

  (define (ending-moments group-list start-beat base-moment)
    (if (null? group-list)
        '()
        (let ((new-start (+ start-beat (car group-list))))
          (cons (ly:moment-mul (ly:make-moment new-start 1) base-moment)
                (ending-moments (cdr group-list) new-start base-moment)))))

  (define (larger-setting test-beam sorted-alist)
   (if (null? sorted-alist)
       '()
       (let* ((first-key (caar sorted-alist))
              (first-moment (fraction->moment first-key)))
         (if (moment<=? test-beam first-moment)
             (car sorted-alist)
             (larger-setting test-beam (cdr sorted-alist))))))

  (define (beat-end? moment beat-structure)
    (pair? (member moment beat-structure)))  ;; member returns a list if found, not #t

  (define (use-special-3-4-rules? fraction base-moment exceptions)
    "Should we use special 3/4 time signature beaming rules?"
     (and (equal? fraction '(3 . 4))
          (equal? base-moment (ly:make-moment 1 4))
          (null? (assoc-get '(1 . 8) exceptions '()))))

  ;; Start of actual auto-beam test routine
  ;;
  ;;
  ;; Don't start auto beams on grace notes
  (if (and (!= (ly:moment-grace-numerator (ly:context-now context)) 0)
           (= dir START))
      #f
      (let* ((base-moment (get 'baseMoment (ly:make-moment 1 4)))
             (measure-length (get 'measureLength (ly:make-moment 1 1)))
             (time-signature-fraction
               (get 'timeSignatureFraction '(4 . 4)))
             (beat-structure (get 'beatStructure '(1 1 1 1)))
             (beat-endings (ending-moments beat-structure 0 base-moment))
             (exceptions (sort (assoc-get 'end
                                          (get 'beamExceptions '())
                                          '())
                               beaming<?))
             (function (if (= dir START) 'begin 'end))
             (beam-whole-measure (get 'beamWholeMeasure #t))
             (beam-half-measure (get 'beamHalfMeasure #f))
             (type (moment->fraction test-beam))
             (non-grace (ly:make-moment
                          (ly:moment-main-numerator measure-pos)
                          (ly:moment-main-denominator measure-pos)))
             (pos (if (ly:moment<? non-grace ZERO-MOMENT)
                      (ly:moment-add measure-length non-grace)
                      non-grace))
             (type-grouping (assoc-get type exceptions '()))
             (default-rule (if (null? type-grouping)
                               (larger-setting test-beam exceptions)
                               '()))
             (default-grouping (if (pair? default-rule)
                                   (cdr default-rule)
                                   '()))
             (default-beat-length (if (pair? default-rule)
                                      (car default-rule)
                                      '()))
             (exception-grouping (if (null? type-grouping)
                                default-grouping
                                type-grouping))
             (grouping-moment (if (null? type-grouping)
                                  (fraction->moment default-beat-length)
                                  test-beam))
             (exception-moments (ending-moments
                                  exception-grouping 0 grouping-moment)))

	(if (= dir START)
            ;; Start rules -- start anywhere unless 3/4 with default rules
            ;; #t if beam is to start
            (or (not (use-special-3-4-rules?
                       time-signature-fraction
                       base-moment
                       exceptions)) ;; start anywhere if not default 3/4
                (= (ly:moment-main-numerator pos) 0) ;; start at beginning of measure
		(and beam-half-measure
                     (equal? type '(1 . 8))
                     (equal? pos (ly:make-moment 3 8))) ;; start at mid-measure if 1/8 note beam
                (beat-end? pos beat-endings)  ;; start if at start of beat
		(and (not (equal? test-beam base-moment)) ;; is beat split?
                     (not (beat-end? (ly:moment-add pos test-beam)
                                     beat-endings))))  ;; will this note end the beat
            ;; End rules -- #t if beam is to end
            (or (= (ly:moment-main-numerator pos) 0) ;; end at measure beginning
                (if (use-special-3-4-rules?
                      time-signature-fraction
                      base-moment
                      exceptions)
                    ;; special rule for default 3/4 beaming
                    (if (and (equal? type '(1 . 8))
                             (or beam-whole-measure
                                 (and beam-half-measure
                                      (not (equal? pos (ly:make-moment 3 8))))))
                        #f
                        (beat-end? pos beat-endings))
                    ;; rules for all other cases -- check for applicable exception
                    (if (null? exception-grouping)
                        (beat-end? pos beat-endings) ;; no exception, so check beat ending
                        (member pos exception-moments)))))))) ;; check exception rule

