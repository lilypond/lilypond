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

;;  Determine whether an auto beam should be extended to the right
;;  of the current stem.  We start anywhere, except mid-measure in
;;  3/4 time. We end according to the follwing rules, in order of
;;  decreasing priority:
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

  (define (ending-moments group-list base-length)
    (let ((beat 0))
      (map-in-order (lambda (x)
                      (set! beat (+ beat x))
                      (* base-length beat))
                    group-list)))

  (define (larger-setting type sorted-alist)
    (assoc type sorted-alist <=))

  (define (beat-end? moment beat-endings)
    (pair? (memv moment beat-endings)))  ;; member returns a list if found, not #t

  ;; Start of actual auto-beam test routine
  ;;
  ;;
  ;; Don't start auto beams on grace notes
  (and (or (zero? (ly:moment-grace (ly:context-current-moment context)))
           (!= dir START))
       (let* ((base-length (cond ((get 'baseMoment #f) => ly:moment-main)
                                 (else 1/4)))
              (measure-length (cond ((get 'measureLength #f) => ly:moment-main)
                                    (else 1)))
              (time-signature-fraction
               (get 'timeSignatureFraction '(4 . 4)))
              (beat-structure (get 'beatStructure '(1 1 1 1)))
              (beat-endings (ending-moments beat-structure base-length))
              (exceptions (sort (map
                                 (lambda (a)
                                   (if (pair? (car a))
                                       (cons (/ (caar a) (cdar a))
                                             (cdr a))
                                       a))
                                 (assoc-get 'end
                                            (get 'beamExceptions '())
                                            '()))
                                car<))
              (function (if (= dir START) 'begin 'end))
              (beam-half-measure (get 'beamHalfMeasure #t))
              (type (ly:moment-main test-beam))
              (non-grace (ly:moment-main measure-pos))
              (pos (if (negative? non-grace)
                       (+ measure-length non-grace)
                       non-grace))
              (type-grouping (assoc-get type exceptions '()))
              (default-rule (and (null? type-grouping)
                                 (larger-setting type exceptions)))
              (default-grouping (and default-rule (cdr default-rule)))
              (default-beat-length (and default-rule (car default-rule)))
              (exception-grouping (if (null? type-grouping)
                                      default-grouping
                                      type-grouping))
              (grouping-moment (if (null? type-grouping)
                                   default-beat-length
                                   type))
              (exception-moments (and exception-grouping
                                      (ending-moments
                                       exception-grouping grouping-moment))))

         (if (= dir START)
             ;; Start rules -- #t if beam is allowed to start
             (or beam-half-measure ;; Start anywhere, but option for mid-measure
                 (not (= (+ pos pos) measure-length))
                 (not (= 3 (car time-signature-fraction))) ;; in triple meter
                 (not (= (denominator type) ;; when the beamed note is 1/6 of a measure
                         (* 2 (cdr time-signature-fraction)))))
             ;; End rules -- #t if beam is required to end
             (or (zero? pos) ;; end at measure beginning
                 (if exception-grouping
                     (beat-end? pos exception-moments) ;; check exception rule
                     (beat-end? pos beat-endings))))))) ;; no exception, so check beat ending


(define-public (extract-beam-exceptions music)
  "Create a value useful for setting @code{beamExceptions} from @var{music}."
  (define (car> a b) (> (car a) (car b)))
  (define (beatify! lst)
    ;; takes a collection of end points, sorts them, and returns the
    ;; non-zero differences as beaming pattern
    (let ((s (sort-list! lst <)))
      (remove! zero?
               (map - s (cons 0 s)))))
  (let ((res '()))
    (let analyze ((m (unfold-repeats-fully (event-chord-reduce music)))
                  (pos 0))
      ;; enter beam ends from m starting at pos into res, return new pos
      (cond ((music-is-of-type? m 'bar-check) 0)
            ((music-is-of-type? m 'simultaneous-music)
             (fold (lambda (m prev) (max (analyze m pos) prev))
                   pos
                   (ly:music-property m 'elements)))
            ((not (music-is-of-type? m 'rhythmic-event))
             (let ((elt (ly:music-property m 'element)))
               (fold analyze
                     (if (ly:music? elt) (analyze elt pos) pos)
                     (ly:music-property m 'elements))))
            ;; Have rhythmic event.
            ((any
              (lambda (art)
                (and (music-is-of-type? art 'beam-event)
                     (= (ly:music-property art 'span-direction START) STOP)))
              (ly:music-property m 'articulations))
             (let* ((len (duration-length (ly:music-property m 'duration)))
                    (pos (+ pos len))
                    (ass (assv len res)))
               (cond ((or (zero? len) (not (integer? (/ pos len))))
                      (ly:warning m (G_ "Beam end fits no pattern")))
                     (ass
                      (set-cdr! ass (cons (/ pos len) (cdr ass))))
                     (else
                      (set! res (cons (list len (/ pos len)) res))))
               pos))
            (else
             (+ pos (duration-length (ly:music-property m 'duration))))))

    ;; takes the output from the loop, generates actual beam exceptions
    (list
     (cons 'end
           (map!
            (lambda (l)
              (cons (car l)
                    (beatify! (cdr l))))
            (sort-list! res car>))))))
