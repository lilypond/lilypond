;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2000--2026 Jan Nieuwenhuizen <janneke@gnu.org>
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

#(use-modules (srfi srfi-11)) ; let-values

;;  Determine whether an auto beam should be extended to the right
;;  of the current stem.  We start anywhere, except mid-measure in
;;  3/4 time. We end according to the following rules, in order of
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

  ;; Start of actual auto-beam test routine
  ;;
  ;;
  ;; Don't start auto beams on grace notes
  (and (or (!= dir START)
           (zero? (ly:moment-grace (ly:context-current-moment context))))
       (let ((type (ly:moment-main test-beam))
             (non-grace (ly:moment-main measure-pos)))

         ;; Start rules -- #t if beam is allowed to start.  Start anywhere,
         ;; optionally excepting the half-measure point in 3/N time signatures
         ;; to avoid the appearance of 6/N beat structure.
         (define (start?)
           (or (get 'beamHalfMeasure #t)
               (let ((tsig (get 'timeSignature '(4 . 4))))
                 (or ;; the time signature is 3/N
                  (not (number-pair? tsig))
                  (not (= 3 (car tsig)))
                  ;; the beamed note is 1/6 of a measure in the time signature
                  ;; (regardless of the current value of measureLength)
                  (not (= (denominator type) (* 2 (cdr tsig))))
                  ;; the measure position is the halfway point
                  ;; (again, regarding the time signature only)
                  (let* ((mlen (calc-measure-length tsig))
                         (pos (euclidean-remainder non-grace mlen)))
                    (or (not (= (+ pos pos) mlen))))))))

         ;; End rules -- #t if beam is required to end
         (define (end?)
           ;; N.B. Defaulted and derived values should be kept in sync with the
           ;; Beaming_options C++ class.
           (let* ((exceptions
                   (stable-sort
                    (map
                     (lambda (a)
                       (if (pair? (car a))
                           (cons (/ (caar a) (cdar a))
                                 (cdr a))
                           a))
                     (assoc-get 'end (get 'beamExceptions '()) '()))
                    car<))
                  (rule (or (assoc type exceptions <=)
                            (cons (get 'beatBase 1/4)
                                  (get 'beatStructure '()))))
                  (base (car rule))
                  (grouping (cdr rule)))
             (when (null? grouping)
               (set! grouping (list +inf.0)))
             ;; Be resilient to having a grouping that is shorter than the
             ;; measure length: repeat the structure as needed.
             (zero? (last (structured-remainder
                           non-grace
                           (map (lambda (x) (* base x)) grouping))))))

         (if (= dir START)
             (start?)
             (end?)))))


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
      (cond ((music-is-of-type? m 'bar-check-event) 0)
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
             (let* ((len (ly:duration->number (ly:music-property m 'duration)))
                    (pos (+ pos len))
                    (ass (assv len res)))
               (cond ((or (zero? len) (not (integer? (/ pos len))))
                      (ly:music-warning m (G_ "Beam end fits no pattern")))
                     (ass
                      (set-cdr! ass (cons (/ pos len) (cdr ass))))
                     (else
                      (set! res (cons (list len (/ pos len)) res))))
               pos))
            (else
             (+ pos (ly:duration->number (ly:music-property m 'duration))))))

    ;; takes the output from the loop, generates actual beam exceptions
    (list
     (cons 'end
           (map!
            (lambda (l)
              (cons (car l)
                    (beatify! (cdr l))))
            (sort-list! res car>))))))
