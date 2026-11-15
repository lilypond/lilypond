;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2017--2026 Thomas Morley <thomasmorley65@gmail.com>
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

(define-public (Merge_rests_engraver context)
  (define (measure-count-eqv? a b)
    (eqv?
     (ly:grob-property a 'measure-count)
     (ly:grob-property b 'measure-count)))

  (define (rests-all-unpitched? rests)
    "Returns true when all rests do not override the staff-position grob
    property. When a rest has a position set we do not want to merge rests at
    that position."
    (every (lambda (rest) (null? (ly:grob-property rest 'staff-position))) rests))

  (define (merge-mmrests mmrests)
    "Move all multimeasure rests to the single voice location."
    (if (all-equal? mmrests measure-count-eqv?)
        (begin
          (for-each
           (lambda (rest) (ly:grob-set-property! rest 'direction CENTER))
           mmrests)
          (for-each
           (lambda (rest) (ly:grob-set-property! rest 'transparent #t))
           (cdr mmrests)))))

  (define (merge-rests rests)
    (for-each
     (lambda (rest) (ly:grob-set-property! rest 'staff-position 0))
     rests)
    (for-each
     (lambda (rest) (ly:grob-set-property! rest 'transparent #t))
     (cdr rests)))

  (let ((mmrests '())
        (rests '())
        (dots '()))
    (make-engraver
     ((start-translation-timestep translator)
      (set! rests '())
      (set! mmrests '())
      (set! dots '()))
     (acknowledgers
      ((dot-column-interface engraver grob source-engraver)
       (if (not (ly:context-property context 'suspendRestMerging #f))
           (set!
            dots
            (append (ly:grob-array->list (ly:grob-object grob 'dots))
                    dots))))
      ((rest-interface engraver grob source-engraver)
       (cond
        ((ly:context-property context 'suspendRestMerging #f)
         #f)
        ((grob::has-interface grob 'multi-measure-rest-interface)
         (set! mmrests (cons grob mmrests)))
        (else
         (set! rests (cons grob rests))))))
     ((stop-translation-timestep translator)
      (let (;; get a list of the rests 'duration-lengths, 'duration-log does
            ;; not take dots into account
            (durs
             (map
              (lambda (g)
                (ly:duration->moment
                 (ly:prob-property
                  (ly:grob-property g 'cause)
                  'duration)))
              rests)))
        (if (and
             (has-at-least-two? rests)
             (all-equal? durs equal?)
             (rests-all-unpitched? rests))
            (begin
              (merge-rests rests)
              ;; ly:grob-suicide! works nicely for dots, as opposed to rests.
              (if (pair? dots) (for-each ly:grob-suicide! (cdr dots)))))
        (if (has-at-least-two? mmrests)
            (merge-mmrests mmrests)))))))

(ly:register-translator
 Merge_rests_engraver 'Merge_rests_engraver
 '((grobs-created . ())
   (events-accepted . ())
   (properties-read . (suspendRestMerging))
   (properties-written . ())
   (description . "\
Engraver to merge rests in multiple voices on the same staff.  This works by
gathering all rests at a time step.  If they are all of the same length and
there are at least two they are moved to the correct location as if there were
one voice.")))
