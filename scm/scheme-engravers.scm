;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2012 David Nalesnik <david.nalesnik@gmail.com>
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

(define-public (ly:make-listener callback)
  "This is a compatibility wrapper for creating a \"listener\" for use
with @code{ly:add-listener} from a @var{callback} taking a single
argument.  Since listeners are equivalent to callbacks, this is no
longer needed."
  callback)

(define-public (Measure_counter_engraver context)
  "This engraver numbers ranges of measures, which is useful in parts as an
aid for counting repeated measures.  There is no requirement that the
affected measures be repeated, however.  The user delimits the area to
receive a count with @code{\\startMeasureCount} and
@code{\\stopMeasureCount}."
  (let ((count-spanner '()) ; a single element of the count
        (go? #f) ; is the count in progress?
        (stop? #f) ; do we end the count?
        (last-measure-seen 0)
        (elapsed 0))

    (make-engraver
     (listeners
      ((measure-counter-event engraver event)
       (cond
        ((and (= START (ly:event-property event 'span-direction))
              go?)
         (set! stop? #t)
         (ly:input-warning
          (ly:event-property event 'origin)
          "count not ended before another begun"))
        ((= START (ly:event-property event 'span-direction))
         (set! go? #t)
         ;; initialize one less so first measure receives a count spanner
         (set! last-measure-seen
               (1- (ly:context-property context 'currentBarNumber))))
        ((= STOP (ly:event-property event 'span-direction))
         (set! stop? #t)
         (set! go? #f)))))

     ((process-music trans)
      (let ((col (ly:context-property context 'currentCommandColumn))
            (now (ly:context-property context 'measurePosition))
            (current-bar (ly:context-property context 'currentBarNumber)))
        ;; Each measure of a count receives a new spanner, which is bounded
        ;; by the first "command column" of that measure and the following one.
        ;; The possibility of initial grace notes (negative measure position)
        ;; is considered.
        (if (and (> current-bar last-measure-seen)
                 (moment<=? now ZERO-MOMENT))
            (begin
              ;; Finish the previous count-spanner if there is one.
              (if (ly:grob? count-spanner)
                  (begin
                    (ly:spanner-set-bound! count-spanner RIGHT col)
                    (ly:pointer-group-interface::add-grob count-spanner 'columns col)
                    (ly:engraver-announce-end-grob trans count-spanner col)
                    (set! count-spanner '())))
              ;; If count is over, reset variables.
              (if stop?
                  (begin
                    (set! elapsed 0)
                    (set! stop? #f)))
              ;; If count is in progress, begin a count-spanner.
              (if go?
                  (let* ((c (ly:engraver-make-grob trans 'MeasureCounter col))
                         (counter (ly:grob-property c 'count-from)))
                    (ly:spanner-set-bound! c LEFT col)
                    (ly:pointer-group-interface::add-grob c 'columns col)
                    (set! (ly:grob-property c 'count-from) (+ counter elapsed))
                    (set! count-spanner c)
                    (set! elapsed (1+ elapsed))))))
        (set! last-measure-seen current-bar)))

     ((finalize trans)
      (if go?
          (begin
            (set! go? #f)
            (ly:grob-suicide! count-spanner)
            (set! count-spanner '())
            (ly:warning "measure count left unfinished")))))))

(ly:register-translator
 Measure_counter_engraver 'Measure_counter_engraver
 '((grobs-created . (MeasureCounter))
   (events-accepted . (measure-counter-event))
   (properties-read . (currentCommandColumn
                       measurePosition
                       currentBarNumber))
   (properties-written . ())
   (description . "\
This engraver numbers ranges of measures, which is useful in parts as an
aid for counting repeated measures.  There is no requirement that the
affected measures be repeated, however.  The user delimits the area to
receive a count with @code{\\startMeasureCount} and
@code{\\stopMeasureCount}.")))

(ly:register-translator
 Span_stem_engraver 'Span_stem_engraver
 '((grobs-created . (Stem))
   (events-accepted . ())
   (properties-read . ())
   (properties-written . ())
   (description . "Connect cross-staff stems to the stems above in the system")))

(define-public (Merge_rests_engraver context)
"Engraver to merge rests in multiple voices on the same staff.

This works by gathering all rests at a time step. If they are all of the same
length and there are at least two they are moved to the correct location as
if there were one voice."

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

  (define (has-one-or-less? lst) (or (null? lst) (null? (cdr lst))))
  (define (has-at-least-two? lst) (not (has-one-or-less? lst)))
  (define (all-equal? lst pred)
    (or (has-one-or-less? lst)
        (and (pred (car lst) (cadr lst)) (all-equal? (cdr lst) pred))))

  (let ((curr-mmrests '())
        (mmrests '())
        (rests '())
        (dots '()))
    (make-engraver
      ((start-translation-timestep translator)
        (set! rests '())
        (set! curr-mmrests '())
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
              (set! curr-mmrests (cons grob curr-mmrests)))
            (else
              (set! rests (cons grob rests))))))
      ((stop-translation-timestep translator)
        (let (;; get a list of the rests 'duration-lengths, 'duration-log does
              ;; not take dots into account
              (durs
                (map
                  (lambda (g)
                    (ly:duration-length
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
          (if (has-at-least-two? curr-mmrests)
              (set! mmrests (cons curr-mmrests mmrests)))))
      ((finalize translator)
        (for-each merge-mmrests mmrests)))))

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
