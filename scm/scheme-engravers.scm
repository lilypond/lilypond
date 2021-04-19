;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2012--2020 David Nalesnik <david.nalesnik@gmail.com>
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
        (start-event #f)
        (go? #f) ; is the count in progress?
        (stop-event #f)
        (last-measure-seen 0)
        (elapsed 0))

    (make-engraver
     (listeners
      ((measure-counter-event engraver event)
       (cond
        ((= START (ly:event-property event 'span-direction))
         (set! start-event event)
         ;; initialize one less so first measure receives a count spanner
         (set! last-measure-seen
               (1- (ly:context-property context 'currentBarNumber))))
        ((= STOP (ly:event-property event 'span-direction))
         (set! stop-event event)))))

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
              (if stop-event
                  (begin
                    (set! go? #f)
                    (set! elapsed 0)))
              (if start-event
                  (if go?
                      (ly:event-warning start-event
                                        "count not ended before another begun")
                      (set! go? #t)))
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

     ((stop-translation-timestep trans)
      (set! start-event #f)
      (set! stop-event #f))

     ((finalize trans)
      (if go?
          (begin
            (set! go? #f)
            (ly:grob-suicide! count-spanner)
            (set! count-spanner '())
            (ly:warning (_ "measure count left unfinished"))))))))

(define-public (Measure_spanner_engraver context)
  (let ((span '())
        (finished '())
        (event-start '())
        (event-stop '()))
    (make-engraver
     (listeners ((measure-spanner-event engraver event)
                 (if (= START (ly:event-property event 'span-direction))
                     (set! event-start event)
                     (set! event-stop event))))
     ((process-music trans)
      (if (ly:stream-event? event-stop)
          (if (null? span)
              (ly:warning (_ "cannot find start of measure spanner"))
              (begin
                (set! finished span)
                (ly:engraver-announce-end-grob trans finished event-start)
                (set! span '())
                (set! event-stop '()))))
      (if (ly:stream-event? event-start)
          (begin
            (set! span (ly:engraver-make-grob trans 'MeasureSpanner event-start))
            (set! event-start '()))))
     ((stop-translation-timestep trans)
      (if (and (ly:spanner? span)
               (null? (ly:spanner-bound span LEFT))
               (moment<=? (ly:context-property context 'measurePosition) ZERO-MOMENT))
          (ly:spanner-set-bound! span LEFT
                                 (ly:context-property context 'currentCommandColumn)))
      (if (and (ly:spanner? finished)
               (moment<=? (ly:context-property context 'measurePosition) ZERO-MOMENT))
          (begin
            (if (null? (ly:spanner-bound finished RIGHT))
                (ly:spanner-set-bound! finished RIGHT
                                       (ly:context-property context 'currentCommandColumn)))
            (set! finished '())
            (set! event-start '())
            (set! event-stop '()))))
     ((finalize trans)
      (if (ly:spanner? finished)
          (begin
            (if (null? (ly:spanner-bound finished RIGHT))
                (set! (ly:spanner-bound finished RIGHT)
                      (ly:context-property context 'currentCommandColumn)))
            (set! finished '())))
      (if (ly:spanner? span)
          (begin
            (ly:warning (_ "unterminated measure spanner"))
            (ly:grob-suicide! span)
            (set! span '())))))))

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
 Measure_spanner_engraver 'Measure_spanner_engraver
 '((grobs-created . (MeasureSpanner))
   (events-accepted . (measure-spanner-event))
   (properties-read . (measurePosition
                       currentCommandColumn))
   (properties-written . ())
   (description . "\
This engraver creates spanners bounded by the columns that start and
end measures in response to @code{\\startMeasureSpanner} and
@code{\\stopMeasureSpanner}.")))

(ly:register-translator
 Span_stem_engraver 'Span_stem_engraver
 '((grobs-created . (Stem))
   (events-accepted . ())
   (properties-read . ())
   (properties-written . ())
   (description . "Connect cross-staff stems to the stems above in the system")))

(define (has-one-or-less? lst) (or (null? lst) (null? (cdr lst))))
(define (has-at-least-two? lst) (not (has-one-or-less? lst)))
(define (all-equal? lst pred)
  (or (has-one-or-less? lst)
      (and (pred (car lst) (cadr lst)) (all-equal? (cdr lst) pred))))

(define-public (Merge_mmrest_numbers_engraver context)
  "Engraver to merge multi-measure rest numbers in multiple voices.

This works by gathering all multi-measure rest numbers at a time step. If they
all have the same text and there are at least two only the first one is retained
and the others are hidden."

  (define (text-equal? a b)
    (equal?
     (ly:grob-property a 'text)
     (ly:grob-property b 'text)))

  (let ((mmrest-numbers '()))
    (make-engraver
     ((start-translation-timestep translator)
      (set! mmrest-numbers '()))
     (acknowledgers
      ((multi-measure-rest-number-interface engraver grob source-engraver)
       (set! mmrest-numbers (cons grob mmrest-numbers))))
     ((stop-translation-timestep translator)
      (if (and (has-at-least-two? mmrest-numbers)
               (all-equal? mmrest-numbers text-equal?))
          (for-each ly:grob-suicide! (cdr (reverse mmrest-numbers))))))))

(ly:register-translator
 Merge_mmrest_numbers_engraver 'Merge_mmrest_numbers_engraver
 '((grobs-created . ())
   (events-accepted . ())
   (properties-read . ())
   (properties-written . ())
   (description . "\
Engraver to merge multi-measure rest numbers in multiple voices.

This works by gathering all multi-measure rest numbers at a time step. If they
all have the same text and there are at least two only the first one is retained
and the others are hidden.")))

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

(define-public (event-has-articulation? event-type stream-event)
  "Is @var{event-type} in the @code{articulations} list of @var{stream-event}?"
  (if (ly:stream-event? stream-event)
      (any
       (lambda (evt-type) (eq? evt-type event-type))
       (append-map
        (lambda (art) (ly:prob-property art 'types))
        (ly:prob-property
         (ly:prob-property stream-event 'music-cause)
         'articulations)))
      #f))

(define-public (Duration_line_engraver context)
  (let ((dur-event #f)
        (start-duration-line #f)
        (stop-duration-line #f)
        (current-dur-grobs #f)
        (rhyth-event #f)
        (mmr-event #f)
        (skip #f)
        (tie #f))
    (make-engraver
     (listeners
      ((duration-line-event this-engraver event)
       (set! dur-event event)
       (set! start-duration-line #t))
      ((multi-measure-rest-event this-engraver event)
       (set! mmr-event event))
      ((rhythmic-event this-engraver event)
       (set! rhyth-event (cons (ly:context-current-moment context) event)))
      ((skip-event this-engraver event)
       (set! skip event))
      ((tie-event this-engraver event)
       (set! tie event)))

     (acknowledgers
      ((multi-measure-rest-interface this-engraver grob source-engraver)
       (if stop-duration-line
           (begin
             (for-each
              (lambda (dur-line)
                ;; TODO rethink:
                ;; For MultiMeasureRest always use to-barline #t
                (ly:grob-set-property! dur-line 'to-barline #t)
                (ly:spanner-set-bound! dur-line RIGHT
                                       (ly:context-property context 'currentMusicalColumn))
                (ly:engraver-announce-end-grob this-engraver dur-line grob))
              current-dur-grobs)
             (set! stop-duration-line #f)
             (set! current-dur-grobs #f)))


       (if (and start-duration-line
                (event-has-articulation? 'duration-line-event mmr-event))
           (begin
             (set! start-duration-line #f)
             (set! stop-duration-line #t)
             (set! current-dur-grobs
                   (let ((dur-line
                          (ly:engraver-make-grob
                           this-engraver
                           'DurationLine
                           dur-event)))
                     (ly:spanner-set-bound! dur-line LEFT
                                            (ly:context-property context 'currentMusicalColumn))
                     (list dur-line)))
             (set! mmr-event #f)
             (set! dur-event #f))))
      ((note-column-interface this-engraver grob source-engraver)
       (let* ((note-heads-array (ly:grob-object grob 'note-heads))
              (nc-rest (ly:grob-object grob 'rest))
              (note-heads
               (if (ly:grob-array? note-heads-array)
                   (ly:grob-array->list note-heads-array)
                   '())))
         (cond  ;; Don't stop at tied NoteHeads
          ;; TODO make this a context-property?
          ((and (ly:stream-event? tie) stop-duration-line)
           (set! tie #f))
          (stop-duration-line
           (begin
             (for-each
              (lambda (dur-line)
                (ly:spanner-set-bound! dur-line RIGHT grob)
                (ly:engraver-announce-end-grob
                 this-engraver dur-line grob))
              current-dur-grobs)
             (set! stop-duration-line #f)
             (set! current-dur-grobs #f))))

         (if start-duration-line
             (begin
               (set! start-duration-line #f)
               (set! stop-duration-line #t)
               (set! current-dur-grobs
                     (cond
                      ;; get one DurationLine for entire NoteColumn
                      ((ly:context-property context 'startAtNoteColumn #f)
                       (let ((dur-line
                              (ly:engraver-make-grob
                               this-engraver
                               'DurationLine
                               dur-event)))
                         (ly:spanner-set-bound! dur-line LEFT grob)
                         (list dur-line)))
                      ;; get DurationLines for every NoteHead
                      ((pair? note-heads)
                       (map
                        (lambda (nhd)
                          (let ((dur-line
                                 (ly:engraver-make-grob
                                  this-engraver
                                  'DurationLine
                                  dur-event)))
                            (ly:spanner-set-bound! dur-line LEFT nhd)
                            dur-line))
                        note-heads))
                      ;; get DurationLine for Rest
                      (else
                       (let ((dur-line
                              (ly:engraver-make-grob
                               this-engraver
                               'DurationLine
                               dur-event)))
                         (ly:spanner-set-bound! dur-line LEFT nc-rest)
                         (list dur-line)))))
               (set! dur-event #f))))))

     ((process-music this-engraver)
      ;; Needed?
                                        ;(if (ly:stream-event? dur-event)
                                        ;    (set! start-duration-line #t))
      ;; If 'endAtSkip is set #t, DurationLine may end at skips.
      ;; In this case set right bound to PaperColumn
      (if (and (pair? current-dur-grobs)
               (ly:stream-event? skip)
               (ly:context-property context 'endAtSkip #f)
               stop-duration-line)
          (begin
            (for-each
             (lambda (dur-line)
               (if (null? (ly:spanner-bound dur-line RIGHT))
                   (let ((cmc (ly:context-property
                               context
                               'currentMusicalColumn)))
                     (ly:spanner-set-bound! dur-line RIGHT cmc)
                     (ly:engraver-announce-end-grob
                      this-engraver dur-line cmc))))
             current-dur-grobs)
            (set! stop-duration-line #f)
            (set! skip #f)
            (set! current-dur-grobs #f)))
      ;; If 'startAtSkip is set #t, DurationLine may start at skips.
      ;; In this case set left bound to PaperColumn .
      ;; We need to care about 'duration-line-event, otherwise we loose the
      ;; ability to ignore skips.
      ;; Thus, only do so if skip has a 'duration-line-event.
      (if (and start-duration-line
               (event-has-articulation? 'duration-line-event skip)
               (ly:context-property context 'startAtSkip #t))
          (begin
            (set! start-duration-line #f)
            (set! stop-duration-line #t)
            (set! current-dur-grobs
                  (let ((dur-line
                         (ly:engraver-make-grob
                          this-engraver
                          'DurationLine
                          dur-event)))
                    (ly:grob-set-property! dur-line 'to-barline #f)
                    (ly:spanner-set-bound! dur-line LEFT
                                           (ly:context-property context 'currentMusicalColumn))
                    (list dur-line)))
            (set! skip #f)
            (set! dur-event #f))))
     ((stop-translation-timestep this-engraver)
      ;; If a context dies or "pauses" (i.e. no rhythmic-event for some time,
      ;; because other contexts are active), set right bound to
      ;; NonMusicalPaperColumn.
      ;; We calculate the end-moment of the rhythmic-event and compare with
      ;; current-moment to get the condition for ending the DurationLine.
      ;; We can't go for (ly:context-property context 'busyGrobs), because
      ;; we then wouldn't know if a skip-event needs to be respected.
      ;;
      ;; FIXME
      ;; As a result a DurationLine running to the very end of a score is the
      ;; first or a middle part of a broken spanner.
      ;; Other parts are disregarded

      (if rhyth-event
          (let* ((rhyhtmic-evt-start (car rhyth-event))
                 (rhyhtmic-evt-length
                  (ly:prob-property (cdr rhyth-event) 'length))
                 (rhyhtmic-evt-end
                  (ly:moment-add rhyhtmic-evt-start rhyhtmic-evt-length))
                 (current-moment (ly:context-current-moment context)))
            (if (and (equal? current-moment rhyhtmic-evt-end)
                     (pair? current-dur-grobs)
                     stop-duration-line)
                (begin
                  (for-each
                   (lambda (dur-line)
                     (if (null? (ly:spanner-bound dur-line RIGHT))
                         (let ((cmc (ly:context-property
                                     context
                                     'currentCommandColumn)))
                           (ly:spanner-set-bound! dur-line RIGHT cmc)
                           (ly:engraver-announce-end-grob
                            this-engraver dur-line cmc))))
                   current-dur-grobs)
                  (set! stop-duration-line #f)
                  (set! current-dur-grobs #f)
                  (set! rhyth-event #f))))))

     ((finalize this-engraver)
      ;; likely unneeded, better be paranoid
      (if (pair? current-dur-grobs)
          (begin
            (for-each
             (lambda (dur-line)
               (ly:warning (_ "unterminated DurationLine"))
               (ly:grob-suicide! dur-line))
             current-dur-grobs)
            (set! current-dur-grobs #f)
            (set! stop-duration-line #f)))

      ;; house-keeping
      (set! rhyth-event #f)
      (set! skip #f)
      (set! mmr-event #f)))))

(ly:register-translator
 Duration_line_engraver 'Duration_line_engraver
 '((grobs-created . (DurationLine))
   (events-accepted . (duration-line-event))
   (properties-read . (currentCommandColumn
                       currentMusicalColumn
                       startAtSkip
                       endAtSkip
                       startAtNoteColumn
                       ))
   (properties-written . ())
   (description . "\
Engraver to print a line representing the duration of a rhythmic event like
@code{NoteHead}, @code{NoteColumn} or @code{Rest}.")))
