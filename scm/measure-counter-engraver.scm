;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2012--2026 David Nalesnik <david.nalesnik@gmail.com>
;;;;                          Jean Abou Samra <jean@abou-samra.fr>
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

(define (set-counter-text! grob
                           property
                           number
                           alternative-number
                           measure-pos
                           context)
  ;; FIXME: slight code duplication with Bar_number_engraver
  (let* ((style (ly:context-property context 'alternativeNumberingStyle))
         (number-alternatives (eq? style 'numbers-with-letters))
         (final-alt-number (if number-alternatives alternative-number 0))
         (formatter (ly:context-property context 'barNumberFormatter #f)))
    (if formatter
        (ly:grob-set-property! grob
                               property
                               (formatter number
                                          measure-pos
                                          (1- final-alt-number)
                                          context)))))

(define-public (Measure_counter_engraver context)
  (let ((count-spanner '()) ; a single element of the count
        (start-event #f)
        (go? #f) ; is the count in progress?
        (stop-event #f)
        (last-measure-seen 0)
        (last-alternative-number #f)
        ;; Acknowledge bar lines and start a new count when there
        ;; is one.  This is similar to the Bar_number_engraver.
        (first-time-step #t)
        (now-is-bar-line #t)
        (done-in-time-step #f)
        (first-measure-in-count 0))

    (make-engraver
     (listeners
      ((measure-counter-event engraver event)
       (cond
        ((= START (ly:event-property event 'span-direction))
         (let ((current-bar-number (ly:context-property context 'currentBarNumber)))
           (set! start-event event)
           (set! first-measure-in-count current-bar-number)
           ;; initialize one less so first measure receives a count spanner
           (set! last-measure-seen (1- current-bar-number))))
        ((= STOP (ly:event-property event 'span-direction))
         (set! stop-event event)))))

     (acknowledgers
      ((bar-line-interface engraver grob source-engraver)
       (set! now-is-bar-line #t)))

     ((process-acknowledged trans)
      (when (and now-is-bar-line
                 (not done-in-time-step))
        (let ((col (ly:context-property context 'currentCommandColumn))
              (measure-pos (ly:context-property context 'measurePosition))
              (current-bar (ly:context-property context 'currentBarNumber)))
          (set! done-in-time-step #t)
          ;; Each measure of a count receives a new spanner, which is bounded
          ;; by the first "command column" of that measure and the following one.
          (if (or (eq? #t (ly:context-property context 'measureStartNow))
                  ;; This first-time-step criterion also applies for a Staff
                  ;; created mid-piece; starting a measure counter mid-measure
                  ;; is not meaningful anyway.
                  first-time-step)
              (begin
                ;; Finish the previous count-spanner if there is one.
                (if (ly:grob? count-spanner)
                    (begin
                      (ly:spanner-set-bound! count-spanner RIGHT col)
                      (ly:pointer-group-interface::add-grob count-spanner 'columns col)
                      (ly:engraver-announce-end-grob trans count-spanner col)
                      (if (> current-bar (1+ last-measure-seen))
                          ;; Measure counter spanning over a compressed MM rest.
                          (let* ((counter (ly:grob-property count-spanner 'count-from))
                                 (right-number
                                  (1- (+ counter
                                         (- current-bar first-measure-in-count)))))
                            (set-counter-text!
                             count-spanner
                             'right-number-text
                             right-number
                             ;; Edge case of compressed MM rests in alternatives.
                             ;; It would be wrong to take the context's
                             ;; alternativeNumber here, because we are
                             ;; looking behind at the last measure before
                             ;; this one.  Actually, a compressed MM rest
                             ;; is one single time step, so there is no
                             ;; right time where we could look up the property.
                             ;; Fortunately, MM rests from different alternatives
                             ;; cannot be compressed together, so we can just take
                             ;; the alternative number that was current at the
                             ;; time of the start of this measure counter.
                             last-alternative-number
                             measure-pos
                             context)))
                      (set! count-spanner '())))
                (if stop-event
                    (set! go? #f))
                (if start-event
                    (if go?
                        (ly:event-warning start-event
                                          (G_ "count not ended before another begun"))
                        (set! go? #t)))
                ;; If count is in progress, begin a count-spanner.
                (if go?
                    (let* ((c (ly:engraver-make-grob trans 'MeasureCounter col))
                           (counter (ly:grob-property c 'count-from))
                           (left-number
                            (+ counter (- current-bar first-measure-in-count)))
                           (alternative-number
                            (ly:context-property context 'alternativeNumber 0)))
                      (ly:spanner-set-bound! c LEFT col)
                      (ly:pointer-group-interface::add-grob c 'columns col)
                      (set-counter-text! c
                                         'left-number-text
                                         left-number
                                         alternative-number
                                         measure-pos
                                         context)
                      (set! count-spanner c)
                      (set! last-alternative-number alternative-number)))))
          (set! last-measure-seen current-bar))))

     ((stop-translation-timestep trans)
      (set! start-event #f)
      (set! stop-event #f)
      (set! now-is-bar-line #f)
      (set! done-in-time-step #f)
      (set! first-time-step #f))

     ((finalize trans)
      (if go?
          (begin
            (set! go? #f)
            (ly:grob-suicide! count-spanner)
            (set! count-spanner '())
            (ly:warning (G_ "measure count left unfinished"))))))))

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
