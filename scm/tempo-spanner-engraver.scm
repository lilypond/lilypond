;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2026 Thomas Morley <thomasmorley65@gmail.com>
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

(define (Tempo_spanner_engraver context)

  (define (tempo-end-mark? grob)
    (and
      grob
      (grob::has-interface grob 'tempo-mark-interface)
      (equal? (ly:grob-property grob 'break-visibility) #(#t #t #f))
      (ly:grob-property grob 'non-musical #f)
      (positive? (ly:grob-property grob 'self-alignment-X))))

  (let ((tempo-span #f)
        (tempo-finished #f)
        (previous-tempo-spanner #f)
        (tempo-span-evt #f)
        (tempo-break-evt #f)
        (bar-line #f)
        (curr-tempo-mark #f)
        (curr-metronome-mark #f)
        (mm #f))
     (make-engraver
      (listeners
        ((tempo-span-event this-engraver event)
          (set! tempo-span-evt event))
        ((break-tempo-span-event this-engraver event)
          (set! tempo-break-evt #t)))
      (acknowledgers
        ((metronome-mark-interface this-engraver grob source-engraver)
          (set! mm grob)
          (set! curr-metronome-mark grob))
        ((tempo-mark-interface this-engraver grob source-engraver)
          (set! curr-tempo-mark grob))
        ((bar-line-interface this-engraver grob source-engraver)
         (set! bar-line grob)))
      ((process-music this-engraver)
       (when tempo-span-evt
         (set! tempo-span
               (ly:engraver-make-grob
                 this-engraver 'TempoSpanner tempo-span-evt))))
      ((process-acknowledged this-engraver)
        ;; Set right bound.
        ;; If 'to-barline is set and a measure BarLine is present, to BarLine
        ;; otherwise to MetronomeMark or TempoMark if present.
        ;; Otherwise, see `finalize`
        (when (and tempo-finished
                   (null? (ly:spanner-bound tempo-finished RIGHT))
                   (or curr-metronome-mark curr-tempo-mark))
          (ly:spanner-set-bound! tempo-finished RIGHT
            (or (and (equal? (ly:context-property context 'measurePosition)
                             ZERO-MOMENT)
                     (ly:grob-property tempo-finished 'to-barline #f)
                     bar-line)
                ;; Prefer `tempoEndMark` over metronome-mark over simple
                ;; tempo-mark
                (if (tempo-end-mark? curr-tempo-mark)
                    curr-tempo-mark
                    #f)
                curr-metronome-mark
                curr-tempo-mark))
          (ly:engraver-announce-end-grob this-engraver tempo-finished '())
          ;; Maintain 'spanner-broken
          ;;
          ;; If the line of a tempo spanner is hidden, end the alignment spanner
          ;; early: this allows tempo marks to be spaced individually instead of
          ;; being linked together.
          ;; (Analogous to dynamics, see comment in dynamic-engraver.cc)
          ;;
          ;; Always do so if a break-tempo-span-event is present.
          (when (or (eq? (ly:grob-property tempo-finished 'style) 'none)
                    tempo-break-evt)
            (ly:grob-set-property! tempo-finished 'spanner-broken #t))
          ;; `tempo-finished` may not be available later, thus store the ready
          ;; TempoSpanner and clear `tempo-break-evt` in
          ;; `stop-translation-timestep`.
          (set! previous-tempo-spanner tempo-finished)
          (set! tempo-finished #f)
          (set! bar-line #f)))
      ((stop-translation-timestep this-engraver)
        (when (and previous-tempo-spanner tempo-break-evt)
          (set! tempo-break-evt #f))
        (when (and (not tempo-span) (not tempo-finished))
          (set! curr-tempo-mark #f)
          (set! curr-metronome-mark #f))
        ;; Maintain 'spanner-broken
        (when (and tempo-span tempo-break-evt)
          (ly:grob-set-property! tempo-span 'spanner-broken #t)
          (set! tempo-break-evt #f))
        ;; Set left bound to TempoMark, MetronomeMark or NonMusicalPaperColumn
        (when (and tempo-span (null? (ly:spanner-bound tempo-span LEFT)))
          (ly:spanner-set-bound! tempo-span LEFT
            ;; Do not use an tempo-end-mark for left bound
            (or (if (tempo-end-mark? curr-tempo-mark)
                    #f
                    curr-tempo-mark)
                curr-metronome-mark
                (ly:context-property context 'currentCommandColumn)))
          (set! curr-metronome-mark #f)
          (set! curr-tempo-mark #f)
          (set! tempo-finished tempo-span)
          (set! tempo-span #f)
          (set! tempo-span-evt #f)))
      ((finalize this-engraver)
        (let ((ccm (ly:context-current-moment context))
              (ccc (ly:context-property context 'currentCommandColumn)))
          ;; Workaround to get MetronomeMark printed, if it is last in score
          (when (and mm (equal? (grob::when mm) ccm))
            (ly:grob-set-property! mm 'break-visibility #(#t #t #t))
            (ly:grob-set-property! mm 'self-alignment-X RIGHT)
            (ly:grob-set-property! mm 'non-musical #t)
            (ly:grob-set-parent! mm X ccc))
          ;; Set right bound of a not terminated TempoSpanner to
          ;; NonMusicalPaperColumn
          (when (and tempo-finished
                     (null? (ly:spanner-bound tempo-finished RIGHT)))
             (ly:spanner-set-bound! tempo-finished RIGHT ccc)
             (ly:engraver-announce-end-grob this-engraver tempo-finished '())
             (when tempo-break-evt
               (ly:grob-set-property! tempo-finished 'spanner-broken #t)
               (set! tempo-break-evt #f))
             (set! tempo-finished #f))
          (set! bar-line #f)
          (set! curr-metronome-mark #f))))))

(ly:register-translator
 Tempo_spanner_engraver 'Tempo_spanner_engraver
 '((grobs-created . (TempoSpanner))
   (events-accepted . (tempo-span-event break-tempo-span-event))
   (properties-read . (currentMusicalColumn measurePosition))
   (properties-written . ())
   (description . "Creates @code{TempoSpanner}, connecting @code{TempoMark}
or @code{MetronomeMark}")))
