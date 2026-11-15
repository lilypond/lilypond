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

(define (Tempo_align_engraver context)

  (define (no-grob-move-y grob)
    (ly:grob-set-property! grob 'outside-staff-priority '())
    (ly:grob-set-property! grob 'padding '())
    (ly:grob-set-property! grob 'Y-offset '()))

  (let ((metronome-mark #f)
        (tempo-mark #f)
        (tempo-spanner #f)
        (prev-tempo-spanner #f)
        (tempo-spanner-dir #f)
        (tempo-line-span #f)
        (tempo-line-span-dir #f))

    (make-engraver

      (acknowledgers
        ((tempo-mark-interface this-engraver grob source-engraver)
         (set! tempo-mark grob))
        ((metronome-mark-interface this-engraver grob source-engraver)
          (set! metronome-mark grob))
        ((tempo-spanner-interface this-engraver grob source-engraver)
          (set! tempo-spanner grob)
          (set! tempo-spanner-dir (ly:grob-property grob 'direction))))
      ((process-acknowledged this-engraver)
        ;; Don't touch tempo or metronome mark if neither TempoLineSpanner nor
        ;; TempoSpanner is present.
        (when (and (not tempo-line-span) (not tempo-spanner))
          (set! tempo-mark #f)
          (set! metronome-mark #f))
        ;; Continue an already started TempoLineSpanner. Or end and restart
        ;; if direction changes or the break-flag is set.
        (when (and tempo-line-span prev-tempo-spanner tempo-spanner)
          (if (or (not (equal? tempo-spanner-dir tempo-line-span-dir))
                  (ly:grob-property prev-tempo-spanner 'spanner-broken #f))
              (let ((ccc (ly:context-property context 'currentCommandColumn)))
                (ly:spanner-set-bound! tempo-line-span RIGHT
                  (or metronome-mark tempo-mark ccc))
                (ly:engraver-announce-end-grob
                  this-engraver
                  tempo-line-span
                  (ly:grob-property (or metronome-mark tempo-mark ccc) 'cause))
                (set! tempo-line-span
                      (ly:engraver-make-grob
                        this-engraver
                        'TempoLineSpanner
                        tempo-spanner))
                (ly:spanner-set-bound! tempo-line-span LEFT
                  (or tempo-mark metronome-mark ccc))
                (set! tempo-line-span-dir
                      (ly:grob-property tempo-spanner 'direction))
                (ly:grob-set-property!
                  tempo-line-span 'direction tempo-spanner-dir)
                ;; Adding 'elements
                (ly:axis-group-interface::add-element
                  tempo-line-span tempo-spanner)
                (when tempo-mark
                  (no-grob-move-y tempo-mark)
                  (ly:axis-group-interface::add-element
                    tempo-line-span tempo-mark))
                (when metronome-mark
                  (no-grob-move-y metronome-mark)
                  (ly:axis-group-interface::add-element
                    tempo-line-span metronome-mark))
                (set! prev-tempo-spanner tempo-spanner)
                (set! tempo-spanner #f)
                (set! tempo-mark #f)
                (set! metronome-mark #f)
                (set! tempo-line-span-dir #f))
              ;; For not broken TempoLineSpanner, fill 'elements.
              (begin
                ;; Adding 'elements
                (ly:axis-group-interface::add-element
                  tempo-line-span tempo-spanner)
                (when tempo-mark
                  (no-grob-move-y tempo-mark)
                  (ly:axis-group-interface::add-element
                    tempo-line-span tempo-mark))
                (when metronome-mark
                  (no-grob-move-y metronome-mark)
                  (ly:axis-group-interface::add-element
                    tempo-line-span metronome-mark))
                (set! prev-tempo-spanner tempo-spanner)
                (set! tempo-spanner #f)
                (set! tempo-mark #f)
                (set! metronome-mark #f))))

        ;; End TempoLineSpanner at a mark, otherwise let run to the very end
        (when (and tempo-line-span
                   (not tempo-spanner)
                   prev-tempo-spanner
                   (or metronome-mark tempo-mark))
          (ly:spanner-set-bound!
            tempo-line-span RIGHT (or metronome-mark tempo-mark))
          (ly:engraver-announce-end-grob
            this-engraver
            tempo-line-span
            (ly:grob-property (or metronome-mark tempo-mark) 'cause))
          ;; Adding to 'elements
          (when
            (not (ly:grob-property prev-tempo-spanner 'spanner-broken #f))
            (when tempo-mark
              (no-grob-move-y tempo-mark)
              (ly:axis-group-interface::add-element
                tempo-line-span tempo-mark))
            (when metronome-mark
              (no-grob-move-y metronome-mark)
              (ly:axis-group-interface::add-element
                tempo-line-span metronome-mark)))
          (set! prev-tempo-spanner #f)
          (set! tempo-spanner-dir #f)
          (set! metronome-mark #f)
          (set! tempo-mark #f)
          (set! tempo-line-span-dir #f)
          (set! tempo-line-span #f))

        ;; Start TempoLineSpanner
        (when (and (not tempo-line-span) tempo-spanner)
          (let ((ccc (ly:context-property context 'currentCommandColumn))
                (t-l-sp
                  (ly:engraver-make-grob
                    this-engraver
                    'TempoLineSpanner
                    tempo-spanner)))
            ;; Take direction of TempoLineSpanner from TempoSpanner
            (ly:grob-set-property! t-l-sp 'direction tempo-spanner-dir)
            (set! tempo-line-span-dir tempo-spanner-dir)
            (ly:spanner-set-bound!
              t-l-sp LEFT (or tempo-mark metronome-mark ccc))
            ;; Adding to 'elements
            (ly:axis-group-interface::add-element t-l-sp tempo-spanner)
            (when tempo-mark
              (no-grob-move-y tempo-mark)
              (ly:axis-group-interface::add-element t-l-sp tempo-mark))
            (when metronome-mark
              (no-grob-move-y metronome-mark)
              (ly:axis-group-interface::add-element t-l-sp metronome-mark))
            (set! tempo-line-span t-l-sp)
            (set! tempo-mark #f)
            (set! metronome-mark #f)
            (set! prev-tempo-spanner tempo-spanner)
            (set! tempo-spanner #f))))

      ((stop-translation-timestep this-engraver)
        ;; Don't touch tempo or metronome mark if neither TempoLineSpanner nor
        ;; TempoSpanner is present.
        (when (and (not tempo-line-span) (not tempo-spanner))
          (set! tempo-mark #f)
          (set! metronome-mark #f)))

      ((finalize this-engraver)
        ;; A TempoLineSpanner without finishing mark runs to the very end of
        ;; the score, i.e terminate it at final NonMusicalPaperColumn.
        (when (and tempo-line-span
              (null? (ly:spanner-bound tempo-line-span RIGHT)))
          (let ((ccc (ly:context-property context 'currentCommandColumn)))
            (ly:spanner-set-bound! tempo-line-span RIGHT ccc)
            (ly:engraver-announce-end-grob
              this-engraver
              tempo-line-span
              (ly:grob-property ccc 'cause))))))))

(ly:register-translator
 Tempo_align_engraver 'Tempo_align_engraver
 '((events-accepted . (break-tempo-span-event))
   (grobs-created . (TempoLineSpanner))
   (properties-read . (currentCommandColumn))
   (properties-written . ())
   (description . "Align tempo mark, tempo spanner and metronome mark on a
horizontal line.")))
