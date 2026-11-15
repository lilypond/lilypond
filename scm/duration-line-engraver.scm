;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2020--2026 Thomas Morley <thomasmorley65@gmail.com>
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

(define-public (duration-line::calc-thickness grob)
  ;; The visible thickness of DurationLine follows staff space.
  ;; Though, if ly:line-interface::line is used in the stencil, it looks at
  ;; grob.thickness not staff space. Thus we recalculate grob.thickness here.
  (let ((style (ly:grob-property grob 'style))
        (grob-thickness (ly:grob-property grob 'thickness))
        (layout-line-thick (layout-line-thickness grob))
        (staff-space (ly:staff-symbol-staff-space grob))
        (staff-line-thickness
         (ly:staff-symbol-line-thickness grob)))

    (* staff-space
       (if (eq? style 'beam)
           (* grob-thickness layout-line-thick)
           (* grob-thickness
              (/  layout-line-thick staff-line-thickness))))))

(define-public (Duration_line_engraver context)
  (let ((dur-event #f)
        (start-duration-line #f)
        (stop-duration-line #f)
        (current-dur-grobs #f)
        (created '())
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
                (let* ((bound-details
                        (ly:grob-property dur-line 'bound-details))
                       (right-details (assoc-get 'right bound-details))
                       (end-on-break-align-group?
                        (assoc-get 'end-on-break-align-group right-details)))
                  (ly:spanner-set-bound! dur-line RIGHT
                                         (if end-on-break-align-group?
                                             (ly:context-property context 'currentCommandColumn)
                                             (ly:context-property context 'currentMusicalColumn)))
                  (ly:engraver-announce-end-grob this-engraver dur-line grob)))
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
                     (set! created (cons dur-line created))
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
                         (set! created (cons dur-line created))
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
                            (set! created (cons dur-line created))
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
                         (set! created (cons dur-line created))
                         (list dur-line)))))
               (set! dur-event #f))))))

     ((process-music this-engraver)
      ;; If 'endAtSkip is set #t, DurationLine may end at skips.
      ;; In this case set right bound to PaperColumn
      (if (and (pair? current-dur-grobs)
               (ly:stream-event? skip)
               (ly:context-property context 'endAtSkip #f)
               stop-duration-line)
          (begin
            (for-each
             (lambda (dur-line)
               (if (not (ly:spanner-bound dur-line RIGHT #f))
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
                    (set! created (cons dur-line created))
                    (list dur-line)))
            (set! skip #f)
            (set! dur-event #f))))
     ((stop-translation-timestep this-engraver)
      ;; If no duration-line is active clear local `tie`.
      (when (and (not start-duration-line) (not stop-duration-line))
        (set! tie #f))
      ;; If a context dies or "pauses" (i.e. no rhythmic-event for some time,
      ;; because other contexts are active), set right bound to
      ;; NonMusicalPaperColumn.
      ;; We calculate the end-moment of the rhythmic-event and compare with
      ;; current-moment to get the condition for ending the DurationLine.
      ;; We can't go for (ly:context-property context 'busyGrobs), because
      ;; we then wouldn't know if a skip-event needs to be respected.

      (if rhyth-event
          (let* ((rhythmic-evt-start (car rhyth-event))
                 (rhythmic-evt-length
                  (ly:prob-property (cdr rhyth-event) 'length ZERO-MOMENT))
                 (rhythmic-evt-end
                  (ly:moment-add rhythmic-evt-start rhythmic-evt-length))
                 (current-moment (ly:context-current-moment context)))
            (if (and (equal? current-moment rhythmic-evt-end)
                     (pair? current-dur-grobs)
                     stop-duration-line)
                (begin
                  (for-each
                   (lambda (dur-line)
                     (if (not (ly:spanner-bound dur-line RIGHT #f))
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
      ;; All created DurationLines were cumulated in `created'.
      ;; Their 'thickness needs to be adjusted to follow staff space, not staff
      ;; symbol thickness, which ly:line-interface::line does.
      ;; We do it here to avoid multiple resetting DurationLine.thickness for
      ;; broken ones. Furthermore 'staff-symbol is not always available at
      ;; earlier steps.
      (for-each
       (lambda (dur-line)
         (ly:grob-set-property! dur-line 'thickness
                                (duration-line::calc-thickness dur-line)))
       created)
      ;; likely unneeded, better be paranoid
      (if (pair? current-dur-grobs)
          (begin
            (for-each
             (lambda (dur-line)
               (ly:warning (G_ "unterminated DurationLine"))
               (ly:grob-suicide! dur-line))
             current-dur-grobs)
            (set! current-dur-grobs #f)
            (set! stop-duration-line #f)))))))

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
