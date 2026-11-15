;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2021--2026 Jean Abou Samra <jean@abou-samra.fr>
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

(define (Staff_highlight_engraver context)
  (let ((start-event #f)
        (stop-event #f)
        (highlight #f)
        (ended-highlight #f)
        ;; List of staff symbols "active" in this time step.  A staff symbol is
        ;; active from the time step of \startStaff (probably implicitly the
        ;; first time step) to the time step of \stopStaff (probably implicitly
        ;; the last time step).  Both ends are included.
        (active-staff-symbols '())
        ;; List of staff symbols considered for the whole highlight.
        ;; #f when there is no highlight in progress.
        (highlight-staff-symbols #f))
    ;; Properly handling ossia staves is a little tricky.
    ;; highlight-staff-symbols contains all staff symbols spanned.  Yet, if an
    ;; ossia staff ended where the highlight started, we actually don't want it.
    ;; It would be complicated to handle this by excluding it in
    ;; highlight-staff-symbols because the end of a staff symbol is actually
    ;; announced one time step later (because Staff_symbol_engraver terminates a
    ;; staff symbol implicitly in finalize).  So we filter those unwanted staff
    ;; symbols here.  Namely, if a staff symbol ended just where the highlight
    ;; started, remove it.  Also, if a staff symbol started just where the
    ;; highlight ended, it is implicitly excluded because
    ;; highlight-staff-symbols doesn't contain it yet.
    (define (set-ended-highlight-staff-symbols! grob)
      (ly:grob-set-object!
       grob
       'elements
       (ly:grob-list->grob-array
        (remove
         (lambda (staff-sym)
           (let ((right-bound (ly:spanner-bound staff-sym RIGHT #f)))
             (and right-bound
                  (equal? (grob::when right-bound)
                          (grob::when grob)))))
         highlight-staff-symbols))))
    (make-engraver
     (listeners
      ((staff-highlight-event engraver event)
       (if (eqv? START (ly:event-property event 'span-direction))
           (set! start-event event)
           (set! stop-event event))))
     ((process-music engraver)
      ;; ignore spurious event
      (when (and stop-event (not highlight))
        (ly:event-warning stop-event (G_ "no highlight to end")))
      ;; \staffHighlight implicitly ends the current highlight, if any,
      ;; acting like a \stopStaffHighlight.  This is handy when you want to
      ;; highlight every single measure.
      (let ((ender (or stop-event start-event)))
        (when (and highlight ender)
          (set! ended-highlight highlight)
          (set! highlight #f)
          (let ((non-musical-col
                 (ly:context-property context 'currentCommandColumn)))
            (ly:spanner-set-bound! ended-highlight RIGHT non-musical-col))
          (ly:engraver-announce-end-grob engraver ended-highlight ender)))
      (when start-event
        (set! highlight
              (ly:engraver-make-grob engraver 'StaffHighlight start-event))
        (let ((non-musical-col
               (ly:context-property context 'currentCommandColumn)))
          (ly:spanner-set-bound! highlight LEFT non-musical-col)))
      (when highlight
        (let ((musical-col
               (ly:context-property context 'currentMusicalColumn)))
          (ly:pointer-group-interface::add-grob highlight 'columns musical-col))))
     (acknowledgers
      ((staff-symbol-interface engraver grob source-engraver)
       (set! active-staff-symbols (cons grob active-staff-symbols))))
     (end-acknowledgers
      ((staff-symbol-interface engraver grob source-engraver)
       (set! active-staff-symbols (delq grob active-staff-symbols))))
     ((stop-translation-timestep engraver)
      (when ended-highlight
        (set-ended-highlight-staff-symbols! ended-highlight)
        (set! highlight-staff-symbols #f)
        (set! ended-highlight #f))
      (when start-event
        ;; Initialize highlight-staff-symbols for the new highlight.
        (set! highlight-staff-symbols '()))
      (when highlight-staff-symbols
        ;; Add new staff symbols found in this time step.  If a highlight was
        ;; just started, this also adds all staff symbols started earlier and
        ;; still current.  This way, when we want to end the highlight,
        ;; highlight-staff-symbols will be the list of all staff symbols that
        ;; were active at one or several of the time steps the highlight spans.
        (set! highlight-staff-symbols
              (lset-union eq? highlight-staff-symbols active-staff-symbols)))
      (set! start-event #f)
      (set! stop-event #f))
     ((finalize engraver)
      (when highlight
        ;; Implicitly terminate the current highlight.
        (let ((col (ly:context-property context 'currentCommandColumn)))
          (ly:spanner-set-bound! highlight RIGHT col))
        (set-ended-highlight-staff-symbols! highlight))))))

(ly:register-translator
 Staff_highlight_engraver 'Staff_highlight_engraver
 '((events-accepted . (staff-highlight-event))
   (grobs-created . (StaffHighlight))
   (properties-read . (currentCommandColumn))
   (properties-written . ())
   (description . "Highlights music passages.")))
