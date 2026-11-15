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

(define Horizontal_script_engraver
  (lambda (ctx)
    (let ((raw-scripts '())
          (nhds '())
          (nc #f)
          (stem #f)
          (acc-placement #f)
          (dot-col #f)
          (script-row #f))
      (make-engraver
       (acknowledgers
        ;; NB 'script-interface is currently used in
        ;; AccidentalSuggestion, CaesuraScript, DynamicText,
        ;; MultiMeasureRestScript and Script, thus we use
        ;; 'ledgered-grob-interface (used for Script and Custos).
        ((ledgered-grob-interface this-engraver grob source-engraver)
         (set! raw-scripts (cons grob raw-scripts)))
        ((script-column-interface this-engraver grob source-engraver)
         (when (eq? (grob::name grob) 'ScriptRow)
           (set! script-row grob)))
        ((note-column-interface this-engraver grob source-engraver)
         (set! nc grob))
        ((note-head-interface this-engraver grob source-engraver)
         (set! nhds (cons grob nhds)))
        ((stem-interface this-engraver grob source-engraver)
         (set! stem grob))
        ((accidental-placement-interface this-engraver grob source-engraver)
         (set! acc-placement grob))
        ((dot-column-interface this-engraver grob source-engraver)
         (set! dot-col grob)))
       ((process-music this-engraver)
        (let ((scripts
               ;; late filtration supports \tweak
               (filter
                (lambda (sc)
                  (and (grob::has-interface sc 'script-interface)
                       (eqv? (ly:grob-property sc 'side-axis) X)))
                raw-scripts)))
          ;; Proceed only if script-grobs with side-axis X are found
          (when (pair? scripts)
            (let* ((dots-array (if dot-col (ly:grob-object dot-col 'dots) #f))
                   (dots
                    (if dots-array
                        (ly:grob-array->list dots-array)
                        '())))
              (if script-row
                  ;; If ScriptRow is present, its 'side-support-elements
                  ;; already contains probable Arpeggio, Accidental and
                  ;; AccidentalPlacement grobs. We add dots in order to avoid
                  ;; collisions for Script added at right side of a dotted
                  ;; note.
                  (for-each
                   (lambda (sc)
                     (let* ((side-support-elements
                             (ly:grob-object sc 'side-support-elements '())))
                       (ly:grob-set-object! sc 'side-support-elements
                                            (ly:grob-list->grob-array
                                             (append
                                              dots
                                              (ly:grob-array->list side-support-elements))))))
                   scripts)
                  ;; If ScriptRow does not exist yet, we manually add
                  ;; Accidentals, 'conditional-elements (i.e. Arpeggio),
                  ;; Stem, NoteHeads and Dots to 'side-support-elements of
                  ;; the Script.
                  ;; Script_row_engraver may then create a ScriptRow-grob.
                  (let* ((acc-alist
                          (if (ly:grob? acc-placement)
                              (ly:grob-object acc-placement 'accidental-grobs)
                              #f))
                         (accs
                          (if acc-alist
                              (map cadr acc-alist)
                              '()))
                         (conditional-elts-array
                          (if nc
                              (ly:grob-object nc 'conditional-elements #f)
                              #f))
                         (conditional-elts
                          (if conditional-elts-array
                              (ly:grob-array->list conditional-elts-array)
                              '())))
                    (for-each
                     (lambda (sc)
                       (ly:grob-set-object! sc 'side-support-elements
                                            (ly:grob-list->grob-array
                                             (append
                                              (list stem)
                                              conditional-elts
                                              accs
                                              nhds
                                              dots))))
                     scripts)))
              ;; Set Script's Y-parent to NoteHead
              ;;
              ;; Script applied to a stand-alone note-head or inside of an
              ;; event-chord has already note-head as X- or Y-parent.
              ;; Catch it.
              ;; Script applied to an event-chord has note-column as
              ;; X-parent. Best we can do is to catch the first note-head.
              ;; This may not fulfill what a user has in mind, better to
              ;; use in-chord script then.
              (for-each
               (lambda (sc)
                 (let* ((par-X (ly:grob-parent sc X))
                        (par-Y (ly:grob-parent sc Y))
                        (new-par-Y
                         (cond
                          ((and (ly:grob? par-X)
                                (grob::has-interface
                                 par-X 'note-column-interface))
                           (car
                            (ly:grob-array->list
                             (ly:grob-object par-X 'note-heads))))
                          ((and (ly:grob? par-X)
                                (grob::has-interface
                                 par-X 'note-head-interface))
                           par-X)
                          ((and (ly:grob? par-Y)
                                (grob::has-interface
                                 par-Y 'note-head-interface))
                           par-Y)
                          (else
                           (ly:warning
                            "Need NoteHead as parent for Script.")))))
                   (ly:grob-set-parent! sc Y new-par-Y)))
               scripts)))

          (set! raw-scripts '())
          (set! nhds '())
          (set! nc #f)
          (set! stem #f)
          (set! acc-placement #f)
          (set! dot-col #f)
          (set! script-row #f)))))))

(ly:register-translator
 Horizontal_script_engraver 'Horizontal_script_engraver
 '((events-accepted . ())
   (grobs-created . ())
   (properties-read . ())
   (properties-written . ())
   (description . "Aligns @code{Script} horizontally")))
