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

(define-public Bend_spanner_engraver
  ;; Creates a BendSpanner, sets its bounds, keeps track and sets
  ;; details.successive-level in order to nest consecutive bends accordingly.
  ;;
  ;; Sets the property 'bend-me to decide which strings should be bent.
  ;; Per default open strings should not be bent unless the user forces it.
  ;; To know which note will be done on open strings we need to know the result
  ;; of the 'noteToFretFunction'.
  ;; User-specified StringNumbers are respected.
  ;; Fingerings are not needed for setting 'bend-me, thus we disregard them.
  (lambda (context)
    (let ((bend-spanner #f)
          (bend-start #f)
          (bend-stop #f)
          (previous-bend-dir #f)
          (nc-start #f)
          (successive-lvl #f)
          (tab-note-heads '()))
      (make-engraver
       ((initialize this-engraver)
        ;; Set 'supportNonIntegerFret #t, if unspecified
        (ly:context-set-property! context 'supportNonIntegerFret
                                  (ly:context-property context 'supportNonIntegerFret #t)))
       ((start-translation-timestep trans)
        ;; Clear 'tab-note-heads' in order not to confuse 'excluding notes
        ;; further below
        (set! tab-note-heads '())
        ;; Set 'bend-spanner' left-bound
        ;; We do it in start-translation-timestep, because here we have access
        ;; to the 'style-property
        (if (and bend-spanner nc-start)
            (begin
              (ly:spanner-set-bound! bend-spanner LEFT nc-start)
              ;; For consecutive BendSpanners, i.e. 'previous-bend-dir' is
              ;; not #f, in/decrease details.successive-level with
              ;;'previous-bend-dir' unless 'bend-style' is 'hold
              (if (and previous-bend-dir successive-lvl)
                  (let* ((hold-style?
                          (eq? (ly:grob-property bend-spanner 'style) 'hold))
                         (increase-lvl
                          (if hold-style? 0 previous-bend-dir)))
                    (ly:grob-set-nested-property!
                     bend-spanner
                     '(details successive-level)
                     (+ successive-lvl increase-lvl))
                    (set! nc-start #f))))))
       (listeners
        ((bend-span-event this-engraver event)
         (set! bend-start event)))
       (acknowledgers
        ((note-column-interface this-engraver grob source-engraver)
         ;; Set the 'bend-me property for notes to be played on open strings
         ;; Per default it will be set #f
         ;; Relies on context-property stringFretFingerList.
         ;;
         ;; Needs to be done here in acknowledgers for note-column-interface,
         ;; otherwise the calculation of bend-dir (relying only on notes
         ;; actually prepares for bending, i.e. 'bend-me should not be #f)
         ;; may cause wrong results.
         (for-each
          (lambda (tnh strg-frt-fngr)
            (if (eq? 0 (cadr strg-frt-fngr))
                (ly:grob-set-property! tnh 'bend-me
                                       (ly:grob-property tnh 'bend-me #f))))
          (reverse tab-note-heads)
          (ly:context-property context 'stringFretFingerList))

            ;;;;
         ;; End the bend-spanner, if found NoteColumn is suitable
            ;;;;
         (if (and bend-stop
                  (ly:spanner? bend-spanner)
                  (ly:grob-property grob 'bend-me #t)
                  (not (ly:spanner-bound bend-spanner RIGHT #f)))
             (let* ((nhds-array (ly:grob-object grob 'note-heads))
                    (nhds
                     (if (ly:grob-array? nhds-array)
                         (ly:grob-array->list nhds-array)
                         #f))
                    (style (ly:grob-property bend-spanner 'style 'default))
                    (boundable?
                     (and nhds
                          (or
                           (eq? style 'pre-bend)
                           (ly:grob-property grob 'bend-me #t)))))
               (if boundable?
                   (begin
                     (ly:spanner-set-bound! bend-spanner RIGHT grob)
                     (set! bend-stop #f)
                     ;; Keep track of 'successive-level, to place
                     ;; consecutive BendSpanners nicely and
                     ;; in/decrease with 'previous-bend-dir'
                     (let* ((details
                             (ly:grob-property bend-spanner 'details))
                            (successive-level
                             (assoc-get 'successive-level details))
                            (span-bound-pitches
                             (bounding-note-heads-pitches bend-spanner)))
                       (if (and (pair? (car span-bound-pitches))
                                (pair? (cdr span-bound-pitches)))
                           (let* ((quarter-diffs
                                   (get-quarter-diffs span-bound-pitches))
                                  (current-bend-dir
                                   (if (negative? quarter-diffs) DOWN UP))
                                  (consecutive-bend?
                                   (and previous-bend-dir
                                        (not (eqv? previous-bend-dir
                                                   current-bend-dir)))))
                             (set! successive-lvl successive-level)
                             (if consecutive-bend?
                                 (begin
                                   (ly:grob-set-nested-property!
                                    bend-spanner
                                    '(details successive-level)
                                    (+ successive-lvl current-bend-dir))
                                   (set! successive-lvl
                                         (+ successive-lvl
                                            current-bend-dir))))
                             (set! previous-bend-dir current-bend-dir)
                             (set! bend-spanner #f))
                           (begin
                             (ly:warning (G_ "No notes found to start from,
ignoring. If you want to bend an open string, consider to override/tweak the
'bend-me property."))
                             (ly:grob-suicide! bend-spanner)))))

                   (begin
                     (set! successive-lvl #f)
                     (set! previous-bend-dir #f)))))

            ;;;;
         ;; Create the bend-spanner grob, if found NoteColumn is suitable
            ;;;;
         (if (and (ly:stream-event? bend-start)
                  (ly:grob-array? (ly:grob-object grob 'note-heads)))
             (let* ((bend-grob
                     (ly:engraver-make-grob
                      this-engraver 'BendSpanner bend-start)))
               (set! bend-spanner bend-grob)
               (set! nc-start grob)
               (set! bend-start #f)
               (set! bend-stop #t))))
        ((tab-note-head-interface this-engraver grob source-engraver)
         (set! tab-note-heads (cons grob tab-note-heads))))
       ((stop-translation-timestep this-engraver)
        (ly:context-set-property! context 'stringFretFingerList '())
        ;; Clear some local variables if no bend-spanner is in work
        (if (and (not bend-start) (not bend-stop))
            (begin
              (set! previous-bend-dir #f)
              (set! successive-lvl #f))))
       ((finalize this-engraver)
        ;; final house-keeping
        (if bend-spanner
            (begin
              (ly:warning (G_ "Unbound BendSpanner, ignoring"))
              (ly:grob-suicide! bend-spanner))))))))

(ly:register-translator
 Bend_spanner_engraver 'Bend_spanner_engraver
 '((grobs-created . (BendSpanner))
   (events-accepted . (bend-span-event
                       note-event
                       string-number-event))
   (properties-read . (stringFretFingerList
                       supportNonIntegerFret))
   (properties-written . (stringFretFingerList
                          supportNonIntegerFret))
   (description . "\
Engraver to print a BendSpanner.")))
