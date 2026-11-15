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

(define-public Glissando_engraver
  (lambda (context)
    (let ((glissando-event #f)
          (start-glissando-line #f)
          (stop-glissando-line #f)
          (current-glissando-grobs '())
          (kill-me '())
          (gliss-map #f))

      (define (bounds lst grobs start-stop)
        ;; `lst` is supposed to be the number-pair-list from `glissandoMap`
        ;; `grobs` are usually note-head grobs
        ;; `start-stop` is car or cdr of a pair
        ;;
        ;; Selects possible left/right bounds for Glissando.
        (if (number-pair-list? lst)
            (let* ((user-start-stops (map start-stop lst))
                   (start-stops
                    (filter
                     (lambda (n)
                       (and (index? n) (<= n (1- (length grobs)))))
                     user-start-stops)))
              (map
               (lambda (start-stop) (list-ref grobs start-stop))
               start-stops))
            grobs))

      (make-engraver
       (listeners
        ((glissando-event this-engraver event)
         (set! glissando-event event)
         (set! start-glissando-line #t)))

       (acknowledgers
        ((note-column-interface this-engraver grob source-engraver)
         (let* ((note-heads-array (ly:grob-object grob 'note-heads #f))
                (note-heads
                 (if note-heads-array
                     (ly:grob-array->list note-heads-array)
                     '()))
                (glissando-skip?
                 (ly:grob-property grob 'glissando-skip #f)))

           ;; Do not start a new Glissando, if the last is not yet ended.
           ;; This may happen if 'glissando-skip is used.
           (when (and glissando-skip? start-glissando-line stop-glissando-line)
             (ly:warning (G_ "overwriting glissando"))
             (set! glissando-event #f)
             (set! start-glissando-line #f))

           (when (and (not glissando-skip?) stop-glissando-line)
             ;; set 'glissando-index
             (for-each
              (lambda (gliss i) (ly:grob-set-property! gliss 'glissando-index i))
              current-glissando-grobs
              (iota (length note-heads)))

             (let ((possible-right-bounds (bounds gliss-map note-heads cdr)))

               ;; Set right bound.
               (for-each
                (lambda (bound gliss )
                  (ly:spanner-set-bound! gliss RIGHT bound)
                  (ly:engraver-announce-end-grob this-engraver gliss bound))
                possible-right-bounds
                current-glissando-grobs)

               ;; Throw away unneeded Glissandi, but postpone it until other
               ;; engravers have acknowledged the end of the spanner.
               ;; This avoids problems like issue 6946
               (for-each
                (lambda (gliss)
                  (when (not (ly:grob? (ly:spanner-bound gliss RIGHT)))
                    (ly:engraver-announce-end-grob this-engraver gliss
                      (ly:context-property context 'currentMusicalColumn))
                    (set! kill-me (cons gliss kill-me))))
                current-glissando-grobs)

               (set! stop-glissando-line #f)
               (set! current-glissando-grobs '())))

           (when start-glissando-line
             (set! gliss-map (ly:context-property context 'glissandoMap #f))
             (let ((possible-left-bounds (bounds gliss-map note-heads car)))
               (set! start-glissando-line #f)
               (set! stop-glissando-line #t)
               (set! current-glissando-grobs
                     (append
                      (map
                       (lambda (bound)
                         (let ((gliss
                                (ly:engraver-make-grob
                                 this-engraver 'Glissando glissando-event)))
                           (ly:spanner-set-bound! gliss LEFT bound)
                           gliss))
                       possible-left-bounds)
                      current-glissando-grobs))
               (set! glissando-event #f))))))
       ((stop-translation-timestep this-engraver)
        (for-each ly:grob-suicide! kill-me)
        (set! kill-me '()))
       ((finalize this-engraver)
        (when (pair? current-glissando-grobs)
          (for-each
           (lambda (gliss)
             (ly:warning (G_ "unterminated Glissando"))
             (ly:grob-suicide! gliss))
           current-glissando-grobs)
          (set! current-glissando-grobs '())
          (set! gliss-map #f)
          (set! stop-glissando-line #f)))))))

(ly:register-translator
 Glissando_engraver 'Glissando_engraver
 '((grobs-created . (Glissando))
   (events-accepted . (glissando-event))
   (properties-read . (glissandoMap))
   (properties-written . ())
   (description . "Engrave glissandi.")))
