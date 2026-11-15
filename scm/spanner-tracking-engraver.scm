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

(define (Spanner_tracking_engraver context)
  ;; Naming note: "spanner" is the grob we take care of
  ;; (e.g., a footnote) and "host" is the grob that
  ;; "spanner" is attached to (e.g., the annotated grob).
  (let (
        ;; Map host spanners to lists of (spanner . source-engraver)
        ;; pairs.  We keep the source-engraver so we can announce the end
        ;; from it rather than from this engraver.
        (table (make-hash-table)))
    (make-engraver
     (acknowledgers
      ((sticky-grob-interface engraver grob source-engraver)
       (if (ly:spanner? grob)
           (let ((host (ly:grob-object grob 'sticky-host)))
             (hashq-set! table
                         host
                         (cons (cons grob source-engraver)
                               (hashq-ref table host '())))))))
     (end-acknowledgers
      ((spanner-interface engraver host source-engraver)
       (let ((spanners (hashq-ref table host)))
         (if spanners
             (begin
               (for-each
                (lambda (spanner-engraver-pair)
                  (let ((spanner (car spanner-engraver-pair))
                        (source-engraver (cdr spanner-engraver-pair)))
                    (ly:engraver-announce-end-grob source-engraver spanner host)))
                spanners)
               (hashq-remove! table host)))))))))

(ly:register-translator
 Spanner_tracking_engraver 'Spanner_tracking_engraver
 '((grobs-created . ())
   (events-accepted . ())
   (properties-read . ())
   (properties-written . ())
   (description . "Helper for creating spanners attached to other spanners.
If a spanner has the @code{sticky-grob-interface}, the engraver tracks the
spanner contained in its @code{sticky-host} object.  When the host ends,
the sticky spanner attached to it has its end announced too.")))
