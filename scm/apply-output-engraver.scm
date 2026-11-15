;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2026 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

(define-public (Apply_output_engraver context)
;;; This engraver is the engine that makes \applyOutput work.
;;;
;;; At each timestep, we collect \applyOutput requests (i.e.
;;; functions that should be applied to grobs in a given
;;; context) and then apply them during the Grob acknowledging
;;; stage.
  (let ((requests '()))
    (make-engraver
     (listeners
      ((apply-output-event engraver event)
       (when (ly:context-alias?
              context
              (ly:event-property event 'context-type))
         (set! requests (cons event requests)))))
     (acknowledgers
      ((grob-interface engraver grob source-engraver)
       (for-each
        (lambda (req)
          (let ((req-grob (ly:event-property req 'symbol))
                (req-proc (ly:event-property req 'procedure)))
            (when (or (not (symbol? req-grob))
                      (eq? req-grob (grob::name grob)))
              (req-proc grob
                        (ly:translator-context source-engraver)
                        context))))
        requests)))
     ((stop-translation-timestep engraver)
      (set! requests '())))))

(ly:register-translator
 Apply_output_engraver 'Apply_output_engraver
 '((events-accepted . (apply-output-event))
   (grobs-created . ())
   (properties-read . ())
   (properties-written . ())
   (description . "Apply a procedure to any grob acknowledged.")))
