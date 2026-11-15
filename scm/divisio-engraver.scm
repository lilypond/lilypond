;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2022--2026 Dan Eble <nine.fierce.ballads@gmail.com>
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

(define-public (Divisio_engraver context)
  ;; "div info" is (priority breath-type event)
  (define (div-info-max old new)
    (if (> (car new) (car old))
        new
        old))

  (let ((div-info (list 0 #f #f))
        (grob #f))

    (make-engraver
     (listeners
      ((volta-repeat-start-event engraver event #:once)
       (set! div-info (div-info-max div-info
                                    (list 5 'chantdoublebar event))))
      ((volta-repeat-end-event engraver event #:once)
       (set! div-info (div-info-max div-info
                                    (list 4 'chantdoublebar event))))
      ((fine-event engraver event #:once)
       (set! div-info (div-info-max div-info
                                    (list 3 'chantdoublebar event))))
      ((section-event engraver event #:once)
       (set! div-info (div-info-max div-info
                                    (list 2 'chantdoublebar event))))
      ((caesura-event engraver event #:once)
       (set! div-info (div-info-max div-info
                                    (list 1 'fromcontext event)))))

     ((process-music engraver)
      (let* ((b-type (cadr div-info))
             (event (caddr div-info))
             (props (if (eq? b-type 'fromcontext)
                        (ly:context-property context 'caesuraType)
                        `((breath . ,b-type)))))
        ;; Add the user's articulations to the caesuraType.
        (let ((art-types
               (if (ly:stream-event? event)
                   (map (lambda (art-event)
                          (ly:event-property art-event 'articulation-type))
                        (ly:event-property event 'articulations))
                   '())))
          (set! props (acons 'articulations art-types props)))
        ;; Pass the caesuraType through the transform function, if it is set.
        (let ((transform (ly:context-property context 'caesuraTypeTransform)))
          (when (procedure? transform)
            (set! props (transform context props '()))))
        (set! b-type (assq-ref props 'breath))
        (when (symbol? b-type)
          (set! grob (ly:engraver-make-grob engraver 'Divisio event))
          (ly:breathing-sign::set-breath-properties grob context b-type))))

     ((stop-translation-timestep engraver)
      (set! div-info (list 0 #f #f))
      (set! grob #f)))))

(ly:register-translator
 Divisio_engraver 'Divisio_engraver
 '((grobs-created . (Divisio))
   (events-accepted . (caesura-event
                       fine-event
                       section-event
                       volta-repeat-end-event
                       volta-repeat-start-event))
   (properties-read . (caesuraType
                       caesuraTypeTransform))
   (properties-written . ())
   (description . "Create divisiones: chant notation for points of
breathing or caesura.")))
