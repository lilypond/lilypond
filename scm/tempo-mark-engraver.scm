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

(define (Tempo_mark_engraver context)
  (let ((evt #f)
        (grob #f))
    (make-engraver
     (listeners
      ((tempo-mark-event this-engraver event)
       ;; We only accept one TempoMark per timestep.
       ;; Warn, if multiple ones are entered and drop the additional one(s).
       (if evt
           (begin
            (ly:input-warning
             (ly:event-property evt 'origin)
             "conflict with event: `~a'"
             (car (ly:event-property evt 'class)))
            (ly:input-warning
             (ly:event-property event 'origin)
             "discarding event: `~a'"
             (car (ly:event-property event 'class))))
           (set! evt event))))
     ((process-music this-engraver)
      (when evt
       (set! grob (ly:engraver-make-grob this-engraver 'TempoMark evt))
       (set! evt #f)))
     ((stop-translation-timestep this-engraver)
      (when grob
       (let ((staves (ly:context-property context 'stavesFound)))
         (ly:grob-set-object!
          grob
          'side-support-elements
          (ly:grob-list->grob-array staves))
         (set! grob #f)))))))

(ly:register-translator
 Tempo_mark_engraver 'Tempo_mark_engraver
 '((events-accepted . (tempo-mark-event))
   (grobs-created . (TempoMark))
   (properties-read . (stavesFound))
   (properties-written . ())
   (description . "Creates @code{TempoMark}.")))
