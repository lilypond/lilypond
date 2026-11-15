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

(define (Lyric_repeat_count_engraver context)
  (let ((end-event '()))
    (make-engraver
     (listeners
      ((volta-repeat-end-event engraver event #:once)
       (let ((count (ly:event-property event 'repeat-count 0)))
         (if (positive? count)
             (set! end-event event)))))

     ((process-music engraver)
      (if (ly:stream-event? end-event)
          (let* ((count (ly:event-property end-event 'repeat-count))
                 (formatter (ly:context-property
                             context 'lyricRepeatCountFormatter))
                 (text (and (procedure? formatter) (formatter context count))))
            (if (markup? text)
                (let ((grob (ly:engraver-make-grob
                             engraver 'LyricRepeatCount end-event)))
                  (ly:grob-set-property! grob 'text text))))))

     ((stop-translation-timestep engraver)
      (set! end-event '())))))

(ly:register-translator
 Lyric_repeat_count_engraver 'Lyric_repeat_count_engraver
 '((grobs-created . (LyricRepeatCount))
   (events-accepted . (volta-repeat-end-event))
   (properties-read . (lyricRepeatCountFormatter))
   (properties-written . ())
   (description . "Create repeat counts within lyrics for modern
transcriptions of Gregorian chant.")))
