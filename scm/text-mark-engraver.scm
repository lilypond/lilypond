;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2022--2026 Jean Abou Samra <jean@abou-samra.fr>
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

(define (Text_mark_engraver context)
  (let ((evs '())
        (grobs '()))
    (make-engraver
     (listeners
      ((text-mark-event engraver event)
       (set! evs (cons event evs))))
     ((process-music engraver)
      (let ((i 0))
        (for-each
         (lambda (ev)
           (let* ((grob (ly:engraver-make-grob engraver 'TextMark ev))
                  ;; FIXME: this is not the only place where we sneakily modify
                  ;; values of outside-staff-priority to enforce a deterministic
                  ;; order.  Improve.
                  (osp (ly:grob-property grob 'outside-staff-priority #f)))
             (when osp ; if unset, leave it unset
               (ly:grob-set-property! grob 'outside-staff-priority (+ osp i))
               (set! i (1+ i)))
             (set! grobs (cons grob grobs))))
         ;; The default order has the first mark heard closest to the staff.
         (reverse evs))))
     ((stop-translation-timestep engraver)
      (let ((staves (ly:context-property context 'stavesFound)))
        (for-each
         (lambda (grob)
           (ly:grob-set-object!
            grob
            'side-support-elements
            (ly:grob-list->grob-array staves)))
         grobs))
      (set! evs '())
      (set! grobs '())))))

(ly:register-translator
 Text_mark_engraver 'Text_mark_engraver
 '((events-accepted . (text-mark-event))
   (grobs-created . (TextMark))
   (properties-read . (stavesFound))
   (properties-written . ())
   (description . "Engraves arbitrary textual marks.")))
