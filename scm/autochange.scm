;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2000--2020  Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;                  Jan Nieuwenhuizen <janneke@gnu.org>
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autochange.scm - fairly related to part combining.

(define-public (make-autochange-music music . ref-pitch)
  (define ref-pitch-steps
    (if (and (pair? ref-pitch) (car ref-pitch))
        (ly:pitch-steps (car ref-pitch))
        0))
  (define (generate-split-list change-moment prev-dir event-list acc)
    (if (null? event-list)
        acc
        (let* ((now-tun (caar event-list))
               (evs (map car (cdar event-list)))
               (now (car now-tun))
               (notes (filter (lambda (x)
                                (ly:in-event-class? x 'note-event))
                              evs))
               (pitch (if (pair? notes)
                          (ly:event-property (car notes) 'pitch)
                          #f))
               (dir (if pitch
                        (sign
                         (- (ly:pitch-steps pitch) ref-pitch-steps))
                        0)))
          ;; tail recursive.
          (if (and (not (= dir 0))
                   (not (= dir prev-dir)))
              (generate-split-list #f
                                   dir
                                   (cdr event-list)
                                   (cons (cons
                                          (if change-moment
                                              change-moment
                                              now)
                                          (if (< dir 0) 'down 'up)) acc))
              (generate-split-list
               (if pitch #f (if change-moment change-moment now))
               dir
               (cdr event-list) acc)))))

  (let* ((m (make-music 'AutoChangeMusic))
         (m1 (context-spec-music (make-non-relative-music music) 'Voice ""))
         (context-list
          (recording-group-emulate m1
                                   (ly:parser-lookup 'partCombineListener)))
         (rev (reverse! (cdar context-list)))
         (split (reverse! (generate-split-list
                           #f
                           0
                           rev
                           '())
                          '())))
    (set! (ly:music-property m 'element) m1)
    (set! (ly:music-property m 'context-change-list) split)
    m))
