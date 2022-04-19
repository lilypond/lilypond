;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2000--2022  Han-Wen Nienhuys <hanwen@xs4all.nl>
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
  (define (generate-change-music-list
           prev-change-mom prev-dir rest-mom event-list acc)
    "@var{prev-change-mom} is the moment (relative to the start of the
auto-change music) of the previous change added to @var{acc}.
@var{rest-mom} is the moment (relative to the start of the auto-change
music) of a potential change at a rest, which is being deferred until
the direction of subsequent notes is seen.
"
    (if (null? event-list)
        ;; add a final skip so that the length of the music is correct
        (cons (skip-of-moment-span prev-change-mom rest-mom) acc)
        ;; event-list is not empty yet
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
              (let* ((change-mom (or rest-mom now))
                     (skip-music
                      (skip-of-moment-span prev-change-mom change-mom))
                     (change-music (make-music
                                    'ContextChange
                                    'change-tag '$autoChange
                                    'change-to-type 'Staff
                                    'change-to-id (if (< dir 0) "down" "up"))))
                (generate-change-music-list now
                                            dir
                                            #f
                                            (cdr event-list)
                                            (cons change-music
                                                  (cons skip-music
                                                        acc))))
              (generate-change-music-list
               prev-change-mom
               prev-dir
               (if pitch #f (or rest-mom now))
               (cdr event-list) acc)))))

  (let* ((context-list
          (recording-group-emulate
           (context-spec-music (make-non-relative-music music) 'Voice "")
           (ly:parser-lookup 'partCombineListener)))
         (changes (reverse! (generate-change-music-list
                             (ly:music-start music)
                             0
                             #f
                             (reverse! (cdar context-list))
                             '())
                            '())))
    ;; \autoChange depends on Simultaneous_music_iterator's processing
    ;; its children in order (at each timestep) so that context
    ;; changes occur before notation events.  If
    ;; Simultaneous_music_iterator's behavior is ever changed, this
    ;; will need to be changed too.
    ;;
    ;; TODO: Is non-relative music necessary here, or only for the
    ;; analysis above?
    (context-spec-music
     (make-music 'SimultaneousMusic
                 'elements (list (make-sequential-music changes)
                                 (make-non-relative-music music))
                 'tags '($autoChange))
     'Voice "")))
