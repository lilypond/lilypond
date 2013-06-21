;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2012 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;                 Neil Puttock <n.puttock@gmail.com>
;;;;                 Carl Sorensen <c_sorensen@byu.edu>
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

;; TODO: should link back into user manual.

(define (mm-rest-child-list music)
  "Generate events for multimeasure rests,
to be used by the sequential-iterator"
  (let ((location (ly:music-property music 'origin))
        (duration (ly:music-property music 'duration)))
    (list (make-music 'BarCheck
                      'origin location)
          (make-event-chord (cons (make-music 'MultiMeasureRestEvent
                                              'origin location
                                              'duration duration)
                                  (ly:music-property music 'articulations)))
          (make-music 'BarCheck
                      'origin location))))

(define (make-volta-set music)
  (let* ((alts (ly:music-property music 'elements))
         (lalts (length alts))
         (times (ly:music-property music 'repeat-count)))
    (map (lambda (x y)
           (make-music
            'SequentialMusic
            'elements
            ;; set properties for proper bar numbering
            (append
             (list (make-music 'AlternativeEvent
                               'alternative-dir (if (= y 0)
                                                    -1
                                                    0)
                               'alternative-increment
                               (if (= 0 y)
                                   (1+ (- times
                                          lalts))
                                   1)))
             (list x)
             (if (= y (1- lalts))
                 (list (make-music 'AlternativeEvent
                                   'alternative-dir 1
                                   'alternative-increment 0))
                 '()))))
         alts
         (iota lalts))))

(define (make-ottava-set music)
  "Set context properties for an ottava bracket."
  (let ((octavation (ly:music-property music 'ottava-number)))

    (list (context-spec-music
           (make-apply-context
            (lambda (context)
              (let ((offset (* -7 octavation))
                    (string (assoc-get octavation '((2 . "15ma")
                                                    (1 . "8va")
                                                    (0 . #f)
                                                    (-1 . "8vb")
                                                    (-2 . "15mb")))))
                (set! (ly:context-property context 'middleCOffset) offset)
                (set! (ly:context-property context 'ottavation) string)
                (ly:set-middle-C! context))))
           'Staff))))

(define (make-time-signature-set music)
  "Set context properties for a time signature."
  (let* ((num (ly:music-property music 'numerator))
         (den (ly:music-property music 'denominator))
         (structure (ly:music-property music 'beat-structure))
         (fraction (cons num den)))
    (list (descend-to-context
           (context-spec-music
            (make-apply-context
             (lambda (context)
               (let* ((time-signature-settings
                       (ly:context-property context 'timeSignatureSettings))
                      (my-base-length
                       (base-length fraction time-signature-settings))
                      (my-beat-structure
                       (if (null? structure)
                           (beat-structure my-base-length
                                           fraction
                                           time-signature-settings)
                           structure))
                      (beaming-exception
                       (beam-exceptions fraction time-signature-settings))
                      (new-measure-length (ly:make-moment num den)))
                 (ly:context-set-property!
                  context 'timeSignatureFraction fraction)
                 (ly:context-set-property!
                  context 'baseMoment (ly:make-moment my-base-length))
                 (ly:context-set-property!
                  context 'beatStructure my-beat-structure)
                 (ly:context-set-property!
                  context 'beamExceptions beaming-exception)
                 (ly:context-set-property!
                  context 'measureLength new-measure-length))))
            'Timing)
           'Score))))
