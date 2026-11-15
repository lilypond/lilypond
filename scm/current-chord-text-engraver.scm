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

(define (Current_chord_text_engraver context)
  (let ((note-events '())
        (rest-event #f))
    (make-engraver
     (listeners
      ((note-event engraver event)
       (set! note-events (cons event note-events)))
      ((general-rest-event engraver event #:once)
       (set! rest-event event)))
     ((pre-process-music engraver)
      (cond
       (rest-event
        (ly:context-set-property! context 'currentChordCause rest-event)
        (let ((symbol (ly:context-property context 'noChordSymbol)))
          (ly:context-set-property! context 'currentChordText symbol)))
       ((pair? note-events)
        (ly:context-set-property! context 'currentChordCause (car note-events))
        (let ((pitches '())
              (bass '())
              (inversion '()))
          (for-each
           (lambda (note-ev)
             (let ((pitch (ly:event-property note-ev 'pitch)))
               (when (ly:pitch? pitch)
                 (let ((is-bass (ly:event-property note-ev 'bass #f))
                       (is-inversion (ly:event-property note-ev 'inversion #f)))
                   (if is-bass
                       (set! bass pitch)
                       (let* ((oct (ly:event-property note-ev 'octavation))
                              (orig
                               (if (integer? oct)
                                   (let ((transp (ly:make-pitch (- oct)
                                                                0)))
                                     (ly:pitch-transpose pitch transp))
                                   pitch)))
                         (set! pitches (cons orig pitches))
                         (when is-inversion
                           (set! inversion pitch)
                           (when (not (integer? oct))
                             (ly:programming-error
                              "inversion does not have original pitch")))))))))
           note-events)
          (let* ((sorted-pitches (sort pitches ly:pitch<?))
                 (formatter (ly:context-property context 'chordNameFunction))
                 (markup (formatter sorted-pitches bass inversion context)))
            (ly:context-set-property! context 'currentChordText markup))))
       (else
        (ly:context-set-property! context 'currentChordCause #f)
        (ly:context-set-property! context 'currentChordText #f))))
     ((stop-translation-timestep engraver)
      (set! note-events '())
      (set! rest-event #f)))))

(ly:register-translator
 Current_chord_text_engraver 'Current_chord_text_engraver
 '((events-accepted . (note-event general-rest-event))
   (properties-read . (chordNameExceptions
                       chordNameFunction
                       chordRootNamer
                       chordNoteNamer
                       majorSevenSymbol
                       noChordSymbol))
   (properties-written . (currentChordCause currentChordText))
   (description . "Catch note and rest events and generate the
appropriate chord text using @code{chordNameFunction}.  Actually
creating a chord name grob is left to other engravers.")))
