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

(define (Chord_name_engraver context)
  (make-engraver
   ((process-music engraver)
    (let ((text (ly:context-property context 'currentChordText #f)))
      ;; Note that user tweaks to ChordName.text are respected.
      ;; However, this is only possible if the default logic would
      ;; have given some text.  Indeed, we cannot create a ChordName
      ;; and look at its text property to decide that we shouldn't
      ;; have created the grob because the property is '().  Practically,
      ;; this means you can't \override ChordName.text to get a ChordName
      ;; if noChordSymbol is '().
      (when (markup? text)
        (let* ((cause (ly:context-property context 'currentChordCause))
               (grob (ly:engraver-make-grob engraver 'ChordName cause))
               ;; Fetching ChordName.text in case it's a callback can't
               ;; be delayed anyway: it needs to be compared for the sake
               ;; of chordChanges.
               (grob-text (ly:grob-property grob 'text #f))
               (final-text (or grob-text text))
               (chord-changes (ly:context-property context 'chordChanges #f)))
          (ly:grob-set-property! grob 'text final-text)
          (when (and chord-changes
                     (equal? final-text (ly:context-property context 'lastChord)))
            (ly:grob-set-property! grob 'once-per-line #t))
          (ly:context-set-property! context 'lastChord final-text)))))))

(ly:register-translator
 Chord_name_engraver 'Chord_name_engraver
 '((events-accepted . ())
   (grobs-created . (ChordName))
   (properties-read . (currentChordCause currentChordText chordChanges lastChord))
   (properties-written . (lastChord))
   (description . "Read @code{currentChordText} to create chord names.")))
