\version "2.23.3"

\header {
  texidoc = "Alternative notation systems using accidentals different
from the Western ones set them systematically, for standalone markups
and all grobs that print accidentals.

This include file provides a function to draw many accidental in
different contexts.  It is used by various tests."
}

% Print a staff having a bunch of grobs with accidentals.
test =
#(define-scheme-function (expected-glyph alteration make-markup?)
                         (string? exact-rational? boolean?)
   (let ((pitch-low (ly:make-pitch 0 0 alteration))
         (pitch-high (ly:make-pitch 1 0 alteration)))
     (add-score
       #{
         \score {
           \header {
             piece = \markup { All \musicglyph #expected-glyph }
           }
           <<
             \new Staff \with {
               % InstrumentName
               instrumentName = \markup \accidental #alteration
             }
             \new Voice \with {
               \consists Ambitus_engraver
               \consists Balloon_engraver
               \consists Horizontal_bracket_engraver
             } {
               % AmbitusAccidental
               $pitch-low
               % Accidental
               $pitch-high
               r
               % AccidentalCautionary
               \accidentalStyle teaching
               $pitch-high
               r
               % AccidentalSuggestion
               \accidentalStyle forget
               \set suggestAccidentals = ##t
               $pitch-high
               % BalloonText
               \balloonGrobText
                 Rest
                 #'(2 . 2)
                 \markup \accidental #alteration
               r2
               % HorizontalBracketText
               \once \override HorizontalBracketText.text =
                 \markup \accidental #alteration
               $pitch-high \startGroup
               $pitch-high \stopGroup
               % TextScripts
               $pitch-high ^\markup \accidental #alteration
             }
             % NoteName
             \new NoteNames {
               $pitch-high
             }
             % ChordName
             \new ChordNames {
               $pitch-high
             }
           >>
         }
       #})
       (if make-markup?
         #{
           \markup \accidental #alteration
         #})))
