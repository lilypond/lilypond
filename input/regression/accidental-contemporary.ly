\version "2.19.21"
\header {

texidoc = "Several automatic accidental rules
aim to reproduce contemporary music notation
practices:
@itemize
@item
@code{'dodecaphonic} style prints accidentals on every
note (including naturals)
@item
@code{'neo-modern style} prints accidentals on every note
(not including naturals), except when a note is
immediately repeated
@item
@code{'neo-modern-cautionary} style acts like neo-modern,
adding cautionary parentheses around accidentals.
@item
@code{'teaching} prints accidentals normally, but adds
cautionary accidentals when an accidental is
already included in the key signature.
@end itemize

Both scores should show the same accidentals.
"

}

\layout { ragged-right = ##t }

\score {
  \relative {
    \accidentalStyle dodecaphonic
    gis'4 a g gisis
    \accidentalStyle neo-modern
    gis8 a gis gis g' gis gis,, a'
    \accidentalStyle neo-modern-cautionary
    eis fis eis eis g2
    \accidentalStyle teaching
    \key e \major
    e8 eis fis fis gis2
  }
}

\score {
  \relative {
    \set Staff.autoAccidentals = #'()
    \set Staff.autoCautionaries = #'()
    gis'!4 a! g! gisis!
    gis!8 a gis! gis g'! gis! gis,,! a'
    eis! fis! eis? eis g?2
    \key e \major
    e8 eis! fis? fis gis?2
  }
}
