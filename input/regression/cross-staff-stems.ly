\version "2.16.0"

\header {
  texidoc = "Test for cross-staff stems.  We check for properly
connected chords, triplets, and auto-beamed beams correctly ended."
}

\layout {
  \context {
    \PianoStaff
    \consists #Span_stem_engraver
  }
}

{
  \new PianoStaff <<
    \new Staff {
      r4 e'8 f' <b d'>8\> r \tuplet 3/2 { e'8. f'16 g'8 } |
      g r\!
    }
   \new Staff {
     \clef bass
      \stemUp
      c8 d \crossStaff { e f <e g>8 r \tuplet 3/2 { e8. f16 g8 } |
      c8 } d
    }
  >>
}
