\version "2.16.0"

\header {

    texidoc = "Test for cross-staff stems.  The test produces a
piano staff with cross-staff connected crochet, semi-quaver,
dotted quaver (beamed with the semi-quaver) and finally a quaver.
All stems should connect, showing correct spacing and
stem length.  The lower connected notes should have no flags." }

\layout {
  \context {
    \PianoStaff
    \consists #Span_stem_engraver
  }
}

{
  \new PianoStaff <<
    \new Staff {
      <b d'>4 r d'16\> e'8. g8 r\!
    }
   \new Staff {
     \clef bass
      \voiceOne
      \autoBeamOff
      \crossStaff { <e g>4 e, g16 a8. c8} d
    }
  >>
}
