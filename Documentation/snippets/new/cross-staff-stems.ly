\version "2.23.2"

\header {
  lsrtags = "contexts-and-engravers, staff-notation, tweaks-and-overrides"

  texidoc = "
This snippet shows the use of the @code{Span_stem_engraver} and
@code{\\crossStaff} to connect stems across staves automatically.

The stem length need not be specified, as the variable distance between
noteheads and staves is calculated automatically.
"

  doctitle = "Cross staff stems"
}


\layout {
  \context {
    \PianoStaff
    \consists "Span_stem_engraver"
  }
}

{
  \new PianoStaff <<
    \new Staff {
      <b d'>4 r d'16\> e'8. g8 r\!
      e'8 f' g'4 e'2
    }
    \new Staff {
      \clef bass
      \voiceOne
      \autoBeamOff
      \crossStaff { <e g>4 e, g16 a8. c8} d
      \autoBeamOn
      g8 f g4 c2
    }
  >>
}
