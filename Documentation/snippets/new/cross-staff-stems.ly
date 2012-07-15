\version "2.15.42"

\header {
  lsrtags = "staff-notation, tweaks-and-overrides, contexts-and-engravers"
  texidoc = "This file demonstrates a scheme engraver that
connects stems across staves.  The stem length need not be specified, as
the code takes care of the variable distance between noteheads and staves."
  doctitle = "Cross staff stems"
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
