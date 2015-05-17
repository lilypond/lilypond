\version "2.19.21"

\layout {
  \context {
    \Voice
    \remove "Note_heads_engraver"
    \consists "Completion_heads_engraver"
    \remove "Rest_engraver"
    \consists "Completion_rest_engraver"
  }
}
\midi {
  \context {
    \Score
    midiChannelMapping = #'instrument
  }
}

\header {
texidoc="Partcombined music is preserved"
options="--skip"
}

instrumentOne = \relative {
  c'4 d e f
  R1
  d'4 c b a
  b4 g2 f4
  e1
}

instrumentTwo = \relative {
  R1
  g'4 a b c
  d c b a
  g f( e) d
  e1
}

\score {
<<
  \new Staff = "staff" \partcombine \instrumentOne \instrumentTwo
  >>
  \layout {}
  \midi {}
}
