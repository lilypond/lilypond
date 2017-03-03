\version "2.18.0"

\header {
  texidoc="A grace in the first alternative does not cause
the beaming to go awry in subsequent material"
}

\layout { ragged-right = ##t }

\relative c'' {
  \repeat volta 2 {
    c8. c16 c4 c2 |
  }
  \alternative {
    { \grace c32 c8. c16 c4 c2 | }
    { c8. c16 c4 c2 | }
  }
  c8. c16 c4 c2 |
}
