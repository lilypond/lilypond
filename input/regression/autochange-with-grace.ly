\header {
    texidoc = "Grace notes are placed on the appropriate staff."
}

\version "2.19.22"
\layout { ragged-right = ##t }

\score {
  <<
    \context Staff = "up" { s1 }
    \context Staff = "down" {
      \clef "bass"
      r2
      \autochange { \grace { g8 g' } e'4 e }
    }
  >>
}
