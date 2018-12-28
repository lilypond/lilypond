\header {
    texidoc = "Grace notes are placed on the appropriate staff."
}

\version "2.21.0"
\layout { ragged-right = ##t }

\score {
  <<
    \context Staff = "up" { s1 }
    \context Staff = "down" {
      \clef "bass"
      r2
      \grace { \autoChange { e16 e'' } }
      c'2
    }
  >>
}
