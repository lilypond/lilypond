\version "2.23.0"

\header {
  texidoc="@code{\\volta} can add volta-specific grace notes."
}

music = \context Voice \fixed c' {
  \repeat volta 2 {
    e2
    <<
      a2
      \volta 1 { s4... \grace { g8 f } }
    >>
  }
  b1 |
}

\score { \music }
\score { \unfoldRepeats \music }
