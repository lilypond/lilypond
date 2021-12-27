\version "2.23.0"

\header {
  texidoc="@code{\\volta} is useful for volta-specific rhythms."
}

music = \context Voice \fixed c' \repeat volta 2 {
  <<
    \volta 1 a1
    \volta 2 \new Voice {
      \volta #'() { \voiceTwo \set fontSize = -3 }
      a2. 4
    }
  >>
}

\score { \music }
\score { \unfoldRepeats \music }
