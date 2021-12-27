\version "2.23.0"

\header {
  texidoc="@code{\\volta} can add a volta-specific tie."
}

music = \context Voice \fixed c' \repeat volta 2 {
  <<
    { a2. 4 }
    \volta 2 { s2.~ s4 }
  >>
}

\score { \music }
\score { \unfoldRepeats \music }
