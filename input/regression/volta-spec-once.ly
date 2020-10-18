\version "2.23.0"

\header {
  texidoc="@code{\\volta} is useful for nth-time-only music.  The
properties of the bracket label may be overridden."
}

music = \context Voice \fixed c' \repeat volta 2 {
  f1
  \once \override Score.VoltaBracket.text = "1st time only"
  \once \override Score.VoltaBracket.font-name = ""
  <<
    \volta 1 { g~ 1 }
    \volta 2 \unfolded R1*2
  >>
  a1
}

\score { \music }
\score { \unfoldRepeats \music }
