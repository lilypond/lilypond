\version "2.13.7"

\header {
  texidoc = "\\markup \\score displays all systems.  Spacing between
  systems is set using @code{baseline-skip}.
"
}


\markup {
  \override #'(baseline-skip . 10)
  \score {
    \new Staff \relative c' {
      c4 d e f \break
      g1
    }
    \layout {
      indent = 0
      ragged-right = ##t
    }
  }
}
