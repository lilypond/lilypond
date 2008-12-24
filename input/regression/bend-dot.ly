\version "2.12.0"
\header {
  texidoc  = "Bends avoid dots, but only if necessary."
}

\paper {
  ragged-right = ##t
}

\score {
  <<
    \new Staff \relative c' {
      \time 4/4
      g''4.-\bendAfter #+1
      s8 s2
      f4.-\bendAfter #+1
    }
  >>
}
