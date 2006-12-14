\version "2.11.2"
\header {
  texidoc  = "Bends avoid dots, but only if necessary."
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

\paper {
  ragged-right = ##t
}
