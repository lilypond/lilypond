\version "2.19.21"
\header {
  texidoc  = "Bends avoid dots, but only if necessary."
}

\paper {
  ragged-right = ##t
}

\score {
  <<
    \new Staff \relative {
      \time 4/4
      g''4.-\bendAfter #+1
      s8 s2
      f4.-\bendAfter #+1
    }
  >>
}
