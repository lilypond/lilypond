\version "2.19.21"
\header {
  texidoc = "Extenders will not protrude into the right margin"
  }

\score{
  {
    \relative {
      c'4 d e f ~ | \break
      f4 e d c |
    }

    \addlyrics {
      c d e effffffffffff __
      e d c
    }
  }
}
