\version "2.12.0"
\header {
  texidoc = "Extenders will not protrude into the right margin"
  }

\score{
  {
    \relative c' {
      \override Score.PaperColumn #'keep-inside-line = ##t
      c4 d e f ~ | \break
      f4 e d c |
    }

    \addlyrics {
      c d e effffffffffff __
      e d c
    }
  }
}