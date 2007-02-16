\version "2.10.19"
\header {
  texidoc = "extenders will not protude into the right margin"
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
\paper {
  ragged-right = ##t
}
