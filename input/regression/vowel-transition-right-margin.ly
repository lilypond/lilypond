\version "2.21.0"
\header {
  texidoc = "Vowel transition arrows are always drawn, but
they do not protrude into the margin.  Instead, space is added
so that the arrow can be drawn at @code{minimum-length}."
  }

\layout {
  ragged-right = ##t
}

\score{
  {
    \relative {
      c'4 d e f | \break
      f4 e d c | \break
      c1 |
    }

    \addlyrics {
      c d e VeryLongSyllable \vowelTransition
      \override VowelTransition.minimum-length = #10
      e d c VeryLongSyllable \vowelTransition
      c
    }
  }
}
