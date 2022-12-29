\version "2.19.21"

\header {

  texidoc = "Falls and doits terminate correctly on the next note,
even when started inside a @code{\\grace}."
}

\layout {
  ragged-right = ##f
}

<<
  \new Staff {
    d'2.\bendAfter 10
    4 \bendAfter 10
    |
    \grace { 2. \bendAfter 10 }
    4 \bendAfter 10
  }
  \new Staff {
    d'4 \bendAfter 2
    4 \bendAfter 4
    4 \bendAfter 6
    4 \bendAfter 8
    |
    \grace {
      4 \bendAfter 2
      4 \bendAfter 4
      4 \bendAfter 6
    }
    4 \bendAfter 8
  }
>>
