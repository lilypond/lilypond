\version "2.23.14"

\header {
  texidoc = "The @code{\\textMark} and @code{\\textEndMark}
commands draw arbitrary textual indications between notes."
}

\paper {
  ragged-right = ##t
}

\new Staff \with { instrumentName = "2 fl." } {
  \repeat volta 2 {
    \textMark "Fl. 1 solo"
    \repeat unfold 5 { c''2 g'' }
    \textEndMark "Volta 2 a due"
  }
}
