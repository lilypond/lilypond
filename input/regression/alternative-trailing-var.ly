\version "2.23.1"

\header {
  texidoc="Alternative music in a variable does not automatically
attach to preceding @code{\\repeat}, but @code{\\alternative} attaches
it."
}

#(ly:set-option 'warning-as-error #t)

alts = \alternative { s1_"B" }

\new Score {
  %% unfold avoids a warning from using \alts outside a repeat
  \new Staff \with { instrumentName = "AAB" } \repeat unfold 1 {
    \repeat volta 2 s1_"A" \alts
  }
}

\new Score {
  \new Staff \with { instrumentName = "ABAB" } {
    \repeat volta 2 s1_"A" \alternative \alts
  }
}
