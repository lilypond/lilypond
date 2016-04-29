\version "2.19.39"

\header {
  texidoc = "Music variables may be structured into alists
indexed by numbers or symbols."
}

\layout { ragged-right = ##t }

violin.1 = { e''-\markup \typewriter "\\violin.1" }
violin.2 = { c'-\markup \typewriter "\\violin.2" }
viola.I = { e-\markup \typewriter "\\viola.I" }
viola.II = { c-\markup \typewriter "\\viola.II" }

\score {
  <<
    \new Staff << \violin.1 \\ \violin.2 >>
    \new Staff \with { \clef "alto" } << \viola.I \\ \viola.II >>
  >>
}
