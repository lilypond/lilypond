\version "2.19.21"

\header {
  texidoc = "@code{TupletBracket} grobs avoid @code{Fingering} grobs.
"
}

\relative {
 \override TupletBracket.direction = #UP
 \tuplet 3/2 { d''4 a8-4 }
}
