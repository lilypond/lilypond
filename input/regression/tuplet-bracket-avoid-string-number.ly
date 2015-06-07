\version "2.19.21"

\header {
  texidoc = "@code{TupletBracket} grobs avoid @code{StringNumber} grobs.
"
}

\relative {
 \override TupletBracket.direction = #UP
 \tuplet 3/2 { d''4 <a\4>8 }
}
