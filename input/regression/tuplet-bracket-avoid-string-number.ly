\version "2.17.11"

\header {
  texidoc = "@code{TupletBracket} grobs avoid @code{StringNumber} grobs.
"
}

\relative d'' {
 \override TupletBracket.direction = #UP
 \tuplet 3/2 { d4 <a\4>8 }
}
