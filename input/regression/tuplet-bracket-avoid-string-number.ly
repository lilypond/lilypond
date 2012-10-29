\version "2.17.6"

\header {
  texidoc = "@code{TupletBracket} grobs avoid @code{StringNumber} grobs.
"
}

\relative d'' {
 \override TupletBracket.direction = #UP
 \times 2/3 { d4 <a\4>8 }
}
