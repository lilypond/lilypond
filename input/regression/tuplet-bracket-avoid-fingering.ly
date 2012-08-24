\version "2.16.0"

\header {
  texidoc = "@code{TupletBracket} grobs avoid @code{Fingering} grobs.
"
}

\relative d'' {
 \override TupletBracket #'direction = #UP
 \times 2/3 { d4 a8-4 }
}
