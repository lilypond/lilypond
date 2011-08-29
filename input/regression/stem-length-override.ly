
\version "2.15.9"

\header {
  texidoc = "Stem length can be overridden via the function
stem::length
"
}

\relative c' {
  \override Stem #'Y-extent = #(stem::length 8)
  e4 f'4
}
