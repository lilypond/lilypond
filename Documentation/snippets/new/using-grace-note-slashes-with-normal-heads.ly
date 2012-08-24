\version "2.16.0"

\header {
  lsrtags = "rhythms"

  texidoc = "
The slash through the stem found in acciaccaturas can be applied in
other situations.

"
  doctitle = "Using grace note slashes with normal heads"
}

\relative c'' {
  \override Flag #'stroke-style = #"grace"
  c8( d2) e8( f4)
}

