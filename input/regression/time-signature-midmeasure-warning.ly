\version "2.19.16"

\header {
  texidoc = "Mid-measure time signature changes not accompanied by
\\partial generate warnings."
}

#(ly:expect-warning "mid-measure time signature without \\partial")

\score {
  \relative {
    \partial 8 \time 4/4
    a'8 | d4 \time 3/4 cis b
  }
}
