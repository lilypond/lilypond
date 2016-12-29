\version "2.19.52"

\header {
  texidoc = "The music function @code{\\unfoldRepeats} can take an
optional argument-list specifying which type(s) of repeated music has
to be unfolded."
}

m =
  \repeat volta 2 {
      \repeat percent 2 { c'1 }
      \repeat tremolo 4 { c'16 d' }
      f'2
  }
  \alternative {
      { d'1 }
      { e'1 }
  }

\markup "not expanding"
\m

\markup "expanding all"
\unfoldRepeats \m

\markup "expanding percent-repeated-music"
\unfoldRepeats percent \m

\markup "expanding tremolo-repeated-music"
\unfoldRepeats tremolo \m

\markup "expanding volta-repeated-music"
\unfoldRepeats volta \m

\markup \column {
  "combinations are possible:"
  "expanding percent-repeated-music and tremolo-repeated-music"
}
\unfoldRepeats percent,tremolo \m
