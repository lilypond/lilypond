\version "2.19.21"

\header {
  texidoc = "The @code{dot-count} property
for @code{Dots} can be modified by the user."

}

\relative {
  c''4.. a16
  \override Dots.dot-count = #4
  g4.. e16
  \override Dots.dot-count = #0
  c4. r8 r2
}
