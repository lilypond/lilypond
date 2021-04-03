\version "2.23.2"

\header {
  texidoc = "Setting @code{Stem.neutral-direction} to an invalid
direction value does not result in a crash."
}

\new Voice {
  \override Stem.neutral-direction = #'()
  <<
    { r }
    \new Voice { c }
  >>
}
