\version "2.23.5"

\header {
  texidoc = "@code{\\keepWithTag} works with @code{\\autoChange}."
}

\new PianoStaff {
  \keepWithTag X \tag X \autoChange { c,2 c'' }
}
