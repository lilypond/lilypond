\version "2.13.2"

\header {
  texidoc = "Collision resolution involving dotted harmonic heads
succeeds when dots are hidden since @code{rhythmic-head-interface}
will only retrieve @code{'dot-count} from live grobs.
"
}

\relative c' {
  <<
    { <fis\harmonic>2. }
    \\
    { e2. }
  >>
  r4
}
