\version "2.15.22"

\header {
  texidoc = "Slurs do not force grobs with outside-staff-priority
too high.
"
}

\relative c' {
  f8^"rit"( c' f c' f) r8 r4 |
  c2( c,2 |
  g1)~\startTrillSpan
  g1\stopTrillSpan
  g1(\f
  g,1)
}