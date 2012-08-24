\version "2.16.0"

\header {
  texidoc = "Grace notes at the start of a partial measure do not
break autobeaming."
}

\relative c' {
  \partial 4
  \grace e16
  d8 d
  c8 c c c c c c c
}
