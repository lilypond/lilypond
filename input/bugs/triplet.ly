\header {
texidoc="manual beam override is ignored in first triplet"
}

\score {
  \context Voice \notes\relative c'' {
    \times 2/3 { < [ a8 c > < a c > < a c ] > }
    \times 2/3 < { [ a8 a a ] } { c c c ] } >
  }
}
