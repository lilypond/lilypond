\version "2.23.0"

\header {
  texidoc="When unfolding volta-specific music, music marked for an
  out-of-range volta is ignored.  In this case, four notes marked 1-4
  should appear."
}

\context Voice \fixed c' {
  \repeat unfold 2 <<
    \volta 1 c4-1
    \volta 2 e-3
    \volta 3 c,, % should not appear
  >> \alternative {
    <<
      \volta 3 e,, % should not appear
      \volta 2 f-4
      \volta 1 d-2
    >>
  }
}
