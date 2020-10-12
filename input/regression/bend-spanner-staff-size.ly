\version "2.23.0"

\header {
  texidoc = "@code{BendSpanner} scales according to different staff sizes."
}

bend-fontSize-change = {
  a,4\6\^ b,\6
}

\score {
  \new StaffGroup
  <<
    \new Staff { \clef "G_8" \bend-fontSize-change }
    \new TabStaff \with { \magnifyStaff #1/2 } \bend-fontSize-change
    \new TabStaff \bend-fontSize-change
    \new TabStaff \with { \magnifyStaff #2 } \bend-fontSize-change
  >>
  \layout {
    \context {
      \Voice
      \omit StringNumber
    }
  }
}
