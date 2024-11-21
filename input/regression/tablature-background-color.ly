\version "2.25.22"

\header {
  texidoc = "Using the @code{whiteout-color} property it is possible to
adjust @code{TabStaff} to a colored background."
}

\markup \combine
  \with-dimensions #'(0 . 0) #'(0 . 0)
    \with-color \red
      \filled-box #'(7 . 24) #'(-5 . 5) #0
  \score {
    \new TabStaff {
      \override TabNoteHead.whiteout-color = \red
      e2 a }
  }
