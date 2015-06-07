\version "2.19.21"

\header {
  texidoc = "Beat repeats for patterns containing mixed durations use
a double percent symbol."
}

\relative {
  \repeat percent 4 {
    c'8. <d f>16
  }
  \repeat percent 2 {
    \tuplet 3/2 {
      r8 d e
    }
    c4
  }
}
