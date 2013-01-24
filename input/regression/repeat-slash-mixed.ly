\version "2.17.11"

\header {
  texidoc = "Beat repeats for patterns containing mixed durations use
a double percent symbol."
}

\relative c' {
  \repeat percent 4 {
    c8. <d f>16
  }
  \repeat percent 2 {
    \tuplet 3/2 {
      r8 d e
    }
    c4
  }
}
