\version "2.13.4"

\header {
  texidoc = "Page layout and stretching work with system-count enabled."
}

#(set-default-paper-size "a6")

\paper {
  system-count = 2
  ragged-last-bottom = ##f
}

\book {
  \score {
    <<
      \relative c'' { \repeat unfold 10 c1 }
      \relative c'' { \repeat unfold 10 c1 }
      \relative c'' { \repeat unfold 10 c1 }
    >>
  }
}
