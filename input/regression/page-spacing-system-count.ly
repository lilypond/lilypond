\version "2.19.21"

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
      \relative { \repeat unfold 10 c''1 }
      \relative { \repeat unfold 10 c''1 }
      \relative { \repeat unfold 10 c''1 }
    >>
  }
}
