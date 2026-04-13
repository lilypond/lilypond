\version "2.25.35"

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
      \relative { \*10 c''1 }
      \relative { \*10 c''1 }
      \relative { \*10 c''1 }
    >>
  }
}
