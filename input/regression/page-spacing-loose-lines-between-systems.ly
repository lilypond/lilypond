\version "2.13.7"

\header {
  texidoc = "The vertical spacing engine is not confused by
a loose line below a system followed by a loose line above
the next system."
}

\paper {
  indent = 0
  ragged-right = ##t
}

\book {
  \score {
    <<
      \chords { s1 \break d1 }
      \relative c { c1 c1 }
      \addlyrics { word }
    >>
  }
}

