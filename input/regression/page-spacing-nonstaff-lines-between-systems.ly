\version "2.14.0"

\header {
  texidoc = "The vertical spacing engine is not confused by a
non-staff line below a system followed by a loose line above the
next system."
}

\paper {
  indent = 0
  ragged-right = ##t
}

\book {
  \score {
    <<
      \chords { s1 \break d1 }
      \new Staff = "s" { c1 c1 }
      \addlyrics { word }
      \addlyrics { \set alignAboveContext = "s" _ up }
    >>
  }
}

