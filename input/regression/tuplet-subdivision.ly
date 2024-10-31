\version "2.25.22"

\header {

  texidoc = "
Subdivision works properly for tuplets.
  "
}

\relative {
  \set subdivideBeams = ##t
  \set beatBase = #1/8
  \set beatStructure = 2,2,2,2
  \repeat unfold 8 {
    \tuplet 3/2 { c''16 e d }
  }
}
