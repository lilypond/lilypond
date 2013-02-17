\version "2.17.11"

\header {

  texinfo = "
Subdivision works properly for tuplets
  "
}

\relative c'' {
  \set subdivideBeams = ##t
  \set baseMoment = #(ly:make-moment 1/8)
  \set beatStructure = #'(2 2 2 2)
  \repeat unfold 8 {
    \tuplet 3/2 { c16 e d }
  }
}
