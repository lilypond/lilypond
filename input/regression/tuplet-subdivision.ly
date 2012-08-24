\version "2.16.0"

\header {

  texinfo = "
Subdivision works properly for tuplets
  "
}

\relative c'' {
  \set subdivideBeams = ##t
  \set baseMoment = #(ly:make-moment 1 8)
  \set beatStructure = #'(2 2 2 2)
  \repeat unfold 8 {
    \times 2/3 { c16 e d }
  }
}
