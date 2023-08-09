\version "2.25.8"

\header {
  doctitle = "Beam subdivision with 2 layers of tuplets"

  texidoc = "A triplet of any level should clearly subdivide its@tie{}3
beats at its topmost level, and any subdivisions (of powers of @tie{}2)
of those beats should be at their own level strictly lower than said
topmost level."
}


\paper {
  indent = 0
  ragged-right = ##t
}

\relative c' {
  \time 1/4
  \set subdivideBeams = ##t
  \omit Staff.Clef


  c32 c
  \tuplet 3/2 {
    \repeat unfold 4 c64
    \tuplet 3/2 { \repeat unfold 12 c128 }
    \repeat unfold 4 c64
  }
  c32 c
  \break

  \tuplet 3/2 {
    \tuplet 3/2 { \repeat unfold 12 c128 }
    \repeat unfold 8 c64
  }
  \repeat unfold 4 c32
  \break

  \repeat unfold 4 c32
  \tuplet 3/2 {
    \repeat unfold 8 c64
    \tuplet 3/2 { \repeat unfold 12 c128 }
  }
  \break
}
