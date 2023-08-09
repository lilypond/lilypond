\version "2.25.8"

\header {

  doctitle = "Beam subdivide with incomplete remainder"

  texidoc = "Beam count at subdivisions should match the count corresponding
to the location of the current subdivision.  However, if the remainder of the
beam is shorter than that and incomplete beams are respected,
the beam count should be adopted accordingly."

}

\paper {
  indent = 0
  ragged-right = ##t
}

\relative c'' {
  \omit Staff.TimeSignature
  \omit Staff.Clef
  \time 1/4
  \set subdivideBeams = ##t
  c64 ^\markup "Full beam " [ c c c  c c c c  c c c c  c c c c ]
  c64 ^\markup "Shortened by 1/32" [ c c c  c c c c  c c c c  c c ] r32
  c64 ^\markup "Shortened by 3/32" [ c c c  c c c c  c c  ] r16.
  \set respectIncompleteBeams = ##t
  c64 ^\markup "Respected & shortened by 1/32" [ c c c  c c c c  c c c c  c c ] r32
  c64 ^\markup "Respected & shortened by 3/32" [ c c c  c c c c  c c  ] r16.
  \unset respectIncompleteBeams
  \break
  c32 ^\markup "Full beam" [ c c c c c c c ]
  c32 ^\markup "Shortened by 1/16" [ c c c c c] r16
  \once \set respectIncompleteBeams = ##t
  c32 ^\markup "Respected & shortened by 1/16" [ c c c c c] r16
}
