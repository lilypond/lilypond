\version "2.25.3"

\header {

  doctitle = "Beam subdivide with incomplete remainder"

  texidoc = "Beam count at subdivisions should match the count corresponding
to the location of the current subdivision.  However, if the remainder of the
beam is shorter than that the beam count should be adopted accordingly."

}

\paper {
  indent = 0
}

\relative c'' {
  \omit Staff.TimeSignature
  \time 1/4
  \set subdivideBeams = ##t
  \set baseMoment = \musicLength 32
  c64 ^\markup "Full beam (1/32 division)" [ c c c  c c c c  c c c c  c c c c ]
  c64 ^\markup "Shortened by 1/32" [ c c c  c c c c  c c c c  c c ] r32
  c64 ^\markup "Shortened by 3/32" [ c c c  c c c c  c c  ] r16.
  \set baseMoment = \musicLength 16
  c32 ^\markup "Full beam (1/16 division)" [ c c c c c c c ]
  c32 ^\markup "Shortened by 1/16" [ c c c c c] r16
}
