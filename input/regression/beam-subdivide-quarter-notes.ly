\version "2.25.8"

\header {

  doctitle = "Beam subdivide over quarter notes"

  texidoc = "Beam count at subdivisions should match the location
of the current subdivision.  However, if the groups are equal or
longer than quarter notes, one beam should always be left."

}

\paper {
  indent = 0
  ragged-right = ##t
}

\relative c' {
  \set subdivideBeams = ##t
  \set minimumBeamSubdivisionInterval = \musicLength 4
  c16 [ c c c c c c c c c c c c c c c ]
}
