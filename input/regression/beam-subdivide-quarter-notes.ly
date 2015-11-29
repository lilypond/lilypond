\version "2.19.32"

\header {
  
  doctitle = "Beam subdivide over quarter notes"

  texidoc = "Beam subdivisions should match the durations of the subdivided
groups, as established by baseMoment.  However, if the groups are equal or
longer than quarter notes, one beam should be left
"

}

\relative c' {
    \set baseMoment = #(ly:make-moment 1/4)
    \set subdivideBeams = ##t
    c16 [ c c c c c c c c c c c c c c c ]
}
