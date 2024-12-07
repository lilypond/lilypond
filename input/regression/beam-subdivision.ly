\version "2.25.23"


\header {

  texidoc = "Beam count at subdivisions should match the location
of the current subdivision."
}


\layout {
  indent = 1
  ragged-right = ##t
}

\relative c'' {
  \set subdivideBeams = ##t
  \omit Staff.Clef
  \omit Staff.TimeSignature

  \set beamMinimumSubdivision = #1/4
  c32[^\markup{"beamMinimumSubdivision 1/4"} c c c c c c c]
  \break

  \set beamMinimumSubdivision = #1/8
  c32[^\markup{"beamMinimumSubdivision 1/8"} c c c c c c c]
  \break

  \set beamMinimumSubdivision = #1/16
  c32^\markup{"beamMinimumSubdivision 1/16"}[ c c c c c c c]
  \break

  \set beamMinimumSubdivision = #1/32
  c64^\markup{"beamMinimumSubdivision 1/32"}[ \repeat unfold 14 {c64} c64]
  \break

  \unset beamMinimumSubdivision

  \set beamMaximumSubdivision = #1/4
  c32[^\markup{"beamMaximumSubdivision 1/4"} c c c c c c c]
  \break

  \set beamMaximumSubdivision = #1/8
  c32[^\markup{"beamMaximumSubdivision 1/8"} c c c c c c c]
  \break

  \set beamMaximumSubdivision = #1/16
  c32^\markup{"beamMaximumSubdivision 1/16"}[ c c c c c c c]
  \break

  \set beamMaximumSubdivision = #1/32
  c64^\markup{"beamMaximumSubdivision 1/32"}[ \repeat unfold 14 {c64} c64]
  \break
}
